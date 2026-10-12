-- -----------------------------------------------------------------------------
-- smk, the smart make (https://github.com/LionelDraghi/smk)
-- SPDX-FileCopyrightText: 2026 Lionel Draghi
-- SPDX-License-Identifier: Apache-2.0
-- -----------------------------------------------------------------------------

with Smk.Files;    use Smk.Files;
with Smk.IO;
with Smk.Settings; use Smk.Settings;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Directories;
with Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Unchecked_Conversion;
with Interfaces;             use Interfaces;
with Interfaces.C;           use Interfaces.C;
with Interfaces.C.Strings;   use Interfaces.C.Strings;
with System;

with linux_ptrace_h; use linux_ptrace_h;
-- Ada binding to /usr/include/linux/ptrace.h (src/ptrace_binding)

package body Smk.Runs.Observers.Ptrace is

   -- -----------------------------------------------------------------------
   -- Imported C subprograms.
   -- ptrace is variadic in C; it is called here with its four usual
   -- arguments, which is valid on the x86_64 ABI.

   subtype C_Pid is C.int;

   function Sys_Fork return C_Pid;
   pragma Import (C, Sys_Fork, "fork");

   procedure Sys_Execvp (File : C.Strings.chars_ptr;
                         Argv : C.Strings.chars_ptr_array);
   pragma Import (C, Sys_Execvp, "execvp");

   procedure Sys_Exit (Status : C.int);
   pragma Import (C, Sys_Exit, "_exit");
   pragma No_Return (Sys_Exit);

   function Sys_Waitpid (Pid     : C_Pid;
                         Status  : access C.int;
                         Options : C.int) return C_Pid;
   pragma Import (C, Sys_Waitpid, "waitpid");

   function Sys_Ptrace (Request : C.long;
                        Pid     : C_Pid;
                        Addr    : C.long;
                        Data    : C.long) return C.long;
   pragma Import (C, Sys_Ptrace, "ptrace");

   function Sys_Readlink (Path : C.Strings.chars_ptr;
                          Buf  : System.Address;
                          Size : C.size_t) return C.long;
   pragma Import (C, Sys_Readlink, "readlink");

   type Iovec is record
      Base : System.Address;
      Len  : C.size_t;
   end record;
   pragma Convention (C_Pass_By_Copy, Iovec);

   function Sys_Process_Vm_Readv (Pid        : C_Pid;
                                 Local_Iov  : access Iovec;
                                 Local_Cnt  : C.unsigned_long;
                                 Remote_Iov : access Iovec;
                                 Remote_Cnt : C.unsigned_long;
                                 Flags      : C.unsigned_long)
     return C.long;
   pragma Import (C, Sys_Process_Vm_Readv, "process_vm_readv");

   -- prctl is variadic in C; it is called here with its five usual
   -- arguments, which is valid on the x86_64 ABI.
   function Sys_Prctl (Option                : C.int;
                       Arg2, Arg3, Arg4, Arg5 : C.unsigned_long)
     return C.int;
   pragma Import (C, Sys_Prctl, "prctl");

   -- Constants from sys/prctl.h, linux/seccomp.h, linux/filter.h and
   -- linux/audit.h, used by the seccomp subscription filter below

   PR_SET_NO_NEW_PRIVS : constant C.int := 38;
   PR_SET_SECCOMP      : constant C.int := 22;
   SECCOMP_MODE_FILTER : constant C.unsigned_long := 2;

   Seccomp_Ret_Trace : constant C.unsigned := 16#7FF0_0000#;
   Seccomp_Ret_Allow : constant C.unsigned := 16#7FFF_0000#;
   -- Seccomp_Ret_Trace notifies the tracer at the syscall entry;
   -- Seccomp_Ret_Allow lets the syscall run at full speed, with no
   -- stop at all.

   Audit_Arch_X86_64 : constant C.unsigned := 16#C000_003E#;

   Bpf_Ld_W_Abs : constant C.unsigned_short := 16#20#;
   -- BPF_LD | BPF_W | BPF_ABS: load a 32 bits word of seccomp_data
   Bpf_Jeq_K    : constant C.unsigned_short := 16#15#;
   -- BPF_JMP | BPF_JEQ | BPF_K: jump if the accumulator equals K
   Bpf_Ret_K    : constant C.unsigned_short := 16#06#;
   -- BPF_RET | BPF_K: return the action given in K

   Seccomp_Data_Nr   : constant C.unsigned := 0;
   Seccomp_Data_Arch : constant C.unsigned := 4;
   -- offsets of the fields of struct seccomp_data

   type Sock_Filter is record
      Code : C.unsigned_short;
      Jt   : C.unsigned_char;
      Jf   : C.unsigned_char;
      K    : C.unsigned;
   end record;
   pragma Convention (C, Sock_Filter);

   type Sock_Fprog is record
      Len    : C.unsigned_short;
      Filter : System.Address;
   end record;
   pragma Convention (C, Sock_Fprog);

   function Addr_To_Long is new Ada.Unchecked_Conversion
     (System.Address, C.long);
   function Long_To_Addr is new Ada.Unchecked_Conversion
     (C.long, System.Address);

   -- -----------------------------------------------------------------------
   -- Constants

   -- the options are computed in a modular type: the logical operators
   -- are not defined on signed types such as C.long
   Trace_Options : constant C.unsigned_long :=
     C.unsigned_long (PTRACE_O_TRACESYSGOOD)
     or C.unsigned_long (PTRACE_O_TRACEFORK)
     or C.unsigned_long (PTRACE_O_TRACEVFORK)
     or C.unsigned_long (PTRACE_O_TRACECLONE)
     or C.unsigned_long (PTRACE_O_TRACEEXEC)
     or C.unsigned_long (PTRACE_O_TRACESECCOMP)
     or C.unsigned_long (PTRACE_O_EXITKILL);

   -- x86_64 syscall numbers (arch-specific, see the spec comment)
   SC_Open      : constant C.long := 2;
   SC_Getcwd    : constant C.long := 79;
   SC_Chdir     : constant C.long := 80;
   SC_Rename    : constant C.long := 82;
   SC_Mkdir     : constant C.long := 83;
   SC_Creat     : constant C.long := 85;
   SC_Openat    : constant C.long := 257;
   SC_Renameat  : constant C.long := 264;
   SC_Renameat2 : constant C.long := 316;
   SC_Openat2   : constant C.long := 437;

   AT_FDCWD : constant C.long := -100;

   -- The syscalls the observer subscribes to: only they stop the
   -- traced processes, all the others run at full speed.
   Subscribed_Syscalls : constant array (Positive range <>) of C.unsigned
     := [C.unsigned (SC_Open),
         C.unsigned (SC_Getcwd),
         C.unsigned (SC_Chdir),
         C.unsigned (SC_Rename),
         C.unsigned (SC_Mkdir),
         C.unsigned (SC_Creat),
         C.unsigned (SC_Openat),
         C.unsigned (SC_Renameat),
         C.unsigned (SC_Renameat2),
         C.unsigned (SC_Openat2)];

   procedure Install_Seccomp_Filter;
   -- Install in the child, before exec, the seccomp BPF filter that
   -- subscribes to Subscribed_Syscalls only (design discussion D1):
   -- without it, every syscall of the traced commands, and of all
   -- their children, stops the process twice.
   -- On prctl failure, or on a foreign architecture, the run simply
   -- proceeds without filter: every syscall will stop, slower but
   -- with the same observations. This procedure is silent: it runs
   -- in the child, and printing there would duplicate the buffered
   -- output inherited from the parent.

   O_Accmask : constant C.long := 16#4#;
   -- the modulus of the access mode bits (O_WRONLY or O_RDWR, that is
   -- a write access): the access mode is Flags mod O_Accmask, as the
   -- logical operators are not defined on signed types such as C.long

   Sig_Trap        : constant C.unsigned := 16#5#;
   Sigtrap_Syscall : constant C.unsigned := 16#85#;
   -- SIGTRAP or 2#1000_0000#, the syscall stops with TRACESYSGOOD
   Sig_Stop        : constant C.unsigned := 19;
   Sig_Tstp        : constant C.unsigned := 20;
   Sig_Ttin        : constant C.unsigned := 21;
   Sig_Ttou        : constant C.unsigned := 22;
   -- The job control signals are never delivered back, to avoid infinite
   -- stop / deliver loops on group stops.
   -- Fixme: use PTRACE_LISTEN for a fully correct job control handling

   W_All : constant C.int := 16#4000_0000#;
   -- __WALL, to catch threads as well as processes

   Max_Path : constant := 1024;

   -- -----------------------------------------------------------------------
   -- PTRACE_GET_SYSCALL_INFO result, read as an array of 64 bits words:
   --  0 : op, reserved, flags, arch
   --  1 : instruction_pointer
   --  2 : stack_pointer
   --  3 : entry.nr, or exit.rval
   --  4 .. 9 : entry.args
   -- 10 : unused (seccomp.ret_data)
   type Syscall_Info_Buffer is array (0 .. 10) of C.long;

   -- -----------------------------------------------------------------------
   -- Traced processes state

   type Pending_Event is record
      Kind          : Operation_Kind := None;
      Name          : File_Name;
      -- source path for a Move
      Name2         : File_Name;
      -- target path for a Move
      Need_Full_Name : Boolean := False;
      -- mkdir: normalize the name at syscall exit, when the file exists
      Fd_Updatable   : Boolean := False;
      -- open* and creat: the name may be replaced by the path
      -- attached to the returned fd
      Chdir      : Boolean := False;
      Getcwd     : Boolean := False;
      Buf_Addr   : C.long := 0;
   end record;

   type Traced_Proc is record
      Cwd     : File_Name;
      Pending : Pending_Event;
   end record;

   package Proc_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => C_Pid, Element_Type => Traced_Proc);

   Procs : Proc_Maps.Map;
   -- The state of the traced processes

   type Raw_Event is record
      Kind  : Operation_Kind := None;
      Name  : File_Name;
      Name2 : File_Name;
   end record;
   -- The operations observed during a run. The names are final, but the
   -- File_Type is not created at observation time: as the strace text
   -- analyzer did, the file attributes are captured after the run,
   -- when the files are complete.

   package Raw_Event_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Raw_Event);
   Raw_Events : Raw_Event_Lists.List;

   Main_Pid    : C_Pid;
   Live_Count  : Natural;
   Exit_Code   : C.int;
   Initial_Cwd : File_Name;
   Filter_On   : Boolean := False;
   -- True when the traced processes run with the seccomp filter:
   -- the subscribed syscalls then come as seccomp stops, and the
   -- others do not stop at all (the flag is read in /proc at the
   -- first stop, and applies to all the descendants, that inherit
   -- the filter and cannot remove it)

   -- -----------------------------------------------------------------------

   procedure Put_Debug (Msg : String) is
   begin
      IO.Put_Line (Msg, Level => IO.Debug);
   end Put_Debug;

   -- -----------------------------------------------------------------------
   function Pid_Image (Pid : C_Pid) return String is
     (Ada.Strings.Fixed.Trim (Integer'Image (Integer (Pid)),
                              Ada.Strings.Both));

   -- -----------------------------------------------------------------------
   function Read_String (Pid : C_Pid; Addr : C.long) return String is
   -- Read a NUL terminated string from the memory of the traced process

      Buffer : aliased C.char_array (0 .. C.size_t (Max_Path - 1));
      Local  : aliased Iovec :=
        (Base => Buffer (0)'Address, Len => C.size_t (Buffer'Length));
      Remote : aliased Iovec :=
        (Base => Long_To_Addr (Addr), Len => C.size_t (Max_Path));
      Result : C.long;

   begin
      if Addr = 0 then
         return "";
      end if;
      Result := Sys_Process_Vm_Readv
        (Pid, Local'Access, 1, Remote'Access, 1, 0);
      if Result <= 0 then
         return "";
      end if;
      for I in Buffer'Range loop
         if Buffer (I) = C.nul then
            return C.To_Ada (Buffer (0 .. I - 1),
                            Trim_Nul => False);
         end if;
      end loop;
      return "";
      -- no NUL found: the string is longer than Max_Path, give up

   end Read_String;

   -- -----------------------------------------------------------------------
   function Read_Long (Pid : C_Pid; Addr : C.long) return C.long is
   -- Read 64 bits from the memory of the traced process

      Value  : aliased C.long := 0;
      Local  : aliased Iovec := (Value'Address, C.size_t (8));
      Remote : aliased Iovec := (Long_To_Addr (Addr), C.size_t (8));

   begin
      if Addr = 0 then
         return 0;
      end if;
      if Sys_Process_Vm_Readv (Pid, Local'Access, 1, Remote'Access, 1, 0) < 0
      then
         return 0;
      end if;
      return Value;
   end Read_Long;

   -- -----------------------------------------------------------------------
   function Fd_Path (Pid : C_Pid; Fd : C.long) return String is
   -- Return the kernel path of an open file descriptor, that is the
   -- equivalent of the strace -y annotation of dirfd arguments

      Link_Path : constant String :=
        "/proc/" & Pid_Image (Pid)
        & "/fd/" & Ada.Strings.Fixed.Trim (Long_Integer'Image
                                             (Long_Integer (Fd)),
                                           Ada.Strings.Both);
      Link : C.Strings.chars_ptr := C.Strings.New_String (Link_Path);
      Buffer : aliased C.char_array (0 .. C.size_t (Max_Path - 1));
      Length : constant C.long :=
        Sys_Readlink (Link, Buffer (0)'Address, C.size_t (Max_Path));

   begin
      C.Strings.Free (Link);
      if Length <= 0 then
         return "";
      end if;
      return C.To_Ada (Buffer (0 .. C.size_t (Length) - 1),
                      Trim_Nul => False);
   end Fd_Path;

   -- -----------------------------------------------------------------------
   function Add_Dir (To_Name : String; Dir : String) return String is
   -- Prepend Dir to To_Name, unless To_Name is already absolute.
   -- This is the path composition done by the strace text analyzer.

      use Ada.Strings.Maps;

   begin
      if To_Name /= "" and then To_Name (To_Name'First) = '/' then
         return To_Name;

      else
         declare
            Prefix : constant String :=
              (if Dir = "" then ""
               else Ada.Strings.Fixed.Trim
                 (Dir,
                  Left  => Null_Set,
                  Right => To_Set ('/')) & '/');

         begin
            return Prefix & To_Name;
         end;
      end if;
   end Add_Dir;

   -- -----------------------------------------------------------------------
   function Dir_Of (Pid : C_Pid; Dirfd : C.long) return String is
   -- Resolve the dirfd argument of a *at syscall

   begin
      if Dirfd = AT_FDCWD then
         return +Procs.Element (Pid).Cwd;
      else
         return Fd_Path (Pid, Dirfd);
      end if;
   end Dir_Of;

   -- -----------------------------------------------------------------------
   function Seccomp_Filter_On (Pid : C_Pid) return Boolean is
   -- True when the process runs with a seccomp filter installed:
   -- its /proc status shows the seccomp mode 2 (filter)

      Name : constant String := "/proc/" & Pid_Image (Pid) & "/status";
      File : Ada.Text_IO.File_Type;

   begin
      begin
         Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Name);
      exception
         when others =>
            return False;
      end;
      while not Ada.Text_IO.End_Of_File (File) loop
         declare
            Line : constant String := Ada.Text_IO.Get_Line (File);
         begin
            if Line'Length >= 10
              and then Line (Line'First .. Line'First + 7) = "Seccomp:"
              and then Line (Line'Last) = '2'
            then
               Ada.Text_IO.Close (File);
               return True;
            end if;
         end;
      end loop;
      Ada.Text_IO.Close (File);
      return False;
   end Seccomp_Filter_On;

   -- -----------------------------------------------------------------------
   procedure Handle_Entry (Pid : C_Pid;
                           Info : Syscall_Info_Buffer) is
   -- Syscall entry: save the pending event, that will be recorded only
   -- if the syscall succeeds.

      Nr  : constant C.long := Info (3);
      Cwd : constant String := +Procs.Element (Pid).Cwd;
      P   : Pending_Event;

   begin
      if Nr = SC_Open then
         P := (Kind => (if Info (5) mod O_Accmask /= 0 then Write
                        else Read),
               Name => To_Unbounded_String
                 (Add_Dir (Read_String (Pid, Info (4)), Cwd)),
               Fd_Updatable => True,
               others => <>);

      elsif Nr = SC_Openat then
         P := (Kind => (if Info (6) mod O_Accmask /= 0 then Write
                        else Read),
               Name => To_Unbounded_String
                 (Add_Dir (Read_String (Pid, Info (5)),
                           Dir_Of (Pid, Info (4)))),
               Fd_Updatable => True,
               others => <>);

      elsif Nr = SC_Openat2 then
         P := (Kind => (if Read_Long (Pid, Info (6))
                              mod O_Accmask /= 0 then Write
                        else Read),
               Name => To_Unbounded_String
                 (Add_Dir (Read_String (Pid, Info (5)),
                           Dir_Of (Pid, Info (4)))),
               Fd_Updatable => True,
               others => <>);

      elsif Nr = SC_Creat then
         P := (Kind => Write,
               Name => To_Unbounded_String
                 (Add_Dir (Read_String (Pid, Info (4)), Cwd)),
               Fd_Updatable => True,
               others => <>);

      elsif Nr = SC_Mkdir then
         P := (Kind => Write,
               Name => To_Unbounded_String
                 (Add_Dir (Read_String (Pid, Info (4)), Cwd)),
               Need_Full_Name => True,
               others => <>);

      elsif Nr = SC_Rename then
         P := (Kind => Move,
               Name  => To_Unbounded_String
                 (Add_Dir (Read_String (Pid, Info (4)), Cwd)),
               Name2 => To_Unbounded_String
                 (Add_Dir (Read_String (Pid, Info (5)), Cwd)),
               others => <>);

      elsif Nr = SC_Renameat or else Nr = SC_Renameat2 then
         P := (Kind => Move,
               Name  => To_Unbounded_String
                 (Add_Dir (Read_String (Pid, Info (5)),
                           Dir_Of (Pid, Info (4)))),
               Name2 => To_Unbounded_String
                 (Add_Dir (Read_String (Pid, Info (7)),
                           Dir_Of (Pid, Info (6)))),
               others => <>);

      elsif Nr = SC_Chdir then
         P := (Chdir => True,
               Name  => To_Unbounded_String
                 (Read_String (Pid, Info (4))),
               others => <>);

      elsif Nr = SC_Getcwd then
         P := (Getcwd   => True,
               Buf_Addr => Info (4),
               others => <>);

      end if;

      Procs.Reference (Pid).Pending := P;

   end Handle_Entry;

   -- -----------------------------------------------------------------------
   procedure Handle_Exit (Pid  : C_Pid;
                          Rval : C.long) is
   -- Syscall exit: on success, turn the pending event into an operation,
   -- and update the process directory on chdir and getcwd.
   -- Nothing is recorded on error (rval < 0), as strace
   -- -e status=successful did.

      Ref : constant Proc_Maps.Reference_Type :=
        Procs.Reference (Pid);

   begin
      if Ref.Pending.Getcwd and then Rval >= 0 then
         Ref.Cwd := To_Unbounded_String
           (Read_String (Pid, Ref.Pending.Buf_Addr));
         Put_Debug ("cwd of " & Pid_Image (Pid)
                    & " = " & (+Ref.Cwd));

      elsif Ref.Pending.Chdir and then Rval >= 0 then
         Ref.Cwd := To_Unbounded_String
           (Add_Dir (+Ref.Pending.Name, +Ref.Cwd));
         Put_Debug ("cwd of " & Pid_Image (Pid)
                    & " = " & (+Ref.Cwd));

      elsif Ref.Pending.Kind /= None and then Rval >= 0 then
         declare
            Name : File_Name := Ref.Pending.Name;
         begin
            if Ref.Pending.Need_Full_Name then
               -- as the strace text analyzer does on mkdir, normalize
               -- the name now that the file exists
               begin
                  Name := To_Unbounded_String
                    (Ada.Directories.Full_Name (+Name));
               exception
                  when others =>
                     Put_Debug ("Full_Name failed on " & (+Name));
               end;
            end if;
            if Ref.Pending.Fd_Updatable then
               -- prefer the path attached to the returned file
               -- descriptor: it is the kernel truth, as the strace
               -- -y annotation
               declare
                  The_Fd_Path : constant String := Fd_Path (Pid, Rval);
               begin
                  if The_Fd_Path /= "" then
                     Name := To_Unbounded_String (The_Fd_Path);
                  end if;
               end;
            end if;

            case Ref.Pending.Kind is
               when Read =>
                  Raw_Events.Append
                    ((Kind => Read, Name => Name, others => <>));
                  Put_Debug ("Read open " & (+Name));

               when Write =>
                  Raw_Events.Append
                    ((Kind => Write, Name => Name, others => <>));
                  Put_Debug ("Write " & (+Name));

               when Move =>
                  Raw_Events.Append
                    ((Kind  => Move,
                      Name  => Name,
                      Name2 => Ref.Pending.Name2));
                  Put_Debug ("Move " & (+Name)
                             & " into " & (+Ref.Pending.Name2));

               when others =>
                  null;
            end case;
         end;
      end if;

      Ref.Pending := (others => <>);

   end Handle_Exit;

   -- -----------------------------------------------------------------------
   procedure Resume_Syscall (Pid : C_Pid; Sig : C.long) is
   -- Resume the process, stopping at the next syscall entry or exit

      Result : constant C.long := Sys_Ptrace (PTRACE_SYSCALL, Pid, 0, Sig);

   begin
      if Result < 0 then
         Put_Debug ("resume of " & Pid_Image (Pid) & " failed");
      end if;
   end Resume_Syscall;

   -- -----------------------------------------------------------------------
   procedure Resume_Cont (Pid : C_Pid; Sig : C.long) is
   -- Resume the process without syscall tracing: when the seccomp
   -- filter is on, the next subscribed syscall stops it anyway

      Result : constant C.long := Sys_Ptrace (PTRACE_CONT, Pid, 0, Sig);

   begin
      if Result < 0 then
         Put_Debug ("resume of " & Pid_Image (Pid) & " failed");
      end if;
   end Resume_Cont;

   -- -----------------------------------------------------------------------
   procedure Resume_Next (Pid : C_Pid; Sig : C.long) is
   -- Resume the process up to the next observation point: with the
   -- seccomp filter on, the subscribed syscalls trap on their own;
   -- without it, the next syscall must be caught by PTRACE_SYSCALL

   begin
      if Filter_On then
         Resume_Cont (Pid, Sig);
      else
         Resume_Syscall (Pid, Sig);
      end if;
   end Resume_Next;

   -- -----------------------------------------------------------------------
   procedure Handle_Syscall_Stop (Pid : C_Pid) is

      Info   : aliased Syscall_Info_Buffer;
      Result : constant C.long :=
        Sys_Ptrace (PTRACE_GET_SYSCALL_INFO, Pid,
                    C.long (Info'Length) * 8, Addr_To_Long (Info'Address));

   begin
      if Result < 0 then
         return;
      end if;
      -- the entry comes either as a syscall stop (op ENTRY), or, when
      -- the seccomp filter is installed, as a seccomp stop (op SECCOMP);
      -- both give the syscall number and arguments
      if Integer (Info (0) mod 256) = PTRACE_SYSCALL_INFO_ENTRY
        or else Integer (Info (0) mod 256) = PTRACE_SYSCALL_INFO_SECCOMP
      then
         Handle_Entry (Pid, Info);
         -- run the syscall, stopping at its exit for the return value
         Resume_Syscall (Pid, 0);

      else
         Handle_Exit (Pid, Info (3));
         -- with the seccomp filter on, the next subscribed syscall
         -- will trap on its own: resume without syscall tracing;
         -- without it, chain PTRACE_SYSCALL to catch the next entry
         if Filter_On then
            Resume_Cont (Pid, 0);
         else
            Resume_Syscall (Pid, 0);
         end if;
      end if;
   end Handle_Syscall_Stop;



   -- -----------------------------------------------------------------------
   procedure Handle_Fork (Pid : C_Pid) is
   -- The traced process Pid stopped on a fork/vfork/clone event: the
   -- new child is auto-attached by the kernel, its pid is given by
   -- PTRACE_GETEVENTMSG, and it inherits the state of its parent.

      Child  : aliased C.unsigned_long := 0;
      Result : constant C.long :=
        Sys_Ptrace (PTRACE_GETEVENTMSG, Pid, 0,
                    Addr_To_Long (Child'Address));

   begin
      if Result >= 0 and then Child < 2 ** 31 then
         declare
            Child_Pid : constant C_Pid := C_Pid (Child);
         begin
            Procs.Include
              (Child_Pid,
               (Cwd     => Procs.Element (Pid).Cwd,
                Pending => (others => <>)));
            Live_Count := Live_Count + 1;
            Put_Debug ("following child " & Pid_Image (Child_Pid)
                       & " of " & Pid_Image (Pid));
         end;
      end if;
   end Handle_Fork;

   -- -----------------------------------------------------------------------
   procedure Handle_Stop (Pid    : C_Pid;
                          Status : C.int) is
   -- Handle one waitpid result:
   --  - exited   : low byte 0, the exit code in the next byte;
   --  - stopped  : low byte 16#7F#, the stop signal in the next byte,
   --                and the ptrace event in the third byte;
   --  - signaled : the low byte is the signal (with 16#80# for a core).

      U   : constant C.unsigned := C.unsigned (Status);
      Low : constant C.unsigned := U and 16#FF#;
      Sig : constant C.unsigned := (U / 256) and 16#FF#;
      Ev  : constant C.unsigned := (U / 65536) and 16#FF#;

   begin
      if Low = 0 then
         -- the process exited, Sig is the exit code
         if Pid = Main_Pid then
            Exit_Code := C.int (Sig);
         end if;
         Live_Count := Live_Count - 1;
         if Procs.Contains (Pid) then
            Procs.Delete (Pid);
         end if;

      elsif Low = 16#7F# then
         -- the process stopped, Sig is the stop signal
         if not Procs.Contains (Pid) then
            -- first stop of an auto-attached child
            Procs.Include (Pid, (Cwd     => Initial_Cwd,
                                 Pending => (others => <>)));
            Live_Count := Live_Count + 1;
            -- Fixme: inherit the state of its parent; the fork event
            -- normally gives the child before this stop, so this is
            -- defensive code
         end if;

         if Sig = Sigtrap_Syscall then
            Handle_Syscall_Stop (Pid);

         elsif Sig = Sig_Trap and then Ev = PTRACE_EVENT_FORK then
            Handle_Fork (Pid);
            Resume_Next (Pid, 0);

         elsif Sig = Sig_Trap and then Ev = PTRACE_EVENT_VFORK then
            Handle_Fork (Pid);
            Resume_Next (Pid, 0);

         elsif Sig = Sig_Trap and then Ev = PTRACE_EVENT_CLONE then
            Handle_Fork (Pid);
            Resume_Next (Pid, 0);

         elsif Sig = Sig_Trap and then Ev = PTRACE_EVENT_SECCOMP then
            -- entry of a subscribed syscall
            Handle_Syscall_Stop (Pid);

         elsif Sig = Sig_Trap and then Ev = PTRACE_EVENT_EXEC then
            Resume_Next (Pid, 0);

         elsif Sig = Sig_Trap and then Ev = PTRACE_EVENT_VFORK_DONE then
            Resume_Next (Pid, 0);

         elsif Sig = Sig_Stop or else Sig = Sig_Tstp
           or else Sig = Sig_Ttin or else Sig = Sig_Ttou
         then
            -- the group stop and job control signals are never
            -- delivered back, to avoid infinite stop / deliver loops
            Resume_Next (Pid, 0);

         else
            -- deliver the signal
            Resume_Next (Pid, C.long (Sig));
         end if;

      else
         -- the process was terminated by a signal
         if Pid = Main_Pid then
            Exit_Code := 128 + C.int (Low and 16#7F#);
         end if;
         Live_Count := Live_Count - 1;
         if Procs.Contains (Pid) then
            Procs.Delete (Pid);
         end if;
      end if;
   end Handle_Stop;

   -- -----------------------------------------------------------------------
   procedure Install_Seccomp_Filter is

      -- the program layout is:
      --   0 : load the architecture
      --   1 : jump to the ALLOW instruction if not x86_64
      --   2 : load the syscall number
      --   3, 5, ... : one comparison per subscribed syscall, jumping
      --               to the TRACE instruction that follows it
      --   last : ALLOW
      Program : array (0 .. 3 + 2 * Subscribed_Syscalls'Length)
        of Sock_Filter;
      Fprog : aliased Sock_Fprog;

   begin
      Program (0) := (Code => Bpf_Ld_W_Abs,
                      Jt    => 0,
                      Jf    => 0,
                      K     => Seccomp_Data_Arch);
      Program (1) := (Code => Bpf_Jeq_K,
                      Jt    => 0,
                      Jf    => C.unsigned_char
                                 (1 + 2 * Subscribed_Syscalls'Length),
                      K     => Audit_Arch_X86_64);
      Program (2) := (Code => Bpf_Ld_W_Abs,
                      Jt    => 0,
                      Jf    => 0,
                      K     => Seccomp_Data_Nr);
      for I in Subscribed_Syscalls'Range loop
         declare
            Base : constant Integer
              := 3 + 2 * (I - Subscribed_Syscalls'First);
         begin
            Program (Base) :=
              (Code => Bpf_Jeq_K,
               Jt   => 0,
               Jf   => 1,
               K    => Subscribed_Syscalls (I));
            Program (Base + 1) :=
              (Code => Bpf_Ret_K,
               Jt   => 0,
               Jf   => 0,
               K    => Seccomp_Ret_Trace);
         end;
      end loop;
      Program (Program'Last) := (Code => Bpf_Ret_K,
                                 Jt    => 0,
                                 Jf    => 0,
                                 K     => Seccomp_Ret_Allow);

      if Sys_Prctl (PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) < 0 then
         return;
      end if;
      Fprog.Len    := C.unsigned_short (Program'Length);
      Fprog.Filter := Program (Program'First)'Address;
      if Sys_Prctl (PR_SET_SECCOMP, SECCOMP_MODE_FILTER,
                    C.unsigned_long (Addr_To_Long (Fprog'Address)),
                    0, 0) < 0
      then
         null;
         -- the filter could not be installed (kernel < 3.5?):
         -- proceed without it, every syscall will stop
      end if;
   end Install_Seccomp_Filter;

   -- -----------------------------------------------------------------------
   procedure Observe (Cmd     : in     Command_Lines;
                      Events  :    out Operation_Lists.List;
                      Success :    out Boolean) is

      Initial_Dir : constant String :=
        Ada.Directories.Current_Directory;

      Shell_Ptr : C.Strings.chars_ptr :=
        C.Strings.New_String (Settings.Shell_Cmd);
      Argv : aliased C.Strings.chars_ptr_array (0 .. 3);

   begin
      Procs.Clear;
      Raw_Events.Clear;
      Exit_Code   := 1;
      Live_Count  := 0;
      Filter_On   := False;
      Initial_Cwd := To_Unbounded_String
        (Ada.Directories.Full_Name (Settings.Run_Dir_Name));

      Ada.Directories.Set_Directory (Settings.Run_Dir_Name);
      -- Fixme: ensure returning to Initial_Dir even in case of exception?

      Argv (0) := C.Strings.New_String (Settings.Shell_Cmd);
      Argv (1) := C.Strings.New_String ("-c");
      Argv (2) := C.Strings.New_String (+Cmd);
      Argv (3) := C.Strings.Null_Ptr;

      declare
         Pid : constant C_Pid := Sys_Fork;
      begin
         if Pid = 0 then
            -- the child: trace itself, subscribe to the file
            -- syscalls, then run the command
            if Sys_Ptrace (PTRACE_TRACEME, 0, 0, 0) < 0 then
               Sys_Exit (127);
            end if;
            Install_Seccomp_Filter;
            Sys_Execvp (Shell_Ptr, Argv);
            Sys_Exit (127);

         else
            -- the parent traces the child
            Main_Pid := Pid;
            Put_Debug ("fork result " & Pid_Image (Pid));

            if Pid < 0 then
               IO.Put_Error (Msg => "fork failed");

            else
               declare
                  Status : aliased C.int := 0;
                  Result : constant C_Pid :=
                    Sys_Waitpid (Main_Pid, Status'Access, 0);
                  -- first stop of the child, after the exec of the shell
               begin
                  if Result < 0 then
                     IO.Put_Error (Msg => "waitpid failed");

                  elsif Sys_Ptrace (PTRACE_SETOPTIONS, Main_Pid, 0,
                                   C.long (Trace_Options)) < 0
                  then
                     IO.Put_Error (Msg => "ptrace setoptions failed");

                  else
                     -- the child installed the seccomp filter before
                     -- exec: check it here, at the first stop
                     Filter_On := Seccomp_Filter_On (Main_Pid);
                     Put_Debug (if Filter_On then "seccomp filter on"
                                else "seccomp filter off");
                     Procs.Include
                       (Main_Pid,
                        (Cwd     => Initial_Cwd,
                         Pending => (others => <>)));
                     Live_Count := 1;
                     Resume_Next (Main_Pid, 0);

                     while Live_Count > 0 loop
                        declare
                           Status : aliased C.int := 0;
                           Result : constant C_Pid :=
                             Sys_Waitpid (-1, Status'Access, W_All);
                        begin
                           exit when Result < 0;
                           Handle_Stop (Result, Status);
                        end;
                     end loop;
                  end if;
               end;
            end if;
         end if;
      end;

      -- Turn the raw observations into events; this is done after the
      -- run, when the files are complete, as the strace text analyzer
      -- was analyzing the trace after the run.
      for E of Raw_Events loop
         case E.Kind is
            when Read =>
               Events.Append
                 ((Kind => Read,
                   Name => E.Name,
                   File => Create (File => E.Name, Role => Source)));
            when Write =>
               Events.Append
                 ((Kind => Write,
                   Name => E.Name,
                   File => Create (File => E.Name, Role => Target)));
            when Move =>
               Events.Append
                 ((Kind        => Move,
                   Source_Name => E.Name,
                   Source      => Create (File => E.Name,
                                         Role => Source),
                   Target_Name => E.Name2,
                   Target      => Create (File => E.Name2,
                                         Role => Target)));
            when others =>
               null;
         end case;
      end loop;

      -- free the strings, the child kept its own copy
      for I in Argv'Range loop
         if Argv (I) /= C.Strings.Null_Ptr then
            C.Strings.Free (Argv (I));
         end if;
      end loop;
      C.Strings.Free (Shell_Ptr);

      Ada.Directories.Set_Directory (Initial_Dir);

      Success := Exit_Code = 0;
      if not Success then
         IO.Put_Error (Msg => "Spawn failed for " & (+Cmd));
      end if;

   end Observe;

end Smk.Runs.Observers.Ptrace;
