pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with asm_generic_int_ll64_h;

package linux_ptrace_h is

   PTRACE_TRACEME : constant := 0;  --  /usr/include/linux/ptrace.h:11
   PTRACE_PEEKTEXT : constant := 1;  --  /usr/include/linux/ptrace.h:12
   PTRACE_PEEKDATA : constant := 2;  --  /usr/include/linux/ptrace.h:13
   PTRACE_PEEKUSR : constant := 3;  --  /usr/include/linux/ptrace.h:14
   PTRACE_POKETEXT : constant := 4;  --  /usr/include/linux/ptrace.h:15
   PTRACE_POKEDATA : constant := 5;  --  /usr/include/linux/ptrace.h:16
   PTRACE_POKEUSR : constant := 6;  --  /usr/include/linux/ptrace.h:17
   PTRACE_CONT : constant := 7;  --  /usr/include/linux/ptrace.h:18
   PTRACE_KILL : constant := 8;  --  /usr/include/linux/ptrace.h:19
   PTRACE_SINGLESTEP : constant := 9;  --  /usr/include/linux/ptrace.h:20

   PTRACE_ATTACH : constant := 16;  --  /usr/include/linux/ptrace.h:22
   PTRACE_DETACH : constant := 17;  --  /usr/include/linux/ptrace.h:23

   PTRACE_SYSCALL : constant := 24;  --  /usr/include/linux/ptrace.h:25

   PTRACE_SETOPTIONS : constant := 16#4200#;  --  /usr/include/linux/ptrace.h:28
   PTRACE_GETEVENTMSG : constant := 16#4201#;  --  /usr/include/linux/ptrace.h:29
   PTRACE_GETSIGINFO : constant := 16#4202#;  --  /usr/include/linux/ptrace.h:30
   PTRACE_SETSIGINFO : constant := 16#4203#;  --  /usr/include/linux/ptrace.h:31

   PTRACE_GETREGSET : constant := 16#4204#;  --  /usr/include/linux/ptrace.h:50
   PTRACE_SETREGSET : constant := 16#4205#;  --  /usr/include/linux/ptrace.h:51

   PTRACE_SEIZE : constant := 16#4206#;  --  /usr/include/linux/ptrace.h:53
   PTRACE_INTERRUPT : constant := 16#4207#;  --  /usr/include/linux/ptrace.h:54
   PTRACE_LISTEN : constant := 16#4208#;  --  /usr/include/linux/ptrace.h:55

   PTRACE_PEEKSIGINFO : constant := 16#4209#;  --  /usr/include/linux/ptrace.h:57

   PTRACE_GETSIGMASK : constant := 16#420a#;  --  /usr/include/linux/ptrace.h:65
   PTRACE_SETSIGMASK : constant := 16#420b#;  --  /usr/include/linux/ptrace.h:66

   PTRACE_SECCOMP_GET_FILTER : constant := 16#420c#;  --  /usr/include/linux/ptrace.h:68
   PTRACE_SECCOMP_GET_METADATA : constant := 16#420d#;  --  /usr/include/linux/ptrace.h:69

   PTRACE_PEEKSIGINFO_SHARED : constant := (2 ** 0);  --  /usr/include/linux/ptrace.h:77

   PTRACE_EVENT_FORK : constant := 1;  --  /usr/include/linux/ptrace.h:80
   PTRACE_EVENT_VFORK : constant := 2;  --  /usr/include/linux/ptrace.h:81
   PTRACE_EVENT_CLONE : constant := 3;  --  /usr/include/linux/ptrace.h:82
   PTRACE_EVENT_EXEC : constant := 4;  --  /usr/include/linux/ptrace.h:83
   PTRACE_EVENT_VFORK_DONE : constant := 5;  --  /usr/include/linux/ptrace.h:84
   PTRACE_EVENT_EXIT : constant := 6;  --  /usr/include/linux/ptrace.h:85
   PTRACE_EVENT_SECCOMP : constant := 7;  --  /usr/include/linux/ptrace.h:86

   PTRACE_EVENT_STOP : constant := 128;  --  /usr/include/linux/ptrace.h:88

   PTRACE_O_TRACESYSGOOD : constant := 1;  --  /usr/include/linux/ptrace.h:91
   --  unsupported macro: PTRACE_O_TRACEFORK (1 << PTRACE_EVENT_FORK)
   --  unsupported macro: PTRACE_O_TRACEVFORK (1 << PTRACE_EVENT_VFORK)
   --  unsupported macro: PTRACE_O_TRACECLONE (1 << PTRACE_EVENT_CLONE)
   --  unsupported macro: PTRACE_O_TRACEEXEC (1 << PTRACE_EVENT_EXEC)
   --  unsupported macro: PTRACE_O_TRACEVFORKDONE (1 << PTRACE_EVENT_VFORK_DONE)
   --  unsupported macro: PTRACE_O_TRACEEXIT (1 << PTRACE_EVENT_EXIT)
   --  unsupported macro: PTRACE_O_TRACESECCOMP (1 << PTRACE_EVENT_SECCOMP)

   PTRACE_O_EXITKILL : constant := (2 ** 20);  --  /usr/include/linux/ptrace.h:101
   PTRACE_O_SUSPEND_SECCOMP : constant := (2 ** 21);  --  /usr/include/linux/ptrace.h:102
   --  unsupported macro: PTRACE_O_MASK ( 0x000000ff | PTRACE_O_EXITKILL | PTRACE_O_SUSPEND_SECCOMP)

  -- SPDX-License-Identifier: GPL-2.0 WITH Linux-syscall-note  
  -- ptrace.h  
  -- structs and defines to help the user use the ptrace system call.  
  -- has the defines to get at the registers.  
  -- 0x4200-0x4300 are reserved for architecture-independent additions.   
  -- * Generic ptrace interface that exports the architecture specific regsets
  -- * using the corresponding NT_* types (which are also used in the core dump).
  -- * Please note that the NT_PRSTATUS note type in a core dump contains a full
  -- * 'struct elf_prstatus'. But the user_regset for NT_PRSTATUS contains just the
  -- * elf_gregset_t that is the pr_reg field of 'struct elf_prstatus'. For all the
  -- * other user_regset flavors, the user_regset layout and the ELF core dump note
  -- * payload are exactly the same layout.
  -- *
  -- * This interface usage is as follows:
  -- *	struct iovec iov = { buf, len};
  -- *
  -- *	ret = ptrace(PTRACE_GETREGSET/PTRACE_SETREGSET, pid, NT_XXX_TYPE, &iov);
  -- *
  -- * On the successful completion, iov.len will be updated by the kernel,
  -- * specifying how much the kernel has written/read to/from the user's iov.buf.
  --  

  -- from which siginfo to start  
   type ptrace_peeksiginfo_args is record
      off : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/ptrace.h:60
      flags : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/ptrace.h:61
      nr : aliased asm_generic_int_ll64_h.uu_s32;  -- /usr/include/linux/ptrace.h:62
   end record;
   pragma Convention (C_Pass_By_Copy, ptrace_peeksiginfo_args);  -- /usr/include/linux/ptrace.h:59

  -- how may siginfos to take  
  -- Input: which filter  
   type seccomp_metadata is record
      filter_off : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/ptrace.h:72
      flags : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/ptrace.h:73
   end record;
   pragma Convention (C_Pass_By_Copy, seccomp_metadata);  -- /usr/include/linux/ptrace.h:71

  -- Output: filter's flags  
  -- Read signals from a shared (process wide) queue  
  -- Wait extended result codes for the above trace options.   
  -- Extended result codes which enabled by means other than options.   
  -- Options set using PTRACE_SETOPTIONS or using PTRACE_SEIZE @data param  
  -- eventless options  
end linux_ptrace_h;
