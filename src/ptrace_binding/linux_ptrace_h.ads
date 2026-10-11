pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with asm_generic_int_ll64_h;

package linux_ptrace_h is

   -- Basic ptrace requests
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

   -- 0x4200-0x4300 are reserved for architecture-independent additions.
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

   -- Syscall info requests (added after 2018)
   PTRACE_GET_SYSCALL_INFO : constant := 16#420e#;  --  /usr/include/linux/ptrace.h:76
   PTRACE_SET_SYSCALL_INFO : constant := 16#4212#;  --  /usr/include/linux/ptrace.h:77
   PTRACE_SYSCALL_INFO_NONE : constant := 0;  --  /usr/include/linux/ptrace.h:78
   PTRACE_SYSCALL_INFO_ENTRY : constant := 1;  --  /usr/include/linux/ptrace.h:79
   PTRACE_SYSCALL_INFO_EXIT : constant := 2;  --  /usr/include/linux/ptrace.h:80
   PTRACE_SYSCALL_INFO_SECCOMP : constant := 3;  --  /usr/include/linux/ptrace.h:81

   -- RSEQ configuration (added after 2018)
   PTRACE_GET_RSEQ_CONFIGURATION : constant := 16#420f#;  --  /usr/include/linux/ptrace.h:108

   -- Syscall User Dispatch configuration (added after 2018)
   PTRACE_SET_SYSCALL_USER_DISPATCH_CONFIG : constant := 16#4210#;  --  /usr/include/linux/ptrace.h:118
   PTRACE_GET_SYSCALL_USER_DISPATCH_CONFIG : constant := 16#4211#;  --  /usr/include/linux/ptrace.h:119

   -- Event message types for syscall stops
   PTRACE_EVENTMSG_SYSCALL_ENTRY : constant := 1;  --  /usr/include/linux/ptrace.h:154
   PTRACE_EVENTMSG_SYSCALL_EXIT : constant := 2;  --  /usr/include/linux/ptrace.h:155

   -- Peek siginfo flags
   PTRACE_PEEKSIGINFO_SHARED : constant := (2 ** 0);  --  /usr/include/linux/ptrace.h:158

   -- Event types
   PTRACE_EVENT_FORK : constant := 1;  --  /usr/include/linux/ptrace.h:161
   PTRACE_EVENT_VFORK : constant := 2;  --  /usr/include/linux/ptrace.h:162
   PTRACE_EVENT_CLONE : constant := 3;  --  /usr/include/linux/ptrace.h:163
   PTRACE_EVENT_EXEC : constant := 4;  --  /usr/include/linux/ptrace.h:164
   PTRACE_EVENT_VFORK_DONE : constant := 5;  --  /usr/include/linux/ptrace.h:165
   PTRACE_EVENT_EXIT : constant := 6;  --  /usr/include/linux/ptrace.h:166
   PTRACE_EVENT_SECCOMP : constant := 7;  --  /usr/include/linux/ptrace.h:167

   PTRACE_EVENT_STOP : constant := 128;  --  /usr/include/linux/ptrace.h:169

   -- Options for PTRACE_SETOPTIONS
   PTRACE_O_TRACESYSGOOD : constant := 1;  --  /usr/include/linux/ptrace.h:172

   -- Trace event options (previously unsupported macros, now explicitly defined)
   PTRACE_O_TRACEFORK : constant := (2 ** PTRACE_EVENT_FORK);
   PTRACE_O_TRACEVFORK : constant := (2 ** PTRACE_EVENT_VFORK);
   PTRACE_O_TRACECLONE : constant := (2 ** PTRACE_EVENT_CLONE);
   PTRACE_O_TRACEEXEC : constant := (2 ** PTRACE_EVENT_EXEC);
   PTRACE_O_TRACEVFORKDONE : constant := (2 ** PTRACE_EVENT_VFORK_DONE);
   PTRACE_O_TRACEEXIT : constant := (2 ** PTRACE_EVENT_EXIT);
   PTRACE_O_TRACESECCOMP : constant := (2 ** PTRACE_EVENT_SECCOMP);

   -- Eventless options
   PTRACE_O_EXITKILL : constant := (2 ** 20);  --  /usr/include/linux/ptrace.h:182
   PTRACE_O_SUSPEND_SECCOMP : constant := (2 ** 21);  --  /usr/include/linux/ptrace.h:183
   -- PTRACE_O_MASK = 0x000000ff | PTRACE_O_EXITKILL | PTRACE_O_SUSPEND_SECCOMP = 3145983
   PTRACE_O_MASK : constant := 3145983;

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
  -- * other user regset flavors, the user_regset layout and the ELF core dump note
  -- * payload are exactly the same layout.
  -- *
  -- * This interface usage is as follows:
  -- *\tstruct iovec iov = { buf, len};
  -- *
  -- *\tret = ptrace(PTRACE_GETREGSET/PTRACE_SETREGSET, pid, NT_XXX_TYPE, &iov);
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

  -- Input: which filter  
  -- Output: filter's flags  
   type seccomp_metadata is record
      filter_off : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/ptrace.h:72
      flags : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/ptrace.h:73
   end record;
   pragma Convention (C_Pass_By_Copy, seccomp_metadata);  -- /usr/include/linux/ptrace.h:71

   -- Syscall info structures for PTRACE_GET_SYSCALL_INFO
   -- This is the key structure for D1: it provides direct access to syscall
   -- number, arguments, and return values without manual register decoding
   type syscall_args_array is array (1 .. 6) of asm_generic_int_ll64_h.uu_u64;
   
   type ptrace_syscall_info_entry_type is record
      nr : asm_generic_int_ll64_h.uu_u64;  -- syscall number
      args : syscall_args_array;  -- syscall arguments
   end record;
   pragma Convention (C_Pass_By_Copy, ptrace_syscall_info_entry_type);

   type ptrace_syscall_info_exit_type is record
      rval : asm_generic_int_ll64_h.uu_s64;  -- return value
      is_error : asm_generic_int_ll64_h.uu_u8;  -- non-zero if error
   end record;
   pragma Convention (C_Pass_By_Copy, ptrace_syscall_info_exit_type);

   type ptrace_syscall_info_seccomp_type is record
      nr : asm_generic_int_ll64_h.uu_u64;  -- syscall number
      args : syscall_args_array;  -- syscall arguments
      ret_data : asm_generic_int_ll64_h.uu_u32;  -- seccomp return data
      reserved2 : asm_generic_int_ll64_h.uu_u32;  -- reserved
   end record;
   pragma Convention (C_Pass_By_Copy, ptrace_syscall_info_seccomp_type);

   -- Main syscall info structure
   -- Note: We use a simple record without variant part to avoid Ada discriminant issues
   -- Users should check the 'op' field and cast to the appropriate type
   type ptrace_syscall_info is record
      op : asm_generic_int_ll64_h.uu_u8;  -- PTRACE_SYSCALL_INFO_*
      reserved : asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/ptrace.h:85
      flags : asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/ptrace.h:86
      arch : asm_generic_int_ll64_h.uu_u32;  -- architecture
      instruction_pointer : asm_generic_int_ll64_h.uu_u64;  -- instruction pointer
      stack_pointer : asm_generic_int_ll64_h.uu_u64;  -- stack pointer
   end record;
   pragma Convention (C_Pass_By_Copy, ptrace_syscall_info);

   -- RSEQ (Restartable Sequences) configuration
   type ptrace_rseq_configuration is record
      rseq_abi_pointer : asm_generic_int_ll64_h.uu_u64;  -- pointer to rseq ABI
      rseq_abi_size : asm_generic_int_ll64_h.uu_u32;  -- size of rseq ABI
      signature : asm_generic_int_ll64_h.uu_u32;  -- signature
      flags : asm_generic_int_ll64_h.uu_u32;  -- flags
      pad : asm_generic_int_ll64_h.uu_u32;  -- padding
   end record;
   pragma Convention (C_Pass_By_Copy, ptrace_rseq_configuration);

  -- * struct ptrace_sud_config - Per-task configuration for Syscall User Dispatch
  -- * @mode:\tOne of PR_SYS_DISPATCH_ON or PR_SYS_DISPATCH_OFF
  -- * @selector:\tTracees user virtual address of SUD selector
  -- * @offset:\tSUD exclusion area (virtual address)
  -- * @len:\tLength of SUD exclusion area
  -- *
  -- * Used to get/set the syscall user dispatch configuration for a tracee.
  -- * Selector is optional (may be NULL), and if invalid will produce
  -- * a SIGSEGV in the tracee upon first access.
  -- *
  -- * If mode is PR_SYS_DISPATCH_ON, syscall dispatch will be enabled. If
  -- * PR_SYS_DISPATCH_OFF, syscall dispatch will be disabled and all other
  -- * parameters must be 0.  The value in *selector (if not null), also determines
  -- * whether syscall dispatch will occur.
  -- *
  -- * The Syscall User Dispatch Exclusion area described by offset/len is the
  -- * virtual address space from which syscalls will not produce a user
  -- * dispatch.
  --  

   type ptrace_sud_config is record
      mode : asm_generic_int_ll64_h.uu_u64;  -- dispatch mode
      selector : asm_generic_int_ll64_h.uu_u64;  -- selector address
      offset : asm_generic_int_ll64_h.uu_u64;  -- exclusion area offset
      len : asm_generic_int_ll64_h.uu_u64;  -- exclusion area length
   end record;
   pragma Convention (C_Pass_By_Copy, ptrace_sud_config);

  -- 0x4212 is PTRACE_SET_SYSCALL_INFO  
  -- * These values are stored in task->ptrace_message
  -- * by ptrace_stop to describe the current syscall-stop.
  --  

  -- Read signals from a shared (process wide) queue  
  -- Wait extended result codes for the above trace options.   
  -- Extended result codes which enabled by means other than options.   
  -- Options set using PTRACE_SETOPTIONS or using PTRACE_SEIZE @data param  
  -- eventless options  
end linux_ptrace_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
