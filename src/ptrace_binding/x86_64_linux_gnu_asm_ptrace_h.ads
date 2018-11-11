pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package x86_64_linux_gnu_asm_ptrace_h is

  -- SPDX-License-Identifier: GPL-2.0 WITH Linux-syscall-note  
  -- For  
  -- this struct defines the way the registers are stored on the
  --   stack during a system call.  

  -- * C ABI says these regs are callee-preserved. They aren't saved on kernel entry
  -- * unless syscall needs a complete, fully filled "struct pt_regs".
  --  

   type pt_regs is record
      r15 : aliased unsigned_long;  -- /usr/include/x86_64-linux-gnu/asm/ptrace.h:46
      r14 : aliased unsigned_long;  -- /usr/include/x86_64-linux-gnu/asm/ptrace.h:47
      r13 : aliased unsigned_long;  -- /usr/include/x86_64-linux-gnu/asm/ptrace.h:48
      r12 : aliased unsigned_long;  -- /usr/include/x86_64-linux-gnu/asm/ptrace.h:49
      rbp : aliased unsigned_long;  -- /usr/include/x86_64-linux-gnu/asm/ptrace.h:50
      rbx : aliased unsigned_long;  -- /usr/include/x86_64-linux-gnu/asm/ptrace.h:51
      r11 : aliased unsigned_long;  -- /usr/include/x86_64-linux-gnu/asm/ptrace.h:53
      r10 : aliased unsigned_long;  -- /usr/include/x86_64-linux-gnu/asm/ptrace.h:54
      r9 : aliased unsigned_long;  -- /usr/include/x86_64-linux-gnu/asm/ptrace.h:55
      r8 : aliased unsigned_long;  -- /usr/include/x86_64-linux-gnu/asm/ptrace.h:56
      rax : aliased unsigned_long;  -- /usr/include/x86_64-linux-gnu/asm/ptrace.h:57
      rcx : aliased unsigned_long;  -- /usr/include/x86_64-linux-gnu/asm/ptrace.h:58
      rdx : aliased unsigned_long;  -- /usr/include/x86_64-linux-gnu/asm/ptrace.h:59
      rsi : aliased unsigned_long;  -- /usr/include/x86_64-linux-gnu/asm/ptrace.h:60
      rdi : aliased unsigned_long;  -- /usr/include/x86_64-linux-gnu/asm/ptrace.h:61
      orig_rax : aliased unsigned_long;  -- /usr/include/x86_64-linux-gnu/asm/ptrace.h:66
      rip : aliased unsigned_long;  -- /usr/include/x86_64-linux-gnu/asm/ptrace.h:68
      cs : aliased unsigned_long;  -- /usr/include/x86_64-linux-gnu/asm/ptrace.h:69
      eflags : aliased unsigned_long;  -- /usr/include/x86_64-linux-gnu/asm/ptrace.h:70
      rsp : aliased unsigned_long;  -- /usr/include/x86_64-linux-gnu/asm/ptrace.h:71
      ss : aliased unsigned_long;  -- /usr/include/x86_64-linux-gnu/asm/ptrace.h:72
   end record;
   pragma Convention (C_Pass_By_Copy, pt_regs);  -- /usr/include/x86_64-linux-gnu/asm/ptrace.h:41

  -- These regs are callee-clobbered. Always saved on kernel entry.  
  -- * On syscall entry, this is syscall#. On CPU exception, this is error code.
  -- * On hw interrupt, it's IRQ number:
  --  

  -- Return frame for iretq  
  -- top of stack page  
end x86_64_linux_gnu_asm_ptrace_h;
