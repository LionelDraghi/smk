pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with asm_generic_int_ll64_h;

package linux_types_h is

  -- SPDX-License-Identifier: GPL-2.0 WITH Linux-syscall-note  
  -- * Below are truly Linux-specific types that should never collide with
  -- * any application/library that wants linux/types.h.
  --  

   subtype uu_le16 is asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/types.h:24

   subtype uu_be16 is asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/types.h:25

   subtype uu_le32 is asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/types.h:26

   subtype uu_be32 is asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/types.h:27

   subtype uu_le64 is asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/types.h:28

   subtype uu_be64 is asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/types.h:29

   subtype uu_sum16 is asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/types.h:31

   subtype uu_wsum is asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/types.h:32

  -- * aligned_u64 should be used in defining kernel<->userspace ABIs to avoid
  -- * common 32/64-bit compat problems.
  -- * 64-bit values align to 4-byte boundaries on x86_32 (and possibly other
  -- * architectures) and to 8-byte boundaries on 64-bit architectures.  The new
  -- * aligned_64 type enforces 8-byte alignment so that structs containing
  -- * aligned_64 values have the same alignment on 32-bit and 64-bit architectures.
  -- * No conversions are necessary between 32-bit user-space and a 64-bit kernel.
  --  

   subtype uu_poll_t is unsigned;  -- /usr/include/linux/types.h:47

end linux_types_h;
