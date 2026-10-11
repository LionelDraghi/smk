pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with asm_generic_int_ll64_h;

package linux_types_h is

  -- SPDX-License-Identifier: GPL-2.0 WITH Linux-syscall-note  
   subtype uu_s128 is Extensions.Signed_128;  -- /usr/include/linux/types.h:12

   subtype uu_u128 is Extensions.Unsigned_128;  -- /usr/include/linux/types.h:13

  -- * Below are truly Linux-specific types that should never collide with
  -- * any application/library that wants linux/types.h.
  --  

  -- sparse defines __CHECKER__; see Documentation/dev-tools/sparse.rst  
  -- The kernel doesn't use this legacy form, but user space does  
   subtype uu_le16 is asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/types.h:31

   subtype uu_be16 is asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/types.h:32

   subtype uu_le32 is asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/types.h:33

   subtype uu_be32 is asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/types.h:34

   subtype uu_le64 is asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/types.h:35

   subtype uu_be64 is asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/types.h:36

   subtype uu_sum16 is asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/types.h:38

   subtype uu_wsum is asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/types.h:39

  -- * aligned_u64 should be used in defining kernel<->userspace ABIs to avoid
  -- * common 32/64-bit compat problems.
  -- * 64-bit values align to 4-byte boundaries on x86_32 (and possibly other
  -- * architectures) and to 8-byte boundaries on 64-bit architectures.  The new
  -- * aligned_64 type enforces 8-byte alignment so that structs containing
  -- * aligned_64 values have the same alignment on 32-bit and 64-bit architectures.
  -- * No conversions are necessary between 32-bit user-space and a 64-bit kernel.
  --  

   subtype uu_poll_t is unsigned;  -- /usr/include/linux/types.h:55

end linux_types_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
