-- -----------------------------------------------------------------------------
-- smk, the smart make (https://github.com/LionelDraghi/smk)
-- SPDX-FileCopyrightText: 2019, 2026 Lionel Draghi
-- SPDX-License-Identifier: Apache-2.0
-- -----------------------------------------------------------------------------

with Smk.Settings;

package body Smk.Assertions is

   -- --------------------------------------------------------------------------
   function Image (A      : Condition;
                   Prefix : String := "") return String is
   begin
      if Settings.Long_Listing_Format then
         return (Prefix & "[" & Trigger_Image (A.Trigger) & "] ");
      else
         return (Prefix);
      end if;
   end Image;

   -- --------------------------------------------------------------------------
   function Count (Cond_List         : Condition_Lists.List;
                   Count_Sources     : Boolean := False;
                   Count_Targets     : Boolean := False;
                   With_System_Files : Boolean := False)
                   return File_Count is
      C : File_Count := 0;
   begin
      for A of Cond_List loop
         if With_System_Files or else not Is_System (A.File)
         then
            if        (Count_Sources and then Is_Source (A.File))
              or else (Count_Targets and then Is_Target (A.File))
            then
               C := C + 1;
            end if;
         end if;
      end loop;
      return C;
   end Count;

   -- --------------------------------------------------------------------------
   function Count_Image (Count : File_Count) return String is
      Raw : constant String := File_Count'Image (Count);
   begin
      return Raw (2 .. Raw'Last);
   end Count_Image;

end Smk.Assertions;
