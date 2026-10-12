-- -----------------------------------------------------------------------------
-- smk, the smart make (https://github.com/LionelDraghi/smk)
-- SPDX-FileCopyrightText: 2018, 2019, 2026 Lionel Draghi
-- SPDX-License-Identifier: Apache-2.0
-- -----------------------------------------------------------------------------

with Smk.Settings;

-- -----------------------------------------------------------------------------
procedure Smk.Files.Put (File_List     : File_Lists.Map;
                         Prefix        : String  := "";
                         Print_Sources : Boolean := False;
                         Print_Targets : Boolean := False;
                         Print_Unused  : Boolean := False) is
   use File_Lists;
   use Smk.Settings;
begin
   for F in File_List.Iterate loop
      if not (Settings.Filter_Sytem_Files and Is_System (File_List (F)))
      then
         if        (Print_Sources and then Is_Source (File_List (F)))
           or else (Print_Targets and then Is_Target (File_List (F)))
           or else (Print_Unused  and then Is_Unused (File_List (F)))
         then
            Put_File_Description (Name   => Key (F),
                                  File   => File_List (F),
                                  Prefix => Prefix);
         end if;
      end if;
   end loop;
end Smk.Files.Put;
