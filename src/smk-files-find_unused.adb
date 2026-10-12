-- -----------------------------------------------------------------------------
-- smk, the smart make (https://github.com/LionelDraghi/smk)
-- SPDX-FileCopyrightText: 2019, 2026 Lionel Draghi
-- SPDX-License-Identifier: Apache-2.0
-- -----------------------------------------------------------------------------

with Smk.Files.File_Lists;
with Smk.IO;
with Smk.Settings;

with Ada.Directories;

procedure Smk.Files.Find_Unused (From   : in     File_Name; -- must be a dir
                                 Not_In : in     File_Lists.Map;
                                 Put_In : in out File_Lists.Map) is
   use Smk.Files.File_Lists;

   -- --------------------------------------------------------------------------
   procedure Walk_Dir (Dir : in String) is
      use Ada.Directories;
      Search : Search_Type;
      File   : Directory_Entry_Type;

   begin
      IO.Put_Line ("Start searching new files in " & Dir,
                   Level => IO.Debug);
      Start_Search (Search,
                    Directory => Dir,
                    Pattern   => "*",
                    Filter    => [Ordinary_File => True,
                                  Directory     => True,
                                  others        => False]);

      while More_Entries (Search) loop
         Get_Next_Entry (Search, File);
         declare
            Full_Name   : constant String := Ada.Directories.Full_Name   (File);
            Simple_Name : constant String := Ada.Directories.Simple_Name (File);

         begin
            if Simple_Name = ".." or Simple_Name = "." then
               IO.Put_Line ("Ignoring " & Simple_Name, Level => IO.Debug);

            elsif Not_In.Contains (+Full_Name) then
               IO.Put_Line (Simple_Name & " already known", Level => IO.Debug);

            else
               -- A genuine new file:
               IO.Put_Line ("New file " & Simple_Name, Level => IO.Debug);
               if not Settings.In_Ignore_List (Simple_Name) then
                  Put_In.Insert (+Full_Name,
                                 Create (File => +Full_Name,
                                         Role => Unused));
               end if;

               if Kind (File) = Directory then
                  Walk_Dir (Full_Name); -- recursive call

               end if;

            end if;
         end;

      end loop;

   end Walk_Dir;

begin
   Walk_Dir (+From);

end Smk.Files.Find_Unused;
