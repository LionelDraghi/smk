-- -----------------------------------------------------------------------------
-- smk, the smart make (http://lionel.draghi.free.fr/smk/)
-- © 2018, 2019 Lionel Draghi <lionel.draghi@free.fr>
-- SPDX-License-Identifier: APSL-2.0
-- -----------------------------------------------------------------------------
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- http://www.apache.org/licenses/LICENSE-2.0
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-- -----------------------------------------------------------------------------

with Smk.IO;                   use Smk.IO;
with Smk.Runfiles;
with Smk.Runs.Strace_Analyzer;
with Smk.Settings;             use Smk.Settings;

with Ada.Strings.Fixed;
with Ada.Text_IO;

separate (Smk.Runs)

-- -----------------------------------------------------------------------------
procedure Analyze_Run (Sources_And_Targets : out Files.File_Lists.Map;
                       Dirs                : out Files.File_Lists.Map;
                       Counts              : out Runfiles.File_Counts)
is
   Strace_Ouput : Ada.Text_IO.File_Type;

   -- --------------------------------------------------------------------------
   procedure Update_List (List      : in out Files.File_Lists.Map;
                          Name      : in     File_Name;
                          With_Role : in     File_Role) is
      use File_Lists;
      C : constant Cursor := List.Find (Name);
   begin
      if Has_Element (C) then
         -- Update:
         if With_Role = Target then
            Set_Target (List (C));
         else
            -- Other values of Role than Target and Source are
            -- not possible, hence no "case" here.
            Set_Source (List (C));
         end if;

      else
         -- Creation:
         List.Insert (Name, Create (Name, With_Role));

      end if;
   end Update_List;

   use Smk.Runs.Strace_Analyzer;

   -- --------------------------------------------------------------------------
   procedure Update_Lists (Write_File : File;
                           With_Role  : File_Role) is
   begin
      if not Is_Null (Write_File)
        and then not In_Ignore_List (Write_File.all)
        and then (not Exists (Write_File.all)
                  or else Kind (Write_File.all) /= Special_File)
      then
         -- Special_File are filtered, meanning that directories and
         -- Ordinary_File files are recorded.
         if Is_Dir (Write_File.all) then
            Update_List (Dirs, +Write_File.all, With_Role);

         else
            Update_List (Sources_And_Targets, +Write_File.all, With_Role);

         end if;
      end if;

   end Update_Lists;

begin
   -- --------------------------------------------------------------------------
   Open (File => Strace_Ouput,
         Name => Strace_Outfile_Name,
         Mode => In_File);

   while not End_Of_File (Strace_Ouput) loop
      File_Filter : declare
         Line  : constant String  := Get_Line (Strace_Ouput);
         Read_File  : File;
         Write_File : File;
         Call_Type  : Line_Type;

      begin
         Analyze_Line (Line, Call_Type, Read_File, Write_File);

         case Call_Type is
            when Read_Call           =>
               Update_Lists (Read_File,  Source);

            when Write_Call          =>
               Update_Lists (Write_File, Target);

            when Read_Write_Call     =>
               Update_Lists (Read_File,  Source);
               Update_Lists (Write_File, Target);

            when Exec_Call | Ignored =>
               null;

         end case;

      end File_Filter;

   end loop;

   -- Add directories informations: if a directory was open, it may be
   -- because the command is searching files in it. In this case, we will
   -- take a picture of the dir (that is store all files even non accessed)
   -- to be able to compare the picture during future run, and detect added
   -- files in that dir.
   --
   declare
      Search   : Search_Type;
      File     : Directory_Entry_Type;
      New_Dirs : File_Lists.Map;

   begin
      for D in Dirs.Iterate loop
         Start_Search (Search,
                       Directory => +File_Lists.Key (D),
                       Pattern   => "./*",
                       Filter    => (Ordinary_File => True,
                                     others        => False));
         IO.Put_Line ("scanning dir " & (+File_Lists.Key (D)),
                      Level => IO.Debug);
         while More_Entries (Search) loop
            Get_Next_Entry (Search, File);

            if Settings.In_Ignore_List (Full_Name (File)) then
               IO.Put_Line ("Ignoring " & (Full_Name (File)),
                            Level => IO.Debug);

            elsif Is_Dir (Full_Name (File)) then
               -- Dir processing
               if Dirs.Contains (+Full_Name (File)) then
                  IO.Put_Line ("dir " & Simple_Name (File) & " already known");
               else
                  IO.Put_Line ("recording not accessed new dir "
                               & Simple_Name (File));
                  New_Dirs.Insert   (+Full_Name (File),
                                     Create (+Full_Name (File), Unused));

               end if;

            else -- Fixme: partial code duplication
               -- File processing
               if Sources_And_Targets.Contains (+Full_Name (File)) then
                  IO.Put_Line ("file " & Simple_Name (File) & " already known");
               else
                  IO.Put_Line ("recording not accessed new file "
                               & Simple_Name (File));
                  Sources_And_Targets.Insert
                    (+Full_Name (File),
                     Create (+Full_Name (File), Unused));

               end if;

            end if;


         end loop;

      end loop;

      -- Merge unused files into dir list:
      for F in New_Dirs.Iterate loop
         Dirs.Insert (Key      => File_Lists.Key (F),
                      New_Item => New_Dirs (F));
      end loop;

   end;

   Runfiles.Reset (Counts);
   Runfiles.Update_Counts (Sources_And_Targets, Counts);
   -- Fixme: no dir in counts   Runfiles.Update_Counts (Dirs, Counts);

   if Debug_Mode then Close (Strace_Ouput);
   else Delete (Strace_Ouput);
   end if;

end Analyze_Run;
