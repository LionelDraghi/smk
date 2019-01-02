-- -----------------------------------------------------------------------------
-- smk, the smart make (http://lionel.draghi.free.fr/smk/)
-- © 2018 Lionel Draghi <lionel.draghi@free.fr>
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

with Smk.IO;            use Smk.IO;
with Smk.Runfiles;
with Smk.Runs.Strace_Analyzer;
with Smk.Settings;      use Smk.Settings;

with Ada.Strings.Fixed;

separate (Smk.Runs)

-- -----------------------------------------------------------------------------
procedure Analyze_Run (Sources_And_Targets : out Files.File_Lists.Map;
                       Dirs                : out Files.File_Lists.Map;
                       Counts              : out Runfiles.File_Counts)
is
   Debug        : constant Boolean := False;
   Prefix       : constant String  := ""; -- smk-main-analyze_run.adb ";
   Strace_Ouput : Ada.Text_IO.File_Type;

   -- -----------------------------------------------------------------------
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

begin
   -- --------------------------------------------------------------------------
   IO.Put_Debug_Line ("Openning " & Strace_Outfile_Name, Debug, Prefix);
   Open (File => Strace_Ouput,
         Name => Strace_Outfile_Name,
         Mode => In_File);

   while not End_Of_File (Strace_Ouput) loop
      File_Filter : declare
         Line  : constant String  := Get_Line (Strace_Ouput);
         use Smk.Runs.Strace_Analyzer;
         Read_File  : File;
         Write_File : File;

      begin
         Analyze_Line (Line, Read_File, Write_File);

         -- Let's ignore :
         -- 1. no more existing files after run, that is temporary file
         -- 2. special file, e. g. /dev/something,
         if not (Read_File = No_File)
           and then not In_Ignore_List (Read_File.all)
           and then Exists (Read_File.all)
           and then Kind   (Read_File.all) /= Special_File
         then
            -- Special_File are filtered, meanning that directories and
            -- Ordinary_File files are recorded.
            if Is_Dir (Read_File.all) then
               Update_List (Dirs, +Read_File.all, Source);

            else
               Update_List (Sources_And_Targets, +Read_File.all, Source);

            end if;
         end if;

         -- Same for Targets:
         if not (Write_File = No_File) -- Fixme: duplicated code
           and then not In_Ignore_List (Write_File.all)
           and then Exists (Write_File.all)
           and then Kind   (Write_File.all) /= Special_File
         then
            -- Special_File are filtered, meanning that directories and
            -- Ordinary_File files are recorded.
            if Is_Dir (Write_File.all) then
               Update_List (Dirs, +Write_File.all, Target);

            else
               Update_List (Sources_And_Targets, +Write_File.all, Target);

            end if;
         end if;

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
