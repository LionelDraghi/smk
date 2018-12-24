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

with Ada.Calendar;
with Ada.Directories;   use Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with Smk.Files;         use Smk.Files;
with Smk.Settings;
with Smk.Runfiles;

separate (Smk.Main)

-- -----------------------------------------------------------------------------
procedure Analyze_Run (Source_Files             :    out Files.File_Lists.Map;
                       Source_System_File_Count :    out Natural;
                       Target_Files             :    out Files.File_Lists.Map;
                       Target_System_File_Count :    out Natural)
is
   Debug        : constant Boolean := False;
   Prefix       : constant String  := ""; -- smk-main-analyze_run.adb ";
   Strace_Ouput : Ada.Text_IO.File_Type;

   -- --------------------------------------------------------------------------
   procedure Classify_Source_Or_Target (Line : in String;
                                        Name : in String) is
      use Ada.Strings.Fixed;
   begin
      if In_Ignore_List (Name) then
         IO.Put_Line ("Ignoring " & Name, Level => IO.Debug);
         return;
      end if;

      if Index (Line, "O_WRONLY") /= 0
        or else Index (Line, "O_RDWR") /= 0
        or else Index (Line, "write", From => 7) /= 0
        or else Index (Line, "creat", From => 7) /= 0
      -- Why seven? because the called system function name comes after
      -- the pid in strace output :
      -- 4372  openat(AT_FDCWD, "/tmp/ccHKHv8W.s", O_RDWR|O_CREAT   etc.

      then
         -- it's a target
         if not Target_Files.Contains (+Name) then
            if Is_System (Name) then
               Target_System_File_Count := Target_System_File_Count + 1;
            end if;
            Target_Files.Insert (+Name, Create (+Name));
         end if;

         if Source_Files.Contains (+Name) then
            Source_Files.Delete (+Name);
            -- can't be both Target and Source
            IO.Put_Line
              ("T : " & Name & " is both source and target, deleting source");
         end if;

      else
         -- it's a source
         if not Source_Files.Contains (+Name)
           and not Target_Files.Contains (+Name)
         then
            if Is_System (Name) then
               Source_System_File_Count := Source_System_File_Count + 1;
            end if;
            Source_Files.Insert (+Name, Create (+Name));
         end if;
      end if;

   end Classify_Source_Or_Target;

begin
   -- --------------------------------------------------------------------------
   Source_System_File_Count := 0;
   Target_System_File_Count := 0;

   IO.Put_Debug_Line ("Openning " & Strace_Outfile_Name, Debug, Prefix);
   Open (File => Strace_Ouput,
         Name => Strace_Outfile_Name,
         Mode => In_File);

   while not End_Of_File (Strace_Ouput) loop
      File_Filter : declare
         use Ada.Strings.Fixed;
         Line  : constant String  := Get_Line (Strace_Ouput);
         First : constant Natural := Index (Line, "<");
         Last  : constant Natural := Index (Line, ">");
         -- use type Runfiles.File_Name;

      begin
         -- IO.Put_Debug_Line ("Processing line: " & Line, Debug, Prefix);
         if Last > First then
            -- the line contains both '<' and '>'.
            declare
               File_Name : constant String  := Line (First + 1 .. Last - 1);
            begin
               -- Let's ignore :
               -- 1. no more existing files after run, that is temporary file
               -- 2. special file, e. g. /dev/something,
               if Exists (File_Name) and then Kind (File_Name) /= Special_File
               then
                  Classify_Source_Or_Target (Line => Line, Name => File_Name);
               end if;
            end;

--           elsif Index (Line, "unlink") /= 0 then
--             -- In some cases, the file name is not inside <> (there is no
--             -- file descriptor), but the line is never the less useful.
--             -- Exemple:
--             --    19258 unlinkat(AT_FDCWD, "tmp.1", 0)    = 0
--             -- is a delete of tmp.1
--              declare
--                 First     : constant Natural := Index (Line, """");
--                 Last      : constant Natural := Index (Line, """",
--                                                        From => First + 1);
--                 File_Name : constant String  := Line (First + 1 .. Last - 1);
--                 Clock_Time : constant Ada.Calendar.Time
--                          := Ada.Calendar.Clock;
--                 -- time used for deleted files
--              begin
--
--                 -- Fixme : code duplicated from Classify_By_Cmd:
--
--                 -- it's a source
--                 if not Source_Files.Contains (+File_Name)
--                   and not Target_Files.Contains (+File_Name)
--                 then
--                    if Is_System (File_Name) then
--                       Source_System_File_Count :=
--                         Source_System_File_Count + 1;
--                       Source_Files.Insert (+File_Name,
--                                            (Clock_Time, Is_System => True));
--                       IO.Put_Debug_Line
--                         ("S deleted system file : " & IO.Image (Clock_Time)
--                          & " " & File_Name,
--                          Debug  => Debug,
--                          Prefix => Prefix);
--                    else
--                       Source_Files.Insert (+File_Name,
--                                            (Clock_Time, Is_System => False));
--                       IO.Put_Debug_Line ("S deleted : "
--                                          & IO.Image (Clock_Time)
--                                          & " " & File_Name,
--                                          Debug  => Debug,
--                                          Prefix => Prefix);
--                    end if;
--                 end if;
--
--              end;
--
         end if;
      end File_Filter;
   end loop;

   if Debug then Close (Strace_Ouput);
   else Delete (Strace_Ouput);
   end if;

end Analyze_Run;
