-- -----------------------------------------------------------------------------
-- smk, the smart make
-- Copyright 2018 Lionel Draghi
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
with Ada.Directories;         use Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;             use Ada.Text_IO;

with Smk.Settings;
with Smk.Run_Files;

separate (Smk.Main)

-- -----------------------------------------------------------------------------
procedure Analyze_Run (Source_Files : out Run_Files.File_Lists.Map;
                       Target_Files : out Run_Files.File_Lists.Map)
is
   Debug        : constant Boolean := False;
   Prefix       : constant String  := ""; -- smk-main-analyze_run.adb ";
   Strace_Ouput : File_Type;

--     procedure Classify_By_Time (Name : in String) is
--        use type Ada.Calendar.Time;
--        File_Time : constant Ada.Calendar.Time := Modification_Time (Name);
--        -- Due to a limitation in the Modification_Time function
--        -- (subsecond are ignored, Modification_Time returns
--        -- Time_Of (Year, Month, Day, Hour, Minute, Second, 0.0);
--        -- This cause the Modification_Time to be < to the
--        -- command execution time, even when the file is
--        -- modified after command execution start.
--        -- To be clear :
--        -- Exec start will be at second 14.45
--        -- File will be modified at second 14.57
--        -- Modification_Time will return 14 instead of 14.57
--        -- So the file will be considered as not modified by the
--        -- command, and never classified as Target.
--        -- To avoid this, I remove sub-seconds in both time,
--        -- both will have the same time tag ending with second
--        -- 14.00, and when equal, file will be considered a Target.
--
--        Line_Nb : constant Integer := Integer (Ada.Text_IO.Line);
--     TT_Image   : constant String := Image (Previous_Run_Time);
--        use Run_Files;
--
--     begin
--        delay (0.1); -- Fixme:
--        if File_Time >= Previous_Run_Time then
--           if not Target_Files.Contains (+Name) then
--              Target_Files.Insert (+Name, File_Time);
--              IO.Put_Debug_Line ("O : " & Image (File_Time,
--                                 Include_Time_Fraction => True) & " >= "
--                                 & TT_Image & " : " & Name,
--                                 Debug  => Debug,
--                                 Prefix => Prefix,
--                                 -- File   => Settings.Strace_Outfile_Name,
--                                 Line   => Line_Nb);
--           end if;
--
--        else
--           if not Source_Files.Contains (+Name) then
--              Source_Files.Insert (+Name, File_Time);
--              IO.Put_Debug_Line ("S : " & Image (File_Time,
--                                 Include_Time_Fraction => True) & " < "
--                                 & TT_Image & " : " & Name,
--                                 Debug  => Debug,
--                                 Prefix => Prefix,
--                                 -- File   => Settings.Strace_Outfile_Name,
--                                 Line   => Line_Nb);
--           end if;
--
--        end if;
--     end Classify_By_Time;

   -- --------------------------------------------------------------------------
   procedure Classify_By_Cmd (Line : in String;
                              Name : in String) is
      File_Time : constant Ada.Calendar.Time := Modification_Time (Name);
      use Ada.Strings.Fixed;
      use Run_Files;

   begin
      if Index (Line, "O_WRONLY") /= 0 or else
        Index (Line, "O_RDWR") /= 0 or else
        Index (Line, "write", From => 7) /= 0  or else
        Index (Line, "creat", From => 7) /= 0
      -- Why seven? because the called system function name comes after
      -- the pid in strace output :
      -- 4372  openat(AT_FDCWD, "/tmp/ccHKHv8W.s", O_RDWR|O_CREAT   etc.

      then
         -- it's a target
         if not Target_Files.Contains (+Name) then
            Target_Files.Insert (+Name, File_Time);
            IO.Put_Debug_Line ("T : " & IO.Image (File_Time) & " " & Name,
                               Debug  => Debug,
                               Prefix => Prefix);
         end if;
         if Source_Files.Contains (+Name) then
            Source_Files.Delete (+Name);
            -- can't be both Target and Source
         end if;

      else
         -- it's a source
         if not Source_Files.Contains (+Name)
           and not Target_Files.Contains (+Name)
         then
            Source_Files.Insert (+Name, File_Time);
            IO.Put_Debug_Line ("S : " & IO.Image (File_Time) & " " & Name,
                               Debug  => Debug,
                               Prefix => Prefix);
         end if;
      end if;

   end Classify_By_Cmd;

begin
   -- --------------------------------------------------------------------------
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
                  -- Classify_By_Time (File_Name);
                  Classify_By_Cmd (Line => Line, Name => File_Name);
               end if;
            end;
         end if;
      end File_Filter;
   end loop;

   if Debug then Close (Strace_Ouput);
   else Delete (Strace_Ouput);
   end if;

end Analyze_Run;
