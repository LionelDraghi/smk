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

with Smk.Files;                          use Smk.Files;
with Smk.Runs.Strace_Analyzer;           use Smk.Runs.Strace_Analyzer;

with Ada.Command_Line;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Text_IO;                        use Ada.Text_IO;


procedure Smk.Runs.Strace_Analyzer.Test_Strace_Analysis is

   Failure_Count : Natural  := 0;

   -- --------------------------------------------------------------------------
   procedure New_Test (Title : String; Line : String) is
   begin
      New_Line;
      Put_Line ("## " & Title);
      Put_Line ("   Line: " & Line);
   end New_Test;

   -- --------------------------------------------------------------------------
   procedure Check (Title    : String;
                    Result   : String;
                    Expected : String) is
   begin
      Put ("   - Expected " & Title & ": ");
      Put ("""" & Expected & """");
      if Ada.Strings.Equal_Case_Insensitive (Result, Expected) then
         Put_Line (", OK");
      else
         Put_Line (", got """ & Result & """, " & "**Failed**");
         Failure_Count := Failure_Count + 1;
      end if;
   end Check;

   Test_Data : Ada.Text_IO.File_Type;

begin
   -- --------------------------------------------------------------------------
   New_Line;
   Put_Line ("# Analyze_Line unit tests");
   New_Line;

   Open (File => Test_Data,
         Name => "test_data.txt",
         Mode => In_File);

   -- Smk.Settings.Verbosity := Debug;

   while not End_Of_File (Test_Data) loop
      declare
         Title           : constant String := Get_Line (Test_Data);
         Line            : constant String := Get_Line (Test_Data);
         Call            : constant String := Get_Line (Test_Data);
         Read_File_Name  : constant String := Get_Line (Test_Data);
         Write_File_Name : constant String := Get_Line (Test_Data);

         Operation  : Operation_Type;

      begin
         New_Test (Title, Line);

         Smk.Runs.Strace_Analyzer.Analyze_Line (Line, Operation);

         case Operation.Kind is
            when None =>
               Check (Title    => "Call_Type",
                      Result   => "Ignored",
                      Expected => Call);

            when Read =>
               Check (Title    => "Read file",
                      Result   => +Operation.Name,
                      Expected => Read_File_Name);

            when Write | Delete =>
               Check (Title    => "Write file",
                      Result   => +Operation.Name,
                      Expected => Write_File_Name);

            when Move =>
               Check (Title    => "Source file",
                      Result   => +Operation.Source_Name,
                      Expected => Read_File_Name);
               Check (Title    => "Target file",
                      Result   => +Operation.Target_Name,
                      Expected => Write_File_Name);

         end case;
      end;
   end loop;
   Close (File => Test_Data);

   -- To Add To data_test when unfinished line processing is implemented:
   --  Unfinished
   --  6911  openat(AT_FDCWD, "/etc/ld.so.cache", \
   --                                        O_RDONLY|O_CLOEXEC <unfinished ...>
   --  Ignored
   --
   --

   -- --------------------------------------------------------------------------
   New_Line;
   if Failure_Count /= 0 then
      Put_Line (Natural'Image (Failure_Count)
      & " tests fails [Failed](tests_status.md#failed)");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Put_Line ("All tests OK [Successful](tests_status.md#successful)");
   end if;

end Smk.Runs.Strace_Analyzer.Test_Strace_Analysis;
