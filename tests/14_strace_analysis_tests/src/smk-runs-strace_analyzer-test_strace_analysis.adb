with Smk.Runs.Strace_Analyzer;    use Smk.Runs.Strace_Analyzer;
--with Smk.Settings;      use Smk.Settings;

with Ada.Command_Line;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Text_IO;       use Ada.Text_IO;


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
   New_Line;
   Put_Line ("# Strace_Analyzer unit tests");
   New_Line;

   -- Verbosity := Debug;

   -- --------------------------------------------------------------------------
   Open (File => Test_Data,
         Name => "test_data.txt",
         Mode => In_File);

   while not End_Of_File (Test_Data) loop
      declare
         Title           : constant String := Get_Line (Test_Data);
         Line            : constant String := Get_Line (Test_Data);
         Call            : constant String := Get_Line (Test_Data);
         Read_File_Name  : constant String := Get_Line (Test_Data);
         Write_File_Name : constant String := Get_Line (Test_Data);

         Call_Type  : Line_Type;
         Read_File  : File;
         Write_File : File;

      begin
         New_Test (Title, Line);

         Smk.Runs.Strace_Analyzer.Analyze_Line (Line,
                                                Call_Type,
                                                Read_File,
                                                Write_File);

         Check (Title    => "Call_Type",
                Result   => Line_Type'Image (Call_Type),
                Expected => Call);
         Check (Title    => "Read file",
                Result   => (if Read_File =  null then "" else Read_File.all),
                Expected => Read_File_Name);
         Check (Title    => "Write file",
                Result   => (if Write_File = null then "" else Write_File.all),
                Expected => Write_File_Name);

      end;
   end loop;

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
