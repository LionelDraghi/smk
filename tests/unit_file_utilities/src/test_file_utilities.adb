with File_Utilities;

with Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;

procedure Test_File_Utilities is

   Failure_Count : Natural   := 0;
   Check_Idx     : Positive  := 1;

   -- --------------------------------------------------------------------------
   procedure Check (Title    : String;
                    From_Dir : String;
                    To_File  : String;
                    Prefix   : String := "";
                    -- Result   : String;
                    Expected : String) is
      Tmp    : constant String := Positive'Image (Check_Idx);
      Idx    : constant String := Tmp (2 .. Tmp'Last);
      use File_Utilities;
      Result : constant String := Short_Path (From_Dir => From_Dir,
                                              To_File  => To_File,
                                              Prefix   => Prefix);
   begin
      Put (Idx & ". " & Title);
      Check_Idx := Check_Idx + 1;

      if Result = Expected then
         Put (" : OK");
      else
         Put (" : NOK ****");
         Failure_Count := Failure_Count + 1;
      end if;
      New_Line;
      Put ("Short_Path (From_Dir => """ & From_Dir & """,");
      New_Line;
      Put ("            To_File  => """ & To_File  & """");
      if Prefix /= "" then
         Put (",");
         New_Line;
         Put ("            Prefix   => """ & Prefix & """");
      end if;
      Put (") = " & Result);
      New_Line;
      if Result /= Expected then
         Put_Line ("Expected " & Expected);
      end if;
      New_Line;
   end Check;

begin
   Put_Line ("# File_Utilities.Short_Path unit tests");
   New_Line;

   -- --------------------------------------------------------------------------
   Check (Title    => "Subdir with default Prefix",
          From_Dir => "/home/tests",
          To_File  => "/home/tests/mysite/site/d1/idx.txt",
          Expected => "mysite/site/d1/idx.txt");

   Check (Title    => "Dir with final /",
          From_Dir => "/home/tests/",
          To_File  => "/home/tests/mysite/site/d1/idx.txt",
          Expected => "mysite/site/d1/idx.txt");

   Check (Title    => "subdir with Prefix",
          From_Dir => "/home/tests",
          To_File  => "/home/tests/mysite/site/d1/idx.txt",
          Prefix   => "." & File_Utilities.Separator,
          Expected => "./mysite/site/d1/idx.txt");

   Check (Title    => "Sibling subdir",
          From_Dir => "/home/tests/12/34",
          To_File  => "/home/tests/mysite/site/d1/idx.txt",
          Expected => "../../mysite/site/d1/idx.txt");

   Check (Title    => "Parent dir",
          From_Dir => "/home/tests/12/34",
          To_File  => "/home/tests/idx.txt",
          Expected => "../../idx.txt");

   Check (Title    => "Other Prefix",
          From_Dir => "/home/tests/12/",
          To_File  => "/home/tests/mysite/site/d1/idx.txt",
          Prefix   => "$PWD/",
          Expected => "$PWD/../mysite/site/d1/idx.txt");

   Check (Title    => "Root dir",
          From_Dir => "/",
          To_File  => "/home/tests/mysite/site/d1/idx.txt",
          Expected => "/home/tests/mysite/site/d1/idx.txt");

   Check (Title    => "File is over dir",
          From_Dir => "/home/tests/mysite/site/d1",
          To_File  => "/home/readme.txt",
          Expected => "../../../../readme.txt");

   Check (Title    => "File is over Dir, Dir with final /",
          From_Dir => "/home/tests/mysite/site/d1/",
          To_File  => "/home/readme.txt",
          Expected => "../../../../readme.txt");

   Check (Title    => "File is the current dir",
          From_Dir => "/home/tests/",
          To_File  => "/home/tests",
          Expected => "./");

   Check (Title    => "File is over Dir, Dir and File with final /",
          From_Dir => "/home/tests/",
          To_File  => "/home/tests/",
          Expected => "./");

   Check (Title    => "No common part",
          From_Dir => "/home/toto/src/tests/",
          To_File  => "/opt/GNAT/2018/lib64/libgcc_s.so",
          Expected => "/opt/GNAT/2018/lib64/libgcc_s.so");

   --  Check (Title    => "Windows PATH, case sensitivity",
   --         Result   => Short_Path
   --           (From_Dir => "c:\Users\Lionel\",
   --            To_File  => "c:\Users\Xavier\Proj"),
   --         Expected => "..\Xavier\Proj");

   -- --------------------------------------------------------------------------
   New_Line;
   if Failure_Count /= 0 then
      Put_Line (Natural'Image (Failure_Count)
                & " tests fails [Failed](tests_status.md#failed)");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Put_Line ("All tests OK [Successful](tests_status.md#successful)");
   end if;

end Test_File_Utilities;
