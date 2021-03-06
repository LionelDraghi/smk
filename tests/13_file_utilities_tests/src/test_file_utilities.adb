with File_Utilities;   use File_Utilities;

with Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;

procedure Test_File_Utilities is

   Failure_Count : Natural   := 0;
   Check_Idx     : Positive  := 1;

   -- --------------------------------------------------------------------------
   procedure New_Test (Title : String) is
   begin
      New_Line;
      Put_Line ("## " & Title);
      New_Line;
   end New_Test;

   -- --------------------------------------------------------------------------
   procedure Check (Title    : String;
                    Result   : String;
                    Expected : String) is
      Tmp : constant String := Positive'Image (Check_Idx);
      Idx : constant String := Tmp (2 .. Tmp'Last);
   begin
      New_Line;
      Put_Line (Idx & ". " & Title);
      Put_Line ("Expected :");
      Put_Line ("""" & Expected & """");
      if Result = Expected then
         Put_Line ("OK");
      else
         Put_Line ("**Failed**,  got """ & Result & """");
         Failure_Count := Failure_Count + 1;
      end if;
      Check_Idx := Check_Idx + 1;
   end Check;

begin
   New_Line;
   Put_Line ("# File_Utilities unit tests");
   New_Line;

   -- --------------------------------------------------------------------------
   New_Test ("Short_Path");

   Check (Title    => "Subdir with default Prefix",
          Result   => Short_Path
            (From_Dir => "/home/tests",
             To_File  => "/home/tests/mysite/site/d1/idx.txt"),
          Expected => "mysite/site/d1/idx.txt");

   Check (Title    => "Dir with final /",
          Result   => Short_Path
            (From_Dir => "/home/tests/",
             To_File  => "/home/tests/mysite/site/d1/idx.txt"),
          Expected => "mysite/site/d1/idx.txt");

   Check (Title    => "subdir with Prefix",
          Result   => Short_Path
            (From_Dir => "/home/tests",
             To_File  => "/home/tests/mysite/site/d1/idx.txt",
             Prefix   => "." & Separator),
          Expected => "./mysite/site/d1/idx.txt");

   Check (Title    => "Sibling subdir",
          Result   => Short_Path
            (From_Dir => "/home/tests/12/34",
             To_File  => "/home/tests/mysite/site/d1/idx.txt"),
          Expected => "../../mysite/site/d1/idx.txt");

   Check (Title    => "Parent dir",
          Result   => Short_Path
            (From_Dir => "/home/tests/12/34",
             To_File  => "/home/tests/idx.txt"),
          Expected => "../../idx.txt");

   Check (Title    => "Other Prefix",
          Result   => Short_Path
            (From_Dir => "/home/tests/12/",
             To_File  => "/home/tests/mysite/site/d1/idx.txt",
             Prefix   => "$PWD/"),
          Expected => "$PWD/../mysite/site/d1/idx.txt");

   Check (Title    => "Root dir",
          Result   => Short_Path
            (From_Dir => "/",
             To_File  => "/home/tests/mysite/site/d1/idx.txt"),
          Expected => "/home/tests/mysite/site/d1/idx.txt");

   Check (Title    => "File is over dir",
          Result   => Short_Path
            (From_Dir => "/home/tests/mysite/site/d1",
             To_File  => "/home/readme.txt"),
          Expected => "../../../../readme.txt");

   Check (Title    => "File is over Dir, Dir with final /",
          Result   => Short_Path
            (From_Dir => "/home/tests/mysite/site/d1/",
             To_File  => "/home/readme.txt"),
          Expected => "../../../../readme.txt");

   Check (Title    => "File is the current dir",
          Result   => Short_Path
            (From_Dir => "/home/tests/",
             To_File  => "/home/tests"),
          Expected => "./");

   Check (Title    => "File is over Dir, Dir and File with final /",
          Result   => Short_Path
            (From_Dir => "/home/tests/",
             To_File  => "/home/tests/"),
          Expected => "./");

   Check (Title    => "No common part",
          Result   => Short_Path
            (From_Dir => "/home/toto/src/tests/",
             To_File  => "/opt/GNAT/2018/lib64/libgcc_s.so"),
          Expected => "/opt/GNAT/2018/lib64/libgcc_s.so");

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
