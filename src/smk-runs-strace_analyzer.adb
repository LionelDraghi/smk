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

with Smk.IO;

with Ada.Directories;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
-- with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
-- with Ada.Strings.Maps;

package body Smk.Runs.Strace_Analyzer is

   Function_First_Char : constant := 7;
   -- Cmd_Set : constant Ada.Strings.Maps.Character_Set := Alphanumeric_Set;
   -- defines char that are expected in the command name ("access", "read",
   -- "write", etc.)

   Current_Dir         : Unbounded_String := Null_Unbounded_String;

   -- --------------------------------------------------------------------------
   type Function_List is array (Positive range <>) of access String;

   Special_Lines : constant Function_List := (new String'("---"),
                                              new String'("<..."));
   -- Filter line like :
   -- "15214 --- SIGCHLD ..."
   -- or
   -- "15225 <... access resumed> ..."

   Ignore_List     : constant Function_List := (new String'("stat"),
                                                new String'("access"),
                                                new String'("fstat"),
                                                new String'("lstat"),
                                                new String'("faccessat"),
                                                new String'("execve"),
                                                new String'("SIGCHLD"),
                                                new String'("fcntl"),
                                                new String'("lseek"),
                                                new String'("mknod"),
                                                new String'("umask"),
                                                new String'("close"),
                                                new String'("statfs"),
                                                new String'("fstatfs"),
                                                new String'("fstatat"),
                                                new String'("newfstatat"),
                                                new String'("freopen"),
                                                new String'("chdir"));
   Read_Only_List  : constant Function_List := (new String'("readlink"),
                                                new String'("readlinkat"));
   Write_Only_List : constant Function_List := (new String'("write"),
                                                new String'("creat"),
                                                new String'("rename"),
                                                new String'("renameat"),
                                                new String'("renameat2"),
                                                new String'("link"),
                                                new String'("unlink"),
                                                new String'("unlinkat"),
                                                new String'("remove"),
                                                new String'("chmod"),
                                                new String'("fchmod"),
                                                new String'("chown"),
                                                new String'("fchown"),
                                                new String'("mkdir"),
                                                new String'("rmdir"),
                                                new String'("lchown"));
   Read_Or_Write_List : constant Function_List := (new String'("open"),
                                                   new String'("fopen"),
                                                   new String'("openat"));
   GetCWD : constant Function_List := (new String'("getcwd"),
                                       new String'("getwd"),
                                       new String'("get_current_dir_name"));
   -- Fixme: to increase performances, those List should be ordered with
   --        most probable command first.

   -- --------------------------------------------------------------------------
   function Not_A_Function_Call (Line : String) return Boolean is
     (for some S of Special_Lines =>
         Line (Line'First + Function_First_Char - 1
               .. Line'First + Function_First_Char - 2 + S.all'Length) = S.all);
   -- --------------------------------------------------------------------------
   function Is_Ignored (Call : String) return Boolean is
     (for some C of Ignore_List => Call = C.all);
   -- --------------------------------------------------------------------------
   function Is_Read_Cmd (Call : String) return Boolean is
     (for some C of Read_Only_List => Call = C.all);
   -- --------------------------------------------------------------------------
   function Is_Write_Cmd (Call : String) return Boolean is
     (for some C of Write_Only_List => Call = C.all);
   -- --------------------------------------------------------------------------
   function Is_Read_Or_Write_Cmd (Call : String) return Boolean is
     (for some C of Read_Or_Write_List => Call = C.all);
   -- --------------------------------------------------------------------------
   function Is_GetCWD (Call : String) return Boolean is
     (for some C of GetCWD => Call = C.all);

   -- --------------------------------------------------------------------------
   procedure Analyze_Line (Line       : in     String;
                           Call_Type  :    out Line_Type;
                           Read_File  :    out File;
                           Write_File :    out File) is

      -- Previous (good) simple algo:
      --        if Index (Line, "O_WRONLY") /= 0
      --          or else Index (Line, "O_RDWR") /= 0
      --          or else Index (Line, "write", From => 7) /= 0
      --          or else Index (Line, "creat", From => 7) /= 0
      --        then
      --           Role := Target;
      --        else
      --           Role := Source;
      --        end if;
      -- It's now far more complex...

      -- -----------------------------------------------------------------------
      function Get_Function_Name (Idx : in out Natural) return String is
         -- A line always start with the command from char 7 to char before '(':
         -- 15167 stat("/bin/sed",  <unfinished ...>
         --       ^^^^
         Last : constant Natural := Index (Source  => Line,
                                           Pattern => "(",
                                           From    => Idx + 1);
         R    : constant String := Line (Line'First + Idx - 1
                                         .. Last - 1);
      begin
         Idx := Last;
         return R;
      end Get_Function_Name;

      -- -----------------------------------------------------------------------
      function Add_Dir (Name : String) return String is
         -- if Name is not a Full_Name, add the known CWD
        (if Name (Name'First) = '/'
         then Name
         else To_String (Current_Dir & '/' & Name));

      -- -----------------------------------------------------------------------
      function Write_Access (From : in Positive) return Boolean is
         WRONLY : constant String := "O_WRONLY";
         RDWR   : constant String := "O_RDWR";
         I      : Natural; -- This function don't move the Idx, because the file
         -- name may be before or after the searched Strings.
      begin
         I := Index (Line, WRONLY, From);
         if I /= 0 then
            return True;
         else
            return Index (Line, RDWR, From) /= 0;
         end if;
      end Write_Access;

      -- -----------------------------------------------------------------------
      function File_Name_In_Box (Idx : in out Natural) return String is
         First     : Natural;
         Local_Idx : Natural;
      begin
         First := Index (Line, "<", From => Idx);
         -- looking for "</" is a way to avoid being confused by line
         -- containing "<unfinished" like in:
         -- 15168 access("/etc/ld.so.nohwcap", F_OK <unfinished ...>

         if First = 0 then
            IO.Put_Line ("File_Name_In_Box =""""",
                         Level => IO.Debug);
            return ""; -- file name not found

         else
            Local_Idx := Index (Line, ">", From => First + 1);
            if Local_Idx = 0 then
               IO.Put_Line ("File_Name_In_Box : no closing >",
                            Level => IO.Debug);
               return "";
            else
               Idx := Local_Idx + 1;
               IO.Put_Line ("File_Name_In_Box = "
                            & Line (First + 1 .. Local_Idx - 1),
                            Level => IO.Debug);
               return Add_Dir (Line (First + 1 .. Local_Idx - 1));
            end if;

         end if;
      end File_Name_In_Box;

      -- -----------------------------------------------------------------------
      function File_Name_In_Doublequote (Idx : in out Natural) return String is
         First     : Natural;
         Local_Idx : Natural;
         use Ada.Directories;

      begin
         First := Index (Line, """", From => Idx);
         -- Note that looking for "/ ignore non
         -- Full_Name, like in
         -- 4670  unlink("hello")                   = 0
         -- and this is a problem!
         -- Fixme: need cwd management

         if First = 0 then
            IO.Put_Line ("File_Name_In_Doublequote =""""",
                         Level => IO.Debug);

            return ""; -- file name not found

         else
            Local_Idx := Index (Line, """", From => First + 1);
            if Local_Idx = 0 then
               IO.Put_Line ("File_Name_In_Doublequote : no closing doublequote",
                            Level => IO.Debug);
               return "";
            else
               Idx := Local_Idx + 1;
               IO.Put_Line ("File_Name_In_Doublequote = "
                            & Full_Name (Line (First + 1 .. Local_Idx - 1)),
                            Level => IO.Debug);
               return (Add_Dir (Line (First + 1 .. Local_Idx - 1)));
            end if;

         end if;
      end File_Name_In_Doublequote;

      -- -----------------------------------------------------------------------
      function File_Name (Idx : in out Natural) return String is
         -- File name are either between double quotes, or when using the
         -- -y option between <>.
         -- The -y option print paths associated with file descriptor arguments,
         -- and path are always Full_Name.
         -- Unfortunatly, there is not always a file descriptor argument,
         -- so that we have look for both format.
         -- A slight difference between format is that when between
         -- double quotes, files name are as passed "as is" during the
         -- system call, hence the Full_Name transformation.

         F : constant String := File_Name_In_Box (Idx);

      begin
         if F = "" then
            return File_Name_In_Doublequote (Idx);
         else
            return F;
         end if;
      end File_Name;

      Idx   : Natural;
      -- Idx is the pointer to where we are in the line analysis
      -- Following functions that analyze the line may move idx to the
      -- last analyzed character.

   begin
      Idx        := Function_First_Char;
      Read_File  := null;
      Write_File := null;
      Call_Type := Ignored;

      -- eliminate null lines,
      -- or lines that do not start with function name,
      -- or call that don't find a file (ENOENT)
      if Line'Length = 0
        or else Not_A_Function_Call (Line)
        or else Index (Line, "ENOENT", Going => Ada.Strings.Backward) /= 0
      then
         -- IO.Put_Line ("Ignoring line : " & Line, Level => IO.Debug);
         return;

      end if;

      IO.Put_Line ("",                         Level => IO.Debug);
      IO.Put_Line ("Analyzing line : " & Line, Level => IO.Debug);

      declare
         Function_Name : constant String := Get_Function_Name (Idx);

      begin
         IO.Put_Line ("Function_Name : " & Function_Name, Level => IO.Debug);

         -- The command is followed by "(", let's jump over
         Idx := Idx + 1;

         if Is_Ignored (Function_Name) then
            -- Ignored ---------------------------------------------------------
            IO.Put_Line ("Ignoring " & Function_Name, Level => IO.Debug);
            null;

         elsif Is_GetCWD (Function_Name) then
            -- CWD processing --------------------------------------------------
            Current_Dir := To_Unbounded_String (File_Name (Idx));
            -- IO.Put_Line ("Current Dir = " & To_String (Current_Dir));

         elsif Is_Read_Cmd (Function_Name) then
            -- Read ------------------------------------------------------------
            Call_Type := Read_Call;
            Read_File := new String'(File_Name (Idx));
            IO.Put_Line (Function_Name & " Read " & Read_File.all,
                         Level => IO.Debug);

         elsif Is_Write_Cmd (Function_Name) then
            -- Write -----------------------------------------------------------
            if Function_Name        = "rename"
              or else Function_Name = "renameat"
              or else Function_Name = "renameat2"
            then
               IO.Put_Line ("Renaming: " & Function_Name, Level => IO.Debug);

               -- 15232 renameat2(AT_FDCWD, "all.filecount.new", \
               --              AT_FDCWD, "all.filecount", RENAME_NOREPLACE) = 0

               Call_Type := Read_Write_Call;
               Read_File := new String'(File_Name (Idx));
               -- First file is the read one
               Idx := Idx + 1; -- jump over the ',' following the first file
               Write_File := new String'(File_Name (Idx));
               IO.Put_Line (Function_Name & " from " & Read_File.all
                            & " to " & Write_File.all,
                            Level => IO.Debug);

            elsif Function_Name = "unlinkat" and
              Index (Line, "AT_REMOVEDIR", From => Idx) /= 0
            then
               -- process a special form of unlink, that gives first the dir as
               -- file descriptor, and then the file name:
               -- 15165 unlinkat(5</home/lionel/.slocdata/dir>, "filelist" ...
               -- (File name is actually /home/lionel/.slocdata/dir/filelist)
               -- other form of unlinkat seem's to fall down in the
               -- 1277  unlinkat(AT_FDCWD, "./site/404.html", 0) = 0
               -- common processing.
               IO.Put_Line (Function_Name, Level => IO.Debug);

               declare
                  Dir  : constant String := File_Name_In_Box (Idx);
                  File : constant String := File_Name_In_Doublequote (Idx);
               begin
                  Call_Type := Write_Call;
                  Write_File := new String'(Dir & "/" & File);
                  IO.Put_Line (Function_Name & " " & Dir & "/" & File,
                               Level => IO.Debug);
               end;

            elsif Function_Name = "unlinkat" then

               declare
                  Name : constant String := File_Name (Idx);
               begin
                  Call_Type  := Write_Call;
                  Write_File := new String'(Name);
                  IO.Put_Line (Function_Name & " " & Name,
                               Level => IO.Debug);
               end;

            else
               Call_Type  := Write_Call;
               Write_File := new String'(File_Name (Idx));
               IO.Put_Line (Function_Name & " Write " & Write_File.all,
                            Level => IO.Debug);

            end if;

         elsif Is_Read_Or_Write_Cmd (Function_Name) then
            -- Read or Write----------------------------------------------------
            if Write_Access (From => Idx) then
               Call_Type  := Write_Call;
               Write_File := new String'(File_Name (Idx));
               IO.Put_Line (Function_Name & " Write " & Write_File.all,
                            Level => IO.Debug);

            else
               Call_Type := Read_Call;
               Read_File := new String'(File_Name (Idx));
               IO.Put_Line (Function_Name & " Read " & Read_File.all,
                            Level => IO.Debug);

            end if;

         else
            -- Unknown ---------------------------------------------------------
            Call_Type := Ignored;
            IO.Put_Line ("Non identified file related call in strace line : >"
                         & Line & "<");
            IO.Put_Line ("Please submit this message to "
                         & "https://github.com/LionelDraghi/smk/issues/new"
                         & "with title ""Non identified call in strace "
                         & "output""");

         end if;

      end;

   exception
      when others =>
         Call_Type := Ignored;
         IO.Put_Line ("Error while analyzing : >" & Line & "<");
         IO.Put_Line ("Please submit this message to "
                      & "https://github.com/LionelDraghi/smk/issues/new"
                      & "with title ""Error analyzing strace output""");

   end Analyze_Line;

end Smk.Runs.Strace_Analyzer;
