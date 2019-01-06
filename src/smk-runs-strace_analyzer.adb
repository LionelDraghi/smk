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

with File_Utilities;
with Smk.IO;
with Smk.Settings;

with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Maps;
with Ada.Strings;
-- with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
-- with Ada.Strings.Maps;

package body Smk.Runs.Strace_Analyzer is

   Function_First_Char : constant := 7;
   -- Cmd_Set : constant Ada.Strings.Maps.Character_Set := Alphanumeric_Set;
   -- defines char that are expected in the command name ("access", "read",
   -- "write", etc.)

   Current_Dir         : Unbounded_String := To_Unbounded_String
     (Smk.Settings.Initial_Directory);

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
                                                new String'("utimensat"),
                                                new String'("futimens"),
                                                new String'("chmod"),
                                                new String'("fchmod"),
                                                new String'("fchmodat"),
                                                new String'("chown"),
                                                new String'("lchown"),
                                                new String'("fchown"),
                                                new String'("fchownat"),
                                                new String'("unlink"),
                                                new String'("unlinkat"),
                                                new String'("remove"),
                                                new String'("mkdir"),
                                                new String'("rmdir"),
                                                new String'("chdir"));
   Read_Only_List  : constant Function_List := (new String'("readlink"),
                                                new String'("readlinkat"));
   Write_Only_List : constant Function_List := (new String'("write"),
                                                new String'("creat"),
                                                new String'("rename"),
                                                new String'("renameat"),
                                                new String'("renameat2"),
                                                new String'("link"));
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

      -- The previous, fairly good and very simple algo:
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
      function Add_Dir (To_Name   : String;
                        Dir       : String := To_String (Current_Dir))
                        return String
      is
         Prefix : constant String := (if Dir (Dir'Last) = '/'
                                      then Dir
                                      else Dir & '/');
      begin
         -- IO.Put_Line ("Current Dir = " & To_String (Current_Dir));
         -- if Name is not a Full_Name, add the known CWD
         if To_Name (To_Name'First) = '/'
         then
            -- IO.Put_Line ("Add_Dir returns " & To_Name);
            return To_Name;
         else
            -- IO.Put_Line ("Add_Dir returns " & Prefix & To_Name);
            return Prefix & To_Name;
         end if;
      end Add_Dir;

      -- -----------------------------------------------------------------------
      function Remove_Dot_Slash (Name : String) return String is
         -- if Name starts with "./", remove it
        (if Name (Name'First .. Name'First + 1) = "./"
         then Name (Name'First + 2 .. Name'Last)
         else Name);

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
      function Get_Parameter (Idx : in out Natural) return String is
         Separator_Set         : constant Ada.Strings.Maps.Character_Set
           := Ada.Strings.Maps.To_Set (",()");
         use Ada.Strings.Maps;
         use Ada.Strings.Maps.Constants;
         Postfix_Ignore_Set    : constant Ada.Strings.Maps.Character_Set
           := Ada.Strings.Maps.To_Set (" <>") or To_Set ('"');
         Prefix_Ignore_Set     : constant Ada.Strings.Maps.Character_Set
           := Postfix_Ignore_Set or Decimal_Digit_Set;
         -- File descriptor number should be also removed in:
         -- 5</home/lionel/.slocdata/x.mp3>
         -- but only on the left side.
         --
         First                 : Positive;
         Last                  : Natural;

      begin
         Find_Token (Source => Line,
                     Set    => Separator_Set,
                     From   => Idx,
                     Test   => Ada.Strings.Outside,
                     First  => First,
                     Last   => Last);
         Idx := Last + 1;
         declare
            Token : constant String := Trim (Line (First .. Last),
                                             Left  => Prefix_Ignore_Set,
                                             Right => Postfix_Ignore_Set);
         begin
            IO.Put_Line ("Token : >" & Token & "<", Level => IO.Debug);
            return Token;
         end;
      end Get_Parameter;

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
               declare
                  Full_Name : constant String := Add_Dir
                    (Remove_Dot_Slash (Line (First + 1 .. Local_Idx - 1)));
                  -- we do not use Ada.Directories.Full_Name because it
                  -- cause link to be modified into there target
               begin
                  IO.Put_Line ("File_Name_In_Box returns " & Full_Name,
                               Level => IO.Debug);
                  return Full_Name;
               end;

            end if;

         end if;
      end File_Name_In_Box;

      -- -----------------------------------------------------------------------
      function File_Name_In_Doublequote (Idx : in out Natural) return String is
         First     : Natural;
         Local_Idx : Natural;

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
                            & Line (First + 1 .. Local_Idx - 1),
                            Level => IO.Debug);
               declare
                  Full_Name : constant String := Add_Dir
                    (Remove_Dot_Slash (Line (First + 1 .. Local_Idx - 1)));
                  -- we do not use Ada.Directories.Full_Name because it
                  -- cause link to be modified into there target
               begin
                  IO.Put_Line ("File_Name_In_Doublequote returns " & Full_Name,
                               Level => IO.Debug);
                  return Full_Name;
               end;

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
        -- file not found:
        or else Index (Line, "ENOENT",       Going => Ada.Strings.Backward) /= 0
      -- error accessing file:
        or else Index (Line, "EACCES",       Going => Ada.Strings.Backward) /= 0
      -- Ignore operation on Dir (used by open or unlink):
        or else Index (Line, "O_DIRECTORY",  Going => Ada.Strings.Backward) /= 0
        or else Index (Line, "AT_REMOVEDIR", Going => Ada.Strings.Backward) /= 0
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

         elsif Is_GetCWD (Function_Name) then
            -- CWD processing --------------------------------------------------
            Current_Dir := To_Unbounded_String (File_Name (Idx));
            IO.Put_Line ("Current Dir = " & To_String (Current_Dir),
                         Level => IO.Debug);

         elsif Is_Read_Cmd (Function_Name) then
            -- Read ------------------------------------------------------------
            Call_Type := Read_Call;
            IO.Put_Line (Function_Name, Level => IO.Debug);

            if Function_Name = "readlinkat" then -- two parameters
               declare
                  P1 : constant String := Get_Parameter (Idx);
                  P2 : constant String := Get_Parameter (Idx);
               begin
                  if P2 (P2'First) = File_Utilities.Separator then
                     -- absolute path, no need to read P1
                     Read_File := new String'(P2);

                  elsif P1 = "AT_FDCWD" then
                     Read_File := new String'
                       (Add_Dir (Dir     => To_String (Current_Dir),
                                 To_Name => P2));
                  else
                     Read_File := new String'(Add_Dir (Dir     => P1,
                                                       To_Name => P2));
                  end if;
               end;

            else -- one parameter
               Read_File := new String'(File_Name (Idx));
               IO.Put_Line (Function_Name & " Read " & Read_File.all,
                            Level => IO.Debug);
            end if;

         elsif Is_Write_Cmd (Function_Name) then
            -- Write -----------------------------------------------------------
            if Function_Name = "rename"
            -- two parameters renames:
            -- 30461 rename("x.mp3", "unknown-unknown.mp3") = 0
            then
               declare
                  P1 : constant String := Get_Parameter (Idx);
                  P2 : constant String := Get_Parameter (Idx);
               begin
                  IO.Put_Line (Function_Name, Level => IO.Debug);
                  Call_Type := Read_Write_Call;

                  Read_File := new String'
                    (Add_Dir (Dir     => To_String (Current_Dir),
                              To_Name => P1));
                  Write_File := new String'
                    (Add_Dir (Dir     => To_String (Current_Dir),
                              To_Name => P2));

                  IO.Put_Line (Function_Name & " from " & Read_File.all
                               & " to " & Write_File.all,
                               Level => IO.Debug);
               end;

            elsif Function_Name = "renameat" or else Function_Name = "renameat2"
               -- four parameters renames
            then -- Fixme: processing of "at" call and AT_FDCDW, cf. unlinkat
               IO.Put_Line (Function_Name, Level => IO.Debug);

               -- 15232 renameat2(AT_FDCWD, "all.filecount.new", \
               --              AT_FDCWD, "all.filecount", RENAME_NOREPLACE) = 0

               declare
                  P1 : constant String := Get_Parameter (Idx);
                  P2 : constant String := Get_Parameter (Idx);
                  P3 : constant String := Get_Parameter (Idx);
                  P4 : constant String := Get_Parameter (Idx);
               begin
                  Call_Type := Read_Write_Call;

                  if P2 (P2'First) = File_Utilities.Separator then
                     -- absolute path, no need to read P1
                     Read_File := new String'(P2);

                  elsif P1 = "AT_FDCWD" then
                     Read_File := new String'
                       (Add_Dir (Dir     => To_String (Current_Dir),
                                 To_Name => P2));
                  else
                     Read_File := new String'(Add_Dir (Dir     => P1,
                                                       To_Name => P2));
                  end if;

                  if P4 (P4'First) = File_Utilities.Separator then
                     -- absolute path, no need to read P3
                     Write_File := new String'(P4);

                  elsif P3 = "AT_FDCWD" then
                     Write_File := new String'
                       (Add_Dir (Dir     => To_String (Current_Dir),
                                 To_Name => P4));
                  else
                     Write_File := new String'(Add_Dir (Dir     => P3,
                                                        To_Name => P4));
                  end if;

                  IO.Put_Line (Function_Name & " from " & Read_File.all
                               & " to " & Write_File.all,
                               Level => IO.Debug);
               end;

--              elsif Function_Name = "unlinkat"
--                and then Index (Line, "AT_REMOVEDIR", From => Idx) /= 0
--              -- process a special form of unlink, that gives first the dir as
--              -- file descriptor, and then the file name:
--              -- 15165 unlinkat(5</home/lionel/.slocdata/dir>, "filelist" ...
--              -- (File name is actually /home/lionel/.slocdata/dir/filelist)
--              -- or
--              -- 1277  unlinkat(AT_FDCWD, "./site/404.html", 0) = 0
--
--                and then Index (Line, "AT_FDCWD", From => Idx) = 0
--              -- Note that this code processes only the first case : if the
--              -- first parameter is not AT_FDCWD, then the dir is
--              -- explicitly given as a file descriptor, and is
--              -- excluded here because we are in the normal processing.
--
--              then
--                 IO.Put_Line (Function_Name, Level => IO.Debug);
--
--                 declare
--                    Dir  : constant String := File_Name_In_Box (Idx);
--                    File : constant String := File_Name_In_Doublequote (Idx);
--                 begin
--                    Call_Type := Write_Call;
--                    Write_File := new String'(Dir & "/" & File);
--                    IO.Put_Line (Function_Name & " " & Dir & "/" & File,
--                                 Level => IO.Debug);
--                 end;

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
                         & "https://github.com/LionelDraghi/smk/issues/new "
                         & "with title ""Non identified call in strace "
                         & "output""");

         end if;

      end;

--        if not Is_Null (Read_File) and then
--          Ada.Directories.Full_Name (Read_File.all) /= Read_File.all
--        then
--           IO.Put_Line ("Read_File (" & Read_File.all
--              & ") /= Full_Name (" & Full_Name (Read_File.all) & ")");
--        end if;
--
--        if not Is_Null (Write_File) and then
--          Ada.Directories.Full_Name (Write_File.all) /= Write_File.all
--        then
--           IO.Put_Line ("Write_File (" & Write_File.all
--             & ") /= Full_Name (" & Full_Name (Write_File.all) & ")");
--        end if;


   exception
      when others =>
         Call_Type := Ignored;
         IO.Put_Line ("Error while analyzing : >" & Line & "<");
         IO.Put_Line ("Please submit this message to "
                      & "https://github.com/LionelDraghi/smk/issues/new "
                      & "with title ""Error analyzing strace output""");

   end Analyze_Line;

end Smk.Runs.Strace_Analyzer;
