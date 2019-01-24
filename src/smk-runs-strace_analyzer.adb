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
with Smk.Settings;

with Ada.Containers.Ordered_Maps; use Ada.Containers;
with Ada.Directories;
with Ada.Strings.Fixed;           use Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Maps;
with Ada.Strings;                 use Ada.Strings;

package body Smk.Runs.Strace_Analyzer is

   -- --------------------------------------------------------------------------
   type Function_List is array (Positive range <>) of access String;

   Special_Lines : constant Function_List := (new String'("---"),
                                              new String'("<..."));
   -- Filter line like :
   --    "15214 --- SIGCHLD ..."
   -- or
   --    "15225 <... access resumed> ..."
   --
   -- Fixme: processing of unfinished line not done
   -- Example:
   --    15168 access("/etc/ld.so.nohwcap", F_OK <unfinished ...>
   --    15168 <... access resumed> {st_mode=0755, st_size=122224, ...}) = 0
   -- In some case, the command name is in the interrupted line,
   -- but the fine name (returned prarameter for instance) is
   -- in the resume line. We need to save the interrupted line
   -- until the line is complete, and then process it.
   -- The drawback may be a screwed timing or sequencing, I'm not sure,
   -- but anyway it will be better than current situation, that cause the
   -- trace to be ignored.

   -- --------------------------------------------------------------------------
   Function_First : constant := 7;
   -- A line always start with the command from char 7 to char before '(':
   -- 15167 stat("/bin/sed",  <unfinished ...>
   --       ^

   Param_First : Natural;
   -- should be set, after get_Command, to the first char after '('
   -- 15167 stat("/bin/sed",  <unfinished ...>
   --            ^

   -- --------------------------------------------------------------------------
   function Not_A_Function_Call (Line : String) return Boolean is
     (for some S of Special_Lines =>
         Line (Line'First + Function_First - 1
               .. Line'First + Function_First - 2 + S.all'Length) = S.all);

   -- --------------------------------------------------------------------------
   type Process_Id is new Natural;
   package Process_WDs is new Ordered_Maps (Process_Id, File_Name);
   -- This is the list of workind directories, indexed by process id, as each
   -- process may work in a different dir.
   use Process_WDs;
   Process_WD : Process_WDs.Map;

   -- --------------------------------------------------------------------------
   procedure Analyze_Line (Line      : in     String;
                           Operation :    out Operation_Type) is

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
      function Get_PID return Process_Id is
      begin
         return Process_Id'Value
           (Line (Line'First .. Line'First + Function_First - 2));
      end Get_PID;

      -- -----------------------------------------------------------------------
      function Get_WD (PID : Process_Id) return File_Name is
      begin
         if Process_WD.Contains (PID) then
            IO.Put_Line ("Process_WD returns >"
                         & (+Process_WD.Element (PID))
                         & "<", Level => IO.Debug);
            return Process_WD.Element (PID);
         else
            return +Ada.Directories.Full_Name (Settings.Run_Dir_Name);
         end if;
      end Get_WD;

      -- -----------------------------------------------------------------------
      function Get_Command return String is
         Last : constant Natural := Index (Source  => Line,
                                           Pattern => "(",
                                           From    => Function_First + 1);
      begin
         Param_First := Last + 1;
         return Line (Line'First + Function_First - 1 .. Last - 1);
      end Get_Command;

      -- -----------------------------------------------------------------------
      function Add_Dir (To_Name : File_Name;
                        Dir     : File_Name) return File_Name is
      begin
         if Head (+To_Name, 1) = "/" then
            -- IO.Put_Line ("Add_Dir returns " & (+To_Name));
            return To_Name;

         else
            -- if Name is not a Full_Name, concat with Dir
            declare
               Prefix : constant String :=
                          (if Dir = No_File then ""
                           else (Trim (+Dir,
                             Left  => Ada.Strings.Maps.Null_Set,
                             Right => Ada.Strings.Maps.To_Set ('/')) & '/'));
            begin
               -- IO.Put_Line ("Add_Dir returns " & Prefix & (+To_Name));
               return Prefix & To_Name;
            end;

         end if;
      end Add_Dir;

      -- -----------------------------------------------------------------------
      function Get_File_Name return File_Name is
         use Ada.Strings.Maps;
         use Ada.Strings.Maps.Constants;

         Separator_Set      : constant Character_Set := To_Set (",()");
         Postfix_Ignore_Set : constant Character_Set
           := To_Set (" <>") or To_Set ('"');
         Prefix_Ignore_Set  : constant Character_Set
           := Postfix_Ignore_Set or Decimal_Digit_Set;
         -- File descriptor number should be also removed in:
         -- 5</home/lionel/.slocdata/x.mp3>
         -- but only on the left side.

         First : Positive;
         Last  : Natural;

      begin
         Find_Token (Source => Line,
                     Set    => Separator_Set,
                     From   => Param_First,
                     Test   => Ada.Strings.Outside,
                     First  => First,
                     Last   => Last);
         declare
            Token : constant String := Trim (Line (First .. Last),
                                             Left  => Prefix_Ignore_Set,
                                             Right => Postfix_Ignore_Set);
         begin
            Param_First := Last + 1;
            return +Token;
         end;
      end Get_File_Name;

      -- -----------------------------------------------------------------------
      function Get_Dir_Name (PID : Process_Id) return File_Name is
         F : constant File_Name := Get_File_Name;
      begin
         if F = "AT_FDCWD" then
            return Get_WD (PID);
         else
            return F;
         end if;
      end Get_Dir_Name;

      -- -----------------------------------------------------------------------
      function Get_Returned_File return File_Name is
         First : Natural;
         Last  : Natural;

      begin
         First := Index (Line, "=", Backward);
         -- Go to the start of the returned parameter
         -- Otherwise, the search of "<" could find a file within parameters

         First := Index (Line, "<", From => First + 1);

         if First = 0 then
            IO.Put_Line ("No returned file named", Level => IO.Debug);
            -- Probably returning an error code:
            -- 8757  openat(...) = -1 ENOENT (No such file or directory)
            return No_File; -- file name not found

         else
            Last := Index (Line, ">", From => First + 1);
            if Last = 0 then
               -- IO.Put_Line ("Get_Returned_File : no closing >",
               --              Level => IO.Debug);
               return No_File;

            else
               declare
                  Name : constant String := Line (First + 1 .. Last - 1);
               begin
                  -- IO.Put_Line ("Get_Returned_File = " & Name,
                  --              Level => IO.Debug);
                  return To_Unbounded_String (Name);
               end;

            end if;

         end if;
      end Get_Returned_File;

      -- -----------------------------------------------------------------------
      function Write_Access return Boolean is
         WRONLY : constant String := "O_WRONLY";
         RDWR   : constant String := "O_RDWR";
         I      : Natural;
      begin
         I := Index (Line, WRONLY);
         if I /= 0 then
            return True;
         else
            return Index (Line, RDWR) /= 0;
         end if;
      end Write_Access;

      Cmd : constant String := Get_Command;

   begin
      -- Eliminate null lines,
      -- or lines that do not start with function name,
      -- or call that don't find a file (ENOENT)
      if Line'Length = 0
        or else Not_A_Function_Call (Line)
        -- file not found:
        or else Index (Line, "ENOENT",       Going => Ada.Strings.Backward) /= 0
      -- error accessing file:
        or else Index (Line, "EACCES",       Going => Ada.Strings.Backward) /= 0
      -- Ignore REMOVEDIR operation on Dir (used by open or unlink):
        or else Index (Line, "AT_REMOVEDIR", Going => Ada.Strings.Backward) /= 0
      then
         IO.Put_Line ("Ignoring strace line : " & Line, Level => IO.Debug);
         Operation := (Kind => None);
         return;

      end if;

      IO.Put_Line ("Analyzing strace line : " & Line, Level => IO.Debug);

      -- Fixme: if to be ordered according to occurence frequence

      if Cmd = "write"
        or  Cmd = "creat"
        or  Cmd = "link"
      then
         -- --------------------------------------------------------------------
         declare
            Name : constant File_Name := Get_Returned_File;
         begin
            IO.Put_Line ("write/creat/link " & (+Name), Level => IO.Debug);
            Operation := (Kind => Write,
                          Name => Name,
                          File => Create (File => Name,
                                          Role => Target));
         end;

      elsif Cmd = "rename" then
         -- --------------------------------------------------------------------
         -- 30461 rename("x.mp3", "unknown-unknown.mp3") = 0
         declare
            PID : constant Process_Id := Get_PID;
            WD  : constant File_Name  := Get_WD (PID);
            P1  : constant File_Name  := Get_File_Name;
            P2  : constant File_Name  := Get_File_Name;
            Source_Name : constant File_Name :=
                            Add_Dir (To_Name => P1, Dir => WD);
            Target_Name : constant File_Name :=
                            Add_Dir (To_Name => P2, Dir => WD);
         begin
            IO.Put_Line ("Rename " & (+Source_Name)
                         & " into " & (+Target_Name), Level => IO.Debug);
            Operation := (Kind        => Move,
                          Source_Name => Source_Name,
                          Source      => Create (File => Source_Name,
                                                 Role => Source),
                          Target_Name => Target_Name,
                          Target      => Create (File => Target_Name,
                                                 Role => Target));
         end;

      elsif Cmd = "renameat" or else Cmd = "renameat2" then
         -- --------------------------------------------------------------------
         -- 15232 renameat2(AT_FDCWD, "all.filecount.new",
         --                 AT_FDCWD, "all.filecount", RENAME_NOREPLACE) = 0
         -- or:
         -- 15165 renameat(5</home/lionel/.slocdata>, "old",
         --                5</home/lionel/.slocdata/>, "new")...
         declare
            PID : constant Process_Id := Get_PID;
            P1  : constant File_Name  := Get_Dir_Name (PID);
            P2  : constant File_Name  := Get_File_Name;
            P3  : constant File_Name  := Get_Dir_Name (PID);
            P4  : constant File_Name  := Get_File_Name;
            Source_Name : constant File_Name :=
                            Add_Dir (To_Name => P2, Dir => P1);
            Target_Name : constant File_Name :=
                            Add_Dir (To_Name => P4, Dir => P3);
         begin
            IO.Put_Line ("renameat " & (+Source_Name)
                         & " into " & (+Target_Name), Level => IO.Debug);
            Operation := (Kind        => Move,
                          Source_Name => Source_Name,
                          Source      => Create (File => Source_Name,
                                                 Role => Source),
                          Target_Name => Target_Name,
                          Target      => Create (File => Target_Name,
                                                 Role => Target));
         end;

      elsif Cmd = "open"
        or  Cmd = "fopen"
        or  Cmd = "openat"
      then
         -- --------------------------------------------------------------------
         -- 11750 openat(AT_FDCWD, "/tmp/ccvHeGYq.res", O_RDWR|O_CREAT|O_EXCL,
         --              0600) = 3</tmp/ccvHeGYq.res>
         -- 11750 openat(AT_FDCWD, "/etc/ld.so.cache", O_RDONLY|O_CLOEXEC)
         --                                               = 3</etc/ld.so.cache>
         declare
            Name : constant File_Name := Get_Returned_File;
         begin
            if Write_Access then
               IO.Put_Line ("Write open " & (+Name), Level => IO.Debug);
               Operation := (Kind => Write,
                             Name => Name,
                             File => Create (File => Name,
                                             Role => Target));
            else
               IO.Put_Line ("Read open " & (+Name), Level => IO.Debug);
               Operation := (Kind => Read,
                             Name => Name,
                             File => Create (File => Name,
                                             Role => Source));
            end if;
         end;

      elsif Cmd = "getcwd"
        or  Cmd = "getwd"
        or  Cmd = "get_current_dir_name"
      then
         -- --------------------------------------------------------------------
         declare
            PID : constant Process_Id := Get_PID;
            P1  : constant File_Name  := Get_File_Name;
            C   : constant Cursor     := Process_WD.Find (PID);
         begin
            if C = No_Element then
               IO.Put_Line ("Insert WD [" & Process_Id'Image (PID)
                            & "] = " & (+P1), Level => IO.Debug);
               Process_WD.Insert  (Key => PID, New_Item => P1);
            else
               IO.Put_Line ("Replacing WD [" & Process_Id'Image (PID)
                            & "] = " & (+P1), Level => IO.Debug);
               Process_WD.Replace (Key => PID, New_Item => P1);
            end if;
            Operation := (Kind => None);
         end;

      end if;

   exception
      when others =>
         Operation := (Kind => None);
         IO.Put_Line ("Error while analyzing : >" & Line & "<");
         IO.Put_Line ("Please submit this message to "
                      & "https://github.com/LionelDraghi/smk/issues/new "
                      & "with title ""Error analyzing strace output""");
         raise;

   end Analyze_Line;

--     Ignore_List     : constant Function_List := (new String'("stat"),
--                                                  new String'("access"),
--                                                  new String'("fstat"),
--                                                  new String'("lstat"),
--                                                  new String'("faccessat"),
--                                                  new String'("execve"),
--                                                  new String'("SIGCHLD"),
--                                                  new String'("fcntl"),
--                                                  new String'("lseek"),
--                                                  new String'("mknod"),
--                                                  new String'("umask"),
--                                                  new String'("close"),
--                                                  new String'("statfs"),
--                                                  new String'("fstatfs"),
--                                                  new String'("fstatat"),
--                                                  new String'("newfstatat"),
--                                                  new String'("freopen"),
--                                                  new String'("utimensat"),
--                                                  new String'("futimens"),
--                                                  new String'("chmod"),
--                                                  new String'("fchmod"),
--                                                  new String'("fchmodat"),
--                                                  new String'("chown"),
--                                                  new String'("lchown"),
--                                                  new String'("fchown"),
--                                                  new String'("fchownat"),
--                                                  new String'("unlink"),
--                                                  new String'("unlinkat"),
--                                                  new String'("remove"),
--                                                  new String'("mkdir"),
--                                                  new String'("rmdir"),
--                                                  new String'("chdir"));
--
-- --------------------------------------------------------------------------
--     function Is_Ignored (Call : String) return Boolean is
--       (for some C of Ignore_List => Call = C.all);

end Smk.Runs.Strace_Analyzer;
