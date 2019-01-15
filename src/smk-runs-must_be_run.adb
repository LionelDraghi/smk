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
with Smk.Settings;    use Smk.Settings;

separate (Smk.Runs)

-- -----------------------------------------------------------------------------
function Must_Be_Run (Command      : in     Command_Lines;
                      Previous_Run : in out Runfiles.Run_Lists.Map)
                      return Boolean
is
   -- --------------------------------------------------------------------------
   procedure Put_Explanation (Text : in String) is
   begin
      if Settings.Explain then
         IO.Put_Line ("run """ & (+Command) & """ " & Text);
      end if;
   end Put_Explanation;


   use Runfiles;
   use Run_Lists;

   C            : Run_Lists.Cursor;
   Updated_List : Condition_Lists.List;

   -- --------------------------------------------------------------------------
   function Assert (C : Condition) return Boolean is
      File_Exists : constant Boolean := Ada.Directories.Exists (+C.Name);
      -- Fixme: not sure this call is needed, why not use the Status?
      Because     : constant String := "because "
                      & (if Is_Dir (C.File) then "dir " else "")
                      & Shorten (C.Name);
      Status      : File_Status renames Files.Status (C.File);

   begin
      if not File_Exists and C.Trigger = File_Absence and Is_Source (C.File)
      then
         -- --------------------------------------------------------------------
         Put_Explanation (Because & " source is missing");
         return False;

      elsif not File_Exists and C.Trigger = File_Absence
        and (Is_Target (C.File) and Settings.Build_Missing_Targets)
      then
            -- -----------------------------------------------------------------
            Put_Explanation (Because & " is missing");
            return False;

      elsif File_Exists and C.Trigger = File_Presence then
         -- --------------------------------------------------------------------
         Put_Explanation (Because & " is present");
         return False;

      elsif File_Exists and C.Trigger = File_Update and Status = Updated then
         -- --------------------------------------------------------------------
         Put_Explanation (Because
         & " has been updated (" & IO.Image (Time_Tag (C.File)) & ")");
         return False;

      elsif File_Exists and C.Trigger = File_Update and Status = New_File then
         -- --------------------------------------------------------------------
         Put_Explanation ("because of new"
                          & (if Is_Dir (C.File) then " dir " else " file ")
                          & Shorten (C.Name));
         return False;

      else
         -- --------------------------------------------------------------------
         -- assertions are OK
         return True;

      end if;

   end Assert;


begin
   -- --------------------------------------------------------------------------
   if Always_Make then
      -- don't even ask, run it!
      Put_Explanation ("because -a option is set");
      return True;
   end if;

   C := Previous_Run.Find (Command);

   if C = No_Element then
      -- never run command
      Put_Explanation ("because it was not run before");
      return True;
   end if;

   Update_Files_Status (Previous_Run (C).Assertions, Updated_List);
   if Debug_Mode then
      Put_Updated (Updated_List);
   end if;

   --
   for P of Element (C).Assertions loop
      if not Assert (P) then return True; end if;
   end loop;

   return False;

end Must_Be_Run;
