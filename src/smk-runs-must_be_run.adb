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
function Must_Be_Run (Command      :        Command_Lines;
                      Previous_Run : in out Runfiles.Run_Lists.Map)
                      return Boolean
is
   -- --------------------------------------------------------------------------
   procedure Put_Explanation (Text : in String) is
   begin
      if Explain then
         IO.Put_Line ("run """ & (+Command) & """ " & Text);
      end if;
   end Put_Explanation;

   use Runfiles;

   -- --------------------------------------------------------------------------
   function A_File_Is_Missing (The_Run : Runfiles.Run;
                               Role    : Files.File_Role) return Boolean is
      -- use Ada.Directories;
      use File_Lists;
   begin
      for File in The_Run.Files.Iterate loop
         declare
            Name : constant String := (+File_Lists.Key (File));
         begin
            if Files.Role (Element (File)) = Role
              and then Status (Element (File)) = Missing
            -- and then not Exists (Name)
            then
               Put_Explanation ("because " & Role_Image (Role) & " " & Name
                            & " is missing");
               return True;
            end if;
         end;
      end loop;
      return False;
   end A_File_Is_Missing;

   -- --------------------------------------------------------------------------
   function A_Source_File_Is_Updated (The_Run : Runfiles.Run) return Boolean is
      use File_Lists;
   begin
      for I in The_Run.Files.Iterate loop
         if Is_Source (Element (I)) and Status (Element (I)) = Updated
         then
            Put_Explanation ("because " & (+Key (I)) & " ("
                             & IO.Image (Time_Tag (Element (I)))
                             & ") has been updated since last run ("
                             & IO.Image (The_Run.Run_Time) & ")");
            return True;
         end if;
      end loop;
      return False;
   end A_Source_File_Is_Updated;

   -- --------------------------------------------------------------------------
   function A_Source_Dir_Is_Updated (The_Run : Runfiles.Run) return Boolean is
   begin
      for File in The_Run.Dirs.Iterate loop
         declare
            use File_Lists;
            Name : constant String := (+File_Lists.Key (File));
         begin
            if Role (Element (File)) = Source and
              Status (Element (File)) = Updated
            then
               Put_Explanation ("because dir " & Name & " is updated");
               return True;
            end if;
         end;
      end loop;
      return False;
   end A_Source_Dir_Is_Updated;

   use Run_Lists;

   C            : Run_Lists.Cursor;
   Updated_List : File_Lists.Map;

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

   Update_Files_Status (Previous_Run (C).Files, Updated_List);
   Update_Dirs_Status  (Previous_Run (C),       Updated_List);
   if Debug_Mode then
      Put_Updated (Updated_List);
   end if;

   return A_File_Is_Missing    (Run_Lists.Element (C), Target)
     or else A_File_Is_Missing (Run_Lists.Element (C), Source)
     or else A_Source_File_Is_Updated (Run_Lists.Element (C))
     or else A_Source_Dir_Is_Updated  (Run_Lists.Element (C));
   -- or else No_Source_Nor_Target (Run_Lists.Element (C));
   -- If there is no sources and no target, it could be because the command
   -- failed in the previous run, and so let's try again.

end Must_Be_Run;
