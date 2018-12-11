-- -----------------------------------------------------------------------------
-- smk, the smart make
-- © 2018 Lionel Draghi <lionel.draghi@free.fr>
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

with Ada.Directories;

separate (Smk.Main)

-- --------------------------------------------------------------------------
function Must_Be_Run (Command      :        Runfiles.Command_Lines;
                      Previous_Run : in out Runfiles.Run_Lists.Map)
                      return Boolean
is
   -- -----------------------------------------------------------------------
   procedure Put_Explanation (Text : in String) is
      use Smk.Runfiles;
   begin
      if Explain then
         IO.Put_Line ("run " & (+Command) & " " & Text);
      end if;
   end Put_Explanation;

   use Runfiles;

   -- -----------------------------------------------------------------------
   function A_Source_Is_Missing (The_Run : Run) return Boolean is
      use Ada.Directories;
      use Runfiles.File_Lists;
   begin
      for T in The_Run.Sources.Iterate loop
         declare
            Name : constant String := (+Key (T));
         begin
            if not Exists (Name) then
               IO.Put_Line ("Source " & Name & " is missing for command "
                            & (+Command));
               return True;
            end if;
         end;
      end loop;
      return False;
   end A_Source_Is_Missing;

   -- -----------------------------------------------------------------------
   function A_Target_Is_Missing (The_Run : Run) return Boolean is
      use Ada.Directories;
      use Runfiles.File_Lists;
   begin
      for T in The_Run.Targets.Iterate loop
         declare
            Name : constant String := (+Key (T));
         begin
            if not Exists (Name) then
               Put_Explanation ("because " & Name & " is missing");
               return True;
            end if;
         end;
      end loop;
      return False;
   end A_Target_Is_Missing;


   -- --------------------------------------------------------------------------
   function A_Source_Is_Updated (The_Run : Run) return Boolean is
      use Ada.Directories;
      use Runfiles.File_Lists;
   begin
      for S in The_Run.Sources.Iterate loop
         declare
            use Ada.Calendar;
            Name        : constant String := Full_Name (+Key (S));
            File_TT     : constant Time   := Modification_Time (Name);
            Last_Run_TT : constant Time   := Element (S).Time_Tag;
         begin
            if File_TT /= Last_Run_TT then
               Put_Explanation ("because " & Name & " (" & IO.Image (File_TT)
                                & ") has been updated since last run ("
                                & IO.Image (Last_Run_TT) & ")");
               return True;
            end if;
         end;
      end loop;
      return False;
   end A_Source_Is_Updated;

   -- --------------------------------------------------------------------------
   function No_Source_Nor_Target (The_Run : Run) return Boolean is
      use Runfiles.File_Lists;
   begin
      -- IO.Put_Line ("Is_Empty: Sources = "
      --              & Boolean'Image (Is_Empty (The_Run.Sources))
      --              & ", Targets = "
      --              & Boolean'Image (Is_Empty (The_Run.Targets)));
      return Is_Empty (The_Run.Sources) and Is_Empty (The_Run.Targets);
   end No_Source_Nor_Target;

   use Runfiles.Run_Lists;

   C : Runfiles.Run_Lists.Cursor;

begin
   -- -----------------------------------------------------------------------
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

   else return
        A_Source_Is_Missing (Runfiles.Run_Lists.Element (C)) or else
        A_Source_Is_Updated (Runfiles.Run_Lists.Element (C)) or else
        A_Target_Is_Missing (Runfiles.Run_Lists.Element (C)) or else
        No_Source_Nor_Target (Runfiles.Run_Lists.Element (C));
      -- If there is no sources and no target, it could be because the command
      -- failed in the previous run, and so let's try again.
   end if;

end Must_Be_Run;
