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

with Smk.Files.File_Lists;

function Smk.Files.Unused (Sources_And_Targets : File_Lists.Map;
                           Root_Dir            : String := "./";
                           Recursive           : Boolean)
                           return File_Lists.Map
is

   Current : constant String := Current_Directory;
   File_List : File_Lists.Map;
   use Ada.Directories;

   -- -----------------------------------------------------------------------
   procedure Walk (Name : String) is
      -- code mostly from :
      -- https://rosettacode.org/wiki/Walk_a_directory/Recursively#Ada

      Extension : constant String
        := File_Extensions (Processor_List (L).all); -- dispatching call

      -- --------------------------------------------------------------------
      procedure Print (Item : Directory_Entry_Type) is
         Name : constant String := Full_Name (Item);
         use type Sources.File_Name;
      begin
         if Name'Length > Current'Length and then
           Name (Name'First .. Name'First + Current'Length - 1) = Current
         -- Simple optimisation : if the long path is a subdir of the
         -- current one, we only print the subdir
         then
            IO.Put_Line ("     opt" &
                         +(Name (Name'First + Current'Length + 1 .. Name'Last)));
         else
            IO.Put_Line (" non opt" & +(Name));
         end if;
      end Print;

      -- --------------------------------------------------------------------
      procedure Walk (Item : Directory_Entry_Type) is
      begin
         if Simple_Name (Item) /= "." and then Simple_Name (Item) /= ".."
         then
            -- This is OK with Unix and Windows dir, so I consider
            -- it as portable.
            Walk (Full_Name (Item));
         end if;
      exception
         when Name_Error => null;
      end Walk;

   begin
      Search (Name, Extension, (Directory => False, others => True),
              Print'Access);
      if Recursive then
         Search (Name, "", (Directory => True, others => False), Walk'Access);
      end if;

   end Walk;

begin
   Put_Debug_Line (Msg => "Analysing directory " & Root_Dir);
   Walk (Root_Dir);

end Smk.Files.Unused;
