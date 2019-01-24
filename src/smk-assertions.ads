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

with Smk.Files;                          use Smk.Files;

-- with Ada.Calendar;                       use Ada.Calendar;
with Ada.Containers.Doubly_Linked_Lists;

private package Smk.Assertions is

   -- --------------------------------------------------------------------------
   type Trigger_Type is (No_Trigger,
                         File_Update,
                         File_Presence,
                         File_Absence);
   function Trigger_Image (Trigger : Trigger_Type) return String is
     (case Trigger is
         when No_Trigger    => "No trigger ",
         when File_Update   => "If update  ",
         when File_Presence => "If presence",
         when File_Absence  => "If absence ");

   Override : constant array (Trigger_Type, Trigger_Type) of Boolean :=
                (No_Trigger    => (others => False),
                 File_Update   => (No_Trigger => True,
                                   others     => False),
                 File_Presence => (others => True),
                 File_Absence  => (others => True));

   type Condition is record
      File    : Files.File_Type;
      Name    : Files.File_Name;
      Trigger : Trigger_Type;
   end record;

   -- --------------------------------------------------------------------------
   function "=" (L, R : Condition) return Boolean is
     (L.Name = R.Name and Role (L.File) = Role (R.File));
   -- Equality is based on Name, but we also discriminate Sources from Targets

   -- --------------------------------------------------------------------------
   function Image (A      : Condition;
                   Prefix : String := "") return String;
   -- return an Image of that kind:
   -- Prefix & [Pre :file exists]

   -- --------------------------------------------------------------------------
   package Condition_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Condition);
   -- NB: "=" redefinition modify Contains (and other operations)

   -- --------------------------------------------------------------------------
   function Name_Order (Left, Right : Condition) return Boolean is
      (Left.Name < Right.Name);
   package Name_Sorting is new Condition_Lists.Generic_Sorting (Name_Order);

--     function Time_Order (Left, Right : Condition) return Boolean is
--       (Time_Tag (Left.File) < Time_Tag (Right.File));
--     package Time_Sorting is new Condition_Lists.Generic_Sorting (Time_Order);

   -- --------------------------------------------------------------------------
   type File_Count is new Natural;
   function Count_Image (Count : File_Count) return String;

   -- --------------------------------------------------------------------------
   function Count (Cond_List         : Condition_Lists.List;
                   Count_Sources     : Boolean := False;
                   Count_Targets     : Boolean := False;
                   With_System_Files : Boolean := False)
                   return File_Count;

   -- --------------------------------------------------------------------------
   type Rule_Kind is (Pattern_Rule, Simple_Rule);
   --     type Rule (Kind : Rule_Kind) is record
   --        when Pattern_Rule =>
   --          From : Unbounded_String;
   --        To   : Unbounded_String;
   --        when Simple_Rule =>
   --        null;
   --     end record;

end Smk.Assertions;
