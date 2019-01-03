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

package body Smk.Definitions is

   -- --------------------------------------------------------------------------
   function "+" (Section : Section_Names) return String is
     (To_String (Section));
   function "+" (Section : String) return Section_Names is
     (To_Unbounded_String (Section));

   -- --------------------------------------------------------------------------
   function "+" (Command : Command_Lines) return String is
     (To_String (Command));
   function "+" (Command : String) return Command_Lines is
     (To_Unbounded_String (Command));
   function "<" (Left, Right : Command_Lines) return Boolean is
     (+Left < +Right);
   function "&" (Left : Command_Lines; Right : String) return Command_Lines is
      (Left & (+Right));

end Smk.Definitions;
