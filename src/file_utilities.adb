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

with Ada.Directories; use Ada.Directories;
-- with Ada.Text_IO;     use Ada.Text_IO;

package body File_Utilities is

   Upper_Dir : constant String := ".." & Separator;

   -- --------------------------------------------------------------------------
   function Short_Path (From_Dir : String;
                        To_File  : String;
                        Prefix   : String := "") return String   is
   begin
       if From_Dir = (1 => Separator) then return To_File; end if;
      -- This test is also the way to stop recursing until error
      -- when From_Dir and To_File have nothing in common.

      if From_Dir (From_Dir'Last) = Separator then
         -- Transform From_Dir
         -- from /home/lionel/Proj/smk/tests/
         -- to   /home/lionel/Proj/smk/tests
         -- Next to this, From_Dir no more ends with a Separator.
         return Short_Path (From_Dir (From_Dir'First .. From_Dir'Last - 1),
                            To_File,
                            Prefix);
      end if;

      if From_Dir'Length < To_File'Length and then
        To_File (To_File'First .. To_File'First + From_Dir'Length - 1)
        = From_Dir
      -- The left part of both string is identical
      -- e.g.:
      --    From_Dir = /home/lionel/Proj/smk/tests
      --    To_File  = /home/lionel/Proj/smk/tests/mysite/idx.txt
      then
         declare
            Right_Part : constant String := To_File
              (To_File'First + From_Dir'Length + 1 .. To_File'Last);
         begin
            -- "/home/lionel/tests",
            -- "/home/lionel/tests/mysite/site/d1/idx.txt"
            return Prefix &
              Right_Part (Right_Part'First .. Right_Part'Last);
         end;

      else
         -- To_File'length <= From_Dir'length, e.g.:
         --    From_Dir = /home/tests/mysite/site/
         --    To_File  = /home/readme.txt
         -- or else From_Dir is not a To_File's parent, e.g.:
         --    From_Dir = /home/lionel/Proj/12/34
         --    To_File  = /home/lionel/Proj/mysite/site/idx.txt

         -- Put_Line ("Dir        = " & From_Dir);
         -- Put_Line ("Parent Dir = " & Containing_Directory (From_Dir));
         -- Put_Line ("Prefix     = " & Prefix & Upper_Dir);
         -- New_Line;
         -- recursive call:
         return Short_Path (From_Dir => Containing_Directory (From_Dir),
                            To_File  => To_File,
                            Prefix   => Prefix & Upper_Dir);
      end if;

   end Short_Path;

end File_Utilities;
