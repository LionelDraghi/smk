-- -----------------------------------------------------------------------------
-- smk, the smart make (http://lionel.draghi.free.fr/smk/)
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
with GNAT.OS_Lib;
with Smk.IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed;

separate (Smk.Main)

-- -----------------------------------------------------------------------------
procedure Run_All_Commands (The_Smkfile   : in out Smkfiles.Smkfile;
                            The_Run_List  : in out Runfiles.Run_Lists.Map;
                            Cmd_To_Run    :    out Boolean;
                            Error_In_Run  :    out Boolean;
                            Section_Found :    out Boolean;
                            Target_Found  :    out Boolean)
is
   Cmd_Run_During_Outer : Boolean := False;

   -- those boolean cumulate the result over the loop,
   -- that will be put in the respective output parameter.
   Some_Command_To_Run  : Boolean := False;
   Some_Error_In_Run    : Boolean := False;

   use type Runfiles.Section_Names;

begin
   Cmd_To_Run    := False;
   Error_In_Run  := False;
   Section_Found := False;
   Target_Found  := False;

   Outer : for I in 1 .. The_Smkfile.Entries.Length loop
      -- This double loop is a pragmatic way to avoid a more complex
      -- dependies analysis.
      --
      -- Why: build command may appears not ordered in the Makefile.
      -- What should we do if a command invalidate another one that was
      -- before in The_Smkfile.Entries?
      -- Exemple :
      -- 1. gcc -o hello hello.o main.o
      -- 2. gcc -o hello.o -c hello.c
      -- 3. gcc -o main.o -c main.c
      -- If main.c is changed, a single loop approach will
      -- re-run 3. and exit.
      -- The double loop will re-loop, and run 1.
      -- If during the inner loop, nothing is run, then OK,
      -- we exit the outer loop.
      --
      -- The the worst order will cause as many loop as
      -- The_Smkfile.Entries. Otherwise, it means that there is
      -- a circular dependency.
      -- To avoid infinite recursion in that case, the outer loop is
      -- limited to the worst case, that is The_Smkfile.Entries.Length.

      Cmd_Run_During_Outer := False;

      Inner : for E of The_Smkfile.Entries loop

         if Settings.Section_Name = "" or else
           Settings.Section_Name = E.Section
         then
            Section_Found := True;
            Target_Found  := Runfiles.Has_Target (The_Run_List,
                                                  Settings.Target_Name);

            if Settings.Target_Name = "" or else Target_Found then

               if not E.Was_Run then
                  Run_Command
                    (E, The_Run_List, Some_Command_To_Run, Some_Error_In_Run);

                  -- sum the results:
                  Cmd_To_Run   := Cmd_To_Run   or Some_Command_To_Run;
                  Error_In_Run := Error_In_Run or Some_Error_In_Run;

                  if E.Was_Run then
                     Cmd_Run_During_Outer := True;
                  end if;

                  if Some_Error_In_Run and not Keep_Going then
                     exit Outer;
                  end if;

               end if;

            end if;

         end if;

      end loop Inner;

      -- Naive loop aproach : each time a cmd is run, and potentialy
      -- invalidate another cmd, we restart the whole cmd list, until
      -- no command is re-run.
      exit Outer when not Cmd_Run_During_Outer;

   end loop Outer;

end Run_All_Commands;
