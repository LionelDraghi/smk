-- -----------------------------------------------------------------------------
-- smk, the smart make (https://github.com/LionelDraghi/smk)
-- SPDX-FileCopyrightText: 2018, 2019, 2026 Lionel Draghi
-- SPDX-License-Identifier: Apache-2.0
-- -----------------------------------------------------------------------------

with Smk.Assertions;
with Smk.IO;
with Smk.Runs.Observers;       use Smk.Runs.Observers;

with Ada.Calendar;

separate (Smk.Runs)

-- -----------------------------------------------------------------------------
procedure Run_Command (E            : in out Smkfiles.Smkfile_Entry;
                       The_Run_List : in out Runfiles.Run_Lists.Map;
                       Cmd_To_Run   :    out Boolean;
                       Error_In_Run :    out Boolean)
is
   use Smk.Runfiles;
   OK           : Boolean;
   New_Run_Time : Ada.Calendar.Time;
   Assertions   : Condition_Lists.List;
   Events       : Operation_Lists.List;

begin
   -- --------------------------------------------------------------------------
   if Must_Be_Run (E.Command, The_Run_List) then

      Cmd_To_Run := True;

      if Settings.Dry_Run then
         -- don't run, just print the command
         IO.Put_Line ("> " & (+E.Command));
         E.Was_Run    := True;
         Error_In_Run := False;

      else
         -- 1. Run the command, and observe its file accesses
         New_Run_Time := Ada.Calendar.Clock;

         IO.Put_Line ((+E.Command));
         Observe (Cmd     => E.Command,
                  Events  => Events,
                  Success => OK);

         E.Was_Run    := OK;
         Error_In_Run := not OK;

         if not OK and not Settings.Keep_Going then
            return;
         end if;

         if OK then
            -- 2. Analyze the observed file operations
            Analyze_Run (Events, Assertions);

            -- 3. Store the results
            Insert_Or_Update
              (The_Command => E.Command,
               The_Run     => (Section    => E.Section,
                               Run_Time   => New_Run_Time,
                               Assertions => Assertions),
               In_Run_List => The_Run_List);
         end if;

      end if;

   else
      E.Was_Run  := False;
      Cmd_To_Run := False;

      IO.Put_Line ("No need to run " & (+E.Command),
                   Level => IO.Verbose);
      Error_In_Run := False;

   end if;

end Run_Command;
