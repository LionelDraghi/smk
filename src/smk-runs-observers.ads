-- -----------------------------------------------------------------------------
-- smk, the smart make (https://github.com/LionelDraghi/smk)
-- SPDX-FileCopyrightText: 2018, 2019, 2026 Lionel Draghi
-- SPDX-License-Identifier: Apache-2.0
-- -----------------------------------------------------------------------------

with Smk.Definitions; use Smk.Definitions;
with Smk.Files;       use Smk.Files;

with Ada.Containers.Doubly_Linked_Lists;

private package Smk.Runs.Observers is

   -- --------------------------------------------------------------------------
   -- This package is the abstraction layer over the OS mechanism used to
   -- observe the file accesses of the commands run by smk.
   -- The event model below is OS independent; the mechanism that produces
   -- the events is selected by the Observe implementation (design
   -- discussion D1: direct ptrace on Linux, ktrace on BSDs...).

   -- --------------------------------------------------------------------------
   type Operation_Kind is (None,
                           Read,
                           Write,
                           Delete,
                           Move);

   -- --------------------------------------------------------------------------
   type Operation_Type (Kind : Operation_Kind := None) is record
      case Kind is
         when None =>
            null;
         when Read
            | Write
            | Delete =>
            Name : File_Name;
            File : File_Type;
         when Move =>
            Source_Name : File_Name;
            Source      : File_Type;
            Target_Name : File_Name;
            Target      : File_Type;
      end case;
   end record;

   -- --------------------------------------------------------------------------
   package Operation_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Operation_Type);

   -- --------------------------------------------------------------------------
   procedure Observe (Cmd     : in     Command_Lines;
                      Events  :    out Operation_Lists.List;
                      Success :    out Boolean);
   -- Run the Cmd in the run directory, observing the file accesses of
   -- the command and of all its children processes.
   -- Events contains one event per successful file operation, in the
   -- order of the operations.
   -- Success is set to False if the command could not be run, or if it
   -- terminated with a non-zero status.

end Smk.Runs.Observers;
