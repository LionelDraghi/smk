-- -----------------------------------------------------------------------------
-- smk, the smart make (https://github.com/LionelDraghi/smk)
-- SPDX-FileCopyrightText: 2019, 2026 Lionel Draghi
-- SPDX-License-Identifier: Apache-2.0
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
      Role        : constant String := Role_Image (Files.Role (C.File)) & " ";
      Because     : constant String := "because "
                      & Role
                      & (if Is_Dir (C.File) then "dir " else "file ")
                      & Shorten (C.Name);
      Status        : File_Status renames Files.Status (C.File);
      File_Exists   : constant Boolean := Status /= Missing;
      Is_The_Target : constant Boolean := Has_Target (C.Name,
                                                      Settings.Target_Name);
      -- Is_The_Target is True if C.Name is the target explicitly given
      -- on command line.

   begin
      if File_Exists and C.Trigger = File_Update and Status = Updated then
         -- --------------------------------------------------------------------
         Put_Explanation (Because
                          & " has been updated (" 
                          & IO.Image (Time_Tag (C.File)) & ")");
         return False;

      elsif not File_Exists and C.Trigger = File_Absence
        and (Is_Target (C.File)
             and (Settings.Build_Missing_Targets or Is_The_Target))
          -- if target and :
          -- - -mt option used, or
          -- - this target is explicitly given on command line
      then
         -- -----------------------------------------------------------------
         Put_Explanation (Because & " is missing");
         return False;

      elsif File_Exists and C.Trigger = File_Presence then
         -- --------------------------------------------------------------------
         Put_Explanation (Because & " is present");
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
      -- command never run before
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
