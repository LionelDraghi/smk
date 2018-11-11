-- -----------------------------------------------------------------------------
-- smk, the smart make
-- Copyright 2018 Lionel Draghi
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

-- -----------------------------------------------------------------------------
-- Package: Smk.IO specification
--
-- Purpose:
--
-- Effects:
--
-- Limitations:
--
-- Performance:
-- -----------------------------------------------------------------------------

with Smk.Settings;

private package Smk.IO is

   -- --------------------------------------------------------------------------
   procedure Put_Debug_Line (Msg    : in String := "";
                             Debug  : in Boolean;
                             Prefix : in String);
   -- procedure Put_Debug (Msg    : in String := "";
   --                      Debug  : in Boolean;
   --                      Prefix : in String);
   -- procedure New_Debug_Line (Debug  : in Boolean);

   -- --------------------------------------------------------------------------
   use type Settings.Print_Out_Level;
   subtype Print_Out_Level is Settings.Print_Out_Level;
   Debug   : constant Print_Out_Level := Settings.Debug;
   Verbose : constant Print_Out_Level := Settings.Verbose;
   Normal  : constant Print_Out_Level := Settings.Normal;
   Quiet   : constant Print_Out_Level := Settings.Quiet;

   -- --------------------------------------------------------------------------
   -- Mimics eponym Text_IO functions, except that :
   --   - if --quiet is set on command line, they have no effect,
   --     unless Even_In_Quiet_Mode is set.
   --   - if Only_When_Verbose is False, they have no effect
   --     unless --verbose is set on command line
   procedure Put_Line (Item  : String;
                       Level : Print_Out_Level := Normal);
   -- procedure Put      (Item  : String;
   --                     Level : Print_Out_Level := Normal);
   -- procedure New_Line (Spacing : Ada.Text_IO.Positive_Count := 1;
   --                     Level   : Print_Out_Level := Normal);

   -- --------------------------------------------------------------------------
   procedure Put_Warning   (Msg : in String := "");
   procedure Put_Error     (Msg : in String := "");
   procedure Put_Exception (Msg : in String := "");

   -- --------------------------------------------------------------------------
   -- Error_Count and Warning_Count return the number of call to Put_Error
   -- and Put_Warning.
   function Error_Count   return Natural;
   function Warning_Count return Natural;

   -- Some_Error return True if some error occured, or if some Warning
   -- occured and option to treat warning as error is set.
   function Some_Error return Boolean is
     (Error_Count /= 0 or
        (Settings.Warnings_As_Errors and Warning_Count /= 0));

end Smk.IO;
