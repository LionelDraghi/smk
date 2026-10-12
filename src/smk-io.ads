-- -----------------------------------------------------------------------------
-- smk, the smart make (https://github.com/LionelDraghi/smk)
-- SPDX-FileCopyrightText: 2018, 2019, 2026 Lionel Draghi
-- SPDX-License-Identifier: Apache-2.0
-- -----------------------------------------------------------------------------

with Ada.Calendar;

with Smk.Settings; use Smk.Settings;

private package Smk.IO is

   -- --------------------------------------------------------------------------
   procedure Put_Debug_Line (Msg    : in String;
                             Debug  : in Boolean;
                             Prefix : in String;
                             File   : in String  := "";
                             Line   : in Integer := 0);

   -- --------------------------------------------------------------------------
   subtype Print_Out_Level is Settings.Print_Out_Level;
   Debug   : constant Print_Out_Level := Settings.Debug;
   Verbose : constant Print_Out_Level := Settings.Verbose;
   Normal  : constant Print_Out_Level := Settings.Normal;
   Quiet   : constant Print_Out_Level := Settings.Quiet;

   -- --------------------------------------------------------------------------
   -- Mimics the eponymous Text_IO functions, except that:
   --   - if --quiet is set on command line, they have no effect,
   --     unless Even_In_Quiet_Mode is set.
   --   - if Only_When_Verbose is False, they have no effect
   --     unless --verbose is set on command line
   procedure Put_Line (Item  : String;
                       File  : in String  := "";
                       Line  : in Integer := 0;
                       Level : Print_Out_Level := Normal);
   procedure Put (Item  : String;
                  File  : in String  := "";
                  Line  : in Integer := 0;
                  Level : Print_Out_Level := Normal);
   procedure New_Line (Level : Print_Out_Level := Normal);

   -- --------------------------------------------------------------------------
   procedure Put_Warning   (Msg  : in String;
                            File : in String  := "";
                            Line : in Integer := 0);
   procedure Put_Error     (Msg  : in String;
                            File : in String  := "";
                            Line : in Integer := 0);
   procedure Put_Exception (Msg  : in String;
                            File : in String  := "";
                            Line : in Integer := 0);

   -- --------------------------------------------------------------------------
   -- Error_Count and Warning_Count return the number of calls to Put_Error
   -- and Put_Warning.
   function Error_Count   return Natural;
   function Warning_Count return Natural;

   -- --------------------------------------------------------------------------
   -- Some_Error returns True if some error occurred, or if some warning
   -- occurred and the option to treat warnings as errors is set.
   function Some_Error return Boolean is
     (Error_Count /= 0 or
        (Settings.Warnings_As_Errors and Warning_Count /= 0));

   -- --------------------------------------------------------------------------
   function Image (Time : in Ada.Calendar.Time) return String;
   -- To ensure common options on calls to Ada.Calendar.Formatting.Image,
   -- that is:
   -- 1. Include_Time_Fraction => True,
   --    to get hundredths of a second printed in the hope that
   --    it will be useful in a future version, or on other OSes.
   -- 2. Time_Zone             => Ada.Calendar.Time_Zones.UTC_Time_Offset
   --    to get the same file time tag printed in local time as the user
   --     would see with a simple ls -l

end Smk.IO;
