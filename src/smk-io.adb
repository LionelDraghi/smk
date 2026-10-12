-- -----------------------------------------------------------------------------
-- smk, the smart make (https://github.com/LionelDraghi/smk)
-- SPDX-FileCopyrightText: 2018, 2019, 2026 Lionel Draghi
-- SPDX-License-Identifier: Apache-2.0
-- -----------------------------------------------------------------------------

with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Smk.IO is

   Warnings : Natural := 0;

   -- --------------------------------------------------------------------------
   -- Function: GNU_Prefix
   --
   -- Purpose:
   --    This function returns a source/line/column prefix for messages,
   --    compatible with the GNU standard
   --    (refer to <https://www.gnu.org/prep/standards/html_node/Errors.html>),
   --    that is:
   --       > program:sourcefile:lineno: message
   --    when there is an appropriate source file, or:
   --       > program: message
   --    otherwise.
   --
   -- --------------------------------------------------------------------------
   function GNU_Prefix (File   : in String;
                        Line   : in Integer := 0) return String
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      Trimed_File   : constant String := Trim (File, Side => Both);
      Trimed_Line   : constant String := Trim (Positive'Image (Line),
                                               Side => Both);
      Common_Part : constant String := "smk:" & Trimed_File;
   begin
      if File = "" then
         return "";
      elsif Line = 0 then
         return Common_Part & " ";
      else
         return Common_Part & ":" & Trimed_Line & ": ";
      end if;
   end GNU_Prefix;

   -- --------------------------------------------------------------------------
   procedure Put_Warning (Msg  : in String;
                          File : in String  := "";
                          Line : in Integer := 0) is
   begin
      Warnings := Warnings + 1;
      Put_Line ("Warning: " & Msg, File, Line);
      -- use the local version of Put_Line, and not the Ada.Text_IO one,
      -- so that warning messages are also ignored when --quiet.
   end Put_Warning;

   Errors : Natural := 0;

   -- --------------------------------------------------------------------------
   procedure Put_Error (Msg  : in String;
                        File : in String  := "";
                        Line : in Integer := 0) is
   begin
      Errors := Errors + 1;
      Put_Line ("Error: " & Msg, File, Line, Level => Quiet);
      -- Quiet because error messages should not be ignored
   end Put_Error;

   -- --------------------------------------------------------------------------
   procedure Put_Exception (Msg  : in String;
                            File : in String  := "";
                            Line : in Integer := 0) is
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                            GNU_Prefix (File, Line) & "Exception: " & Msg);
   end Put_Exception;

   -- --------------------------------------------------------------------------
   function Error_Count   return Natural is (Errors);
   function Warning_Count return Natural is (Warnings);

   -- --------------------------------------------------------------------------
   procedure Put_Debug_Line (Msg    : in String;
                             Debug  : in Boolean;
                             Prefix : in String;
                             File   : in String  := "";
                             Line   : in Integer := 0) is
   begin
      if Debug then
         Ada.Text_IO.Put_Line (GNU_Prefix (File, Line) & Prefix & Msg);
      end if;
   end Put_Debug_Line;

   -- --------------------------------------------------------------------------
   procedure Put_Line (Item  : String;
                       File  : in String       := "";
                       Line  : in Integer      := 0;
                       Level : Print_Out_Level := Normal) is
   begin
      if Level >= Settings.Verbosity then
         Ada.Text_IO.Put_Line (GNU_Prefix (File, Line) & Item);
      end if;
   end Put_Line;

   -- --------------------------------------------------------------------------
   procedure Put (Item  : String;
                  File  : in String       := "";
                  Line  : in Integer      := 0;
                  Level : Print_Out_Level := Normal) is
   begin
      if Level >= Settings.Verbosity then
         Ada.Text_IO.Put (GNU_Prefix (File, Line) & Item);
      end if;
   end Put;

   -- --------------------------------------------------------------------------
   procedure New_Line (Level : Print_Out_Level := Normal) is
   begin
      if Level >= Settings.Verbosity then
         Ada.Text_IO.New_Line;
      end if;
   end New_Line;

   -- --------------------------------------------------------------------------
   function Image (Time : in Ada.Calendar.Time) return String is
   begin
      return Ada.Calendar.Formatting.Image
        (Date                  => Time,
         Include_Time_Fraction => True,
         Time_Zone             => Ada.Calendar.Time_Zones.UTC_Time_Offset);
   end Image;

end Smk.IO;
