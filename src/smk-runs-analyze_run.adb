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

with Smk.IO;                   use Smk.IO;
with Smk.Runfiles;
with Smk.Runs.Strace_Analyzer;
with Smk.Settings;             use Smk.Settings;

with Ada.Text_IO;

separate (Smk.Runs)

-- -----------------------------------------------------------------------------
procedure Analyze_Run (Assertions : out Condition_Lists.List) is

   Strace_Ouput : Ada.Text_IO.File_Type;

   use Smk.Runs.Strace_Analyzer;

   -- --------------------------------------------------------------------------
   procedure Add (Cond : in Condition) is
   begin
      IO.Put ("Add " & (+Cond.Name) & " " & Trigger_Image (Cond.Trigger),
             Level => IO.Debug);
      if not In_Ignore_List (+Cond.Name)
        and then (not Exists (+Cond.Name)
                  or else Kind (+Cond.Name) /= Special_File)
      then
         declare
            use Condition_Lists;
            C : constant Cursor := Assertions.Find (Cond);
         begin
            if C = No_Element then
               Assertions.Append (Cond);
               IO.Put (" inserted", Level => IO.Debug);
            else

               if Override (Cond.Trigger, Element (C).Trigger) then
                  IO.Put (" trigger modified, was "
                          & Trigger_Image (Element (C).Trigger),
                          Level => IO.Debug);
                  Assertions.Replace_Element (C, Cond);
                  -- else
                  --    IO.Put_Line (" not inserted");
               end if;
            end if;
         end;
      end if;
      IO.New_Line (Level => IO.Debug);
   end Add;

begin
   -- --------------------------------------------------------------------------
   Open (File => Strace_Ouput,
         Name => Strace_Outfile_Name,
         Mode => In_File);

   while not End_Of_File (Strace_Ouput) loop
      declare
         Line      : constant String  := Get_Line (Strace_Ouput);
         Operation : Operation_Type;

      begin
         Analyze_Line (Line, Operation);

         case Operation.Kind is
            when None   =>
               null;

            when Read   =>
               Add ((Name    => Operation.Name,
                     File    => Operation.File,
                     Trigger => File_Update));

            when Write  =>
               Add ((Name    => Operation.Name,
                     File    => Operation.File,
                     Trigger => File_Absence));

            when Delete =>
               Add ((Name    => Operation.Name,
                     File    => Operation.File,
                     Trigger => File_Presence));

            when Move   =>
               Add ((Name    => Operation.Source_Name,
                     File    => Operation.Source,
                     Trigger => File_Presence));
               Add ((Name    => Operation.Target_Name,
                     File    => Operation.Target,
                     Trigger => File_Absence));
               -- Fixme: pas un trigger ou une condition, à réorganiser

         end case;

      end;

   end loop;

   -- Add directories informations: if a directory was open, it may be
   -- because the command is searching files in it. In this case, we will
   -- take a picture of the dir (that is store all files even non accessed)
   -- to be able to compare the picture during future run, and detect added
   -- files in that dir.
   --
--     declare
--        Search   : Search_Type;
--        File     : Directory_Entry_Type;
--        New_Dirs : File_Lists.Map;
--
--     begin
--        for D in Dirs.Iterate loop
--           Start_Search (Search,
--                         Directory => +File_Lists.Key (D),
--                         Pattern   => "./*",
--                         Filter    => (Ordinary_File => True,
--                                       others        => False));
--           IO.Put_Line ("scanning dir " & (+File_Lists.Key (D)),
--                        Level => IO.Debug);
--           while More_Entries (Search) loop
--              Get_Next_Entry (Search, File);
--
--              if Settings.In_Ignore_List (Full_Name (File)) then
--                 IO.Put_Line ("Ignoring " & (Full_Name (File)),
--                              Level => IO.Debug);
--
--              elsif Is_Dir (Full_Name (File)) then
--                 -- Dir processing
--                 if Dirs.Contains (+Full_Name (File)) then
--                IO.Put_Line ("dir " & Simple_Name (File) & " already known");
--                 else
--                    IO.Put_Line ("recording not accessed new dir "
--                                 & Simple_Name (File));
--                    New_Dirs.Add   (+Full_Name (File),
--                                       Create (+Full_Name (File), Unused));
--
--                 end if;
--
--              else -- Fixme: partial code duplication
--                 -- File processing
--                 if Sources_And_Targets.Contains (+Full_Name (File)) then
--               IO.Put_Line ("file " & Simple_Name (File) & " already known");
--                 else
--                    IO.Put_Line ("recording not accessed new file "
--                                 & Simple_Name (File));
--                    Sources_And_Targets.Add
--                      (+Full_Name (File),
--                       Create (+Full_Name (File), Unused));
--
--                 end if;
--
--              end if;
--
--
--           end loop;
--
--        end loop;
--
--        -- Merge unused files into dir list:
--        for F in New_Dirs.Iterate loop
--           Dirs.Add (Key      => File_Lists.Key (F),
--                        New_Item => New_Dirs (F));
--        end loop;
--
--     end;
--
--     Runfiles.Reset (Counts);
--     Runfiles.Update_Counts (Sources_And_Targets, Counts);
--     -- Fixme: no dir in counts   Runfiles.Update_Counts (Dirs, Counts);

   if Debug_Mode
   then Close  (Strace_Ouput);
   else Delete (Strace_Ouput);
   end if;

end Analyze_Run;
