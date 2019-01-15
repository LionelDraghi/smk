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

with Smk.Files; use Smk.Files;

private package Smk.Runs.Strace_Analyzer is

   -- --------------------------------------------------------------------------
   type Line_Type is (Read_Call,
                      Write_Call,
                      Read_Write_Call,
                      Exec_Call,
                      Ignored);

   -- --------------------------------------------------------------------------
   type Operation_Kind is (None,
                           Read,
                           Write,
                           Delete,
                           Move);

   -- --------------------------------------------------------------------------
   type Operation_Type (Kind : Operation_Kind := None) is record
      case Kind is
         when None => null;
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
   procedure Analyze_Line (Line       : in     String;
                           Operation  :    out Operation_Type);
   -- Here is the kind of output from strace that we process here:
   --
   -- 19171 rename("x.mp3", "Luke-Sentinelle.mp3") = 0
   -- 4372  openat(AT_FDCWD, "/tmp/ccHKHv8W.s", O_RDWR|O_CREAT ...
   -- 12345 open("xyzzy", O_WRONLY|O_APPEND|O_CREAT, 0666) = 3
   -- 15165 unlinkat(5</home/lionel/.slocdata/top_dir>, "filelist", 0) = 0
   -- 15168 openat(AT_FDCWD, "/home/lionel/.sl", O_RDONLY <unfinished ...>
   -- 15167 <... stat resumed> {st_mode=S_IFREG|0755, st_size=122224, ...}) = 0
   -- 15232 renameat2(AT_FDCWD, "all.filect.new", AT_FDCWD, "all.filect"...
   -- 15214 --- SIGCHLD {si_signo=SIGCHLD, si_code=CLD_EXITED, ...

end Smk.Runs.Strace_Analyzer;
