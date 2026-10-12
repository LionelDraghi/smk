-- -----------------------------------------------------------------------------
-- smk, the smart make (https://github.com/LionelDraghi/smk)
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

with Smk.Definitions; use Smk.Definitions;
with Smk.Files;       use Smk.Files;

with Smk.Runs.Observers; use Smk.Runs.Observers;

private package Smk.Runs.Strace_Analyzer is

   -- --------------------------------------------------------------------------
   type Line_Type is (Read_Call,
                      Write_Call,
                      Read_Write_Call,
                      Exec_Call,
                      Ignored);

   -- --------------------------------------------------------------------------
   procedure Observe (Cmd     : in     Command_Lines;
                      Events  :    out Operation_Lists.List;
                      Success :    out Boolean);
   -- Spawn the Cmd under strace in the run directory, then analyze the
   -- strace output file: one event is pushed in Events per successful
   -- file operation.
   -- Success is set to False if the command could not be run, or if it
   -- terminated with a non-zero status.

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
   -- 7120  mkdir("dir1", 0777)               = 0

end Smk.Runs.Strace_Analyzer;
