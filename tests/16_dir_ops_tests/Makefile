# ------------------------------------------------------------------------------
# smk, the smart make (http://lionel.draghi.free.fr/smk/)
#  © 2018, 2019 Lionel Draghi <lionel.draghi@free.fr>
# SPDX-License-Identifier: APSL-2.0
# ------------------------------------------------------------------------------
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# ------------------------------------------------------------------------------

TR=../Tools/testrec

.PHONY : clean

check: ../../smk header mkdir1 updatedir1 cleandir1 accessdir1 rmdir1

header: clean
	@ #-------------------------------------------------------------------------
	${TR} create "Directory tests"
	
mkdir1: header
	@ #-------------------------------------------------------------------------
	${TR} start 'mkdir dir1'
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk run mkdir dir1`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.10 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk run mkdir dir1 >  out.10
	${TR} assert true /usr/bin/sdiff "-sbB expected.10 out.10" || meld expected.10 out.10 

	${TR} cmt 'Run:'
	${TR} cmt '`smk st -l`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.11 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk st -l | \
	  sed "s/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g" | \
      sed "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g" >  out.11
	${TR} assert true /usr/bin/sdiff "-sbB expected.11 out.11" || meld expected.11 out.11 

	${TR} cmt 'Run:'
	${TR} cmt '`smk ls`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.12 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk ls >  out.12
	${TR} assert true /usr/bin/sdiff "-sbB expected.12 out.12" || meld expected.12 out.12

	${TR} cmt 'Run:'
	${TR} cmt '`smk lt`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.13 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk lt >  out.13
	${TR} assert true /usr/bin/sdiff "-sbB expected.13 out.13" || meld expected.13 out.13

	${TR} cmt 'Run:'
	${TR} cmt '`smk lu`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.14 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk lu >  out.14
	${TR} assert true /usr/bin/sdiff "-sbB expected.14 out.14" || meld expected.14 out.14

	${TR} cmt 'Run:'
	${TR} cmt '`smk wn`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.15 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk wn >  out.15
	${TR} assert true /usr/bin/sdiff "-sbB expected.15 out.15" || meld expected.15 out.15

	${TR} end

updatedir1:
	@ #-------------------------------------------------------------------------
	${TR} start 'updating dir1'
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk run touch dir1/f1`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.20 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	sleep 1.0
	../../smk run touch dir1/f1 >  out.20
	${TR} assert true /usr/bin/sdiff "-sbB expected.20 out.20" || meld expected.20 out.20 

	${TR} cmt 'Run:'
	${TR} cmt '`smk st -l`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.21 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk st -l | \
	  sed "s/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g" | \
      sed "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g" >  out.21
	${TR} assert true /usr/bin/sdiff "-sbB expected.21 out.21" || meld expected.21 out.21 

	${TR} cmt 'Run:'
	${TR} cmt '`smk ls`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.22 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk ls >  out.22
	${TR} assert true /usr/bin/sdiff "-sbB expected.22 out.22" || meld expected.22 out.22

	${TR} cmt 'Run:'
	${TR} cmt '`smk lt`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.23 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk lt > out.23
	${TR} assert true /usr/bin/sdiff "-sbB expected.23 out.23" || meld expected.23 out.23

	${TR} cmt 'Run:'
	${TR} cmt '`smk lu`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.24 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk lu > out.24
	${TR} assert true /usr/bin/sdiff "-sbB expected.24 out.24" || meld expected.24 out.24

	${TR} cmt 'Run:'
	${TR} cmt '`smk wn`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.25 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk wn >  out.25
	${TR} assert true /usr/bin/sdiff "-sbB expected.25 out.25" || meld expected.25 out.25

	${TR} cmt 'Run:'
	${TR} cmt '`smk`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.26 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk >  out.26
	${TR} assert true /usr/bin/sdiff "-sbB expected.26 out.26" || meld expected.26 out.26

	${TR} cmt 'Run:'
	${TR} cmt '`touch dir1`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt "It's a target, so touching it should not run the command"
	${TR} cmt '```' 
	cat expected.26 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk > out.26
	${TR} assert true /usr/bin/sdiff "-sbB expected.26 out.26" || meld expected.26 out.26

	${TR} end

cleandir1: clean
	@ #-------------------------------------------------------------------------
	${TR} start 'cleaning dir1'
	${TR} cmt 
	${TR} cmt 'This test check that smk correctly identify target files,'
	${TR} cmt 'and remove it with "clean", and preserve "unused" files,'
	${TR} cmt 'even if those are in a target dir.'
	${TR} cmt
	${TR} cmt 'Run:'
	${TR} cmt '`smk run mkdir dir1`'
	${TR} cmt '`smk run touch dir1/f1`'
	${TR} cmt '`smk run mkdir dir2`'
	${TR} cmt '`smk run mv dir1/* dir2`'
	${TR} cmt '`touch dir1/f5`'
	${TR} cmt '`mkdir dir2/dir3`'
	${TR} cmt '`smk st`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.40 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -q run mkdir dir1
	../../smk -q run touch dir1/f1
	../../smk -q run mkdir dir2
	../../smk -q run mv dir1/* dir2
	@ touch dir1/f5
	@ mkdir dir2/dir3

	../../smk st | \
	  sed "s/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g" | \
      sed "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g" >  out.40
	${TR} assert true /usr/bin/sdiff "-sbB expected.40 out.40" || meld expected.40 out.40 

	${TR} cmt 'Run:'
	${TR} cmt '`smk lt -l`'
	${TR} cmt '(files that should be erased when cleaning)'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.41 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	sleep 1.0
	../../smk lt -l | \
	  sed "s/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g" | \
      sed "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g" >  out.41
	${TR} assert true /usr/bin/sdiff "-sbB expected.41 out.41" || meld expected.41 out.41 

	${TR} cmt 'Run:'
	${TR} cmt '`smk lu -l`'
	${TR} cmt '(files that should not be erased when cleaning)'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.42 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk lu -l | \
	  sed "s/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g" | \
      sed "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g" >  out.42
	${TR} assert true /usr/bin/sdiff "-sbB expected.42 out.42" || meld expected.42 out.42 

	${TR} cmt 'Run:'
	${TR} cmt '`smk clean`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.43 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk clean >  out.43
	${TR} assert true /usr/bin/sdiff "-sbB expected.43 out.43" || meld expected.43 out.43 

	${TR} end

accessdir1: clean
	@ ../../smk -q reset
	@ - rm -rf default.smk dir?
	
	@ #-------------------------------------------------------------------------
	${TR} start 'accessing dir1 contents, write access'
	${TR} cmt 'Run:'
	${TR} cmt '`mkdir -p dir1`'
	${TR} cmt '`smk run mkdir dir1/dir2`'
	${TR} cmt '`smk st -l`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.30 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	mkdir -p dir1
	../../smk -q run mkdir dir1/dir2
	../../smk st -l | \
	  sed "s/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g" | \
      sed "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g" > out.30
	${TR} assert true /usr/bin/sdiff "-sbB expected.30 out.30" || meld expected.30 out.30

	${TR} cmt 'Run:'
	${TR} cmt '`smk wn`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.31 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk wn > out.31
	${TR} assert true /usr/bin/sdiff "-sbB expected.31 out.31" || meld expected.31 out.31

	${TR} cmt 'Run:'
	${TR} cmt '`smk`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.32 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk > out.32
	${TR} assert true /usr/bin/sdiff "-sbB expected.32 out.32" || meld expected.32 out.32

	${TR} cmt 'Run:'
	${TR} cmt '`touch dir1/f2`'
	${TR} cmt '`smk wn`'
	${TR} cmt 'nothing expected as dir1 is not involved in a known command'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.32b >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	touch dir1/f2
	../../smk wn > out.32b
	${TR} assert true /usr/bin/sdiff "-sbB expected.32b out.32b" || meld expected.32b out.32b

	${TR} cmt 'Run:'
	${TR} cmt '`touch dir1/dir2/f3`'
	${TR} cmt '`smk wn`'
	${TR} cmt 'dir2 update should be reported, as dir2 is involved in a known command'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.32c >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	${TR} cmt 'but there is nothing to run'
	${TR} cmt 'Run:'
	${TR} cmt '`smk`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.32d >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk > out.32d
	${TR} assert true /usr/bin/sdiff "-sbB expected.32d out.32d" || meld expected.32d out.32d

	@ #-------------------------------------------------------------------------
	${TR} start 'accessing dir1 contents, read access'

	${TR} cmt '`let s now add a command reading dir1`'
	${TR} cmt 'Run:'
	${TR} cmt '`smk run ls -1 dir1`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.33 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk run ls -1 dir1 > out.33
	${TR} assert true /usr/bin/sdiff "-sbB expected.33 out.33" || meld expected.33 out.33

	${TR} cmt '`if nothing changes, nothing to run`'
	${TR} cmt 'Run:'
	${TR} cmt '`smk`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.34 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk > out.34
	${TR} assert true /usr/bin/sdiff "-sbB expected.34 out.34" || meld expected.34 out.34

	${TR} cmt '`but if we add a file in dir1, ls should re-run`'
	${TR} cmt 'Run:'
	${TR} cmt '`touch dir1/f4`'
	${TR} cmt '`smk -e`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.35 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	sleep 1.0
	touch dir1/f4
	../../smk -e | \
	  sed "s/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g" | \
      sed "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g" > out.35
	${TR} assert true /usr/bin/sdiff "-sbB expected.35 out.35" || meld expected.35 out.35

	${TR} end

rmdir1: 
	../../smk -q reset
	@ - rm -rf default.smk dir?
	../../smk -q run mkdir dir1
	../../smk -q run touch dir1/f1
	../../smk -q run mkdir dir1/dir2

	@ #-------------------------------------------------------------------------
	${TR} start 'removing dir1'
	${TR} cmt 'Run:'
	${TR} cmt '`rm -rf dir1`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.27 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	rm -rf dir1
	../../smk wn > out.27
	${TR} assert true /usr/bin/sdiff "-sbB expected.27 out.27" || meld expected.27 out.27

	${TR} cmt 'Run:'
	${TR} cmt '`smk -e`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt "Missing target, should not run without -mt"
	${TR} cmt '```' 
	cat expected.28 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -e > out.28
	${TR} assert true /usr/bin/sdiff "-sbB expected.28 out.28" || meld expected.28 out.28

	${TR} cmt 'Run:'
	${TR} cmt '`smk -e -mt`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt "should cause the command to rebuild missing targets"
	${TR} cmt '```' 
	cat expected.29 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -e -mt > out.29
	${TR} assert true /usr/bin/sdiff "-sbB expected.29 out.29" || meld expected.29 out.29

	${TR} end

clean:
	${TR} clean
	../../smk -q reset
	- ${RM} -rf dir? default.smk out.*
	