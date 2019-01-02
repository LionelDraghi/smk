# ------------------------------------------------------------------------------
# smk, the smart make (http://lionel.draghi.free.fr/smk/)
#  © 2018 Lionel Draghi <lionel.draghi@free.fr>
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

check: ../../smk header list_queries

header:
	@ #-------------------------------------------------------------------------
	${TR} clean

list_queries:
	${TR} create "List queries"

	@ #-------------------------------------------------------------------------
	${TR} start 'lr | list-runs'
	${TR} cmt	
	${TR} cmt "Test available previous runs"
	
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -q reset`'
	${TR} cmt '`smk lr`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_lr1.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -q build ../hello.c/Makefile.2`'
	${TR} cmt '`smk -q ../hello.c/Makefile.3`'
	${TR} cmt '`smk list-runs`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_lr2.txt >> testrec.md
	echo ""              >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -q reset
	test ! `../../smk lr > out.lr1.txt`
	${TR} assert true /usr/bin/sdiff "-sbB expected_lr1.txt out.lr1.txt" 

	../../smk -q build ../hello.c/Makefile.2 
	../../smk -q ../hello.c/Makefile.3 
	../../smk list-runs > out.lr2.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_lr2.txt out.lr2.txt" 

	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start 'lt | list-targets'
	
	${TR} cmt 'Run:'
	${TR} cmt '`smk list-targets --long-listing ../hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected: (long form)'
	${TR} cmt '```' 
	cat expected_lt1.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk list-targets --long-listing ../hello.c/Makefile.2 | \
	  sed "s/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g" | \
	  sed "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g" > out.lt1.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_lt1.txt out.lt1.txt" 

	${TR} cmt 'Run:'
	${TR} cmt '`smk lt ../hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected: (short form)'
	${TR} cmt '```' 
	cat expected_lt2.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk lt ../hello.c/Makefile.2 > out.lt2.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_lt2.txt out.lt2.txt" 

	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start 'ls | list-sources'
	
	${TR} cmt 
	${TR} cmt 'Run: (long form)'
	${TR} cmt '`smk ls -l ../hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_ls1.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk ls ../hello.c/Makefile.2 > out.ls1.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_ls1.txt out.ls1.txt" 

	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk list-sources ../hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_ls2.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk list-sources ../hello.c/Makefile.2 > out.ls2.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_ls2.txt out.ls2.txt" 

	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start 'ls | list-sources --show-all-files'
	
	${TR} cmt 
	${TR} cmt 'Run: (short form)'
	${TR} cmt '`smk list-sources --shows-system-files ../hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_las1.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk list-sources --show-all-files ../hello.c/Makefile.2 > out.las1.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_las1.txt out.las1.txt" 

	${TR} cmt 
	${TR} cmt 'Run: (long form)'
	${TR} cmt '`smk -l ls -sa ../hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_las2.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -l ls -sa ../hello.c/Makefile.2 | \
	  sed "s/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g" | \
	  sed "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g" > out.las2.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_las2.txt out.las2.txt" 

	${TR} end

clean:
	${TR} clean
	../../smk reset
	- ${RM} out.*
