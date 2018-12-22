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

check: ../../smk header targets

header:
	@ #-------------------------------------------------------------------------
	${TR} clean
	${TR} create "Targets related functions"

targets:
	@ #-------------------------------------------------------------------------
	${TR} start 'dry-run clean'
	${TR} cmt	
	${TR} cmt "Test targets cleaning (dry run and real)"
	
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk reset`'
	${TR} cmt '`smk -q build ../hello.c/Makefile.2`'
	${TR} cmt '`smk clean --dry-run`'
	${TR} cmt '`smk --explain`  (to check that nothing was actually deleted)'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_targets1.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -q reset
	../../smk -q build ../hello.c/Makefile.2
	../../smk clean --dry-run > out.1.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_targets1.txt out.1.txt" 
	../../smk --explain       > out.2.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_targets2.txt out.2.txt" 

	@ #-------------------------------------------------------------------------
	${TR} start 'real clean'
	${TR} cmt 'Run:'
	${TR} cmt '`smk clean`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_targets3.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 

	../../smk clean > out.3.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_targets3.txt out.3.txt" 

	${TR} cmt 'Run:'
	${TR} cmt '`smk -e`  (to check effective cleaning)'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_targets4.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 

	../../smk -e      > out.4.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_targets4.txt out.4.txt" 

	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start 'Build selected target'
	${TR} cmt	
	${TR} cmt 'Run:'
	${TR} cmt '`smk -q reset`'
	${TR} cmt '`smk ../hello.c/Makefile.1`'
	${TR} cmt '`smk build main.o`'
	${TR} cmt 'Note that to avoid any confusion, this smkfile do not contain any target named main.o' 
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected5.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -q reset
	../../smk -q ../hello.c/Makefile.1
	../../smk build main.o	> out.5.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected5.txt out.5.txt" 

	${TR} cmt 'Run:'
	${TR} cmt '`touch ../hello.c/main.c`'
	${TR} cmt '`smk build main.o`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected6.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	sleep 1 # Fixme:
	touch ../hello.c/main.c
	../../smk build main.o	> out.6.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected6.txt out.6.txt" 

	${TR} end


clean:
	${TR} clean
	../../smk reset
	- ${RM} out.*
