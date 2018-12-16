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
SMK=../../smk

.PHONY : clean

check: ${SMK} header targets

header:
	@ #-------------------------------------------------------------------------
	${TR} clean
	${TR} create "Targets related functions"

targets:
	@ #-------------------------------------------------------------------------
	${TR} start 'clean'
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
	
	${SMK} -q reset
	${SMK} -q build ../hello.c/Makefile.2
	${SMK} clean --dry-run > out.targets1.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_targets1.txt out.targets1.txt" 
	${SMK} --explain       > out.targets2.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_targets2.txt out.targets2.txt" 

	${TR} cmt 'Run:'
	${TR} cmt '`smk clean`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_targets3.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 

	${SMK} clean > out.targets3.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_targets3.txt out.targets3.txt" 

	${TR} cmt 'Run:'
	${TR} cmt '`smk -e`  (to check effective cleaning)'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_targets4.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 

	${SMK} -e      > out.targets4.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_targets4.txt out.targets4.txt" 

	${TR} end

clean:
	${TR} clean
	${SMK} reset
	- ${RM} out.*
