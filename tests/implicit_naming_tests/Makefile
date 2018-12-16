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

check: ${SMK} header implicit_name

header:
	@ #-------------------------------------------------------------------------
	${TR} clean

implicit_name:
	${TR} create "Implicit naming"

	@ #-------------------------------------------------------------------------
	${TR} start 'Implicit naming'
	${TR} cmt	
	${TR} cmt "Test that when there is only one run file in the directory, smk assume it without giving it on the command line."
	${TR} cmt 

	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -q reset`'
	${TR} cmt '`smk`'
	${TR} cmt 
	${TR} cmt 'Expected: help message as there is nothing in the directory'
	${TR} cmt 
	${TR} cmt '```' 						
	cat expected_implicit1.txt >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt
	 
	${SMK} -q reset
	test ! `${SMK} > out.implicit1.txt`
	${TR} assert true /usr/bin/sdiff "-sbB expected_implicit1.txt out.implicit1.txt" 

	${TR} cmt 'Run:'
	${TR} cmt '`smk -q ../hello.c/Makefile.2`'
	${TR} cmt '`touch ../hello.c/hello.c`'
	${TR} cmt '`smk`'
	${TR} cmt 
	${TR} cmt "Expected: smk re-run Makefile.2, as it is the only one in the dir"
	${TR} cmt '```' 
	cat expected_implicit2.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 

	${SMK} -q ../hello.c/Makefile.2 
	# Fixme: sleep needed because of close consecutive smk run that disrupt the algorithm
	sleep 1
	touch ../hello.c/hello.c
	${SMK} > out.implicit2.txt
	${TR} assert true /usr/bin/sdiff "-sbB expected_implicit2.txt out.implicit2.txt" 

	${TR} cmt 'Run:'
	${TR} cmt '`smk -q ../hello.c/Makefile.3`'
	${TR} cmt '`smk `'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt "   There is more than one possible run, smk display the list but don't do anything else "
	${TR} cmt '```' 
	cat expected_implicit3.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	${SMK} -q ../hello.c/Makefile.3 
	test ! `${SMK} > out.implicit3.txt`
	${TR} assert true /usr/bin/sdiff "-sbB expected_implicit3.txt out.implicit3.txt" 

	${TR} end

clean:
	${TR} clean
	${SMK} reset
	- ${RM} out.* 
