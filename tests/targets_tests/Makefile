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
	
	${SMK} -q reset
	${SMK} -q build ../hello.c/Makefile.2
	${SMK} clean --dry-run > out.1.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_targets1.txt out.1.txt" 
	${SMK} --explain       > out.2.txt 
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

	${SMK} clean > out.3.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_targets3.txt out.3.txt" 

	${TR} cmt 'Run:'
	${TR} cmt '`smk -e`  (to check effective cleaning)'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_targets4.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 

	${SMK} -e      > out.4.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_targets4.txt out.4.txt" 

	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start 'specific section building'
	${TR} cmt 'Run:'
	${TR} cmt '`smk :main.o` with main.o up to date'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_targets5.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	${SMK} build -q
	${SMK} :main.o > out.5.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_targets5.txt out.5.txt" 

	@ #-------------------------------------------------------------------------
	${TR} cmt 'Run:'
	${TR} cmt '`touch main.c`'
	${TR} cmt '`smk :main.o'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_targets6.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	touch ../hello.c/main.c
	${SMK} :main.o      > out.6.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_targets6.txt out.6.txt" 

	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start 'unknow section'
	${TR} cmt 'Run:'
	${TR} cmt '`smk :qzdsqdq.o'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_targets7.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	${SMK} :qzdsqdq.o      > out.7.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_targets7.txt out.7.txt" 

	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start 'smkmfile:section notation'
	${TR} cmt 'Run:'
	${TR} cmt '`smk build ../hello.c/Makefile.4'
	${TR} cmt '`touch ../hello.c/hello.c'
	${TR} cmt '`smk ../hello.c/Makefile.4:hello.o'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_targets8.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	${SMK} build -q ../hello.c/Makefile.4
	touch ../hello.c/hello.c
	${SMK} ../hello.c/Makefile.4:hello.o      > out.8.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_targets8.txt out.8.txt" 

	${TR} cmt 'Run:'
	${TR} cmt '`smk ../hello.c/Makefile.4:mrproper'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_targets9.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	${SMK} ../hello.c/Makefile.4:mrproper      > out.9.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_targets9.txt out.9.txt" 

	${TR} end

clean:
	${TR} clean
	${SMK} reset
	- ${RM} out.*
