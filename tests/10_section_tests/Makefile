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

check: ../../smk header sections

header:
	@ #-------------------------------------------------------------------------
	${TR} clean
	${TR} create "Sections related functions"

sections:
	@ #-------------------------------------------------------------------------
	${TR} start 'specific section building'
	${TR} cmt 'Run:'
	${TR} cmt '`smk :main.o` with main.o up to date`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_targets5.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	../../smk -q reset
	../../smk build -q ../hello.c/Makefile.2
	../../smk :main.o > out.5.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_targets5.txt out.5.txt" 

	@ #-------------------------------------------------------------------------
	${TR} cmt 'Run:'
	${TR} cmt '`touch main.c`'
	${TR} cmt '`smk :main.o`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_targets6.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	sleep 1.0 # Fixme: touch to close to rebuild, not detected othewise
	touch ../hello.c/main.c
	../../smk :main.o      > out.6.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_targets6.txt out.6.txt" 

	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start 'unknow section'
	${TR} cmt 'Run:'
	${TR} cmt '`smk :qzdsqdq.o`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_targets7.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	../../smk :qzdsqdq.o      > out.7.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_targets7.txt out.7.txt" 

	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start 'smkmfile:section notation'
	${TR} cmt 'Run:'
	${TR} cmt '`smk build -q ../hello.c/Makefile.4`'
	${TR} cmt '`touch ../hello.c/hello.c`'
	${TR} cmt '`smk ../hello.c/Makefile.4:hello.o`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_targets8.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	../../smk build -q ../hello.c/Makefile.4
	touch ../hello.c/hello.c
	../../smk ../hello.c/Makefile.4:hello.o      > out.8.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_targets8.txt out.8.txt" 

	${TR} cmt 'Run:'
	${TR} cmt '`smk -a ../hello.c/Makefile.4:mrproper`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_targets9.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	../../smk -a ../hello.c/Makefile.4:mrproper      > out.9.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_targets9.txt out.9.txt" 

	${TR} end

clean:
	${TR} clean
	../../smk reset
	- ${RM} out.*
