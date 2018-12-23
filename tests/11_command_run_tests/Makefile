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

check: ../../smk header add run

header:
	@ #-------------------------------------------------------------------------
	${TR} clean
	${TR} create "Command Run features"

add:
	@ #-------------------------------------------------------------------------
	${TR} start 'Add and build'
	${TR} cmt	
	${TR} cmt 'Run:'
	${TR} cmt '`smk -q clean`'
	${TR} cmt '`smk -q reset`'
	${TR} cmt '`rm default.smk`'
	${TR} cmt '`smk add gcc -c ../hello.c/main.c`'
	${TR} cmt '`smk add gcc -c ../hello.c/hello.c`'
	${TR} cmt '`smk add gcc -o hello hello.o main.o`'
	${TR} cmt '`smk build`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected1.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -q clean
	../../smk -q reset
	- rm default.smk
	../../smk add gcc -c ../hello.c/main.c
	../../smk add gcc -c ../hello.c/hello.c
	../../smk add gcc -o hello hello.o main.o
	../../smk build > out.1.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected1.txt out.1.txt" 
	${TR} end

run:
	@ #-------------------------------------------------------------------------
	${TR} start 'Run'
	${TR} cmt	
	${TR} cmt 'Run:'
	${TR} cmt '`smk -q clean`'
	${TR} cmt '`smk -q reset`'
	${TR} cmt '`rm default.smk`'
	${TR} cmt '`smk run gcc -c ../hello.c/main.c`'
	${TR} cmt '`smk run gcc -c ../hello.c/hello.c`'
	${TR} cmt '`smk run gcc -o hello hello.o main.o`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected2.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -q clean
	../../smk -q reset
	- rm default.smk
	../../smk run gcc -c ../hello.c/main.c		>  out.2.txt
	../../smk run gcc -c ../hello.c/hello.c		>> out.2.txt
	../../smk run gcc -o hello hello.o main.o	>> out.2.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected2.txt out.2.txt" 
	${TR} end

clean:
	${TR} clean
	../../smk reset
	- ${RM} out.* *.o hello
