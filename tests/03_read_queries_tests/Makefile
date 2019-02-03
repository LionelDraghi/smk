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

check: ../../smk header read_queries 

header:
	@ #-------------------------------------------------------------------------
	${TR} clean
	${TR} create "Read queries"

read_queries:

	@ #-------------------------------------------------------------------------
	${TR} start 'read-smkfile'
	${TR} cmt  								
	${TR} cmt "Read a smkfile and shows what is understud by smk"
	
	${TR} cmt 
	${TR} cmt "Makefile:"
	${TR} cmt '```' 						
	cat ../hello.c/Makefile.3 >> testrec.md
	${TR} cmt 
	${TR} cmt '```' 						
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk read-smkfile ../hello.c/Makefile.3`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 						
	cat expected_read_smkfile.txt >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt 

	../../smk read-smkfile ../hello.c/Makefile.3 | sed "s/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g" | \
		         sed "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g" > out.read_smkfile.txt 
	# to get the comparison working, numbers in date are removed
	${TR} assert true /usr/bin/sdiff "-sbB expected_read_smkfile.txt out.read_smkfile.txt" || meld expected_read_smkfile.txt out.read_smkfile.txt 

	${TR} end


	@ #-------------------------------------------------------------------------
	${TR} start 'status'
	${TR} cmt 
	${TR} cmt "Read the previous run dump and shows sources and targets"

	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -q reset`'
	${TR} cmt '`smk -q build ../hello.c/Makefile.2`'
	${TR} cmt '`smk status ../hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt "(note that to ease comparison, dates are removed)"
	${TR} cmt '```' 
	cat expected_lpr.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -q reset
	../../smk -q build ../hello.c/Makefile.2 
	../../smk status | sed "s/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g" | \
		         sed "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g" > out.lpr.txt 
	# to get the comparison working, numbers in date are removed
	${TR} assert true /usr/bin/sdiff "-sbB expected_lpr.txt out.lpr.txt" || meld expected_lpr.txt out.lpr.txt 

	${TR} cmt 'Run: (same with system files not ignored and long form) '
	${TR} cmt '`smk -q reset`'
	${TR} cmt '`smk -q build ../hello.c/Makefile.2`'
	${TR} cmt '`smk st -l -sa ../hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt "(note that to ease comparison, dates are removed)"
	${TR} cmt '```' 
	cat expected_lpr1.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -q reset
	../../smk -q build ../hello.c/Makefile.2 
	../../smk st -l -sa | \
	  sed "s/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g" | \
	  sed "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g" > out.lpr1.txt 
	# to get the comparison working, numbers in date are removed
	${TR} assert true /usr/bin/sdiff "-sbB expected_lpr1.txt out.lpr1.txt" || meld expected_lpr1.txt out.lpr1.txt 

	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -q reset`'
	${TR} cmt '`smk status ../hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_lpr2.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt

	../../smk -q reset
	test ! `../../smk status ../hello.c/Makefile.2 > out.lpr2.txt`
	${TR} assert true /usr/bin/sdiff "-sbB expected_lpr2.txt out.lpr2.txt" || meld expected_lpr2.txt out.lpr2.txt 

	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk status`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_lpr3.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -q reset
	test ! `../../smk status > out.lpr3.txt`
	${TR} assert true /usr/bin/sdiff "-sbB expected_lpr3.txt out.lpr3.txt" || meld expected_lpr3.txt out.lpr3.txt 

	${TR} end

clean:
	${TR} clean
	../../smk -q clean ../hello.c/Makefile.2
	../../smk -q clean ../hello.c/Makefile.3
	../../smk -q reset
	- ${RM} out.*
