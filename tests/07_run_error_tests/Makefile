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

check: ../../smk header run_errors 

header:
	@ #-------------------------------------------------------------------------
	${TR} clean
	${TR} create 'Run errors'
	${TR} cmt 'test -k and -i behavior'

run_errors:
	@ #-------------------------------------------------------------------------
	${TR} start 'no option' 
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -q reset`'
	${TR} cmt '`smk ../hello.c/Wrong_Makefile`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_run_error1.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -q reset
	test ! `../../smk ../hello.c/Wrong_Makefile > out.run_error1.txt  2>&1`
	${TR} assert true /usr/bin/sdiff "-sbB expected_run_error1.txt out.run_error1.txt" 

	@ #-------------------------------------------------------------------------
	${TR} start '-k' 
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -q reset`'
	${TR} cmt '`smk -k ../hello.c/Wrong_Makefile`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_run_error2.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -q reset
	test ! `../../smk -k ../hello.c/Wrong_Makefile > out.run_error2.txt 2>&1`
	${TR} assert true /usr/bin/sdiff "-sbB expected_run_error2.txt out.run_error2.txt" 
	
	@ #-------------------------------------------------------------------------
	${TR} start '-i' 
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -q reset`'
	${TR} cmt '`smk -i ../hello.c/Wrong_Makefile`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '   Same as with -k, but without returning an error code'
	${TR} cmt '```' 
	cat expected_run_error3.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -q reset
	../../smk -i ../hello.c/Wrong_Makefile > out.run_error3.txt 2>&1
	${TR} assert true /usr/bin/sdiff "-sbB expected_run_error3.txt out.run_error3.txt" 

	@ #-------------------------------------------------------------------------
	${TR} start '-k -i' 
	${TR} cmt 
	${TR} cmt 'Run: with both!'
	${TR} cmt '`smk -q reset`'
	${TR} cmt '`smk --keep-going --ignore-errors ../hello.c/Wrong_Makefile`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '   Same as with -k, but without returning an error code'
	${TR} cmt '```' 
	cat expected_run_error4.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -q reset
	../../smk --keep-going --ignore-errors ../hello.c/Wrong_Makefile > out.run_error4.txt 2>&1
	${TR} assert true /usr/bin/sdiff "-sbB expected_run_error4.txt out.run_error4.txt" 

	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start '`run` command fails' 
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -q reset`'
	${TR} cmt '`rm *.smk`'
	${TR} cmt '`smk run non_existing_command`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt 
	${TR} cmt '```' 
	cat expected.5 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -q reset
	@ - rm *.smk
	test ! `../../smk run non_existing_command > out.5 2>&1`
	${TR} assert true /usr/bin/sdiff "-sbB expected.5 out.5" 

	${TR} cmt 
	${TR} cmt 'default.smk should nevertheless contains the failed command'
	${TR} cmt 
	${TR} cmt '```' 
	cat expected.6 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 

	${TR} assert true /usr/bin/sdiff "-sbB expected.6 default.smk" 

	${TR} cmt 'Run:'
	${TR} cmt '`smk read-smkfile`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt 
	${TR} cmt '```' 
	cat expected.7 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk read-smkfile | \
	  sed "s/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g" | \
      sed "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g" > out.7
	${TR} assert true /usr/bin/sdiff "-sbB expected.7 out.7" 

	${TR} cmt 'Run:'
	${TR} cmt '`smk status`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt 
	${TR} cmt '```' 
	cat expected.8 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk status > out.8
	${TR} assert true /usr/bin/sdiff "-sbB expected.8 out.8" 

	${TR} cmt 'Other commands should return nothing:'
	${TR} cmt '`smk whatsnew`'
	${TR} cmt '`smk list-sources`'
	${TR} cmt '`smk list-targets`'
	${TR} cmt 
	
	> out.null
	../../smk whatsnew > out.9
	${TR} assert true /usr/bin/sdiff "-sbB out.null out.9" 
	../../smk list-sources > out.10
	${TR} assert true /usr/bin/sdiff "-sbB out.null out.10" 
	../../smk list-targets > out.11
	${TR} assert true /usr/bin/sdiff "-sbB out.null out.11" 

	${TR} end

clean:
	${TR} clean
	../../smk reset
	- ${RM} out.* *.smk 
