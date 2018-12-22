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

check: ../../smk header run_error 

header:
	@ #-------------------------------------------------------------------------
	${TR} clean
	${TR} create 'Run errors'
	${TR} cmt 'test -k and -i behavior'

run_error:
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
	${TR} cmt 'Note that the two command that fail are rerun'
	${TR} cmt 'during a second loop in case the command' 
	${TR} cmt 'that run successfully during the first loop' 
	${TR} cmt 'changed the the situation.'
	${TR} cmt 'And that is not the case here, nothing is run with'
	${TR} cmt 'success during the second loop, so full stop.' 
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

clean:
	${TR} clean
	../../smk reset
	- ${RM} out.*  
