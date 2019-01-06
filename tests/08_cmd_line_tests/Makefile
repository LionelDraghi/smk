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

check: ../../smk header cmd_line

header:
	@ #-------------------------------------------------------------------------
	${TR} clean
	${TR} create "Command line"

cmd_line:
	@ #-------------------------------------------------------------------------
	${TR} start "Help options" 
	${TR} cmt  								
	${TR} cmt "Test the -h and help output :" 	
	../../smk -h > out.expected_help.txt

	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk `'
	${TR} cmt '`smk help`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt 
	${TR} cmt '```' 						
	cat out.expected_help.txt >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt 

	../../smk help	> out.help1.txt
	${TR} assert true /usr/bin/sdiff "-sbB out.expected_help.txt out.help1.txt" || meld out.expected_help.txt out.help1.txt 
	
	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start "Version option" 					
	${TR} cmt  								
	${TR} cmt "Test that the version command will put :" 			
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk version`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt 
	${TR} cmt '```' 						
	echo "0.3.0"		> out.expected_version.txt
	cat out.expected_version.txt >> testrec.md					
	${TR} cmt '```' 						
	${TR} cmt 

	../../smk version	> out.version1.txt
	${TR} assert true /usr/bin/sdiff "-sbB out.expected_version.txt out.version1.txt" || meld out.expected_version.txt out.version1.txt

	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start "Illegal cmd lines" 
	${TR} cmt  								
	${TR} cmt 'Run:'
	${TR} cmt '`smk read-smkfile status`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt 
	${TR} cmt '```' 						
	cat expected_wrong_cmd_line1.txt >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt 

	test ! `../../smk read-smkfile status > out.wrong_cmd_line1.txt`
	${TR} assert true /usr/bin/sdiff "-sbB expected_wrong_cmd_line1.txt out.wrong_cmd_line1.txt" || meld expected_wrong_cmd_line1.txt out.wrong_cmd_line1.txt 
	
	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start "Unknow Makefile" 
	${TR} cmt  								
	${TR} cmt "Test the error message if an unknow MakeFile is given" 	
	
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk My_Makefile`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt 
	${TR} cmt '```' 						
	cat expected_unknow_makefile.txt >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt 

	test ! `../../smk My_Makefile > out.err_unknow_makefile.txt`
	${TR} assert true /usr/bin/sdiff "-sbB expected_unknow_makefile.txt out.err_unknow_makefile.txt" || meld expected_unknow_makefile.txt out.err_unknow_makefile.txt 

	${TR} end
	
clean:
	${TR} clean
	../../smk reset
	- ${RM} out.*
