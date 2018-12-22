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

check: ../../smk header multiline

header:
	@ #-------------------------------------------------------------------------
	${TR} clean
	${TR} create "Multiline Commands"

out.sloccount:
	sloccount ../hello.c/* | grep "ansic=" > out.sloccount

multiline: header out.sloccount
	@ #-------------------------------------------------------------------------
	${TR} start 'multiline single command'
	${TR} cmt 
	${TR} cmt 'cat `multiline_smkfile1.txt`:'
	${TR} cmt '```' 
	cat multiline_smkfile1.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -q reset`'
	${TR} cmt '`smk multiline_smkfile1.txt`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_multiline1.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -q reset
	../../smk multiline_smkfile1.txt > out.multiline1.txt
	${TR} assert true /usr/bin/sdiff "-sbB expected_multiline1.txt out.multiline1.txt" 
	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start 'multiline with more commands and pipes'
	${TR} cmt 
	${TR} cmt 'cat `multiline_smkfile2.txt`:'
	${TR} cmt '```' 
	cat multiline_smkfile2.txt >> testrec.md
	${TR} cmt 
	${TR} cmt '```' 
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -q reset`'
	${TR} cmt '`smk multiline_smkfile2.txt`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_multiline2.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -q reset
	../../smk multiline_smkfile2.txt > out.multiline2.txt
	${TR} assert true /usr/bin/sdiff "-sbB expected_multiline2.txt out.multiline2.txt" 
	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start 'Hill formatted multiline'
	${TR} cmt 
	${TR} cmt 'cat `hill_multiline_smkfile.txt`:'
	${TR} cmt '```' 
	cat hill_multiline_smkfile.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -q reset`'
	${TR} cmt '`smk hill_multiline_smkfile.txt`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_multiline3.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -q reset
	test ! `../../smk hill_multiline_smkfile.txt > out.multiline3.txt`
	${TR} assert true /usr/bin/sdiff "-sbB expected_multiline3.txt out.multiline3.txt" 
	${TR} end

clean:
	${TR} clean
	../../smk reset
	- ${RM} out.*
