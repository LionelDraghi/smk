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

check: ../../smk header sections listing build rebuild cleaning rebuild2

header:
	@ #-------------------------------------------------------------------------
	${TR} clean
	${TR} create "Website building sanity tests"

sections:
	@ #-------------------------------------------------------------------------
	${TR} start 'cleaning and building using clean and doc sections'
	${TR} cmt	

	${TR} cmt 'Run:'
	${TR} cmt '`smk -q reset`'
	${TR} cmt '`smk -q ../mysite/Makefile:clean`'
	${TR} cmt '`smk ../mysite/Makefile:doc`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected1.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -q reset
	../../smk -q ../mysite/Makefile:clean
	../../smk ../mysite/Makefile:doc > out.1.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected1.txt out.1.txt" 

	${TR} end

listing:
	@ #-------------------------------------------------------------------------
	${TR} start 'listing'
	${TR} cmt	

	${TR} cmt 'Run:'
	${TR} cmt '`smk ls -l`   (long listing format)'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected2.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk ls -l > out.2.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected2.txt out.2.txt" 

	${TR} cmt 'Run:'
	${TR} cmt '`smk ls`   (default short listing format)'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected3.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk ls > out.3.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected3.txt out.3.txt" 

	${TR} cmt 'Run:'
	${TR} cmt '`smk list-targets --long-listing`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected4.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk list-targets --long-listing > out.4.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected4.txt out.4.txt" 

	${TR} cmt 'Run:'
	${TR} cmt '`smk rs`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected5.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk rs > out.5.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected5.txt out.5.txt" 

	${TR} end

build:
	@ #-------------------------------------------------------------------------
	${TR} start 'building without section'
	${TR} cmt	

	${TR} cmt 'Run:'
	${TR} cmt '`smk -q reset`'
	${TR} cmt '   First run after reset : all command should be executed, including `clean`'
	${TR} cmt '`smk ../mysite/Makefile`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected6.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -q reset
	../../smk ../mysite/Makefile > out.6.txt
	${TR} assert true /usr/bin/sdiff "-sbB expected6.txt out.6.txt" 

	${TR} cmt 'Run:'
	${TR} cmt '   Second run : all command should be executed except the "no target" command `clean`'
	${TR} cmt '`smk --explain`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected7.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk --explain > out.7.txt
	${TR} assert true /usr/bin/sdiff "-sbB expected7.txt out.7.txt" 

	${TR} cmt 'Run:'
	${TR} cmt '   Third run : all targets are up-to-date'
	${TR} cmt '`smk --explain --verbose`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected8.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk --explain --verbose > out.8.txt
	${TR} assert true /usr/bin/sdiff "-sbB expected8.txt out.8.txt" 

	${TR} end

rebuild:
	@ #-------------------------------------------------------------------------
	${TR} start 're-building'
	${TR} cmt	

	${TR} cmt 'Run:'
	${TR} cmt '`touch ../mysite/docs/about.md`'
	${TR} cmt '`smk whatsnew`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected9a.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	sleep 1.0 # Fixme: if "touched" just after being build, the change is not detected
	touch ../mysite/docs/about.md
	../../smk whatsnew | sed "s/[0-9]//g" > out.9a.txt 	# Time tag neutralized
	${TR} assert true /usr/bin/sdiff "-sbB expected9a.txt out.9a.txt" 

	${TR} cmt 'Run:'
	${TR} cmt '`rm ../mysite/site/tutorial/index.html`'
	${TR} cmt '`smk wn`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected9b.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	rm ../mysite/site/tutorial/index.html
	../../smk wn | sed "s/[0-9]//g" > out.9b.txt 	# Time tag neutralized
	${TR} assert true /usr/bin/sdiff "-sbB expected9b.txt out.9b.txt" 

	${TR} cmt 'Run:'
	${TR} cmt '`smk -e -v`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected9c.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -e -v | sed "s/[0-9]//g" > out.9c.txt 	# Time tag neutralized
	${TR} assert true /usr/bin/sdiff "-sbB expected9c.txt out.9c.txt" 

	${TR} end

cleaning:
	@ #-------------------------------------------------------------------------
	${TR} start 'cleaning'
	${TR} cmt	

	${TR} cmt 'Run:'
	${TR} cmt '`smk clean`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected10.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	touch ../mysite/docs/about.md
	../../smk clean > out.10.txt
	${TR} assert true /usr/bin/sdiff "-sbB expected10.txt out.10.txt" 

	${TR} end

rebuild2:
	@ #-------------------------------------------------------------------------
	${TR} start 're-building after a clean'
	${TR} cmt	

	${TR} cmt 'Run:'
	${TR} cmt '`smk --explain`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected11.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk --explain | sed "s/[0-9]//g" > out.11.txt 	# Time tag neutralized
	${TR} assert true /usr/bin/sdiff "-sbB expected11.txt out.11.txt" 

	${TR} end

clean:
	${TR} clean
	../../smk reset
	- ${RM} out.*
