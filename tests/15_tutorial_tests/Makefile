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

check: ../../smk header quick_start 

header: clean
	@ #-------------------------------------------------------------------------
	${TR} create "Tutorial"
	${TR} cmt 'This test ensure that the current version'	
	${TR} cmt 'of smk behave as described in the tutorial'	

quick_start: header
	@ #-------------------------------------------------------------------------
	${TR} start 'start conversion'
	${TR} cmt 
	${TR} cmt 'This is the "Quick Start" part of the tutorial'
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '# converting ogg to mp3:'
	${TR} cmt '`sox x.ogg x.mp3`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.10a >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -e run sox x.ogg x.mp3 							>  out.10a
	${TR} assert true /usr/bin/sdiff "-sbB expected.10a out.10a" || meld expected.10a out.10a 

	${TR} cmt '# setting Artist and Tittle tags:'
	${TR} cmt '`id3v2 -a Luke -t Sentinelle x.mp3`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.10b >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 

	../../smk -e run id3v2 -a Luke -t Sentinelle x.mp3 			>> out.10b
	${TR} assert true /usr/bin/sdiff "-sbB expected.10b out.10b" || meld expected.10b out.10b 
	
	${TR} cmt '# renaming according to tags:'
	${TR} cmt '`id3ren -quiet -template='%a - %s.mp3' x.mp3`'	
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.10c >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -e run id3ren -quiet -template='%a-%s.mp3' x.mp3 	>> out.10c
	${TR} assert true /usr/bin/sdiff "-sbB expected.10c out.10c" || meld expected.10c out.10c 

	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start 'second run'
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk`'
	${TR} cmt 
	${TR} cmt 'Expected: nothing, situation is up to date'
	${TR} cmt '```' 
	cat expected.11 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -e > out.11
	${TR} assert true /usr/bin/sdiff "-sbB expected.11 out.11" || meld expected.11 out.11 

	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start 'smk do not rebuild if a target is missing!!!'
	${TR} cmt 'Run:'
	${TR} cmt '`rm Luke-Sentinelle.mp3`'
	${TR} cmt '`smk`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.12a >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	rm Luke-Sentinelle.mp3
	../../smk > out.12a
	${TR} assert true /usr/bin/sdiff "-sbB expected.12a out.12a" || meld expected.12a out.12a

	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start 'unless using the `-mt` / `--build-missing-target` option'
	${TR} cmt 'Run:'
	${TR} cmt '`smk -mt -e`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.12b >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	sleep 1.0
	../../smk -mt -e | \
	  sed "s/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g" | \
      sed "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g" > out.12b
	${TR} assert true /usr/bin/sdiff "-sbB expected.12b out.12b" || meld expected.12b out.12b

	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start 'touch x.ogg'
	${TR} cmt 'Run:'
	${TR} cmt '`touch x.ogg`'
	${TR} cmt '`rm Luke-Sentinelle.mp3`'
	${TR} cmt '`smk`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.13 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	sleep 1.0
	touch x.ogg
	rm Luke-Sentinelle.mp3
	../../smk -e | \
	  sed "s/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g" | \
      sed "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g" > out.13
	${TR} assert true /usr/bin/sdiff "-sbB expected.13 out.13" || meld expected.13 out.13

	@ #-------------------------------------------------------------------------
	${TR} start 'smk do rebuild if you give the target'
	${TR} cmt 'Run:'
	${TR} cmt '`smk lt -l > out.20`'
	${TR} cmt '`rm Luke-Sentinelle.mp3`'
	${TR} cmt '`smk Luke-Sentinelle.mp3`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.20a >> testrec.md
	cat expected.20c >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk lt -l	| \
	  sed "s/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g" | \
      sed "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g" >  out.20a
	rm Luke-Sentinelle.mp3
	../../smk Luke-Sentinelle.mp3	>> out.20c
	${TR} assert true /usr/bin/sdiff "-sbB expected.20a out.20a" || meld expected.20a out.20a
	${TR} assert true /usr/bin/sdiff "-sbB expected.20c out.20c" || meld expected.20c out.20c

	${TR} end

clean:
	${TR} clean
	../../smk reset
	- ${RM} -rf dir? default.smk out.* *.mp3 z.ogg
	