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

check: ../../smk header run conv new_file ogg-to-mp3 sub_dir smk_clean

header: clean
	@ #-------------------------------------------------------------------------
	${TR} create "Directory update tests"
	
run: header
	@ #-------------------------------------------------------------------------
	${TR} start 'start conversion'
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -q reset`'
	# converting ogg to mp3:
	${TR} cmt '`sox x.ogg x.mp3`'
	# setting Artist and Tittle tags:
	${TR} cmt '`id3v2 -a Luke -t Sentinelle x.mp3`'
	# renaming according to tags:
	${TR} cmt '`id3ren -template='%a - %s.mp3' x.mp3`'	
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.30 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -q reset
	../../smk run sox x.ogg x.mp3 								>  out.30
	#sleep 1.0
	../../smk run id3v2 -a Luke -t Sentinelle x.mp3 			>> out.30
	#sleep 1.0
	../../smk run id3ren -quiet -template='%a-%s.mp3' x.mp3 	>> out.30
	${TR} assert true /usr/bin/sdiff "-sbB expected.30 out.30" 

	${TR} cmt 'Run:'
	${TR} cmt '`smk st -l`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.31 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk st -l | \
	  sed "s/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g" | \
	  sed "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g" > out.31
	${TR} assert true /usr/bin/sdiff "-sbB expected.31 out.31" 

	${TR} cmt 'Run: (same with system files)'
	${TR} cmt '`smk st -l -sa`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.32 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk st -l -sa | \
	  sed "s/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g" | \
	  sed "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g" > out.32
	${TR} assert true /usr/bin/sdiff "-sbB expected.32 out.32" 

	${TR} end


conv: header 
	@ #-------------------------------------------------------------------------
	${TR} start 'start conversion'
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`rm default.smk`'
	${TR} cmt '`smk -q reset`'
	${TR} cmt "`smk run ./ogg-to-mp3.sh`"
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.1 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	rm default.smk
	../../smk -q reset
	../../smk run ./ogg-to-mp3.sh > out.1
	${TR} assert true /usr/bin/sdiff "-sbB expected.1 out.1" 

	${TR} cmt 'Run:'
	${TR} cmt '`smk st -l`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.2 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk st -l | \
	  sed "s/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g" | \
	  sed "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g" > out.2
	${TR} assert true /usr/bin/sdiff "-sbB expected.2 out.2" 

	${TR} end

new_file:
	@ #-------------------------------------------------------------------------
	${TR} start 'new ogg in dir'
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`cp x.ogg z.ogg`'
	${TR} cmt "`smk whatsnew`"
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.5 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	sleep 1.0 # immediate "touch" bug
	cp x.ogg z.ogg
	../../smk whatsnew > out.3
	${TR} assert true /usr/bin/sdiff "-sbB expected.3 out.3" 

	${TR} end

ogg-to-mp3:
	@ #-------------------------------------------------------------------------
	${TR} start 'ogg-to-mp3 is modified'
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt "`smk -q run ./ogg-to-mp3.sh`"
	${TR} cmt '`touch ogg-to-mp3`'
	${TR} cmt "`smk -e`"
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.10 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -q run ./ogg-to-mp3.sh
	touch ./ogg-to-mp3.sh
	../../smk -e | \
	  sed "s/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g" | \
	  sed "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g" > out.10
	${TR} assert true /usr/bin/sdiff "-sbB expected.10 out.10" 

	${TR} end

sub_dir:
	@ #-------------------------------------------------------------------------
	${TR} start 'adding a .ogg file in a subdir'
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt "`mkdir dir1`"
	${TR} cmt "`smk wn`"
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.20 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	- mkdir dir1
	../../smk wn > out.20
	${TR} assert true /usr/bin/sdiff "-sbB expected.20 out.20" 

	${TR} cmt 'Run:'
	${TR} cmt "`smk -e -n run ./ogg-to-mp3.sh`"
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.21 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	cp x.ogg dir1/t.ogg
	../../smk -e -n run ./ogg-to-mp3.sh > out.21
	${TR} assert true /usr/bin/sdiff "-sbB expected.21 out.21" 

	../../smk run > out.22
	${TR} assert true /usr/bin/sdiff "-sbB expected.22 out.22" 

	exit

	${TR} cmt 'Run:'
	${TR} cmt "`cp x.ogg dir1/t.ogg`"
	${TR} cmt "`smk wn`"
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.21 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	cp x.ogg dir1/t.ogg
	../../smk wn > out.21
	${TR} assert true /usr/bin/sdiff "-sbB expected.21 out.21" 

	${TR} cmt 'Run:'
	${TR} cmt "`smk -e`"
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.22 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk -e > out.10
	${TR} assert true /usr/bin/sdiff "-sbB expected.22 out.22" 

	${TR} end
	
smk_clean:
	@ #-------------------------------------------------------------------------
	${TR} start 'smk clean'
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt "`smk clean`"
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected.clean.1 >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	../../smk clean > out.clean.1
	${TR} assert true /usr/bin/sdiff "-sbB expected.clean.1 out.clean.1" 
	
	${TR} end

clean:
	${TR} clean
	../../smk reset
	- ${RM} -rf dir1 default.smk out.* *.mp3 z.ogg
	