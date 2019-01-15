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

check: ../../smk header sanity

header:
	@ #-------------------------------------------------------------------------
	${TR} clean

sanity:
	${TR} create "Sanity"

	${TR} cmt  								
	${TR} cmt "Makefile:"
	${TR} cmt '```' 						
	cat ../hello.c/Makefile.2 >> testrec.md
	${TR} cmt 
	${TR} cmt '```' 						

	@ #-------------------------------------------------------------------------
	${TR} start 'First `smk`, after `make`, should run no command'
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -q ../hello.c/Makefile.2`'
	${TR} cmt '`smk -e ../hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 						
	cat expected_sanity.1 >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt 
	# $(MAKE) --directory=../hello.c -f Makefile.2 
	# let's do it with smk:
	../../smk -q ../hello.c/Makefile.2
	../../smk -e ../hello.c/Makefile.2 > out.1
	${TR} assert true /usr/bin/sdiff "-sbB expected_sanity.1 out.1" || meld expected_sanity.1 out.1 
	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start 'Second `smk`, should not run any command'
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -e ../hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 						
	cat expected_sanity.2 >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt 
	../../smk -e ../hello.c/Makefile.2 > out.2
	${TR} assert true /usr/bin/sdiff "-sbB expected_sanity.2 out.2" || meld expected_sanity.2 out.2 
	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start '`smk reset`, no more history, should run all commands'
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk reset --quiet`'
	${TR} cmt '`smk -e ../hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 						
	cat expected_sanity.3 >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt 
	../../smk reset --quiet
	../../smk -e ../hello.c/Makefile.2 > out.3 
	${TR} assert true /usr/bin/sdiff "-sbB expected_sanity.3 out.3" || meld expected_sanity.3 out.3 
	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start '`smk -a`, should run all commands even if not needed'
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -e -a ../hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 						
	cat expected_sanity.4 >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt 
	../../smk -e -a ../hello.c/Makefile.2 > out.4 
	${TR} assert true /usr/bin/sdiff "-sbB expected_sanity.4 out.4" || meld expected_sanity.4 out.4 
	${TR} end
	
	# Fixme: sleep needed because of close consecutive smk run that disrupt the algorithm
	sleep 1

	@ #-------------------------------------------------------------------------
	${TR} start '`rm main.o` (missing file)'
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`rm ../hello.c/main.o`'
	${TR} cmt '`smk -e --missing-targets ../hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 						
	cat expected_sanity.5 >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt 
	rm ../hello.c/main.o
	../../smk -e --missing-targets ../hello.c/Makefile.2 | sed "s/[0-9]//g" > out.5
	# Time tag neutralized
	${TR} assert true /usr/bin/sdiff "-sbB expected_sanity.5 out.5" || meld expected_sanity.5 out.5 
	${TR} end

	# Fixme: sleep needed because of close consecutive smk run that disrupt the algorithm
	sleep 1

	@ #-------------------------------------------------------------------------
	${TR} start '`touch hello.c` (updated file)'
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`rm ../hello.c/main.o`'
	${TR} cmt '`smk -e ../hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 						
	cat expected_sanity.6 >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt 
	touch ../hello.c/hello.c
	../../smk -e ../hello.c/Makefile.2 | sed "s/[0-9]//g" > out.6
	# Time tag neutralized
	${TR} assert true /usr/bin/sdiff "-sbB expected_sanity.6 out.6" || meld expected_sanity.6 out.6 
	${TR} end

clean:
	${TR} clean
	../../smk -q clean
	../../smk -q reset
	- ${RM} out.*
