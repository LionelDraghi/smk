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

.SILENT:
all: build check doc

build:
	echo
	echo --- build:
	@ - mkdir -p obj
	echo
	gprbuild -P smk.gpr
	echo

check: smk
	echo --- tests:
	$(MAKE) --directory=tests
	echo

	echo --- tests summary:
	echo
	cat tests/tests_count.txt

	# --------------------------------------------------------------------
	echo
	echo Coverage report: 
	lcov --quiet --capture --directory obj -o obj/coverage.info
	lcov --quiet --remove obj/coverage.info -o obj/coverage.info \
		"*/adainclude/*" "*.ads" "obj/b__smk-main.adb"
	# Ignoring :
	# - spec (results are not consistent with current gcc version) 
	# - the false main
	# - libs (Standard)

	genhtml obj/coverage.info -o docs/lcov --title "smk tests coverage" \
		--prefix "/home/lionel/Proj/smk" --frames | tail -n 2 > cov_sum.txt
	# --title  : Display TITLE in header of all pages
	# --prefix : Remove PREFIX from all directory names
	# --frame  : Use HTML frames for source code view
	cat cov_sum.txt
	echo

.PHONY : dashboard
dashboard: obj/coverage.info tests/tests_count.txt
	
	@ # Language pie
	@ # --------------------------------------------------------------------
	sloccount src | grep "ada=" | ploticus  -prefab pie 	\
		data=stdin labels=2 colors="blue red green orange"	\
		explode=0.1 values=1 title="Ada sloc `date +%x`"	\
		-png -o docs/img/sloc.png

	@ # Code coverage Pie
	
	@ # Test pie	
	@ # --------------------------------------------------------------------
	ploticus -prefab pie legend=yes							\
		data=tests/tests_count.txt labels=1 colors="green red orange"	\
		explode=0.1 values=2 title="tests results `date +%x`"			\
		-png -o docs/img/tests.png

	>  docs/dashboard.md
	echo "Dashboard"				>> docs/dashboard.md
	echo "========="				>> docs/dashboard.md
	echo 							>> docs/dashboard.md
	echo "Version"					>> docs/dashboard.md
	echo "-------"					>> docs/dashboard.md
	echo "> smk version"			>> docs/dashboard.md
	echo 	 						>> docs/dashboard.md
	echo '```' 						>> docs/dashboard.md
	./smk version					>> docs/dashboard.md
	echo '```' 						>> docs/dashboard.md
	echo 	 						>> docs/dashboard.md
	echo "> date -r ./smk --iso-8601=seconds" 	>> docs/dashboard.md
	echo 	 						>> docs/dashboard.md
	echo '```' 						>> docs/dashboard.md
	date -r ./smk --iso-8601=seconds 			>> docs/dashboard.md
	echo '```' 						>> docs/dashboard.md
	echo 	 						>> docs/dashboard.md
	echo "Test results"				>> docs/dashboard.md
	echo "------------"				>> docs/dashboard.md
	echo '```'			 			>> docs/dashboard.md
	cat tests/tests_count.txt		>> docs/dashboard.md
	echo '```'			 			>> docs/dashboard.md
	echo "![](img/tests.png)"		>> docs/dashboard.md
	echo 							>> docs/dashboard.md
	echo "Coverage"					>> docs/dashboard.md
	echo "--------"					>> docs/dashboard.md
	echo 							>> docs/dashboard.md
	echo '```'			 			>> docs/dashboard.md
	cat cov_sum.txt					>> docs/dashboard.md
	echo '```'			 			>> docs/dashboard.md
	echo 							>> docs/dashboard.md
	echo '[**Coverage details in the sources**](http://lionel.draghi.free.fr/smk/lcov/home/lionel/Proj/smk/src/index-sort-f.html)'	>> docs/dashboard.md
	echo 							>> docs/dashboard.md

	# badge making:
	wget -q "https://img.shields.io/badge/Version-`./smk version`-blue.svg" -O docs/img/version.svg
	wget -q "https://img.shields.io/badge/tests_OK-`cat tests/tests_count.txt | sed -n "s/Successful  //p"`-green.svg" -O docs/img/tests_ok.svg
	wget -q "https://img.shields.io/badge/tests_KO-`cat tests/tests_count.txt | sed -n "s/Failed      //p"`-green.svg" -O docs/img/tests_ko.svg

.PHONY : cmd_line.md
cmd_line.md:
	> docs/cmd_line.md
	echo "smk command line"		>> docs/cmd_line.md
	echo "----------------"		>> docs/cmd_line.md
	echo ""						>> docs/cmd_line.md
	echo '```'					>> docs/cmd_line.md
	echo "$ smk -h" 			>> docs/cmd_line.md
	echo '```'					>> docs/cmd_line.md
	echo ""						>> docs/cmd_line.md
	echo '```'					>> docs/cmd_line.md
	./smk -h		 			>> docs/cmd_line.md
	echo '```'					>> docs/cmd_line.md
	echo ""						>> docs/cmd_line.md
	echo "smk current version"	>> docs/cmd_line.md
	echo "-------------------"	>> docs/cmd_line.md
	echo ""						>> docs/cmd_line.md
	echo '```'					>> docs/cmd_line.md
	echo "$ smk version"		>> docs/cmd_line.md
	echo '```'					>> docs/cmd_line.md
	echo ""						>> docs/cmd_line.md
	echo '```'					>> docs/cmd_line.md
	./smk version				>> docs/cmd_line.md
	echo '```'					>> docs/cmd_line.md
	echo ""						>> docs/cmd_line.md

doc: dashboard cmd_line.md
	echo --- doc:
	
	>  docs/fixme.md
	rgrep -ni "Fixme" docs/*.md | sed "s/:/|/2"	>> /tmp/fixme.md

	echo 'Fixme in current version:'		>  docs/fixme.md
	echo '-------------------------'		>> docs/fixme.md
	echo                            		>> docs/fixme.md
	echo 'Location | Text'             		>> docs/fixme.md
	echo '---------|-----'             		>> docs/fixme.md
	cat /tmp/fixme.md                       >> docs/fixme.md
	rm /tmp/fixme.md
	rgrep -ni "Fixme" src/*   | sed "s/:/|/2"	>> docs/fixme.md
	rgrep -ni "Fixme" tests/* | sed "s/:/|/2"	>> docs/fixme.md

	mkdocs build --clean --quiet
	@ - chmod --silent +x ./site/smk

	echo OK
	echo

.PHONY : clean
clean:
	echo --- clean:
	- $(MAKE) --directory=tests clean
	- ${RM} -rf obj/* docs/lcov/* tmp.txt *.lst *.dat cov_sum.txt gmon.out .smk.*
	- gnat clean -q -P smk.gpr
	echo OK
