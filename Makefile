# -----------------------------------------------------------------------------
# Copyright 2018 Lionel Draghi
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# -----------------------------------------------------------------------------
# This file is part of the smk project
# available at https://github.com/LionelDraghi/smk
# -----------------------------------------------------------------------------

.SILENT:

all: build check

build:
	echo --- build:
	gprbuild -P smk.gpr
	echo

check: smk
	echo --- tests:
	echo
	$(MAKE) --directory=tests
	
	echo
	echo --- tests summary :
	cat tests/tests_count.txt

	# --------------------------------------------------------------------
	echo
	echo - Coverage report :

	lcov --quiet --capture --directory obj -o obj/coverage.info
	lcov -q --remove obj/coverage.info -o obj/coverage.info \
		"/usr/*" "*.ads" "*/obj/smk-main.adb"
	# Ignoring :
	# - spec (results are not consistent with current gcc version) 
	# - the false main
	# - libs (Standard)
	# - OpenToken

	genhtml obj/coverage.info -o docs/lcov --title "smk tests coverage" \
		--prefix "/home/lionel/Proj/smk" --frames | tail -n 2 > cov_sum.txt
	# --title  : Display TITLE in header of all pages
	# --prefix : Remove PREFIX from all directory names
	# --frame  : Use HTML frames for source code view
	cat cov_sum.txt
	echo

.PHONY : dashboard
dashboard: obj/coverage.info tests/tests_count.txt
	echo Make dashboard

	@ # Language pie
	@ # --------------------------------------------------------------------
	sloccount src | grep "ada=" |  ploticus  -prefab pie 	\
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
	echo "> smk --version"			>> docs/dashboard.md
	echo 	 						>> docs/dashboard.md
	echo '```' 						>> docs/dashboard.md
	./smk --version					>> docs/dashboard.md
	echo '```' 						>> docs/dashboard.md
	echo 	 						>> docs/dashboard.md
	echo "> date -r ./smk --iso-8601=seconds" 	>> docs/dashboard.md
	echo 	 						>> docs/dashboard.md
	echo '```' 						>> docs/dashboard.md
	date -r ./smk --iso-8601=seconds 	>> docs/dashboard.md
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
	wget -q "https://img.shields.io/badge/Version-`./smk --version`-blue.svg" -O docs/img/version.svg
	wget -q "https://img.shields.io/badge/tests_OK-`cat tests/tests_count.txt |sed -n "s/Successful  //p"`-green.svg" -O docs/img/tests_ok.svg
	wget -q "https://img.shields.io/badge/tests_KO-`cat tests/tests_count.txt |sed -n "s/Failed      //p"`-green.svg" -O docs/img/tests_ko.svg

.PHONY : cmd_line.md
cmd_line.md:
	echo Make cmd_line.md
	> docs/cmd_line.md
	echo "smk command line"		>> docs/cmd_line.md
	echo "================"		>> docs/cmd_line.md
	echo ""						>> docs/cmd_line.md
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
	echo "$ smk --version"		>> docs/cmd_line.md
	echo '```'					>> docs/cmd_line.md
	echo ""						>> docs/cmd_line.md
	echo '```'					>> docs/cmd_line.md
	./smk --version				>> docs/cmd_line.md
	echo '```'					>> docs/cmd_line.md
	echo ""						>> docs/cmd_line.md

doc: dashboard cmd_line.md
	echo Make Doc
	mkdocs build --clean
	@ - chmod --silent +x ./site/smk

.PHONY : clean
clean:
	echo --- clean:
	- gnat clean -q -P smk.gpr
	- ${RM} -rf obj/* docs/lcov/* tmp.txt *.lst *.dat cov_sum.txt gmon.out
	- $(MAKE) --directory=tests clean
	echo OK
