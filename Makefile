# ------------------------------------------------------------------------------
# smk, the smart make (https://github.com/LionelDraghi/smk)
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

.PHONY : help
help:
	echo "Usage: make [target]"
	echo ""
	echo "Targets:"
	echo "  all         : build, check and doc (default when no target given)"
	echo "  build       : build smk in validation mode"
	echo "  release     : build smk in release mode"
	echo "  install     : build in release mode, and copy smk to ~/bin"
	echo "  check       : run the test suites (15 dirs), and build"
	echo "                 the coverage report"
	echo "  dashboard   : regenerate docs/dashboard.md and docs/tests.json"
	echo "                 (requires a previous make check)"
	echo "  cmd_line.md : regenerate docs/cmd_line.md"
	echo "  doc         : regenerate the generated docs (cmd_line.md,"
	echo "                 dashboard, tests badge, fixme index)"
	echo "  clean       : remove the build and test artifacts"
	echo ""
	echo "Refer to README.md and docs/contributing.md for more details."
	echo "(NB: run make with LD_LIBRARY_PATH unset if your environment"
	echo "     pollutes it, e.g. from a VSCode extension)"

build:
	echo
	echo --- build:
	echo
	alr --non-interactive build --validation
	# Alire profiles : --release --validation --development (default)
	echo

release:
	echo
	echo --- build for release:
	echo
	alr --non-interactive build --release
	echo

install: release
	echo
	echo --- install:
	cp -p smk ~/bin
	echo OK
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
	alr exec -- lcov --quiet --capture --directory obj -o obj/coverage.info
	alr exec -- lcov --quiet --remove obj/coverage.info -o obj/coverage.info \
		"*/adainclude/*" "*.ads" "*/obj/b__*.adb" "*/tests/*"
	# Ignoring :
	# - spec (results are not consistent with current gcc version) 
	# - the false main
	# - libs (Standard)
	# - unit test main

	alr exec -- genhtml obj/coverage.info -o docs/lcov --title "smk tests coverage" \
		--prefix "$(CURDIR)/src" --frames | tail -n 2 > cov_sum.txt
	# --title  : Display TITLE in header of all pages
	# --prefix : Remove PREFIX from all directory names
	# --frame  : Use HTML frames for source code view
	cat cov_sum.txt
	echo

.PHONY : dashboard
dashboard: obj/coverage.info tests/tests_count.txt

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
	echo 							>> docs/dashboard.md
	echo "Coverage"					>> docs/dashboard.md
	echo "--------"					>> docs/dashboard.md
	echo 							>> docs/dashboard.md
	echo '```'			 			>> docs/dashboard.md
	cat cov_sum.txt					>> docs/dashboard.md
	echo '```'			 			>> docs/dashboard.md
	echo 							>> docs/dashboard.md
	echo '[**Coverage details in the sources**](lcov/src/index.html)'	>> docs/dashboard.md
	echo 							>> docs/dashboard.md

	# dynamic tests badge for shields.io
	# (read by the README badge through raw.githubusercontent.com):
	ok=`sed -n "s/Successful  //p" tests/tests_count.txt`; \
	ko=`sed -n "s/Failed      //p" tests/tests_count.txt`; \
	color=`if [ "$$ko" = "0" ]; then echo green; else echo red; fi`; \
	echo '{"schemaVersion": 1, "label": "tests", "message": "'$$ok' OK, '$$ko' failed", "color": "'$$color'"}' > docs/tests.json

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
	grep -rni "Fixme" docs/*.md | sed "s/:/|/2"	>> /tmp/fixme.md

	echo 'Fixme in current version:'		>  docs/fixme.md
	echo '-------------------------'		>> docs/fixme.md
	echo                            		>> docs/fixme.md
	echo 'Location | Text'             		>> docs/fixme.md
	echo '---------|-----'             		>> docs/fixme.md
	cat /tmp/fixme.md                       >> docs/fixme.md
	rm /tmp/fixme.md
	grep -rn "Fixme:" src/*           | sed "s/:/|/2"	>> docs/fixme.md
	grep -rn "Fixme:" tests/*_tests/* | sed "s/:/|/2"	>> docs/fixme.md

	echo OK
	echo

.PHONY : clean
clean:
	echo --- clean:
	- $(MAKE) --directory=tests clean
	- ${RM} -rf obj/* docs/lcov/* tmp.txt *.lst *.dat cov_sum.txt gmon.out .smk.*
	- alr clean
	echo OK
