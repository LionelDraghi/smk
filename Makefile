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

# Tests counts, extracted from the bbt results summary table
TESTS_COUNT = grep -E '^\| (Successful|Failed) ' docs/tests/results.md | sed 's/|//g;s/^ *//;s/ *$$//;s/  */ /g'

.PHONY : help
help:
	echo "Usage: make [target]"
	echo ""
	echo "Targets:"
	echo "  all         : build, check and doc (default when no target given)"
	echo "  build       : build smk in validation mode"
	echo "  release     : build smk in release mode"
	echo "  install     : build in release mode, and copy smk to ~/bin"
	echo "  check       : run the test suites (bbt scenarios, then unit tests)"
	echo "  dashboard   : regenerate docs/dashboard.md"
	echo "                 (requires a previous make check)"
	echo "  cmd_line.md : regenerate docs/cmd_line.md"
	echo "  doc         : regenerate the generated docs (cmd_line.md,"
	echo "                 dashboard, fixme index)"
	echo "  clean       : remove the build and test artifacts"
	echo ""
	echo "Refer to README.md and docs/dev/development_workflow.md for more details."
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
	echo --- install:
	cp -p smk ~/bin
	echo OK
	echo

check: build
	echo --- tests:
	$(MAKE) --directory=tests
	echo

	echo --- tests summary:
	echo
	$(TESTS_COUNT)
	echo

.PHONY : dashboard
dashboard: docs/tests/results.md

	>  docs/dashboard.md
	echo "Dashboard"				>> docs/dashboard.md
	echo "========="				>> docs/dashboard.md
	echo 					>> docs/dashboard.md
	echo "Version"					>> docs/dashboard.md
	echo "-------"					>> docs/dashboard.md
	echo "> smk version"				>> docs/dashboard.md
	echo 						>> docs/dashboard.md
	echo '```' 					>> docs/dashboard.md
	./smk version					>> docs/dashboard.md
	echo '```' 					>> docs/dashboard.md
	echo 						>> docs/dashboard.md
	echo "> date -r ./smk --iso-8601=seconds" 	>> docs/dashboard.md
	echo 						>> docs/dashboard.md
	echo '```' 					>> docs/dashboard.md
	date -r ./smk --iso-8601=seconds 		>> docs/dashboard.md
	echo '```' 					>> docs/dashboard.md
	echo 						>> docs/dashboard.md
	echo "Test results"				>> docs/dashboard.md
	echo "------------"				>> docs/dashboard.md
	echo '```'					>> docs/dashboard.md
	$(TESTS_COUNT)					>> docs/dashboard.md
	echo '```'					>> docs/dashboard.md
	echo 						>> docs/dashboard.md

.PHONY : cmd_line.md
cmd_line.md:
	> docs/cmd_line.md
	echo "smk command line"			>> docs/cmd_line.md
	echo "----------------"			>> docs/cmd_line.md
	echo ""					>> docs/cmd_line.md
	echo '```'				>> docs/cmd_line.md
	echo "$ smk -h" 				>> docs/cmd_line.md
	echo '```'				>> docs/cmd_line.md
	echo ""					>> docs/cmd_line.md
	echo '```'				>> docs/cmd_line.md
	./smk -h			 		>> docs/cmd_line.md
	echo '```'				>> docs/cmd_line.md
	echo ""					>> docs/cmd_line.md
	echo "smk current version"		>> docs/cmd_line.md
	echo "-------------------"		>> docs/cmd_line.md
	echo ""					>> docs/cmd_line.md
	echo '```'				>> docs/cmd_line.md
	echo "$ smk version"			>> docs/cmd_line.md
	echo '```'				>> docs/cmd_line.md
	echo ""					>> docs/cmd_line.md
	echo '```'				>> docs/cmd_line.md
	./smk version				>> docs/cmd_line.md
	echo '```'				>> docs/cmd_line.md
	echo ""					>> docs/cmd_line.md

doc: dashboard cmd_line.md
	echo --- doc:

	>  docs/dev/fixme_index.md
	grep -rni "Fixme" docs/*.md docs/dev/*.md | sed "s/:/|/2"	>> /tmp/fixme.md

	echo 'Fixme in current version:'		>  docs/dev/fixme_index.md
	echo '-------------------------'		>> docs/dev/fixme_index.md
	echo                           		>> docs/dev/fixme_index.md
	echo 'Location | Text'            		>> docs/dev/fixme_index.md
	echo '---------|-----'            		>> docs/dev/fixme_index.md
	cat /tmp/fixme.md                       	>> docs/dev/fixme_index.md
	rm /tmp/fixme.md
	grep -rn "Fixme:" src/*           	| sed "s/:/|/2"	>> docs/dev/fixme_index.md
	grep -rn "Fixme:" tests/sanity tests/unit_* docs/Features | sed "s/:/|/2"	>> docs/dev/fixme_index.md

	echo OK
	echo

.PHONY : clean
clean:
	echo --- clean:
	- $(MAKE) --directory=tests clean
	- ${RM} -rf obj/* tmp.txt *.lst *.dat gmon.out .smk.*
	- alr clean
	echo OK
