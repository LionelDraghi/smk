# ------------------------------------------------------------------------------
# smk, the smart make (https://github.com/LionelDraghi/smk)
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

.PHONY : clean

all: check doc

check: ../smk
	@ #-------------------------------------------------------------------------
	# The scenarios are run in a fresh run dir, recreated at each check,
	# with a smk link to the built binary, so that the scenarios just
	# invoke `smk`:
	rm -rf run
	mkdir run
	ln -s ../../smk run/smk
	cd run && PATH="$$PWD:$$PATH" bbt -k --yes --index results.md \
	  ../sanity/sanity.md \
	  ../../docs/Features/A010_queries.md \
	  ../../docs/Features/A020_build.md \
	  ../../docs/Features/A030_smkfile_format.md \
	  ../../docs/Features/A040_errors.md \
	  ../../docs/Features/A050_use_cases.md \
	  ../../docs/tutorial.md
	$(MAKE) check --directory=unit_file_utilities
	$(MAKE) check --directory=unit_strace_analysis
	# the unit tests results are appended to the bbt index:
	cat unit_file_utilities/results.md unit_strace_analysis/results.md >> run/results.md
	mv run/results.md ../docs/tests/results.md

doc:
	# bbt format: "- [X] scenario ..."; unit tests format: "Tests [Successful]"
	- grep -E '\[X\] scenario'  ../docs/tests/results.md > Successful.lst
	- grep -E '\[ \] scenario'  ../docs/tests/results.md > Failed.lst
	- grep '\[Successful\]'     ../docs/tests/results.md > Successful_unit.lst
	- grep '\[Failed\]'         ../docs/tests/results.md > Failed_unit.lst

	> tests_count.txt
	echo "Successful " `cat Successful.lst Successful_unit.lst | wc -l`	>> tests_count.txt
	echo "Failed     " `cat Failed.lst Failed_unit.lst | wc -l`		>> tests_count.txt

	>  tests_status.md
	echo '# Tests Status'		>> tests_status.md
	echo ""			>> tests_status.md
	echo '## Successful'		>> tests_status.md
	echo ""			>> tests_status.md
	echo "   "`cat Successful.lst Successful_unit.lst | wc -l`" tests OK"	>> tests_status.md
	echo ""			>> tests_status.md
	sed "s/^ */  - \[/;s/ \[Successful\].*/\]\(results.md\#\)/" Successful_unit.lst	>> tests_status.md

	echo ""			>> tests_status.md
	echo '## Failed'		>> tests_status.md
	echo ""			>> tests_status.md
	echo "   "`cat Failed.lst Failed_unit.lst | wc -l`" tests failed"	>> tests_status.md
	echo ""			>> tests_status.md
	sed "s/^ */  - \[/;s/ \[Failed\].*/\]\(results.md\#\)/" Failed_unit.lst		>> tests_status.md

	mv tests_status.md ../docs/tests

clean:
	- ${RM} tests_count.txt *.lst
	rm -rf run
	$(MAKE) -i clean --directory=unit_file_utilities
	$(MAKE) -i clean --directory=unit_strace_analysis
