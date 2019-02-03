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

TESTS=$(wildcard *_tests)

.PHONY : clean

all: check doc

check: ../smk 
	@ #-------------------------------------------------------------------------
	# for test in $(TESTS); do				\
		# 	echo ;								\
		# 	echo "   - "$$test;					\
		# 	$(MAKE) check --directory=$$test;	\
		# done
	echo
	echo 01_sanity_tests
	$(MAKE) check --directory=01_sanity_tests
	echo
	echo 02_website_sanity_tests
	$(MAKE) check --directory=02_website_sanity_tests
	echo
	echo 03_read_queries_tests
	$(MAKE) check --directory=03_read_queries_tests
	echo
	echo 04_list_queries_tests
	$(MAKE) check --directory=04_list_queries_tests
	echo
	echo 05_target_tests
	$(MAKE) check --directory=05_target_tests
	echo
	echo 06_implicit_naming_tests
	$(MAKE) check --directory=06_implicit_naming_tests
	echo
	echo 07_run_error_tests
	$(MAKE) check --directory=07_run_error_tests
	echo
	echo 08_cmd_line_tests
	$(MAKE) check --directory=08_cmd_line_tests
	echo
	echo 09_multiline_tests
	$(MAKE) check --directory=09_multiline_tests
	echo
	echo 10_section_tests
	$(MAKE) check --directory=10_section_tests
	echo
	echo 11_command_run_tests
	$(MAKE) check --directory=11_command_run_tests
	echo
	echo 12_mp3_conversions_tests
	$(MAKE) check --directory=12_mp3_conversions_tests
	echo
	echo 13_file_utilities_tests
	$(MAKE) check --directory=13_file_utilities_tests
	echo
	echo 14_strace_analysis_tests
	$(MAKE) check --directory=14_strace_analysis_tests
	echo
	echo 15_tutorial_tests
	$(MAKE) check --directory=15_tutorial_tests
	echo
	echo 16_dir_ops_tests
	$(MAKE) check --directory=16_dir_ops_tests
	echo

doc:
	cat *_tests/testrec.md > testrec.md

	- grep '\[Successful\]'  testrec.md > Successful.lst
	- grep '\[Failed\]'      testrec.md > Failed.lst
	- grep '\[Empty\]'       testrec.md > Empty.lst
	
	> tests_count.txt
	echo "Successful " `cat Successful.lst | wc -l`	>> tests_count.txt
	echo "Failed     " `cat Failed.lst     | wc -l`	>> tests_count.txt
	echo "Empty      " `cat Empty.lst      | wc -l`	>> tests_count.txt

	>  tests_status.md
	echo '# Tests Status'	                               	>> tests_status.md
	echo ""                                                 >> tests_status.md
	echo '## Successful'	                               	>> tests_status.md
	echo ""                                                 >> tests_status.md
	echo "   "`cat Successful.lst | wc -l`" tests OK"       >> tests_status.md
	echo ""                                                 >> tests_status.md
	sed "s/^ */  - \[/;s/ \[Successful\].*/\]\(testrec.md\#\)/" Successful.lst	>> tests_status.md

	echo ""                                                 >> tests_status.md
	echo '## Failed'                                   		>> tests_status.md
	echo ""                                                 >> tests_status.md
	echo "   "`cat Failed.lst | wc -l`" tests failed"       >> tests_status.md
	echo ""                                                 >> tests_status.md
	sed "s/^** /  - \[/;s/ \[Failed\].*/\]\(testrec.md\#\)/" Failed.lst >> tests_status.md

	echo ""                                                 >> tests_status.md
	echo '## Empty'                                    		>> tests_status.md
	echo ""                                                 >> tests_status.md
	echo "   "`cat Empty.lst | wc -l`" empty tests"         >> tests_status.md
	echo ""                                                 >> tests_status.md
	sed "s/^ */  - \[/;s/ \[Empty\].*/\]\(testrec.md\#\)/" Empty.lst	>> tests_status.md

	mv tests_status.md ../docs/tests
	mv testrec.md ../docs/tests/

clean:
	- ${RM} tests_count.txt *.lst 
	for test in $(TESTS); do \
		$(MAKE) -i clean --directory=$$test; \
	done
	- $(MAKE) --directory=./hello.c clean

