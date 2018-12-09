# all: check doc

TR=Tools/testrec
SMK=../smk

.PHONY : clean

check: ${SMK} header sanity cmd_line read_queries list_queries implicit_name targets multiline doc

header:
	@ #-------------------------------------------------------------------------
	${TR} clean

sanity:
	${TR} create "Sanity"

	${TR} cmt  								
	${TR} cmt "Makefile:"
	${TR} cmt '```' 						
	cat hello.c/Makefile.1 >> testrec.md
	${TR} cmt 
	${TR} cmt '```' 						

	@ #-------------------------------------------------------------------------
	${TR} start 'First `smk`, after `make`, should run no command'
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -q hello.c/Makefile.2`'
	${TR} cmt '`smk -e hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 						
	cat expected_sanity.1 >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt 
	# $(MAKE) --directory=hello.c -f Makefile.2 
	# let's do it with smk:
	${SMK} -q hello.c/Makefile.2
	${SMK} -e hello.c/Makefile.2 > out.1
	${TR} assert true /usr/bin/sdiff "-sbB expected_sanity.1 out.1" 
	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start 'Second `smk`, should not run any command'
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -e hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 						
	cat expected_sanity.2 >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt 
	${SMK} -e hello.c/Makefile.2 > out.2
	${TR} assert true /usr/bin/sdiff "-sbB expected_sanity.2 out.2" 
	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start '`smk --reset`, no more history, should run all commands'
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk --reset --quiet`'
	${TR} cmt '`smk -e hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 						
	cat expected_sanity.3 >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt 
	${SMK} --reset --quiet
	${SMK} -e hello.c/Makefile.2 > out.3 
	${TR} assert true /usr/bin/sdiff "-sbB expected_sanity.3 out.3" 
	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start '`smk -a`, should run all commands even if not needed'
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -e -a hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 						
	cat expected_sanity.4 >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt 
	${SMK} -e -a hello.c/Makefile.2 > out.4 
	${TR} assert true /usr/bin/sdiff "-sbB expected_sanity.4 out.4" 
	${TR} end
	
	# Fixme: sleep needed because of close consecutive smk run that disrupt the algorithm
	sleep 1

	@ #-------------------------------------------------------------------------
	${TR} start '`rm main.o` (missing file)'
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`rm hello.c/main.o`'
	${TR} cmt '`smk -e hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 						
	cat expected_sanity.5 >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt 
	rm hello.c/main.o
	${SMK} -e hello.c/Makefile.2 | sed "s/[0-9]//g" > out.5
	# Time tag neutralized
	${TR} assert true /usr/bin/sdiff "-sbB expected_sanity.5 out.5" 
	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start '`touch hello.c` (updated file)'
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`rm hello.c/main.o`'
	${TR} cmt '`smk -e hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 						
	cat expected_sanity.6 >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt 
	touch hello.c/hello.c
	${SMK} -e hello.c/Makefile.2 | sed "s/[0-9]//g" > out.6
	# Time tag neutralized
	${TR} assert true /usr/bin/sdiff "-sbB expected_sanity.6 out.6" 
	${TR} end

cmd_line:
	@ #-------------------------------------------------------------------------
	${TR} create "Command line"

	${TR} start "Help options" 
	${TR} cmt  								
	${TR} cmt "Test the -h and --help output :" 	
	${SMK} -h > out.expected_help.txt

	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk `'
	${TR} cmt '`smk --help`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt 
	${TR} cmt '```' 						
	cat out.expected_help.txt >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt 

	${SMK} --help	> out.help1.txt
	${TR} assert true /usr/bin/sdiff "-sbB out.expected_help.txt out.help1.txt" 
	
	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start "Version option" 					
	${TR} cmt  								
	${TR} cmt "Test that the --version will put :" 			
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk --version`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt 
	${TR} cmt '```' 						
	echo "0.0.5"		> out.expected_version.txt
	cat out.expected_version.txt >> testrec.md					
	${TR} cmt '```' 						
	${TR} cmt 

	${SMK} --version	> out.version1.txt
	${TR} assert true /usr/bin/sdiff "-sbB out.expected_version.txt out.version1.txt"

	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start "Illegal cmd lines" 
	${TR} cmt  								
	${TR} cmt 'Run:'
	${TR} cmt '`smk -rs -rl`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt 
	${TR} cmt '```' 						
	cat expected_wrong_cmd_line1.txt >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt 

	test ! `${SMK} -rs -rl > out.wrong_cmd_line1.txt`
	${TR} assert true /usr/bin/sdiff "-sbB expected_wrong_cmd_line1.txt out.wrong_cmd_line1.txt" 
	
	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start "Unknow Makefile" 
	${TR} cmt  								
	${TR} cmt "Test the error message if an unknow MakeFile is given" 	
	
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk My_Makefile`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt 
	${TR} cmt '```' 						
	cat expected_unknow_makefile.txt >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt 

	test ! `${SMK} My_Makefile > out.err_unknow_makefile.txt`
	${TR} assert true /usr/bin/sdiff "-sbB expected_unknow_makefile.txt out.err_unknow_makefile.txt" 

	${TR} end
	
read_queries:
	${TR} create "Read queries"

	@ #-------------------------------------------------------------------------
	${TR} start '-rs | --read-smkfile'
	${TR} cmt  								
	${TR} cmt "Read a smkfile and shows what is understud by smk"
	
	${TR} cmt 
	${TR} cmt "Makefile:"
	${TR} cmt '```' 						
	cat hello.c/Makefile.3 >> testrec.md
	${TR} cmt 
	${TR} cmt '```' 						
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -rs hello.c/Makefile.3`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 						
	cat expected_read_smkfile.txt >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt 

	${SMK} -rs hello.c/Makefile.3 | sed "s/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g" | \
		         sed "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g" > out.read_smkfile.txt 
	# to get the comparison working, numbers in date are removed
	${TR} assert true /usr/bin/sdiff "-sbB expected_read_smkfile.txt out.read_smkfile.txt" 

	${TR} end


	@ #-------------------------------------------------------------------------
	${TR} start '-rl | --read-last-run'
	${TR} cmt 
	${TR} cmt "Read the previous run dump and shows sources and targets"

	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -q --reset`'
	${TR} cmt '`smk -q --build hello.c/Makefile.2`'
	${TR} cmt '`smk -rl hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt "(note that to ease comparison, dates are removed)"
	${TR} cmt '```' 
	cat expected_lpr.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	${SMK} -q --reset
	${SMK} -q --build hello.c/Makefile.2 
	${SMK} -rl | sed "s/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g" | \
		         sed "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g" > out.lpr.txt 
	# to get the comparison working, numbers in date are removed
	${TR} assert true /usr/bin/sdiff "-sbB expected_lpr.txt out.lpr.txt" 

	${TR} cmt 'Run: (same with system files not ignored) '
	${TR} cmt '`smk -q --reset`'
	${TR} cmt '`smk -q --build hello.c/Makefile.2`'
	${TR} cmt '`smk -rl -sa hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt "(note that to ease comparison, dates are removed)"
	${TR} cmt '```' 
	cat expected_lpr1.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	${SMK} -q --reset
	${SMK} -q --build hello.c/Makefile.2 
	${SMK} -rl -sa | sed "s/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g" | \
		             sed "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g" > out.lpr1.txt 
	# to get the comparison working, numbers in date are removed
	${TR} assert true /usr/bin/sdiff "-sbB expected_lpr1.txt out.lpr1.txt" 

	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -q --reset`'
	${TR} cmt '`smk -rl hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_lpr2.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt

	${SMK} -q --reset
	test ! `${SMK} -rl hello.c/Makefile.2 > out.lpr2.txt`

	# to get the comparison working, numbers in date are removed
	${TR} assert true /usr/bin/sdiff "-sbB expected_lpr2.txt out.lpr2.txt" 

	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk --read-last-run`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_lpr3.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	${SMK} -q --reset
	test ! `${SMK} --read-last-run > out.lpr3.txt`
	# to get the comparison working, numbers in date are removed
	${TR} assert true /usr/bin/sdiff "-sbB expected_lpr3.txt out.lpr3.txt" 

	${TR} end

list_queries:
	${TR} create "List queries"

	@ #-------------------------------------------------------------------------
	${TR} start '-lr | --list-runs'
	${TR} cmt	
	${TR} cmt "Test available previous runs"
	
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -q --reset`'
	${TR} cmt '`smk -lr`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_lr1.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -q -b hello.c/Makefile.2`'
	${TR} cmt '`smk -q hello.c/Makefile.3`'
	${TR} cmt '`smk --list-runs`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_lr2.txt >> testrec.md
	echo ""              >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	${SMK} -q --reset
	test ! `${SMK} -lr > out.lr1.txt`
	${TR} assert true /usr/bin/sdiff "-sbB expected_lr1.txt out.lr1.txt" 

	${SMK} -q -b hello.c/Makefile.2 
	${SMK} -q hello.c/Makefile.3 
	${SMK} --list-runs > out.lr2.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_lr2.txt out.lr2.txt" 

	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start '-lt | --list-targets'
	
	${TR} cmt 'Run:'
	${TR} cmt '`smk -lt hello.c/Makefile.2`'
	${TR} cmt '`smk --list-targets hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_lt.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	${SMK} -lt hello.c/Makefile.2 > out.lt1.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_lt.txt out.lt1.txt" 

	${SMK} --list-targets hello.c/Makefile.2 > out.lt2.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_lt.txt out.lt2.txt" 

	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start '-ls | --list-sources'
	
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -ls hello.c/Makefile.2`'
	${TR} cmt '`smk --list-sources hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_ls.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	${SMK} -ls hello.c/Makefile.2 > out.ls1.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_ls.txt out.ls1.txt" 

	${SMK} --list-sources hello.c/Makefile.2 > out.ls2.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_ls.txt out.ls2.txt" 

	${TR} end

	@ #-------------------------------------------------------------------------
	${TR} start '-ls | --list-sources --show-all-files'
	
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -las hello.c/Makefile.2`'
	${TR} cmt '`smk --list-sources --shows-system-files hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_las.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	${SMK} --list-sources --show-all-files hello.c/Makefile.2 > out.las1.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_las.txt out.las1.txt" 

	${SMK} -ls -sa hello.c/Makefile.2 > out.las2.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_las.txt out.las2.txt" 

	${TR} end

implicit_name:
	${TR} create "Implicit naming"

	@ #-------------------------------------------------------------------------
	${TR} start 'Implicit naming'
	${TR} cmt	
	${TR} cmt "Test that when there is only one run file in the directory, smk assume it without giving it on the command line."
	${TR} cmt 

	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -q --reset`'
	${TR} cmt '`smk`'
	${TR} cmt 
	${TR} cmt 'Expected: help message as there is nothing in the directory'
	${TR} cmt 
	${TR} cmt '```' 						
	cat expected_implicit1.txt >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt
	 
	${SMK} -q --reset
	test ! `${SMK} > out.implicit1.txt`
	${TR} assert true /usr/bin/sdiff "-sbB expected_implicit1.txt out.implicit1.txt" 

	${TR} cmt 'Run:'
	${TR} cmt '`smk -q hello.c/Makefile.2`'
	${TR} cmt '`touch hello.c/hello.c`'
	${TR} cmt '`smk`'
	${TR} cmt 
	${TR} cmt "Expected: smk re-run Makefile.2, as it is the only one in the dir"
	${TR} cmt '```' 
	cat expected_implicit2.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 

	${SMK} -q hello.c/Makefile.2 
	# Fixme: sleep needed because of close consecutive smk run that disrupt the algorithm
	sleep 1
	touch hello.c/hello.c
	${SMK} > out.implicit2.txt
	${TR} assert true /usr/bin/sdiff "-sbB expected_implicit2.txt out.implicit2.txt" 

	${TR} cmt 'Run:'
	${TR} cmt '`smk -q hello.c/Makefile.3`'
	${TR} cmt '`smk `'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt "   There is more than one possible run, smk display the list but don't do anything else "
	${TR} cmt '```' 
	cat expected_implicit3.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	${SMK} -q hello.c/Makefile.3 
	test ! `${SMK} > out.implicit3.txt`
	${TR} assert true /usr/bin/sdiff "-sbB expected_implicit3.txt out.implicit3.txt" 

	${TR} end

targets:
	${TR} create "Targets related functions"

	@ #-------------------------------------------------------------------------
	${TR} start '--clean'
	${TR} cmt	
	${TR} cmt "Test targets cleaning"
	
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk --reset`'
	${TR} cmt '`smk -q -b hello.c/Makefile.2`'
	${TR} cmt '`smk --clean --dry-run`'
	${TR} cmt '`smk --explain`  (to check that nothing was actually deleted)'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_targets1.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	${SMK} -q --reset
	${SMK} -q -b hello.c/Makefile.2
	${SMK} --clean --dry-run > out.targets1.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_targets1.txt out.targets1.txt" 
	${SMK} --explain          > out.targets2.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_targets2.txt out.targets2.txt" 

	${TR} cmt 'Run:'
	${TR} cmt '`smk --clean`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_targets3.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 

	${SMK} --clean > out.targets3.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_targets3.txt out.targets3.txt" 

	${TR} cmt 'Run:'
	${TR} cmt '`smk -e`  (to check effective cleaning)'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_targets4.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 

	${SMK} -e      > out.targets4.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_targets4.txt out.targets4.txt" 

	${TR} end

multiline:
	${TR} create "Multiline commands in smkfile"
	sloccount ../src | grep "ada=" > out.sloccount

	@ #-------------------------------------------------------------------------
	${TR} cmt 
	${TR} cmt 'cat `multiline_smkfile.txt`:'
	${TR} cmt '```' 
	cat multiline_smkfile.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -q --reset`'
	${TR} cmt '`smk multiline_smkfile.txt`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_multiline1.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	${SMK} -q --reset
	${SMK} multiline_smkfile.txt > out.multiline1.txt
	${TR} assert true /usr/bin/sdiff "-sbB expected_multiline1.txt out.multiline1.txt" 

	@ #-------------------------------------------------------------------------
	${TR} cmt 'Hill formatted multiline:'
	${TR} cmt 
	${TR} cmt 'cat `hill_multiline_smkfile.txt`:'
	${TR} cmt '```' 
	cat hill_multiline_smkfile.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	${TR} cmt 'Run:'
	${TR} cmt '`smk -q --reset`'
	${TR} cmt '`smk hill_multiline_smkfile.txt`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 
	cat expected_multiline2.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	${SMK} -q --reset
	test ! `${SMK} hill_multiline_smkfile.txt > out.multiline2.txt`
	${TR} assert true /usr/bin/sdiff "-sbB expected_multiline2.txt out.multiline2.txt" 

	${TR} end

doc:
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

	cp testrec.md ../docs/tests/

clean:
	${TR} clean
	${SMK} --reset
	- ${RM} -rf tests_count.txt *.lst out.* *.o hello 
	- $(MAKE) --directory=hello.c clean
