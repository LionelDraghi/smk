all: check doc

TR=Tools/testrec
SMK=../smk

.PHONY : clean

check: ${SMK} header help version unknow_makefile list_makefile list_saved sanity

header:
	@ #---------------------------------------------------------------------
	${TR} clean
	${TR} create "Command line checks"

help:
	${TR} start "Help options" 
	${TR} cmt  								
	${TR} cmt "Test that the -h, --help or no command line will output :" 	
	
	${SMK} -h > out.expected_help.txt
	${TR} cmt 
	${TR} cmt 'Run:  '
	${TR} cmt '`smk `  '
	${TR} cmt '`smk --help`  '
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt 
	${TR} cmt '```' 						
	cat out.expected_help.txt >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt 

	${SMK} --help	> out.help1.txt
	${TR} assert true /usr/bin/sdiff "-sbB out.help1.txt out.expected_help.txt" 
	
	${SMK}  		> out.help2.txt 
	${TR} assert true /usr/bin/sdiff "-sbB out.help2.txt out.expected_help.txt" 

	${TR} end

version:
	${TR} start "Version option" 					
	${TR} cmt  								
	${TR} cmt "Test that the --version will put :" 			
	${TR} cmt 
	${TR} cmt 'Run:  '
	${TR} cmt '`smk --version`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt 
	${TR} cmt '```' 						
	echo "0.0.4"		> out.expected_version.txt
	cat out.expected_version.txt >> testrec.md					
	${TR} cmt '```' 						
	${TR} cmt 

	${SMK} --version	> out.version1.txt
	${TR} assert true /usr/bin/sdiff "-sbB out.version1.txt out.expected_version.txt"

	${TR} end

unknow_makefile:
	${TR} start "Unknow Makefile" 
	${TR} cmt  								
	${TR} cmt "Test the error message if an unknow MakeFile is given" 	
	
	${TR} cmt 
	${TR} cmt 'Run:  '
	${TR} cmt '`smk My_Makefile`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt 
	${TR} cmt '```' 						
	cat expected_unknow_makefile.txt >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt 

	test ! `${SMK} My_Makefile > out.err_unknow_makefile.txt`
	${TR} assert true /usr/bin/sdiff "-sbB out.err_unknow_makefile.txt expected_unknow_makefile.txt" 

	${TR} end
	
list_makefile:
	${TR} create "List functions checks"

	${TR} start '--list-makefile'
	${TR} cmt  								
	${TR} cmt "Test the MakeFile dump" 	
	
	${TR} cmt 
	${TR} cmt "Makefile:"
	${TR} cmt '```' 						
	cat hello.c/Makefile.3 >> testrec.md
	${TR} cmt 
	${TR} cmt '```' 						
	${TR} cmt 
	${TR} cmt 'Run:  '
	${TR} cmt '`smk -lm hello.c/Makefile.3`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 						
	cat expected_list_makefile.txt >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt 

	# $(MAKE) --directory=hello.c -f Makefile.2 hello
	${SMK} -lm hello.c/Makefile.3 > out.list_makefile.txt 
	${TR} assert true /usr/bin/sdiff "-sbB expected_list_makefile.txt out.list_makefile.txt" 

	${TR} end

list_saved:
	${TR} start '-ls | --list-saved-run'
	${TR} cmt	
	${TR} cmt "Test the previous run dump"
	
	${TR} cmt 
	${TR} cmt 'Run:  '
	${TR} cmt '`smk -q hello.c/Makefile.2`  '
	${TR} cmt '`smk -ls hello.c/Makefile.2`  '
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt "(note that to ease comparison, dates are removed)"
	${TR} cmt '```' 
	cat expected_ls.txt >> testrec.md
	${TR} cmt '```' 
	${TR} cmt 
	
	${SMK} -q hello.c/Makefile.2 
	# $(MAKE) --directory=hello.c -f Makefile.2
	${SMK} -ls hello.c/Makefile.2 | \
		sed "s/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g" | \
		sed "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g" > out.ls.txt 
	# to get the comparison working, numbers in date are removed
	${TR} assert true /usr/bin/sdiff "-sbB expected_ls.txt out.ls.txt" 

	${TR} end

sanity:
	${TR} create "Sanity checks"

	${TR} cmt  								
	${TR} cmt "Makefile:"
	${TR} cmt '```' 						
	cat hello.c/Makefile.1 >> testrec.md
	${TR} cmt 
	${TR} cmt '```' 						

	${TR} start 'First `smk`, after `make`, should run no command'
	${TR} cmt 
	${TR} cmt 'Run:  '
	${TR} cmt '`smk -e hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 						
	cat expected_sanity.1 >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt 
	$(MAKE) --directory=hello.c -f Makefile.2
	${SMK} -e hello.c/Makefile.2 > out.1
	${TR} assert true /usr/bin/sdiff "-sbB expected_sanity.1 out.1" 
	${TR} end

	${TR} start 'Second `smk`, should not run any command'
	${TR} cmt 
	${TR} cmt 'Run:  '
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

	${TR} start 'smk --clean, no more history, should run all commands'
	${TR} cmt 
	${TR} cmt 'Run:  '
	${TR} cmt '`smk --clean`  '
	${TR} cmt '`smk -e hello.c/Makefile.2`'
	${TR} cmt 
	${TR} cmt 'Expected:'
	${TR} cmt '```' 						
	cat expected_sanity.3 >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt 
	${SMK} --clean
	${SMK} -e hello.c/Makefile.2 > out.3 
	${TR} assert true /usr/bin/sdiff "-sbB expected_sanity.3 out.3" 
	${TR} end

	${TR} start '`smk -a`, should run all commands even if not needed'
	${TR} cmt 
	${TR} cmt 'Run:  '
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

	${TR} start '`rm main.o` (missing file)'
	${TR} cmt 
	${TR} cmt 'Run:  '
	${TR} cmt '`rm hello.c/main.o`  '
	${TR} cmt '`smk -e hello.c/Makefile.2`  '
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

	${TR} start '`touch hello.c` (updated file)'
	${TR} cmt 
	${TR} cmt 'Run:  '
	${TR} cmt '`rm hello.c/main.o`  '
	${TR} cmt '`smk -e hello.c/Makefile.2`  '
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
	${SMK} --clean
	- ${RM} -rf tests_count.txt *.lst out.* *.o hello 
	- $(MAKE) --directory=hello.c clean
