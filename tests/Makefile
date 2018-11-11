all: check doc

TR=Tools/testrec
SMK=../smk

.PHONY : clean

check: ${SMK} header help version unknow_makefile

header:
	@ #---------------------------------------------------------------------
	${TR} clean
	${TR} create "Smk test suite"

	${TR} cmt  								
	${TR} cmt "This test check that illegal command lines cause archicheck to"	
	${TR} cmt "exit with a non null return code." 				
	${TR} cmt "- Note that normal use is overly tested in other tests," 		
	${TR} cmt "  so here mainly error situations are tested."			
	${TR} cmt "- Note also that quiet and verbose mode (-q / -v) are also tested"	
	${TR} cmt "  in other tests."							
	${TR} cmt  								

help:
	${TR} start "Help options" 
	${TR} cmt  								
	${TR} cmt "Test that the -h, --help or no command line will output :" 	
	
	${SMK} -h > expected_help.txt
	${TR} cmt 
	${TR} cmt '```' 						
	cat expected_help.txt >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt 

	${SMK} --help	> help1.txt
	${TR} assert true /usr/bin/sdiff "-sbB help1.txt expected_help.txt" 
	
	${SMK}  		> help2.txt 
	${TR} assert true /usr/bin/sdiff "-sbB help2.txt expected_help.txt" 

	${TR} end

version:
	${TR} start "Version option" 					
	${TR} cmt  								
	${TR} cmt "Test that the --version will put :" 			
	${TR} cmt 
	${TR} cmt '```' 						
	echo "0.0.1"		> expected_version.txt
	cat expected_version.txt >> testrec.md					
	${TR} cmt '```' 						
	${TR} cmt 

	${SMK} --version	> version1.txt
	${TR} assert true /usr/bin/sdiff "-sbB version1.txt expected_version.txt"

	${TR} end
	
unknow_makefile:
	${TR} start "Unknow Makefile" 
	${TR} cmt  								
	${TR} cmt "Test the error message if an unknow MakeFile is given" 	
	
	${TR} cmt 
	${TR} cmt '```' 						
	cat expected_unknow_makefile.txt >> testrec.md
	${TR} cmt '```' 						
	${TR} cmt 

	test ! `${SMK} My_Makefile > err_unknow_makefile.txt 2>&1`
	${TR} assert true /usr/bin/sdiff "-sbB err_unknow_makefile.txt expected_unknow_makefile.txt" 

	${TR} end

<<<<<<< HEAD
clean:
	- ${RM} -rf dir? dir?? err*.txt expected_err*.txt expected_help.txt expected_version.txt src.adb rules.txt version1.txt help?.txt template.ac 
	${TR} clean

=======
>>>>>>> Minor fixes
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
	sed "s/^ */  - \[/;s/ \[Successful\].*/\]\(\)/" Successful.lst	>> tests_status.md

	echo ""                                                 >> tests_status.md
	echo '## Failed'                                   		>> tests_status.md
	echo ""                                                 >> tests_status.md
	echo "   "`cat Failed.lst | wc -l`" tests failed"       >> tests_status.md
	echo ""                                                 >> tests_status.md
	sed "s/^** /  - \[/;s/ \[Failed\].*/\]\(\)/" Failed.lst >> tests_status.md

	echo ""                                                 >> tests_status.md
	echo '## Empty'                                    		>> tests_status.md
	echo ""                                                 >> tests_status.md
	echo "   "`cat Empty.lst | wc -l`" empty tests"         >> tests_status.md
	echo ""                                                 >> tests_status.md
	sed "s/^ */  - \[/;s/ \[Empty\].*/\]\(\)/" Empty.lst	>> tests_status.md

	mv tests_status.md ../docs/tests

	cp testrec.md ../docs/tests/
<<<<<<< HEAD
=======

clean:
	- ${RM} -rf dir? dir?? err*.txt expected_err*.txt expected_help.txt expected_version.txt version1.txt help?.txt tests_count.txt .smk_ 
	${TR} clean

>>>>>>> Minor fixes
