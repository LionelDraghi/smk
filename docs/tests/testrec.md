
# Sanity



 Makefile:  
```  
all: hello

hello.o: hello.c
	gcc -o hello.o -c hello.c

main.o: main.c hello.h
	gcc -o main.o -c main.c

hello: hello.o main.o
	gcc -o hello hello.o main.o

```  

##  Sanity / First `smk`, after `make`, should run no command


  Run:  
  `smk -q ../hello.c/Makefile.2`  
  `smk -e ../hello.c/Makefile.2`  

  Expected:  
```  
Nothing to run
```  


Sanity / First `smk`, after `make`, should run no command [Successful](tests_status.md#successful)

##  Sanity / Second `smk`, should not run any command


  Run:  
  `smk -e ../hello.c/Makefile.2`  

  Expected:  
```  
Nothing to run
```  


Sanity / Second `smk`, should not run any command [Successful](tests_status.md#successful)

##  Sanity / `smk reset`, no more history, should run all commands


  Run:  
  `smk reset --quiet`  
  `smk -e ../hello.c/Makefile.2`  

  Expected:  
```  
run "gcc -o hello.o -c hello.c" because it was not run before
gcc -o hello.o -c hello.c
run "gcc -o main.o -c main.c" because it was not run before
gcc -o main.o -c main.c
run "gcc -o hello hello.o main.o" because it was not run before
gcc -o hello hello.o main.o
```  


Sanity / `smk reset`, no more history, should run all commands [Successful](tests_status.md#successful)

##  Sanity / `smk -a`, should run all commands even if not needed


  Run:  
  `smk -e -a ../hello.c/Makefile.2`  

  Expected:  
```  
run "gcc -o hello.o -c hello.c" because -a option is set
gcc -o hello.o -c hello.c
run "gcc -o main.o -c main.c" because -a option is set
gcc -o main.o -c main.c
run "gcc -o hello hello.o main.o" because -a option is set
gcc -o hello hello.o main.o
```  


Sanity / `smk -a`, should run all commands even if not needed [Successful](tests_status.md#successful)

##  Sanity / `rm main.o` (missing file)


  Run:  
  `rm ../hello.c/main.o`  
  `smk -e ../hello.c/Makefile.2`  

  Expected:  
```  
run "gcc -o main.o -c main.c" because Target /home/lionel/Proj/smk/tests/hello.c/main.o is missing
gcc -o main.o -c main.c
run "gcc -o hello hello.o main.o" because /home/lionel/Proj/smk/tests/hello.c/main.o (-- ::.) has been updated since last run (-- ::.)
gcc -o hello hello.o main.o
```  


Sanity / `rm main.o` (missing file) [Successful](tests_status.md#successful)

##  Sanity / `touch hello.c` (updated file)


  Run:  
  `rm ../hello.c/main.o`  
  `smk -e ../hello.c/Makefile.2`  

  Expected:  
```  
run "gcc -o hello.o -c hello.c" because /home/lionel/Proj/smk/tests/hello.c/hello.c (-- ::.) has been updated since last run (-- ::.)
gcc -o hello.o -c hello.c
run "gcc -o hello hello.o main.o" because /home/lionel/Proj/smk/tests/hello.c/hello.o (-- ::.) has been updated since last run (-- ::.)
gcc -o hello hello.o main.o
```  


Sanity / `touch hello.c` (updated file) [Successful](tests_status.md#successful)

# Website building sanity tests



##  Website building sanity tests / cleaning and building using clean and doc sections


  Run:  
  `smk -q reset`  
  `smk -q ../mysite/Makefile:clean`  
  `smk ../mysite/Makefile:doc`  

  Expected:  
```  

mkdocs build --clean --quiet
```  


Website building sanity tests / cleaning and building using clean and doc sections [Successful](tests_status.md#successful)

##  Website building sanity tests / listing


  Sources are all md files in docs directory, and the mkdocs.yml file  
  Targets are all files in site directory (the directory is fully build by mkdocs)  

  Run:  
  `smk ls -l`   (long listing format)  

  Expected:  
```  
"mkdocs build --clean --quiet" [doc] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/about.md
"mkdocs build --clean --quiet" [doc] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/changelog.md
"mkdocs build --clean --quiet" [doc] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/cmd_line.md
"mkdocs build --clean --quiet" [doc] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/compare_with_make.md
"mkdocs build --clean --quiet" [doc] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/contributing.md
"mkdocs build --clean --quiet" [doc] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/dashboard.md
"mkdocs build --clean --quiet" [doc] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/design_notes.md
"mkdocs build --clean --quiet" [doc] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/fixme.md
"mkdocs build --clean --quiet" [doc] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/img/sloc.png
"mkdocs build --clean --quiet" [doc] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/img/tests.png
"mkdocs build --clean --quiet" [doc] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/img/tests_ko.svg
"mkdocs build --clean --quiet" [doc] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/img/tests_ok.svg
"mkdocs build --clean --quiet" [doc] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/img/version.svg
"mkdocs build --clean --quiet" [doc] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/index.md
"mkdocs build --clean --quiet" [doc] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/limitations.md
"mkdocs build --clean --quiet" [doc] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/smkfile_format.md
"mkdocs build --clean --quiet" [doc] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/tutorial.md
"mkdocs build --clean --quiet" [doc] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/mkdocs.yml
```  

  Run:  
  `smk ls`   (default short listing format)  

  Expected:  
```  
/home/lionel/Proj/smk/tests/mysite/docs/about.md
/home/lionel/Proj/smk/tests/mysite/docs/changelog.md
/home/lionel/Proj/smk/tests/mysite/docs/cmd_line.md
/home/lionel/Proj/smk/tests/mysite/docs/compare_with_make.md
/home/lionel/Proj/smk/tests/mysite/docs/contributing.md
/home/lionel/Proj/smk/tests/mysite/docs/dashboard.md
/home/lionel/Proj/smk/tests/mysite/docs/design_notes.md
/home/lionel/Proj/smk/tests/mysite/docs/fixme.md
/home/lionel/Proj/smk/tests/mysite/docs/img/sloc.png
/home/lionel/Proj/smk/tests/mysite/docs/img/tests.png
/home/lionel/Proj/smk/tests/mysite/docs/img/tests_ko.svg
/home/lionel/Proj/smk/tests/mysite/docs/img/tests_ok.svg
/home/lionel/Proj/smk/tests/mysite/docs/img/version.svg
/home/lionel/Proj/smk/tests/mysite/docs/index.md
/home/lionel/Proj/smk/tests/mysite/docs/limitations.md
/home/lionel/Proj/smk/tests/mysite/docs/smkfile_format.md
/home/lionel/Proj/smk/tests/mysite/docs/tutorial.md
/home/lionel/Proj/smk/tests/mysite/mkdocs.yml
```  

  Run:  
  `smk list-targets --long-listing`  

  Expected:  
```  
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/404.html
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/about/index.html
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/changelog/index.html
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/cmd_line/index.html
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/compare_with_make/index.html
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/contributing/index.html
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/css/base.css
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/css/bootstrap-custom.min.css
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/css/font-awesome.min.css
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/dashboard/index.html
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/design_notes/index.html
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/fixme/index.html
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/fonts/FontAwesome.otf
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/fonts/fontawesome-webfont.eot
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/fonts/fontawesome-webfont.svg
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/fonts/fontawesome-webfont.ttf
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/fonts/fontawesome-webfont.woff
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/fonts/fontawesome-webfont.woff2
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/fonts/glyphicons-halflings-regular.eot
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/fonts/glyphicons-halflings-regular.svg
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/fonts/glyphicons-halflings-regular.ttf
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/fonts/glyphicons-halflings-regular.woff
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/fonts/glyphicons-halflings-regular.woff2
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/img/favicon.ico
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/img/grid.png
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/img/sloc.png
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/img/tests.png
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/img/tests_ko.svg
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/img/tests_ok.svg
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/img/version.svg
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/index.html
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/js/base.js
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/js/bootstrap-3.0.3.min.js
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/js/jquery-1.10.2.min.js
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/limitations/index.html
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/search/lunr.js
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/search/main.js
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/search/search_index.json
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/search/worker.js
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/sitemap.xml
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/sitemap.xml.gz
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/smkfile_format/index.html
"mkdocs build --clean --quiet" [doc] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/tutorial/index.html
```  

  Run:  
  `smk rs`  

  Expected:  
```  
../mysite/Makefile (2018-12-22 00:17:20.00) :
21: [doc] mkdocs build --clean --quiet
24: [clean] echo --- clean:
25: [clean] rm -rf ./site/*
26: [clean] echo OK
```  


Website building sanity tests / listing [Successful](tests_status.md#successful)

##  Website building sanity tests / building without section


  Run:  
  `smk -q reset`  
     First run after reset : all command should be executed, including `clean`  
  `smk ../mysite/Makefile`  

  Expected:  
```  

mkdocs build --clean --quiet
echo --- clean:
--- clean:
rm -rf ./site/*
echo OK
OK
```  

  Run:  
     Second run : all command should be executed except the "no target" command `clean`  
  `smk --explain`  

  Expected:  
```  
run "mkdocs build --clean --quiet" because Target /home/lionel/Proj/smk/tests/mysite/site/404.html is missing
mkdocs build --clean --quiet
```  

  Run:  
     Third run : all targets are up-to-date  
  `smk --explain --verbose`  

  Expected:  
```  
No need to run mkdocs build --clean --quiet
No need to run echo --- clean:
No need to run rm -rf ./site/*
No need to run echo OK
Nothing to run
```  


**Website building sanity tests / building without section [Failed](tests_status.md#failed)**

##  Website building sanity tests / re-building


  Run:  
  `touch ../mysite/docs/about.md`  
  `smk whatsnew`  

  Expected:  
```  
[Updated] /home/lionel/Proj/smk/tests/mysite/docs/about.md
```  

  Run:  
  `rm ../mysite/site/tutorial/index.html`  
  `smk wn`  

  Expected:  
```  
[Updated] /home/lionel/Proj/smk/tests/mysite/docs/about.md
[Missing] /home/lionel/Proj/smk/tests/mysite/site/tutorial/index.html
```  

  Run:  
  `smk -e -v`  

  Expected:  
```  
run "mkdocs build --clean --quiet" because Target /home/lionel/Proj/smk/tests/mysite/site/tutorial/index.html is missing
mkdocs build --clean --quiet
No need to run echo --- clean:
No need to run rm -rf ./site/*
No need to run echo OK

```  


**Website building sanity tests / re-building [Failed](tests_status.md#failed)**

##  Website building sanity tests / cleaning


  Run:  
  `smk clean`  

  Expected:  
```  

Deleting /home/lionel/Proj/smk/tests/mysite/site/404.html
Deleting /home/lionel/Proj/smk/tests/mysite/site/about/index.html
Deleting /home/lionel/Proj/smk/tests/mysite/site/changelog/index.html
Deleting /home/lionel/Proj/smk/tests/mysite/site/cmd_line/index.html
Deleting /home/lionel/Proj/smk/tests/mysite/site/compare_with_make/index.html
Deleting /home/lionel/Proj/smk/tests/mysite/site/contributing/index.html
Deleting /home/lionel/Proj/smk/tests/mysite/site/css/base.css
Deleting /home/lionel/Proj/smk/tests/mysite/site/css/bootstrap-custom.min.css
Deleting /home/lionel/Proj/smk/tests/mysite/site/css/font-awesome.min.css
Deleting /home/lionel/Proj/smk/tests/mysite/site/dashboard/index.html
Deleting /home/lionel/Proj/smk/tests/mysite/site/design_notes/index.html
Deleting /home/lionel/Proj/smk/tests/mysite/site/fixme/index.html
Deleting /home/lionel/Proj/smk/tests/mysite/site/fonts/FontAwesome.otf
Deleting /home/lionel/Proj/smk/tests/mysite/site/fonts/fontawesome-webfont.eot
Deleting /home/lionel/Proj/smk/tests/mysite/site/fonts/fontawesome-webfont.svg
Deleting /home/lionel/Proj/smk/tests/mysite/site/fonts/fontawesome-webfont.ttf
Deleting /home/lionel/Proj/smk/tests/mysite/site/fonts/fontawesome-webfont.woff
Deleting /home/lionel/Proj/smk/tests/mysite/site/fonts/fontawesome-webfont.woff2
Deleting /home/lionel/Proj/smk/tests/mysite/site/fonts/glyphicons-halflings-regular.eot
Deleting /home/lionel/Proj/smk/tests/mysite/site/fonts/glyphicons-halflings-regular.svg
Deleting /home/lionel/Proj/smk/tests/mysite/site/fonts/glyphicons-halflings-regular.ttf
Deleting /home/lionel/Proj/smk/tests/mysite/site/fonts/glyphicons-halflings-regular.woff
Deleting /home/lionel/Proj/smk/tests/mysite/site/fonts/glyphicons-halflings-regular.woff2
Deleting /home/lionel/Proj/smk/tests/mysite/site/img/favicon.ico
Deleting /home/lionel/Proj/smk/tests/mysite/site/img/grid.png
Deleting /home/lionel/Proj/smk/tests/mysite/site/img/sloc.png
Deleting /home/lionel/Proj/smk/tests/mysite/site/img/tests.png
Deleting /home/lionel/Proj/smk/tests/mysite/site/img/tests_ko.svg
Deleting /home/lionel/Proj/smk/tests/mysite/site/img/tests_ok.svg
Deleting /home/lionel/Proj/smk/tests/mysite/site/img/version.svg
Deleting /home/lionel/Proj/smk/tests/mysite/site/index.html
Deleting /home/lionel/Proj/smk/tests/mysite/site/js/base.js
Deleting /home/lionel/Proj/smk/tests/mysite/site/js/bootstrap-3.0.3.min.js
Deleting /home/lionel/Proj/smk/tests/mysite/site/js/jquery-1.10.2.min.js
Deleting /home/lionel/Proj/smk/tests/mysite/site/limitations/index.html
Deleting /home/lionel/Proj/smk/tests/mysite/site/search/lunr.js
Deleting /home/lionel/Proj/smk/tests/mysite/site/search/main.js
Deleting /home/lionel/Proj/smk/tests/mysite/site/search/search_index.json
Deleting /home/lionel/Proj/smk/tests/mysite/site/search/worker.js
Deleting /home/lionel/Proj/smk/tests/mysite/site/sitemap.xml
Deleting /home/lionel/Proj/smk/tests/mysite/site/sitemap.xml.gz
Deleting /home/lionel/Proj/smk/tests/mysite/site/smkfile_format/index.html
Deleting /home/lionel/Proj/smk/tests/mysite/site/tutorial/index.html
```  


**Website building sanity tests / cleaning [Failed](tests_status.md#failed)**

##  Website building sanity tests / re-building after a clean


  Run:  
  `smk --explain`  

  Expected:  
```  
run "mkdocs build --clean --quiet" because Target /home/lionel/Proj/smk/tests/mysite/site/.html is missing
mkdocs build --clean --quiet
```  


**Website building sanity tests / re-building after a clean [Failed](tests_status.md#failed)**

# Read queries



##  Read queries / read-smkfile


  Read a smkfile and shows what is understud by smk  

  Makefile:  
```  
all: hello

hello.o: hello.c
	gcc -o hello.o -c hello.c

main.o: main.c hello.h
	gcc -o main.o -c main.c

hello: hello.o main.o
	gcc -o hello hello.o main.o

# let's add some section that should not be run with the rest
clean:
	rm -rf *.o

mrproper: clean
	rm -rf hello
```  

  Run:  
  `smk read-smkfile ../hello.c/Makefile.3`  

  Expected:  
```  
../hello.c/Makefile.3 (YYYY:MM:DD HH:MM:SS.SS) :
4: [hello.o] gcc -o hello.o -c hello.c
7: [main.o] gcc -o main.o -c main.c
10: [hello] gcc -o hello hello.o main.o
14: [clean] rm -rf *.o
17: [mrproper] rm -rf hello
```  


Read queries / read-smkfile [Successful](tests_status.md#successful)

##  Read queries / status


  Read the previous run dump and shows sources and targets  

  Run:  
  `smk -q reset`  
  `smk -q build ../hello.c/Makefile.2`  
  `smk status ../hello.c/Makefile.2`  

  Expected:  
  (note that to ease comparison, dates are removed)  
```  
Command "gcc -o hello hello.o main.o" in section [hello], last run [YYYY:MM:DD HH:MM:SS.SS]
Command "gcc -o hello.o -c hello.c" in section [hello.o], last run [YYYY:MM:DD HH:MM:SS.SS]
Command "gcc -o main.o -c main.c" in section [main.o], last run [YYYY:MM:DD HH:MM:SS.SS]
```  

  Run: (same with system files not ignored and long form)   
  `smk -q reset`  
  `smk -q build ../hello.c/Makefile.2`  
  `smk st -l -sa ../hello.c/Makefile.2`  

  Expected:  
  (note that to ease comparison, dates are removed)  
```  
Command "gcc -o hello hello.o main.o" in section [hello], last run [YYYY:MM:DD HH:MM:SS.SS]
Sources: (19)
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/hello.c/hello.o
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/hello.c/main.o
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/ld-2.28.so
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libc-2.28.so
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libdl-2.28.so
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libm-2.28.so
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/crtbegin.o
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/crtend.o
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/libgcc.a
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib64/libgcc_s.so
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib64/libgcc_s.so.1
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/libexec/gcc/x86_64-pc-linux-gnu/7.3.1/liblto_plugin.so.0.0.0
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/locale-archive
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/crt1.o
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/crti.o
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/crtn.o
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/gconv/gconv-modules.cache
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libc.so
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libc_nonshared.a
Targets: (1)
  - [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/hello.c/hello

Command "gcc -o hello.o -c hello.c" in section [hello.o], last run [YYYY:MM:DD HH:MM:SS.SS]
Sources: (52)
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/hello.c/hello.c
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libc-2.28.so
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libdl-2.28.so
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libm-2.28.so
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stdarg.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stddef.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/alloca.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/endian.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/features.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdc-predef.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdio.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdlib.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/byteswap.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/endian.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/floatn-common.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/floatn.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/long-double.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/select.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/FILE.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/time_t.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/typesizes.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/waitflags.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/waitstatus.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/wordsize.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/gnu/stubs.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/cdefs.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/select.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/types.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/locale-archive
Targets: (1)
  - [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/hello.c/hello.o

Command "gcc -o main.o -c main.c" in section [main.o], last run [YYYY:MM:DD HH:MM:SS.SS]
Sources: (53)
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/hello.c/hello.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/hello.c/main.c
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libc-2.28.so
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libdl-2.28.so
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libm-2.28.so
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stdarg.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stddef.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/alloca.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/endian.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/features.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdc-predef.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdio.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdlib.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/byteswap.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/endian.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/floatn-common.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/floatn.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/long-double.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/select.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/FILE.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/time_t.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/typesizes.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/waitflags.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/waitstatus.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/wordsize.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/gnu/stubs.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/cdefs.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/select.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/types.h
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/locale-archive
Targets: (1)
  - [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/hello.c/main.o

```  


  Run:  
  `smk -q reset`  
  `smk status ../hello.c/Makefile.2`  

  Expected:  
```  
Error : No previous run found.
```  


  Run:  
  `smk status`  

  Expected:  
```  
Error : No smkfile given, and no existing runfile in dir
```  


Read queries / status [Successful](tests_status.md#successful)

# List queries



##  List queries / lr | list-runs


  Test available previous runs  

  Run:  
  `smk -q reset`  
  `smk lr`  

  Expected:  
```  
No run file
```  

  Run:  
  `smk -q build ../hello.c/Makefile.2`  
  `smk -q ../hello.c/Makefile.3`  
  `smk list-runs`  

  Expected:  
```  
Makefile.2
Makefile.3
```  


**List queries / lr | list-runs [Failed](tests_status.md#failed)**

##  List queries / lt | list-targets

  Run:  
  `smk list-targets --long-listing ../hello.c/Makefile.2`  

  Expected: (long form)  
```  
"gcc -o hello hello.o main.o" [hello] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/hello.c/hello
"gcc -o hello.o -c hello.c" [hello.o] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/hello.c/hello.o
"gcc -o main.o -c main.c" [main.o] [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/hello.c/main.o
```  

  Run:  
  `smk lt ../hello.c/Makefile.2`  

  Expected: (short form)  
```  
/home/lionel/Proj/smk/tests/hello.c/hello
/home/lionel/Proj/smk/tests/hello.c/hello.o
/home/lionel/Proj/smk/tests/hello.c/main.o
```  


List queries / lt | list-targets [Successful](tests_status.md#successful)

##  List queries / ls | list-sources


  Run: (long form)  
  `smk ls -l ../hello.c/Makefile.2`  

  Expected:  
```  
/home/lionel/Proj/smk/tests/hello.c/hello.o
/home/lionel/Proj/smk/tests/hello.c/main.o
/home/lionel/Proj/smk/tests/hello.c/hello.c
/home/lionel/Proj/smk/tests/hello.c/hello.h
/home/lionel/Proj/smk/tests/hello.c/main.c
```  


  Run:  
  `smk list-sources ../hello.c/Makefile.2`  

  Expected:  
```  
/home/lionel/Proj/smk/tests/hello.c/hello.o
/home/lionel/Proj/smk/tests/hello.c/main.o
/home/lionel/Proj/smk/tests/hello.c/hello.c
/home/lionel/Proj/smk/tests/hello.c/hello.h
/home/lionel/Proj/smk/tests/hello.c/main.c
```  


List queries / ls | list-sources [Successful](tests_status.md#successful)

##  List queries / ls | list-sources --show-all-files


  Run: (short form)  
  `smk list-sources --shows-system-files ../hello.c/Makefile.2`  

  Expected:  
```  
/home/lionel/Proj/smk/tests/hello.c/hello.o
/home/lionel/Proj/smk/tests/hello.c/main.o
/lib/x86_64-linux-gnu/ld-2.28.so
/lib/x86_64-linux-gnu/libc-2.28.so
/lib/x86_64-linux-gnu/libdl-2.28.so
/lib/x86_64-linux-gnu/libm-2.28.so
/opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/crtbegin.o
/opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/crtend.o
/opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/libgcc.a
/opt/GNAT/2018/lib64/libgcc_s.so
/opt/GNAT/2018/lib64/libgcc_s.so.1
/opt/GNAT/2018/libexec/gcc/x86_64-pc-linux-gnu/7.3.1/liblto_plugin.so.0.0.0
/usr/lib/locale/locale-archive
/usr/lib/x86_64-linux-gnu/crt1.o
/usr/lib/x86_64-linux-gnu/crti.o
/usr/lib/x86_64-linux-gnu/crtn.o
/usr/lib/x86_64-linux-gnu/gconv/gconv-modules.cache
/usr/lib/x86_64-linux-gnu/libc.so
/usr/lib/x86_64-linux-gnu/libc_nonshared.a
/home/lionel/Proj/smk/tests/hello.c/hello.c
/lib/x86_64-linux-gnu/libc-2.28.so
/lib/x86_64-linux-gnu/libdl-2.28.so
/lib/x86_64-linux-gnu/libm-2.28.so
/opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stdarg.h
/opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stddef.h
/usr/include/alloca.h
/usr/include/endian.h
/usr/include/features.h
/usr/include/stdc-predef.h
/usr/include/stdio.h
/usr/include/stdlib.h
/usr/include/x86_64-linux-gnu/bits/byteswap.h
/usr/include/x86_64-linux-gnu/bits/endian.h
/usr/include/x86_64-linux-gnu/bits/floatn-common.h
/usr/include/x86_64-linux-gnu/bits/floatn.h
/usr/include/x86_64-linux-gnu/bits/libc-header-start.h
/usr/include/x86_64-linux-gnu/bits/long-double.h
/usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
/usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
/usr/include/x86_64-linux-gnu/bits/select.h
/usr/include/x86_64-linux-gnu/bits/stdint-intn.h
/usr/include/x86_64-linux-gnu/bits/stdio_lim.h
/usr/include/x86_64-linux-gnu/bits/stdlib-float.h
/usr/include/x86_64-linux-gnu/bits/sys_errlist.h
/usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
/usr/include/x86_64-linux-gnu/bits/types.h
/usr/include/x86_64-linux-gnu/bits/types/FILE.h
/usr/include/x86_64-linux-gnu/bits/types/__FILE.h
/usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
/usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
/usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
/usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
/usr/include/x86_64-linux-gnu/bits/types/clock_t.h
/usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
/usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
/usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
/usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
/usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
/usr/include/x86_64-linux-gnu/bits/types/time_t.h
/usr/include/x86_64-linux-gnu/bits/types/timer_t.h
/usr/include/x86_64-linux-gnu/bits/typesizes.h
/usr/include/x86_64-linux-gnu/bits/uintn-identity.h
/usr/include/x86_64-linux-gnu/bits/waitflags.h
/usr/include/x86_64-linux-gnu/bits/waitstatus.h
/usr/include/x86_64-linux-gnu/bits/wordsize.h
/usr/include/x86_64-linux-gnu/gnu/stubs-64.h
/usr/include/x86_64-linux-gnu/gnu/stubs.h
/usr/include/x86_64-linux-gnu/sys/cdefs.h
/usr/include/x86_64-linux-gnu/sys/select.h
/usr/include/x86_64-linux-gnu/sys/types.h
/usr/lib/locale/locale-archive
/home/lionel/Proj/smk/tests/hello.c/hello.h
/home/lionel/Proj/smk/tests/hello.c/main.c
/lib/x86_64-linux-gnu/libc-2.28.so
/lib/x86_64-linux-gnu/libdl-2.28.so
/lib/x86_64-linux-gnu/libm-2.28.so
/opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stdarg.h
/opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stddef.h
/usr/include/alloca.h
/usr/include/endian.h
/usr/include/features.h
/usr/include/stdc-predef.h
/usr/include/stdio.h
/usr/include/stdlib.h
/usr/include/x86_64-linux-gnu/bits/byteswap.h
/usr/include/x86_64-linux-gnu/bits/endian.h
/usr/include/x86_64-linux-gnu/bits/floatn-common.h
/usr/include/x86_64-linux-gnu/bits/floatn.h
/usr/include/x86_64-linux-gnu/bits/libc-header-start.h
/usr/include/x86_64-linux-gnu/bits/long-double.h
/usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
/usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
/usr/include/x86_64-linux-gnu/bits/select.h
/usr/include/x86_64-linux-gnu/bits/stdint-intn.h
/usr/include/x86_64-linux-gnu/bits/stdio_lim.h
/usr/include/x86_64-linux-gnu/bits/stdlib-float.h
/usr/include/x86_64-linux-gnu/bits/sys_errlist.h
/usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
/usr/include/x86_64-linux-gnu/bits/types.h
/usr/include/x86_64-linux-gnu/bits/types/FILE.h
/usr/include/x86_64-linux-gnu/bits/types/__FILE.h
/usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
/usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
/usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
/usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
/usr/include/x86_64-linux-gnu/bits/types/clock_t.h
/usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
/usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
/usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
/usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
/usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
/usr/include/x86_64-linux-gnu/bits/types/time_t.h
/usr/include/x86_64-linux-gnu/bits/types/timer_t.h
/usr/include/x86_64-linux-gnu/bits/typesizes.h
/usr/include/x86_64-linux-gnu/bits/uintn-identity.h
/usr/include/x86_64-linux-gnu/bits/waitflags.h
/usr/include/x86_64-linux-gnu/bits/waitstatus.h
/usr/include/x86_64-linux-gnu/bits/wordsize.h
/usr/include/x86_64-linux-gnu/gnu/stubs-64.h
/usr/include/x86_64-linux-gnu/gnu/stubs.h
/usr/include/x86_64-linux-gnu/sys/cdefs.h
/usr/include/x86_64-linux-gnu/sys/select.h
/usr/include/x86_64-linux-gnu/sys/types.h
/usr/lib/locale/locale-archive
```  


  Run: (long form)  
  `smk -l ls -sa ../hello.c/Makefile.2`  

  Expected:  
```  
"gcc -o hello hello.o main.o" [hello] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/hello.c/hello.o
"gcc -o hello hello.o main.o" [hello] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/hello.c/main.o
"gcc -o hello hello.o main.o" [hello] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/ld-2.28.so
"gcc -o hello hello.o main.o" [hello] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libc-2.28.so
"gcc -o hello hello.o main.o" [hello] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libdl-2.28.so
"gcc -o hello hello.o main.o" [hello] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libm-2.28.so
"gcc -o hello hello.o main.o" [hello] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/crtbegin.o
"gcc -o hello hello.o main.o" [hello] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/crtend.o
"gcc -o hello hello.o main.o" [hello] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/libgcc.a
"gcc -o hello hello.o main.o" [hello] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib64/libgcc_s.so
"gcc -o hello hello.o main.o" [hello] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib64/libgcc_s.so.1
"gcc -o hello hello.o main.o" [hello] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/libexec/gcc/x86_64-pc-linux-gnu/7.3.1/liblto_plugin.so.0.0.0
"gcc -o hello hello.o main.o" [hello] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/locale-archive
"gcc -o hello hello.o main.o" [hello] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/crt1.o
"gcc -o hello hello.o main.o" [hello] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/crti.o
"gcc -o hello hello.o main.o" [hello] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/crtn.o
"gcc -o hello hello.o main.o" [hello] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/gconv/gconv-modules.cache
"gcc -o hello hello.o main.o" [hello] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libc.so
"gcc -o hello hello.o main.o" [hello] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libc_nonshared.a
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/hello.c/hello.c
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libc-2.28.so
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libdl-2.28.so
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libm-2.28.so
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stdarg.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stddef.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/alloca.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/endian.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/features.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdc-predef.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdio.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdlib.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/byteswap.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/endian.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/floatn-common.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/floatn.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/long-double.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/select.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/FILE.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/time_t.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/typesizes.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/waitflags.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/waitstatus.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/wordsize.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/gnu/stubs.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/cdefs.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/select.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/types.h
"gcc -o hello.o -c hello.c" [hello.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/locale-archive
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/hello.c/hello.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/hello.c/main.c
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libc-2.28.so
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libdl-2.28.so
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libm-2.28.so
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stdarg.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stddef.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/alloca.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/endian.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/features.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdc-predef.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdio.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdlib.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/byteswap.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/endian.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/floatn-common.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/floatn.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/long-double.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/select.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/FILE.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/time_t.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/typesizes.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/waitflags.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/waitstatus.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/wordsize.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/gnu/stubs.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/cdefs.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/select.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/types.h
"gcc -o main.o -c main.c" [main.o] [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/locale-archive
```  


List queries / ls | list-sources --show-all-files [Successful](tests_status.md#successful)

# Targets related functions



##  Targets related functions / dry-run clean


  Test targets cleaning (dry run and real)  

  Run:  
  `smk reset`  
  `smk -q build ../hello.c/Makefile.2`  
  `smk clean --dry-run`  
  `smk --explain`  (to check that nothing was actually deleted)  

  Expected:  
```  
Deleting /home/lionel/Proj/smk/tests/hello.c/hello
Deleting /home/lionel/Proj/smk/tests/hello.c/hello.o
Deleting /home/lionel/Proj/smk/tests/hello.c/main.o
```  


##  Targets related functions / real clean

  Run:  
  `smk clean`  

  Expected:  
```  
Deleting /home/lionel/Proj/smk/tests/hello.c/hello
Deleting /home/lionel/Proj/smk/tests/hello.c/hello.o
Deleting /home/lionel/Proj/smk/tests/hello.c/main.o
```  

  Run:  
  `smk -e`  (to check effective cleaning)  

  Expected:  
```  
run "gcc -o hello.o -c hello.c" because Target /home/lionel/Proj/smk/tests/hello.c/hello.o is missing
gcc -o hello.o -c hello.c
run "gcc -o main.o -c main.c" because Target /home/lionel/Proj/smk/tests/hello.c/main.o is missing
gcc -o main.o -c main.c
run "gcc -o hello hello.o main.o" because Target /home/lionel/Proj/smk/tests/hello.c/hello is missing
gcc -o hello hello.o main.o
```  


Targets related functions / real clean [Successful](tests_status.md#successful)

##  Targets related functions / Build selected target


  Run:  
  `smk -q reset`  
  `smk ../hello.c/Makefile.1`  
  `smk build main.o`  
  Note that to avoid any confusion, this smkfile do not contain any target named main.o  

  Expected:  
```  
Nothing to run
```  

  Run:  
  `touch ../hello.c/main.c`  
  `smk build main.o`  

  Expected:  
```  
gcc -o main.o -c main.c
gcc -o hello hello.o main.o
```  


Targets related functions / Build selected target [Successful](tests_status.md#successful)

# Implicit naming



##  Implicit naming / Implicit naming


  Test that when there is only one run file in the directory, smk assume it without giving it on the command line.  


  Run:  
  `smk -q reset`  
  `smk`  

  Expected: help message as there is nothing in the directory  

```  
Error : No smkfile given, and no existing runfile in dir
```  

  Run:  
  `smk -q ../hello.c/Makefile.2`  
  `touch ../hello.c/hello.c`  
  `smk`  

  Expected: smk re-run Makefile.2, as it is the only one in the dir  
```  
gcc -o hello.o -c hello.c
gcc -o hello hello.o main.o
```  

  Run:  
  `smk -q ../hello.c/Makefile.3`  
  `smk `  

  Expected:  
     There is more than one possible run, smk display the list but don't do anything else   
```  
Error : No smkfile given, and more than one runfile in dir
```  


**Implicit naming / Implicit naming [Failed](tests_status.md#failed)**

# Run errors


 test -k and -i behavior  

##  Run errors / no option


  Run:  
  `smk -q reset`  
  `smk ../hello.c/Wrong_Makefile`  

  Expected:  
```  
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c --WTF
gcc: error: unrecognized command line option '--WTF'
Error : Spawn failed for gcc -o main.o -c main.c --WTF
```  


##  Run errors / -k


  Run:  
  `smk -q reset`  
  `smk -k ../hello.c/Wrong_Makefile`  

  Expected:  
```  
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c --WTF
gcc: error: unrecognized command line option '--WTF'
Error : Spawn failed for gcc -o main.o -c main.c --WTF
gcc -o hello hello.o main.o
gcc: error: main.o: No such file or directory
Error : Spawn failed for gcc -o hello hello.o main.o

```  


##  Run errors / -i


  Run:  
  `smk -q reset`  
  `smk -i ../hello.c/Wrong_Makefile`  

  Expected:  
     Same as with -k, but without returning an error code  
```  
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c --WTF
gcc: error: unrecognized command line option '--WTF'
Error : Spawn failed for gcc -o main.o -c main.c --WTF

```  


##  Run errors / -k -i


  Run: with both!  
  `smk -q reset`  
  `smk --keep-going --ignore-errors ../hello.c/Wrong_Makefile`  

  Expected:  
     Same as with -k, but without returning an error code  
```  
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c --WTF
gcc: error: unrecognized command line option '--WTF'
Error : Spawn failed for gcc -o main.o -c main.c --WTF
gcc -o hello hello.o main.o
gcc: error: main.o: No such file or directory
Error : Spawn failed for gcc -o hello hello.o main.o

```  


**Run errors / -k -i [Failed](tests_status.md#failed)**

##  Run errors / `run` command fails


  Run:  
  `smk -q reset`  
  `rm *.smk`  
  `smk run non_existing_command`  

  Expected:  

```  
non_existing_command
/usr/bin/strace: Can't stat 'non_existing_command': No such file or directory
Error : Spawn failed for non_existing_command
```  


  default.smk should nevertheless contains the failed command  

```  
non_existing_command
```  

  Run:  
  `smk read-smkfile`  

  Expected:  

```  
default.smk (YYYY:MM:DD HH:MM:SS.SS) :
1: [] non_existing_command
```  

  Run:  
  `smk status`  

  Expected:  

```  
No recorded run
```  

  Other commands should return nothing:  
  `smk whatsnew`  
  `smk list-sources`  
  `smk list-targets`  


Run errors / `run` command fails [Successful](tests_status.md#successful)

# Command line



##  Command line / Help options


  Test the -h and help output :  

  Run:  
  `smk `  
  `smk help`  

  Expected:  

```  

Usage : smk [Options]* Command [Smkfile][:target]

Use example :
   when run the first time   : smk MyBuild.txt
   and then,to rebuild, just : smk
   to run a specific target  : smk MyBuild.txt:target
   or just                   : smk :target

Commands :
   build             : run the build
   status       | st : shows what smk knows about the previous
                       runs (commands, sources and targets)
   read-smkfile | rs : shows Smk understanding of a Smkfile
   whatsnew     | wn : list changes since last run
   add               : add the rest of the command line to
                       default.smk
   run               : equivalent to `add` followed by `build`
   clean             : remove all targets files
   reset             : remove all local Smk files
                       (equivalent to rm .smk.*)
   version           : put Smk version
   help              : this message
   dump              : list all smk known info on files,
                       including unused and dir
   list-runs    | lr : list runfiles in current directory
   list-sources | ls : list sources, except system files
   list-targets | lt : list targets, except system files
   list-unused  | lu : list files not involved in build

   NB : when no command is given, build is assumed

Options :
   -a   | --always-make    : unconditionally make all targets
   -e   | --explain        : explain why each target is made
   -n   | --dry-run        : print the commands that would be
                             executed, but do not execute them
   -sa  | --show-all-files : prevent -ls and -rl from
                             ignoring system files
   -i   | --ignore-errors  : ignore all errors in commands
                             executed to remake files
   -l   | --long-listing   : use a long listing format when
                             listing files
   -k   | --keep-going     : Do as much work as possible
   -We  | --Warnings=error : treat warnings as errors
   -v   | --verbose
   -q   | --quiet          : no message unless error,
                             Warning are also ignored
   -h   | --help           : this message

http://lionel.draghi.free.fr/smk/

```  


Command line / Help options [Successful](tests_status.md#successful)

##  Command line / Version option


  Test that the version command will put :  

  Run:  
  `smk version`  

  Expected:  

```  
0.3.0
```  


Command line / Version option [Successful](tests_status.md#successful)

##  Command line / Illegal cmd lines


  Run:  
  `smk read-smkfile status`  

  Expected:  

```  
Error : More than one command on command line : STATUS and READ_SMKFILE

Usage : smk Command [Options]* [Smkfile][:target]

Use example :
   when run the first time   : smk MyBuild.txt
   and then,to rebuild, just : smk
   to run a specific target  : smk MyBuild.txt:target
   or just                   : smk :target

Commands :
   build             : run the build
   status       | st : shows what smk knows about the previous
                       runs (commands, sources and targets)
   read-smkfile | rs : shows Smk understanding of a Smkfile
   whatsnew     | wn : list changes since last run
   add               : add the rest of the command line to
                       default.smk
   run               : equivalent to `add` followed by `build`
   clean             : remove all targets files
   reset             : remove all local Smk files
                       (equivalent to rm .smk.*)
   version           : put Smk version
   help              : this message
   dump              : list all smk known info on files,
                       including unused and dir
   list-runs    | lr : list runfiles in current directory
   list-sources | ls : list sources, except system files
   list-targets | lt : list targets, except system files
   list-unused  | lu : list files not involved in build

   NB : when no command is given, build is assumed

Options :
   -a   | --always-make    : unconditionally make all targets
   -e   | --explain        : explain why each target is made
   -n   | --dry-run        : print the commands that would be
                             executed, but do not execute them
   -sa  | --show-all-files : prevent -ls and -rl from
                             ignoring system files
   -i   | --ignore-errors  : ignore all errors in commands
                             executed to remake files
   -l   | --long-listing   : use a long listing format when
                             listing files
   -k   | --keep-going     : Do as much work as possible
   -We  | --Warnings=error : treat warnings as errors
   -v   | --verbose
   -q   | --quiet          : no message unless error,
                             Warning are also ignored
   -h   | --help           : this message


http://lionel.draghi.free.fr/smk/

```  


**Command line / Illegal cmd lines [Failed](tests_status.md#failed)**

##  Command line / Unknow Makefile


  Test the error message if an unknow MakeFile is given  

  Run:  
  `smk My_Makefile`  

  Expected:  

```  
Error : No smkfile given, and no existing runfile in dir
```  


Command line / Unknow Makefile [Successful](tests_status.md#successful)

# Multiline Commands



##  Multiline Commands / multiline single command


  cat `multiline_smkfile1.txt`:  
```  
	ploticus -prefab pie 	\
		data=out.sloccount labels=2 colors="blue red green orange"	\
# comment in the middle should not get in the way
		explode=0.1 values=1 title="Ada sloc `date +%x`"	\
		 -png -o out.sloc.png 
```  

  Run:  
  `smk -q reset`  
  `smk multiline_smkfile1.txt`  

  Expected:  
```  
ploticus -prefab pie data=out.sloccount labels=2 colors="blue red green orange" explode=0.1 values=1 title="Ada sloc `date +%x`" -png -o out.sloc.png
```  


Multiline Commands / multiline single command [Successful](tests_status.md#successful)

##  Multiline Commands / multiline with more commands and pipes


  cat `multiline_smkfile2.txt`:  
```  
// multiline with command and pipes
sloccount ../hello.c/* | 	\
grep "ansic=" 			\
|sed "s/ansic/C/"
		-- comment at the end
```  

  Run:  
  `smk -q reset`  
  `smk multiline_smkfile2.txt`  

  Expected:  
```  

sloccount ../hello.c/* | grep "ansic=" |sed "s/ansic/C/"
18      top_dir         C=18
```  


Multiline Commands / multiline with more commands and pipes [Successful](tests_status.md#successful)

##  Multiline Commands / Hill formatted multiline


  cat `hill_multiline_smkfile.txt`:  
```  
# Hill formatted multiline command:

	ploticus -prefab pie 	\
		data=out.sloccount labels=2 colors="blue red green orange"	\
		explode=0.1 values=1 title="Ada sloc `date +%x`"	\
// the end of the command is missing 

-- Note that the comment immediatly following the command 
-- should not be considered as the end of the command, neither 
-- should the following blank line or any of the following lines.
```  

  Run:  
  `smk -q reset`  
  `smk hill_multiline_smkfile.txt`  

  Expected:  
```  
Error : hill_multiline_smkfile.txt ends with incomplete multine, last command ignored
Nothing to run
```  


Multiline Commands / Hill formatted multiline [Successful](tests_status.md#successful)

# Sections related functions



##  Sections related functions / specific section building

  Run:  
  `smk :main.o` with main.o up to date`  

  Expected:  
```  
Nothing to run
```  

  Run:  
  `touch main.c`  
  `smk :main.o`  

  Expected:  
```  
gcc -o main.o -c main.c
```  


Sections related functions / specific section building [Successful](tests_status.md#successful)

##  Sections related functions / unknow section

  Run:  
  `smk :qzdsqdq.o`  

  Expected:  
```  
No section "qzdsqdq.o" in ../hello.c/Makefile.2
```  


Sections related functions / unknow section [Successful](tests_status.md#successful)

##  Sections related functions / smkmfile:section notation

  Run:  
  `smk build -q ../hello.c/Makefile.4`  
  `touch ../hello.c/hello.c`  
  `smk ../hello.c/Makefile.4:hello.o`  

  Expected:  
```  
gcc -o hello.o -c hello.c
```  

  Run:  
  `smk -a ../hello.c/Makefile.4:mrproper`  

  Expected:  
```  
rm -rf hello
```  


**Sections related functions / smkmfile:section notation [Failed](tests_status.md#failed)**

# Command Run features



##  Command Run features / Add and build


  Run:  
  `smk -q clean`  
  `smk -q reset`  
  `rm default.smk`  
  `smk add gcc -c ../hello.c/main.c`  
  `smk add gcc -c ../hello.c/hello.c`  
  `smk add gcc -o hello hello.o main.o`  
  `smk build`  

  Expected:  
```  
gcc -c ../hello.c/main.c
gcc -c ../hello.c/hello.c
gcc -o hello hello.o main.o
```  


Command Run features / Add and build [Successful](tests_status.md#successful)

##  Command Run features / Run


  Run:  
  `smk -q clean`  
  `smk -q reset`  
  `rm default.smk`  
  `smk run gcc -c ../hello.c/main.c`  
  `smk run gcc -c ../hello.c/hello.c`  
  `smk run gcc -o hello hello.o main.o`  

  Expected:  
```  

gcc -c ../hello.c/main.c
gcc -c ../hello.c/hello.c
gcc -o hello hello.o main.o
```  


Command Run features / Run [Successful](tests_status.md#successful)

# Directory update tests



##  Directory update tests / start conversion


  Run:  
  `smk -q reset`  
  `sox x.ogg x.mp3`  
  `id3v2 -a Luke -t Sentinelle x.mp3`  
  `id3ren -template=%a  

  Expected:  
```  
sox x.ogg x.mp3
id3v2 -a Luke -t Sentinelle x.mp3
id3ren -quiet -template=%a-%s.mp3 x.mp3
```  

  Run:  
  `smk st -l`  

  Expected:  
```  
Command "id3ren -quiet -template=%a-%s.mp3 x.mp3", last run [YYYY:MM:DD HH:MM:SS.SS]
Sources: (1)
  - [Both  ] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/x.mp3
Targets: (1)
  - [Both  ] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/Luke-Sentinelle.mp3

Command "id3v2 -a Luke -t Sentinelle x.mp3", last run [YYYY:MM:DD HH:MM:SS.SS]
Sources: (1)
  - [Both  ] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/x.mp3
Targets: (1)
  - [Both  ] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/x.mp3

Command "sox x.ogg x.mp3", last run [YYYY:MM:DD HH:MM:SS.SS]
Sources: (1)
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/x.ogg
Targets: (1)
  - [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/x.mp3

```  

  Run: (same with system files)  
  `smk st -l -sa`  

  Expected:  
```  
Command "id3ren -quiet -template=%a-%s.mp3 x.mp3", last run [YYYY:MM:DD HH:MM:SS.SS]
Sources: (1)
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libc-2.28.so
Targets: (0)

Command "id3v2 -a Luke -t Sentinelle x.mp3", last run [YYYY:MM:DD HH:MM:SS.SS]
Sources: (10)
  - [Both  ] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/x.mp3
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libc-2.28.so
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libgcc_s.so.1
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libm-2.28.so
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libz.so.1.2.11
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/locale-archive
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/gconv/ISO8859-1.so
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/gconv/gconv-modules.cache
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libid3-3.8.so.3.0.0
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libstdc++.so.6.0.25
Targets: (1)
  - [Both  ] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/x.mp3

Command "sox x.ogg x.mp3", last run [YYYY:MM:DD HH:MM:SS.SS]
Sources: (46)
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/x.ogg
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libc-2.28.so
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libdl-2.28.so
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libm-2.28.so
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libpthread-2.28.so
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/librt-2.28.so
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libz.so.1.2.11
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libFLAC.so.8.3.0
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libasound.so.2.0.0
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libgomp.so.1.0.0
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libgsm.so.1.0.12
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libid3tag.so.0.3.0
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libltdl.so.7.3.1
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libmad.so.0.2.1
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libmagic.so.1.0.0
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libmp3lame.so.0.0.0
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libogg.so.0.8.2
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libopencore-amrnb.so.0.0.3
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libopencore-amrwb.so.0.0.3
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libpng16.so.16.34.0
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libsndfile.so.1.0.28
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libsox.so.3.0.0
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libtwolame.so.0.0.0
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libvorbis.so.0.4.8
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libvorbisenc.so.2.0.11
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libvorbisfile.so.3.3.7
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libwavpack.so.1.2.0
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/sox/libsox_fmt_alsa.so
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/sox/libsox_fmt_amr_nb.so
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/sox/libsox_fmt_amr_wb.so
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/sox/libsox_fmt_caf.so
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/sox/libsox_fmt_fap.so
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/sox/libsox_fmt_flac.so
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/sox/libsox_fmt_gsm.so
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/sox/libsox_fmt_lpc10.so
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/sox/libsox_fmt_mat4.so
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/sox/libsox_fmt_mat5.so
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/sox/libsox_fmt_mp3.so
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/sox/libsox_fmt_paf.so
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/sox/libsox_fmt_pvf.so
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/sox/libsox_fmt_sd2.so
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/sox/libsox_fmt_sndfile.so
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/sox/libsox_fmt_vorbis.so
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/sox/libsox_fmt_w64.so
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/sox/libsox_fmt_wavpack.so
  - [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/sox/libsox_fmt_xi.so
Targets: (1)
  - [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/x.mp3

```  


**Directory update tests / start conversion [Failed](tests_status.md#failed)**

##  Directory update tests / start conversion


  Run:  
  `rm default.smk`  
  `smk -q reset`  


  Expected:  
```  
./ogg-to-mp3.sh
sox ./y.ogg ./y.mp3
sox ./x.ogg ./x.mp3
```  

  Run:  
  `smk st -l`  

  Expected:  
```  
Command "./ogg-to-mp3.sh", last run [YYYY:MM:DD HH:MM:SS.SS]
Sources: (4)
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/ogg-to-mp3.sh
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/to-mp3.sh
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/x.ogg
  - [Source] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/y.ogg
Targets: (2)
  - [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/x.mp3
  - [Target] [Created] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/y.mp3

```  


Directory update tests / start conversion [Successful](tests_status.md#successful)

##  Directory update tests / new ogg in dir


  Run:  
  `cp x.ogg z.ogg`  


  Expected:  
```  
[Updated] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests
[Missing] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/x.mp3
[Updated] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/y.ogg
[Created] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/z.ogg
```  


Directory update tests / new ogg in dir [Successful](tests_status.md#successful)

##  Directory update tests / ogg-to-mp3 is modified


  Run:  

  `touch ogg-to-mp3`  


  Expected:  
```  

run "./ogg-to-mp3.sh" because /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/ogg-to-mp3.sh (YYYY:MM:DD HH:MM:SS.SS) has been updated since last run (YYYY:MM:DD HH:MM:SS.SS)
./ogg-to-mp3.sh
```  


Directory update tests / ogg-to-mp3 is modified [Successful](tests_status.md#successful)

##  Directory update tests / adding a .ogg file in a subdir


  Run:  



  Expected:  
```  
[Updated] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests
```  

  Run:  


  Expected:  
```  
[Created] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/t.ogg
```  

  Run:  



  Expected:  
```  
[Created] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/t.ogg
```  

  Run:  


  Expected:  
```  
```  


**Directory update tests / adding a .ogg file in a subdir [Failed](tests_status.md#failed)**

##  Directory update tests / smk clean


  Run:  


  Expected:  
```  
Deleting /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/x.mp3
Deleting /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/y.mp3
Deleting /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/z.mp3
```  


**Directory update tests / smk clean [Failed](tests_status.md#failed)**
# File_Utilities unit tests

## Short_Path


1. Subdir with default Prefix
Expected :
"mysite/site/d1/idx.txt"
OK

2. Dir with final /
Expected :
"mysite/site/d1/idx.txt"
OK

3. subdir with Prefix
Expected :
"./mysite/site/d1/idx.txt"
OK

4. Sibling subdir
Expected :
"../../mysite/site/d1/idx.txt"
OK

5. Parent dir
Expected :
"../../idx.txt"
OK

6. Other Prefix
Expected :
"$PWD/../mysite/site/d1/idx.txt"
OK

7. Root dir
Expected :
"/home/tests/mysite/site/d1/idx.txt"
OK

8. File is over dir
Expected :
"../../../../readme.txt"
OK

9. File is over Dir, Dir with final /
Expected :
"../../../../readme.txt"
OK

--------------------------------------------------------------------------------
All tests OK.
# File_Utilities unit tests

## Short_Path


1. Subdir with default Prefix
Expected :
"mysite/site/d1/idx.txt"
OK

2. Dir with final /
Expected :
"mysite/site/d1/idx.txt"
OK

3. subdir with Prefix
Expected :
"./mysite/site/d1/idx.txt"
OK

4. Sibling subdir
Expected :
"../../mysite/site/d1/idx.txt"
OK

5. Parent dir
Expected :
"../../idx.txt"
OK

6. Other Prefix
Expected :
"$PWD/../mysite/site/d1/idx.txt"
OK

7. Root dir
Expected :
"/home/tests/mysite/site/d1/idx.txt"
OK

8. File is over dir
Expected :
"../../../../readme.txt"
OK

9. File is over Dir, Dir with final /
Expected :
"../../../../readme.txt"
OK

--------------------------------------------------------------------------------
All tests OK.
