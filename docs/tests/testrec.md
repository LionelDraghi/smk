
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
  `smk -e --missing-targets ../hello.c/Makefile.2`  

  Expected:  
```  
run "gcc -o main.o -c main.c" because Target file ../hello.c/main.o is missing
gcc -o main.o -c main.c
run "gcc -o hello hello.o main.o" because Source file ../hello.c/main.o has been updated (-- ::.)
gcc -o hello hello.o main.o
```  


Sanity / `rm main.o` (missing file) [Successful](tests_status.md#successful)

##  Sanity / `touch hello.c` (updated file)


  Run:  
  `touch ../hello.c/hello.c`  
  `smk -e ../hello.c/Makefile.2`  

  Expected:  
```  
run "gcc -o hello.o -c hello.c" because Source file ../hello.c/hello.c has been updated (-- ::.)
gcc -o hello.o -c hello.c
run "gcc -o hello hello.o main.o" because Source file ../hello.c/hello.o has been updated (-- ::.)
gcc -o hello hello.o main.o
```  


Sanity / `touch hello.c` (updated file) [Successful](tests_status.md#successful)

##  Sanity / `touch hello.c` and dry run


  Run:  
  `touch ../hello.c/hello.c`  
  `smk whatsnew ../hello.c/Makefile.2`  

  Expected:  
```  
[Updated] [Source] ../hello.c/hello.c
```  


  Run:  
  `smk -e -n`  

  Expected:  
```  
run "gcc -o hello.o -c hello.c" because Source file ../hello.c/hello.c has been updated (-- ::.)
> gcc -o hello.o -c hello.c
```  


  whatsnew should still return the same, as nothing was changed by previous run with -n) :  
  `smk wn`  

  Expected:  
```  
[Updated] [Source] ../hello.c/hello.c
```  


  Run same without -n :  
  `smk -e ../hello.c/Makefile.2`  

  Expected:  
```  
run "gcc -o hello.o -c hello.c" because Source file ../hello.c/hello.c has been updated (-- ::.)
gcc -o hello.o -c hello.c
run "gcc -o hello hello.o main.o" because Source file ../hello.c/hello.o has been updated (-- ::.)
gcc -o hello hello.o main.o
```  


  and now whatsnew returns null:  
  `smk wn`  

  Expected:  
```  
Nothing new
```  


Sanity / `touch hello.c` and dry run [Successful](tests_status.md#successful)

# Website building sanity tests



##  Website building sanity tests / cleaning and building using clean and doc sections


  Run:  
  `smk -q reset`  
  `smk ../mysite/Makefile:doc`  

  Expected:  
```  

mkdocs build --clean --quiet
```  


Website building sanity tests / cleaning and building using clean and doc sections [Successful](tests_status.md#successful)

##  Website building sanity tests / listings & dump


  Sources are all md files in docs directory, and the mkdocs.yml file  
  Targets are all files in site directory (the directory is fully build by mkdocs)  

  Run:  
  `smk ls -l`   (long listing format)  

  Expected:  
```  
"mkdocs build --clean --quiet" [doc] [If update  ] [Dir] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/docs
"mkdocs build --clean --quiet" [doc] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/docs/about.md
"mkdocs build --clean --quiet" [doc] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/docs/changelog.md
"mkdocs build --clean --quiet" [doc] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/docs/cmd_line.md
"mkdocs build --clean --quiet" [doc] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/docs/compare_with_make.md
"mkdocs build --clean --quiet" [doc] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/docs/contributing.md
"mkdocs build --clean --quiet" [doc] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/docs/dashboard.md
"mkdocs build --clean --quiet" [doc] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/docs/design_notes.md
"mkdocs build --clean --quiet" [doc] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/docs/fixme.md
"mkdocs build --clean --quiet" [doc] [If update  ] [Dir] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/docs/img
"mkdocs build --clean --quiet" [doc] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/docs/img/sloc.png
"mkdocs build --clean --quiet" [doc] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/docs/img/tests.png
"mkdocs build --clean --quiet" [doc] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/docs/img/tests_ko.svg
"mkdocs build --clean --quiet" [doc] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/docs/img/tests_ok.svg
"mkdocs build --clean --quiet" [doc] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/docs/img/version.svg
"mkdocs build --clean --quiet" [doc] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/docs/index.md
"mkdocs build --clean --quiet" [doc] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/docs/limitations.md
"mkdocs build --clean --quiet" [doc] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/docs/smkfile_format.md
"mkdocs build --clean --quiet" [doc] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/docs/tutorial.md
"mkdocs build --clean --quiet" [doc] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/mkdocs.yml
"mkdocs build --clean --quiet" [doc] [If update  ] [Dir] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site

```  

  Run:  
  `smk ls`   (default short listing format)  

  Expected:  
```  
../mysite/docs
../mysite/docs/about.md
../mysite/docs/changelog.md
../mysite/docs/cmd_line.md
../mysite/docs/compare_with_make.md
../mysite/docs/contributing.md
../mysite/docs/dashboard.md
../mysite/docs/design_notes.md
../mysite/docs/fixme.md
../mysite/docs/img
../mysite/docs/img/sloc.png
../mysite/docs/img/tests.png
../mysite/docs/img/tests_ko.svg
../mysite/docs/img/tests_ok.svg
../mysite/docs/img/version.svg
../mysite/docs/index.md
../mysite/docs/limitations.md
../mysite/docs/smkfile_format.md
../mysite/docs/tutorial.md
../mysite/mkdocs.yml
../mysite/site

```  

  Run:  
  `smk list-targets --long-listing`  

  Expected:  
```  
"mkdocs build --clean --quiet" [doc] [If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/about
"mkdocs build --clean --quiet" [doc] [If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/changelog
"mkdocs build --clean --quiet" [doc] [If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/cmd_line
"mkdocs build --clean --quiet" [doc] [If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/compare_with_make
"mkdocs build --clean --quiet" [doc] [If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/contributing
"mkdocs build --clean --quiet" [doc] [If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/css
"mkdocs build --clean --quiet" [doc] [If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/dashboard
"mkdocs build --clean --quiet" [doc] [If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/design_notes
"mkdocs build --clean --quiet" [doc] [If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/fixme
"mkdocs build --clean --quiet" [doc] [If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/fonts
"mkdocs build --clean --quiet" [doc] [If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/img
"mkdocs build --clean --quiet" [doc] [If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/js
"mkdocs build --clean --quiet" [doc] [If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/limitations
"mkdocs build --clean --quiet" [doc] [If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/search
"mkdocs build --clean --quiet" [doc] [If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/smkfile_format
"mkdocs build --clean --quiet" [doc] [If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/tutorial
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/404.html
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/about/index.html
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/changelog/index.html
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/cmd_line/index.html
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/compare_with_make/index.html
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/contributing/index.html
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/css/base.css
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/css/bootstrap-custom.min.css
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/css/font-awesome.min.css
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/dashboard/index.html
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/design_notes/index.html
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/fixme/index.html
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/fonts/FontAwesome.otf
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/fonts/fontawesome-webfont.eot
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/fonts/fontawesome-webfont.svg
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/fonts/fontawesome-webfont.ttf
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/fonts/fontawesome-webfont.woff
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/fonts/fontawesome-webfont.woff2
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/fonts/glyphicons-halflings-regular.eot
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/fonts/glyphicons-halflings-regular.svg
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/fonts/glyphicons-halflings-regular.ttf
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/fonts/glyphicons-halflings-regular.woff
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/fonts/glyphicons-halflings-regular.woff2
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/img/favicon.ico
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/img/grid.png
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/img/sloc.png
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/img/tests_ko.svg
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/img/tests_ok.svg
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/img/tests.png
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/img/version.svg
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/index.html
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/js/base.js
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/js/bootstrap-3.0.3.min.js
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/js/jquery-1.10.2.min.js
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/limitations/index.html
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/search/lunr.js
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/search/main.js
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/search/search_index.json
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/search/worker.js
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/sitemap.xml
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/sitemap.xml.gz
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/smkfile_format/index.html
"mkdocs build --clean --quiet" [doc] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../mysite/site/tutorial/index.html
```  

  Run:  
  `smk rs`  

  Expected:  
```  
../mysite/Makefile (YYYY:MM:DD HH:MM:SS.SS) :
21: [doc] mkdocs build --clean --quiet

```  

  Run:  
  `smk dump`  

  Expected:  
```  
Command "mkdocs build --clean --quiet" in section [doc], last run [YYYY:MM:DD HH:MM:SS.SS]
[If update  ] [Dir] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs
[If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/about.md
[If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/changelog.md
[If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/cmd_line.md
[If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/compare_with_make.md
[If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/contributing.md
[If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/dashboard.md
[If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/design_notes.md
[If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/fixme.md
[If update  ] [Dir] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/img
[If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/img/sloc.png
[If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/img/tests.png
[If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/img/tests_ko.svg
[If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/img/tests_ok.svg
[If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/img/version.svg
[If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/index.md
[If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/limitations.md
[If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/smkfile_format.md
[If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/docs/tutorial.md
[If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/mkdocs.yml
[If update  ] [Dir] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/404.html
[If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/about
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/about/index.html
[If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/changelog
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/changelog/index.html
[If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/cmd_line
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/cmd_line/index.html
[If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/compare_with_make
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/compare_with_make/index.html
[If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/contributing
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/contributing/index.html
[If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/css
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/css/base.css
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/css/bootstrap-custom.min.css
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/css/font-awesome.min.css
[If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/dashboard
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/dashboard/index.html
[If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/design_notes
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/design_notes/index.html
[If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/fixme
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/fixme/index.html
[If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/fonts
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/fonts/FontAwesome.otf
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/fonts/fontawesome-webfont.eot
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/fonts/fontawesome-webfont.svg
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/fonts/fontawesome-webfont.ttf
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/fonts/fontawesome-webfont.woff
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/fonts/fontawesome-webfont.woff2
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/fonts/glyphicons-halflings-regular.eot
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/fonts/glyphicons-halflings-regular.svg
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/fonts/glyphicons-halflings-regular.ttf
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/fonts/glyphicons-halflings-regular.woff
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/fonts/glyphicons-halflings-regular.woff2
[If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/img
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/img/favicon.ico
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/img/grid.png
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/img/sloc.png
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/img/tests.png
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/img/tests_ko.svg
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/img/tests_ok.svg
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/img/version.svg
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/index.html
[If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/js
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/js/base.js
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/js/bootstrap-3.0.3.min.js
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/js/jquery-1.10.2.min.js
[If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/limitations
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/limitations/index.html
[If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/search
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/search/lunr.js
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/search/main.js
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/search/search_index.json
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/search/worker.js
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/sitemap.xml
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/sitemap.xml.gz
[If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/smkfile_format
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/smkfile_format/index.html
[If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/tutorial
[If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /home/lionel/Proj/smk/tests/mysite/site/tutorial/index.html
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libbz2.so.1.0.4
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libc-2.28.so
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libdl-2.28.so
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libexpat.so.1.6.8
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/liblzma.so.5.2.2
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libm-2.28.so
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libpthread-2.28.so
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libutil-2.28.so
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libz.so.1.2.11
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/bin
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/bin/mkdocs
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/locale-archive
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/__future__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/_bootlocale.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/_collections_abc.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/_compat_pickle.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/_compression.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/_markupbase.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/_sitebuiltins.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/_weakrefset.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/abc.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/base64.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/bisect.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/bz2.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/calendar.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/codecs.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/contextlib.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/contextvars.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/copy.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/copyreg.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/datetime.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/decimal.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/dis.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/enum.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/fnmatch.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/functools.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/genericpath.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/gzip.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/hashlib.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/heapq.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/inspect.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/io.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/keyword.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/linecache.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/locale.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/lzma.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/numbers.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/opcode.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/operator.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/os.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/pickle.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/pkgutil.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/platform.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/plistlib.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/posixpath.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/pprint.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/quopri.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/random.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/re.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/reprlib.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/selectors.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/shutil.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/signal.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/site.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/sitecustomize.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/socket.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/sre_compile.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/sre_constants.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/sre_parse.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/ssl.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/stat.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/string.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/struct.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/subprocess.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/sysconfig.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/tempfile.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/textwrap.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/threading.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/token.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/tokenize.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/traceback.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/types.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/warnings.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/weakref.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/__pycache__/zipfile.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/asyncio
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/asyncio/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/asyncio/__pycache__/base_events.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/asyncio/__pycache__/base_futures.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/asyncio/__pycache__/base_subprocess.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/asyncio/__pycache__/base_tasks.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/asyncio/__pycache__/constants.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/asyncio/__pycache__/coroutines.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/asyncio/__pycache__/events.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/asyncio/__pycache__/format_helpers.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/asyncio/__pycache__/futures.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/asyncio/__pycache__/locks.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/asyncio/__pycache__/log.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/asyncio/__pycache__/protocols.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/asyncio/__pycache__/queues.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/asyncio/__pycache__/runners.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/asyncio/__pycache__/selector_events.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/asyncio/__pycache__/sslproto.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/asyncio/__pycache__/streams.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/asyncio/__pycache__/subprocess.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/asyncio/__pycache__/tasks.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/asyncio/__pycache__/transports.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/asyncio/__pycache__/unix_events.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/collections
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/collections/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/collections/__pycache__/abc.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/concurrent
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/concurrent/__pycache__/__init__.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/concurrent/futures
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/concurrent/futures/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/concurrent/futures/__pycache__/_base.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/email
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/email/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/email/__pycache__/_parseaddr.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/email/__pycache__/_policybase.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/email/__pycache__/base64mime.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/email/__pycache__/charset.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/email/__pycache__/encoders.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/email/__pycache__/errors.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/email/__pycache__/feedparser.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/email/__pycache__/header.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/email/__pycache__/parser.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/email/__pycache__/quoprimime.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/email/__pycache__/utils.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/encodings
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/encodings/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/encodings/__pycache__/aliases.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/encodings/__pycache__/latin_1.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/encodings/__pycache__/unicode_escape.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/encodings/__pycache__/utf_8.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/encodings/__pycache__/utf_8_sig.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/html
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/html/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/html/__pycache__/entities.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/html/__pycache__/parser.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/importlib
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/importlib/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/importlib/__pycache__/abc.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/importlib/__pycache__/machinery.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/importlib/__pycache__/util.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/json
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/json/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/json/__pycache__/decoder.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/json/__pycache__/encoder.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/json/__pycache__/scanner.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/lib-dynload
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/lib-dynload/_asyncio.cpython-37m-x86_64-linux-gnu.so
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/lib-dynload/_bz2.cpython-37m-x86_64-linux-gnu.so
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/lib-dynload/_contextvars.cpython-37m-x86_64-linux-gnu.so
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/lib-dynload/_decimal.cpython-37m-x86_64-linux-gnu.so
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/lib-dynload/_hashlib.cpython-37m-x86_64-linux-gnu.so
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/lib-dynload/_json.cpython-37m-x86_64-linux-gnu.so
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/lib-dynload/_lzma.cpython-37m-x86_64-linux-gnu.so
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/lib-dynload/_opcode.cpython-37m-x86_64-linux-gnu.so
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/lib-dynload/_ssl.cpython-37m-x86_64-linux-gnu.so
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/logging/__pycache__/__init__.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/urllib
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/urllib/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/urllib/__pycache__/parse.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/xml
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/xml/__pycache__/__init__.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/xml/etree
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/xml/etree/__pycache__/ElementPath.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/xml/etree/__pycache__/ElementTree.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/xml/etree/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/xml/etree/__pycache__/cElementTree.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/xml/parsers
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/xml/parsers/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3.7/xml/parsers/__pycache__/expat.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/Brlapi-0.6.7.egg-info
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/Click-7.0.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/Click-7.0.egg-info/PKG-INFO
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/Jinja2-2.10.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/Jinja2-2.10.egg-info/PKG-INFO
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/Jinja2-2.10.egg-info/entry_points.txt
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/Mako-1.0.7.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/Mako-1.0.7.egg-info/PKG-INFO
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/Mako-1.0.7.egg-info/entry_points.txt
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/Markdown-3.0.1.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/Markdown-3.0.1.egg-info/PKG-INFO
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/Markdown-3.0.1.egg-info/entry_points.txt
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/MarkupSafe-1.1.0.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/MarkupSafe-1.1.0.egg-info/PKG-INFO
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/Pillow-5.4.1.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/Pillow-5.4.1.egg-info/PKG-INFO
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/PyGObject-3.30.4.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/PyGObject-3.30.4.egg-info/PKG-INFO
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/PySimpleSOAP-1.16.2.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/PyYAML-3.13.egg-info
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/Pygments-2.3.1.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/Pygments-2.3.1.egg-info/PKG-INFO
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/Pygments-2.3.1.egg-info/entry_points.txt
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/SecretStorage-2.3.1.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/SecretStorage-2.3.1.egg-info/PKG-INFO
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/_yaml.cpython-37m-x86_64-linux-gnu.so
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/arrow-0.12.1.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/arrow-0.12.1.egg-info/PKG-INFO
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/asn1crypto-0.24.0.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/asn1crypto-0.24.0.egg-info/PKG-INFO
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/certifi-2018.8.24.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/certifi-2018.8.24.egg-info/PKG-INFO
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/chardet-3.0.4.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/chardet-3.0.4.egg-info/PKG-INFO
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/chardet-3.0.4.egg-info/entry_points.txt
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/chrome_gnome_shell-0.0.0.egg-info
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/click
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/click/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/click/__pycache__/_compat.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/click/__pycache__/_unicodefun.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/click/__pycache__/core.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/click/__pycache__/decorators.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/click/__pycache__/exceptions.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/click/__pycache__/formatting.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/click/__pycache__/globals.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/click/__pycache__/parser.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/click/__pycache__/termui.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/click/__pycache__/types.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/click/__pycache__/utils.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/colorama-0.3.7.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/colorama-0.3.7.egg-info/PKG-INFO
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/colour-0.1.5.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/colour-0.1.5.egg-info/PKG-INFO
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/cryptography-2.3.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/cryptography-2.3.egg-info/PKG-INFO
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/cupshelpers-1.0.egg-info
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/distro-1.3.0.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/distro-1.3.0.egg-info/PKG-INFO
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/distro-1.3.0.egg-info/entry_points.txt
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/distro_info-0.20.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/distro_info-0.20.egg-info/PKG-INFO
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/easygui-0.96.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/entrypoints.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/gphoto2-1.9.0.egg-info
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/httplib2-0.11.3.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/httplib2-0.11.3.egg-info/PKG-INFO
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/idna-2.6.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/idna-2.6.egg-info/PKG-INFO
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/jinja2
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/jinja2/__init__.py
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/jinja2/_compat.py
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/jinja2/_identifier.py
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/jinja2/asyncfilters.py
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/jinja2/asyncsupport.py
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/jinja2/bccache.py
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/jinja2/compiler.py
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/jinja2/defaults.py
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/jinja2/environment.py
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/jinja2/exceptions.py
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/jinja2/filters.py
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/jinja2/idtracking.py
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/jinja2/lexer.py
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/jinja2/loaders.py
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/jinja2/nodes.py
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/jinja2/optimizer.py
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/jinja2/parser.py
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/jinja2/runtime.py
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/jinja2/tests.py
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/jinja2/utils.py
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/jinja2/visitor.py
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/keyring-17.1.1.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/keyring-17.1.1.egg-info/PKG-INFO
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/keyring-17.1.1.egg-info/entry_points.txt
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/keyrings.alt-3.1.1.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/keyrings.alt-3.1.1.egg-info/PKG-INFO
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/keyrings.alt-3.1.1.egg-info/entry_points.txt
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/livereload-2.6.0.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/livereload-2.6.0.egg-info/PKG-INFO
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/livereload-2.6.0.egg-info/entry_points.txt
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/louis-3.8.0.egg-info
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/markdown
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/markdown/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/markdown/__pycache__/blockparser.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/markdown/__pycache__/blockprocessors.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/markdown/__pycache__/core.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/markdown/__pycache__/inlinepatterns.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/markdown/__pycache__/postprocessors.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/markdown/__pycache__/preprocessors.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/markdown/__pycache__/serializers.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/markdown/__pycache__/treeprocessors.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/markdown/__pycache__/util.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/markdown/extensions
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/markdown/extensions/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/markdown/extensions/__pycache__/codehilite.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/markdown/extensions/__pycache__/fenced_code.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/markdown/extensions/__pycache__/tables.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/markdown/extensions/__pycache__/toc.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/markupsafe
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/markupsafe/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/markupsafe/__pycache__/_compat.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/markupsafe/_speedups.cpython-37m-x86_64-linux-gnu.so
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/meld-3.20.0.egg-info
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs-1.0.4.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs-1.0.4.egg-info/PKG-INFO
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs-1.0.4.egg-info/entry_points.txt
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs-1.0.4.egg-info/requires.txt
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/__pycache__/__main__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/__pycache__/exceptions.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/__pycache__/plugins.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/__pycache__/theme.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/commands
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/commands/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/commands/__pycache__/build.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/commands/__pycache__/gh_deploy.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/commands/__pycache__/new.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/commands/__pycache__/serve.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/config
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/config/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/config/__pycache__/base.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/config/__pycache__/config_options.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/config/__pycache__/defaults.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/contrib
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/contrib/__pycache__/__init__.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/contrib/search
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/contrib/search/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/contrib/search/__pycache__/search_index.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/contrib/search/templates
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/contrib/search/templates/search
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/contrib/search/templates/search/main.js
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/contrib/search/templates/search/worker.js
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/structure
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/structure/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/structure/__pycache__/files.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/structure/__pycache__/nav.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/structure/__pycache__/pages.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/structure/__pycache__/toc.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/templates
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/templates/sitemap.xml
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/themes
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/themes/__pycache__/__init__.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/themes/mkdocs
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/themes/mkdocs/404.html
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/themes/mkdocs/__pycache__
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/themes/mkdocs/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/themes/mkdocs/base.html
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/themes/mkdocs/content.html
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/themes/mkdocs/css
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/themes/mkdocs/css/base.css
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/themes/mkdocs/fonts
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/themes/mkdocs/img
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/themes/mkdocs/img/favicon.ico
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/themes/mkdocs/img/grid.png
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/themes/mkdocs/js
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/themes/mkdocs/js/base.js
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/themes/mkdocs/keyboard-modal.html
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/themes/mkdocs/main.html
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/themes/mkdocs/mkdocs_theme.yml
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/themes/mkdocs/nav-sub.html
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/themes/mkdocs/search-modal.html
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/themes/mkdocs/toc.html
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/utils
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/utils/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/utils/__pycache__/filters.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/utils/__pycache__/ghp_import.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/mkdocs/utils/__pycache__/meta.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/olefile-0.46.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/olefile-0.46.egg-info/PKG-INFO
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pexpect-4.6.0.egg-info
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pkg_resources
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pkg_resources/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pkg_resources/__pycache__/py31compat.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pkg_resources/_vendor
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pkg_resources/_vendor/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pkg_resources/_vendor/__pycache__/appdirs.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pkg_resources/_vendor/__pycache__/pyparsing.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pkg_resources/_vendor/__pycache__/six.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pkg_resources/_vendor/packaging
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pkg_resources/_vendor/packaging/__pycache__/__about__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pkg_resources/_vendor/packaging/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pkg_resources/_vendor/packaging/__pycache__/_compat.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pkg_resources/_vendor/packaging/__pycache__/_structures.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pkg_resources/_vendor/packaging/__pycache__/markers.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pkg_resources/_vendor/packaging/__pycache__/requirements.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pkg_resources/_vendor/packaging/__pycache__/specifiers.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pkg_resources/_vendor/packaging/__pycache__/version.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pkg_resources/_vendor/pyparsing.py
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pkg_resources/extern
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pkg_resources/extern/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/psutil-5.5.0.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pycairo-1.16.2.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pycrypto-2.6.1.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pycups-1.9.73.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pycurl-7.43.0.2.egg-info
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pygments
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pygments/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pygments/__pycache__/modeline.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pygments/__pycache__/plugin.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pygments/__pycache__/util.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pygments/formatters
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pygments/formatters/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pygments/formatters/__pycache__/_mapping.cpython-37.pyc
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pygments/lexers
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pygments/lexers/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pygments/lexers/__pycache__/_mapping.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pyinotify-0.9.6.egg-info
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pymediainfo-3.0.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pymediainfo-3.0.egg-info/PKG-INFO
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pymediainfo-3.0.egg-info/namespace_packages.txt
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pysmbc-1.0.15.6.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/python_apt-1.8.1.egg-info
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/python_dateutil-2.6.1.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/python_dateutil-2.6.1.egg-info/PKG-INFO
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/python_debian-0.1.34.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/python_debian-0.1.34.egg-info/PKG-INFO
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/python_debianbts-2.8.2.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/python_debianbts-2.8.2.egg-info/PKG-INFO
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/python_debianbts-2.8.2.egg-info/entry_points.txt
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/python_pam-1.8.4.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/python_pam-1.8.4.egg-info/PKG-INFO
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/python_xapp-1.2.0.egg-info
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/python_xlib-0.23.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/python_xlib-0.23.egg-info/PKG-INFO
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pyxdg-0.25.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/pyzmq-17.1.2.egg-info
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/rapid_photo_downloader-0.9.13.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/rapid_photo_downloader-0.9.13.egg-info/PKG-INFO
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/rapid_photo_downloader-0.9.13.egg-info/entry_points.txt
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/rawkit-0.6.0.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/rawkit-0.6.0.egg-info/PKG-INFO
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/reportbug-7.5.1.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/reportbug-7.5.1.egg-info/PKG-INFO
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/reportlab-3.5.13.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/reportlab-3.5.13.egg-info/PKG-INFO
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/requests-2.20.0.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/requests-2.20.0.egg-info/PKG-INFO
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/rpl-1.5.6.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/rpl-1.5.6.egg-info/PKG-INFO
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/scour-0.37.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/scour-0.37.egg-info/PKG-INFO
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/scour-0.37.egg-info/entry_points.txt
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/setproctitle-1.1.10.egg-info
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/setuptools-40.6.3.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/setuptools-40.6.3.egg-info/PKG-INFO
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/setuptools-40.6.3.egg-info/entry_points.txt
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/six-1.12.0.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/six-1.12.0.egg-info/PKG-INFO
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/sortedcontainers-2.0.4.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/sortedcontainers-2.0.4.egg-info/PKG-INFO
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/tornado-5.1.1.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/tornado-5.1.1.egg-info/PKG-INFO
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/unattended_upgrades-0.1.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/unattended_upgrades-0.1.egg-info/PKG-INFO
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/urllib3-1.24.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/urllib3-1.24.egg-info/PKG-INFO
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/yaml
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/yaml/__pycache__/__init__.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/yaml/__pycache__/composer.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/yaml/__pycache__/constructor.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/yaml/__pycache__/cyaml.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/yaml/__pycache__/dumper.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/yaml/__pycache__/emitter.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/yaml/__pycache__/error.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/yaml/__pycache__/events.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/yaml/__pycache__/loader.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/yaml/__pycache__/nodes.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/yaml/__pycache__/parser.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/yaml/__pycache__/reader.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/yaml/__pycache__/representer.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/yaml/__pycache__/resolver.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/yaml/__pycache__/scanner.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/yaml/__pycache__/serializer.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/python3/dist-packages/yaml/__pycache__/tokens.cpython-37.pyc
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/gconv/gconv-modules.cache
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libcrypto.so.1.1
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libmpdec.so.2.4.2
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libssl.so.1.1
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libyaml-0.so.2.0.5
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/local/lib/python3.7/dist-packages
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/fonts-font-awesome/css/font-awesome.min.css
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/fonts-glyphicons/glyphicons-halflings-regular.eot
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/fonts-glyphicons/glyphicons-halflings-regular.svg
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/fonts-glyphicons/glyphicons-halflings-regular.woff
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/fonts-glyphicons/glyphicons-halflings-regular.woff2
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/fonts/eot/font-awesome/fontawesome-webfont.eot
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/fonts/opentype/font-awesome/FontAwesome.otf
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/fonts/svg/font-awesome/fontawesome-webfont.svg
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/fonts/truetype/font-awesome/fontawesome-webfont.ttf
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/fonts/truetype/glyphicons/glyphicons-halflings-regular.ttf
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/fonts/woff/font-awesome/fontawesome-webfont.woff
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/fonts/woff/font-awesome/fontawesome-webfont.woff2
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/javascript/bootstrap/js/bootstrap.min.js
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/javascript/bootswatch/cerulean/bootstrap.min.css
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/javascript/jquery/jquery.min.js
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/javascript/lunr/lunr.js
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/mkdocs/themes
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/mkdocs/themes/mkdocs_bootstrap-0.2.0.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/mkdocs/themes/mkdocs_bootstrap-0.2.0.egg-info/PKG-INFO
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/mkdocs/themes/mkdocs_bootstrap-0.2.0.egg-info/entry_points.txt
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/mkdocs/themes/mkdocs_bootswatch-0.4.0.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/mkdocs/themes/mkdocs_bootswatch-0.4.0.egg-info/PKG-INFO
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/mkdocs/themes/mkdocs_bootswatch-0.4.0.egg-info/entry_points.txt
[If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/mkdocs/themes/mkdocs_nature-0.3.1.egg-info
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/mkdocs/themes/mkdocs_nature-0.3.1.egg-info/PKG-INFO
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/mkdocs/themes/mkdocs_nature-0.3.1.egg-info/entry_points.txt
[If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/zoneinfo/Europe/Paris
```  


Website building sanity tests / listings & dump [Successful](tests_status.md#successful)

##  Website building sanity tests / building without section


  Run:  
     First run after reset : all command should be executed, including `clean`  
  `smk reset`  

  Expected:  
```  
Deleting .smk.Makefile
```  


  `smk ../mysite/Makefile`  

  Expected:  
```  
mkdocs build --clean --quiet

```  

  Run:  
     Third run : all targets are up-to-date  
  `smk --explain --verbose`  

  Expected:  
```  
No need to run mkdocs build --clean --quiet
Nothing to run
```  


Website building sanity tests / building without section [Successful](tests_status.md#successful)

##  Website building sanity tests / re-building


  Run:  
  `touch ../mysite/docs/about.md`  
  `smk whatsnew`  

  Expected:  
```  
[Updated] [Source] ../mysite/docs/about.md
```  

  Run:  
  `rm ../mysite/site/tutorial/index.html`  
  `smk wn`  

  Expected:  
```  
[Updated] [Source] ../mysite/docs/about.md
[Updated] [Source] ../mysite/site/tutorial
[Updated] [Target] ../mysite/site/tutorial
[Missing] [Target] ../mysite/site/tutorial/index.html
```  

  Run:  
  `smk -e -v`  

  Expected:  
```  
run "mkdocs build --clean --quiet" because Source file ../mysite/docs/about.md has been updated (-- ::.)
mkdocs build --clean --quiet

```  


Website building sanity tests / re-building [Successful](tests_status.md#successful)

##  Website building sanity tests / cleaning


  Run:  
  `smk clean`  

  Expected:  
```  
Deleting dir ../mysite/site/about
Deleting dir ../mysite/site/changelog
Deleting dir ../mysite/site/cmd_line
Deleting dir ../mysite/site/compare_with_make
Deleting dir ../mysite/site/contributing
Deleting dir ../mysite/site/css
Deleting dir ../mysite/site/dashboard
Deleting dir ../mysite/site/design_notes
Deleting dir ../mysite/site/fixme
Deleting dir ../mysite/site/fonts
Deleting dir ../mysite/site/img
Deleting dir ../mysite/site/js
Deleting dir ../mysite/site/limitations
Deleting dir ../mysite/site/search
Deleting dir ../mysite/site/smkfile_format
Deleting dir ../mysite/site/tutorial
Deleting file ../mysite/site/404.html
Deleting file ../mysite/site/about/index.html
Deleting file ../mysite/site/changelog/index.html
Deleting file ../mysite/site/cmd_line/index.html
Deleting file ../mysite/site/compare_with_make/index.html
Deleting file ../mysite/site/contributing/index.html
Deleting file ../mysite/site/css/base.css
Deleting file ../mysite/site/css/bootstrap-custom.min.css
Deleting file ../mysite/site/css/font-awesome.min.css
Deleting file ../mysite/site/dashboard/index.html
Deleting file ../mysite/site/design_notes/index.html
Deleting file ../mysite/site/fixme/index.html
Deleting file ../mysite/site/fonts/FontAwesome.otf
Deleting file ../mysite/site/fonts/fontawesome-webfont.eot
Deleting file ../mysite/site/fonts/fontawesome-webfont.svg
Deleting file ../mysite/site/fonts/fontawesome-webfont.ttf
Deleting file ../mysite/site/fonts/fontawesome-webfont.woff
Deleting file ../mysite/site/fonts/fontawesome-webfont.woff2
Deleting file ../mysite/site/fonts/glyphicons-halflings-regular.eot
Deleting file ../mysite/site/fonts/glyphicons-halflings-regular.svg
Deleting file ../mysite/site/fonts/glyphicons-halflings-regular.ttf
Deleting file ../mysite/site/fonts/glyphicons-halflings-regular.woff
Deleting file ../mysite/site/fonts/glyphicons-halflings-regular.woff2
Deleting file ../mysite/site/img/favicon.ico
Deleting file ../mysite/site/img/grid.png
Deleting file ../mysite/site/img/sloc.png
Deleting file ../mysite/site/img/tests_ko.svg
Deleting file ../mysite/site/img/tests_ok.svg
Deleting file ../mysite/site/img/tests.png
Deleting file ../mysite/site/img/version.svg
Deleting file ../mysite/site/index.html
Deleting file ../mysite/site/js/base.js
Deleting file ../mysite/site/js/bootstrap-3.0.3.min.js
Deleting file ../mysite/site/js/jquery-1.10.2.min.js
Deleting file ../mysite/site/limitations/index.html
Deleting file ../mysite/site/search/lunr.js
Deleting file ../mysite/site/search/main.js
Deleting file ../mysite/site/search/search_index.json
Deleting file ../mysite/site/search/worker.js
Deleting file ../mysite/site/sitemap.xml
Deleting file ../mysite/site/sitemap.xml.gz
Deleting file ../mysite/site/smkfile_format/index.html
Deleting file ../mysite/site/tutorial/index.html
```  


Website building sanity tests / cleaning [Successful](tests_status.md#successful)

##  Website building sanity tests / re-building after a clean


  Run:  
  `smk --explain`  

  Expected:  
```  
run "mkdocs build --clean --quiet" because Source file ../mysite/docs/about.md has been updated (-- ::.)
mkdocs build --clean --quiet
```  


Website building sanity tests / re-building after a clean [Successful](tests_status.md#successful)

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
  Sources: (2)
  - ../hello.c/hello.o
  - ../hello.c/main.o
  Targets: (1)
  - ../hello.c/hello

Command "gcc -o hello.o -c hello.c" in section [hello.o], last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (1)
  - ../hello.c/hello.c
  Targets: (1)
  - ../hello.c/hello.o

Command "gcc -o main.o -c main.c" in section [main.o], last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (2)
  - ../hello.c/hello.h
  - ../hello.c/main.c
  Targets: (1)
  - ../hello.c/main.o

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
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/hello.o
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/main.o
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/ld-2.28.so
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libc-2.28.so
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libdl-2.28.so
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libm-2.28.so
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/crtbegin.o
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/crtend.o
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/libgcc.a
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib64/libgcc_s.so
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib64/libgcc_s.so.1
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/libexec/gcc/x86_64-pc-linux-gnu/7.3.1/liblto_plugin.so.0.0.0
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/locale-archive
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/crt1.o
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/crti.o
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/crtn.o
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/gconv/gconv-modules.cache
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libc.so
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libc_nonshared.a
  Targets: (1)
  - [If absence ] [Fil] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/hello

Command "gcc -o hello.o -c hello.c" in section [hello.o], last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (52)
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/hello.c
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libc-2.28.so
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libdl-2.28.so
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libm-2.28.so
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stdarg.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stddef.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/alloca.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/endian.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/features.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdc-predef.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdio.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdlib.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/byteswap.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/endian.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/floatn-common.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/floatn.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/long-double.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/select.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/FILE.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/time_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/typesizes.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/waitflags.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/waitstatus.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/wordsize.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/gnu/stubs.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/cdefs.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/select.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/types.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/locale-archive
  Targets: (1)
  - [If absence ] [Fil] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/hello.o

Command "gcc -o main.o -c main.c" in section [main.o], last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (53)
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/hello.h
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/main.c
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libc-2.28.so
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libdl-2.28.so
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libm-2.28.so
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stdarg.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stddef.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/alloca.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/endian.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/features.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdc-predef.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdio.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdlib.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/byteswap.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/endian.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/floatn-common.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/floatn.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/long-double.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/select.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/FILE.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/time_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/typesizes.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/waitflags.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/waitstatus.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/wordsize.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/gnu/stubs.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/cdefs.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/select.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/types.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/locale-archive
  Targets: (1)
  - [If absence ] [Fil] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/main.o

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


List queries / lr | list-runs [Successful](tests_status.md#successful)

##  List queries / lt | list-targets

  Run:  
  `smk list-targets --long-listing ../hello.c/Makefile.2`  

  Expected: (long form)  
```  
"gcc -o hello hello.o main.o" [hello] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/hello
"gcc -o hello.o -c hello.c" [hello.o] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/hello.o
"gcc -o main.o -c main.c" [main.o] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/main.o
```  

  Run:  
  `smk lt ../hello.c/Makefile.2`  

  Expected: (short form)  
```  
../hello.c/hello
../hello.c/hello.o
../hello.c/main.o
```  


List queries / lt | list-targets [Successful](tests_status.md#successful)

##  List queries / ls | list-sources


  Run: ls without shortening file names  
  `smk ls -ds ../hello.c/Makefile.2`  

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
../hello.c/hello.o
../hello.c/main.o
../hello.c/hello.c
../hello.c/hello.h
../hello.c/main.c
```  


List queries / ls | list-sources [Successful](tests_status.md#successful)

##  List queries / ls | list-sources --show-all-files


  Run: (short form)  
  `smk list-sources --shows-system-files ../hello.c/Makefile.2`  

  Expected:  
```  
../hello.c/hello.o
../hello.c/main.o
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
../hello.c/hello.c
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
../hello.c/hello.h
../hello.c/main.c
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
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/hello.o
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/main.o
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/ld-2.28.so
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libc-2.28.so
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libdl-2.28.so
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libm-2.28.so
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib64/libgcc_s.so
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib64/libgcc_s.so.1
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/libexec/gcc/x86_64-pc-linux-gnu/7.3.1/liblto_plugin.so.0.0.0
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/crtbegin.o
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/crtend.o
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/libgcc.a
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/locale-archive
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/crt1.o
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/crti.o
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/crtn.o
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/gconv/gconv-modules.cache
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libc_nonshared.a
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libc.so
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/hello.c
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libc-2.28.so
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libdl-2.28.so
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libm-2.28.so
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stdarg.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stddef.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/alloca.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/endian.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/features.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdc-predef.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdio.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdlib.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/byteswap.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/endian.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/floatn-common.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/floatn.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/long-double.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/select.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/FILE.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/typesizes.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/time_t.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/waitflags.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/waitstatus.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/wordsize.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/gnu/stubs.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/cdefs.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/select.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/types.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/locale-archive
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/hello.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/main.c
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libc-2.28.so
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libdl-2.28.so
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /lib/x86_64-linux-gnu/libm-2.28.so
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stdarg.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stddef.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/alloca.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/endian.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/features.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdc-predef.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdio.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdlib.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/byteswap.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/endian.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/floatn-common.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/floatn.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/long-double.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/select.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/FILE.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/typesizes.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/time_t.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/waitflags.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/waitstatus.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/wordsize.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/gnu/stubs.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/cdefs.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/select.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/types.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/locale-archive
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
Deleting file ../hello.c/hello
Deleting file ../hello.c/hello.o
Deleting file ../hello.c/main.o
```  


##  Targets related functions / real clean

  Run:  
  `smk clean`  

  Expected:  
```  
Deleting file ../hello.c/hello
Deleting file ../hello.c/hello.o
Deleting file ../hello.c/main.o
```  

  Run:  
  `smk -e -mt`  (to check effective cleaning)  

  Expected:  
```  
run "gcc -o hello.o -c hello.c" because Target file ../hello.c/hello.o is missing
gcc -o hello.o -c hello.c
run "gcc -o main.o -c main.c" because Target file ../hello.c/main.o is missing
gcc -o main.o -c main.c
run "gcc -o hello hello.o main.o" because Target file ../hello.c/hello is missing
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

##  Targets related functions / Build unknown target


  Run:  
  `smk build mainzzzzz.o`  

  Expected:  
```  
Target "mainzzzzz.o" not found
run "smk list-targets" to get a list of possible target
```  


Targets related functions / Build unknown target [Successful](tests_status.md#successful)

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


Implicit naming / Implicit naming [Successful](tests_status.md#successful)

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


Run errors / -k -i [Successful](tests_status.md#successful)

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
  `smk list-unused`  


Run errors / `run` command fails [Successful](tests_status.md#successful)

##  Run errors / debug option


  Run:  
  `smk -d dump`  

  Expected:  

```  
Error : No smkfile given, and no existing runfile in dir

Settings / Command line analysis:
---------------------------------

   Verbosity         : DEBUG
   Command           : DUMP
   Smkfile name      : 
   Runfile name      : 
   Strace out file   : 
   Section name      : 
   Cmd Line          : 
   Target name       : 
   Unidentified Opt  : 
   Initial directory : /home/lionel/Proj/smk/tests/07_run_error_tests

   System Files      : 
   - /usr/*
   - /lib/*
   - /etc/*
   - /opt/*

   Ignore list       : 
   - /sys/*
   - /proc/*
   - /dev/*
   - /tmp/*
   - /etc/ld.so.cache

   [ ] Build_Missing_Targets
   [ ] Always_Make
   [ ] Explain
   [ ] Dry_Run
   [ ] Keep_Going
   [ ] Ignore_Errors
   [ ] Long_Listing_Format
   [ ] Warnings_As_Errors
   [X] Shorten_File_Names
   [X] Filter_Sytem_Files

---------------------------------

```  


Run errors / debug option [Successful](tests_status.md#successful)

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
   read-smkfile | rs : shows smk understanding of a Smkfile
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
   -mt  | --missing-targets : build if missing targets
                              (default is to build only if
                               sources are updated)
   -a   | --always-make     : unconditionally make all targets
   -e   | --explain         : explain why each target is made
   -n   | --dry-run         : print the commands that would be
                              executed, but do not execute them
   -sa  | --show-all-files  : show also system files
   -ds  | --dont-shorten    : print files with full path
   -i   | --ignore-errors   : ignore all errors in commands
                              executed to remake files
   -l   | --long-listing    : use a long listing format when
                              listing files
   -k   | --keep-going      : Do as much work as possible
   -We  | --Warnings=error  : treat warnings as errors
   -v   | --verbose
   -q   | --quiet           : no message unless error,
                              Warning are also ignored
   -h   | --help            : this message

http://lionel.draghi.free.fr/smk/

```  


Command line / Help options [Successful](tests_status.md#successful)

##  Command line / Version option


  Test that the version command will put :  

  Run:  
  `smk version`  

  Expected:  

```  
0.4.0
```  


Command line / Version option [Successful](tests_status.md#successful)

##  Command line / Illegal cmd lines


  Run:  
  `smk read-smkfile status`  

  Expected:  

```  
Error : More than one command on command line : STATUS and READ_SMKFILE

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
   read-smkfile | rs : shows smk understanding of a Smkfile
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
   -mt  | --missing-targets : build if missing targets
                              (default is to build only if
                               sources are updated)
   -a   | --always-make     : unconditionally make all targets
   -e   | --explain         : explain why each target is made
   -n   | --dry-run         : print the commands that would be
                              executed, but do not execute them
   -sa  | --show-all-files  : show also system files
   -ds  | --dont-shorten    : print files with full path
   -i   | --ignore-errors   : ignore all errors in commands
                              executed to remake files
   -l   | --long-listing    : use a long listing format when
                              listing files
   -k   | --keep-going      : Do as much work as possible
   -We  | --Warnings=error  : treat warnings as errors
   -v   | --verbose
   -q   | --quiet           : no message unless error,
                              Warning are also ignored
   -h   | --help            : this message


http://lionel.draghi.free.fr/smk/

```  


Command line / Illegal cmd lines [Successful](tests_status.md#successful)

##  Command line / Option given after a command


  Run:  
  `smk reset -l`  

  Expected:  

```  
```  


Command line / Option given after a command [Successful](tests_status.md#successful)

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
  `smk :main.o` with main.o up to date  

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

  Run:  
  `smk -a ../hello.c/Makezzzzzfile.4:mrproper`  

  Expected:  
```  
Error : Unknown Smkfile ../hello.c/Makezzzzzfile.4 in ../hello.c/Makezzzzzfile.4:mrproper
Error : No smkfile given, and more than one runfile in dir
```  


Sections related functions / smkmfile:section notation [Successful](tests_status.md#successful)

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
  `rm default.smk`  
  `smk -q reset`  
  `smk run ./ogg-to-mp3.sh`  

  Expected:  
```  
./ogg-to-mp3.sh

```  

  Run:  
  `smk st -l`  

  Expected:  
```  
Command "./ogg-to-mp3.sh", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (5)
  - [If update  ] [Dir] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] ./
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] ogg-to-mp3.sh
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] to-mp3.sh
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] x.ogg
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] y.ogg
  Targets: (2)
  - [If absence ] [Fil] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] x.mp3
  - [If absence ] [Fil] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] y.mp3

```  


Directory update tests / start conversion [Successful](tests_status.md#successful)

##  Directory update tests / new ogg in dir


  Run:  
  `cp x.ogg z.ogg`  
  `smk whatsnew`  

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
  `smk -q run ./ogg-to-mp3.sh`  
  `touch ogg-to-mp3`  
  `smk -e`  

  Expected:  
```  
run "./ogg-to-mp3.sh" because Source file ogg-to-mp3.sh has been updated (YYYY:MM:DD HH:MM:SS.SS)
./ogg-to-mp3.sh
```  


Directory update tests / ogg-to-mp3 is modified [Successful](tests_status.md#successful)

##  Directory update tests / adding a .ogg file in a subdir


  Run:  
  `mkdir dir1`  
  `smk wn -l`  

  Expected:  
```  
[Dir] [Normal] [Source] [Updated] [YYYY:MM:DD HH:MM:SS.SS] ./
```  

  Run:  
  `smk -e run ./ogg-to-mp3.sh`  

  Expected:  
```  
run "./ogg-to-mp3.sh" because Source dir ./ has been updated (YYYY:MM:DD HH:MM:SS.SS)
./ogg-to-mp3.sh
```  

  Run:  
  `cp x.ogg dir1/t.ogg`  
  `smk -e run ./ogg-to-mp3.sh`  

  Expected:  
```  
run "./ogg-to-mp3.sh" because Source dir dir1 has been updated (YYYY:MM:DD HH:MM:SS.SS)
./ogg-to-mp3.sh
```  


Directory update tests / adding a .ogg file in a subdir [Successful](tests_status.md#successful)

##  Directory update tests / smk clean


  Run:  
  `smk clean`  

  Expected:  
```  
Deleting file dir1/t.mp3
Deleting file x.mp3
Deleting file y.mp3
Deleting file z.mp3
```  


Directory update tests / smk clean [Successful](tests_status.md#successful)

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

10. File is the current dir
Expected :
"./"
OK

11. File is over Dir, Dir and File with final /
Expected :
"./"
OK

12. No common part
Expected :
"/opt/GNAT/2018/lib64/libgcc_s.so"
OK

All tests OK [Successful](tests_status.md#successful)

# Analyze_Line unit tests


## execve, should be ignored
   Line: 11750 execve("/opt/GNAT/2018/bin/gcc", ["gcc", "-o", "hello", "hello.o", "main.o"], 0x7ffd629baf60 /* 45 vars */) = 0
   - Expected Call_Type: "ignored", OK

## SIGCHLD line
   Line: 11751 --- SIGCHLD {si_signo=SIGCHLD, si_code=CLD_EXITED, si_pid=11752, si_uid=1000, si_status=0, si_utime=0, si_stime=0} ---
   - Expected Call_Type: "ignored", OK

## Read openat
   Line: 11750 openat(AT_FDCWD, "/etc/ld.so.cache", O_RDONLY|O_CLOEXEC) = 3</etc/ld.so.cache>
   - Expected Read file: "/etc/ld.so.cache", OK

## Write openat
   Line: 11750 openat(AT_FDCWD, "/tmp/ccvHeGYq.res", O_RDWR|O_CREAT|O_EXCL, 0600) = 3</tmp/ccvHeGYq.res>
   - Expected Write file: "/tmp/ccvHeGYq.res", OK

## Dir openat 
   Line: 2918  openat(AT_FDCWD, "./site/about", O_RDONLY|O_NOCTTY|O_NONBLOCK|O_NOFOLLOW|O_CLOEXEC|O_DIRECTORY) = 3</home/lionel/Proj/smk/tests/mysite/site/about>
   - Expected Read file: "/home/lionel/Proj/smk/tests/mysite/site/about", OK

## Dir openat without AT_FDCWD
   Line: 904   openat(5</home/lionel/Proj/smk/tests/12_mp3_conversions_tests>, "dir1", O_RDONLY|O_NOCTTY|O_NONBLOCK|O_NOFOLLOW|O_CLOEXEC|O_DIRECTORY) = 6</home/lionel/Proj/smk/tests/12_mp3_conversions_tests/dir1>
   - Expected Read file: "/home/lionel/Proj/smk/tests/12_mp3_conversions_tests/dir1", OK

## Access Error (EACCES)
   Line: 25242 mkdir("/usr/lib/python3/dist-packages/click/__pycache__", 0777) = -1 EACCES (Permission denied)
   - Expected Call_Type: "ignored", OK

## File not found (ENOENT)
   Line: 11751 openat(AT_FDCWD, "/tmp/ccQ493FX.ld", O_RDONLY) = -1 ENOENT (No such file or directory)
   - Expected Call_Type: "ignored", OK

## access for exec
   Line: 11750 access("/opt/GNAT/2018/bin/gcc", X_OK) = 0
   - Expected Call_Type: "Ignored", OK

## access file with no dir
   Line: 11750 access("hello.o", F_OK)           = 0
   - Expected Call_Type: "Ignored", OK

## RW access to a dir
   Line: 11750 access("/tmp", R_OK|W_OK|X_OK)    = 0
   - Expected Call_Type: "Ignored", OK

## unlink (rm)
   Line: 11750 unlink("/tmp/ccvHeGYq.res")       = 0
   - Expected Call_Type: "Ignored", OK

## unlinkat AT_REMOVEDIR
   Line: 29164 unlinkat(AT_FDCWD, "./site/about", AT_REMOVEDIR) = 0
   - Expected Call_Type: "Ignored", OK

## Set a current directory for process 30461
   Line: 30461 getcwd("/dir1/dir2", 4096) = 36
   - Expected Call_Type: "ignored", OK

## Set a current directory for process 15232 with final /
   Line: 15232 getcwd("/dir3/dir4/", 4096) = 36
   - Expected Call_Type: "ignored", OK

## Read AND Write test
   Line: 30461 rename("x.mp3", "unknown-unknown.mp3") = 0
   - Expected Source file: "/dir1/dir2/x.mp3", OK
   - Expected Target file: "/dir1/dir2/unknown-unknown.mp3", OK

## Rename with two AT_FDCWD
   Line: 15232 renameat2(AT_FDCWD, "all.filecount.new", AT_FDCWD, "all.filecount", RENAME_NOREPLACE) = 0
   - Expected Source file: "/dir3/dir4/all.filecount.new", OK
   - Expected Target file: "/dir3/dir4/all.filecount", OK

## renameat with explicit dir (and not AT_FDCWD), with and without final /
   Line: 15165 renameat(5</home/lionel/.slocdata>, "old", 5</home/lionel/.slocdata/>, "new")...
   - Expected Source file: "/home/lionel/.slocdata/old", OK
   - Expected Target file: "/home/lionel/.slocdata/new", OK

All tests OK [Successful](tests_status.md#successful)

# Tutorial


 This test ensure that the current version  
 of smk behave as described in the tutorial  

##  Tutorial / start conversion


  This is the "Quick Start" part of the tutorial  

  Run:  
  # converting ogg to mp3:  
  `sox x.ogg x.mp3`  

  Expected:  
```  
run "sox x.ogg x.mp3" because it was not run before
sox x.ogg x.mp3
```  

  # setting Artist and Tittle tags:  
  `id3v2 -a Luke -t Sentinelle x.mp3`  

  Expected:  
```  
run "id3v2 -a Luke -t Sentinelle x.mp3" because it was not run before
id3v2 -a Luke -t Sentinelle x.mp3
```  

  # renaming according to tags:  
  `id3ren -quiet -template=%a  

  Expected:  
```  
run "id3ren -quiet -template=%a-%s.mp3 x.mp3" because it was not run before
id3ren -quiet -template=%a-%s.mp3 x.mp3
```  


Tutorial / start conversion [Successful](tests_status.md#successful)

##  Tutorial / second run


  Run:  
  `smk`  

  Expected: nothing, situation is up to date  
```  
Nothing to run
```  


Tutorial / second run [Successful](tests_status.md#successful)

##  Tutorial / smk do not rebuild if a target is missing!!!

  Run:  
  `rm Luke-Sentinelle.mp3`  
  `smk`  

  Expected:  
```  
Nothing to run
```  


Tutorial / smk do not rebuild if a target is missing!!! [Successful](tests_status.md#successful)

##  Tutorial / unless using the `-mt` / `--build-missing-target` option

  Run:  
  `smk -mt -e`  

  Expected:  
```  
run "sox x.ogg x.mp3" because Target file x.mp3 is missing
sox x.ogg x.mp3
run "id3v2 -a Luke -t Sentinelle x.mp3" because Source file x.mp3 has been updated (YYYY:MM:DD HH:MM:SS.SS)
id3v2 -a Luke -t Sentinelle x.mp3
run "id3ren -quiet -template=%a-%s.mp3 x.mp3" because Target file Luke-Sentinelle.mp3 is missing
id3ren -quiet -template=%a-%s.mp3 x.mp3
```  


Tutorial / unless using the `-mt` / `--build-missing-target` option [Successful](tests_status.md#successful)

##  Tutorial / touch x.ogg

  Run:  
  `touch x.ogg`  
  `rm Luke-Sentinelle.mp3`  
  `smk`  

  Expected:  
```  
run "sox x.ogg x.mp3" because Source file x.ogg has been updated (YYYY:MM:DD HH:MM:SS.SS)
sox x.ogg x.mp3
run "id3v2 -a Luke -t Sentinelle x.mp3" because Source file x.mp3 has been updated (YYYY:MM:DD HH:MM:SS.SS)
id3v2 -a Luke -t Sentinelle x.mp3
run "id3ren -quiet -template=%a-%s.mp3 x.mp3" because Source file x.mp3 is present
id3ren -quiet -template=%a-%s.mp3 x.mp3
```  


##  Tutorial / smk do rebuild if you give the target

  Run:  
  `smk lt -l > out.20`  
  `rm Luke-Sentinelle.mp3`  
  `smk Luke-Sentinelle.mp3`  

  Expected:  
```  
"id3ren -quiet -template=%a-%s.mp3 x.mp3" [] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] Luke-Sentinelle.mp3
"id3v2 -a Luke -t Sentinelle x.mp3" [] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] x.mp3
"sox x.ogg x.mp3" [] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] x.mp3
id3ren -quiet -template=%a-%s.mp3 x.mp3
```  


Tutorial / smk do rebuild if you give the target [Successful](tests_status.md#successful)

# Directory tests



##  Directory tests / mkdir dir1


  Run:  
  `smk run mkdir dir1`  

  Expected:  
```  
mkdir dir1
```  

  Run:  
  `smk st -l`  

  Expected:  
```  
Command "mkdir dir1", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (0)
  Targets: (1)
  - [If absence ] [Dir] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] dir1

```  

  Run:  
  `smk ls`  

  Expected:  
```  
```  

  Run:  
  `smk lt`  

  Expected:  
```  
dir1
```  

  Run:  
  `smk lu`  

  Expected:  
```  
```  

  Run:  
  `smk wn`  

  Expected:  
```  
Nothing new
```  


Directory tests / mkdir dir1 [Successful](tests_status.md#successful)

##  Directory tests / updating dir1


  Run:  
  `smk run touch dir1/f1`  

  Expected:  
```  
touch dir1/f1
```  

  Run:  
  `smk st -l`  

  Expected:  
```  
Command "mkdir dir1", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (0)
  Targets: (1)
  - [If absence ] [Dir] [Normal] [Target] [Updated] [YYYY:MM:DD HH:MM:SS.SS] dir1

Command "touch dir1/f1", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (0)
  Targets: (1)
  - [If absence ] [Fil] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] dir1/f1

```  

  Run:  
  `smk ls`  

  Expected:  
```  
```  

  Run:  
  `smk lt`  

  Expected:  
```  
dir1
dir1/f1
```  

  Run:  
  `smk lu`  

  Expected:  
```  
```  

  Run:  
  `smk wn`  

  Expected:  
```  
[Updated] [Target] dir1
```  

  Run:  
  `smk`  

  Expected:  
```  
Nothing to run
```  

  Run:  
  `touch dir1`  

  Expected:  
  It's a target, so touching it should not run the command  
```  
Nothing to run
```  


Directory tests / updating dir1 [Successful](tests_status.md#successful)

##  Directory tests / cleaning dir1


  This test check that smk correctly identify target files,  
  and remove it with "clean", and preserve "unused" files,  
  even if those are in a target dir.  

  Run:  
  `smk run mkdir dir1`  
  `smk run touch dir1/f1`  
  `smk run mkdir dir2`  
  `smk run mv dir1/* dir2`  
  `touch dir1/f5`  
  `mkdir dir2/dir3`  
  `smk st`  

  Expected:  
```  
Command "mkdir dir1", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (0)
  Targets: (1)
  - dir1

Command "mkdir dir2", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (0)
  Targets: (1)
  - dir2

Command "mv dir1/f1 dir2", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (1)
  - dir1/f1
  Targets: (2)
  - dir2
  - dir2/f1

Command "touch dir1/f1", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (0)
  Targets: (1)
  - dir1/f1

```  

  Run:  
  `smk lt -l`  
  (files that should be erased when cleaning)  

  Expected:  
```  
"mkdir dir1" [] [If absence ] [Dir] [Normal] [Target] [Updated] [YYYY:MM:DD HH:MM:SS.SS] dir1
"mkdir dir2" [] [If absence ] [Dir] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] dir2
"mv dir1/f1 dir2" [] [If absence ] [Dir] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] dir2
"mv dir1/f1 dir2" [] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] dir2/f1
"touch dir1/f1" [] [If absence ] [Fil] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] dir1/f1
```  

  Run:  
  `smk lu -l`  
  (files that should not be erased when cleaning)  

  Expected:  
```  
/home/lionel/Proj/smk/tests/16_dir_ops_tests/dir1/f5
/home/lionel/Proj/smk/tests/16_dir_ops_tests/dir2/dir3
```  

  Run:  
  `smk clean`  

  Expected:  
```  
Deleting file dir2/f1
Deleting dir dir1
Deleting dir dir2
Deleting dir dir2
```  


Directory tests / cleaning dir1 [Successful](tests_status.md#successful)

##  Directory tests / accessing dir1 contents, write access

  Run:  
  `mkdir -p dir1`  
  `smk run mkdir dir1/dir2`  
  `smk st -l`  

  Expected:  
```  
Command "mkdir dir1/dir2", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (0)
  Targets: (1)
  - [If absence ] [Dir] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] dir1/dir2

```  

  Run:  
  `smk wn`  

  Expected:  
```  
Nothing new
```  

  Run:  
  `smk`  

  Expected:  
```  
Nothing to run
```  

  Run:  
  `touch dir1/f2`  
  `smk wn`  
  nothing expected as dir1 is not involved in a known command  

  Expected:  
```  
Nothing new
```  

  Run:  
  `touch dir1/dir2/f3`  
  `smk wn`  
  dir2 update should be reported, as dir2 is involved in a known command  

  Expected:  
```  
[Updated] [Target] dir1/dir2
```  

  but there is nothing to run  
  Run:  
  `smk`  

  Expected:  
```  
Nothing to run
```  


##  Directory tests / accessing dir1 contents, read access

  `let s now add a command reading dir1`  
  Run:  
  `smk run ls -1 dir1`  

  Expected:  
```  
ls -1 dir1
dir2
f2

```  

  `if nothing changes, nothing to run`  
  Run:  
  `smk`  

  Expected:  
```  
Nothing to run
```  

  `but if we add a file in dir1, ls should re-run`  
  Run:  
  `touch dir1/f4`  
  `smk -e`  

  Expected:  
```  
run "ls -1 dir1" because Source dir dir1 has been updated (YYYY:MM:DD HH:MM:SS.SS)
ls -1 dir1
dir2
f2
f4

```  


Directory tests / accessing dir1 contents, read access [Successful](tests_status.md#successful)

##  Directory tests / removing dir1

  Run:  
  `rm -rf dir1`  

  Expected:  
```  
[Missing] [Target] dir1
[Missing] [Target] dir1/dir2
[Missing] [Target] dir1/f1
```  

  Run:  
  `smk -e`  

  Expected:  
  Missing target, should not run without -mt  
```  
Nothing to run
```  

  Run:  
  `smk -e -mt`  

  Expected:  
  should cause the command to rebuild missing targets  
```  
run "mkdir dir1" because Target dir dir1 is missing
mkdir dir1
run "touch dir1/f1" because Target file dir1/f1 is missing
touch dir1/f1
run "mkdir dir1/dir2" because Target dir dir1/dir2 is missing
mkdir dir1/dir2
```  


Directory tests / removing dir1 [Successful](tests_status.md#successful)
