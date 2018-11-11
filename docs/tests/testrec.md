
# Smk test suite



 This test check that illegal command lines cause archicheck to
 exit with a non null return code.
 - Note that normal use is overly tested in other tests,
   so here mainly error situations are tested.
 - Note also that quiet and verbose mode (-q / -v) are also tested
   in other tests.


##  Smk test suite / Help options


  Test that the -h, --help or no command line will output :

```

Usage : smk [Options] make_file

Options :
   -We | --Warnings=error : treat warnings as errors
   -v  | --verbose
   -q  | --quiet          : no message unless error.
                            Warning are also ignored.
         --version        : smk version
   -h  | --help           : this message

https://github.com/LionelDraghi/smk

```


Smk test suite / Help options [Successful](tests_status.md#successful)

##  Smk test suite / Version option


  Test that the --version will put :

```
0.0.1
```


Smk test suite / Version option [Successful](tests_status.md#successful)

##  Smk test suite / Unknow Makefile


  Test the error message if an unknow MakeFile is given

```
Error : Unknown Makefile or unknow option My_Makefile
```


Smk test suite / Unknow Makefile [Successful](tests_status.md#successful)
