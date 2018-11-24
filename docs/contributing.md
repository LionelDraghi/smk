# Contributing

Comments and issues are welcome [here](https://github.com/LionelDraghi/smk/issues/new).  
Feel free to suggest whatever improvement.
I'm not a native english speaker. English improvements are welcome, in documentation as in the code.  
Use cases or features suggestions are also welcome.  

# submitting code / docs

To propose some patch          | git command
-------------------------------|-------------------------------------
 1. Fork the project           | fork button on https://github.com/LionelDraghi/Smk
 2. Clone your own copy        | `git clone https://github.com/your_user_name/Smk.git`
 3. Create your feature branch | `git checkout -b my-new-feature`    
 4. Commit your changes        | `git commit -am 'Add some feature'` 
 5. Push to the branch         | `git push origin my-new-feature`    
 6. Create new Pull Request    | on your GitHub fork, go to "Compare & pull request".

The project policy is that code shall be 100% covered (except debug or error specific lines).

Coverage is computed on each `make check`, and non covered code is easy to check with (for example) `chromium docs/lcov/index.html`.  

> **NB : please note that code proposed with matching tests, doc and complete coverage is very appreciated!**  

# Design Overview

Main Smk components are :

- **Smk.Main** procedure is the controller, in charge (with separate units) of running operation according to the command line analysis;

- **Smk.Settings** defines various application wide constant, and stores various parameters set on command line;

Fixme: **TBC**

# Tests Overview

The global intent is to have tests documenting the software behavior. Test execution result in a global count of passed/failed/empty tests, and in a text output in Markdown format, integrated in this documentation.

Tests are at exe level, with no unit testing at this stage. To have tests in Makefile behaving like unit test, I created a utility, called `testrec` to record tests execution. 
This utility is build before test execution, and record in a local testrec.md file : comments, test suite start, test start, assertion result, etc.
This is how the test documentation is created, and how I can compile a global tests results (like if it was a single Ada unit test), despite multiple Makefiles and executions.

Tests are defined in the `tests` dir, in a (hopfully) comprehensice way.
Note that most of the Makefile code aim at documentation production. 

A Test typically documents (order may vary) :

1. When running _this_ command, 
2. with _this_ Makefile,
3. and _those_ sources files or situation (details are not always printed),
4. I should have _this_ result (on standard output, but also on error output, and returned code)

Comments are created in Makefile thanks to `testrec cmt`. They are put in the testrec.md file, not on standard output. 
On standard output, only a line of the form  

> _Test suite name / test name [Successful]_  

is put, to keep Makefile execution clean.

Execution is typically :

1. at the beginning of the Makefile, `testrec create` is called to start a new test suite.
   NB : this is a convention, each Makefile run a single test suite.
2. Then, for each test, `testrec start` is called to start the test (and name it).
3. During the test, `testrec assert` is called at least once, generally to check that the output of Smk execution is equal to the expected output.
4. Finally, `testrec end` is called. `testrec` will then output the test result on standard output.

The test result may be : 

- _Successul_, if all Assert are verified,
- _Failed_, if at least one is not,
- and _Empty_, if no Assert is called between test start and test end. This is usefull when starting to write a test in the Makefile before code exists : it wouldn't be fair to have those test "Failed".

After all tests execution, `testrec clean` is called to remove the hidden file that stores intermediate results and state, and the `testrec.md` file is moved in the docs directory under a name matching the test suite name, where it will be automatically taken into account.