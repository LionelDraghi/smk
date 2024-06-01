[smk (SmartMake)](http://lionel.draghi.free.fr/smk/index.html)
==============================================================



**`smk` is a devastatingly simple and powerful make. So simple that you don't even have to write a Makefile.  
Just run your commands once through `smk`, and then, all you'll have to type is `smk`!  
`smk` will automatically check the modifications in the file system relevant for each command, and run only what have to!** 

------------------------------------------------------------------------
![](img/version.svg) ![](img/tests_ok.svg) ![](img/tests_ko.svg)


Table of contents  
- [smk (SmartMake)](#smk-smartmake)
  - [Quick start](#quick-start)
  - [How is this possible?](#how-is-this-possible)
  - [Overview](#overview)
  - [A little more on usage](#a-little-more-on-usage)
  - [Cool features TBD](#cool-features-tbd)
    - [using `smk` without `smkfile`](#using-smk-without-smkfile)
    - [Whats new?](#whats-new)
    - [Auto-clean](#auto-clean)
    - [Working with files: smkfiles, Makefiles or shell scripts](#working-with-files-smkfiles-makefiles-or-shell-scripts)
  - [Next Step:](#next-step)
  - [Downloading and building](#downloading-and-building)
  - [Further reading](#further-reading)
  
------------------------------------------------------------------------

(Une introduction en français est disponible  [ici](https://linuxfr.org/users/3tus/journaux/smk-un-make-sans-makefile))

## Overview & main features

`smk` is a make tools aiming at maximum simplicity. You don't need to learn a specific syntax to use it, and you don't even have to write a proper Makefile. Run the commands under `smk` once, just run `smk` to replay only the commands that need to.
`smk` :
- stores the commands in simple text files. You don't need to manipulate it, but you also can edit those file. You can use simple existing script files or Makefiles;
- don't need you to describe preconditions, sources, target, rules, etc. Quite the opposite, just run the command, and then ask `smk` what are the inputs and ouputs;
- tracks for you unused files;
- provides a whatsnew function, similar to git `status`;
- provides an automatic clean function;
- can build whatever output files (all output files are targets you can give on the command line).

## Quick start

Let's start with a basic example : converting an audio file, changing tags, and renaming it according to the tag.  
Here is my `script.sh` file that will transform the `x.ogg` file into `Luke-Sentinelle.mp3`.  
It contains:

```bash
# converting ogg to mp3:
sox x.ogg x.mp3
# setting Artist and Tittle tags:
id3v2 -a Luke -t Sentinelle x.mp3
# renaming according to tags:
id3ren -template='%a-%s.mp3' x.mp3
```

You may run this through `smk`, either directly with 

```bash
smk script.sh
```

or by giving commands one by one, on the fly, thanks to the `run` command:

```bash
smk run sox x.ogg x.mp3
smk run id3v2 -a Luke -t Sentinelle x.mp3
smk run id3ren -quiet -template='%a-%s.mp3' x.mp3
```

Note that this test case, yet simple, is already fairly representative:  
- the first command is reading an input file and creating an output file,  
- the second has the same file as input and output,  
- and the third is "moving" a file, that is deleting the input file.  

The ugly part of of the work is over, now all you have to do to get the things done is: 

```bash
smk
```

The result will be:

> Nothing to run  

But if you modify an involved file:  

```bash
touch x.ogg
rm Luke-Sentinelle.mp3
smk 
```

> sox x.ogg x.mp3  
> id3v2 -a Luke -t Sentinelle x.mp3  
> id3ren -quiet -template=%a-%s.mp3 x.mp3  

For more information on why is a command run, just give the `--explain` option (`-e` in short form).  


## How is this possible? 

`smk` uses [`strace`](https://en.wikipedia.org/wiki/Strace), to understand what is read and what is written by each command, and thus what is a source, and what is a target.  
And this is why you don't have to manually give those information within a Makefile : **no more rules, recipes, dependencies, targets, etc.**

## `smk` vocabulary 

`smk` vocabulary is simple:  

- **Command** : commands are whatever can be run in the shell, from the simple `gcc` command above to script, or `make whatever`.

- **smkfile** : an smkfile is text file containing commands. It's the closest thing in `smk` to Makefile.  
The main differences are that smkfiles are simple : no cryptic syntax to describe rules or recipes, you don't need it. Both previous examples are valid smkfiles (yes, even the Makefile, `smk` can read very simple Makefile).  
And above all, smkfiles are not mandatory to use `smk`! 

- **Run** : a run is an execution of a command, or a list of command through an smkfile. This is what `smk` analyzes thanks to strace.

- **runfile** : runfiles are the internal files created by `smk` to store information about the various run, that is mainly : executed commands, when, source and target files and related time stamps. 

## A little more on usage

Check what `smk` stored from the previous run:
```
smk status --long-listing
```

Note that common commands (here *status*), and options (here *--long-listing*) exist in short form. A strict equivalent is:
```
smk st -l
```

(More on commands and options with the usual `smk -h`)

Let's be back to the status:
> 2018-12-25 03:09:54.99 [] "sox x.mp3 x.ogg"  
>  Sources (1) :  
>  - 2018-12-25 02:06:10.00:/home/lionel/Proj/smk/tests/15_mp3_conversions/x.mp3  
>  Targets (1) :  
>  - 2018-12-25 03:09:59.00:/home/lionel/Proj/smk/tests/15_mp3_conversions/x.ogg  

The first time stamp is the command run time, other time stamps are files modification time.  
(and yes, this example was written during christmas night!)

> Note that `smk` output focus on useful files, system files are filtered by default.  
> (On my system, 166 system files are read by ffmpeg during such a conversion, that you generally don't care!)  
> Note that this is only an output filter. `smk` manage all dependencies, and if whatever lib.so changes in /usr/lib, the command will be re-run.  
> To see system files involved, add the `-sa` (`--show-all-files`) option. 

And now:
```
touch x.mp3
smk -e
```
(`-e` stands for `--explain`)

> run "ffmpeg -i x.mp3 x.ogg" because /home/lionel/Proj/smk/tests/15_mp3_conversions/x.mp3 (2018-12-25 03:18:28.00) has been updated since last run (2018-12-25 02:06:10.00)  
ffmpeg -i x.mp3 x.ogg

etc.

## Next Step:
A short, but highly recommended, [`smk` tutorial](tutorial.md) is the best way to quickly start.  

And don't forget [`smk help`](cmd_line.md).

## Downloading and building

1. First, get the sources
   
   - Either download it on [the GitHub project page](https://github.com/LionelDraghi/smk)  

   - or directly with `git`:  
     > `git clone https://github.com/LionelDraghi/smk.git`

2. Go to the created `smk` directory

3. Ensure you have a gnat compiler (gcc Ada)  
   On Debian family:  
   >  `apt install gnat make`

   Or get the [GNAT Community Edition](https://www.adacore.com/download).

4. Build it:  
   > `gprbuild -p -P smk.gpr`


## Further reading

* [More on the `smkfile` format](smkfile_format.md)

* [Not sure to understand what is the difference with `make`...](compare_with_make.md)

* Documentation
  - [Tutorial](tutorial.md)
  - [Command line](cmd_line.md)
  - [Tests](tests/testrec.md)
  - [Limitations and bugs](limitations.md)

* Design:
  - [Design notes](design_notes.md)
  - [Contributing](contributing.md)

* Current Status:
  - [Build Dashboard](dashboard.md)
  - [Tests status](tests/tests_status.md)
  - [Fixme](fixme.md)
  - [Changelog](changelog.md)

* [About](about.md)

-------------------------------------------------------------------------- 
**`smk`** is released under 

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)  

**but is no more under active development.**  

Comments and issues are very welcome [here](https://github.com/LionelDraghi/smk/issues/new)!

Lionel
