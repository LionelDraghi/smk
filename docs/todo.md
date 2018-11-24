# Options of make and mk, for inspiration...

- whatif option




## https://9fans.github.io/plan9port/man/man1/mk.html
--------------------------------------------------

'''

−a      Assume all targets to be out of date. Thus, everything is updated.

−d[egp]Produce debugging output (p is for parsing, g for graph building, e for execution).

−e      Explain why each target is made.

−i      Force any missing intermediate targets to be made.

−k      Do as much work as possible in the face of errors.

−n      Print, but do not execute, the commands needed to update the targets.

−s      Make the command line arguments sequentially rather than in parallel.

−t      Touch (update the modified date of) file targets, without executing any recipes.

−wtarget1,target2,...
Pretend the modify time for each target is the current time; useful in conjunction with −n to learn what updates would be triggered by modifying the targets.

'''

## https://linux.die.net/man/1/make
--------------------------------

'''

-B, --always-make
Unconditionally make all targets.

-d
Print debugging information in addition to normal processing. The debugging information says which files are being considered for remaking, which file-times are being compared and with what results, which files actually need to be remade, which implicit rules are considered and which are applied---everything interesting about how make decides what to do.

-i, --ignore-errors
Ignore all errors in commands executed to remake files.

-k, --keep-going
Continue as much as possible after an error. While the target that failed, and those that depend on it, cannot be remade, the other dependencies of these targets can be processed all the same.

-n, --just-print, --dry-run, --recon
Print the commands that would be executed, but do not execute them.

-q, --question
''Question mode''. Do not run any commands, or print anything; just return an exit status that is zero if the specified targets are already up to date, nonzero otherwise.

-s, --silent, --quiet
Silent operation; do not print the commands as they are executed.

-S, --no-keep-going, --stop
Cancel the effect of the -k option. This is never necessary except in a recursive make where

-v, --version
Print the version of the make program plus a copyright, a list of authors and a notice that there is no warranty.

-W file, --what-if=file, --new-file=file, --assume-new=file
Pretend that the target file has just been modified. When used with the -n flag, this shows you what would happen if you were to modify that file. Without -n, it is almost the same as running a touch command on the given file before running make, except that the modification time is changed only in the imagination of make.
'''