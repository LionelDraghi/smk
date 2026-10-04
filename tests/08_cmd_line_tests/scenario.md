# Command line

Test the command line analysis: help, version, and error cases.

_Table of Contents:_
- [Scenario: help options](#scenario-help-options)
- [Scenario: version option](#scenario-version-option)
- [Scenario: illegal command lines](#scenario-illegal-command-lines)
- [Scenario: option given after a command](#scenario-option-given-after-a-command)
- [Scenario: unknown smkfile](#scenario-unknown-smkfile)

## Scenario : help options

Test that the `-h` and `help` outputs are the same:

- When I run `sh -c "../../smk -h > out.help1.txt"`
- When I run `sh -c "../../smk help > out.help2.txt"`
- Then the file `out.help1.txt` is equal to file `out.help2.txt`

## Scenario : version option

Test that the version command displays the crate version, as defined
in the Alire manifest:

- Given I run `sh -c "grep ^version ../../alire.toml | cut -d'\"' -f2 > out.expected_version.txt"`
- When I run `../../smk version`
- Then the output is equal to file `out.expected_version.txt`

## Scenario : illegal command lines

More than one command is an error. The full error message, including the
usage and help, is compared to the `expected_wrong_cmd_line1.txt` golden file:

- When I run `sh -c "../../smk read-smkfile status > out.wrong_cmd_line1.txt 2>&1"`
- Then the file `out.wrong_cmd_line1.txt` is equal to file `expected_wrong_cmd_line1.txt`
- Then I get error

## Scenario : option given after a command

An option given after the command is ignored:

- When I run `../../smk reset -l`
- Then there is no output

## Scenario : unknown smkfile

Test the error message if an unknown smkfile is given:

- When I run `../../smk My_Makefile`
- Then I get `Error : No smkfile given, and no existing runfile in dir`
- Then I get error
