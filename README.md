# Code from Racketfest 2019

## Installing `minijazyk` as a package

To run the code, you must install the minijazyk subdirectory as a
Racket package. Go to the directory containing this file and run the
following command:

    raco pkg install --link ./minijazyk

That installs the package and compiles its contents. The `--link`
argument means to remember the directory as the source of the
package. If you modify or add files in the directory, you do *not*
need to re-install the package. (You can run `raco setup minijazyk` to
recompile the files, which decreases loading time but usually isn't
necessary.)

## Running an example

To run an example (which usually has the effect of printing the
jazyk's "phrasebook"), run the `racket` executable on the module file
with the example. For example:

    racket ./minijazyk/ex-v1.rkt

## Organization of the source files

There are four versions of the code in the `minijazyk` directory. They
implement slightly different languages (with different features and
behavior). The changelog comment at the bottom of each module gives a
brief explanation of the difference from the previous version.

- `minijazyk/v0.rkt` -- version 0 of the code, which just defines `jazyk`
  as a macro (which relies on the run-time support code in
  `minijazyk/run-time.rkt`)
- `minijazyk/ex-v0.rkt` -- a runnable example using the code from
  `minijazyk/v0.rkt`
- and so on for `v1` through `v4`
