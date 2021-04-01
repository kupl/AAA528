# HW4 - AAA528 2021S

## Table of Contents

- [HW4 - AAA528 2021S](#hw4---aaa528-2021s)
  - [Table of Contents](#table-of-contents)
  - [Target](#target)
    - [Benchmarks](#benchmarks)
  - [Usage](#usage)
    - [Build](#build)
    - [Run](#run)
      - [Options](#options)
    - [Logging](#logging)
  - [Reference](#reference)
  - [Contact](#contact)

## Target

The implementation target is [`hw4.ml`](./src/hw4/hw4.ml).
Verifier starts verification at the [`Hw4.verify`](https://github.com/kupl/AAA528/blob/main/hw4/src/hw4/hw4.ml#L13) function.
If you edit other files, please notify it in your submission.

### Benchmarks

Benchmarks for this problem is in [benchmarks](./benchmarks) directory.
They are separated by the target correctness of verification.

## Usage

### Build

```bash
$ make          # CASE 1: Use Makefile
$ dune build    # CASE 2: Use dune build
Done: 250/256
```

The executable binary file will be placed `_build/install/default/bin/aaa528_hw4` after the building program.

### Run

```bash
$ ./build/install/default/bin/aaa528_hw4 --input [FILE_PATH] [options]  # CASE 1: Execute binary file
$ dune exec -- aaa528_hw4 --input [FILE_PATH] [options]                 # CASE 2: Use dune execution
No buffer-overflow error has been detected.
```

#### Options

```txt
  --input           File path for input simple c program
  --verbose         Verbose log mode
  --z3-timeout      Timebudget for Z3 solver - default - 30s
  --total-timeout   Timebudget for program (if time is over, the program will be halt) - default: 180s
  --print-arg       Print out arguments of this process
  --print-adt       Print out ADT form of input program
  -help             Display this list of options
  --help            Display this list of options
```

### Logging

When you need to log the state, please using log module [`Utils.Log`](./utils/log.ml).

``` ocaml
(* Log Level: Application *)
# Utils.Log.app (fun m -> m "%d: %s level logging." 1 "Application")
- : string = "1: Application level logging." 
             (* Print result of application to STDOUT *)

(* Log Level: Infomation *)
# Utils.Log.info (fun m -> m "%d: %s level logging" 2 "Information")
- : string = "[INFO][+000001ms] 2: Application level logging."
             (* Print formatted string with tag to STDERR when verbose mode *)

(* Log Level: Warning *)
# Utils.Log.warn (fun m -> m "%d: %s level logging" 3 "Warning")
- : string = "[WARN][+000001ms] 3: Warning level logging."
             (* Print formatted string with tag to STDERR *)

(* Log Level: Error *)
# Utils.Log.err (fun m -> m "%d: %s level logging" 4 "Error")
- : string = "[ERROR][+000001ms] 4: Error level logging."
             (* Print formatted string with tag to STDERR *)
```

## Reference

- [Z3 ML Documentation](https://z3prover.github.io/api/html/ml/Z3.html)

## Contact

If there is any problem or question, please contact to [TA](mailto:jormal@korea.edu).
