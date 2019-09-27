# Functional Programming and Verification (IN0003)

## Setup
We provide a virtual machine where everything is already set up, there's also a video on how to use it (see Moodle).

If you don't want to use the virtual machine, run `./setup.sh` (Debian/Ubuntu/WSL, replace your package manager, e.g., `brew` on macOS).
This makes sure that you have [OCaml](https://caml.inria.fr/pub/docs/manual-ocaml-4.07/) and some packages installed via [opam](https://opam.ocaml.org/).
As an editor we recommend using [Visual Studio Code](https://code.visualstudio.com/) with [extension](https://marketplace.visualstudio.com/items?itemName=freebroccolo.reasonml) (or [vim/emacs](https://github.com/ocaml/merlin#editor-setup) if you know it).
All Linux and macOS work, under Windows use [WSL](https://en.wikipedia.org/wiki/Windows_Subsystem_for_Linux) and [this extension](https://marketplace.visualstudio.com/items?itemName=raiscui.reasonml-wsl).

## Usage
Use `git pull` to get the new exercise files (and templates for the homework) every week (or more often if we need to fix something).
You can use `git log --patch` to keep track of changes.
If you use the provided virtual machine, there will be a little downward-arrow indicating that you should pull online changes.
If you want, you can push your solutions to a private repository (if you want to avoid merging, rename your solutions).
To submit your homework, upload your `.ml` file(s) to Moodle.

There will be some basic tests provided with the homeworks. For grading, more elaborate tests will be run on your submissions after the deadline.
You can test your homework by just executing the corresponding file, e.g., `ocaml hw05.ml` (set up as the default build task (`ctrl+shift+b`) in `.vscode/`).

## Resources
Slides are available on the [lecture website](https://www.in.tum.de/i02/lehre/wintersemester-1819/vorlesungen/functional-programming-and-verification/).
Use [Piazza](https://piazza.com/class/jnadp6tfu067ov) for questions!