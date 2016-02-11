
# A template for my own documents

This folder holds a template for short document, ready
to be compiled with `texlive`. It is modular and
simple changes to configuration files allow generalization:

  - change personal info within `preamble/newcommands.tex`
  - customize document structure modifying file in `preamble` folder
  - add your content in dedicated files under `chapters` folder
    and add a corresponding `\input` command within `structure/chapters.tex`,
    taking it apart in dedicated files for sections
  - add your graphics and pictures within `gfx` folder   

`Makefile` provides rules for pdf compilation, bibtex compilation
and cleaning. Moreover, a rule for an initial `git add` command is
provided.

