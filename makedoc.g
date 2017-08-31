#!/usr/bin/env gap
LoadPackage("GAPDoc");
LoadPackage("AutoDoc");
SetGapDocLaTeXOptions(rec(EarlyExtraPreamble := "\\usepackage{amsopn}\n"));
AutoDoc(rec(scaffold := true, autodoc := true));
QUIT;
