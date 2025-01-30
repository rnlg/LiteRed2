# **LiteRed2** MATHEMATICA package

***LiteRed2*** is conceived as an essential update of the LiteRed package by the same author (Roman Lee). Its purpose is the IBP reduction of the multiloop diagrams. 

Please, check [Discussions](https://github.com/rnlg/LiteRed2/discussions) for announcements and feedback.

### Installation

1. Copy the content of the 'Source/' directory to the desired location, say `home/of/LiteRed2`
2. Change to this location with `cd home/of/LiteRed2`
3. Run `math -script makeShortcut.m` (for version 14.1 and later use `wolfram -script makeShortcut.m`).

Result: 
You can load LiteRed2 package from Mathematica session with ``<<LiteRed2` ``

### Additional tools

##### Using Mint package

***Mint*** package is described in [JHEP11 (2013) 165](https://doi.org/10.1007/JHEP11(2013)165). It counts the number of master integrals in a given sector. It is distributed together with ***LiteRed2***, file *source/RNL/Mint.m*. This file should be copied into \$UserBaseDirectory/Applications either manually or via running `math -script installMint.m`. When ***Mint*** is installed, it should be loaded via `` <<Mint` `` <u>after</u>  ***LiteRed2*** is loaded (as it detects the presence of ***LiteRed2*** and adjusts its procedures correspondingly). Then ***Mint*** can be used via option `NMIs->Automatic` of `SolvejSector` procedure.

##### Using Fermat CAS

LiteRed2 can benefit from using Robert H. Lewis' [Fermat CAS](http://home.bway.net/lewis/). The interface package ***Fermatica*** can be downloaded from [Fermatica repository](https://github.com/rnlg/Fermatica). When ***Fermatica*** is installed and working, it can be loaded along with ***LiteRed2***  via `` <<Fermatica` `` and used via  option `UseFermat->True` of `SolvejSector`  procedure.
