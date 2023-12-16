---
title: "Mainstreaming Race Science"
author: "Daniel J. Hicks, Emilio Lobato"
email: <dhicks4@ucmerced.edu>
---

This repository contains the preprocessed analysis data and code for the paper "Mainstreaming Race Science" by Daniel J. Hicks and Emilio Lobato.  

Reproducing this analysis requires [R](https://cran.r-project.org/index.html) version 4.1 and [renv version 0.15.2](https://cran.r-project.org/src/contrib/Archive/renv/).  We also recommend using GNU make, which should be installed automatically with the OS on Unix-based systems (including macOS and Linux).  

Once these prerequisites are installed: 

1. In R, install [`tmfast` version 0.0.0.2023-04-15](https://github.com/dhicks/tmfast/releases/tag/0.0.0.2023-04-15)
    - From the command line, this can be done using `devtools` using `Rscript -e "devtools::install_github('dhicks/tmfast@0.0.0.2023-04-15')"`. 
    - This step will install the package used for vocabulary selection and topic modeling. 

2. In R, from the top folder of the project, run `renv::restore()`
    - From the command line, this can be done using `Rscript -e "renv::restore()"`. 
    - This step will install all dependencies for the entire analysis pipeline, using the versions used in the original analysis. 
    - Note that this may require compiling packages from source, which can take a significant amount of time. 

3. Run the analysis scripts in the `scripts` folder in numerical order.  
    - If GNU make is installed, this can be done from the command line using `make` or `make pipe` in either the top-level folder or the `scripts` folder. 
    
4. Optional: Recreating the manuscript has further prerequisites.  See the README in the `paper` folder. 

# License

Shield: [![CC BY-SA 4.0][cc-by-sa-shield]][cc-by-sa]

This work is licensed under a
[Creative Commons Attribution-ShareAlike 4.0 International License][cc-by-sa].

[![CC BY-SA 4.0][cc-by-sa-image]][cc-by-sa]

[cc-by-sa]: http://creativecommons.org/licenses/by-sa/4.0/
[cc-by-sa-image]: https://licensebuttons.net/l/by-sa/4.0/88x31.png
[cc-by-sa-shield]: https://img.shields.io/badge/License-CC%20BY--SA%204.0-lightgrey.svg
