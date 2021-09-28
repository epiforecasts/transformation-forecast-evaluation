#!bin/bash

# render the workflow
Rscript -e "rmarkdown::render('_targets.Rmd')" 

# run the workflow
Rscript -e "targets::tar_make()" 
