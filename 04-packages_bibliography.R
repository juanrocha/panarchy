## This are the key packages used in the analysis:

library(tidytext)
library(tidyverse)
library(tm)
library (topicmodels)
library(lda)

pkgs <- c("tidytext", "tidyverse", "tm", "topicmodels", "lda") 

## automatically create a bib datasbase of R packages:
knitr::write_bib( c(.packages(), pkgs), "packages.bib")
