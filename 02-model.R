#  Topic modelling of "panarchy" data
#  source data: scopus
#  date: 20201114
#  Juan Rocha
#
# libraries for modeling
library(tidytext)
library(tidyverse)
library(tm)
library (topicmodels)
library(lda)
library(tictoc)
load("data/dtm.Rdata")

SEED <- 2010

## Choosing best algorithm
k <- 10
tic()
tset.TM <- list (
    VEM0 = LDA(dtm, k=k, control = list ( seed = SEED)),
    VEM_fixed= LDA(dtm, k=k, control= list (estimate.alpha = F, seed = SEED)),
    Gibbs = LDA (dtm, k=k, method ="Gibbs", control = list (seed = SEED, burnin= 1000, thin = 100, iter= 1000)),
    CTM = CTM (dtm, k=k, control = list(seed = SEED, var= list (tol= 10^-4), em= list (tol = 10^-3))))
toc() # 164s
sapply (tset.TM[1:3], slot, "alpha")

#Finding number of topics
k <- c(5,10,25,50,100)
tic()
topicNumber.TM <- map(
    .x = k,
    .f = function(x) {
    LDA(dtm, k = x, control= list (seed = SEED), method = "Gibbs")
    })
toc() # 314s

save(tset.TM, topicNumber.TM, file = "data/models_gibbs.RData")

#### Lance Gunderson pubs ####

lance <- bib2df::bib2df(
    file = "data/scopus_gunderson_201121.bib",
    separate_names = TRUE) %>%
    janitor::clean_names()

lance %>% pull(doi) %>% unique()
