#  Topic modelling of "panarchy" data
#  source data: scopus
#  date: 20201114
#  Juan Rocha
#  
# libraries for modeling
library(tidytext)
library(tidyverse)



files <- list.files("data/") %>% str_subset(pattern = ".bib")
files <- paste0("data/", files) 

dat <- map(.x = files, .f = function(x){
    x %>% 
        bib2df::bib2df(separate_names = TRUE) %>%
        janitor::clean_names()
})

dat <- bind_rows(dat)

dat ## contains 2796 entries, of which 2357 are unique.

dat %>% select(category, bibtexkey, author, journal, key, title, year, doi, abstract, coden, document_type) %>% unique()

## Deciding which selection to use: needs to be unique and have an abstract
dat %>% pull(bibtexkey) %>% unique() %>%  length() # 2357
dat %>% pull(doi) %>% unique() %>%  length() # 2142
dat %>% unique() # 2420
dat %>% filter(!is.na(abstract)) # 2605
dat %>% pull(abstract) %>% unique() %>% length() # 2192



#### document-term matrix ####

## extra words
too_words <- tibble(
    word = c("paper", "study", "aim", "aims", "objective", "purpose")
)

## creates the document term matrix
dtm <- dat %>% 
    select(abstract, title, year) %>%
    filter(!is.na(abstract)) %>%
    unique() %>% # 2193
    unnest_tokens(word, abstract) %>% 
    filter(!is.na(word)) %>% ## Books don't have abstract so they get dropped (n = 8)
    anti_join(stop_words) %>% 
    anti_join(too_words) %>%
    filter(!str_detect(word, "[:digit:]")) %>%
    group_by(title) %>%
    count(word, sort = TRUE) %>%
    cast_dtm(document = title, term = word, value = n)

dtm

save(dtm, file = "data/dtm.Rdata")

