library(tidytext)
library(tidyverse)
library(tm)
library (topicmodels)
library(lda)

load("data/models_gibbs.RData")

#Finding number of topics
k <- c(5,10,25,50,100)

## visualizations:
df_topic_number <- tibble(
    topic_number = k,
    entropy = map_dbl (topicNumber.TM, function (x)
        mean(apply(posterior(x)$topics, 1, function (z) - sum(z * log(z)))) # maximize Entropy
    ),
    alpha = map_dbl(topicNumber.TM, slot, "alpha"),
    log_lik = map_dbl(topicNumber.TM, logLik) #,  #maximize loglik
    #perplexity = map_dbl(topicNumber.TM, perplexity) #minimize perplexity
)

#### algorithm selection ####
df_stats <- tibble(
    model = names(lapply(tset.TM, logLik)),
    loglik = as.numeric(lapply(tset.TM, logLik)), #maximize loglik
    entropy = lapply (tset.TM, function (x) 
        mean(apply(posterior(x)$topics,
                   1, function (z) - sum(z * log(z))))) %>% as.numeric()#maximize ENTROPY
    
)


perp <-  lapply(tset.TM[c(1,2,4)], perplexity)
perp$Gibbs <- NA
# pretty names:
df_stats$model <- c("VEM alpha", "VEM fixed", "Gibbs", "CTM")

df_stats %>%
    add_column(perplexity = as.numeric(perp[c(1,2,4,3)])) %>% #minimize perplexity
    pivot_longer(cols = 2:4, names_to = "measure", values_to = "value") %>%
    ggplot(aes(model, value)) + 
    geom_col() + facet_wrap(.~measure, scales = "free") +
    theme_light(base_size = 6) 

# ggsave(filename = "figures/fig1_algorithm_selection.png",
#        device = "png", width = 5, height = 2)

# J201117: the best algo is Gibbs sampling!

#### number of topics ####
df_topic_number %>%
    # mutate(alpha_log = log10(alpha)) %>%
    pivot_longer(cols = 2:last_col(), names_to = "measure", values_to = "value") %>%
    # filter(measure != "alpha") %>%
    ggplot(aes(as.factor(topic_number), value)) +
    geom_col() + 
    labs(x = "Number of topics") +
    facet_wrap(.~measure, scales = "free", ncol = 4, nrow = 1) +
    theme_light(base_size = 6)

# ggsave(filename = "figures/fig2_topicnumber.png",
#        device = "png", width = 5, height = 2)


#### heatmaps topics ####

gplots::heatmap.2(
    posterior(topicNumber.TM[[2]])$topics, 
    key=T, trace="none", margins=c(5,5), 
    cexRow=0.7, cexCol=0.7, col="topo.colors", keysize=1, key.par = list(cex = 0.5),
    labRow = FALSE, key.title = NA, key.xlab = "probability", key.ylab = "count" )

#### topics ####

df_topics10 <- tidy(topicNumber.TM[[2]], matrix = "beta")
df_topics25 <- tidy(topicNumber.TM[[3]], matrix = "beta")

## visualization of topics
g1 <- df_topics25 %>%
    group_by(topic) %>%
    top_n(10, beta) %>% 
    ungroup() %>%
    arrange(topic, - beta) %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta)) +
    geom_col(aes(fill = as.factor(topic)),show.legend = FALSE) +
    coord_flip() + 
    scale_x_reordered() +
    # labs(tag = "A") +
    facet_wrap(.~ topic, scales = "free_y", ncol = 5) +
    theme_light(base_size = 6) + 
    theme(axis.text.x = element_text(size = 3))

ggsave("fig3_topics25_time.png", device = "png", width = unit(5,"in"), height = unit(5,"in"))


#### change over time ####
df_documents <- tidy(topicNumber.TM[[3]], matrix = "gamma")

## read data agian from 01-read_data.R until line 21

g <- df_documents %>% 
    left_join(
        dat %>% 
            select(abstract, title, year) %>%
            filter(!is.na(abstract)) %>%
            unique(),
        by = c("document" = "title") ) %>%
    mutate(year = as.numeric(year)) %>%
    filter(year > 2000) %>%
    ggplot(aes(year, gamma) )+
    geom_jitter(
        aes(color = as.factor(topic), fill = as.factor(topic), alpha = gamma),
        size = 1, show.legend = FALSE) +
    geom_smooth(method = "loess") +
    scale_alpha_continuous(range = c(0.1,1)) +
    facet_wrap(.~topic, ncol = 5) +
    #labs(tag = "B") +
    theme_light(base_size = 6) + 
    theme(axis.text.x = element_text(size = 4))

papers_yr <- dat %>%
    filter(year > 2000) %>%
    group_by(year) %>%
    tally() %>% rename(total_papers = n)


g <- df_documents %>% 
    left_join(
        dat %>% 
            select(abstract, title, year) %>%
            filter(!is.na(abstract)) %>%
            unique(),
        by = c("document" = "title") ) %>%
    mutate(year = as.numeric(year)) %>%
    filter(year > 2000) %>%
    group_by(document) %>% 
    slice_max(gamma) %>% 
    ungroup() %>% group_by(topic, year) %>%
    tally() %>%
    left_join(papers_yr) %>%
    mutate(proportion = n / total_papers) %>%
    ggplot(aes(x = year, y = proportion, group = topic)) +
    geom_line(aes(color = as.factor(topic)), show.legend = FALSE) +
    labs(y = "Proportion of papers", x = "Year") +
    facet_wrap(.~topic, ncol = 5) +
    theme_light(base_size = 6) + 
    theme(axis.text.x = element_text(size = 4))
    
    
    # ggplot(aes(year, gamma) )+
    # geom_jitter(
    #     aes(color = as.factor(topic), fill = as.factor(topic), alpha = gamma),
    #     size = 1, show.legend = FALSE) +
    # geom_smooth(method = "loess") +
    # scale_alpha_continuous(range = c(0.1,1)) +
    # facet_wrap(.~topic, ncol = 5) +
    # #labs(tag = "B") +
    # theme_light(base_size = 6) + 
    # theme(axis.text.x = element_text(size = 4))


g

library(patchwork)
g1/g

ggsave("fig3_topis25_timeline2.png", device = "png", width = unit(6,"in"), height = unit(4,"in"))

#### qualitative data ####

qual <- googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1pBjjgTAqhXYIsMoaf0a_gE0gY40iGtUtzkMi2pFBcWs/edit#gid=0")

names(qual)

qual_summary <- qual %>% 
    select(2:8, 13:16, 19,20) %>%
    rename(`qualitative methods` = qual, `quantitative methods` = quant) %>%
    map_df(function(x) sum(x, na.rm = TRUE)/42) %>% 
    pivot_longer(cols = 1:last_col(), names_to = "feature", values_to = "proportion") %>%
    add_column(var_type = c(
        rep("panarchy feature", 2), rep("paper type", 5), 
        rep("panarchy feature", 4),rep("paper type", 2))) %>% 
    mutate(feature = as_factor(feature)) %>%
    mutate(feature = fct_reorder(.f = feature, .x = proportion, sort))

qual_summary %>%
    ggplot(aes(feature, proportion)) +
    geom_col() + 
    facet_wrap(~var_type, scales = "free_y") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_reordered() +
    coord_flip() + labs(x = "", y = "") +
    theme_light(base_size = 6)

ggsave("fig5_qual_results.png", device = "png", width = unit(5,"in"), height = unit(2,"in"))

