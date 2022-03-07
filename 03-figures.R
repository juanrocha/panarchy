library(tidytext)
library(tidyverse)
library(tm)
library (topicmodels)
library(lda)
library(patchwork)
library(png)
library(gg3D)

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

g1 <- df_stats %>%
    add_column(perplexity = as.numeric(perp[c(1,2,4,3)])) %>% #minimize perplexity
    pivot_longer(cols = 2:4, names_to = "measure", values_to = "value") %>%
    ggplot(aes(model, value)) + 
    geom_col() + 
    # scale_y_continuous(labels = scales::label_scientific) +
    facet_wrap(.~measure, scales = "free_y") +
    labs(x = "Algorithm", y = "Value", tag = "A") +
    theme_light(base_size = 8) + 
    theme(axis.text.x = element_text(size = 5))
g1
# ggsave(filename = "figures/fig1_algorithm_selection.png",
#        device = "png", width = 5, height = 2)

# J201117: the best algo is Gibbs sampling!

#### number of topics ####
g2 <- df_topic_number %>%
    # mutate(alpha_log = log10(alpha)) %>%
    pivot_longer(cols = 2:last_col(), names_to = "measure", values_to = "value") %>%
    # filter(measure != "alpha") %>%
    ggplot(aes(as.factor(topic_number), value)) +
    geom_col() + 
    # scale_y_continuous(labels = scales::label_scientific) +
    labs(x = "Number of topics", y = "Value", tag = "B") +
    facet_wrap(.~measure, scales = "free", ncol = 4, nrow = 1) +
    theme_light(base_size = 8)

g1/g2

ggsave(filename = "paper/figures/fig2_topicnumber.png",
       device = "png", width = 5, height = 4, dpi = 400)


#### heatmaps topics ####

gplots::heatmap.2(
    posterior(topicNumber.TM[[3]])$topics, 
    key=T, trace="none", margins=c(5,5), 
    cexRow=0.7, cexCol=0.7, col="topo.colors", keysize=1, key.par = list(cex = 0.5),
    labRow = FALSE, key.title = NA, key.xlab = "probability", key.ylab = "count" )

#### topics ####

df_topics10 <- tidy(topicNumber.TM[[2]], matrix = "beta")
df_topics25 <- tidy(topicNumber.TM[[3]], matrix = "beta")

## visualization of topics
g_25 <- df_topics25 %>%
    group_by(topic) %>%
    top_n(10, beta) %>% 
    ungroup() %>%
    arrange(topic, - beta) %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta)) +
    geom_col(aes(fill = as.factor(topic)),show.legend = FALSE) +
    coord_flip() + 
    scale_x_reordered() +
    labs(tag = "D", y = "Probability of word explaining the topic", x = "Word ranking") +
    facet_wrap(.~ topic, scales = "free_y", ncol = 5) +
    theme_light(base_size = 6) + 
    theme(axis.text.x = element_text(size = 5))


g_25

ggsave("fig3_topics25_time.png", device = "png", width = unit(5,"in"), height = unit(5,"in"))


#### change over time ####
df_documents <- tidy(topicNumber.TM[[3]], matrix = "gamma")

## read data agian from 01-read_data.R until line 21

# g <- df_documents %>% 
#     left_join(
#         dat %>% 
#             select(abstract, title, year) %>%
#             filter(!is.na(abstract)) %>%
#             unique(),
#         by = c("document" = "title") ) %>%
#     mutate(year = as.numeric(year)) %>%
#     filter(year > 2000) %>%
#     ggplot(aes(year, gamma) )+
#     geom_jitter(
#         aes(color = as.factor(topic), fill = as.factor(topic), alpha = gamma),
#         size = 1, show.legend = FALSE) +
#     geom_smooth(method = "loess") +
#     scale_alpha_continuous(range = c(0.1,1)) +
#     facet_wrap(.~topic, ncol = 5) +
#     #labs(tag = "B") +
#     theme_light(base_size = 6) + 
#     theme(axis.text.x = element_text(size = 4))

g_tl <- dat %>% #filter(year > 2000, year < 2021) %>%
    group_by(year) %>%
    tally() %>% 
    ggplot(aes(year, n)) + geom_line(size = 0.1) + 
    ggplot2::annotate("rect", xmin = 2000, xmax = 2020, ymin = 0, ymax = 323, alpha = 0.2 ) +
    labs(y = "Number of papers", x = "Year", tag = "A") + 
    theme_classic(base_size = 6)

papers_yr <- dat %>%
    filter(year > 2000, year < 2021) %>%
    group_by(year) %>%
    tally() %>% rename(total_papers = n)


g_prop <- df_documents %>% 
    left_join(
        dat %>% 
            select(abstract, title, year) %>%
            filter(!is.na(abstract)) %>%
            unique(),
        by = c("document" = "title") ) %>%
    mutate(year = as.numeric(year)) %>%
    filter(year > 2000, year < 2021) %>%
    group_by(document) %>% 
    slice_max(gamma) %>% 
    ungroup() %>% group_by(topic, year) %>%
    tally() %>%
    left_join(papers_yr) %>%
    mutate(proportion = n / total_papers) %>%
    ggplot(aes(x = year, y = proportion, group = topic)) +
   # geom_col(aes(fill = topic), position = "stack") +
    geom_line(aes(color = as.factor(topic)), show.legend = FALSE, size = 0.25) +
    geom_hline(yintercept = 0.04, color = "gray50", size = 0.5, linetype = 2 ) +
    labs(y = "Proportion of papers", x = "Year", tag = "B") +
    #facet_wrap(.~topic, ncol = 5) +
    theme_light(base_size = 6) 

## alternative with stacked lines
g_stack <-  df_documents %>% 
    left_join(
        dat %>% 
            select(abstract, title, year) %>%
            filter(!is.na(abstract)) %>%
            unique(),
        by = c("document" = "title") ) %>%
    mutate(year = as.numeric(year)) %>% 
    filter(year > 2000, year <2021) %>%
    group_by(topic, year) %>% 
    summarize(gamma_yr_tp = sum(gamma), .groups = "drop") %>% 
    group_by(year) %>%
    mutate(total = sum(gamma_yr_tp)) %>%
    ungroup() %>% 
    mutate(proportion = gamma_yr_tp / total,
           topic = as.factor(topic)) %>%
    ggplot(aes(x = year, y = proportion, group = topic)) +
    geom_area(aes(fill = topic, color = topic), 
              position = "stack", show.legend = FALSE, alpha = 0.5, size = 0.25) +
    labs(y = "Relative proportion of topics", x = "Year", tag = "C") +
    theme_classic(base_size = 6) 

# + 
#     theme(axis.text.x = element_text(size = 4))
sm2 <- df_documents %>% 
    left_join(
        dat %>% 
            select(abstract, title, year) %>%
            filter(!is.na(abstract)) %>%
            unique(),
        by = c("document" = "title") ) %>%
    mutate(year = as.numeric(year)) %>% 
    filter(year > 2000, year <2021) %>%
    ggplot(aes(year, gamma) )+
    geom_jitter(
        aes(color = as.factor(topic), fill = as.factor(topic), alpha = gamma),
        size = 1, show.legend = FALSE) +
    geom_smooth(method = "loess") +
    scale_alpha_continuous(range = c(0.1,1)) +
    facet_wrap(.~topic, ncol = 5) +
    labs(y = "Relative proportion of topics", x = "Year") +
    #theme_classic(base_size = 6) 
    theme_light(base_size = 6) +
    theme(axis.text.x = element_text(size = 4))
sm2
ggsave(
    plot = sm2,
    filename = "sm2.png", path = "paper/figures",
    device = "png", width = unit(5,"in"), height = unit(5,"in"),
    dpi = 500)

(g_tl | g_prop | g_stack) / g_25 + plot_layout(heights = unit(c(1.5,4), "in"))

ggsave(
    plot = last_plot(),
    filename = "fig3_topis25.png", path = "paper/figures",
    device = "png", width = unit(6.5,"in"), height = unit(6.5,"in"), 
    dpi = 500)

## useful for writing:
gg <- plotly::ggplotly(g_prop)
gg

plotly::highlight(gg, dynamic = TRUE, on = "plotly_hover")


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
    mutate(feature = str_replace_all(feature, "idenfity phases", "identify phases")) %>% 
    mutate(feature = as_factor(feature)) %>%
    mutate(feature = fct_reorder(.f = feature, .x = proportion, sort))

qual_summary %>%
    ggplot(aes(feature, proportion)) +
    geom_col() + 
    facet_wrap(~var_type, scales = "free_y") +
    scale_y_continuous(labels = scales::percent) +
    tidytext::scale_x_reordered() +
    coord_flip() + labs(x = "", y = "") +
    theme_light(base_size = 6)

ggsave("paper/figures/fig4_qual_results.png", device = "png", width = unit(5,"in"), height = unit(2.5,"in"))

save(qual, file = "data/qual_coding.RData")

#### Fig 1 ####

p1 <- ggplot(data = data_frame(x = 0, y= 0), aes(x,y)) + geom_blank() +
    labs(tag = "A") +
    theme_void(base_size = 6) +
    annotation_custom(
        grob = grid::rasterGrob( image = readPNG(
            "paper/extra_figs/adaptive_cycle.png"), interpolate = TRUE),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
    )
p2 <- ggplot(data = data_frame(x = 0, y= 0), aes(x,y)) + geom_blank() +
    labs(tag = "B") +
    theme_void(base_size = 6) +
    annotation_custom(
        grob = grid::rasterGrob( image = readPNG(
            "paper/extra_figs/panarchy.png"), interpolate = TRUE),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
    )


p3 <- ggplot(data = data_frame(x = 0, y= 0), aes(x,y)) + geom_blank() +
    labs(tag = "C") +
    theme_void(base_size = 6) +
    annotation_custom(
        grob = grid::rasterGrob( image = readPNG(
            "paper/extra_figs/Figure_2-2.png"), interpolate = TRUE),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
    )

p1 + p2 + p3 + plot_layout(widths = 2, heights = 2)

ggsave(filename = "paper/figures/fig1_panarchy.png",
       plot = last_plot(), width = 6, height = 2, device = "png", dpi = 400)


### 3D adaptive cycle for reviewer...
library(deSolve)
# parameters
a <--8/3;b<--10;c<-28  
# Initial conditions
yini <- c(X = 1, Y = 1, Z = 1)  

## Lorenz function:
Lorenz <- function (t, y, parms) { 
    with(as.list(y), {
        dX <- a * X + Y * Z
        dY <- b * (Y - Z)
        dZ <- -X * Y + c * Y - Z 
        list(c(dX, dY, dZ)) })
}

# set up
times <- seq(from = 0, to = 100, by = 0.01)
out <- ode(y = yini, times = times, func = Lorenz,
           parms = NULL)


scatterplot3d::scatterplot3d (
    out[,"X"], out[,"Y"], out[,"Z"], 
    type = "l", xlab="", ylab = "", zlab = "")

out |> 
    as_tibble() |>
    pivot_longer(cols = c(X,Y,Z), names_to = "dim", values_to = "value") |> 
    ggplot(aes(time, value)) +
    geom_line(aes(color = time)) +
    facet_wrap(~dim)

theta <- 135 # azimuthal
phi <-  20  # colatitude

out |> 
    as_tibble() |>
    filter(time > 29.5, time < 35) |> 
    ggplot(aes(x = X, y = Y, z = Z,)) + 
    #geom_line(aes(color = time)) +
    theme_void() +
    axes_3D(theta = theta, phi = phi)+
    stat_3D(geom="path",theta = theta, phi = phi) +
    labs_3D(theta = theta, phi = phi,
            labs= c( "Resilience","Connectedness", "Potential"))

