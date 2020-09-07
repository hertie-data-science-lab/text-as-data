## ----r, include = FALSE----------------------------------------------------------------------------------
options(width = 60)
library(knitr)
library(tidyverse)
library(quanteda)
library(xtable)

opts_knit$set(error = FALSE)
opts_chunk$set(comment = NA,
               fig.align = "center",
               echo = FALSE)
options(xtable.floating = FALSE,
        latex.environments = "center",
        xtable.comment = FALSE,
        xtable.booktabs = TRUE)


## ----results="hide"--------------------------------------------------------------------------------------
load("data/corpus_uk_manif.rda")
main_parties <- c('Con', 'Lab', 'LD', 'UKIP', 'SNP')
election_years <- c(2017, 2015, 2010, 2005)
subcorp <- corpus_subset(corpus_uk_manif,
                         year %in% election_years &
                         party %in% main_parties)
dfmsubcorp <- dfm(subcorp, remove_punct = TRUE)

con2017dfm <- dfm_subset(dfmsubcorp, year == 2017 & party == "Con")
wcounts <- textstat_frequency(con2017dfm)

used <- nrow(wcounts) # used at least once
used_per <- 100 * used / nfeat(con2017dfm)
hapaxes <- nrow(wcounts[wcounts$frequency == 1,])
hapaxes_per <- 100 * hapaxes / nfeat(con2017dfm)


## ----allscalesprep,results = "hide"----------------------------------------------------------------------
res <- bind_rows("Cons 1997" = wcounts,
                 "Corpus" = textstat_frequency(dfmsubcorp),
                 .id = "Source")
theme_set(theme_minimal())


## ----allscalesorig,fig.align='center', fig.height = 3.2, fig.width = 3.2---------------------------------
ggplot(sample_frac(res, 0.5),
       aes(rank, frequency, color = Source)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("Corpus" = "grey",
                                "Cons 1997" = "black")) +
  theme(legend.position = c(.8, .8))
ggsave("figures/allscalesorig.pdf", height = 3.2, width = 3.2)

## ----allscaleslog,fig.align='center', fig.height = 3.2, fig.width = 3.2----------------------------------
ggplot(res, aes(rank, frequency, color = Source)) +
  geom_point() +
  scale_color_manual(values = c("Corpus" = "grey",
                                "Cons 1997" = "black"))+
    scale_x_log10() + scale_y_log10() +
  theme(legend.position=c(.8, .8))
ggsave("figures/allscaleslog.pdf", height = 3.2, width = 3.2)


## ----heaps,warnings=FALSE,fig.align='center', fig.height = 3.2, fig.width = 3.2--------------------------
tks <- as.character(tokens_tolower(tokens(subcorp[["UK_natl_2017_en_Con.txt"]], remove_punct = TRUE)))
unique_tks <- unique(tks)
ll <- list()
newtypes <- integer(length(unique_tks))
for (i in seq_along(newtypes)) {
  if (is.null(ll[[ tks[i] ]]))
    ll[[ tks[i] ]] <- 1
  newtypes[i] <- length(ll)
}
dd <- data.frame(newtype = newtypes, token = 1:length(newtypes))

tksall <- sample(as.character(tokens_tolower(tokens(subcorp, remove_punct = TRUE))))
unique_tksall <- unique(tksall)
ll <- list()
newtypes <- integer(length(unique_tksall))
for (i in seq_along(newtypes)) {
  if (is.null(ll[[ tksall[i] ]]))
    ll[[ tksall[i] ]] <- 1
  newtypes[i] <- length(ll)
}
ddall <- data.frame(newtype = newtypes, token = 1:length(newtypes))

ggplot(ddall, aes(token, newtype)) +
  geom_abline(intercept = 0, slope = 1, col = "gray", linetype = "dashed") +
  geom_line() +
  scale_x_log10() + scale_y_log10() +
  labs(x = "tokens", y = "word types")
ggsave("figures/heaps.pdf", height = 3.2, width = 3.2)

## ----inter,results="asis"--------------------------------------------------------------------------------
tbl <- textstat_frequency(dfmsubcorp)[1:6, 1:2]
names(tbl) <- c("Word", "Freq.")
print(xtable(tbl, digits = 0), file = "tables/table1.tex")


## ----bottomten,results="asis"----------------------------------------------------------------------------
tbl <- tail(textstat_frequency(dfmsubcorp), 6)[, 1:2]
names(tbl) <- c("Word", "Freq.")
print(xtable(tbl, digits = 0), file = "tables/table2.tex")


## ----topfiltered,results="asis"--------------------------------------------------------------------------
tf <- filter(textstat_frequency(dfmsubcorp),
             !(feature %in% stopwords("english")))
tbl <- head(tf, 6)[, 1:2]
names(tbl) <- c("Word", "Freq.")
print(xtable(tbl, digits = 0),  file = "tables/table3.tex")

## save the rest
save(used, hapaxes_per, used_per, file = "scalars/week01.rda")
