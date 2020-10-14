library(tidyverse)
library(stm)
library(quanteda)

d <- dictionary(file = "bara-et-al.ykd")
load("data/corpus_bara_para.rda")

load("assignments/assignment2/data/corpus_uk_manif.rda")
scorp <- corpus_subset(corpus_uk_manif, year > 1979)
scp <- corpus_reshape(scorp, to = "paragraph")
scp2 <- corpus_subset(scp, ntoken(scp) > 10)
dfscp <- dfm(scp2, stem = TRUE, tolower = TRUE, remove = stopwords(),
            remove_punct = TRUE, remove_numbers = TRUE,
            split_hyphens = TRUE, remove_separators = TRUE,
            remove_symbols = TRUE )
dfstm <- asSTMCorpus(dfscp)
mod50 <- stm(dfstm$documents, dfstm$vocab,
             K = 50, prevalence = ~ s(year) + party, data = dfstm$data)

mod10c <- stm(dfstm$documents, dfstm$vocab,
              K = 10, prevalence = ~ s(year) + party, data = dfstm$data)

prep <- estimateEffect(c(6:10) ~ party + s(year), mod10c, metadata = dfstm$data)
plot(prep, "year", model = mod10c, method = "continuous")

res <- searchK(dfstm$documents, dfstm$vocab, K = c(5, 10, 20, 30))

mod30 <- stm(dfstm$documents, dfstm$vocab, K = 30,
             prevalence = ~ party + s(year), data = dfstm$data)
prep <- estimateEffect(c(21) ~ party + s(year), mod10c, metadata = dfstm$data)
plot(prep, "year", model = mod10c, method = "continuous")



cbar <- corpus_subset(corpus_bara_para, ntoken(corpus_bara_para) > 20)
swds <- setdiff(stopwords(), c("he", "him", "his", "she", "her"))
bdfm <- dfm(cbar, tolower = TRUE, remove = swds,
    remove_punct = TRUE, remove_numbers = TRUE,
    split_hyphens = TRUE, remove_separators = TRUE)
bdfm_stm <- asSTMCorpus(bdfm)



