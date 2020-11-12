library(tidyverse)
library(quanteda)
library(stm)

load("data/df_pba_debate_by_speaker.rda")
names(df_pba_debate_by_speaker)

df_pba_debate_by_speaker <- mutate(df_pba_debate_by_speaker,
       contributions = str_replace_all(contributions, "'s$", ""))

corp <- corpus(df_pba_debate_by_speaker,
               text_field = "contributions",
               docid_field = "speaker")
docvars(corp, "speaker") <- df_pba_debate_by_speaker$speaker # add speaker field
summary(corp)

para_corp <- corpus_reshape(corp, to = "paragraphs")
head(summary(para_corp))

para_dfm <- dfm(para_corp,
                   remove_punct = TRUE,
                   remove = stopwords(),
                   remove_symbols = TRUE,
                   remove_separators = TRUE,
                   split_hyphens = TRUE,
                   remove_numbers = TRUE)
feats1 <- featnames(para_dfm)

para_dfm <- dfm_select(para_dfm, selection = "remove",
                       min_nchar = 2, # at least two letter long
                       pattern = "^\\d", valuetype = "regex") # things starting with a digit
para_dfm <- dfm_subset(para_dfm, ntoken(para_dfm) > 4)
para_dfm <- dfm_trim(para_dfm,
                     min_termfreq = 2, min_docfreq = 2)

cat("Removed:", setdiff(feats1, featnames(para_dfm))) # lots

para_stm <- asSTMCorpus(para_dfm)

mod <- stm(documents = para_stm$documents, vocab = para_stm$vocab,
           K = 10, prevalence = ~ party, data = para_stm$data,
           seed = 12345)


plot(topicCorr(mod))
topicQuality(mod, documents = para_stm$documents)
dim(mod2$beta$logbeta[[1]])

# what should we call these topics?
labels <- apply(sageLabels(mod)$marginal$frex, 1,
                function(x){ paste(x[1:4], collapse = "-") })

speaker_dfm <- dfm(corp,
                remove_punct = TRUE,
                remove = stopwords(),
                remove_symbols = TRUE,
                remove_separators = TRUE,
                split_hyphens = TRUE,
                remove_numbers = TRUE)

new_docs <- asSTMCorpus(speaker_dfm)
new_stm <- alignCorpus(new_docs,
                       old.vocab = para_stm$vocab)

nds <- fitNewDocuments(mod, documents = new_stm$documents,
                newData = new_stm$data,
                prevalence = ~ party)

thetas <- data.frame(nds$theta)
colnames(thetas) <- labels

# inflate them back into counts
inflated <- round(thetas * outer(ntoken(speaker_dfm), rep(1, 10)))
rownames(inflated) <- paste(docvars(speaker_dfm)$speaker,
                            docvars(speaker_dfm)$party,
                            sep = "-")
library(ca)
plot(ca(inflated))
