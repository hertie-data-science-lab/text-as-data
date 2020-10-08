library(quanteda)
library(stm)

load("data/df_pba_debate_by_speaker.rda")
names(df_pba_debate_by_speaker)


corp <- corpus(df_pba_debate_by_speaker,
               text_field = "contributions",
               docid_field = "speaker")
docvars(corp, "speaker") <- df_pba_debate_by_speaker$speaker # add speaker field
summary(corp)

para_corp <- corpus_reshape(corp, to = "paragraphs")
head(summary(para_corp))

table(ntoken(para_corp)) # 15 documents with, err, no words

para_corp <- corpus_subset(para_corp, ntoken(para_corp) > 2)

para_dfm <- dfm(para_corp,
  remove_punct = TRUE,
  remove = stopwords(),
  remove_numbers = TRUE)

# fit a simple model
mod <- stm(para_dfm, K = 10, seed = 12345)

labelTopics(mod)

plot(mod, type = "labels", labeltype = "prob") # or frex, lift, score


# findThoughts(mod, texts = texts(para_corp), topics = 1)


## -----------------------------------------------------------------------------
# checkBeta(mod)$problemWords # we'll look at the 'problem words field'


## -----------------------------------------------------------------------------
# dotchart(exclusivity(mod), labels = 1:10)


## -----------------------------------------------------------------------------
# cohere <- semanticCoherence(mod, para_dfm)
# dotchart(cohere, labels = 1:10)


head(mod$theta)
rowSums(mod$theta)

df <- data.frame(topic1 = mod$theta[,1], docvars(para_dfm))
head(df)

## -----------------------------------------------------------------------------
# library(lme4)
# library(lattice)
#
# lmer_topic1 <- lmer(topic1 ~ (1 | speaker), data = df)
# dotplot(ranef(lmer_topic1, condVar = TRUE))
#

mod2 <- stm(para_dfm, K = 10,
           prevalence = ~ party, data = docvars(para_dfm),
           seed = 12345)


gg <- estimateEffect(c(1,8) ~ party, mod2, docvars(para_dfm))
summary(gg)

plot(gg, "party", labeltype = "prob", model = mod2)

