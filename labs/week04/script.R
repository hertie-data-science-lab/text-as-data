library(tidyverse)
library(igraph)

library(quanteda)
d <- dictionary(file = "bara-et-al.ykd")
load("data/corpus_bara_para.rda")

wds <- unique(unlist(d))
tps <- dfm(corpus_bara_para, dictionary = d)

tps_all <- dfm(corpus_bara_para)
tps_dict <- intersect(featnames(tps_all), wds)
# coverage
length(tps_dict) / length(featnames(tps_all))


tpsdf <- data.frame(docvars(tps), convert(tps, "data.frame"))
tpsdf <- mutate(tpsdf, all = bara_et_al.advocacy + bara_et_al.legal + bara_et_al.medical +
         bara_et_al.moral + bara_et_al.procedural + bara_et_al.social)

# transition matrix of speakers
trans_speak <- table(pre = tpsdf$speaker[-1], post = tpsdf$speaker[-nrow(tpsdf)])

# graph
grp <- graph_from_adjacency_matrix(trans_speak, mode = "directed")

plot(grp)
sort(eigen_centrality(grp)$vector, decreasing = TRUE)

# of votes
trans_vote <- table(pre = tpsdf$vote[-1], post = tpsdf$vote[-nrow(tpsdf)])
prop.table(trans_vote, 1)

# of topics
gg <- unlist(tokens_lookup(tokens(corpus_bara_para), dictionary = d))
trans_topics <- table(pre = gg[-1], post = gg[-length(gg)])

as.data.frame(prop.table(trans_topics, 2)) %>%
  ggplot(aes(x = pre, y = Freq, color = post, fill = post)) +
  geom_col(position = "dodge")# +
  facet_wrap(. ~ post) +
  coord_flip()

# collapse the runs
# runs <- rle(tpsdf$vote)
# table(pre = runs$values[-1], post = runs$values[-length(runs$values)])

# topics over turns


