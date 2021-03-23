# ?fcm
# corp <- corpus(df_pba_debate)
# library(quanteda)
# corp <- corpus(df_pba_debate)
# head(corp)
# load("data/df_pba_debate_by_speaker.rda")
# corp <- corpus(df_pba_debate_by_speaker)
# View(df_pba_debate_by_speaker)
# corp <- corpus(df_pba_debate_by_speaker, text_field = "contributions")
# corp
# dd <- dfm(corp, remove = stopwords(), tolower = TRUE, remove_numbers = TRUE)
# dim(dd)
# ff <- fcm(dd, context = "window", window = 5)
# ff <- fcm(corp, context = "window", window = 5)
# dim(ff)
# docvars(ff)
# ff[1:5,1:4]
# ?fcm_remove
# ff <- fcm(tokens(corp, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_separators = TRUE), context = "window", window = 5)
# dim(ff)
# ff[1:5,1:4]
# ff[5:10,1:4]
# ff <- fcm(tokens_tolower(tokens(corp, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_separators = TRUE)), context = "window", window = 5, tri = FALSE)
# dim(ff)
# load("data/corpus_bara_speaker.rda")
# baradict <- dictionary(file = "data/bara-et-al.ykd")
# baradict
# #ff <- fcm(tokens_tolower(tokens(corp, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_separators = TRUE)), context = "window", window = 5, tri = FALSE)
# load("data/corpus_bara_speaker.rda")
# ff <- NULL
# ff <- fcm(tokens_tolower(tokens(corpus_bara_speaker, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_separators = TRUE)), context = "window", window = 5, tri = FALSE)
# dim(ff)
# ff[5:10,1:4]
# featnames(fF)
# featnames(ff)
# ff["statute",]
# baradict
# unlist(baradict)
# unname(unlist(baradict))
# barawds <- unname(unlist(baradict))
# bdist <- dist(ff[barawds,])
# ff[barawds,]
# setdiff(barawds, featnames(ff))
# ff[intersect(barawds, featnames(ff)),]
# bdits <- dist(ff[intersect(barawds, featnames(ff)),] )
# dim(bdits)
# str(bdits)
#
#
# voc <- intersect(barawds, featnames(ff))
# vv <- voc[colSums(ff[,voc]) > 0]
# ffb <- fcm_keep(ff, pattern = vv, valuetype = "fixed")
# plot(hclust(dist(ffb)))

load("data/corpus_bara_para.rda")
genp <-  c("he", "him", "his", "she", "her")
bb <- dfm(corpus_bara_para, remove_punct = TRUE,
          remove_symbols = TRUE, remove_numbers = TRUE,
          remove_separators = TRUE,
          remove = setdiff(stopwords(), genp))

bb_lsa <- textmodel_lsa(bb, nd = 50)
wds <- intersect(c(baradict$bara_et_al$social),
                 rownames(bb_lsa$features))
bb_ff <- bb_lsa$features[wds,]
bb_cs <- lsa::cosine(t(bb_ff))
rownames(bb_cs) <- rownames(bb_ff)
plot(hclust(dist(1-bb_cs)))

word_vectors[socmed, ]
library("text2vec")
load("data/corpus_bara_para.rda")
genp <-  c("he", "him", "his", "she", "her")
bb <- dfm(corpus_bara_para, remove_punct = TRUE,
remove_symbols = TRUE, remove_numbers = TRUE,
remove_separators = TRUE,
remove = setdiff(stopwords(), genp))
bb <- dfm_trim(bb, min_termfreq = 3)
featnames(bb)
wds <- featnames(bb)
length(unlist(tokens(corpus_bara_para)))
head(unlist(tokens(corpus_bara_para)))
toks <- (unlist(tokens(corpus_bara_para)))
toks2 <- tokens_select(toks, wds, padding = TRUE)
toks2 <- tokens_select(tokens(corpus_bara_para), wds, padding = TRUE)
length(toks)
bea_fcm <- fcm(toks2, context = "window", count = "weighted", weights = 1 / (1:5), tri = TRUE)
glove <- GlobalVectors$new(rank = 50, x_max = 10)
wv_main <- glove$fit_transform(bea_fcm, n_iter = 10,
convergence_tol = 0.01, n_threads = 4)
dim(wv_main)
wv_context <- glove$components
dim(wv_context)
word_vectors <- wv_main + t(wv_context)
intersect(wds, c(baradict$bara_et_al$social, baradict$bara_et_al$social)
rownames(bb_lsa$features))
intersect(wds, c(baradict$bara_et_al$social, baradict$bara_et_al$medical))
socmed <- intersect(wds, c(baradict$bara_et_al$social, baradict$bara_et_al$medical))
socmed_vecs <- word_vectors[socmed, ]
head(socmed)
dim(socmed_vec)
dim(socmed_vecs)
plot(hclust(dist(socmed_vecs)))
legsocmed <- intersect(wds, c(baradict$bara_et_al$social, baradict$bara_et_al$legal, baradict$bara_et_al$medical))
plot(hclust(dist(word_vectors[legsocmed, ])))



