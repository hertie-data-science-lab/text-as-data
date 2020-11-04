library(tidyverse)
theme_set(theme_minimal())

require(quanteda)
require(quanteda.textmodels)
require(glmnet)
require(caret)
require(plotROC)

## sentiment

corp_movies <- data_corpus_moviereviews
summary(corp_movies, 5)

mmovierev <- dfm(data_corpus_moviereviews)

# data_dictionary_geninqposneg
res <- textstat_polarity(corp_movies,
                         data_dictionary_LSD2015)
res$sent_prob <- 1/(1 + exp(-res$sentiment))

# Since the first 1000 reviews are negative and the remaining
# reviews are classified postive
# draw a random sample of the documents.

set.seed(800)
id_train <- sample(1:2000, 1500)
head(id_train, 10)

corp_movies$id_numeric <- 1:ndoc(corp_movies)

# train
dfmat_training <- corpus_subset(corp_movies,
                                id_numeric %in% id_train) %>%
  dfm(remove = stopwords("en"),
      remove_number = TRUE, stem = TRUE)

# test
dfmat_test <- corpus_subset(corp_movies, !id_numeric %in% id_train) %>%
  dfm(remove = stopwords("en"), remove_number = TRUE, stem = TRUE)

lasso <- cv.glmnet(x = dfmat_training,
                   y = as.integer(dfmat_training$sentiment == "pos"),
                   alpha = 1, # 0 (ridge) to 1 (lasso)
                   nfold = 5,
                   family = "binomial")
index_best <- which(lasso$lambda == lasso$lambda.min)
beta <- lasso$glmnet.fit$beta[, index_best]
head(sort(beta, decreasing = TRUE), 20)
table(beta == 0.0)

# make them match
dfmat_matched <- dfm_match(dfmat_test,
                           features = featnames(dfmat_training))

pred <- predict(lasso, dfmat_matched, type = "response",
                s = lasso$lambda.min)
head(pred)

# Error analysis: confusion matrix
actual_class <- as.integer(dfmat_matched$sentiment == "pos")
lasso_class <- predict(lasso, dfmat_matched, type = "class")
predicted_class <- as.integer(lasso_class)
predicted_prob <- predict(lasso, dfmat_matched, type = "response")
tab_class <- table(actual_class, predicted_class)
tab_class

# Error analysis: ROC curves
lasso_prob <- predicted_prob[,1]
dict_prob <- res$sent_prob[-id_train]
dict_sent <- res$sentiment[-id_train]

plot(predicted_prob[,1], dict_prob)
cor(predicted_prob[,1], dict_sent)

calculate_roc(lasso_prob, actual_class)
calculate_roc(dict_prob, actual_class)

rs <- data.frame(M = c(dict_prob, lasso_prob),
                 D = c(actual_class, actual_class),
                 classifier = rep(c("dict", "lasso"), each = 500))

ggplot(rs, aes(m = M, d = D, color = classifier)) +
  geom_roc()


