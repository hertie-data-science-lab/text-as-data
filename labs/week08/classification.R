library(tidyverse)
theme_set(theme_minimal())

library(quanteda)
library(quanteda.textmodels)
library(glmnet)
library(caret)
library(plotROC)

## sentiment - the old way
library(quanteda.sentiment)
corp_movies <- data_corpus_moviereviews
summary(corp_movies, 5)

mmovierev <- dfm(data_corpus_moviereviews)

# or data_dictionary_geninqposneg
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
train_corp <- corpus_subset(corp_movies,
                                id_numeric %in% id_train)
dfmat_train <- dfm(train_corp,
                   remove = stopwords("en"),
                   remove_number = TRUE,
                   stem = TRUE)
# test
test_corp <- corpus_subset(corp_movies, !id_numeric %in% id_train)
dfmat_test <- dfm(test_corp,
                  remove = stopwords("en"),
                  remove_number = TRUE,
                  stem = TRUE)
#yval <- as.integer(dfmat_training$sentiment == "pos")
lasso <- cv.glmnet(x = dfmat_training,
                   y = dfmat_training$sentiment,
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

predicted_prob <- predict(lasso, dfmat_matched,
                          type = "response",
                          s = lasso$lambda.min)
head(predicted_prob)

# Error analysis: confusion matrix
actual_class <- dfmat_matched$sentiment
head(actual_class)
class(actual_class)
lasso_class <- factor(predict(lasso, dfmat_matched, type = "class"))
#predicted_class <- as.integer(lasso_class)
#predicted_prob <- predict(lasso, dfmat_matched, type = "response")
tab_class <- table(actual_class, lasso_class)
tab_class

# Error analysis: ROC curves
lasso_prob <- predicted_prob[,1]
dict_prob <- res$sent_prob[-id_train]
dict_sent <- res$sentiment[-id_train]

plot(predicted_prob[,1], dict_prob)
cor(predicted_prob[,1], dict_sent)

# for the purposes of roc, let's transform actual_class
# into a 0/1 variable for comparison to the predicted probability

levels(actual_class) # 0 -> "neg" and 1 -> "pos" so as.numeric will work

actual_class_binary <- as.numeric(actual_class)-1
calculate_roc(lasso_prob, actual_class_binary)
calculate_roc(dict_prob, actual_class_binary)

rs <- data.frame(M = c(dict_prob, lasso_prob),
                 D = c(actual_class_binary, actual_class_binary),
                 classifier = rep(c("dict", "lasso"), each = 500))
class(rs$D)

p <- ggplot(rs, aes(m = M, d = D, color = classifier)) +
  geom_roc()
p

# two single number summaries
calc_auc(p)


# More ROC interpretation help here:
# https://towardsdatascience.com/understanding-auc-roc-curve-68b2303cc9c5




