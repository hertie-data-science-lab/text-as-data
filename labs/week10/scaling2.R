library(tidyverse)
library(ca)

itemcodes <- read_csv("data/itemcodes.csv")

allcmp20 <- read_csv("data/MPDataset_MPDS2020a.csv")
allcmp20$partyabbrev[allcmp20$partyname == "Pirates"] <- "Piraten"
cmp20 <- allcmp20 %>%
  select(edate, countryname, partyname, partyabbrev,
         total, voteper = pervote, uncoded = peruncod,
         matches("per\\d\\d\\d$")) %>% # per%d%d%d% are subcategories, so we ignore them
  mutate(edate = as.Date(edate, format = "%d/%m/%Y"),
         eyear = lubridate::year(edate), # make a nice year just in case we want to filter with it
         label = paste(partyabbrev, eyear, sep = ":"), # for graphing
         across(c(starts_with("per"), "uncoded"), function(x) round(total * (x/100))))
write_csv(cmp20, "data/cmp20.csv")

oldest <- as.Date("1990-01-01")
de <- read_csv("data/cmp20.csv") %>%
  filter(edate > oldest, countryname == "Germany")

counts <- select(de, starts_with("per"))
de_rest <- select(de, -starts_with("per"))

de_mat <- data.matrix(counts)
dimnames(de_mat) <- list("party-year" = de_rest$label,
                         topic = itemcodes$name)
de_mat <- data.matrix(counts)
rownames(de_mat) <- de_rest$label
colnames(de_mat) <- itemcodes$name

################################## a little exploration

camod <- ca(de_mat)
plot(camod)

## no AfD 2017
afd17 <- which(rownames(de_mat) == "AfD:2017")
de_mat_a <- de_mat[-afd17, ]
camod_a <- ca(de_mat_a)

plot(camod_a)

## projected this time
camod_s <- ca(de_mat, suprow = afd17)

plot(camod_s)

## other dimensions
plot(camod, dim = c(1,3))

## how did this change the items?
cor(cbind(camod$colcoord[,1:3], camod_a$colcoord[,1:3]))

betas <- rownames_to_column(data.frame(camod$colcoord[,1:3]), var = "topic")
betas_a <- rownames_to_column(data.frame(camod_a$colcoord[,1:3]), var = "topic")

data.frame(camod$rowcoord[,1:3])

filter(betas, topic == "Internationalism: Negative")
filter(betas_a, topic == "Internationalism: Negative")


