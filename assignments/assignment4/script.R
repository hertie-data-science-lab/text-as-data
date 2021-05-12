library(tidyverse)
library(ggrepel)
library(ca)

#comparison time

allcmp20 <- read_csv("data/MPDataset_MPDS2020a.csv")
allcmp20$partyabbrev[allcmp20$partyname == "Pirates"] <- "Piraten"

cmp20 <- allcmp20 %>%
  select(edate, countryname, partyname, partyabbrev,
         total, voteper = pervote, uncoded = peruncod,
         matches("per\\d\\d\\d$")) # per%d%d%d% are subcategories, so we ignore them


filter(cmp20, countryname == "Germany") %>%
  select(starts_with("per")) %>%
  summarise(across(.fns = function(x) mean(as.numeric(x != 0))))

filter(cmp20, countryname == "Germany") %>% mutate(edate =  as.Date(edate, format = "%d/%m/%Y")) %>%
  select(edate,starts_with("per")) %>%
  pivot_longer(-edate, names_to = "topic") %>%
  group_by(edate, topic) %>%
  summarise(mean_prop = mean(value))

cmp20 %>%
  mutate(edate = as.Date(edate, format = "%d/%m/%Y"),
         eyear = lubridate::year(edate), # make a nice year just in case we want to filter with it
         label = paste(partyabbrev, eyear, sep = ":"), # for graphing
         across(starts_with("per"), function(x) round(total * (x/100))))

itemcodes <- read_csv("data/itemcodes.csv")
counts <- select(cmp20, starts_with("per"))
mat <- data.matrix(counts)
rownames(mat) <- cmp20$label
colnames(mat) <- itemcodes$name

## Analysis: Germany alone

oldest <- as.Date("1990-01-01")
de_mat <- mat[cmp20$edate > oldest & cmp20$countryname == "Germany",]
de_rest <- filter(select(cmp20, -starts_with("per")),
                  edate > oldest,
                  cmp20$countryname == "Germany")


mod_de <- ca(de_mat)

theta_de <- data.frame(partyyear = de_rest$label,
                       mod_de$rowcoord[,1:2])
ggplot(theta_de, aes(Dim1, Dim2, label = partyyear)) +
  geom_point() +
  geom_text_repel(alpha = 0.5)

# hmm, maybe we'll flip both axes to better correspond to left right
theta_de <- mutate(theta_de,
                   Dim1 = -Dim1,
                   Dim2 = -Dim2)
ggplot(theta_de, aes(Dim1, Dim2, label = partyyear)) +
  geom_point() +
  geom_text_repel(alpha = 0.5)

ggplot(

## Analysis: Germany and Switzerland
desw_mat <- mat[cmp20$edate > oldest &
                cmp20$countryname %in% c("Germany", "Switzerland")]
desw_rest <- filter(select(cmp20, -starts_with("per")),
                  edate > oldest,
                  cmp20$countryname %in% c("Germany", "Switzerland"))
proj <- which(desw_rest$countryname == "Switzerland")
mod_desw <- ca(desw_mat, suprow = proj)




