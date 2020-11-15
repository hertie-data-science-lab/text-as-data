
allcmp20 <- read_csv("data/MPDataset_MPDS2020a.csv")
allcmp20$partyabbrev[allcmp20$partyname == "Pirates"] <- "Piraten"

cmp20 <- allcmp20 %>%
  select(edate, countryname, partyname, partyabbrev,
         total, voteper = pervote, uncoded = peruncod,
         matches("per\\d\\d\\d$")) %>% # per%d%d%d% are subcategories, so we ignore them
  mutate(edate = as.Date(edate, format = "%d/%m/%Y"),
         eyear = lubridate::year(edate), # make a nice year just in case we want to filter with it
         label = paste(partyabbrev, eyear, sep = ":"), # for graphing
         across(starts_with("per"), function(x) round(total * (x/100))))

itemcodes <- read_csv("data/itemcodes.csv")
counts <- select(cmp20, starts_with("per"))
mat <- data.matrix(counts)
rownames(mat) <- cmp20$label
colnames(mat) <- itemcodes$name

## Analysis: Germany

oldest <- as.Date("1990-01-01")
de_mat <- mat[cmp20$edate > oldest & cmp20$countryname == "Germany",]
de_rest <- filter(select(cmp20, -starts_with("per")),
                  edate > oldest,
                  cmp20$countryname == "Germany")

library(ca)
mod1 <- ca(de_mat)

