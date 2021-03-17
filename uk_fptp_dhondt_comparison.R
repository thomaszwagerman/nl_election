library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(ggiraph)
library(ggpol)
election_dir <- "C:/Users/thowag/Desktop/nl_election/"
df_results <- read.csv(paste0(election_dir, "UK/HoC-GE2019-results-by-constituency-csv.csv"))
uk_colourscheme <- read.csv(paste0(election_dir, "UK/uk_colourscheme.csv"))

#results if D'hondt method was used
df_ukpr <- df_results %>% 
  pivot_longer(cols = con:alliance, names_to = "party", values_to = "votes") %>%
  select(party, votes) %>% 
  group_by(party) %>% 
  summarise(votes=sum(votes))

dHondt <- function(votes, parties, n_seats = 150) {
  
  divisor.mat           <- sum(votes) / sapply(votes, "/", seq(1, n_seats, 1))
  colnames(divisor.mat) <- parties
  
  m.mat     <- tidyr::gather(as.data.frame(divisor.mat), key="name", value="value",
                             everything())
  m.mat     <- m.mat[rank(m.mat$value, ties.method = "random") <= n_seats, ]
  rle.seats <- rle(as.character(m.mat$name))
  
  if (sum(rle.seats$length) != n_seats)
    stop(paste("Number of seats distributed not equal to", n_seats))
  
  # fill up the vector with parties that got no seats
  if (any(!(parties %in% rle.seats$values))) {
    # add parties
    missing_parties <- parties[!(parties %in% rle.seats$values)]
    for (party in missing_parties) {
      rle.seats$lengths <- c(rle.seats$lengths, 0)
      rle.seats$values  <- c(rle.seats$values, party)
    }
    # sort results
    rle.seats$lengths <- rle.seats$lengths[match(parties, rle.seats$values)]
    rle.seats$values  <- rle.seats$values[match(parties, rle.seats$values)]
  }
  
  rle.seats$length
  
}

df_ukpr$seats<- dHondt(df_ukpr$votes,df_ukpr$party,650)
df_ukpr <- df_ukpr %>% 
  filter(seats>0) %>% 
  as.data.frame()
df_ukpr <- df_ukpr[order(df_ukpr$seats,decreasing =T),] 
df_ukpr <- left_join(df_ukpr,uk_colourscheme)

hoc_pr <- ggplot(df_ukpr) +
  ggpol::geom_parliament(aes(seats = seats, fill = party),color="black") + 
  #highlight the party in control of the House with a black line
  scale_fill_manual(values = df_ukpr$colour, 
                    labels = df_ukpr$party)+
  coord_fixed()+
  theme_void()

#actual results---
df_ukfptp <- df_results %>% 
  select(constituency_name,ons_id,first_party)
df_ukfptp <- table(df_ukfptp$first_party) %>% as.data.frame()
colnames(df_ukfptp) <- c("party","seats")
df_ukfptp <- df_ukfptp[order(df_ukfptp$seats,decreasing =T),] 
df_ukfptp$party <- tolower(df_ukfptp$party)
df_ukfptp <- left_join(df_ukfptp,uk_colourscheme)

hoc_fptp <- ggplot(df_ukfptp) +
  ggpol::geom_parliament(aes(seats = seats, fill = party),color="black") + 
  #highlight the party in control of the House with a black line
  scale_fill_manual(values = df_ukfptp$colour, 
                    labels = df_ukfptp$party)+
  coord_fixed()+
  theme_void()
