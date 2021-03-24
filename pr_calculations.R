library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(ggiraph)
library(ggpol)
df_results <- read.csv("Uitslag_alle_gemeenten_TK20170315.csv", sep=";")
shp_gemeentes <- st_read("Gemeentegrenzen_2019-shp/Gemeentegrenzen__voorlopig____kustlijn.shp")
colourscheme <- read.csv( "partycolours.csv")

#sort in long format
df_results[is.na(df_results)] <- 0

df_pr <- df_results %>% 
  pivot_longer(cols = VVD:Vrije.Democratische.Partij..VDP., names_to = "party", values_to = "votes") %>%
  select(party, votes) %>% 
  group_by(party) %>% 
  summarise(votes=sum(votes))

df_pr$party <- gsub("Partij.van.de.Arbeid..P.v.d.A..","PvdA",df_pr$party)
df_pr$party <- gsub("Democraten.66..D66.","D66",df_pr$party)
df_pr$party <- gsub("X50PLUS","Partij 50PLUS",df_pr$party)
df_pr$party <- gsub("Forum.voor.Democratie","FvD",df_pr$party)
df_pr$party <- gsub("Forum.voor.Democratie","FvD",df_pr$party)
df_pr$party <- gsub("SP..Socialistische.Partij.","SP",df_pr$party)
df_pr$party <- gsub("PVV..Partij.voor.de.Vrijheid.","PVV",df_pr$party)
df_pr$party <- gsub("Staatkundig.Gereformeerde.Partij..SGP.","SGP",df_pr$party)
df_pr$party <- gsub("Partij.voor.de.Dieren","PvdD",df_pr$party)
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

df_pr$seats<- dHondt(df_pr$votes,df_pr$party,150)
df_pr <- df_pr %>% 
  filter(seats>0) %>% 
  as.data.frame()
df_pr <- df_pr[order(df_pr$seats,decreasing =T),] 

df_pr <- left_join(df_pr,colourscheme)

representatives_pr <- ggplot(df_pr) +
  ggpol::geom_parliament(aes(seats = seats, fill = party),color="black") + 
  #highlight the party in control of the House with a black line
  scale_fill_manual(values = df_pr$colour, 
                      labels = df_pr$party)+
  coord_fixed()+
  theme_void()
