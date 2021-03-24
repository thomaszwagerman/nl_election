library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(ggiraph)
library(widgetframe)
library(plotly)
library(ggpol)
df_results <- read.csv("Uitslag_alle_gemeenten_TK20170315.csv", sep=";")
shp_gemeentes <- st_read("Gemeentegrenzen_2019-shp/Gemeentegrenzen__voorlopig____kustlijn.shp")
colourscheme <- read.csv("partycolours.csv")

#sort in long format
df_long <- df_results %>% 
  pivot_longer(cols = VVD:Vrije.Democratische.Partij..VDP., names_to = "party", values_to = "votes") %>%
  select(regio =RegioNaam, code = RegioCode,party, votes) %>% 
  filter(!is.na(votes)) %>% 
  group_by(code) %>% 
  mutate(total = sum(votes)) %>% 
  slice_max(votes)

df_long$percentage <- (df_long$votes/df_long$total)*100
df_long$percentage <- round(df_long$percentage,1)

df_long$code <- gsub("G","",df_long$code)

#fix the names
df_long$party <- gsub("Partij.van.de.Arbeid..P.v.d.A..","PvdA",df_long$party)
df_long$party <- gsub("Democraten.66..D66.","D66",df_long$party)
df_long$party <- gsub("X50PLUS","Partij 50PLUS",df_long$party)
df_long$party <- gsub("Forum.voor.Democratie","FvD",df_long$party)
df_long$party <- gsub("Forum.voor.Democratie","FvD",df_long$party)
df_long$party <- gsub("SP..Socialistische.Partij.","SP",df_long$party)
df_long$party <- gsub("PVV..Partij.voor.de.Vrijheid.","PVV",df_long$party)
df_long$party <- gsub("Staatkundig.Gereformeerde.Partij..SGP.","SGP",df_long$party)
df_long$party <- gsub("Partij.voor.de.Dieren","PvdD",df_long$party)
df_long$party <- as.factor(df_long$party)
df_long$regio <- gsub("'","",df_long$regio)
df_long$regio <- gsub("-"," ",df_long$regio)
df_long <- left_join(df_long,colourscheme)

fptp_results <- table(df_long$party) %>% as.data.frame()
colnames(fptp_results) <- c("party","seats")
fptp_results <- left_join(fptp_results,colourscheme)

representatives_fptp <- ggplot(fptp_results) +
  ggpol::geom_parliament(aes(seats = seats, fill = party),color="black") + 
  #highlight the party in control of the House with a black line
  scale_fill_manual(values = fptp_results$colour, 
                    labels = fptp_results$party)+
  coord_fixed()+
  theme_void()

#link to spatial
shp_fptp <- left_join(shp_gemeentes, df_long, by = c("Code"="code")) %>% 
  select(-FID,Code,-Gemeentena,-Gemeenteco)
shp_fptp <- shp_fptp %>% 
  filter(percentage >0.1)


g <- ggplot() + 
  geom_sf_interactive(data = shp_fptp, aes(data_id = regio, tooltip = regio, fill = party))+
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_blank(),
        axis.line.y=element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  scale_fill_manual(values = shp_fptp$colour, 
                      limits = shp_fptp$party)
