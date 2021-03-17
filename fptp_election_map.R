library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(ggiraph)
library(widgetframe)
library(plotly)
election_dir <- "C:/Users/thowag/Desktop/nl_election/"
df_results <- read.csv(paste0(election_dir, "Uitslag_alle_gemeenten_TK20170315.csv"), sep=";")
shp_gemeentes <- st_read(paste0(election_dir,"Gemeentegrenzen_2019-shp/Gemeentegrenzen__voorlopig____kustlijn.shp"))
colourscheme <- read.csv(paste0(election_dir, "partycolours.csv"))

shp_gemeentes[!shp_gemeentes$Code %in% df_long$code,]
df_long[!df_long$code %in% shp_gemeentes$Code,]

#sort in long format
df_long <- df_results %>% 
  pivot_longer(cols = VVD:Vrije.Democratische.Partij..VDP., names_to = "party", values_to = "votes") %>%
  select(regio =ï..RegioNaam, code = RegioCode,party, votes) %>% 
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
df_long$party <- as.factor(df_long$party)

df_long$regio <- gsub("'","",df_long$regio)
df_long$regio <- gsub("-"," ",df_long$regio)

df_long <- left_join(df_long,colourscheme)


fptp_results <- table(df_long$party) %>% as.data.frame()
colnames(fptp_results) <- c("party","seats")
fptp_results <- left_join(fptp_results,colourscheme)
nl_house <- parliament_data(election_data = fptp_results,
                            type = "semicircle",
                            parl_rows = 7,
                            party_seats = fptp_results$seats)
sum(fptp_results$seats)/2
representatives <- ggplot(nl_house, aes(x, y, colour = party)) +
  geom_parliament_seats() + 
  #highlight the party in control of the House with a black line
  #geom_highlight_government(government == 1) +
  #draw majority threshold
  draw_majoritythreshold(n = 196, label = TRUE, type = 'semicircle')+
  #set theme_ggparliament
  theme_ggparliament() +
  #other aesthetics
  labs(colour = NULL, 
       title = "Nederland FPTP",
       subtitle = "Party that controls the House highlighted.") +
  scale_colour_manual(values = nl_house$colour, 
                      limits = nl_house$party)

#link to spatial
shp_fptp <- left_join(shp_gemeentes, df_long, by = c("Code"="code")) %>% 
  select(-FID,Code,-Gemeentena,-Gemeenteco)
shp_fptp <- left_join(shp_fptp,colourscheme)
shp_fptp <- shp_fptp %>% 
  filter(!is.na(party))

#0A2CCA VVD
#2CC84D CDA
#DF111A PVDA
#39A935 (GROEN) #DD0031 (ROOD) GROENLINKS
#7d32a8 50PLUS (gokje?)
#701a1a FvD (gokje?)
#00AF3F D66
#ed8600 SGP (gokje), CU #00a7eb

g <- ggplot() + 
  geom_sf(data = shp_fptp, aes(fill = party))+
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_blank(),
        axis.line.y=element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  scale_fill_manual(values = shp_fptp$colour, 
                      limits = shp_fptp$party)
