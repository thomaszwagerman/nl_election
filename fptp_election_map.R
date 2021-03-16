library(dplyr)
library(sf)
library(ggplot2)
election_dir <- "C:/Users/thowag/Desktop/nl_election/"
df_results <- read.csv(paste0(election_dir, "Uitslag_alle_gemeenten_EP20190523.csv"), sep=";")
shp_gemeentes <- st_read(paste0(election_dir,"Gemeentegrenzen_2019-shp/Gemeentegrenzen__voorlopig____kustlijn.shp"))

#sort in long format
df_long <- df_results %>% 
  pivot_longer(cols = Partij.van.de.Arbeid..P.v.d.A..:De.Groenen, names_to = "party", values_to = "votes") %>%
  select(regio =RegioNaam, code = RegioCode,party, votes) %>% 
  group_by(code) %>% 
  slice_max(votes)

df_long$code <- gsub("G","",df_long$code)

#fix the names
df_long$party <- gsub("Partij.van.de.Arbeid..P.v.d.A..","PvdA",df_long$party)
df_long$party <- gsub("ChristenUnie.SGP","CU/SGP",df_long$party)
df_long$party <- gsub("Democraten.66..D66.","D66",df_long$party)
df_long$party <- gsub("X50PLUS","50PLUS",df_long$party)
df_long$party <- gsub("Forum.voor.Democratie","FvD",df_long$party)
df_long$party <- as.factor(df_long$party)

fptp_results <- table(df_long$party)

#link to spatial
shp_fptp <- left_join(shp_gemeentes, df_long, by = c("Code"="code")) %>% 
  select(-FID,Code,-Gemeentena,-Gemeenteco)

plot(shp_fptp)

ggplot() + 
  geom_sf(data = shp_fptp, aes(fill = party))+
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  scale_fill_manual(values = party_colours)                 # Adding custom colours for solid geoms (ribbon)

  party_colours <- c("#7d32a8","#2CC84D","#ed8600","#00AF3F","#701a1a", "#39A935",
                     "#DF111A","#0A2CCA")
  
  names(party_colours) <- levels(df_long$party)
  #0A2CCA VVD
  #2CC84D CDA
  #DF111A PVDA
  #39A935 (GROEN) #DD0031 (ROOD) GROENLINKS
  #7d32a8 50PLUS (gokje?)
  #701a1a FvD (gokje?)
  #00AF3F D66
  #ed8600 SGP (gokje), CU #00a7eb