library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(ggiraph)
library(ggparliament)
df_results <- read.csv("Uitslag_alle_gemeenten_TK20170315.csv", sep=";")
shp_gemeentes <- st_read("Gemeentegrenzen_2019-shp/Gemeentegrenzen__voorlopig____kustlijn.shp")
colourscheme <- read.csv("partycolours.csv")

#sort in long format
df_pie <- df_results %>% 
  pivot_longer(cols = VVD:Vrije.Democratische.Partij..VDP., names_to = "party", values_to = "votes") %>%
  select(regio =RegioNaam, code = RegioCode,party, votes) %>% 
  filter(!is.na(votes)) %>% 
  group_by(code) %>% 
  mutate(total = sum(votes)) 

df_pie$percentage <- (df_pie$votes/df_pie$total)*100
df_pie$percentage <- round(df_pie$percentage,1)
# 
df_pie$party <- gsub("Partij.van.de.Arbeid..P.v.d.A..","PvdA",df_pie$party)
df_pie$party <- gsub("Democraten.66..D66.","D66",df_pie$party)
df_pie$party <- gsub("X50PLUS","Partij 50PLUS",df_pie$party)
df_pie$party <- gsub("Forum.voor.Democratie","FvD",df_pie$party)
df_pie$party <- gsub("Forum.voor.Democratie","FvD",df_pie$party)
df_pie$party <- gsub("SP..Socialistische.Partij.","SP",df_pie$party)
df_pie$party <- gsub("PVV..Partij.voor.de.Vrijheid.","PVV",df_pie$party)
df_pie$party <- gsub("Staatkundig.Gereformeerde.Partij..SGP.","SGP",df_pie$party)
df_pie$party <- gsub("Partij.voor.de.Dieren","PvdD",df_pie$party)
df_pie$party <- as.factor(df_pie$party)

df_pie$party <- as.character(df_pie$party)
df_pie$percentage <- as.numeric(df_pie$percentage) 

nijmegen <- df_pie %>% 
  filter(regio == "Nijmegen") %>%
  dplyr::ungroup() %>% 
  select(party,percentage) %>% 
  as.data.frame()

pie_plot_list <- lapply(unique(df_pie$regio), function(i) {
  regio_df <- df_pie %>% 
    filter(regio == i) %>%
    dplyr::ungroup() %>% 
    select(party,percentage) %>%
    filter(percentage >0.1) %>% 
    as.data.frame()
  regio_df <- regio_df[order(regio_df$percentage,decreasing = T),]
  regio_df <- left_join(regio_df,colourscheme)
  
  ggplot(regio_df) +
    geom_bar(aes(x = "", y = percentage, fill = party),
             stat = "identity", colour = "black", width =1) +
    #geom_text(aes(x = "", y = pct, label = percent(pct)), position = position_stack(vjust = 0.5))+
    coord_polar("y", start = 0) +
    labs(x = NULL, y = NULL, fill = NULL, 
         title = paste("Verkiezingsuitslag",i)) +
    theme_classic()+
    scale_fill_manual(values = regio_df$colour, 
                      limits = regio_df$party)+ 
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5, color = "black"))
})

# 
# nijmegen <- nijmegen[order(nijmegen$percentage,decreasing = T),]
# nijmegen <- left_join(nijmegen,colourscheme)
# 
# nijmegen$party <- factor(nijmegen$party, levels = rev(nijmegen$party))
# nijmegen <- nijmegen %>% 
#   filter(percentage >0.1)
# 
# ggplot(nijmegen) +
#   geom_bar(aes(x = "", y = percentage, fill = party),
#            stat = "identity", colour = "black", width =1) +
#   #geom_text(aes(x = "", y = pct, label = percent(pct)), position = position_stack(vjust = 0.5))+
#   coord_polar("y", start = 0) +
#   labs(x = NULL, y = NULL, fill = NULL, 
#        title = "Verkiezingsuitslag Nijmegen") +
#   theme_classic()+
#   scale_fill_manual(values = nijmegen$colour, 
#                     limits = nijmegen$party)+ 
#   theme(axis.line = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         plot.title = element_text(hjust = 0.5, color = "black"))
