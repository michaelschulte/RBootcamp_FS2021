#Blutiger_Anfaenger
#15.02.2020
#Explorative Datenanalyse Wettbewerb

library(tidyverse)
library(viridis)
library(patchwork)

tourismus <- read_csv("1_Data/Tourismus.csv")
summary(tourismus)
names(tourismus)
tourismus

europa <- read_csv("1_Data/Europa.csv")
summary(europa)
names(europa)
europa

ggplot(tourismus, 
       mapping = aes(
         x = Jahr, 
         y = Besucher)) +
  geom_col() +
  labs(title = "Tourismus",
       subtitle = "to be defined")

ggplot(tourismus, 
       mapping = aes(
         x = Jahr, 
         y = Besucher)) +
  geom_col() +
  labs(title = "Tourismus",
       subtitle = "to be defined",
       x = "Anzahl Besucher",
       y = "Jahr")
tourismus

ggplot(tourismus, 
       mapping = aes(
         x = Jahr == 2018, 
         y = Besucher)) +
  geom_point() +
  labs(title = "Tourismus",
       subtitle = "to be defined",
       x = "Jahr",
       y = "Anzahl Besucher")

tourismus <- tourismus %>%
  group_by(Jahr) 



ggplot(tourismus, 
       mapping = aes(
         x = Jahr,
         y = Besucher) +
  geom_point() +
  labs(title = "Tourismus",
       subtitle = "to be defined",
       x = "Jahr",
       y = "Anzahl Besucher")  

ggplot(tourismus, 
         mapping = aes(
           x = Jahr, 
           y = Besucher)) +
    geom_point() +
    labs(title = "Tourismus",
         subtitle = "to be defined")

inner_join(tourismus) %>%
  select(termin_tag, , XX)
names(tourismus)
names(europa)

tourismus_europa <- tourismus %>%
  select(Land, Besucher) %>%
  inner_join(europa)

mein_plot <- ggplot(tourismus_europa %>% filter(Jahr == 2018),
       mapping = aes(
         x = Land, 
         y = BIP,
         label = Land)) +
  geom_col(fill=c(rep("grey75",15),"red",rep("grey75",18))) +
  geom_text(data = tourismus_europa, 
            col = "black", 
            check_overlap = TRUE, 
            hjust = 0) +
  theme(panel.background = element_rect (fill = "white"))

ggsave(filename = "plot_blutiger_anfaenger.png",
       plot = mein_plot,
       device = "png",       
       units = 'in',
       width = 10,        
       height = 5)
  





