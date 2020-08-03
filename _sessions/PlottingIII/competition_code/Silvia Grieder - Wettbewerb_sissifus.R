## Wettbewerb

library(tidyverse)
library(viridis)
library(patchwork)

# Datensätze laden
touri <- read_csv("1_Data/Tourismus.csv")
euro <- read_csv("1_Data/Europa.csv")

# Von long in wide format ändern
touri_wide <- touri %>%
  mutate(tot = Besucher * Dauer) %>% 
  select(-Dauer, -Besucher) %>%
  pivot_wider(names_from = Jahr,
              values_from = tot) %>%
  mutate(tot_diff = `2018` - `2019`)

# Datensätze zusammenfügen
touri_euro <- touri_wide %>% 
  full_join(euro,
            by = "Land") %>% 
  subset(Region == "Europa")

# Colorvectors erzeugen
colvec <- case_when(touri_wide$tot_diff < -50 ~ "red", TRUE ~ "black")
colvec2 <- c(rep("black", 58), rep("red", 13))
             
# Plot mit Differenz abgetragen nach Land
mein_plot <- ggplot(data = touri_wide, aes(x = fct_reorder(Land, desc(tot_diff)),
                              y = tot_diff)) +
  geom_point(col = colvec) +
  geom_smooth(method = "lm") +
  labs(title = "Auf welche Länder sollte sich die Werbung konzentrieren?",
       subtitle = "Differenz in total verbrachter Nächte in Basel zwischen 2018 bis 2019",
       x = "Land",
       y = "2019 - 2018") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, colour = colvec2))

ggsave(filename = 'plot_sissifus.png',
       plot = mein_plot,
       device = "png",
       units = "in",
       width = 10,
       height = 5)
