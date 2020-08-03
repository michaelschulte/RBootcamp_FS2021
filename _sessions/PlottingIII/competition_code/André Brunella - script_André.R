## André Brunella
## 2020-02-15
## Competition Practical

library(tidyverse)
tourismus <- read_csv("1_Data/Tourismus.csv")
europa <- read_csv("1_Data/Europa.csv")

tourismus2 <- tourismus %>%
  mutate(Gaestetage = Besucher * Dauer) %>%
  select(Land, Region, Jahr, Gaestetage) %>%
  pivot_wider(names_from = "Jahr", values_from = "Gaestetage") %>%
  rename(Gaestetage2018 = "2018", Gaestetage2019 = "2019") %>%
  mutate(Gaestetagetrend = Gaestetage2019 - Gaestetage2018) %>%
  arrange(desc(Gaestetagetrend))

tourismusplot <- ggplot(data = tourismus2, mapping = aes(x = Region, y = Gaestetagetrend, col = Region, label = Land, size = 4)) +
  geom_point() +
  xlab("") +
  ylab("Veränderung Logiernächte") +
  labs(title = "In welche Herkunftsländer müssen wir investieren?", subtitle = "Trend Logiernächte 2018 - 2019 pro Herkunftland") +
  geom_text(check_overlap = TRUE, hjust = "left", nudge_x = 0.1, data = tourismus2 %>% filter(Gaestetagetrend > 100))

mein_theme <- theme(
  panel.background = element_rect(fill = "white"),
  panel.grid.major = element_line(color = "grey75", size = 0.25),
  panel.grid.minor = element_line(color = "grey75", size = 0.1),
  axis.title.x = element_text(color = "black", size = 14, margin = margin(t = 10)),
  axis.title.y = element_text(color = "black", size = 14, margin = margin(r = 10)),
  axis.text = element_text(color = "black", size = 14, margin = margin(t = 10)),
  legend.position = "none"
)

mein_plot <- tourismusplot + mein_theme

ggsave(filename = "plot_André.png",
       plot = mein_plot,
       device = "png",       
       units = 'in',
       width = 5,         # ggf. vergrössern/verkleinern
       height = 5         # ggf. vergrössern/verkleinern
)
