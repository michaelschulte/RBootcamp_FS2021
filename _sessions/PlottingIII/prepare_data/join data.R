

library(tidyverse) ; library(ggrepel)
# Lade Tourismus Daten
tour <- read_csv('_sessions/PlottingIII/1_Data/Tourismus.csv') 
europa <- read_csv('_sessions/PlottingIII/1_Data/Europa.csv') 
# Verbinde Nächte mit Äquivalenzeinkommen
tour %>%
  mutate(Nächte = Besucher * Dauer) %>%
  left_join(europa) %>%
  ggplot(aes(x = Äquivalenzeinkommen, 
             y = Nächte,
             label = Land)) +
  scale_y_continuous(trans = 'log2') + 
  geom_point() +
  geom_label_repel(size = 2) +
  theme_bw()


tour <- read_csv('_sessions/PlottingIII/1_Data/Tourismus.csv') 
europa <- read_csv('_sessions/PlottingIII/1_Data/Europa.csv') 
tour %>%
  mutate(Nächte = Besucher * Dauer) %>%
  left_join(europa) %>%
  write_csv('1_Data/tourismus_basel.csv')
