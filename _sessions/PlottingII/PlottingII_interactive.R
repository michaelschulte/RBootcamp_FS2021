## Plotting II Interactive

# Lese Daten ein
verbrechen <- read_csv('1_Data/verbrechen.csv')

# Kreire plot als Funktion der Armut
ggplot(verbrechen, aes(x = prozent_metro, 
                       y = haeufigkeit)) +
  geom_point(alpha = .2) +
  labs(title = "Verbrechen",
       subtitle = "Haeufigkeit und Nutzung öffentlicher Verkehrsmittel") +
  scale_x_continuous(trans = 'pseudo_log') + 
  scale_y_continuous(trans = 'pseudo_log') + 
  facet_grid(prozent_armut > 10 ~ verbrechen)

# Kreire plot als Funktion der Dichte
ggplot(verbrechen, aes(x = prozent_metro, 
                       y = haeufigkeit)) +
  geom_point(alpha = .2) +
  labs(title = "Verbrechen",
       subtitle = "Haeufigkeit und Nutzung öffentlicher Verkehrsmittel") +
  scale_x_continuous(trans = 'pseudo_log') + 
  scale_y_continuous(trans = 'pseudo_log') + 
  facet_grid(bevoelkerung_dichte > 5000 ~ verbrechen)

# Speichere plot
verbrechen_gg <- ggplot(verbrechen, aes(x = prozent_metro, 
                                        y = haeufigkeit)) +
  geom_point(alpha = .2) +
  labs(title = "Verbrechen",
       subtitle = "Haeufigkeit und Nutzung öffentlicher Verkehrsmittel") +
  scale_x_continuous(trans = 'pseudo_log') + 
  scale_y_continuous(trans = 'pseudo_log') + 
  facet_grid(prozent_armut > 10 ~ verbrechen)

# Ergänze theme
verbrechen_gg + 
  theme(
    panel.background = element_rect(fill = 'white'),
    panel.grid.major = element_line(color = 'grey75', size = .25),
    panel.grid.minor = element_line(color = 'grey75', size = .1),
    strip.background = element_rect(fill = 'white'),
    strip.text = element_text(face = 'italic', size = 12, hjust = 1),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    panel.spacing = unit(1.1, units = "lines")
  )

# Speichere als pdf
ggsave(filename = "3_Figures/verbrechen.pdf", 
       device = "pdf", 
       plot = verbrechen_gg,
       width = 4, 
       height = 4, 
       units = "in")