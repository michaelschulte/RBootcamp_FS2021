## Plotting Interactive

# Lese Daten ein
mcdonalds <- read_csv('1_Data/mcdonalds.csv')

# Kreire plot mit minimal theme
ggplot(mcdonalds, aes(x = Calories, y = SaturatedFat, col = Category)) +
  geom_point() +
  geom_smooth(col = "black") +
  labs(title = "McDonalds Nährwerte",
       subtitle = "Jeder Punkt ist ein Menü-Item",
       caption = "Quelle: Kaggle.com") +
  xlim(0, 1250) +
  theme_minimal()

# Verwende geom_text
ggplot(mcdonalds, aes(x = Sugars, 
                      y = Calories, 
                      col = Category, 
                      label = Item)) +
  geom_point() +
  geom_text(data = mcdonalds %>% 
              filter(Calories > 1100),
            col = "black", 
            check_overlap = TRUE, 
            hjust = "left") +
  xlim(0, 150) +
  ylim(0, 2000) +
  theme_minimal() +
  labs(title = "Ergänze Labels zu einem Plot mit geom_text()")
