
require(tidyverse)

a = readr::read_delim('~/Downloads/winequality-red.csv',delim=';')
b = readr::read_delim('~/Downloads/winequality-white.csv',delim=';')

a = a %>% mutate_if(is.character, as.numeric) %>% mutate(Farbe = "rot")
b = b %>% mutate_if(is.character, as.numeric) %>% mutate(Farbe = "weiss")

wein = rbind(a, b)

wein = wein %>% 
  rename(
    "Gelöste_Säure" = "fixed acidity",
    'Freie_Säure' = 'volatile acidity',
    'Citronensäure' = 'citric acid',
    'Restzucker' = 'residual sugar',
    'Chloride' = 'chlorides',
    'Freie_Schwefeldioxide' = 'free sulfur dioxide',
    'Gesamt_Schwefeldioxide' = 'total sulfur dioxide',
    'Dichte' = 'density',
    'pH_Wert' = 'pH',
    'Sulphate' = 'sulphates',
    'Alkohol' = 'alcohol',
    'Qualität' = 'quality',
    'Farbe' = 'Farbe'
    ) %>% 
  select(Qualität, Farbe, everything())

write_csv(wein,'_sessions/LinearModelsI/1_Data/wein.csv')

cor(wein$`Gelöste Säure`, wein$`pH-Wert`)


as.numeric(a$chlorides)

summary(lm(quality ~ ., data = as.data.frame(scale(a))))
plot(a$`fixed acidity`, a$`volatile acidity`)

m = summary(lm(Qualität ~ ., data = wein))

summary(glm(factor(Farbe) ~ ., data = wein,family='binomial'))




        