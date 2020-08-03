### Wrangling II
### Explorative Datenanalyse mit R
### Februar 2020

library(tidyverse)


kc_house <- read_csv(file = "1_Data/kc_house.csv")

# Datensatz vorbereiten (Aufgaben B1, B4-B6)
kc_house <- kc_house %>%
  mutate(qm_wohnraum = qf_wohnraum * 0.093,
         qm_dachstock = qf_dachstock * 0.093,
         qm_keller  = qf_keller * 0.093,
         qm_total = qm_wohnraum + qm_dachstock + qm_keller,
         villa = case_when(
           qm_total > 750 ~ "ja",
           qm_total <= 750~ "nein"))

# E6. Berechne die mittlere Anzahl Stockwerke (`stoecke`) und Badezimmer
# (`badezimmer`) von Villen (`villa`).


# G1. Dein Freund Theodorus interessiert sich für Häuser einer bestimmten Gegend,
# nämlich mit den Postleitzahlen (`postleitzahl`) 98001, 98109, 98117 und 98199.
# Stelle ihm einen neuen Datensatz `theodorus` zusammen, welcher nur die Häuser
# dieser Gegend enthält. Tipp: der `%in%` Operator testet ob Elemente des Vektors
# auf der rechten Seite im Vektor auf der linken Seite enthalten sind.


# G2. Berechne für Theodorus den mittleren, den Median-, den Minimum-, den
# Maximumpreis, sowie die Anzahl Häuser pro Gruppe, separat für die
# Postleitzahlen und ob es eine Villa ist.

# X1. Welche Postleitzahl hat den höchsten Anteil Häuser, welche am Wasser gebaut
# sind (`ufer`)? Printe nur diese Zeile des Datensatzes.