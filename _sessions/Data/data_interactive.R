## Data Interactive

# Vektoren
alter <- c(44, 65, 31, 27, 24)
groesse <- c(174, 180, 168, 209, 177)
gewicht <- c(113, 75, 56, 934, NA)

# Data frame
tibble(alter, groesse, gewicht)

# Einlesen
diamonds_path <- "1_Data/diamonds.csv"
diamonds <- read_csv(file = diamonds_path)
diamonds <- read_csv(file = diamonds_path,
                     col_names = c("karat", "cut", "farbe", "klarheit", "tiefe", "tabelle", "preis"))

# Logische Vektoren
diamonds$preis[diamonds$karat > 1]
diamonds$preis[diamonds$karat < 1]
diamonds$preis[!diamonds$karat > 1]
