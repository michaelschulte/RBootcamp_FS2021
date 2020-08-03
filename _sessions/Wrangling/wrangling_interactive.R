## Wrangling Interactive

# Lese Daten ein
appointments <- read_csv('1_Data/appointments.csv')
weather <- read_csv('1_Data/weather.csv')

# verarbeite appointments
appointments <- appointments %>%
  rename(alter_jahre = Age,
         geschlecht = Gender,
         bluthochdruck = Hypertension,
         diabetes = Diabetes,
         alkoholismus = Alcoholism) %>%
  mutate(krankheitsgrad = bluthochdruck + diabetes + alkoholismus) %>%
  mutate(geschlecht = case_when(
    geschlecht == "M" ~ "männlich",
    geschlecht == "F" ~ "weiblich")) %>%
    filter(geschlecht == 'weiblich',
           alter_jahre > 30)
  
# Verbinde Daten
verbunden <- appointments %>%
  left_join(weather)

# Verbinde Daten 2. Versuch
verbunden <- appointments %>%
  left_join(weather, by = c("AppointmentDay" =  "YYYYMMDD"))
