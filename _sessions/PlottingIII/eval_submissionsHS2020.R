# Competition Analysis
# MSM
# Oct 2020

# load packages
library(googlesheets4)
library(tidyverse)
library(patchwork)
# read in data 
raw <- read_sheet(ss = '1qODIQcqM8VrWeEyGl2bCfqFqRE9MBtkJyMXiksSzYVU')
# generate graphs for each group  
graph1 <- 
  raw %>%
  select(2:4) %>%
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  mutate(graph = 'Covid-Boot-Camp')

graph2 <- 
  raw %>%
  select(5:7) %>%
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  mutate(graph = 'Arr-PiRaten')

graph3 <- 
  raw %>%
  select(8:10) %>%
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  mutate(graph = 'We-R-lost')

graph4 <- 
  raw %>%
  select(11:13) %>%
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  mutate(graph = 'R-Pandemi')
# bind together, make long
result <- 
bind_rows(graph1, graph2, graph3, graph4) %>%
  rename('Ästhetik' = 'Ist die Grafik ästhetisch ansprechend?', 
         'Überzeugend' = 'Überzeugen dich die dargestellten Datenmuster (unabhängig von der Fragestellung)?', 
         'Relevant' = 'Findest du die dargestellten Datenmuster entscheidungsrelevant in Bezug auf die Fragestellung?') %>%
  pivot_longer(cols = 1:3, names_to = 'Frage')
# plot single result 
single <- ggplot(result, aes(Frage, value)) +
 geom_point(stat = 'identity') + 
 facet_wrap(~graph, nrow = 1) +
  ylim(1,5) +
  labs(y = 'Bewertung',
       x = 'Bereich',
       caption = 'RBootcamp Bern 2020',
       title = 'Ergebnisse Plotting Competition') +
  theme_bw()
# plot overall
overall_result <- 
  result %>%
    group_by(graph) %>%
    summarise(overall = mean(value))
  
overall_graph <- ggplot(overall_result, aes(reorder(graph, -overall), overall)) +
  geom_point() +
  labs(y = 'Bewertung Across',
       x = 'Gruppe',
       caption = 'RBootcamp Bern 2020',
       title = 'Ergebnisse Plotting Competition') +
  theme_bw()
# show next to each other
overall_graph / single
