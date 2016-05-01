# Number of conflicts and causalities

load("data/uppsalaConflictData.RData")

library(data.table); library(dplyr); library(tidyr)

eventDat = data.table(ged40.rg@data)

summary(eventDat)

usOnly = eventDat %>%
  filter(side_a == "Government of United States of America") %>%
  select(side_a, year, side_b, deaths_b) %>%
  group_by(side_a, year, side_b) %>%
  summarize(n = n()) %>%
  sum(n)

sideA = eventDat %>%
  select(side_a, year, side_b, deaths_b) %>%
  group_by(side_a, year, side_b) %>%
  arrange() %>%
  summarize(n = n()) %>%
  mutate(role = "sideA") %>%
  rename(perp = side_a, victim = side_b)

sideB = eventDat %>%
  select(side_a, year, side_b, deaths_a) %>%
  group_by(side_b, year, side_a) %>%
  arrange() %>%
  summarize(n = n()) %>%
  mutate(role = "sideB") %>%
  rename(perp = side_b, victim = side_a)

total = rbind(sideA, sideB) %>%
  arrange(perp, year)

countryTotals = eventDat %>%
  select(side_a, deaths_b) %>%
  group_by(side_a) %>%
  arrange() %>%
  summarize(total = n())