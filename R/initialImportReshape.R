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

allDat = eventDat %>%
  select(side_a, year, side_b, deaths_b) %>%
  group_by(side_a, year, side_b) %>%
  arrange() %>%
  summarize(n = n())

countryTotals = eventDat %>%
  select(side_a, deaths_b) %>%
  group_by(side_a) %>%
  arrange() %>%
  summarize(n = n()) %>%
  mutate(year = "total")

#everything = inner_join(x = allDat, y = countryTotals, by = c("side_a", "year"))
