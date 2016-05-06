# Number of conflicts and causalities

load("data/uppsalaConflictData.RData")

library(data.table); library(dplyr); library(tidyr); library(dygraphs)

eventDat = data.table(ged40.rg@data)

summary(eventDat)

sideA = eventDat %>%
  select(side_a, year, side_b, deaths_b, country, region) %>%
  group_by(side_a, year, side_b, country, region) %>%
  #arrange() %>%
  summarize(n = sum(deaths_b)) %>%
  mutate(role = "sideA") %>%
  rename(perp = side_a, victim = side_b)

sideB = eventDat %>%
  select(side_a, year, side_b, deaths_a, country, region) %>%
  group_by(side_b, year, side_a, country, region) %>%
  arrange() %>%
  summarize(n = sum(deaths_a)) %>%
  mutate(role = "sideB") %>%
  rename(perp = side_b, victim = side_a)

total = rbind(sideA, sideB) %>%
  arrange(perp, year)

groupTotalsSide = total %>%
  select(perp, year, n, role, country, region) %>%
  group_by(perp, year, role, country, region) %>%
  arrange()

groupTotals = total %>%
  select(perp, year, n, country, region) %>%
  group_by(perp, year, country, region) %>%
  summarize(tot = sum(n)) %>%
  arrange()

as.data.frame(groupTotals) %>%
  select(year, perp, tot) %>%
  arrange(year) %>%
  spread(perp, tot) %>%
  dygraph() %>%
  dyLegend(show = "onmouseover") %>%
  dyHighlight(highlightSeriesBackgroundAlpha = .2,
              hideOnMouseOut = FALSE) %>%
  dyCSS("dygraphLegend.css")

save(groupTotals, groupTotalsSide, file = "conflictDat.RData")
