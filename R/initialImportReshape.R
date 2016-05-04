# Number of conflicts and causalities

load("data/uppsalaConflictData.RData")

library(data.table); library(dplyr); library(tidyr); library(dygraphs)

eventDat = data.table(ged40.rg@data)

summary(eventDat)

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

groupTotalsSide = total %>%
  select(perp, year, n, role) %>%
  group_by(perp, year, role) %>%
  arrange()

groupTotals = total %>%
  select(perp, year, n) %>%
  group_by(perp, year) %>%
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