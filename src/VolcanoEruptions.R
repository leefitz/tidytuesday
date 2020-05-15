library(tidyverse)
library(magrittr)
library(lubridate)
library(gtools)

eruptionsraw <- read_csv("data/2020/2020-05-12/eruptions.csv")
eventsraw <- read_csv("data/2020/2020-05-12/events.csv")
sulfurraw <- read_csv("data/2020/2020-05-12/sulfur.csv")
treeringsraw <- read_csv("data/2020/2020-05-12/tree_rings.csv")
volcanoraw <- read_csv("data/2020/2020-05-12/volcano.csv")

eruptions <-
  eruptionsraw %>% 
  mutate(
    startdatetype = case_when(
      is.na(start_year) ~ "None",
      is.na(start_month) | start_month == 0 ~ "Year",
      is.na(start_day)  | start_day == 0 ~ "Year Month",
      TRUE ~ "Year Month Day"
    ),
    startdate = case_when(
      startdatetype == "None" ~ NA_Date_,
      startdatetype == "Year" ~ make_date(start_year, 01, 01),
      startdatetype == "Year Month" ~ make_date(start_year, start_month, 01),
      startdatetype == "Year Month Day" ~ make_date(start_year, start_month, start_day)
    ),
    enddatetype = case_when(
      is.na(end_year) ~ "None",
      is.na(end_month) | end_month == 0 ~ "Year",
      is.na(end_day) | end_day == 0 ~ "Year Month",
      TRUE ~ "Year Month Day"
    ),
    enddate = case_when(
      enddatetype == "None" ~ startdate, #consider adding a constant or making NA?
      enddatetype == "Year" ~ make_date(end_year, 01, 01),
      enddatetype == "Year Month" ~ make_date(end_year, end_month, 01),
      enddatetype == "Year Month Day" ~ make_date(end_year, end_month, end_day)
    )
  ) %>% 
  left_join(
    eventsraw %>% 
      group_by(eruption_number) %>% 
      summarise(eventcount = n()) %>% 
      ungroup(),
    by = "eruption_number"
  ) %>% 
  mutate(eventcount = eventcount %>% replace_na(0))

events <- 
  eventsraw %>% 
  left_join( #bring in eruption dates for event date assumptions
    eruptions %>% 
      rename(
        eruptionstartdate = startdate,
        eruptionenddate = enddate
      ) %>% 
      select(-eventcount),
    by = c(
      "volcano_number",
      "volcano_name",
      "eruption_number",
      "eruption_start_year" = "start_year"
    )
  ) %>% 
  mutate(
    eventdatetype = case_when(
      is.na(event_date_year) ~ "None",
      is.na(event_date_month) ~ "Year",
      is.na(event_date_day) ~ "Year Month",
      TRUE ~ "Year Month Day"
    ),
    eventdate = case_when(
      eventdatetype == "None" ~ eruptionstartdate,
      eventdatetype == "Year" ~ make_date(event_date_year, 01, 01),
      eventdatetype == "Year Month" ~ make_date(event_date_year, event_date_month, 01),
      eventdatetype == "Year Month Day" ~ make_date(event_date_year, event_date_month, event_date_day)
    )
  )

volcano <- 
  volcanoraw %>% 
  pivot_longer(
    contains("_rock_"),
    names_to = "rocktype",
    names_pattern = "(.*_\\d)",
    values_to = "rockname"
  ) %>% 
  filter(rockname != chr(160)) %>% 
  mutate(
    rocktype = rocktype %>% str_sub(1, -3) %>% str_replace("_", " ") %>% str_to_title(),
    seen = TRUE
    ) %>% 
  unite(rock, rocktype:rockname, sep = " - ") %>% 
  pivot_wider(
    names_from = rock,
    values_from = seen,
    values_fill = list(seen = FALSE)
  )
