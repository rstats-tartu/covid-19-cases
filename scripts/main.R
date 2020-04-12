#' ---
#' title: ""
#' author: ""
#' date: ""
#' ---
#' 
#' ![Render and Deploy Readme](https://github.com/rstats-tartu/covid-19-cases/workflows/Render%20and%20Deploy%20Readme/badge.svg)
#' 
#' # COVID-19 cases and deaths
#' rstats-tartu    
#' last update: `r Sys.time()`
#' 
#' ## Dataset
#' 
#' Daily COVID-19 data is from [European Centre for Disease Prevention 
#' and Control](https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide).
#'
#+ chunk_opts, include=FALSE
knitr::opts_chunk$set(message = FALSE)


#' ## Setting up data
#' 
#' Loading libraries.
#+ libs
pkg <- c("dplyr", "tidyr", "readr", "lubridate", "here", "ggplot2", "directlabels")
invisible(lapply(pkg, library, character.only = TRUE))


#' Importing downloaded ECDC daily COVID-19 dataset. 
#+ import
path <- here("data/COVID-19-geographic-distribution-worldwide.csv")
covid <- read_csv(path, col_types = cols(daterep = col_date(format = "%d/%m/%Y")))

#' Resetting timeline to days since first case in each country.
#+
covid_by_country <- covid %>% 
  filter(cases != 0, deaths != 0) %>% 
  group_by(country) %>% 
  mutate(tp = interval(Sys.Date(), daterep) / ddays(1),
         tp = tp - min(tp))

#' Calculating number of cases and deaths per country.
#' Keeping only informative rows.
#+ cumsums
lag_n <- 7
covid_cum <- covid_by_country %>% 
  mutate(cum_cases = with_order(tp, cumsum, cases),
         cum_deaths = with_order(tp, cumsum, deaths),
         risk = cum_deaths / cum_cases,
         risk_lag = cum_deaths / lag(cum_cases, n = lag_n, order_by = tp)) %>% 
  ungroup()

#' ## Cases and deaths in real time
#' 
#' Covid-19 cases.
#'  
#+ plot-cases-dates
covid_cum %>% 
  ggplot(aes(daterep, cum_cases, group = country)) +
  geom_line() +
  geom_dl(aes(label = geoid), method = list("first.points", cex = 0.8)) +
  scale_y_log10() +
  labs(x = "Date", 
       y = "Cumulative number of cases",
       caption = "Each line represents one country")

#' Covid-19 deaths. 
#' 
#+ plot-deaths-dates
covid_cum %>% 
  ggplot(aes(daterep, cum_deaths, group = country)) +
  geom_line() +
  geom_dl(aes(label = geoid), method = list("first.points", cex = 0.8)) +
  scale_y_log10() +
  labs(x = "Date", 
       y = "Cumulative number of deaths",
       caption = "Each line represents one country")


#' ## Cases and deaths on relative time scale
#' 
#' Number of cases per country.
#+ plot-cases
covid_cum %>% 
  ggplot(aes(tp, cum_cases, group = country)) +
  geom_line() +
  geom_dl(aes(label = geoid), method = list("last.points", cex = 0.8)) +
  scale_y_log10() +
  labs(x = "Days since first case in each country", 
       y = "Cumulative number of cases",
       caption = "Each line represents one country")

#' Number of deaths per country.
#+ plot-deaths
covid_cum %>% 
  ggplot(aes(tp, cum_deaths)) +
  geom_line(aes(group = country)) +
  geom_dl(aes(label = geoid), method = list("last.points", cex = 0.8)) +
  scale_y_log10() +
  labs(x = "Days since first death in each country", 
       y = "Cumulative number of deaths",
       caption = "Each line represents one country")

#' ## Risk of death
#' 
#+ plot-risk
covid_cum %>% 
  ggplot(aes(tp, risk)) +
  geom_line(aes(group = country)) +
  geom_dl(aes(label = geoid), method = list("last.points", cex = 0.8)) +
  scale_y_log10() +
  labs(x = "Time, days from first case", 
       y = "Risk of death",
       caption = "Each line represents one country")

#' Lagged (7 days) risk.
#' Risk of death relative to number of cases 7 days earlier.
#' 
#+ plot-risk-lag, warning=FALSE
covid_cum %>% 
  ggplot(aes(tp, risk_lag)) +
  geom_line(aes(group = country)) +
  geom_dl(aes(label = geoid), method = list("last.points", cex = 0.8)) +
  scale_y_log10() +
  labs(x = "Time, days from first case", 
       y = "Risk of death",
       caption = "Each line represents one country")

