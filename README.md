Covid-19 cases
================
rstats-tartu
2020-03-22 14:10:37

Daily covid-19 data is from [European Centre for Disease Prevention and
Control](https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide).

Loading libraries

``` r
library("dplyr")
library("readxl")
library("lubridate")
library("here")
library("glue")
library("brms")
library("ggplot2")
```

Downloading dataset

``` r
if (!dir.exists(here("data"))) {
  system(glue("mkdir {here('data')}"))
}
yesterday <- Sys.Date() - 1
dataset <- here(glue("data/COVID-19-geographic-disbtribution-worldwide-{yesterday}.xlsx"))
if (!file.exists(dataset)) {
  url <- glue("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-{yesterday}.xlsx")
  system(glue("curl -o {dataset} {url}"))
} 
```

Importing downloaded dataset.

``` r
sheet_1 <- excel_sheets(dataset)[1]
covid <- read_excel(dataset, sheet = sheet_1)
covid <- covid %>% 
  rename(Country = `Countries and territories`) %>% 
  rename_all(tolower)
```

Days since first case in each country

``` r
covid <- covid %>% 
  group_by(country) %>% 
  mutate(day = interval(daterep, yesterday) / ddays(1))
```

Number of cases per country.

``` r
covid %>% 
  mutate(cum_cases = cumsum(cases)) %>% 
  ungroup() %>% 
  ggplot(aes(day, cum_cases)) +
  geom_line(aes(group = country)) +
  labs(x = "Days since first case in each country", 
       y = "Cumulative number of cases",
       caption = "Each line represents one country")
```

![](/__w/covid-19-cases/covid-19-cases/README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Number of deaths per country.

``` r
covid %>% 
  mutate(cum_deaths = cumsum(deaths)) %>% 
  ungroup() %>% 
  ggplot(aes(day, cum_deaths)) +
  geom_line(aes(group = country)) +
  labs(x = "Days since first death in each country", 
       y = "Cumulative number of deaths",
       caption = "Each line represents one country")
```

![](/__w/covid-19-cases/covid-19-cases/README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
