
![Render and Deploy
Readme](https://github.com/rstats-tartu/covid-19-cases/workflows/Render%20and%20Deploy%20Readme/badge.svg)

# COVID-19 cases and deaths

rstats-tartu  
last update: 2020-07-21 21:10:18

## Contents

[Dataset](#dataset)  
[Worldwide cases and deaths](#worldwide-cases-and-deaths)  
[Cases and deaths on relative time
scale](#cases-and-deaths-on-relative-time-scale)  
[Risk of death](#risk-of-death)  
[COVID-19 cases in Estonia](#covid-19-cases-in-estonia)  
[Estonian COVID-19 tests handling](#estonian-covid-19-tests-handling)

## Intro

Small selection of graphs illustrating daily developments in COVID-19
epidemic. Code is shown on purpose, in case you want to recreate these
plots.

**Phylogenetic- and geographic distribution of SARS-CoV-2**, COVID-19
causing virus, is available on
<https://auspice.credibleinterval.ee/sarscov2>.

SARS-Cov-2 phylogenetic tree is based solely on sequences published on
NCBI <https://www.ncbi.nlm.nih.gov/genbank/sars-cov-2-seqs/>

## Dataset

Daily COVID-19 worldwide data is from [European Centre for Disease
Prevention and
Control](https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide).

Estonian COVID-19 dataset was downloaded from Estonian Healthboard
<https://opendata.digilugu.ee/opendata_covid19_test_results.csv>

Datasets were downloaded using [script/get\_data.R](script/get_data.R)
script and saved to `data` folder.

Report was rendered using `rmarkdown::render()` command. To render, you
can run following command from shell:

``` bash
Rscript -e "rmarkdown::render('scripts/main.R', output_format = rmarkdown::github_document(), output_file = 'README.md')"
```

## Setting up data

Loading libraries.

``` r
pkg <- c("dplyr", "tidyr", "readr", "lubridate", "here", "ggplot2", "directlabels", "tibbletime")
invisible(lapply(pkg, library, character.only = TRUE))
```

Importing downloaded ECDC daily COVID-19 dataset.

``` r
path <- here("data/COVID-19-geographic-distribution-worldwide.csv")
covid <- read_csv(path, col_types = cols(daterep = col_date(format = "%d/%m/%Y")))
```

Resetting timeline to days since first case in each country.

``` r
covid_by_country <- covid %>% 
  filter(cases != 0, deaths != 0) %>% 
  group_by(country) %>% 
  mutate(tp = interval(Sys.Date(), daterep) / ddays(1),
         tp = tp - min(tp),
         cases_100k = (cases / popdata) * 1e5,
         deaths_100k = (deaths / popdata) * 1e5)
```

Calculating number of cases and deaths per country. Keeping only
informative rows.

``` r
lag_n <- 7
covid_cum <- covid_by_country %>% 
  mutate(cum_cases = with_order(tp, cumsum, cases),
         cum_deaths = with_order(tp, cumsum, deaths),
         risk = cum_deaths / cum_cases,
         risk_lag = cum_deaths / lag(cum_cases, n = lag_n, order_by = tp)) %>% 
  ungroup()
```

## Worldwide cases and deaths

``` r
cumulative <- covid_by_country %>% 
  group_by(daterep) %>% 
  summarise_at(c("cases", "deaths"), sum) %>% 
  mutate_at(c("cases", "deaths"), list(cum = cumsum))


cumlong <- cumulative %>% 
  pivot_longer(cases:deaths_cum)
cumlong %>% 
  group_by(name) %>% 
  summarise_at("value", max) %>% 
  filter(name %in% c("cases_cum", "deaths_cum"))
```

    ## # A tibble: 2 x 2
    ##   name          value
    ##   <chr>         <dbl>
    ## 1 cases_cum  14271097
    ## 2 deaths_cum   609627

``` r
cumlong %>% 
  filter(name %in% c("cases_cum", "deaths_cum")) %>% 
  ggplot(aes(daterep, value, linetype = name)) +
  geom_line() +
  geom_dl(data = cumlong %>% 
            group_by(name) %>% 
            filter(name %in% c("cases_cum", "deaths_cum"), value == max(value)), 
          aes(label = prettyNum(value, big.mark = ",")), 
          method = list("last.points", hjust = 1.05, vjust = -0.3)) +
  labs(x = "Date", 
       y = "Number of cases or deaths",
       title = "Global cases and deaths") +
  scale_y_continuous(limits = c(0, max(cumlong$value) * 1.1)) +
  scale_linetype_discrete(labels = c("Cases", "Deaths")) +
  theme(legend.title = element_blank(),
        legend.position = "bottom")
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

COVID-19 cases worldwide by country.

``` r
top_10_cases <- covid_cum %>% 
  group_by(country) %>% 
  summarise_at("cum_cases", max) %>% 
  top_n(10, cum_cases) %>% 
  pull(country)
top_10_deaths <- covid_cum %>% 
  group_by(country) %>% 
  summarise_at("cum_deaths", max) %>% 
  top_n(10, cum_deaths) %>% 
  pull(country)

covid_cum %>% 
  ggplot(aes(daterep, cum_cases)) +
  geom_line(aes(group = country, color = country %in% top_10_cases)) +
  geom_dl(aes(label = geoid, color = country %in% top_10_cases), method = list("last.points", cex = 0.8)) +
  scale_y_log10() +
  scale_color_manual(values = c("gray", "black")) +
  theme(legend.position = "none") +
  labs(title = "Cases", 
       x = "Date", 
       y = "Cumulative number of cases",
       caption = "Each line represents one country.\nTop 10 is shown in black.")
```

![](README_files/figure-gfm/plot-cases-dates-1.png)<!-- -->

COVID-19 deaths worldwide.

``` r
covid_cum %>% 
  ggplot(aes(daterep, cum_deaths)) +
  geom_line(aes(group = country, color = country %in% top_10_deaths)) +
  geom_dl(aes(label = geoid, color = country %in% top_10_deaths), method = list("last.points", cex = 0.8)) +
  scale_y_log10() +
  scale_color_manual(values = c("gray", "black")) +
  theme(legend.position = "none") +
  labs(title = "Deaths", 
       x = "Date", 
       y = "Cumulative number of deaths",
       caption = "Each line represents one country.\nTop 10 is shown in black.")
```

![](README_files/figure-gfm/plot-deaths-dates-1.png)<!-- -->

## Cases and deaths on relative time scale

Number of cases per country.

``` r
covid_cum %>% 
  ggplot(aes(tp, cum_cases)) +
  geom_line(aes(group = country, color = country %in% top_10_cases)) +
  geom_dl(aes(label = geoid, color = country %in% top_10_cases), method = list("last.points", cex = 0.8)) +
  scale_y_log10() +
  scale_color_manual(values = c("gray", "black")) +
  theme(legend.position = "none") +
  labs(x = "Days since first case in each country", 
       y = "Cumulative number of cases",
       caption = "Each line represents one country.\nTop 10 is shown in black.")
```

![](README_files/figure-gfm/plot-cases-1.png)<!-- -->

Number of deaths per country.

``` r
covid_cum %>% 
  ggplot(aes(tp, cum_deaths)) +
  geom_line(aes(group = country, color = country %in% top_10_deaths)) +
  geom_dl(aes(label = geoid, color = country %in% top_10_deaths), method = list("last.points", cex = 0.8)) +
  scale_y_log10() +
  scale_color_manual(values = c("gray", "black")) +
  theme(legend.position = "none") +
  labs(x = "Days since first death in each country", 
       y = "Cumulative number of deaths",
       caption = "Each line represents one country.\nTop 10 is shown in black.")
```

![](README_files/figure-gfm/plot-deaths-1.png)<!-- -->

## Risk of death

``` r
covid_cum %>% 
  ggplot(aes(tp, risk)) +
  geom_line(aes(group = country)) +
  geom_dl(aes(label = geoid), method = list("last.points", cex = 0.8)) +
  scale_y_log10() +
  labs(x = "Time, days from first case", 
       y = "Risk of death",
       caption = "Each line represents one country")
```

![](README_files/figure-gfm/plot-risk-1.png)<!-- -->

Lagged (7 days) risk. Risk of death relative to number of cases 7 days
earlier.

``` r
covid_cum %>% 
  ggplot(aes(tp, risk_lag)) +
  geom_line(aes(group = country)) +
  geom_dl(aes(label = geoid), method = list("last.points", cex = 0.8)) +
  scale_y_log10() +
  labs(x = "Time, days from first case", 
       y = "Risk of death",
       caption = "Each line represents one country")
```

![](README_files/figure-gfm/plot-risk-lag-1.png)<!-- -->

## COVID-19 cases in Estonia

``` r
est <- read_csv(here("data/opendata_covid19_test_results.csv"))
est <- est %>% 
  mutate(result_wk = isoweek(ResultTime),
         ResultDate = date(ResultTime))
```

14 day rolling number of cases.

``` r
rolling_sum <- rollify(sum, window = 14)
est %>% 
  select(id, ResultDate, ResultValue) %>% 
  mutate(ResultValue = case_when(
    ResultValue == "N" ~ "Negative",
    ResultValue == "P" ~ "Positive"
  )) %>% 
  group_by(ResultValue) %>% 
  complete(ResultDate = seq.Date(min(ResultDate), max(ResultDate), "day")) %>%
  group_by(ResultValue, ResultDate) %>% 
  summarise(n = sum(!is.na(id))) %>% 
  mutate(n14 = rolling_sum(n)) %>% 
  drop_na() %>% 
  ggplot() +
  geom_col(aes(ResultDate, n14)) +
  facet_wrap(~ ResultValue, scales = "free_y") +
  labs(x = "Date of the 2020",
       y = "Number of tests")
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Percent of positive cases per week.

``` r
est %>% 
  count(result_wk, ResultValue) %>% 
  pivot_wider(names_from = ResultValue, values_from = n) %>% 
  mutate(tests = N + P,
         pp = P / tests) %>% 
  na.omit() %>% 
  ggplot() +
  geom_point(aes(result_wk, pp, size = tests)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Week of the 2020",
       y = "Positive tests, %")
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Estonian COVID-19 tests handling

We are looking at the test result reporting and publishing timestamps by
Estonian Healthboard.

When are the analyses performed and reported during the day. Be extra
careful with interpretations\!

``` r
daytime <- function(x) {
  s <- as.character(second(x))
  if (nchar(s) == 1) {
    s <- paste0(s, s)
  }
  paste0(hour(x), ":", minute(x), ":", s) %>% 
    hms::parse_hms()
}

processing <- est %>% 
  mutate(result_to_insert = interval(ResultTime, AnalysisInsertTime) / dhours(1),
         result_time = daytime(ResultTime),
         insert_time = daytime(AnalysisInsertTime)) %>% 
  select(id, result_wk, result_to_insert, result_time, insert_time)
```

Results timestamps during day.

``` r
processing %>% 
  ggplot() +
  geom_histogram(aes(x = result_time, y = ..count.. / sum(..count..)), bins = 24) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Result time", y = "Percent cases")
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Timestamps of result insertion to database.

``` r
processing %>% 
  ggplot() +
  geom_histogram(aes(x = insert_time, y = ..count.. / sum(..count..)), bins = 24) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Result database insertion time", y = "Percent cases")
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Time from test result to database insertion.

``` r
processing %>% 
  ggplot() +
  geom_histogram(aes(x = result_to_insert, y = ..count.. / sum(..count..)), bins = 24) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_log10(labels = formatC) +
  labs(x = "Timespan from test result to database insertion, hours", 
       y = "Percent cases")
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Test results processing times.

``` r
processing %>% 
  group_by(result_wk) %>% 
  summarise_at("result_to_insert", list(median = median, n = length)) %>% 
  ggplot() +
  geom_point(aes(result_wk, median, size = log10(n))) +
  scale_y_log10() +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(x = "Week of the 2020", 
       y = "Median timespan from\ntest result to database insertion, hours",
       size = "Number of tests\nper week, log10")
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
