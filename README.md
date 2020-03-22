Covid-19 cases
================
rstats-tartu
2020-03-22

Daily covid-19 data is from European Centre for Disease Prevention and
Control.

Loading libraries

``` r
library("tidyverse")
library("readxl")
library("here")
library("glue")
library("brms")
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
  rename_all(str_to_lower)
```

``` r
covid
```

    ## # A tibble: 6,012 x 8
    ##    daterep               day month  year cases deaths country     geoid
    ##    <dttm>              <dbl> <dbl> <dbl> <dbl>  <dbl> <chr>       <chr>
    ##  1 2020-03-21 00:00:00    21     3  2020     2      0 Afghanistan AF   
    ##  2 2020-03-20 00:00:00    20     3  2020     0      0 Afghanistan AF   
    ##  3 2020-03-19 00:00:00    19     3  2020     0      0 Afghanistan AF   
    ##  4 2020-03-18 00:00:00    18     3  2020     1      0 Afghanistan AF   
    ##  5 2020-03-17 00:00:00    17     3  2020     5      0 Afghanistan AF   
    ##  6 2020-03-16 00:00:00    16     3  2020     6      0 Afghanistan AF   
    ##  7 2020-03-15 00:00:00    15     3  2020     3      0 Afghanistan AF   
    ##  8 2020-03-11 00:00:00    11     3  2020     3      0 Afghanistan AF   
    ##  9 2020-03-08 00:00:00     8     3  2020     3      0 Afghanistan AF   
    ## 10 2020-03-02 00:00:00     2     3  2020     0      0 Afghanistan AF   
    ## # … with 6,002 more rows
