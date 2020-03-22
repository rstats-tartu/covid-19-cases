#' ---
#' title: Covid-19 cases
#' author: rstats-tartu
#' date: "`r Sys.Date()`"
#' ---
#' 
#' 
#' Daily covid-19 data is from [European Centre for Disease Prevention 
#' and Control](https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide).
#'
#+ chunk_opts, include=FALSE
knitr::opts_chunk$set(message = FALSE)


#' Loading libraries
#+ libs
library("dplyr")
library("readxl")
library("here")
library("glue")
library("brms")

#' Downloading dataset
#+ download
if (!dir.exists(here("data"))) {
  system(glue("mkdir {here('data')}"))
}
yesterday <- Sys.Date() - 1
dataset <- here(glue("data/COVID-19-geographic-disbtribution-worldwide-{yesterday}.xlsx"))
if (!file.exists(dataset)) {
  url <- glue("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-{yesterday}.xlsx")
  system(glue("curl -o {dataset} {url}"))
} 

#' Importing downloaded dataset.
#+ import
sheet_1 <- excel_sheets(dataset)[1]
covid <- read_excel(dataset, sheet = sheet_1)
covid <- covid %>% 
  rename(Country = `Countries and territories`) %>% 
  rename_all(str_to_lower)

#' 
#+
covid

