pkg <- c("dplyr", "readr")
invisible(lapply(pkg, library, character.only = TRUE))


# Download worldwide data
url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
temp <- tempfile()
resp <- try(download.file(url, temp, method = "libcurl"), silent = TRUE)

if (!inherits(resp, "try-error")) {
  raw <- read_csv(temp)
  renamed <- 
  # Check if colnames match
  proc <- raw %>% 
    rename_all(tolower) %>% 
    rename_all(~gsub("_", "", .x)) %>% 
    rename_all(~gsub("popdata\\d+", "popdata", .x)) %>% 
    rename(country = countriesandterritories)
  stopifnot(
    all(
      c("daterep", "day", "month", "year", "cases", "deaths", 
        "country", "geoid", 
        "countryterritorycode", "popdata") %in%
        colnames(proc))
  )
  write_csv(proc, "data/COVID-19-geographic-distribution-worldwide.csv")
}

unlink(temp)

# Download Estonian data
download.file(
  "https://opendata.digilugu.ee/opendata_covid19_test_results.csv", 
  "data/opendata_covid19_test_results.csv"
  )

