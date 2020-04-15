pkg <- c("dplyr", "readr")
invisible(lapply(pkg, library, character.only = TRUE))


# Download worldwide data
url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
temp <- tempfile()
resp <- try(download.file(url, temp, method = "libcurl"), silent = TRUE)

if (!inherits(resp, "try-error")) {
  raw <- read_csv(temp)
  # Check if colnames match
  stopifnot(
    all.equal(
      c("dateRep", "day", "month", "year", "cases", "deaths", 
              "countriesAndTerritories", "geoId", 
              "countryterritoryCode", "popData2018"), 
      colnames(raw))
    )
  proc <- raw %>% 
    rename_all(tolower) %>% 
    rename_all(~gsub("_", "", .x)) %>% 
    rename(country = countriesandterritories)
  write_csv(proc, "data/COVID-19-geographic-distribution-worldwide.csv")
}

unlink(temp)

# Download Estonian data
download.file(
  "https://opendata.digilugu.ee/opendata_covid19_test_results.csv", 
  "data/opendata_covid19_test_results.csv"
  )

