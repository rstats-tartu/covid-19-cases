pkg <- c("dplyr", "readxl", "readr", "glue")
invisible(lapply(pkg, library, character.only = TRUE))

yesterday <- Sys.Date() - 1
url <- glue("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-{yesterday}.xlsx")
temp <- tempfile()
resp <- try(download.file(url, temp), silent = TRUE)

if (!inherits(resp, "try-error")) {
  raw <- read_excel(temp)
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
  write_csv(proc, "data/COVID-19-geographic-disbtribution-worldwide.csv")
}

unlink(temp)
