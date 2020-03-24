library(glue)
library(readxl)
library(readr)

yesterday <- Sys.Date() - 1
url <- glue("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-{yesterday}.xlsx")
temp <- tempfile()
resp <- try(download.file(url, temp), silent = TRUE)

if (!inherits(resp, "try-error")) {
  data <- read_excel(temp)
  write_csv(data, "data/COVID-19-geographic-disbtribution-worldwide.csv")
}

unlink(temp)
