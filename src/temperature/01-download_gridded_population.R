# Download gridded global population data

# Init ------------------------------------------------------------

cat(crayon::blue(crayon::bold('Download gridded global population data')), sep = '\n')

library(here); library(glue)
library(yaml); library(httr)

# Constants -------------------------------------------------------

wd <- here()

config <- read_yaml(glue('{wd}/cfg/config.yaml'))

paths <- list()
paths$input <- list(
  gridded_pop_url =
    'https://sedac.ciesin.columbia.edu/downloads/data/gpw-v4/gpw-v4-population-count-rev11/gpw-v4-population-count-rev11_totpop_30_min_nc.zip'
)
paths$output <- list(
  gridded_population_zip =
    glue('{wd}/build/cache/gridded_population.zip'),
  destination =
    glue('{wd}/build/data_raw/temperature')
)

# Download --------------------------------------------------------

GET(
  url = paths$input$gridded_pop_url,
  authenticate(user = config$credentials$earthdata$user,
               password = config$credentials$earthdata$pswd),
  write_disk(paths$output$gridded_population_zip, overwrite = TRUE),
  progress()
)

# Export ----------------------------------------------------------

unzip(
  zipfile = paths$output$gridded_population_zip,
  exdir = paths$output$destination
)
