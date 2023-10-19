# Download gridded global daily temperature data

# Init ------------------------------------------------------------

cat(crayon::blue(crayon::bold('Download gridded global daily temperature data')), sep = '\n')

library(here); library(glue)
library(curl)

# Constants -------------------------------------------------------

wd <- here()

paths <- list()
paths$input <- list(
  temperature_url = 'ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_temp/'
)
paths$output <- list(
  destination = glue('{wd}/build/data_raw/temperature')
)

cnst <- list()
cnst <- within(cnst, {
  # year range
  years = 2000:as.integer(format(Sys.time(), '%Y'))
  # daily minimum and maximum temperature files
  t_filenames =
    c(paste0('tmin.', years, '.nc'), paste0('tmax.', years, '.nc'))
})

# Download --------------------------------------------------------

for (file in cnst$t_filenames) {
  cat(crayon::blue(glue('Download {file}')), sep = '\n')
  curl_download(
    url = glue('{paths$input$temperature_url}/{file}'),
    destfile = glue('{paths$output$destination}/{file}')
  )
}
