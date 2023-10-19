cat('Install R dependencies\n')

pkg_list <- c(
  'crayon',
  'here', 'glue',
  'yaml', 'readr',
  'dplyr', 'tidyr', 'stringr',
  'lubridate', 'purrr',
  'ggplot2', 'curl',
  'ISOweek',
  'ncdf4', 'abind', 'httr', 'openxlsx',
  'zoo',
  'rmarkdown', 'xml2', 'rmarkdown', 'pander', 'tidyverse',
  'ncdf4'
)

install.packages(
  setdiff(pkg_list, rownames(installed.packages())),
  repos = 'http://cran.rstudio.com/'
)
