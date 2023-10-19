# Assemble mocy data set

# Init ------------------------------------------------------------

cat(crayon::blue(crayon::bold('Assemble final data set\n')))

library(here); library(glue)
library(dplyr); library(readr); library(openxlsx); library(yaml)

# Constants -------------------------------------------------------

setwd(here())

config <- read_yaml('cfg/config.yaml')

# Data ------------------------------------------------------------

cat(crayon::blue('Load skeleton\n'))
load('build/data_skeleton/skeleton.Rdata')
cat(crayon::blue('Load temperature\n'))
load('build/data_harmonized/temperature.Rdata')
cat(crayon::blue('Load region\n'))
load('build/data_harmonized/region.Rdata')

# Join ------------------------------------------------------------

cat(crayon::blue('Join data subsets\n'))
joined <-
  skeleton %>%
  left_join(region |>select(id, region_name), by = 'id') %>%
  left_join(temperature, by = 'id')

# Export ----------------------------------------------------------

cat(crayon::blue('Save final data set as .Rdata\n'))
save(joined, file = 'out/tanomaly.Rdata')

cat(crayon::blue('Save final data set as .csv\n'))
write_csv(joined, file = 'out/tanomaly.csv')

cat(crayon::blue('Save final data set as .xlsx\n'))
write.xlsx(joined, 'out/tanomaly.xlsx',
           keepNA = TRUE, na.string = '.', overwrite = TRUE,
           firstRow = TRUE, firstCol = TRUE)
