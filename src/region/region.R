# Add region specific meta to skeleton

# Init ------------------------------------------------------------

cat(crayon::blue(crayon::bold('Add region specific meta to skeleton\n')))

library(here); library(glue)
library(readr); library(dplyr)

# Constants -------------------------------------------------------

wd <- here()

region_meta <- read_csv(glue('{wd}/cfg/who_regions.csv'))

# Data ------------------------------------------------------------

load(glue('{wd}/build/data_skeleton/skeleton.Rdata'))

# Add info on regions ---------------------------------------------

region_meta_to_join <-
  region_meta |>
  rename(region_iso = country_code_iso2)

region <-
  left_join(skeleton, region_meta_to_join, by = 'region_iso') |>
  select(id, region_iso, region_name = country_name)

# Export ----------------------------------------------------------

save(region, file = glue('{wd}/build/data_harmonized/region.Rdata'))
