# Create data base skeleton

# Init ------------------------------------------------------------

cat(crayon::blue(crayon::bold('Create data base skeleton\n')))

library(here); library(glue)
library(yaml)
library(dplyr); library(tidyr)

# Constants -------------------------------------------------------

wd <- here()

config <- read_yaml(glue('{wd}/cfg/config.yaml'))

# Functions -------------------------------------------------------

source(glue('{wd}/src/global/funs.R'))

# Generate skeleton -----------------------------------------------

skeleton <-
  expand_grid(
    region_iso = config$skeleton$region,
    year = as.integer(seq(config$skeleton$year$min, config$skeleton$year$max, 1)),
    week = as.integer(seq(config$skeleton$week$min, config$skeleton$week$max, 1))
  )

# only keep ISO week 53 in ISO years which feature that week
skeleton <- filter(
  skeleton,
  YearHasIsoWeek53(year) | (!YearHasIsoWeek53(year) & week < 53)
)

# Add unique row id -----------------------------------------------

skeleton <-
  skeleton %>%
  mutate(
    id = GenerateRowID(region_iso, year, week)
  )

# Define order of rows and columns --------------------------------

col_order <- quos(id, region_iso, year, week)
skeleton <-
  skeleton %>%
  arrange(id) %>%
  select(!!!col_order)

# Export ---------------------------------------------------------

save(skeleton, file = glue('{wd}/build/data_skeleton/skeleton.Rdata'))
