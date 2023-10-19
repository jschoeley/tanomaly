# Prepare temperature data

# Init ------------------------------------------------------------

cat(crayon::blue(crayon::bold('Prepare temperature data/n')))

#memory.limit(60e3)

library(here); library(glue)
library(ncdf4); library(abind)
library(readr); library(yaml)
library(dplyr); library(tidyr); library(stringr)
library(lubridate); library(ggplot2)

# Constants -------------------------------------------------------

setwd(here())

paths <- list()
paths$input <- list(
  config = 'cfg/config.yaml',
  global = 'src/global/funs.R',
  # gridded population count files
  gridded_population_data = 'build/data_raw/temperature/gpw_v4_population_count_rev11_30_min.nc',
  # gridded population metadata files
  gridded_population_metadata = 'build/data_raw/temperature//gpw_v4_national_identifier_grid_rev11_lookup.txt',
  # gridded temperature files
  gridded_temperature = 'build/data_raw/temperature//',
  # who region codes
  who_regions = 'cfg/who_regions.csv'
)
paths$output <- list(
  # national identifyer grid
  nat_array = 'build/cache/nat_array.Rdata',
  pop_array = 'build/cache/pop_array.Rdata',
  pop_array_png = 'build/cache/pop_array.png',
  nat_array_png = 'build/cache/nat_array.png',
  landmask_png = 'build/cache/landmask.png',
  tyw_array = 'build/cache/tyw_array.Rdata',
  temperature = 'build/data_harmonized/temperature.Rdata',
  cache = 'build/cache'
)

config <- read_yaml(paths$input$config)

source(paths$input$global)

cnst <- list()
cnst <- within(cnst, {
  # all files relevant for creation of data set
  files = list.files(
    c(
      paths$input$gridded_temperature
    )
  )
  # daily maximum temperature files
  tmax_files = str_subset(files, 'tmax\\.\\d{4}\\.nc')
  # daily minimum temperature files
  tmin_files = str_subset(files, 'tmin\\.\\d{4}\\.nc')
  # long term mean maximum and minimum temperature files
  tltm_max_file = 'tmax.day.1981-2010.ltm.nc'
  tltm_min_file = 'tmin.day.1981-2010.ltm.nc'

  # number of years
  n_year = length(tmin_files)
  # origin date
  origin_date = date('2000-01-01')

  # longitude and latitude coordinates over array x,y dimensions
  # (for use with temperature grid)
  lon =
    # coordinates as given in the data
    seq(0.25, 359.75, 0.5) %>%
    # convert to longitude centered at Greenwich
    {ifelse(. > 180, -360+., .)}
  lat =
    seq(89.75, -89.75, -0.5)
  # (for use with population and national id grid)
  lon2 =
    seq(-179.75, 179.75, 0.5)
  lat2 =
    seq(89.75, -89.75, -0.5)
  # indices of lon2 in lon1
  lon2tolon1 =
    c(which(lon2==0.25):length(lon2),
      1:which(lon2==-0.25))

})

# Functions -------------------------------------------------------

PlotMatrix <- function (X) {
  par(mar=c(0,0,0,0))
  image(X[,ncol(X):1], useRaster = TRUE,
        axes = FALSE, col =  hcl.colors(100))
}

# Prepare national identifier grid --------------------------------

nat <- list()

# national identifier grid
nat$nat_nc <- nc_open(paths$input$gridded_population_data)

# national identifier grid
nat$nat_array <- ncvar_get(nat$nat_nc, 'Population Count, v4.11 (2000, 2005, 2010, 2015, 2020): 30 arc-minutes')
# subset to national identifier grid
nat$nat_array <- nat$nat_array[,,11]
dimnames(nat$nat_array) <- list(lon = cnst$lon2, lat = cnst$lat2)

# reorder array so that greenwich longitude is at x=0
# this is to harmonize the format with the temperature data
nat_array <- nat$nat_array[cnst$lon2tolon1,]

# plot grid
png(file = paths$output$nat_array_png, bg = 'white',
    antialias = 'none', width = dim(nat_array)[1],
    height = dim(nat_array)[2], units = 'px', res = NA)
PlotMatrix(log1p(nat_array))
dev.off()

# cache data for use later
save(nat_array, file = paths$output$nat_array)

# clean
rm(nat, nat_array)

# Prepare population grid -----------------------------------------

pop <- list()

# gridded population
pop$pop_nc <- nc_open(paths$input$gridded_population_data)

# population data
# convert to array, select population 2010, and convert to
# long format data frame
# pop array
pop$pop_array <- ncvar_get(pop$pop_nc, 'Population Count, v4.11 (2000, 2005, 2010, 2015, 2020): 30 arc-minutes')
# subset to population count 2020
pop$pop_array <- pop$pop_array[,,5]
dimnames(pop$pop_array) <- list(lon = cnst$lon2, lat = cnst$lat2)

# reorder array so that greenwich longitude is at x=0
# this is to harmonize the format with the temperature data
pop_array <- pop$pop_array[cnst$lon2tolon1,]

# turn NA's (water) to 0 population
pop_array[is.na(pop_array)] <- 0

# plot grid
png(file = paths$output$pop_array_png, bg = 'white',
    antialias = 'none', width = dim(pop_array)[1],
    height = dim(pop_array)[2], units = 'px', res = NA)
PlotMatrix(log1p(pop_array))
dev.off()

# cache data for use later
save(pop_array, file = paths$output$pop_array)

# clean
rm(pop, pop_array)

# Prepare weekly average temperature grid -------------------------

temp <- list()

# 1. load daily minimum and maximum temperature grids by year
# 2. calculate daily average temperature
# 3. save result in list (x: lon, y: lat, z: day of year, list_item: year)
for (i in 1:cnst$n_year) {
  cat('Calculate daily average gridded temperature:',
      cnst$tmin_files[i], cnst$tmax_files[i], '\n')

  tmin_file <- cnst$tmin_files[i]
  tmax_file <- cnst$tmax_files[i]

  year <- str_extract(cnst$tmin_files[i], '\\d{4}')

  tmin_y <- nc_open(
    glue('{paths$input$gridded_temperature}{tmin_file}')
  )
  tmax_y <- nc_open(
    glue('{paths$input$gridded_temperature}{tmax_file}')
  )

  # min array
  tmin_y_array <-
    ncvar_get(tmin_y, 'tmin')
  dimnames(tmin_y_array) <-
    list(lon = cnst$lon, lat = cnst$lat, day = 1:dim(tmin_y_array)[3])

  # max array
  tmax_y_array <-
    ncvar_get(tmax_y, 'tmax')
  dimnames(tmax_y_array) <-
    list(lon = cnst$lon, lat = cnst$lat, day = 1:dim(tmax_y_array)[3])

  # approximate average daily temperature
  tavg_y_array <- (tmax_y_array + tmin_y_array) / 2

  temp$t_avg[[year]] <- tavg_y_array

}; rm(tavg_y_array, tmax_y_array,
      tmin_y_array, tmax_y, tmin_y,
      tmin_file, tmax_file, year, i)

# bind all years into a single 3D array where the z axis
# marks the days since cnst$origin_date minus 1
temp$t_avg <- abind(temp$t_avg, along = 3)
temp$dates <- cnst$origin_date + ((1:dim(temp$t_avg)[3])-1)
# annotate the 3d array with iso-year-week calendar
# multiple days will be situated in the same week
dimnames(temp$t_avg)[[3]] <-
  Date2ISOWeek(temp$dates, format = 'iso') %>% str_sub(1, 8)

# apply a uniform landmask

# unfortunatly the landmask changes from day to day in the raw
# temperature data. as a result, averages over countries show
# discontinuities over time. here we apply the landmask of a single week
# to all weeks. we choose a week with a low amount of pixels designated
# as land so that weeks with more generous landmasks get harmonized into
# less generous landmasks
temp$landmask <- temp$t_avg[,,'2020-W53']
temp$landmask[!is.na(temp$landmask)] <- 1
temp$landmask[is.na(temp$landmask)] <- 0

# export grid
png(file = paths$output$landmask_png, bg = 'white',
    antialias = 'none', width = dim(temp$landmask)[1],
    height = dim(temp$landmask)[2], units = 'px', res = NA)
PlotMatrix(temp$landmask)
dev.off()

#hashes <- apply(temp$t_avg, 3, function (x) rlang::hash(ifelse(is.na(c(x)),0,1)))
# table(hashes)
#country_codes_before_mask <- names(table(nat_array))
#country_codes_after_mask <- names(table(landmask*nat_array))
#countrys_lost_due_to_mask <- setdiff(country_codes_before_mask, country_codes_after_mask)

# apply landmask
for (k in 1:dim(temp$t_avg)[3]) {
  cat('Apply landmask to day', k, '\n')
  temp$t_avg[,,k] <- temp$t_avg[,,k]*temp$landmask
}

# average daily temperatures into weeks
temp$year_week <- unique(dimnames(temp$t_avg)[[3]])
temp$n_year_week <- length(temp$year_week)
tyw_array <-
  array(
    dim = c(dim(temp$t_avg)[1:2], temp$n_year_week),
    dimnames = list(lon = cnst$lon, lat = cnst$lat, yearweek = temp$year_week)
  )
for (week_i in 1:temp$n_year_week) {
  cat(crayon::blue(glue('Averaging global daily gridded temperature into week {temp$year_week[week_i]}')), sep = '\n')
  daily_temperature_grid_single_week <-
    temp$t_avg[,,dimnames(temp$t_avg)[[3]] %in% temp$year_week[week_i],
               drop = FALSE]
  # average over days
  tyw_array[,,week_i] <-
    rowMeans(daily_temperature_grid_single_week, na.rm = FALSE, dims = 2)
}; rm(daily_temperature_grid_single_week)
temp$t_avg <- NULL

# export global weekly temperature images
for (week_i in 1:temp$n_year_week) {
  cat(crayon::blue(glue('Export png global weekly gridded temperature {temp$year_week[week_i]}')), sep = '\n')
  png(file = glue('{paths$output$cache}/{temp$year_week[week_i]}.png'), bg = 'white',
      antialias = 'none', width = dim(tyw_array)[1],
      height = dim(tyw_array)[2], units = 'px', res = NA)
  PlotMatrix(tyw_array[,,week_i])
  dev.off()
}

# cache data for use later
save(tyw_array, file = paths$output$tyw_array)

# clean
rm(temp, tyw_array, week_i)

# Aggregate to weekly country level average temperature -----------

load(paths$output$nat_array)
load(paths$output$pop_array)
load(paths$output$tyw_array)

# for translation between numeric nation code and ISO codes
nat_lookup <-
  left_join(
    read_csv(paths$input$who_regions),
    read_tsv(paths$input$gridded_population_metadata) %>%
      select(ISOCODE, Value),
    by = c('country_code_iso3'= 'ISOCODE')
  )

# indices
country_codes <- unique(nat_lookup$Value)
country_names <- unique(nat_lookup$country_code_iso2)
country_index <- 1:length(country_codes)
week_index <- 1:dim(tyw_array)[3]
# 3D array weeks x country x statistic
country_timeseries <- array(
  NA,
  dim = c(length(week_index), length(country_index), 2),
  dimnames = list(
    week_index, country_names, c('traw', 'twgt')
  )
)

# perform country level aggregation
for (cntry_i in country_index) {
  # a mask with 1 for grid cells covering the current country and
  # NA otherwise
  single_country_mask <- array(0, dim(nat_array))
  single_country_mask[
    which(nat_array == country_codes[cntry_i], arr.ind = TRUE)
  ] <- 1

  single_country_total_population <-
    sum(single_country_mask*pop_array)
  n_country_pixels <- sum(single_country_mask)

  for (week_i in week_index) {

    cat(crayon::blue(glue('Averaging weekly temperature for country {country_names[cntry_i]} {week_i}')), sep = '\n')

    # average temperature by year-week and country
    x <- single_country_mask*tyw_array[,,week_i]
    country_timeseries[week_i,cntry_i,'traw'] <-
      sum(x/n_country_pixels, na.rm = TRUE)
    if (all(is.na(x))) {country_timeseries[week_i,cntry_i,'traw'] <- NA}
    # population weighted average temperature by year-week and country
    x <- single_country_mask*pop_array*tyw_array[,,week_i]
    country_timeseries[week_i,cntry_i,'twgt'] <-
      sum(x/single_country_total_population, na.rm = TRUE)
    if (all(is.na(x))) {country_timeseries[week_i,cntry_i,'twgt'] <- NA}
  }
}

# convert to data frame
temperature <-
  expand_grid(
    country_iso = country_names,
    weeks = week_index
  ) %>%
  mutate(
    date = cnst$origin_date + weeks*7 - 7,
    year = isoyear(date),
    week = isoweek(date)
  ) %>%
  mutate(
    traw = c(country_timeseries[,,'traw']),
    twgt = c(country_timeseries[,,'twgt'])
  )

rm(
  x, single_country_mask, pop_array, nat_lookup,
  cntry_i, country_codes, country_index, country_names, week_index,
  week_i, k, tyw_array, single_country_total_population,
  n_country_pixels, nat_array, country_timeseries
)

# Add temperature anomalies ---------------------------------------

library(mgcv)

temperature_split <- split(temperature, temperature$country_iso)

X_fit <- lapply(temperature_split, function (X) {
  cat(crayon::blue(glue(
    'Calculating weekly temperature z-scores for country {X[["country_iso"]][1]}')
  ), sep = '\n')

  fit_X <- try({
    gam(
      twgt ~ 1 + s(weeks, bs = 'tp') + s(week, bs = 'cp'),
      data = X, family = 'gaussian'
    )
  })
  if (class(fit_X)[1] == 'try-error') {
    X$twgt_fit <- NA_real_
    X$twgt_rsd <- NA_real_
    X$twgt_zsc <- NA_real_
  } else {
    X$twgt_fit <- c(predict(fit_X))
    X$twgt_rsd <- X$twgt - X$twgt_fit
    X$twgt_zsc <- X$twgt_rsd / sqrt(fit_X$sig2)
  }
  return(X)
})
X_fit <- do.call('rbind', X_fit)

temperature <-
  X_fit |>
  mutate(id = GenerateRowID(country_iso, year, week)) |>
  filter(country_iso %in% config$output_region) |>
  select(id, traw, twgt, twgt_rsd, twgt_zsc)

# cache data for use later
save(temperature, file = paths$output$temperature)
