# Build the temperature data base

### EXECUTABLES --------------------------------------------------------

MD := mkdir -p
R := Rscript --vanilla

### PATHS --------------------------------------------------------------

PWD_PATH := $(shell pwd)

SOURCE_PATH := $(PWD_PATH)/src
BUILD_PATH := $(PWD_PATH)/build

CACHE_PATH := $(BUILD_PATH)/cache
MARKER_PATH := $(BUILD_PATH)/marker
DATA_SKELETON_PATH := $(BUILD_PATH)/data_skeleton
DATA_RAW_PATH := $(BUILD_PATH)/data_raw
DATA_HARMONIZED_PATH := $(BUILD_PATH)/data_harmonized

OUT_PATH := $(PWD_PATH)/out

DIRS_TO_CREATE := $(BUILD_PATH) $(CACHE_PATH) $(MARKER_PATH) $(DATA_SKELETON_PATH) $(DATA_RAW_PATH) $(DATA_HARMONIZED_PATH) $(OUT_PATH)

### CREATE BASIC FOLDER STRUCTURE --------------------------------------

$(shell $(MD) $(DIRS_TO_CREATE))

### INIT ---------------------------------------------------------------

# download and install some dependencies
init: $(MARKER_PATH)/init
$(MARKER_PATH)/init:
	# install and update r packages
	$(R) $(SOURCE_PATH)/init/install_r_dependencies.R
	touch $@

### SKELETON -----------------------------------------------------------

# build a the skeleton for the data base
skeleton: $(MARKER_PATH)/skeleton
$(MARKER_PATH)/skeleton: $(MARKER_PATH)/init
	$(R) $(SOURCE_PATH)/skeleton/skeleton.R
	touch $@

### DOWNLOAD -----------------------------------------------------------

# download raw data
download: download_temperature

download_temperature: $(MARKER_PATH)/download_temperature
$(MARKER_PATH)/download_temperature: $(MARKER_PATH)/init $(MARKER_PATH)/skeleton
	$(MD) $(DATA_RAW_PATH)/temperature
	$(R) $(SOURCE_PATH)/temperature/01-download_gridded_population.R
	$(R) $(SOURCE_PATH)/temperature/02-download_gridded_daily_temperature.R
	touch $@

### HARMONIZE ----------------------------------------------------------

harmonize: harmonize_temperature harmonize_region

harmonize_temperature: $(MARKER_PATH)/harmonize_temperature
$(MARKER_PATH)/harmonize_temperature: $(MARKER_PATH)/download_temperature
	$(R) $(SOURCE_PATH)/temperature/03-derive_country_level_temperature.R
	touch $@

harmonize_region: $(MARKER_PATH)/harmonize_region
$(MARKER_PATH)/harmonize_region:
	$(R) $(SOURCE_PATH)/region/region.R
	touch $@

### ASSEMBLE -----------------------------------------------------------

join: $(MARKER_PATH)/join
$(MARKER_PATH)/join: $(MARKER_PATH)/harmonize_temperature $(MARKER_PATH)/harmonize_region
	$(R) $(SOURCE_PATH)/join/join.R
	touch $@

test: join
	$(R) $(SOURCE_PATH)/test/test.R

all: join test

### CLEAN --------------------------------------------------------------

# remove build files
clean:
	rm -r $(BUILD_PATH)
	rm -r $(OUT_PATH)
