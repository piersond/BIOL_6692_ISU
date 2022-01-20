### Compile annual water chemistry data from Reynolds Creek CZO

# Load packages
library(readxl)
library(tidyverse)


#############################
### Short code ###
# Adapted from: https://dominicroye.github.io/en/2019/import-excel-sheets-with-r/
#############################

## Load WY2015 data for each watershed
# Set file location for WY2015 data
WY2015_file_loc <- "/Users/Derek/Google Drive/Work/Courses/BIOL 6692 - ISU Data Management/Lecture materials/Lecture 2/Water Chem/Data/WY2015_RCEW_stream chemistry.xlsx"

# Map all workbook sheets to a combined dataframe
WChem_2015 <- WY2015_file_loc  %>%
                excel_sheets() %>%
                set_names() %>%
                map_dfr(read_excel,
                          na = c("NA"),
                          col_types=c("text"),
                          path = WY2015_file_loc,
                          .id = "original_file")

# Check the dataframe structure
str(WChem_2015)




