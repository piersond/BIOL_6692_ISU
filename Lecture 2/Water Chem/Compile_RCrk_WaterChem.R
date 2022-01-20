### Compile annual water chemistry data from Reynolds Creek CZO

# Load packages
library(readxl)


#############################
### Straight forward code ###
#############################

## Load WY2015 data for each watershed
# Set file location for WY2015 data
WY2015_file_loc <- "/Users/Derek/Google Drive/Work/Courses/BIOL 6692 - ISU Data Management/Lecture materials/Lecture 2/Water Chem/Data/WY2015_RCEW_stream chemistry.xlsx"

# Read excel sheet for each watershed
RMTN_2015 <- read_excel(WY2015_file_loc, sheet = "Reynolds Mtn")
Dobson_2015 <- read_excel(WY2015_file_loc, sheet = "Dobson")
Johnston_2015 <- read_excel(WY2015_file_loc, sheet = "Johnston")
Tollgate_2015 <- read_excel(WY2015_file_loc, sheet = "Tollgate")
Outlet_2015 <- read_excel(WY2015_file_loc, sheet = "Outlet")

#Check to ensure all column names are the same
all(colnames(RMTN_2015) == colnames(Dobson_2015))
all(colnames(RMTN_2015) == colnames(Johnston_2015))
all(colnames(RMTN_2015) == colnames(Tollgate_2015))
all(colnames(RMTN_2015) == colnames(Outlet_2015))

master_2015<- rbind(RMTN_2015,Dobson_2015,Johnston_2015,Tollgate_2015,Outlet_2015)
#write.csv(master_2015, "RCEW_WChem_2015.csv")

### --> Proceed to repeat code for all WY files and rbind them into one master file
#master_all <- rbind(master_2015,master_2016,master_2017,master_2018,master_2019,master_2020)
#write.csv(master_all, "RCEW_Timeseries_allyears.csv")



## Load WY2016 data for each watershed
# Set file location for WY2016 data
WY2016_file_loc <- "/Users/Derek/Google Drive/Work/Courses/BIOL 6692 - ISU Data Management/Lecture materials/Lecture 2/Water Chem/Data/WY2016_RCEW_stream chemistry.xlsx"

# Read excel sheet for each watershed
RMTN_2016 <- read_excel(WY2016_file_loc, sheet = "Reynolds Mtn")
Dobson_2016 <- read_excel(WY2016_file_loc, sheet = "Dobson")
Johnston_2016 <- read_excel(WY2016_file_loc, sheet = "Johnston")
Tollgate_2016 <- read_excel(WY2016_file_loc, sheet = "Tollgate")
Outlet_2016 <- read_excel(WY2016_file_loc, sheet = "Outlet")

#Check to ensure all column names are the same
all(colnames(RMTN_2016) == colnames(Dobson_2016))
all(colnames(RMTN_2016) == colnames(Johnston_2016))
all(colnames(RMTN_2016) == colnames(Tollgate_2016))
all(colnames(RMTN_2016) == colnames(Outlet_2016))

master_2016<- rbind(RMTN_2016,Dobson_2016,Johnston_2016,Tollgate_2016,Outlet_2016)



















## Load WY2015 data for each watershed
# Set file location for WY2016 data
WY2016_file_loc <- "/Users/Derek/Google Drive/Work/Courses/BIOL 6692 - ISU Data Management/Lecture materials/Lecture 2/Water Chem/Data/WY2016_RCEW_stream chemistry.xlsx"

# Read excel sheet for each watershed
RMTN_2016 <- read_excel(WY2016_file_loc, sheet = "Reynolds Mtn")
Dobson_2016 <- read_excel(WY2016_file_loc, sheet = "Dobson")
Johnston_2016 <- read_excel(WY2016_file_loc, sheet = "Johnston")
Tollgate_2016 <- read_excel(WY2016_file_loc, sheet = "Tollgate")
Outlet_2016<- read_excel(WY2016_file_loc, sheet = "Outlet")

#Check to ensure all column names are the same
all(colnames(RMTN_2016) == colnames(Dobson_2016))
colnames(RMTN_2016) == colnames(Dobson_2016)
colnames(RMTN_2016)
colnames(Dobson_2016)




