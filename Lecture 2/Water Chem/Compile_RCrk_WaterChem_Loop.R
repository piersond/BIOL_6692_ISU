### Compile annual water chemistry data from Reynolds Creek CZO

# Load packages
library(readxl)
library(dplyr)


#############################
### Use a for loop to load the files ###
#############################

# Set data file location
data_folder <- "C:/Users/Derek/Google Drive/Work/Courses/BIOL 6692 - ISU Data Management/Lecture materials/Lecture 2/Water Chem/Data/"

# Get filenames of .xlsx files in the data folder
WChem_files <- list.files(data_folder, pattern = ".xlsx")

# Create a data frame to store the combined data
WChem_master <- NULL

# Use loop to load data from each file and combine into the WChem_master dataframe
for(i in 1:length(WChem_files)) {

  RMTN <- read_excel(paste0(data_folder, WChem_files[i]), sheet = "Reynolds Mtn")
  Dobson <- read_excel(paste0(data_folder, WChem_files[i]), sheet = "Dobson")
  Johnston <- read_excel(paste0(data_folder, WChem_files[i]), sheet = "Johnston")
  Tollgate <- read_excel(paste0(data_folder, WChem_files[i]), sheet = "Tollgate")
  Outlet <- read_excel(paste0(data_folder, WChem_files[i]), sheet = "Outlet")
  
  #Check to ensure all column names are the same
  if(all(colnames(RMTN) == colnames(Dobson)) != TRUE) {
    print(WChem_files[i])
    print("Error in colnames for Dobson")
    break
  }
  if(all(colnames(RMTN) == colnames(Johnston)) != TRUE) {
    print(WChem_files[i])
    print("Error in colnames for Johnston")
    break
  }
  if(all(colnames(RMTN) == colnames(Tollgate)) != TRUE) {
    print(WChem_files[i])
    print("Error in colnames for Tollgate")
    break
  }
  if(all(colnames(RMTN) == colnames(Outlet)) != TRUE) {
    print(WChem_files[i])
    print("Error in colnames for Outlet")
    break
  }
  
  # Row bind the watershed dataframe together
  WY_all <- rbind(RMTN, Dobson, Johnston, Tollgate, Outlet)
  
  # Create a filename for the compiled WY data
  WY_filename <- paste0(strsplit(WChem_files[i],"_")[[1]][1],"_compiled.csv")
  
  # Export csv of the compiled WY data
  write.csv(WY_all, paste0(data_folder, WY_filename))
  
  # Combine all WY data into the master dataframe
  WChem_master <- rbind(WChem_master, WY_all)
}





