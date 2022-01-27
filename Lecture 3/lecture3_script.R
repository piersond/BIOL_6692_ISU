# Description: Plot Reynolds Creek CZO water chemistry data
# Author: Derek Pierson
# Date: Jan 26, 2022

# Load packages
library(dplyr)
library(ggplot2)

# Set working drive to script folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Load data
wq <- read.csv("WaterQ_data.csv", as.is=T)


#####################################
# Data prep
#####################################

# Set format for date columns
# simple date
wq$Sample_Collection.Date..mm.dd.yyyy. <- as.Date(wq$Sample_Collection.Date..mm.dd.yyyy., format = "%m/%d/%Y")

# POSIXlt date format
wq$datetime <- as.POSIXlt(wq$datetime, tryFormats = c("%Y-%m-%d %H:%M:%OS",
                                       "%Y/%m/%d %H:%M:%OS",
                                       "%Y-%m-%d %H:%M",
                                       "%Y/%m/%d %H:%M",
                                       "%Y-%m-%d",
                                       "%Y/%m/%d"))

# Select data of interest
dobson_grab <- wq %>% filter(Subcatchment == "Dobson") %>%
                      filter(Sample_Type == "Grab") %>%
                      filter(!is.na(datetime)) %>% # remove data rows without a datetime value
                      select(Subcatchment, Sample_Type, datetime, # Select the columns to keep using the column names
                             qcms, # Discharge in cm s-1
                             X.Carbon..dissolved.organic..mg.L.,
                             X.Carbon..dissolved.organic.blank.corrected...mg.L.,
                             Nitrogen..total..mg.L.,
                             Nitrogen..total_Blank_Corrected..mg.L.) %>%
                     `colnames<-`(c("Subcatchment", "Sample_Type", "datetime",
                                    "Discharge", "DOC", "DOC_BC", "TN", "TN_BC"))

# Create a month column
dobson_grab$sample_month <- format(dobson_grab$datetime,"%b") # %b gives the abbreviated month name
dobson_grab$sample_month_num <- as.numeric(format(dobson_grab$datetime,"%m")) # %m gives the month number

# Summarize data by month
dobson_monthly <- dobson_grab %>% group_by(sample_month_num, sample_month) %>%
                                  summarize(n = n(),
                                            Q_avg = mean(Discharge, na.rm = T), # na.rm is important, otherwise if any value is NA, returns NA
                                            Q_sd = sd(Discharge, na.rm = T),
                                            Q_min = min(Discharge, na.rm = T),
                                            Q_max = max(Discharge, na.rm = T))


#####################################
# Plots 
#####################################

### Plot mean discharge by month
# Example 1
ggplot(data=dobson_monthly, aes(x=sample_month, y=Q_avg)) + geom_histogram(stat='identity')

# Example 2
ggplot(dobson_monthly, aes(x = reorder(sample_month, sample_month_num), y = Q_avg)) + geom_bar(stat = "identity", fill="dark green")

# Example 3
dobson_monthly$sample_month <- factor(dobson_monthly$sample_month,levels = c("Jan", "Feb", "Mar", "Apr",
                                                           "May", "Jun", "Jul", "Aug",
                                                           "Sep", "Oct", "Nov", "Dec"))

ggplot(dobson_monthly, aes(x=sample_month, y=Q_avg)) + geom_histogram(stat='identity', fill='dark blue')


### Add min and max discharge
# Plot part 1
plot_p1 <- ggplot(dobson_monthly, aes(x=sample_month, y=Q_avg)) + geom_histogram(stat='identity', fill='dark blue')

# Add max Q points to part 1
plot_p2 <- plot_p1 + geom_point(aes(x=sample_month, y=Q_max, colour = 'Max Q'))
plot_p2

# Add min Q points
plot_p3 <- plot_p2 + geom_point(aes(x=sample_month, y=Q_min, colour = 'Min Q'))
plot_p3


### Format and finalize the plot
# Create a theme variable to build upon
plot_theme <- theme_bw() + theme(
                panel.grid.major = element_blank(), # Clear the major grid lines
                panel.grid.minor = element_blank(), # Clear the minor grid lines
                legend.position = c(0.9, 0.9), # Change legend position, values are percent of axis length
                legend.background = element_rect(fill = "white")) 

# Plot code
plot_p3 + scale_y_continuous(expand = c(0, 0),
                             limits = c(0,1.3)) + #Force y-axis to start at zero
          labs(title="Dobson Creek: Monthly discharge",
               subtitle="Reynolds Creek CZO",
               caption="Caption text goes here") +
          xlab("Month") +
          ylab("Discharge (cm s-1)") +
          #ylab(expression(paste("Discharge (cm ",s^-1,")"))) +
          guides(color=guide_legend(title="")) +
          plot_theme

    
### Saving a plot
# png(file="plot_name.png", width=3000, height=6000, units="px", res=600)
# dev.off()

### Panel plot
# library(ggpubr)
# ggarrange(plot_p1, plot_p3, 
#           ncol = 2, nrow = 1,
#           common.legend = TRUE,
#           legend="bottom")

