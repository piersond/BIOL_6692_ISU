### BIOL 6692: Env Data Mngt & Prgm Cpts
### Lecture 1 script
### Author: Derek Pierson
### Created on Jan 1, 2022
#################################################

# Working directory (...where does this script look for files?)
getwd()
setwd("C:/github")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Variable types
x <- 5.23
y <- "Derek"
i <- as.integer(5.4)

str(x)
str(y)
str(i)

# Load .csv data
df <- read.csv("RCrk_SOC_calibration.csv", as.is=T, na.strings = c("Bad data","NA", 99999))

# Check dataframe, column names, column data formats
str(df)
colnames(df)
colnames(df)[1] <- c("ID")

# Loading packages
library(dplyr)

df_filter <- df %>% group(trt) %>% summarize(CLAY_field, mean_clay = mean(CLAY_field))

# ggplot basics (see cheat sheet for more examples)
library(ggplot2)

# Quick plot example
ggplot(df, aes(x=SOC_field, y=CLAY_field)) + geom_point() + theme_bw() +
  xlim(0,100) + ylim(0,100) +
  xlab("X axis") + ylab("Y axis") +
  ggtitle ("Title")
  
