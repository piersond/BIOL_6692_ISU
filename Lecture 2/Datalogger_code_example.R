#This code processes O2, CO2, and TDR data collected at CZN sites
#Created by Jacqueline Gerson, Silver Lab, UC Berkeley
#Last updated: 1/18/2021
#Reynolds Data

##This program is created for a generic site using 3 CO2, 3 O2, and 3 moisture sensors. If you have more or less sensors, you'll need to edit this code.
##To use for your site, fill out the information to input section, set working directory, and edit file names as saved on your computer
##If using 0-20% CO2 sensors, you'll need to change the data cleaning to reflect the max values of 200,000 insetad of 30,000 (see comment in code below)
##Note that sensor 1 is shallow, 2 is mid, and 3 is deep

#Information to input (replace xxx):
setwd("~/Dropbox/Berkeley/Research/CZN/CZN Data/Reynolds Data Processing") #select working directory

NB_T<-(read.delim("h138i10geomicro.dat",sep = ",",skip=1)) #Type in file name
NL_T<-(read.delim("h138i10geomicro.dat",sep = ",",skip=1)) #Type in file name
SWL_T<-(read.delim("h138g10geomicro.dat",sep = ",",skip=1)) #Type in file name
NL_M<-(read.delim("h138k09geomicro.dat",sep = ",",skip=1)) #Type in file name
SiteName5<-(read.delim("SWB_T.dat",sep = ",",skip=1)) #Type in file name

#NB_T<-SiteName1 ##Search and replace all NB_T manually #h138i10
#NL_T<-SiteName2 ##Search and replace all NL_T manually #h138k10
#SWL_T<-SiteName3 ##Search and replace all SWL_T manually #h138g10
#NL_M<-SiteName4 ##Search and replace all NL_M manually #h138k09
#SWB_T<-SiteName5 ##Search and replace all NL_M manually

###Note that these elevations are incorrect and need to be fixed!
NB_T.elev<-1899  #site-specific for Site 1 (in m)
NL_T.elev<-1904  #site-specific for Site 2 (in m)
SWL_T.elev<-1891  #site-specific for Site 3 (in m)
NL_M.elev<-1906  #site-specific for Site 4 (in m)
SiteName5.elev<-1868  #site-specific for Site 5 (in m)

NB_T.amb.cal.1<-53.32 #shallow sensor mV reading for ambient O2 (from google sheet, specific to each sensor)
NB_T.N2.cal.1<-2.05  #shallow sensor  mV reading for 0 O2 (from google sheet, specific to each sensor)

NL_T.amb.cal.1<-49.89 #shallow sensor mV reading for ambient O2 (from google sheet, specific to each sensor)
NL_T.N2.cal.1<-2.05  #shallow sensor  mV reading for 0 O2 (from google sheet, specific to each sensor)
NL_T.amb.cal.2<-52.63  #mid sensor mV reading for ambient O2 (from google sheet, specific to each sensor)
NL_T.N2.cal.2<-2.05  #mid sensor mV reading for 0 O2 (from google sheet, specific to each sensor)
NL_T.amb.cal.3<-51.26 #deep sensor mV reading for ambient O2 (from google sheet, specific to each sensor)
NL_T.N2.cal.3<-2.05   #deep sensor mV reading for 0 O2 (from google sheet, specific to each sensor)

SWL_T.amb.cal.1<-51.26 #shallow sensor mV reading for ambient O2 (from google sheet, specific to each sensor)
SWL_T.N2.cal.1<-2.05  #shallow sensor  mV reading for 0 O2 (from google sheet, specific to each sensor)
SWL_T.amb.cal.2<-51.95  #mid sensor mV reading for ambient O2 (from google sheet, specific to each sensor)
SWL_T.N2.cal.2<-2.05  #mid sensor mV reading for 0 O2 (from google sheet, specific to each sensor)
SWL_T.amb.cal.3<-50.58 #deep sensor mV reading for ambient O2 (from google sheet, specific to each sensor)
SWL_T.N2.cal.3<-2.05   #deep sensor mV reading for 0 O2 (from google sheet, specific to each sensor)

NL_M.amb.cal.1<-51.95 #shallow sensor mV reading for ambient O2 (from google sheet, specific to each sensor)
NL_M.N2.cal.1<-2.05  #shallow sensor  mV reading for 0 O2 (from google sheet, specific to each sensor)
NL_M.amb.cal.2<-50.58  #mid sensor mV reading for ambient O2 (from google sheet, specific to each sensor)
NL_M.N2.cal.2<-2.05  #mid sensor mV reading for 0 O2 (from google sheet, specific to each sensor)

SiteName5.amb.cal.1<-54.68 #shallow sensor mV reading for ambient O2 (from google sheet, specific to each sensor)
SiteName5.N2.cal.1<-2.05  #shallow sensor  mV reading for 0 O2 (from google sheet, specific to each sensor)

#load packages
library(ggplot2)
library(gridExtra)
library(reshape2)
library(tidyr)
library(chron)
library(dplyr)
library(zoo)
library(ggpubr)

#sensor sensitivities:
#O2: 52-58 mV in 21 % O2; 2.6 mV per % O2; 26 μV per 0.01 % O2
#0-3% CO2: +/-40 ppm for 0-3,000, +/-2%  for 3,000-10,000; +/-3.5% for 10,000-30,000
#0-20% CO2: +/-1,000 ppm for 50,000, +/-2,000 ppm  for 0-80,000; +/-4,000 for 80,000-200,000
#TDR: permittivity +/-2; VWC +/- 2.5%; ECb +/- 5%; temp +/- 0.25 degrees; ECw limited accuracy



##NB_T (1 dpeth)
#remove rows with extra metadata (rows 1,2)
NB_T<-NB_T[-c(1,2),]

NB_T$DateTime15 <- strptime(as.character(NB_T$TIMESTAMP), format="%Y-%m-%d %H:%M:%S")
NB_T$DateTime15 <- as.POSIXct(NB_T$DateTime15)

#convert all data values to numbers
NB_T$Batt.V<-as.numeric(as.character(NB_T$BattV_Min))
NB_T$O2.Avg.1<-as.numeric(as.character(NB_T$O2_Avg.1.))
NB_T$CO2.Avg.1<-as.numeric(as.character(NB_T$CO2_1_Avg))
NB_T$Temp.Avg.1<-as.numeric(as.character(NB_T$TDR310_1_Temp_Avg))
NB_T$Ka.Avg.1<-as.numeric(as.character(NB_T$TDR310_1_Ka_Avg))
NB_T$ECb.Avg.1<-as.numeric(as.character(NB_T$TDR310_1_ECb_Avg))
NB_T$ECw.Avg.1<-as.numeric(as.character(NB_T$TDR310_1_ECw_Avg))
NB_T$VWC.Avg.1<-as.numeric(as.character(NB_T$TDR310_1_VWC_Avg))

NB_T$O2.Std.1<-as.numeric(as.character(NB_T$O2_Std.1.))
NB_T$CO2.Std.1<-as.numeric(as.character(NB_T$CO2_1_Std))
NB_T$Temp.Std.1<-as.numeric(as.character(NB_T$TDR310_1_Temp_Std))
NB_T$Ka.Std.1<-as.numeric(as.character(NB_T$TDR310_1_Ka_Std))
NB_T$ECb.Std.1<-as.numeric(as.character(NB_T$TDR310_1_ECb_Std))
NB_T$ECw.Std.1<-as.numeric(as.character(NB_T$TDR310_1_ECw_Std))
NB_T$VWC.Std.1<-as.numeric(as.character(NB_T$TDR310_1_VWC_Std))

#remove bad O2 and CO2 data
NB_T$O2.Avg.1[NB_T$O2.Avg.1 < 0] <- NA
NB_T$CO2.Avg.1[NB_T$CO2.Avg.1 < 0] <- NA

##if have 0-20% sensors, then change these values to be 200,000 for CO2!!
NB_T$O2.Avg.1[NB_T$O2.Avg.1 > 70] <- NA
NB_T$CO2.Avg.1[NB_T$CO2.Avg.1 > 30000] <- NA

#remove bad temperature data
NB_T$Temp.Avg.1[NB_T$Temp.Avg.1 < -40] <- NA
NB_T$Ka.Avg.1[NB_T$Ka.Avg.1 < 1] <- NA #permittivity
NB_T$ECb.Avg.1[NB_T$ECb.Avg.1 < 0] <- NA #bulk electrical conductivity
NB_T$ECw.Avg.1[NB_T$ECw.Avg.1 < 0] <- NA #pore water electrical conductivity
NB_T$VWC.Avg.1[NB_T$VWC.Avg.1 < 0] <- NA #volumetric water content

NB_T$Temp.Avg.1[NB_T$Temp.Avg.1 > 60] <- NA #(degrees C)
NB_T$Ka.Avg.1[NB_T$Ka.Avg.1 > 80] <- NA #permittivity (no units)
NB_T$ECb.Avg.1[NB_T$ECb.Avg.1 > 5000] <- NA #bulk electrical conductivity (uS/cm)
NB_T$ECw.Avg.1[NB_T$ECw.Avg.1 < 55000] <- NA #pore water electrical conductivity (uS/cm)
NB_T$VWC.Avg.1[NB_T$VWC.Avg.1 > 100] <- NA #volumetric water content (%)

#convert O2 voltage to % O2 for each sensor
CF.O2.1<-20.95/(NB_T.amb.cal.1-NL_T.N2.cal.1) #correction factor for O2 sensor 1
NB_T$O2M.1<-CF.O2.1*NB_T$O2.Avg.1-CF.O2.1*NL_T.N2.cal.1 #measured %O2 (before temp correction)

#apply temperature correction to O2 measurements
NB_T$O2.Avg.Tcorrected.1<-NB_T$O2M.1-8.213*(10^-7)*(NB_T$Temp.Avg.1)^3+1.422*(10^-3)*(NB_T$Temp.Avg.1)^2-6.949*(10^-2)*NB_T$Temp.Avg.1-(-8.213*(10^-7)*22^3+1.422*(10^-3)*22^2-6.949*(10^-2)*22)

#elevation correction
P.NB_T<-101.325-101.325*(1-(1-(NB_T.elev)/44307.69231)^5.25328)
NB_T$O2.Avg.TPcorrected.1<-NB_T$O2.Avg.Tcorrected.1*(P.NB_T/101.325)

#plot data to check that it looks ok
#O2
p1<-ggplot(data=NB_T,aes(x=DateTime15,y=O2.Avg.TPcorrected.1))+geom_point()+theme_classic()+labs(x="",y="O2 (%)")

#CO2
p2<-ggplot(data=NB_T,aes(x=DateTime15,y=CO2.Avg.1))+geom_point()+theme_classic()+labs(x="",y="CO2 (ppm)")

#VWC
p3<-ggplot(data=NB_T,aes(x=DateTime15,y=VWC.Avg.1))+geom_point()+theme_classic()+labs(x="",y="VWC (%)")

#Temperature
p4<-ggplot(data=NB_T,aes(x=DateTime15,y=Temp.Avg.1))+geom_point()+theme_classic()+labs(x="",y="Temperature (degrees C)")

#ECb
p5<-ggplot(data=NB_T,aes(x=DateTime15,y=ECb.Avg.1))+geom_point()+theme_classic()+labs(x="",y="Bulk EC (us/cm)")

#ECw
p6<-ggplot(data=NB_T,aes(x=DateTime15,y=ECw.Avg.1))+geom_point()+theme_classic()+labs(x="",y="Porewater EC (uS/cm)")

#Permittivity
p7<-ggplot(data=NB_T,aes(x=DateTime15,y=Ka.Avg.1))+geom_point()+theme_classic()+labs(x="",y="Permittivity")

#Battery voltage
p8<-ggplot(data=NB_T,aes(x=DateTime15,y=Batt.V))+geom_point()+theme_classic()+labs(x="",y="Battery Voltage (V)")

#Export files as pdf saved with NB_T
#a1<-annotate_figure((ggarrange(p1+theme(axis.text.x = element_blank()),(p2+theme(axis.text.x = element_blank())),(p3+theme(axis.text.x = element_blank())),(p4+theme(axis.text.x = element_blank())),(p5+theme(axis.text.x = element_blank())),(p6+theme(axis.text.x = element_blank())),(p7+theme(axis.text.x = element_blank())),p8,ncol=1,nrow=8,align ="v",font.label=list(size=4))),top=text_grob("Site 1",face="bold",size=14))
a1<-annotate_figure((ggarrange((p1+theme(axis.text.x = element_blank())),(p2+theme(axis.text.x = element_blank())),(p3+theme(axis.text.x = element_blank())),p4,ncol=1,nrow=4,align ="v",font.label=list(size=4))),top=text_grob("NB_T",face="bold",size=14))
a2<-annotate_figure((ggarrange((p5+theme(axis.text.x = element_blank())),(p6+theme(axis.text.x = element_blank())),(p7+theme(axis.text.x = element_blank())),p8,ncol=1,nrow=4,align ="v",font.label=list(size=4))),top=text_grob("NB_T",face="bold",size=14))



##NL_T (3 depths)
#remove rows with extra metadata (rows 1,2)
NL_T<-NL_T[-c(1,2),]

NL_T$DateTime15 <- strptime(as.character(NL_T$TIMESTAMP), format="%Y-%m-%d %H:%M:%S")
NL_T$DateTime15 <- as.POSIXct(NL_T$DateTime15)

#convert all data values to numbers
NL_T$Batt.V<-as.numeric(as.character(NL_T$BattV_Min))
NL_T$O2.Avg.1<-as.numeric(as.character(NL_T$O2_Avg.1.))
NL_T$O2.Avg.2<-as.numeric(as.character(NL_T$O2_Avg.2.))
NL_T$O2.Avg.3<-as.numeric(as.character(NL_T$O2_Avg.3.))
NL_T$CO2.Avg.1<-as.numeric(as.character(NL_T$CO2_1_Avg))
NL_T$CO2.Avg.2<-as.numeric(as.character(NL_T$CO2_2_Avg))
NL_T$CO2.Avg.3<-as.numeric(as.character(NL_T$CO2_3_Avg))
NL_T$Temp.Avg.1<-as.numeric(as.character(NL_T$TDR310_1_Temp_Avg))
NL_T$Ka.Avg.1<-as.numeric(as.character(NL_T$TDR310_1_Ka_Avg))
NL_T$ECb.Avg.1<-as.numeric(as.character(NL_T$TDR310_1_ECb_Avg))
NL_T$ECw.Avg.1<-as.numeric(as.character(NL_T$TDR310_1_ECw_Avg))
NL_T$VWC.Avg.1<-as.numeric(as.character(NL_T$TDR310_1_VWC_Avg))
NL_T$Temp.Avg.2<-as.numeric(as.character(NL_T$TDR310_2_Temp_Avg))
NL_T$Ka.Avg.2<-as.numeric(as.character(NL_T$TDR310_2_Ka_Avg))
NL_T$ECb.Avg.2<-as.numeric(as.character(NL_T$TDR310_2_ECb_Avg))
NL_T$ECw.Avg.2<-as.numeric(as.character(NL_T$TDR310_2_ECw_Avg))
NL_T$VWC.Avg.2<-as.numeric(as.character(NL_T$TDR310_2_VWC_Avg))
NL_T$Temp.Avg.3<-as.numeric(as.character(NL_T$TDR310_3_Temp_Avg))
NL_T$Ka.Avg.3<-as.numeric(as.character(NL_T$TDR310_3_Ka_Avg))
NL_T$ECb.Avg.3<-as.numeric(as.character(NL_T$TDR310_3_ECb_Avg))
NL_T$ECw.Avg.3<-as.numeric(as.character(NL_T$TDR310_3_ECw_Avg))
NL_T$VWC.Avg.3<-as.numeric(as.character(NL_T$TDR310_3_VWC_Avg))

NL_T$O2.Std.1<-as.numeric(as.character(NL_T$O2_Std.1.))
NL_T$O2.Std.2<-as.numeric(as.character(NL_T$O2_Std.2.))
NL_T$O2.Std.3<-as.numeric(as.character(NL_T$O2_Std.3.))
NL_T$CO2.Std.1<-as.numeric(as.character(NL_T$CO2_1_Std))
NL_T$CO2.Std.2<-as.numeric(as.character(NL_T$CO2_2_Std))
NL_T$CO2.Std.3<-as.numeric(as.character(NL_T$CO2_3_Std))
NL_T$Temp.Std.1<-as.numeric(as.character(NL_T$TDR310_1_Temp_Std))
NL_T$Ka.Std.1<-as.numeric(as.character(NL_T$TDR310_1_Ka_Std))
NL_T$ECb.Std.1<-as.numeric(as.character(NL_T$TDR310_1_ECb_Std))
NL_T$ECw.Std.1<-as.numeric(as.character(NL_T$TDR310_1_ECw_Std))
NL_T$VWC.Std.1<-as.numeric(as.character(NL_T$TDR310_1_VWC_Std))
NL_T$Temp.Std.2<-as.numeric(as.character(NL_T$TDR310_2_Temp_Std))
NL_T$Ka.Std.2<-as.numeric(as.character(NL_T$TDR310_2_Ka_Std))
NL_T$ECb.Std.2<-as.numeric(as.character(NL_T$TDR310_2_ECb_Std))
NL_T$ECw.Std.2<-as.numeric(as.character(NL_T$TDR310_2_ECw_Std))
NL_T$VWC.Std.2<-as.numeric(as.character(NL_T$TDR310_2_VWC_Std))
NL_T$Temp.Std.3<-as.numeric(as.character(NL_T$TDR310_3_Temp_Std))
NL_T$Ka.Std.3<-as.numeric(as.character(NL_T$TDR310_3_Ka_Std))
NL_T$ECb.Std.3<-as.numeric(as.character(NL_T$TDR310_3_ECb_Std))
NL_T$ECw.Std.3<-as.numeric(as.character(NL_T$TDR310_3_ECw_Std))
NL_T$VWC.Std.3<-as.numeric(as.character(NL_T$TDR310_3_VWC_Std))

#remove bad O2 and CO2 data
NL_T$O2.Avg.1[NL_T$O2.Avg.1 < 0] <- NA
NL_T$O2.Avg.2[NL_T$O2.Avg.2 < 0] <- NA
NL_T$O2.Avg.3[NL_T$O2.Avg.3 < 0] <- NA
NL_T$CO2.Avg.1[NL_T$CO2.Avg.1 < 0] <- NA
NL_T$CO2.Avg.2[NL_T$CO2.Avg.2 < 0] <- NA
NL_T$CO2.Avg.3[NL_T$CO2.Avg.3 < 0] <- NA

##if have 0-20% sensors, then change these values to be 200,000 for CO2!!
NL_T$O2.Avg.1[NL_T$O2.Avg.1 > 70] <- NA
NL_T$O2.Avg.2[NL_T$O2.Avg.2 > 70] <- NA
NL_T$OO2.Avg.3[NL_T$O2.Avg.3 > 70] <- NA
NL_T$CO2.Avg.1[NL_T$CO2.Avg.1 > 30000] <- NA
NL_T$CO2.Avg.2[NL_T$CO2.Avg.2 > 30000] <- NA
NL_T$CO2.Avg.3[NL_T$CO2.Avg.3 > 30000] <- NA

#remove bad temperature data
NL_T$Temp.Avg.1[NL_T$Temp.Avg.1 < -40] <- NA
NL_T$Ka.Avg.1[NL_T$Ka.Avg.1 < 1] <- NA #permittivity
NL_T$ECb.Avg.1[NL_T$ECb.Avg.1 < 0] <- NA #bulk electrical conductivity
NL_T$ECw.Avg.1[NL_T$ECw.Avg.1 < 0] <- NA #pore water electrical conductivity
NL_T$VWC.Avg.1[NL_T$VWC.Avg.1 < 0] <- NA #volumetric water content
NL_T$Temp.Avg.2[NL_T$Temp.Avg.2 < -40] <- NA
NL_T$Ka.Avg.2[NL_T$Ka.Avg.2 < 1] <- NA #permittivity
NL_T$ECb.Avg.2[NL_T$ECb.Avg.2 < 0] <- NA #bulk electrical conductivity
NL_T$ECw.Avg.2[NL_T$ECw.Avg.2 < 0] <- NA #pore water electrical conductivity
NL_T$VWC.Avg.2[NL_T$VWC.Avg.2 < 0] <- NA #volumetric water content
NL_T$Temp.Avg.3[NL_T$Temp.Avg.3 < -40] <- NA
NL_T$Ka.Avg.3[NL_T$Ka.Avg.3 < 1] <- NA #permittivity
NL_T$ECb.Avg.3[NL_T$ECb.Avg.3 < 0] <- NA #bulk electrical conductivity
NL_T$ECw.Avg.3[NL_T$ECw.Avg.3 < 0] <- NA #pore water electrical conductivity
NL_T$VWC.Avg.3[NL_T$VWC.Avg.3 < 0] <- NA #volumetric water content

NL_T$Temp.Avg.1[NL_T$Temp.Avg.1 > 60] <- NA #(degrees C)
NL_T$Ka.Avg.1[NL_T$Ka.Avg.1 > 80] <- NA #permittivity (no units)
NL_T$ECb.Avg.1[NL_T$ECb.Avg.1 > 5000] <- NA #bulk electrical conductivity (uS/cm)
NL_T$ECw.Avg.1[NL_T$ECw.Avg.1 < 55000] <- NA #pore water electrical conductivity (uS/cm)
NL_T$VWC.Avg.1[NL_T$VWC.Avg.1 > 100] <- NA #volumetric water content (%)
NL_T$Temp.Avg.2[NL_T$Temp.Avg.2 > 60] <- NA #(degrees C)
NL_T$Ka.Avg.2[NL_T$Ka.Avg.2 > 80] <- NA #permittivity (no units)
NL_T$ECb.Avg.2[NL_T$ECb.Avg.2 > 5000] <- NA #bulk electrical conductivity (uS/cm)
NL_T$ECw.Avg.2[NL_T$ECw_Avg.2 < 55000] <- NA #pore water electrical conductivity (uS/cm)
NL_T$VWC.Avg.2[NL_T$VWC.Avg.2 > 100] <- NA #volumetric water content (%)
NL_T$Temp.Avg.3[NL_T$Temp.Avg.3 > 60] <- NA #(degrees C)
NL_T$Ka.Avg.3[NL_T$Ka.Avg.3 > 80] <- NA #permittivity (no units)
NL_T$ECb.Avg.3[NL_T$ECb.Avg.3 > 5000] <- NA #bulk electrical conductivity (uS/cm)
NL_T$ECw.Avg.3[NL_T$ECw.Avg.3 < 55000] <- NA #pore water electrical conductivity (uS/cm)
NL_T$VWC.Avg.3[NL_T$VWC.Avg.3 > 100] <- NA #volumetric water content (%)

#convert O2 voltage to % O2 for each sensor
CF.O2.1<-20.95/(NL_T.amb.cal.1-NL_T.N2.cal.1) #correction factor for O2 sensor 1
CF.O2.2<-20.95/(NL_T.amb.cal.2-NL_T.N2.cal.2)
CF.O2.3<-20.95/(NL_T.amb.cal.3-NL_T.N2.cal.3)
NL_T$O2M.1<-CF.O2.1*NL_T$O2.Avg.1-CF.O2.1*NL_T.N2.cal.1 #measured %O2 (before temp correction)
NL_T$O2M.2<-CF.O2.2*NL_T$O2.Avg.2-CF.O2.2*NL_T.N2.cal.2
NL_T$O2M.3<-CF.O2.3*NL_T$O2.Avg.3-CF.O2.3*NL_T.N2.cal.3

#apply temperature correction to O2 measurements
NL_T$O2.Avg.Tcorrected.1<-NL_T$O2M.1-8.213*(10^-7)*(NL_T$Temp.Avg.1)^3+1.422*(10^-3)*(NL_T$Temp.Avg.1)^2-6.949*(10^-2)*NL_T$Temp.Avg.1-(-8.213*(10^-7)*22^3+1.422*(10^-3)*22^2-6.949*(10^-2)*22)
NL_T$O2.Avg.Tcorrected.2<-NL_T$O2M.2-8.213*(10^-7)*(NL_T$Temp.Avg.2)^3+1.422*(10^-3)*(NL_T$Temp.Avg.2)^2-6.949*(10^-2)*NL_T$Temp.Avg.2-(-8.213*(10^-7)*22^3+1.422*(10^-3)*22^2-6.949*(10^-2)*22)
NL_T$O2.Avg.Tcorrected.3<-NL_T$O2M.3-8.213*(10^-7)*(NL_T$Temp.Avg.3)^3+1.422*(10^-3)*(NL_T$Temp.Avg.3)^2-6.949*(10^-2)*NL_T$Temp.Avg.3-(-8.213*(10^-7)*22^3+1.422*(10^-3)*22^2-6.949*(10^-2)*22)

#elevation correction
P.NL_T<-101.325-101.325*(1-(1-(NL_T.elev)/44307.69231)^5.25328)
NL_T$O2.Avg.TPcorrected.1<-NL_T$O2.Avg.Tcorrected.1*(P.NL_T/101.325)
NL_T$O2.Avg.TPcorrected.2<-NL_T$O2.Avg.Tcorrected.2*(P.NL_T/101.325)
NL_T$O2.Avg.TPcorrected.3<-NL_T$O2.Avg.Tcorrected.3*(P.NL_T/101.325)

#plot data to check that it looks ok
#O2
p1<-ggplot(data=NL_T,aes(x=DateTime15,y=O2.Avg.TPcorrected.1))+geom_point()+theme_classic()+geom_point(aes(y=O2.Avg.TPcorrected.2),color="red",shape=2)+geom_point(aes(y=O2.Avg.TPcorrected.3),color="blue",shape=3)+labs(x="",y="O2 (%)")

#CO2
p2<-ggplot(data=NL_T,aes(x=DateTime15,y=CO2.Avg.1))+geom_point()+theme_classic()+geom_point(aes(y=CO2.Avg.2),color="red",shape=2)+geom_point(aes(y=CO2.Avg.3),color="blue",shape=3)+labs(x="",y="CO2 (ppm)")

#VWC
p3<-ggplot(data=NL_T,aes(x=DateTime15,y=VWC.Avg.1))+geom_point()+theme_classic()+geom_point(aes(y=VWC.Avg.2),color="red",shape=2)+geom_point(aes(y=VWC.Avg.3),color="blue",shape=3)+labs(x="",y="VWC (%)")

#Temperature
p4<-ggplot(data=NL_T,aes(x=DateTime15,y=Temp.Avg.1))+geom_point()+theme_classic()+geom_point(aes(y=Temp.Avg.2),color="red",shape=2)+geom_point(aes(y=Temp.Avg.3),color="blue",shape=3)+labs(x="",y="Temperature (degrees C)")

#ECb
p5<-ggplot(data=NL_T,aes(x=DateTime15,y=ECb.Avg.1))+geom_point()+theme_classic()+geom_point(aes(y=ECb.Avg.2),color="red",shape=2)+geom_point(aes(y=ECb.Avg.3),color="blue",shape=3)+labs(x="",y="Bulk EC (us/cm)")

#ECw
p6<-ggplot(data=NL_T,aes(x=DateTime15,y=ECw.Avg.1))+geom_point()+theme_classic()+geom_point(aes(y=ECw.Avg.2),color="red",shape=2)+geom_point(aes(y=ECw.Avg.3),color="blue",shape=3)+labs(x="",y="Porewater EC (uS/cm)")

#Permittivity
p7<-ggplot(data=NL_T,aes(x=DateTime15,y=Ka.Avg.1))+geom_point()+theme_classic()+geom_point(aes(y=Ka.Avg.2),color="red",shape=2)+geom_point(aes(y=Ka.Avg.3),color="blue",shape=3)+labs(x="",y="Permittivity")

#Battery voltage
p8<-ggplot(data=NL_T,aes(x=DateTime15,y=Batt.V))+geom_point()+theme_classic()+labs(x="",y="Battery Voltage (V)")

#Export files as pdf saved with NL_T
#a1<-annotate_figure((ggarrange(p1+theme(axis.text.x = element_blank()),(p2+theme(axis.text.x = element_blank())),(p3+theme(axis.text.x = element_blank())),(p4+theme(axis.text.x = element_blank())),(p5+theme(axis.text.x = element_blank())),(p6+theme(axis.text.x = element_blank())),(p7+theme(axis.text.x = element_blank())),p8,ncol=1,nrow=8,align ="v",font.label=list(size=4))),top=text_grob("Site 1",face="bold",size=14))
a3<-annotate_figure((ggarrange((p1+theme(axis.text.x = element_blank())),(p2+theme(axis.text.x = element_blank())),(p3+theme(axis.text.x = element_blank())),p4,ncol=1,nrow=4,align ="v",font.label=list(size=4))),top=text_grob("NL_T",face="bold",size=14))
a4<-annotate_figure((ggarrange((p5+theme(axis.text.x = element_blank())),(p6+theme(axis.text.x = element_blank())),(p7+theme(axis.text.x = element_blank())),p8,ncol=1,nrow=4,align ="v",font.label=list(size=4))),top=text_grob("NL_T",face="bold",size=14))



##SWL_T (3 depths)
#remove rows with extra metadata (rows 1,2)
SWL_T<-SWL_T[-c(1,2),]

SWL_T$DateTime15 <- strptime(as.character(SWL_T$TIMESTAMP), format="%Y-%m-%d %H:%M:%S")
SWL_T$DateTime15 <- as.POSIXct(SWL_T$DateTime15)

#convert all data values to numbers
SWL_T$Batt.V<-as.numeric(as.character(SWL_T$BattV_Min))
SWL_T$O2.Avg.1<-as.numeric(as.character(SWL_T$O2_Avg.1.))
SWL_T$O2.Avg.2<-as.numeric(as.character(SWL_T$O2_Avg.2.))
SWL_T$O2.Avg.3<-as.numeric(as.character(SWL_T$O2_Avg.3.))
SWL_T$CO2.Avg.1<-as.numeric(as.character(SWL_T$CO2_1_Avg))
SWL_T$CO2.Avg.2<-as.numeric(as.character(SWL_T$CO2_2_Avg))
SWL_T$CO2.Avg.3<-as.numeric(as.character(SWL_T$CO2_3_Avg))
SWL_T$Temp.Avg.1<-as.numeric(as.character(SWL_T$TDR310_1_Temp_Avg))
SWL_T$Ka.Avg.1<-as.numeric(as.character(SWL_T$TDR310_1_Ka_Avg))
SWL_T$ECb.Avg.1<-as.numeric(as.character(SWL_T$TDR310_1_ECb_Avg))
SWL_T$ECw.Avg.1<-as.numeric(as.character(SWL_T$TDR310_1_ECw_Avg))
SWL_T$VWC.Avg.1<-as.numeric(as.character(SWL_T$TDR310_1_VWC_Avg))
SWL_T$Temp.Avg.2<-as.numeric(as.character(SWL_T$TDR310_2_Temp_Avg))
SWL_T$Ka.Avg.2<-as.numeric(as.character(SWL_T$TDR310_2_Ka_Avg))
SWL_T$ECb.Avg.2<-as.numeric(as.character(SWL_T$TDR310_2_ECb_Avg))
SWL_T$ECw.Avg.2<-as.numeric(as.character(SWL_T$TDR310_2_ECw_Avg))
SWL_T$VWC.Avg.2<-as.numeric(as.character(SWL_T$TDR310_2_VWC_Avg))
SWL_T$Temp.Avg.3<-as.numeric(as.character(SWL_T$TDR310_3_Temp_Avg))
SWL_T$Ka.Avg.3<-as.numeric(as.character(SWL_T$TDR310_3_Ka_Avg))
SWL_T$ECb.Avg.3<-as.numeric(as.character(SWL_T$TDR310_3_ECb_Avg))
SWL_T$ECw.Avg.3<-as.numeric(as.character(SWL_T$TDR310_3_ECw_Avg))
SWL_T$VWC.Avg.3<-as.numeric(as.character(SWL_T$TDR310_3_VWC_Avg))

SWL_T$O2.Std.1<-as.numeric(as.character(SWL_T$O2_Std.1.))
SWL_T$O2.Std.2<-as.numeric(as.character(SWL_T$O2_Std.2.))
SWL_T$O2.Std.3<-as.numeric(as.character(SWL_T$O2_Std.3.))
SWL_T$CO2.Std.1<-as.numeric(as.character(SWL_T$CO2_1_Std))
SWL_T$CO2.Std.2<-as.numeric(as.character(SWL_T$CO2_2_Std))
SWL_T$CO2.Std.3<-as.numeric(as.character(SWL_T$CO2_3_Std))
SWL_T$Temp.Std.1<-as.numeric(as.character(SWL_T$TDR310_1_Temp_Std))
SWL_T$Ka.Std.1<-as.numeric(as.character(SWL_T$TDR310_1_Ka_Std))
SWL_T$ECb.Std.1<-as.numeric(as.character(SWL_T$TDR310_1_ECb_Std))
SWL_T$ECw.Std.1<-as.numeric(as.character(SWL_T$TDR310_1_ECw_Std))
SWL_T$VWC.Std.1<-as.numeric(as.character(SWL_T$TDR310_1_VWC_Std))
SWL_T$Temp.Std.2<-as.numeric(as.character(SWL_T$TDR310_2_Temp_Std))
SWL_T$Ka.Std.2<-as.numeric(as.character(SWL_T$TDR310_2_Ka_Std))
SWL_T$ECb.Std.2<-as.numeric(as.character(SWL_T$TDR310_2_ECb_Std))
SWL_T$ECw.Std.2<-as.numeric(as.character(SWL_T$TDR310_2_ECw_Std))
SWL_T$VWC.Std.2<-as.numeric(as.character(SWL_T$TDR310_2_VWC_Std))
SWL_T$Temp.Std.3<-as.numeric(as.character(SWL_T$TDR310_3_Temp_Std))
SWL_T$Ka.Std.3<-as.numeric(as.character(SWL_T$TDR310_3_Ka_Std))
SWL_T$ECb.Std.3<-as.numeric(as.character(SWL_T$TDR310_3_ECb_Std))
SWL_T$ECw.Std.3<-as.numeric(as.character(SWL_T$TDR310_3_ECw_Std))
SWL_T$VWC.Std.3<-as.numeric(as.character(SWL_T$TDR310_3_VWC_Std))

#remove bad O2 and CO2 data
SWL_T$O2.Avg.1[SWL_T$O2.Avg.1 < 0] <- NA
SWL_T$O2.Avg.2[SWL_T$O2.Avg.2 < 0] <- NA
SWL_T$O2.Avg.3[SWL_T$O2.Avg.3 < 0] <- NA
SWL_T$CO2.Avg.1[SWL_T$CO2.Avg.1 < 0] <- NA
SWL_T$CO2.Avg.2[SWL_T$CO2.Avg.2 < 0] <- NA
SWL_T$CO2.Avg.3[SWL_T$CO2.Avg.3 < 0] <- NA

##if have 0-20% sensors, then change these values to be 200,000 for CO2!!
SWL_T$O2.Avg.1[SWL_T$O2.Avg.1 > 70] <- NA
SWL_T$O2.Avg.2[SWL_T$O2.Avg.2 > 70] <- NA
SWL_T$OO2.Avg.3[SWL_T$O2.Avg.3 > 70] <- NA
SWL_T$CO2.Avg.1[SWL_T$CO2.Avg.1 > 30000] <- NA
SWL_T$CO2.Avg.2[SWL_T$CO2.Avg.2 > 30000] <- NA
SWL_T$CO2.Avg.3[SWL_T$CO2.Avg.3 > 30000] <- NA

#remove bad temperature data
SWL_T$Temp.Avg.1[SWL_T$Temp.Avg.1 < -40] <- NA
SWL_T$Ka.Avg.1[SWL_T$Ka.Avg.1 < 1] <- NA #permittivity
SWL_T$ECb.Avg.1[SWL_T$ECb.Avg.1 < 0] <- NA #bulk electrical conductivity
SWL_T$ECw.Avg.1[SWL_T$ECw.Avg.1 < 0] <- NA #pore water electrical conductivity
SWL_T$VWC.Avg.1[SWL_T$VWC.Avg.1 < 0] <- NA #volumetric water content
SWL_T$Temp.Avg.2[SWL_T$Temp.Avg.2 < -40] <- NA
SWL_T$Ka.Avg.2[SWL_T$Ka.Avg.2 < 1] <- NA #permittivity
SWL_T$ECb.Avg.2[SWL_T$ECb.Avg.2 < 0] <- NA #bulk electrical conductivity
SWL_T$ECw.Avg.2[SWL_T$ECw.Avg.2 < 0] <- NA #pore water electrical conductivity
SWL_T$VWC.Avg.2[SWL_T$VWC.Avg.2 < 0] <- NA #volumetric water content
SWL_T$Temp.Avg.3[SWL_T$Temp.Avg.3 < -40] <- NA
SWL_T$Ka.Avg.3[SWL_T$Ka.Avg.3 < 1] <- NA #permittivity
SWL_T$ECb.Avg.3[SWL_T$ECb.Avg.3 < 0] <- NA #bulk electrical conductivity
SWL_T$ECw.Avg.3[SWL_T$ECw.Avg.3 < 0] <- NA #pore water electrical conductivity
SWL_T$VWC.Avg.3[SWL_T$VWC.Avg.3 < 0] <- NA #volumetric water content

SWL_T$Temp.Avg.1[SWL_T$Temp.Avg.1 > 60] <- NA #(degrees C)
SWL_T$Ka.Avg.1[SWL_T$Ka.Avg.1 > 80] <- NA #permittivity (no units)
SWL_T$ECb.Avg.1[SWL_T$ECb.Avg.1 > 5000] <- NA #bulk electrical conductivity (uS/cm)
SWL_T$ECw.Avg.1[SWL_T$ECw.Avg.1 < 55000] <- NA #pore water electrical conductivity (uS/cm)
SWL_T$VWC.Avg.1[SWL_T$VWC.Avg.1 > 100] <- NA #volumetric water content (%)
SWL_T$Temp.Avg.2[SWL_T$Temp.Avg.2 > 60] <- NA #(degrees C)
SWL_T$Ka.Avg.2[SWL_T$Ka.Avg.2 > 80] <- NA #permittivity (no units)
SWL_T$ECb.Avg.2[SWL_T$ECb.Avg.2 > 5000] <- NA #bulk electrical conductivity (uS/cm)
SWL_T$ECw.Avg.2[SWL_T$ECw_Avg.2 < 55000] <- NA #pore water electrical conductivity (uS/cm)
SWL_T$VWC.Avg.2[SWL_T$VWC.Avg.2 > 100] <- NA #volumetric water content (%)
SWL_T$Temp.Avg.3[SWL_T$Temp.Avg.3 > 60] <- NA #(degrees C)
SWL_T$Ka.Avg.3[SWL_T$Ka.Avg.3 > 80] <- NA #permittivity (no units)
SWL_T$ECb.Avg.3[SWL_T$ECb.Avg.3 > 5000] <- NA #bulk electrical conductivity (uS/cm)
SWL_T$ECw.Avg.3[SWL_T$ECw.Avg.3 < 55000] <- NA #pore water electrical conductivity (uS/cm)
SWL_T$VWC.Avg.3[SWL_T$VWC.Avg.3 > 100] <- NA #volumetric water content (%)

#convert O2 voltage to % O2 for each sensor
CF.O2.1<-20.95/(SWL_T.amb.cal.1-SWL_T.N2.cal.1) #correction factor for O2 sensor 1
CF.O2.2<-20.95/(SWL_T.amb.cal.2-SWL_T.N2.cal.2)
CF.O2.3<-20.95/(SWL_T.amb.cal.3-SWL_T.N2.cal.3)
SWL_T$O2M.1<-CF.O2.1*SWL_T$O2.Avg.1-CF.O2.1*SWL_T.N2.cal.1 #measured %O2 (before temp correction)
SWL_T$O2M.2<-CF.O2.2*SWL_T$O2.Avg.2-CF.O2.2*SWL_T.N2.cal.2
SWL_T$O2M.3<-CF.O2.3*SWL_T$O2.Avg.3-CF.O2.3*SWL_T.N2.cal.3

#apply temperature correction to O2 measurements
SWL_T$O2.Avg.Tcorrected.1<-SWL_T$O2M.1-8.213*(10^-7)*(SWL_T$Temp.Avg.1)^3+1.422*(10^-3)*(SWL_T$Temp.Avg.1)^2-6.949*(10^-2)*SWL_T$Temp.Avg.1-(-8.213*(10^-7)*22^3+1.422*(10^-3)*22^2-6.949*(10^-2)*22)
SWL_T$O2.Avg.Tcorrected.2<-SWL_T$O2M.2-8.213*(10^-7)*(SWL_T$Temp.Avg.2)^3+1.422*(10^-3)*(SWL_T$Temp.Avg.2)^2-6.949*(10^-2)*SWL_T$Temp.Avg.2-(-8.213*(10^-7)*22^3+1.422*(10^-3)*22^2-6.949*(10^-2)*22)
SWL_T$O2.Avg.Tcorrected.3<-SWL_T$O2M.3-8.213*(10^-7)*(SWL_T$Temp.Avg.3)^3+1.422*(10^-3)*(SWL_T$Temp.Avg.3)^2-6.949*(10^-2)*SWL_T$Temp.Avg.3-(-8.213*(10^-7)*22^3+1.422*(10^-3)*22^2-6.949*(10^-2)*22)

#elevation correction
P.SWL_T<-101.325-101.325*(1-(1-(SWL_T.elev)/44307.69231)^5.25328)
SWL_T$O2.Avg.TPcorrected.1<-SWL_T$O2.Avg.Tcorrected.1*(P.SWL_T/101.325)
SWL_T$O2.Avg.TPcorrected.2<-SWL_T$O2.Avg.Tcorrected.2*(P.SWL_T/101.325)
SWL_T$O2.Avg.TPcorrected.3<-SWL_T$O2.Avg.Tcorrected.3*(P.SWL_T/101.325)

#plot data to check that it looks ok
#O2
p1<-ggplot(data=SWL_T,aes(x=DateTime15,y=O2.Avg.TPcorrected.1))+geom_point()+theme_classic()+geom_point(aes(y=O2.Avg.TPcorrected.2),color="red",shape=2)+geom_point(aes(y=O2.Avg.TPcorrected.3),color="blue",shape=3)+labs(x="",y="O2 (%)")

#CO2
p2<-ggplot(data=SWL_T,aes(x=DateTime15,y=CO2.Avg.1))+geom_point()+theme_classic()+geom_point(aes(y=CO2.Avg.2),color="red",shape=2)+geom_point(aes(y=CO2.Avg.3),color="blue",shape=3)+labs(x="",y="CO2 (ppm)")

#VWC
p3<-ggplot(data=SWL_T,aes(x=DateTime15,y=VWC.Avg.1))+geom_point()+theme_classic()+geom_point(aes(y=VWC.Avg.2),color="red",shape=2)+geom_point(aes(y=VWC.Avg.3),color="blue",shape=3)+labs(x="",y="VWC (%)")

#Temperature
p4<-ggplot(data=SWL_T,aes(x=DateTime15,y=Temp.Avg.1))+geom_point()+theme_classic()+geom_point(aes(y=Temp.Avg.2),color="red",shape=2)+geom_point(aes(y=Temp.Avg.3),color="blue",shape=3)+labs(x="",y="Temperature (degrees C)")

#ECb
p5<-ggplot(data=SWL_T,aes(x=DateTime15,y=ECb.Avg.1))+geom_point()+theme_classic()+geom_point(aes(y=ECb.Avg.2),color="red",shape=2)+geom_point(aes(y=ECb.Avg.3),color="blue",shape=3)+labs(x="",y="Bulk EC (us/cm)")

#ECw
p6<-ggplot(data=SWL_T,aes(x=DateTime15,y=ECw.Avg.1))+geom_point()+theme_classic()+geom_point(aes(y=ECw.Avg.2),color="red",shape=2)+geom_point(aes(y=ECw.Avg.3),color="blue",shape=3)+labs(x="",y="Porewater EC (uS/cm)")

#Permittivity
p7<-ggplot(data=SWL_T,aes(x=DateTime15,y=Ka.Avg.1))+geom_point()+theme_classic()+geom_point(aes(y=Ka.Avg.2),color="red",shape=2)+geom_point(aes(y=Ka.Avg.3),color="blue",shape=3)+labs(x="",y="Permittivity")

#Battery voltage
p8<-ggplot(data=SWL_T,aes(x=DateTime15,y=Batt.V))+geom_point()+theme_classic()+labs(x="",y="Battery Voltage (V)")

#Export files as pdf saved with SWL_T
#a1<-annotate_figure((ggarrange(p1+theme(axis.text.x = element_blank()),(p2+theme(axis.text.x = element_blank())),(p3+theme(axis.text.x = element_blank())),(p4+theme(axis.text.x = element_blank())),(p5+theme(axis.text.x = element_blank())),(p6+theme(axis.text.x = element_blank())),(p7+theme(axis.text.x = element_blank())),p8,ncol=1,nrow=8,align ="v",font.label=list(size=4))),top=text_grob("Site 1",face="bold",size=14))
a5<-annotate_figure((ggarrange((p1+theme(axis.text.x = element_blank())),(p2+theme(axis.text.x = element_blank())),(p3+theme(axis.text.x = element_blank())),p4,ncol=1,nrow=4,align ="v",font.label=list(size=4))),top=text_grob("SWL_T",face="bold",size=14))
a6<-annotate_figure((ggarrange((p5+theme(axis.text.x = element_blank())),(p6+theme(axis.text.x = element_blank())),(p7+theme(axis.text.x = element_blank())),p8,ncol=1,nrow=4,align ="v",font.label=list(size=4))),top=text_grob("SWL_T",face="bold",size=14))



##NL_M (2 depths)
#remove rows with extra metadata (rows 1,2)
NL_M<-NL_M[-c(1,2),]

NL_M$DateTime15 <- strptime(as.character(NL_M$TIMESTAMP), format="%Y-%m-%d %H:%M:%S")
NL_M$DateTime15 <- as.POSIXct(NL_M$DateTime15)

#convert all data values to numbers
NL_M$Batt.V<-as.numeric(as.character(NL_M$BV_Min))
NL_M$O2.Avg.1<-as.numeric(as.character(NL_M$O2_010_Avg))
NL_M$O2.Avg.2<-as.numeric(as.character(NL_M$O2_050_Avg))
NL_M$CO2.Avg.1<-as.numeric(as.character(NL_M$CO2_010_Avg))
NL_M$CO2.Avg.2<-as.numeric(as.character(NL_M$CO2_050_Avg))
NL_M$Temp.Avg.1<-as.numeric(as.character(NL_M$TDR310_1_Temp_Avg))
NL_M$Ka.Avg.1<-as.numeric(as.character(NL_M$TDR310_1_Ka_Avg))
NL_M$ECb.Avg.1<-as.numeric(as.character(NL_M$TDR310_1_ECb_Avg))
NL_M$ECw.Avg.1<-as.numeric(as.character(NL_M$TDR310_1_ECw_Avg))
NL_M$VWC.Avg.1<-as.numeric(as.character(NL_M$TDR310_1_VWC_Avg))
NL_M$Temp.Avg.2<-as.numeric(as.character(NL_M$TDR310_2_Temp_Avg))
NL_M$Ka.Avg.2<-as.numeric(as.character(NL_M$TDR310_2_Ka_Avg))
NL_M$ECb.Avg.2<-as.numeric(as.character(NL_M$TDR310_2_ECb_Avg))
NL_M$ECw.Avg.2<-as.numeric(as.character(NL_M$TDR310_2_ECw_Avg))
NL_M$VWC.Avg.2<-as.numeric(as.character(NL_M$TDR310_2_VWC_Avg))

NL_M$O2.Std.1<-as.numeric(as.character(NL_M$O2_Std.1.))
NL_M$O2.Std.2<-as.numeric(as.character(NL_M$O2_Std.2.))
NL_M$CO2.Std.1<-as.numeric(as.character(NL_M$CO2_1_Std))
NL_M$CO2.Std.2<-as.numeric(as.character(NL_M$CO2_2_Std))
NL_M$Temp.Std.1<-as.numeric(as.character(NL_M$TDR310_1_Temp_Std))
NL_M$Ka.Std.1<-as.numeric(as.character(NL_M$TDR310_1_Ka_Std))
NL_M$ECb.Std.1<-as.numeric(as.character(NL_M$TDR310_1_ECb_Std))
NL_M$ECw.Std.1<-as.numeric(as.character(NL_M$TDR310_1_ECw_Std))
NL_M$VWC.Std.1<-as.numeric(as.character(NL_M$TDR310_1_VWC_Std))
NL_M$Temp.Std.2<-as.numeric(as.character(NL_M$TDR310_2_Temp_Std))
NL_M$Ka.Std.2<-as.numeric(as.character(NL_M$TDR310_2_Ka_Std))
NL_M$ECb.Std.2<-as.numeric(as.character(NL_M$TDR310_2_ECb_Std))
NL_M$ECw.Std.2<-as.numeric(as.character(NL_M$TDR310_2_ECw_Std))
NL_M$VWC.Std.2<-as.numeric(as.character(NL_M$TDR310_2_VWC_Std))

#remove bad O2 and CO2 data
NL_M$O2.Avg.1[NL_M$O2.Avg.1 < 0] <- NA
NL_M$O2.Avg.2[NL_M$O2.Avg.2 < 0] <- NA
NL_M$CO2.Avg.1[NL_M$CO2.Avg.1 < 0] <- NA
NL_M$CO2.Avg.2[NL_M$CO2.Avg.2 < 0] <- NA

##if have 0-20% sensors, then change these values to be 200,000 for CO2!!
NL_M$O2.Avg.1[NL_M$O2.Avg.1 > 70] <- NA
NL_M$O2.Avg.2[NL_M$O2.Avg.2 > 70] <- NA
NL_M$CO2.Avg.1[NL_M$CO2.Avg.1 > 30000] <- NA
NL_M$CO2.Avg.2[NL_M$CO2.Avg.2 > 30000] <- NA

#remove bad temperature data
NL_M$Temp.Avg.1[NL_M$Temp.Avg.1 < -40] <- NA
NL_M$Ka.Avg.1[NL_M$Ka.Avg.1 < 1] <- NA #permittivity
NL_M$ECb.Avg.1[NL_M$ECb.Avg.1 < 0] <- NA #bulk electrical conductivity
NL_M$ECw.Avg.1[NL_M$ECw.Avg.1 < 0] <- NA #pore water electrical conductivity
NL_M$VWC.Avg.1[NL_M$VWC.Avg.1 < 0] <- NA #volumetric water content
NL_M$Temp.Avg.2[NL_M$Temp.Avg.2 < -40] <- NA
NL_M$Ka.Avg.2[NL_M$Ka.Avg.2 < 1] <- NA #permittivity
NL_M$ECb.Avg.2[NL_M$ECb.Avg.2 < 0] <- NA #bulk electrical conductivity
NL_M$ECw.Avg.2[NL_M$ECw.Avg.2 < 0] <- NA #pore water electrical conductivity
NL_M$VWC.Avg.2[NL_M$VWC.Avg.2 < 0] <- NA #volumetric water content

NL_M$Temp.Avg.1[NL_M$Temp.Avg.1 > 60] <- NA #(degrees C)
NL_M$Ka.Avg.1[NL_M$Ka.Avg.1 > 80] <- NA #permittivity (no units)
NL_M$ECb.Avg.1[NL_M$ECb.Avg.1 > 5000] <- NA #bulk electrical conductivity (uS/cm)
NL_M$ECw.Avg.1[NL_M$ECw.Avg.1 < 55000] <- NA #pore water electrical conductivity (uS/cm)
NL_M$VWC.Avg.1[NL_M$VWC.Avg.1 > 100] <- NA #volumetric water content (%)
NL_M$Temp.Avg.2[NL_M$Temp.Avg.2 > 60] <- NA #(degrees C)
NL_M$Ka.Avg.2[NL_M$Ka.Avg.2 > 80] <- NA #permittivity (no units)
NL_M$ECb.Avg.2[NL_M$ECb.Avg.2 > 5000] <- NA #bulk electrical conductivity (uS/cm)
NL_M$ECw.Avg.2[NL_M$ECw_Avg.2 < 55000] <- NA #pore water electrical conductivity (uS/cm)
NL_M$VWC.Avg.2[NL_M$VWC.Avg.2 > 100] <- NA #volumetric water content (%)

#convert O2 voltage to % O2 for each sensor
CF.O2.1<-20.95/(NL_M.amb.cal.1-NL_M.N2.cal.1) #correction factor for O2 sensor 1
CF.O2.2<-20.95/(NL_M.amb.cal.2-NL_M.N2.cal.2)
NL_M$O2M.1<-CF.O2.1*NL_M$O2.Avg.1-CF.O2.1*NL_M.N2.cal.1 #measured %O2 (before temp correction)
NL_M$O2M.2<-CF.O2.2*NL_M$O2.Avg.2-CF.O2.2*NL_M.N2.cal.2

#apply temperature correction to O2 measurements
NL_M$O2.Avg.Tcorrected.1<-NL_M$O2M.1-8.213*(10^-7)*(NL_M$Temp.Avg.1)^3+1.422*(10^-3)*(NL_M$Temp.Avg.1)^2-6.949*(10^-2)*NL_M$Temp.Avg.1-(-8.213*(10^-7)*22^3+1.422*(10^-3)*22^2-6.949*(10^-2)*22)
NL_M$O2.Avg.Tcorrected.2<-NL_M$O2M.2-8.213*(10^-7)*(NL_M$Temp.Avg.2)^3+1.422*(10^-3)*(NL_M$Temp.Avg.2)^2-6.949*(10^-2)*NL_M$Temp.Avg.2-(-8.213*(10^-7)*22^3+1.422*(10^-3)*22^2-6.949*(10^-2)*22)

#elevation correction
P.NL_M<-101.325-101.325*(1-(1-(NL_M.elev)/44307.69231)^5.25328)
NL_M$O2.Avg.TPcorrected.1<-NL_M$O2.Avg.Tcorrected.1*(P.NL_M/101.325)
NL_M$O2.Avg.TPcorrected.2<-NL_M$O2.Avg.Tcorrected.2*(P.NL_M/101.325)

#plot data to check that it looks ok
#O2
p1<-ggplot(data=NL_M,aes(x=DateTime15,y=O2.Avg.TPcorrected.1))+geom_point()+theme_classic()+geom_point(aes(y=O2.Avg.TPcorrected.2),color="red",shape=2)+labs(x="",y="O2 (%)")

#CO2
p2<-ggplot(data=NL_M,aes(x=DateTime15,y=CO2.Avg.1))+geom_point()+theme_classic()+geom_point(aes(y=CO2.Avg.2),color="red",shape=2)+labs(x="",y="CO2 (ppm)")

#VWC
p3<-ggplot(data=NL_M,aes(x=DateTime15,y=VWC.Avg.1))+geom_point()+theme_classic()+geom_point(aes(y=VWC.Avg.2),color="red",shape=2)+labs(x="",y="VWC (%)")

#Temperature
p4<-ggplot(data=NL_M,aes(x=DateTime15,y=Temp.Avg.1))+geom_point()+theme_classic()+geom_point(aes(y=Temp.Avg.2),color="red",shape=2)+labs(x="",y="Temperature (degrees C)")

#ECb
p5<-ggplot(data=NL_M,aes(x=DateTime15,y=ECb.Avg.1))+geom_point()+theme_classic()+geom_point(aes(y=ECb.Avg.2),color="red",shape=2)+labs(x="",y="Bulk EC (us/cm)")

#ECw
p6<-ggplot(data=NL_M,aes(x=DateTime15,y=ECw.Avg.1))+geom_point()+theme_classic()+geom_point(aes(y=ECw.Avg.2),color="red",shape=2)+labs(x="",y="Porewater EC (uS/cm)")

#Permittivity
p7<-ggplot(data=NL_M,aes(x=DateTime15,y=Ka.Avg.1))+geom_point()+theme_classic()+geom_point(aes(y=Ka.Avg.2),color="red",shape=2)+labs(x="",y="Permittivity")

#Battery voltage
p8<-ggplot(data=NL_M,aes(x=DateTime15,y=Batt.V))+geom_point()+theme_classic()+labs(x="",y="Battery Voltage (V)")

#Export files as pdf saved with NL_M
#a1<-annotate_figure((ggarrange(p1+theme(axis.text.x = element_blank()),(p2+theme(axis.text.x = element_blank())),(p3+theme(axis.text.x = element_blank())),(p4+theme(axis.text.x = element_blank())),(p5+theme(axis.text.x = element_blank())),(p6+theme(axis.text.x = element_blank())),(p7+theme(axis.text.x = element_blank())),p8,ncol=1,nrow=8,align ="v",font.label=list(size=4))),top=text_grob("Site 1",face="bold",size=14))
a7<-annotate_figure((ggarrange((p1+theme(axis.text.x = element_blank())),(p2+theme(axis.text.x = element_blank())),(p3+theme(axis.text.x = element_blank())),p4,ncol=1,nrow=4,align ="v",font.label=list(size=4))),top=text_grob("NL_M",face="bold",size=14))
a8<-annotate_figure((ggarrange((p5+theme(axis.text.x = element_blank())),(p6+theme(axis.text.x = element_blank())),(p7+theme(axis.text.x = element_blank())),p8,ncol=1,nrow=4,align ="v",font.label=list(size=4))),top=text_grob("NL_M",face="bold",size=14))

##SWB_T (1 depth)
#remove rows with extra metadata (rows 1,2)
SiteName5<-SiteName5[-c(1,2),]

SiteName5$DateTime15 <- strptime(as.character(SiteName5$TIMESTAMP), format="%Y-%m-%d %H:%M:%S")
SiteName5$DateTime15 <- as.POSIXct(SiteName5$DateTime15)

#convert all data values to numbers
SiteName5$Batt.V<-as.numeric(as.character(SiteName5$BattV_Min))
SiteName5$O2.Avg.1<-as.numeric(as.character(SiteName5$O2_Avg.1.))
SiteName5$CO2.Avg.1<-as.numeric(as.character(SiteName5$CO2_1_Avg))
SiteName5$Temp.Avg.1<-as.numeric(as.character(SiteName5$TDR310_1_Temp_Avg))
SiteName5$Ka.Avg.1<-as.numeric(as.character(SiteName5$TDR310_1_Ka_Avg))
SiteName5$ECb.Avg.1<-as.numeric(as.character(SiteName5$TDR310_1_ECb_Avg))
SiteName5$ECw.Avg.1<-as.numeric(as.character(SiteName5$TDR310_1_ECw_Avg))
SiteName5$VWC.Avg.1<-as.numeric(as.character(SiteName5$TDR310_1_VWC_Avg))

SiteName5$O2.Std.1<-as.numeric(as.character(SiteName5$O2_Std.1.))
SiteName5$CO2.Std.1<-as.numeric(as.character(SiteName5$CO2_1_Std))
SiteName5$Temp.Std.1<-as.numeric(as.character(SiteName5$TDR310_1_Temp_Std))
SiteName5$Ka.Std.1<-as.numeric(as.character(SiteName5$TDR310_1_Ka_Std))
SiteName5$ECb.Std.1<-as.numeric(as.character(SiteName5$TDR310_1_ECb_Std))
SiteName5$ECw.Std.1<-as.numeric(as.character(SiteName5$TDR310_1_ECw_Std))
SiteName5$VWC.Std.1<-as.numeric(as.character(SiteName5$TDR310_1_VWC_Std))

#remove bad O2 and CO2 data
SiteName5$O2.Avg.1[SiteName5$O2.Avg.1 < 0] <- NA
SiteName5$CO2.Avg.1[SiteName5$CO2.Avg.1 < 0] <- NA

##if have 0-20% sensors, then change these values to be 200,000 for CO2!!
SiteName5$O2.Avg.1[SiteName5$O2.Avg.1 > 70] <- NA
SiteName5$CO2.Avg.1[SiteName5$CO2.Avg.1 > 30000] <- NA

#remove bad temperature data
SiteName5$Temp.Avg.1[SiteName5$Temp.Avg.1 < -40] <- NA
SiteName5$Ka.Avg.1[SiteName5$Ka.Avg.1 < 1] <- NA #permittivity
SiteName5$ECb.Avg.1[SiteName5$ECb.Avg.1 < 0] <- NA #bulk electrical conductivity
SiteName5$ECw.Avg.1[SiteName5$ECw.Avg.1 < 0] <- NA #pore water electrical conductivity
SiteName5$VWC.Avg.1[SiteName5$VWC.Avg.1 < 0] <- NA #volumetric water content

SiteName5$Temp.Avg.1[SiteName5$Temp.Avg.1 > 60] <- NA #(degrees C)
SiteName5$Ka.Avg.1[SiteName5$Ka.Avg.1 > 80] <- NA #permittivity (no units)
SiteName5$ECb.Avg.1[SiteName5$ECb.Avg.1 > 5000] <- NA #bulk electrical conductivity (uS/cm)
SiteName5$ECw.Avg.1[SiteName5$ECw.Avg.1 < 55000] <- NA #pore water electrical conductivity (uS/cm)
SiteName5$VWC.Avg.1[SiteName5$VWC.Avg.1 > 100] <- NA #volumetric water content (%)

#convert O2 voltage to % O2 for each sensor
CF.O2.1<-20.95/(SiteName5.amb.cal.1-NL_T.N2.cal.1) #correction factor for O2 sensor 1
SiteName5$O2M.1<-CF.O2.1*SiteName5$O2.Avg.1-CF.O2.1*NL_T.N2.cal.1 #measured %O2 (before temp correction)

#apply temperature correction to O2 measurements
SiteName5$O2.Avg.Tcorrected.1<-SiteName5$O2M.1-8.213*(10^-7)*(SiteName5$Temp.Avg.1)^3+1.422*(10^-3)*(SiteName5$Temp.Avg.1)^2-6.949*(10^-2)*SiteName5$Temp.Avg.1-(-8.213*(10^-7)*22^3+1.422*(10^-3)*22^2-6.949*(10^-2)*22)

#elevation correction
P.SiteName5<-101.325-101.325*(1-(1-(SiteName5.elev)/44307.69231)^5.25328)
SiteName5$O2.Avg.TPcorrected.1<-SiteName5$O2.Avg.Tcorrected.1*(P.SiteName5/101.325)

#plot data to check that it looks ok
#O2
p1<-ggplot(data=SiteName5,aes(x=DateTime15,y=O2.Avg.TPcorrected.1))+geom_point()+theme_classic()+labs(x="",y="O2 (%)")

#CO2
p2<-ggplot(data=SiteName5,aes(x=DateTime15,y=CO2.Avg.1))+geom_point()+theme_classic()+labs(x="",y="CO2 (ppm)")

#VWC
p3<-ggplot(data=SiteName5,aes(x=DateTime15,y=VWC.Avg.1))+geom_point()+theme_classic()+labs(x="",y="VWC (%)")

#Temperature
p4<-ggplot(data=SiteName5,aes(x=DateTime15,y=Temp.Avg.1))+geom_point()+theme_classic()++labs(x="",y="Temperature (degrees C)")

#ECb
p5<-ggplot(data=SiteName5,aes(x=DateTime15,y=ECb.Avg.1))+geom_point()+theme_classic()+labs(x="",y="Bulk EC (us/cm)")

#ECw
p6<-ggplot(data=SiteName5,aes(x=DateTime15,y=ECw.Avg.1))+geom_point()+theme_classic()+labs(x="",y="Porewater EC (uS/cm)")

#Permittivity
p7<-ggplot(data=SiteName5,aes(x=DateTime15,y=Ka.Avg.1))+geom_point()+theme_classic()+labs(x="",y="Permittivity")

#Battery voltage
p8<-ggplot(data=SiteName5,aes(x=DateTime15,y=Batt.V))+geom_point()+theme_classic()+labs(x="",y="Battery Voltage (V)")

#Export files as pdf saved with SiteName5
#a1<-annotate_figure((ggarrange(p1+theme(axis.text.x = element_blank()),(p2+theme(axis.text.x = element_blank())),(p3+theme(axis.text.x = element_blank())),(p4+theme(axis.text.x = element_blank())),(p5+theme(axis.text.x = element_blank())),(p6+theme(axis.text.x = element_blank())),(p7+theme(axis.text.x = element_blank())),p8,ncol=1,nrow=8,align ="v",font.label=list(size=4))),top=text_grob("Site 1",face="bold",size=14))
a9<-annotate_figure((ggarrange((p1+theme(axis.text.x = element_blank())),(p2+theme(axis.text.x = element_blank())),(p3+theme(axis.text.x = element_blank())),p4,ncol=1,nrow=4,align ="v",font.label=list(size=4))),top=text_grob("NB_T",face="bold",size=14))
a10<-annotate_figure((ggarrange((p5+theme(axis.text.x = element_blank())),(p6+theme(axis.text.x = element_blank())),(p7+theme(axis.text.x = element_blank())),p8,ncol=1,nrow=4,align ="v",font.label=list(size=4))),top=text_grob("NB_T",face="bold",size=14))

pdf("Reynolds Sensor Plots.pdf")
a1
a2
a3
a4
a5
a6
a7
a8
a9
a10
dev.off()

#Or this puts each plot onto separate page
#pdf("NL_M.pdf",onefile=TRUE)
#p1
#p2
#p3
#p4
#p5
#p6
#p7
#p8
#dev.off()
