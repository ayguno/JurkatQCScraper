################################################################################
# Author: Ozan Aygun
# Date: 01/27/2017 
#
# Purpose: this is the global code for the Shiny app "JurkatQC Scraper Ver 0.3"
# The global functions will be defined here.
# Libraries to run the app are also launched here.
# Also provides the initial access to the API-stored data to launch the ui definition.
# Server.R provides a dynamic means of updating the most recent form of the API-stored
# data, especially importnat in the event of serving mutiple users simultaneously.
################################################################################


#################################################################
## global parameters
#################################################################

## app name
APPNAME <<- sub('.*/','',getwd())





library(RColorBrewer)

library(shiny)

library(maptools)

library(plotly)

library(colorspace)

library(dplyr)
library(ape)

library(googleVis)
library(lubridate)
library(shinydashboard)
library(stringr)

source("pheatmap.r")

require(grid)
require(gtable)
require(scales)

require(rdrop2)



  
###############################################################################
# Reads the available QC data that is actively scraped by the "recoverJurkatQC.R"  
###############################################################################

#If archive exists, read from the local folder (when running the app locally)

if(file.exists("QMreportArchive.rds")) {
        report <<- readRDS("QMreportArchive.rds")
        latest_scan <<-readRDS("latest_scan.rds")
}

# If archive is not found use the archived version in dropbox
if(!file.exists("QMreportArchive.rds")) {
        token<- readRDS("droptoken.rds")
        drop_get("QMreportArchive.rds",dtoken = token)
        drop_get("latest_scan.rds",dtoken = token)
        report <<- readRDS("QMreportArchive.rds")
        latest_scan <<-readRDS("latest_scan.rds")
        unlink("QMreportArchive.rds") #Delete after reading the archived report.
        unlink("latest_scan.rds")
}
# Locally tested and the app can download and read the report from dropbox sucessfully




report$time <- ymd_hms(report$time)
report$date <- date(report$date)
report <- report[which(report$date >= date("2016-01-01")),]
report$weekdays <- weekdays.Date(report$date)

# Prepare a time-based summary of the QM report

report_summary <- report %>% group_by(instrument,date) %>% 
        summarize(MSMS_Spectra_Collected = median(MS.MSspectracollectedC,na.rm=T),
                  MSMS_Spectra_Valid = median(MS.MSspectravalidV,na.rm=T),
                  Collection_Yield = median(CollectionYieldV.C..., na.rm=T),
                  Validation_Yield = median(ValidationYieldV.F...,na.rm = T),
                  Median_SPI = median(MedianSPI...,na.rm = T),
                  Mean_Precursor_Mass_Error_ppm = median(MeanPrecursorMassError.ppm.,na.rm =T),
                  Timespan_mid_90percent_matched_spectra_in_run_min = median(Timespanmid90.matchedspectrainrun.min.,na.rm = T),
                  Gradient_shape_mid_90percent_matched_spectra_in_run_min = median(GradientShapemid90.matchedspectrainrun, na.rm = T),
                  Median_MS1_peak_width_mid_90percent_matched_spectra_sec = median(MedianMS1peakwidthmid90.matchedspectra.sec.,na.rm=T),
                  Median_MS2_filltime_mid_90percent_matched_spectra_msec = median(MedianMS2filltimemid90.matchedspectra.msec.,na.rm = T),
                  Digest_Missed_Cleavages_trypsin_Distinctpeps_CS = median(DigestMissedCleavagestrypsin..DistinctpepsCS.,na.rm = T),
                  DistinctPeps_CS_Total_number_of= median(DistinctPepsCSTotal.number_of., na.rm = T),
                  FDRSpectra = median(FDRSpectra...,na.rm = T),
                  number_of_Spectra_False = median(number_ofSpectraFalse,na.rm = T))

report_summary <-as.data.frame(report_summary)
report_summary$instrument <- as.factor(report_summary$instrument)
report_summary <- report_summary[!is.na(report_summary$date),]

# Prepare the parameter-centric report with better variable names

QM_report <- report[!is.na(report$DistinctPepsCSTotal.number_of.),]
names(QM_report)[2:15] <- names(report_summary)[3:16] 



        