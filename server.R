################################################################################
# Author: Ozan Aygun
# Date: 01/27/2017 
#
# Purpose: this is the server logic code for the Shiny app "JurkatQC Scraper Ver 0.3"
# 
################################################################################



library(shiny)
library(shinydashboard)
library(googleVis)



shinyServer(function(input, output,session) {
        
        options(shiny.maxRequestSize=30*1024^2) #to the top of server.R 
        # this would increase the upload limit to 30MB    

withProgress({
        isolate({
                
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
                
                # Extract to add user attribute to QM_report, to use in "facts and figures" (tab5)
                
                
                initials <- str_extract(QM_report$File,"(_[a-zA-Z][a-zA-Z][a-zA-Z]_)|_[a-zA-Z][a-zA-Z]_" )
                
                QM_report <- data.frame(QM_report, MS_user=initials, stringsAsFactors = FALSE)
                QM_report$MS_user[which(is.na(QM_report$MS_user))] <- "_unknown_"
                
                QM_report$MS_user <- str_trim(gsub("_"," ",QM_report$MS_user),side="both")
                QM_report$MS_user[which(QM_report$MS_user == "GCP" | QM_report$MS_user == "QC" )] <- "unknown"
                
                QM_report$MS_user <- toupper(QM_report$MS_user)
                QM_report$MS_user[which(QM_report$MS_user == "UNKNOWN")] <- "unknown"
                
                # Extract to add LC name attribute to QM_report, wherever possible:
                
                LC_label_extract <- substr(QM_report$File,1,3)
                LC_labels <- ifelse(substr(LC_label_extract,1,2) == "H2",substr(LC_label_extract,3,3),substr(LC_label_extract,2,2))
                QM_report$LC_labels <- toupper(ifelse(grepl("[a-zA-Z]",LC_labels),LC_labels,"UNKNOWN"))
                
                
                # Prepare the Gauge report for monitors
                
                Gauge_report <- QM_report %>% 
                        group_by(instrument) %>% 
                        summarise(Historical_median = median(DistinctPeps_CS_Total_number_of, na.rm = T),
                                  percentile_95 = quantile(DistinctPeps_CS_Total_number_of,probs = seq(0,1,0.05))[20],
                                  percentile_75 = quantile(DistinctPeps_CS_Total_number_of)[4],
                                  percentile_25 = quantile(DistinctPeps_CS_Total_number_of)[2],
                                  percentile_10 = quantile(DistinctPeps_CS_Total_number_of,probs = seq(0,1,0.05))[3])
                
                QM_report <- arrange(QM_report, instrument, desc(time),desc(File))
                temp <- data.frame(instrument = unique(QM_report$instrument))
                for(i in seq_along(temp$instrument)){
                        w <- which(QM_report$instrument == temp$instrument[i])
                        temp$Most_recent_QC[i] <- QM_report$DistinctPeps_CS_Total_number_of[w][1]
                        temp$time[i] <- as.character(QM_report$time[w][1])
                        temp$File[i] <- as.character(QM_report$File[w][1])
                }
                
                Gauge_report <- merge(Gauge_report,temp,by = "instrument")
                Gauge_report$instrument <-as.factor(Gauge_report$instrument)
                Gauge_report$Most_recent_QC <- as.numeric(Gauge_report$Most_recent_QC)
                
                
                # Prepare the latest QC report for latestQC tab:
                # QM_report$time <- as.character(QM_report$time)
                # temp1 <- merge(QM_report,temp, by= c("instrument","time", "File" ), sort = F)
                # temp1$time<- substr(temp1$time, 12,nchar(temp1$time)-3)
                # 
                # latest <- temp1 %>% select(instrument,MS_user,date,weekdays,time,File,MSMS_Spectra_Collected:number_of_Spectra_False)
                # names(latest)[which(names(latest)== "time")] <- "Time (EST)"
                # names(latest)[which(names(latest)== "weekdays")] <- "Day"
                # latest$date <- as.character(latest$date)
                
                # read the existing comments file to use in the latest QC tab (work with dropbox copy)
                token<- readRDS("droptoken.rds")
                drop_get("instrumentcomments.rds",dtoken = token, overwrite = TRUE) # Get from dropbox
                comments <- readRDS("instrumentcomments.rds")
                
                
                number_of_dates <- length(min(QM_report$date):Sys.Date())
                instrument <- rep(unique(QM_report$instrument),number_of_dates)
                
                x <- min(QM_report$date):Sys.Date()
                date <- rep(x,each = length(unique(QM_report$instrument)))
                
                date_update <- data.frame(date,instrument, stringsAsFactors = FALSE)
                date_update$date <- as.Date(date_update$date,origin = "1970-01-01")
                
                comments <- merge(date_update,comments,by = c("date","instrument"), all=T, sort = F)
                
                if(sum(is.na(comments$usercomments))>0){
                        comments$usercomments[is.na(comments$usercomments)] <- ""
                }
                
                # Provide information about the latest scrape
                
                latest_scan <<- unlist(strsplit(as.character(latest_scan[1,1])," "))
                latest_scan <-c(weekdays.Date(date(latest_scan[1])),latest_scan)
                
                # Increment and update the user counter (work with dropbox copy)
                token<- readRDS("droptoken.rds")
                drop_get("user_counter.rds",dtoken = token, overwrite = TRUE) # Get from dropbox
                user_counter <- readRDS("user_counter.rds")
                temp_user_counter <-data_frame(users=1,date=as.character(Sys.Date()),
                                               time=unlist(strsplit(as.character(Sys.time())," "))[2],
                                               weekday=weekdays(Sys.Date()))
                user_counter <- rbind(user_counter,temp_user_counter)
                saveRDS(user_counter,"user_counter.rds")
                drop_upload("user_counter.rds",dtoken = token)# push back to dropbox
                
                ##############################################################
                # Read the active.archive.rds from API 
                ##############################################################
                token<- readRDS("droptoken.rds")
                drop_get("active.archive.rds",dtoken = token)
                drop_get("latest_active_archive_patrol.rds",dtoken = token)
                
                active.archive <<- readRDS("active.archive.rds")
                latest_active_archive_patrol <<-readRDS("latest_active_archive_patrol.rds")
                
                unlink("active.archive.rds") #Delete after reading the archived report.
                unlink("latest_active_archive_patrol.rds")
                active.archive$date <- as.Date(active.archive$ctime)
                active.archive <- filter(active.archive, date > as.Date("2015-09-13"))
                
                
                
                
        }) },message = "Checking for the latest JurkatQC scan")
        
        
                
        
###############################################################################        
# Actual computation for the welcome tab       
###############################################################################        
        
output$latest_update <- renderInfoBox({
                     
latest_scrape <-paste("The latest JurkatQC scan has occured on\n",latest_scan[1],", ",
                      latest_scan[2]," at ",latest_scan[3]," EST",sep = "")

infoBox("Latest Scrape",subtitle = latest_scrape, 
        icon = icon("bullhorn"),fill = TRUE,color = "maroon")


        
})        
        
output$QC_size <-renderInfoBox({
        
        infoBox(title="Archived",value=paste(nrow(QM_report)," Jurkat QC records",sep = "")
                ,subtitle="and still recording...",
                 color = "purple", icon = icon("feed"),fill = TRUE)
})        

output$user_size <- renderInfoBox({
        user_count = sum(user_counter$users)
        infoBox(title= "Proudly served", value = paste(user_count," users",sep=""), 
                subtitle = "since Jan 1st, 2017",
                icon = icon("users"),color = "green", fill = TRUE)
})


### Generating dynamic InfoBoxes as action links to redirect to tabs

output$QC_summary_box <-renderUI({

        infoBox("Monitor daily summary", fill = TRUE, color = "orange",
                subtitle = "Daily summary of Mass Spectrometer Quality Metrics")
})

observeEvent(input$link_to_QC_summary, {
        newvalue <- "dailysummary"
        updateTabsetPanel(session, "tabitems",selected= newvalue)
})

################
output$timeseries_box <-renderUI({
        
        infoBox("Monitor timeseries", icon = icon("line-chart"), fill = TRUE,
                subtitle ="Quality metrics time-series monitoring",color = "purple")
})

observeEvent(input$link_to_timeseries, {
        newvalue <- "timeseries"
        updateTabsetPanel(session, "tabitems",selected= newvalue)
})
#################

output$current_box <-renderUI({
        
        infoBox("Current MS Conditions",icon = icon("dashboard"),fill = TRUE,
                color = "fuchsia",subtitle = "Check the distinct peptide gauges for all instruments")
})

observeEvent(input$link_to_current, {
        newvalue <- "current"
        updateTabsetPanel(session, "tabitems",selected= newvalue)
})

#################

output$thelatest_box <-renderUI({
        
        infoBox("Latest MS Quality Metrics",icon = icon("table"),fill = TRUE,
                color = "blue",subtitle = "Check the detailed latest quality metrics reports for all instruments")
})

observeEvent(input$link_to_thelatest, {
        newvalue <- "thelatest"
        updateTabsetPanel(session, "tabitems",selected= newvalue)
})

#################

output$facts_box <-renderUI({
        
        infoBox("Facts and Figures",icon = icon("users"),fill = TRUE, 
                color = "maroon",subtitle = "Detalied analytics focused on instruments,
                MS users or weekdays")
})

observeEvent(input$link_to_facts, {
        newvalue <- "facts"
        updateTabsetPanel(session, "tabitems",selected= newvalue)
})


#################

output$instrument_centric_box <-renderUI({
        
        infoBox("Instrument vs User Analytics",icon = icon("cogs"),fill = TRUE,
                color = "olive", subtitle = "Inspect instrument
                quality metrics by MS users and weekdays")
})

observeEvent(input$link_to_instrument_centric, {
        newvalue <- "instrument_centric"
        updateTabsetPanel(session, "tabitems",selected= newvalue)
})

#################


#################

output$comment_box <-renderUI({
        
        infoBox("Instrument comments",icon = icon("wrench"),fill = TRUE,
                color = "teal", subtitle = "I have comments or maintanence 
                records related to an instrument.")
})

observeEvent(input$link_to_thelatest2, {
        newvalue <- "thelatest"
        updateTabsetPanel(session, "tabitems",selected= newvalue)
})

#################

#################

output$support_box <-renderUI({
        
        infoBox("Support",icon = icon("bug"),fill = TRUE,
                color = "red", value = "I found a bug")
})

observeEvent(input$link_to_support, {
        newvalue <- "support"
        updateTabsetPanel(session, "tabitems",selected= newvalue)
})

#################

#################

output$tutorial_box <-renderUI({
        
        infoBox(title = "New!",icon = icon("film"),fill = TRUE,width = 3,
                color = "green", value = "Watch tutorial",subtitle = "Get Started!")
})

observeEvent(input$link_to_tutorial, {
        newvalue <- "tutorial"
        updateTabsetPanel(session, "tabitems",selected= newvalue)
})

#################

#################
output$downtime_box <-renderUI({
        
        infoBox(title = "New!",icon = icon("clock-o"),fill = TRUE,width = 3,
                color = "red", value = "Monitor MS downtime",subtitle = "Click Here!")
})

observeEvent(input$link_to_downtime, {
        newvalue <- "downtime"
        updateTabsetPanel(session, "tabitems",selected= newvalue)
})

###############################################################################        
# Actual computation for the first tab (motion plots)       
###############################################################################         
        
 
output$motionplot<- renderGvis(expr = { 
 
withProgress(expr = {                
        myStateSettings <<-'
        {"xZoomedDataMax":9,"xLambda":1,"yZoomedIn":false,"yZoomedDataMax":70000,"playDuration":15000,"iconType":"VBAR","orderedByY":false,"xAxisOption":"_ALPHABETICAL","orderedByX":true,"yAxisOption":"13","time":"2016-10-30","uniColorForNonSelected":false,"yZoomedDataMin":0,"dimensions":{"iconDimensions":["dim0"]},"yLambda":1,"iconKeySettings":[{"key":{"dim0":"Hubble1"}},{"key":{"dim0":"Hubble2"}},{"key":{"dim0":"McClintock"}},{"key":{"dim0":"Tesla"}},{"key":{"dim0":"Curie"}},{"key":{"dim0":"Beaker"}},{"key":{"dim0":"Franklin"}},{"key":{"dim0":"Galileo"}},{"key":{"dim0":"Yoda"}}],"duration":{"timeUnit":"D","multiplier":1},"colorOption":"_UNIQUE_COLOR","showTrails":false,"xZoomedDataMin":0,"xZoomedIn":false,"nonSelectedAlpha":0.4,"sizeOption":"_UNISIZE"}
                '
        
        gvisMotionChart( report_summary, 
                         idvar = "instrument", timevar = "date", 
                         options = list(width=920,height=350, showChartButtons =T, 
                                        sizeAxis = 0.5,showXMetricPicker =F,showAdvancedPanel=T,
                                        state = myStateSettings)) 
        
                        },message = "Preparing the daily summary chart")
        
        })  # renderplot closure
        
        
###############################################################################        
# Actual computation for the second tab (time-series plots)       
###############################################################################

reTab2<- eventReactive( c(input$action2,input$action3), {  
       
withProgress(expr = {
        
        start_date <<- date(format(input$range)[1])
        end_date <<- date(format(input$range)[2])
        
        QM_report <- QM_report %>% filter(date >= start_date & date <= end_date)
        
        QMy <- QM_report[,input$QM2]
        tx <-as.POSIXct(QM_report$time)
        
        tendered <<- data.frame(QMy,Date_time = tx, Instrument = QM_report$instrument, 
                                Raw.file= QM_report$File)
        names(tendered)[1] <<- input$QM2
        Qmetric <<- gsub("_"," ",names(tendered)[1])
        
        
        p <- ggplot(data = tendered, aes_string(x = "Date_time" , y = names(tendered)[1] ))+
               
                geom_point(aes_string(colour = "Instrument"), size = 0.8)+
                
                geom_line(aes_string(colour = "Instrument"))+labs(x="Date and time (EST)")+
                scale_color_discrete()+
                #scale_color_brewer(type= "qual" , palette = 2)+ # depreciated since we have more than 8 instruments to monitor now
                theme(panel.background=element_rect(fill = "white",colour = "black"),
                      panel.border=element_rect(colour = "black", fill = NA))
                
      
        
        ggplotly(p)
        
     },message = "Preparing the timeseries chart") 

}, ignoreNULL = FALSE) # end of the eventReactive

output$timeplot<- renderPlotly(expr = { 
        
        reTab2()        

        
})  # renderplotly closure



###############################################################################        
# Actual computation for the third tab (Gauge_plot)       
###############################################################################



output$Gauge_plot <- renderGvis({

        session$clientData$output_trend_width
        session$clientData$output_trend_height
        
        withProgress(expr = {        
        
                instrument_list <- as.character(Gauge_report$instrument)
                gauge_list <-list(NULL)
                
                
                
                for(i in seq_along(instrument_list)) {
                        
                        
                        Gauge_report_temp <- Gauge_report %>% dplyr::select(instrument,Most_recent_QC) %>%
                                filter(instrument == instrument_list[i])
                        
                        MAX <- as.numeric(format(Gauge_report$percentile_95[which(Gauge_report$instrument == instrument_list[i])],digits = 5))
                        MEDIAN <- Gauge_report$Historical_median[which(Gauge_report$instrument == instrument_list[i])]
                        PER25 <- Gauge_report$percentile_25[which(Gauge_report$instrument == instrument_list[i])]
                        PER10 <- as.numeric(format(Gauge_report$percentile_10[which(Gauge_report$instrument == instrument_list[i])],digits = 5))
                        
                        gauge_list[[i]] <- gvisGauge(Gauge_report_temp, 
                                                     labelvar = "instrument", 
                                                     numvar = "Most_recent_QC",
                                                     options=list(min=PER10, max=MAX, greenFrom=MEDIAN,
                                                                  greenTo=MAX, yellowFrom=PER25, yellowTo=MEDIAN,
                                                                  redFrom=PER10, redTo=PER25, width=165 ))
          
                }
                
                # Make a key table to read gauge plots
                
                KEY <- data.frame(Gauge_Color = c("Green", "Yellow","Red"),
                                  Percentile_range = c("Median - 95%","25% - Median","10% - 25%"))
                names(KEY) <- c("Color", "Percentile Range")
                
                # Convert the key table to a gvis plot
                
                gauge_list[[length(gauge_list)+1]] <- gvisTable(KEY,options = list(width = 160))
                                                                                   
                
                
                # Here I need to make a custom gVis merge function
                merge_function <- function(x,y) {gvisMerge(x,y,horizontal = TRUE)}
                
                # Successively apply this function to merge gauge plots 
                A<-Reduce(merge_function,gauge_list[1:5])
                B<-Reduce(merge_function,gauge_list[6:10])
                
                AB <- gvisMerge(A,B)
                
                return(AB)
        
        },message = "Preparing the gauge report")
        
})




output$gaugetable <- renderGvis( {
        Gauge_report <- Gauge_report[,-(which(grepl("percentile",names(Gauge_report))))]
        names(Gauge_report) <- gsub("_"," ",names(Gauge_report))
        Gauge_report$TimeEST <- substr(Gauge_report$time, 12,nchar(Gauge_report$time)-3)
        Gauge_report$time <- substr(Gauge_report$time, 1,11)
        names(Gauge_report)[which(names(Gauge_report)== "time")] <- "Date"
        names(Gauge_report)[which(names(Gauge_report)== "TimeEST")] <- "Time (EST)"
        gvisTable( Gauge_report, options = list(height = 196, 
                                                width = 450, 
                                                alternatingRowStyle = T,
                                                frozenColumns = 1)) 
        })


###############################################################################        
# Actual computation for the fourth tab (latestQC)       
###############################################################################

observeEvent(input$action8,{

withProgress(expr = {        
        com_date  <<- as.character(date(format(input$comment_date)))

        Instrument <<- as.character(input$Instrument_commented)
        
        user_initials <- as.character(input$Commenting_user)
        
        user_comment <- as.character(input$comment)
        
        w <- which(as.character(comments$date) == com_date & comments$instrument == Instrument)
        
        
        if(comments$usercomments[w] != ""){comments$usercomments[w] <<- paste(comments$usercomments[w],
                                                                        user_initials,"on",com_date,":",user_comment,"***", sep = " ")}
        
        if(comments$usercomments[w] == ""){comments$usercomments[w] <<- paste("***",user_initials,"on",com_date,":",user_comment,"***", sep = " ")}
        
        
        saveRDS(comments, file = "instrumentcomments.rds")
        token<- readRDS("droptoken.rds")
        drop_upload("instrumentcomments.rds",dtoken = token,overwrite = TRUE) #also update the dropbox version
        
        reTab4()
        
        },message = "Updating the user comments")
})

reTab4_2 <- eventReactive(input$action8,{
        
        box(title = "Do you have comments about an instrument?", width = 9, status = "danger",
            solidHeader = TRUE, background = "navy",
            collapsible = TRUE, 
      
          column(6,      
            
            h5(icon("sticky-note-o"), "Tell me what you know about the instrument conditions."),
            selectInput("Commenting_user",
                        label = "Who are you?",selected = "AO",
                        choices = unique(QM_report$MS_user),
                        width = 100),
            h5("Which instrument are you talking about?"),  
            selectInput("Instrument_commented", 
                        label = NULL,
                        choices = unique(QM_report$instrument),
                        width = 100),
            h5("The date for the instrument condition"),
            dateInput("comment_date",label = NULL,
                      min = min(QM_report$date),
                      width = 100)
          ),
          
          column(6,
            h5("Enter your comments",icon("commenting-o"),"and maintanence records",icon("wrench") ,"here:"),
            textAreaInput("comment",label = NULL,rows=8,width = "100%",height = "100%"),
            actionButton("action8",label = "Post your comments")
            
          )
        )
        
}, ignoreNULL = FALSE)



output$Reactive_comments <-renderUI({
        
        reTab4_2()
        
})



reTab4 <- eventReactive(c(input$action4,input$action8),{ 

       
withProgress(expr = {        
        
# Update the latest QC report to allow date-dependent changes in the latestQC tab:

alt_date <<- date(input$alternativedate)
QM_rep <- QM_report %>% filter(date <= alt_date)        

QM_rep <- arrange(QM_rep, instrument, desc(time), desc(File))
temp2 <- data.frame(instrument = unique(QM_rep$instrument))
for(i in seq_along(temp2$instrument)){
        w <- which(QM_rep$instrument == temp2$instrument[i])
        temp2$Most_recent_QC[i] <- QM_rep$DistinctPeps_CS_Total_number_of[w][1]
        temp2$time[i] <- as.character(QM_rep$time[w][1])
        temp2$File[i] <- as.character(QM_rep$File[w][1])
}        
        
QM_rep$time <- as.character(QM_rep$time)
temp3 <- merge(QM_rep,temp2, by= c("instrument","time","File" ), sort = F)
temp3$time<- substr(temp3$time, 12,nchar(temp3$time)-3)

latest <<- temp3 %>% dplyr::select(instrument,MS_user,date,weekdays,time,File,MSMS_Spectra_Collected:number_of_Spectra_False)
names(latest)[which(names(latest)== "time")] <<- "Time (EST)"
names(latest)[which(names(latest)== "weekdays")] <<- "Day"
latest$date <<- as.character(latest$date)

temp2 <- data.frame(instrument = unique(comments$instrument),
                    latest_instrument_user_comment_recorded=1:9,latest_comment_date = 1:9,stringsAsFactors = FALSE)
comments <- comments %>% filter(date <= alt_date) %>% arrange(instrument,desc(date))
for(i in seq_along(temp2$instrument)){
        x <- comments %>% filter(comments$instrument == temp2$instrument[i] & comments$usercomments != "") %>%
                arrange(desc(date))
        temp2$latest_instrument_user_comment_recorded[i] <- x$usercomments[1]
        temp2$latest_comment_date[i] <- as.character(x$date[1])
}



latest <<- merge(latest,temp2,by="instrument",sort=F)

w<- which(colnames(latest) == "latest_instrument_user_comment_recorded")
colnames(latest)[w] <<- "The_latest_mass_spectrometry_user_comment_recorded_by_the_selected_calendar_date"

},message = "Preparing the latest QC report")

},ignoreNULL = FALSE)





observeEvent(c(input$action4,input$action8),{     
        
        isolate(reTab4())
        
output$latestQC <- DT::renderDataTable(
        
        latest, extensions = c('FixedColumns'),
        options = list(scrollX = TRUE, scrollY= TRUE, paging= FALSE, 
                       fixedColumns = list(leftColumns = 5, rightColumns = 0),
                       searching = TRUE)
        
) 


}, ignoreNULL = FALSE)



reTab_allQC <- eventReactive(input$Instrument_choice_allQC,{
        
        instrument.allQC <- input$Instrument_choice_allQC
        allQC.table.temp <- QM_report %>% filter(instrument == instrument.allQC)
        allQC.table.temp <- arrange(allQC.table.temp, instrument, desc(time), desc(File))
        allQC.table <<- allQC.table.temp %>% dplyr::select(instrument,MS_user,date,weekdays,time,File,MSMS_Spectra_Collected:number_of_Spectra_False)
        
},ignoreNULL =FALSE)


observeEvent(input$Instrument_choice_allQC,{     
        
        isolate(reTab_allQC())
        
        output$allQC <- DT::renderDataTable(
                
                allQC.table, extensions = c('FixedColumns',"FixedHeader"),
                options = list(scrollX = TRUE, scrollY= TRUE, paging= FALSE, fixedHeader = TRUE,
                               fixedColumns = list(leftColumns = 5, rightColumns = 0),
                               searching = TRUE)
                
        ) 
        
        
}, ignoreNULL = FALSE)






###############################################################################        
# Actual computation for the fifth tab (facts)       
###############################################################################

reTab5 <- eventReactive(input$action5,{
 
withProgress(expr = {         
        
        int_report <<- QM_report[, c("MS_user", "weekdays", "instrument")]        
        intw <<- which(names(int_report) == input$centric)  
        
      
        
        names(int_report)[intw] <<- "focus"
        names(int_report)[!(names(int_report)== "focus")] <<- c("int1", "int2")
        
        
      

        if(input$centric == "MS_user"){        
                myStateSettings <<-'
                {"playDuration":15000,"orderedByX":true,"uniColorForNonSelected":false,"xAxisOption":"2","xZoomedDataMin":0,"yLambda":1,"iconKeySettings":[{"key":{"dim0":"HK"}},{"key":{"dim0":"SF"}},{"key":{"dim0":"MSZ"}},{"key":{"dim0":"OA"}},{"key":{"dim0":"TS"}},{"key":{"dim0":"GG"}},{"key":{"dim0":"MP"}},{"key":{"dim0":"SAM"}},{"key":{"dim0":"BT"}},{"key":{"dim0":"FM"}},{"key":{"dim0":"AO"}},{"key":{"dim0":"MAG"}},{"key":{"dim0":"JGC"}},{"key":{"dim0":"CRH"}},{"key":{"dim0":"PM"}},{"key":{"dim0":"SV"}},{"key":{"dim0":"LCT"}},{"key":{"dim0":"SE"}},{"key":{"dim0":"unknown"}}],"yAxisOption":"2","colorOption":"_UNIQUE_COLOR","showTrails":false,"dimensions":{"iconDimensions":["dim0"]},"xZoomedIn":false,"duration":{"timeUnit":"D","multiplier":1},"sizeOption":"_UNISIZE","xZoomedDataMax":19,"xLambda":1,"yZoomedIn":false,"yZoomedDataMax":300,"time":"2016-12-21","iconType":"VBAR","nonSelectedAlpha":0.4,"yZoomedDataMin":0,"orderedByY":false}
                '        
        }
        
        if(input$centric == "weekdays"){
                myStateSettings <<-'
                {"xZoomedDataMax":7,"time":"2016-12-21","sizeOption":"_UNISIZE","yAxisOption":"2","yZoomedIn":false,"uniColorForNonSelected":false,"orderedByY":false,"yZoomedDataMax":300,"iconType":"VBAR","dimensions":{"iconDimensions":["dim0"]},"yZoomedDataMin":0,"xAxisOption":"2","duration":{"timeUnit":"D","multiplier":1},"showTrails":false,"xZoomedDataMin":0,"playDuration":15000,"yLambda":1,"xZoomedIn":false,"nonSelectedAlpha":0.4,"iconKeySettings":[{"key":{"dim0":"Saturday"}},{"key":{"dim0":"Sunday"}},{"key":{"dim0":"Wednesday"}},{"key":{"dim0":"Monday"}},{"key":{"dim0":"Thursday"}},{"key":{"dim0":"Friday"}},{"key":{"dim0":"Tuesday"}}],"xLambda":1,"colorOption":"_UNIQUE_COLOR","orderedByX":true}
                '        
        }
        
        if(input$centric == "instrument"){        
                myStateSettings <<-'
                {"nonSelectedAlpha":0.4,"orderedByY":false,"yZoomedDataMin":0,"orderedByX":true,"xAxisOption":"2","dimensions":{"iconDimensions":["dim0"]},"uniColorForNonSelected":false,"playDuration":15000,"xLambda":1,"yLambda":1,"yAxisOption":"2","duration":{"timeUnit":"D","multiplier":1},"xZoomedIn":false,"colorOption":"_UNIQUE_COLOR","yZoomedIn":false,"xZoomedDataMax":9,"sizeOption":"_UNISIZE","time":"2017-01-24","showTrails":false,"xZoomedDataMin":0,"yZoomedDataMax":300,"iconKeySettings":[{"key":{"dim0":"Franklin"}},{"key":{"dim0":"Galileo"}},{"key":{"dim0":"Hubble1"}},{"key":{"dim0":"Hubble2"}},{"key":{"dim0":"Curie"}},{"key":{"dim0":"Tesla"}},{"key":{"dim0":"Yoda"}},{"key":{"dim0":"Beaker"}},{"key":{"dim0":"McClintock"}}],"iconType":"VBAR"}
                '                
        }        
        

# Re-organize QM_report to use in this tab  
                
QM_report5 <<- QM_report
                        
w <<- which(names(QM_report5) == input$centric)  

names(QM_report5)[w] <<- "focus"



dates <<- min(QM_report5$date):max(QM_report5$date)

class(dates) <<- "Date"


nQC <<- NULL
for(i in seq_along(dates )){
        
temp5 <<- QM_report5 %>% filter(date <= dates[i]) %>% group_by(focus) %>%
                         summarize(number_of_QC = n()) %>% 
                                mutate(date = dates[i])

nQC <<- rbind(nQC,temp5)

}

nQC <- data.frame(nQC)


 },message = "Performing focus-specific computations")

}, ignoreNULL = FALSE)


output$nQM_centric<- renderGvis(expr = { 
withProgress(expr = {
        if(input$action5 == 0){        
                myStateSettings <<-'
                {"playDuration":15000,"orderedByX":true,"uniColorForNonSelected":false,"xAxisOption":"2","xZoomedDataMin":0,"yLambda":1,"iconKeySettings":[{"key":{"dim0":"HK"}},{"key":{"dim0":"SF"}},{"key":{"dim0":"MSZ"}},{"key":{"dim0":"OA"}},{"key":{"dim0":"TS"}},{"key":{"dim0":"GG"}},{"key":{"dim0":"MP"}},{"key":{"dim0":"SAM"}},{"key":{"dim0":"BT"}},{"key":{"dim0":"FM"}},{"key":{"dim0":"AO"}},{"key":{"dim0":"MAG"}},{"key":{"dim0":"JGC"}},{"key":{"dim0":"CRH"}},{"key":{"dim0":"PM"}},{"key":{"dim0":"SV"}},{"key":{"dim0":"LCT"}},{"key":{"dim0":"SE"}},{"key":{"dim0":"unknown"}}],"yAxisOption":"2","colorOption":"_UNIQUE_COLOR","showTrails":false,"dimensions":{"iconDimensions":["dim0"]},"xZoomedIn":false,"duration":{"timeUnit":"D","multiplier":1},"sizeOption":"_UNISIZE","xZoomedDataMax":19,"xLambda":1,"yZoomedIn":false,"yZoomedDataMax":300,"time":"2016-12-21","iconType":"VBAR","nonSelectedAlpha":0.4,"yZoomedDataMin":0,"orderedByY":false}
                '
        }
        reTab5()

        gvisMotionChart(nQC,idvar = "focus", timevar = "date",
                options = list(width=600,height=250,
                               showAdvancedPanel=F,
                               showXMetricPicker =F,showYMetricPicker=T,
                               showYScalePicker = F,
                               showSelectListComponent=F,
                               showSidePanel=F,
                               showHeader=F,
                               state = myStateSettings) )

                },message = "Preparing the Quality Metrics Distribution Chart")
        
})


        
reTab5_2 <- eventReactive(c(input$action5,input$action6),{
        
withProgress(expr = {        
        
        QM_selected<- as.character(input$QM_choice5)
        Focus_selected <- as.character(input$centric)
        
        QM_report$weekdays <- factor(QM_report$weekdays, 
                        levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
        
        cat(QM_selected," ", Focus_selected, "\n", str(QM_selected)," ", str(Focus_selected))
        
        
        p <- ggplot(data = QM_report, aes_string(x = Focus_selected , y = QM_selected, fill=Focus_selected  ))+
                geom_boxplot()+labs(fill="")+
                scale_fill_discrete()+
                theme(panel.background=element_rect(fill = "white",colour = "black"),
                      panel.border=element_rect(colour = "black", fill = NA))
        
        
        ggplotly(p)
        
},message = "Preparing the selected quality metric plot")
        
},ignoreNULL = FALSE)        
                        


output$QM_centric<- renderPlotly(expr = { 
        
        reTab5_2()        
        
        
})  # renderplotly closure



reTab5_3 <- eventReactive(input$action5,{
       
 withProgress(expr = {         
       int1_report_summary <- int_report %>% group_by(focus, int1) %>% 
               summarise(total = n() )
     

       int1_report_summary2 <- int1_report_summary %>% group_by(focus) %>%
               summarise(sum = sum(total))
       
       int1_report_summary <-merge(int1_report_summary,int1_report_summary2,by="focus")
       
       int1_report_summary$percent_total <- (int1_report_summary$total/int1_report_summary$sum)*100
       
       
       names(int1_report_summary)[names(int1_report_summary)=="int1"] <- "monitor"
          
      p53 <- ggplot(int1_report_summary,aes(x = focus, y = percent_total,fill = monitor)) + 
               geom_bar(position = "fill",stat = "identity")+
               scale_y_discrete(labels = percent_format())+ labs(fill="", x=" ", y="")+
               theme(axis.text.x=element_text(angle = 45),
                     panel.background = element_rect(fill = "white") 
                       )
      
      ggplotly(p53)
  
 }, message = "Preparing the user tendencies plot Part1")   
            
}, ignoreNULL = FALSE)



output$Interaction1 <- renderPlotly(expr = {
 reTab5()       
 reTab5_3()               
        
        
})



reTab5_4 <- eventReactive(input$action5,{
   
 withProgress(expr = {         
             
        int2_report_summary <- int_report %>% group_by(focus, int2) %>% 
                summarise(total = n() )
        
        int2_report_summary2 <- int2_report_summary %>% group_by(focus) %>%
                summarise(sum = sum(total))
        
        int2_report_summary <-merge(int2_report_summary,int2_report_summary2,by="focus")
        
        int2_report_summary$percent_total <- (int2_report_summary$total/int2_report_summary$sum)*100
        
        
        
        names(int2_report_summary)[names(int2_report_summary)=="int2"] <- "monitor"
        
        p54 <- ggplot(int2_report_summary,aes(x = focus, y = percent_total,fill = monitor)) + 
                geom_bar(position = "fill",stat = "identity")+
                scale_y_discrete(labels = percent_format())+ labs(fill="", x=" ", y="")+
                theme(axis.text.x=element_text(angle = 45),
                      panel.background = element_rect(fill = "white") 
                )
        
        ggplotly(p54)
        
 }, message = "Preparing the user tendencies plot Part2" )        
        
}, ignoreNULL = FALSE)

output$Interaction2 <- renderPlotly(expr = {

reTab5()                
reTab5_4()      
        
        
})


###############################################################################        
# Actual computation for the 6th tab (instrument vs user analytics)       
###############################################################################


reTab6_2<- eventReactive( c(input$action6_2,input$action6_2_2), {  
        
        withProgress(expr = {
                
                start_date <<- date(format(input$range6_2)[1])
                end_date <<- date(format(input$range6_2)[2])
                selected_instrument <<- as.character(input$Instrument_choice6_2)
                
                
                QM_report6_2 <<- QM_report %>% filter(date >= start_date & date <= end_date & instrument == selected_instrument)
                
                QMy <<- QM_report6_2[,input$QM_choice6_2]
                tx <<- QM_report6_2$time
                
                tendered <<- data.frame(QMy,Date_time = tx, Instrument = QM_report6_2$instrument, User = QM_report6_2$MS_user)
                names(tendered)[1] <<- input$QM_choice6_2
                Qmetric <<- gsub("_"," ",names(tendered)[1])
                
                
                p <- ggplot(data = tendered, aes_string(x = "Date_time" , y = names(tendered)[1] ))+
                        geom_point(aes_string(colour = "User"), size = 4)+
                        geom_line()+labs(x="Date and time (EST)")+
                        scale_color_brewer(type= "qual" , palette = 2)+
                        theme(panel.background=element_rect(fill = "white",colour = "black"),
                              panel.border=element_rect(colour = "black", fill = NA))
                
                
                ggplotly(p)
                
        },message = "Preparing the timeseries chart") 
        
}, ignoreNULL = FALSE) # end of the eventReactive



output$time_series_USER <- renderPlotly(expr = { 
        
        reTab6_2()        
        
        
})  # renderplotly closure



reTab6 <- eventReactive(input$action7,{
        
  withProgress(expr = {
          
        Instrument_selected <- as.character(input$Instrument_choice6)
        QM_selected <- as.character(input$QM_choice6)
        
        
        MS_user <- "MS_user"
        
        QM_report$weekdays <- factor(QM_report$weekdays, 
                                     levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
        
        QM_report6 <- QM_report %>% filter(instrument == Instrument_selected)
        

        p55 <- ggplot(data = QM_report6, aes_string(x = MS_user , 
                                                   y = QM_selected, fill= MS_user))+
                geom_boxplot()+labs(fill="", x="  ")+
                facet_grid(. ~ weekdays)+
                scale_fill_discrete() +
                theme(panel.background=element_rect(fill = "white",colour = "black"),
                            panel.border=element_rect(colour = "black", fill = NA),
                      axis.text.x = element_text(angle = 45),
                      strip.background = element_rect(fill = "darkblue"),
                      strip.text.x = element_text(colour = "white"))
                      
        
        ggplotly(p55)
        
  }, message = "Preparing the instrument vs user analytics")        
        
},ignoreNULL = FALSE) 





output$Interaction3 <- renderPlotly(expr = {
   
                        
        reTab6()              
        
})


###############################################################################        
# Actual computation for the 7th tab (LC vs MS analytics)       
###############################################################################

reTab7<- eventReactive( c(input$action9,input$action10), {  
        
        withProgress(expr = {
                
                start_date <<- date(format(input$range7)[1])
                end_date <<- date(format(input$range7)[2])
                selected_instrument <<- as.character(input$Instrument_choice7)
                
                
                QM_report7 <<- QM_report %>% filter(date >= start_date & date <= end_date & instrument == selected_instrument)
                
                QMy <<- QM_report7[,input$QM_choice7]
                tx <<- QM_report7$time
                
                tendered <<- data.frame(QMy,Date_time = tx, Instrument = QM_report7$instrument, LC = QM_report7$LC_labels)
                names(tendered)[1] <<- input$QM_choice7
                Qmetric <<- gsub("_"," ",names(tendered)[1])
                
                
                p <- ggplot(data = tendered, aes_string(x = "Date_time" , y = names(tendered)[1] ))+
                        geom_point(aes_string(colour = "LC"), size = 4)+
                        geom_line()+labs(x="Date and time (EST)")+
                        scale_color_brewer(type= "qual" , palette = 2)+
                        theme(panel.background=element_rect(fill = "white",colour = "black"),
                              panel.border=element_rect(colour = "black", fill = NA))
                
                
                ggplotly(p)
                
        },message = "Preparing the timeseries chart") 
        
}, ignoreNULL = FALSE) # end of the eventReactive



output$time_series_LC <- renderPlotly(expr = { 
        
        reTab7()        
        
        
})  # renderplotly closure


reTab7_2 <- eventReactive(input$action11,{
        
        withProgress(expr = {
                
                
                QM_selected <- as.character(input$QM_choice7_2)
                selected_instrument <- as.character(input$Instrument_choice7_2)
                
                LC_labels <- "LC_labels"
                
                QM_report7_2 <- QM_report %>% filter(instrument == selected_instrument)

                
                p72 <- ggplot(data = QM_report7_2, aes_string(y = QM_selected,x = LC_labels , 
                                                             fill= LC_labels))+
                        geom_boxplot()+labs(fill="", x="  ")+
                        scale_fill_brewer(type= "qual" , palette = 2) +
                        theme(panel.background=element_rect(fill = "white",colour = "black"),
                              panel.border=element_rect(colour = "black", fill = NA),
                              axis.text.x = element_text(angle = 0),
                              strip.background = element_rect(fill = "darkblue"),
                              strip.text.x = element_text(colour = "white"))
                                    
                
                
                ggplotly(p72)
                
        }, message = "Preparing the instrument vs LC analytics")        
        
},ignoreNULL = FALSE) 





output$LC_MS_QM <- renderPlotly(expr = {
        
        
        reTab7_2()              
        
})


reTab7_3 <- eventReactive(input$action12,{
        
        withProgress(expr = {
                
                
                QM_selected <- as.character(input$QM_choice7_3)
               
                
                LC_labels <- "LC_labels"
                
                
                p73 <- ggplot(data = QM_report, aes_string(y = QM_selected,x = LC_labels 
                                                              ))+
                        geom_boxplot(fill= "purple")+labs(fill="", x="  ")+
                        theme(panel.background=element_rect(fill = "white",colour = "black"),
                              panel.border=element_rect(colour = "black", fill = NA),
                              axis.text.x = element_text(angle = 0),
                              strip.background = element_rect(fill = "darkblue"),
                              strip.text.x = element_text(colour = "white"))
                
                
                
                ggplotly(p73)
                
        }, message = "Preparing the LC analytics") 
                     
        
},ignoreNULL = FALSE) 

output$LC_QM <- renderPlotly(expr = {
        
        
        reTab7_3()              
        
})


reTab7_4 <- eventReactive(input$action13,{
        
        
        withProgress(expr = {
                
                
                QM_selected <- as.character(input$QM_choice7_4)
                
                QM_report$LC_labels <- factor(QM_report$LC_labels)
                QM_report$instrument <- factor(QM_report$instrument)
                
                LC_labels <- "LC_labels"
                
                
                p74 <- ggplot(data = QM_report, aes_string(y = QM_selected,x = LC_labels 
                                                           ))+
                        geom_jitter(position = position_jitter(0.2),alpha=0.8,aes(colour=instrument))+
                        labs(fill="", x="  ")+
                        scale_fill_brewer(type= "qual" , palette = 2) +
                        theme(panel.background=element_rect(fill = "white",colour = "black"),
                              panel.border=element_rect(colour = "black", fill = NA),
                              axis.text.x = element_text(angle = 0),
                              strip.background = element_rect(fill = "darkblue"),
                              strip.text.x = element_text(colour = "white"))+
                        coord_cartesian()
                
                
                
                ggplotly(p74)
                
        }, message = "Preparing the LC_MS analytics")        
        
        
        
},ignoreNULL = FALSE)


output$LC_MS_interaction <- renderPlotly(expr = {
        
        
        reTab7_4()              
        
})


reTab7_5 <- eventReactive(input$action14,{
        
        
        withProgress(expr = {
                
                # Re-organize QM_report to use in this tab  
                
                QM_report7_5 <<- QM_report
                
                w <- which(names(QM_report7_5) == input$centric7)  
                
                names(QM_report7_5)[w] <- "focus"  
                
                
                LC_summary <- QM_report7_5 %>% group_by(LC_labels, focus) %>% summarise(LC_utilization = n())
                
                p75 <- ggplot(LC_summary,aes(x = LC_labels, y = LC_utilization,fill = focus)) + 
                        geom_bar(position = "stack",stat = "identity")+
                        labs(fill="", x="LC choice", y="Number of utilization")+
                        theme(axis.text.x=element_text(angle = 45),
                              panel.background = element_rect(fill = "white") 
                        )+
                        coord_flip()
                
                ggplotly(p75)
                
                
                
        }, message = "Preparing the LC_MS_user analytics")        
        
        
        
},ignoreNULL = FALSE)


output$LC_MS_User <- renderPlotly(expr = {
        
        
        reTab7_5()              
        
})




observeEvent(input$range.cumulative.downtime,{
        
        start.date <- date(format(input$range.cumulative.downtime)[1])
        end.date <- date(format(input$range.cumulative.downtime)[2])
        
        temp.active.cumulative <<- active.archive %>% filter(date >= start.date & date <= end.date )
        
        temp.active.cumulative.summary <- temp.active.cumulative %>% group_by(instrument,status) %>%
                summarise(cumulative.Downtime_Days = sum(time.difference_days)) %>% filter(status == "DOWNTIME")
        
        temp.active.cumulative.summary$instrument <- factor(temp.active.cumulative.summary$instrument)
        
        temp.active.cumulative.summary$instrument <- reorder(temp.active.cumulative.summary$instrument,
                                                             temp.active.cumulative.summary$cumulative.Downtime_Days)
        
        output$cumulative.downtime <- renderPlotly({ 
                
                g <-ggplot(data = temp.active.cumulative.summary,aes(x = instrument, 
                                                         y = cumulative.Downtime_Days))+
                        ggtitle(label = paste0(" Mass Spectrometer cumulative downtime report between: ", as.character(start.date),
                                               " and ",as.character(end.date) ))+
                        ylab(label = "MS Downtime (Days)")+
                        xlab(label = "Mass Spectrometer")+
                        geom_bar(stat = "identity", fill = "navy")+
                        theme(panel.background=element_rect(fill = "white",colour = "black"),
                              panel.border=element_rect(colour = "black", fill = NA),
                              plot.margin = margin(10,3,3,10, "pt"),
                              axis.title = element_text(size = 14, face = "bold"),
                              plot.title = element_text(size = 14, face = "bold",hjust = 0.5),
                              axis.text.x = element_text(size = 12,face = "bold"),
                              axis.text.y = element_text(size = 10,face = "bold"))+
                        coord_flip()
                
                ggplotly(g)
                
                })
        
        
})


output$cumulative.filesize <- renderPlotly({
        
        isolate({
          
          instrument.list <- unique(active.archive$instrument)        
          
          temp.size.cumulative.summary <- NULL
          
          for(i in seq_along(instrument.list)){
                  
                  temp <- data.frame(active.archive %>% filter(instrument == instrument.list[i] & date > as.Date("2015-09-13")) %>%
                          arrange(date) %>% group_by(date) %>% summarise(daily_size = sum(size/(2^20))))
                  
                  temp$instrument = instrument.list[i]
                  
                  cumulative_filesize <- NULL
                  
                  for(j in seq_along(temp$daily_size)){
                          
                          cumulative_filesize[j]<- sum(temp$daily_size[1:j])
                  }
                  
                  temp$cumulative_filesize_MB <- cumulative_filesize
                  
                  temp.size.cumulative.summary <- rbind(temp.size.cumulative.summary,temp)                  
          }           
   })
                  
                  
                  
                  
     g<- ggplot(data = temp.size.cumulative.summary, aes(x = date, y = cumulative_filesize_MB,
                                                     color = instrument))+
             geom_line(size =1.5)+
             theme(panel.background=element_rect(fill = "white",colour = "black"),
                   panel.border=element_rect(colour = "black", fill = NA),
                   plot.margin = margin(10,3,3,10, "pt"),
                   axis.title = element_text(size = 14, face = "bold"),
                   plot.title = element_text(size = 14, face = "bold",hjust = 0.5),
                   axis.text.x = element_text(size = 12,face = "bold"),
                   axis.text.y = element_text(size = 10,face = "bold"))+
             ggtitle(label = " Mass Spectrometer active archive total file size ")+
             ylab(label = "Active archive total file size (MB)")+
             xlab(label = "Time")
                  
          
      
        ggplotly(g)
      
        

        
        
})

observeEvent(input$Instrument_choice.downtime.QC,{
        instrument.down <- input$Instrument_choice.downtime.QC
        
        
        
        temp.active.update <- active.archive %>% filter(instrument == instrument.down )
        new.date.range <- temp.active.update$date
        
        if(max(new.date.range) <= Sys.Date() & (max(new.date.range)- min(new.date.range) >= 30)){
                updateDateRangeInput(session = session,inputId = "range.downtime.QC",label = NULL,
                                     min = min(new.date.range),
                                     max = max(new.date.range),start = (max(new.date.range)-30),
                                     end = max(new.date.range))
                
        }else if(max(new.date.range)- min(new.date.range) <= 30){
                updateDateRangeInput(session = session,inputId = "range.downtime.QC",label = NULL,
                                     min = min(new.date.range),
                                     max = max(new.date.range),start = max(new.date.range),
                                     end = min(new.date.range))
                
        }else{
                updateDateRangeInput(session = session,inputId = "range.downtime.QC",label = NULL,
                                     min = min(new.date.range),
                                     max = max(new.date.range),start = (Sys.Date()-30),
                                     end = Sys.Date())
        }
        
       
},ignoreNULL = FALSE)


observeEvent(c(input$Instrument_choice.downtime.QC,input$range.downtime.QC,
               input$downtime.QM),{ 
        
        
        
        isolate({
                instrument.down <- input$Instrument_choice.downtime.QC
                
                if(instrument.down == "Hubble"){
                        instrument.down.QC <- "Hubble1"
                }else{
                        instrument.down.QC = instrument.down
                }
                
                start.date <- date(format(input$range.downtime.QC)[1])
                end.date <- date(format(input$range.downtime.QC)[2])
                QM <<- as.character(input$downtime.QM)
                
                temp.active <<- active.archive %>% filter(date >= start.date & date <= end.date & instrument == instrument.down )
                
                timeline <<- seq.Date(to = as.Date(max(temp.active$ctime)), from = as.Date(min(temp.active$ctime)),
                                      length.out = 20)
                
                rect.x.min <<- min( as.numeric(temp.active$ctime))
                rect.x.max <<- max(as.numeric(temp.active$ctime))
                
                QM_report_temp <<- QM_report %>% filter(date >= start.date & date <= end.date & instrument == instrument.down.QC )
                
                
        })
        
        
        w <- which(temp.active$status == "DOWNTIME")
        down.rects.max.points <- as.numeric(temp.active$ctime[w])
        down.rects.min.points <- as.numeric(temp.active$ctime[w+1])
        
        QMindex <- which(names(QM_report_temp) == QM)
        
        y.seq <- seq(from = min(QM_report[,QMindex],na.rm = T), to = max(QM_report[,QMindex],na.rm = T), 
                     length.out = length(temp.active$ctime) )
        
        plotter <- function(){
        par(mar=c(8,10,5,1))
        plot(x = temp.active$ctime,y = y.seq , type = "n",
             axes=F, xaxt="n", ylab = paste0(QM,"\n\n"), xlab = "",
             main = paste("Mass Spectrometer Downtime v.s JurkatQC Report for ",
                          instrument.down," between ",
                          as.character(start.date), 
                          " and ",as.character(end.date),sep = ""))
        rect(rect.x.min , par("usr")[3],rect.x.max, par("usr")[4], col = 
                     "lightgreen")
        if(length(down.rects.max.points) > 0){
                
                for(i in seq_along(down.rects.max.points)){
                        
                        rect(down.rects.min.points[i], par("usr")[3],
                             down.rects.max.points[i], par("usr")[4], 
                             col = "navy",border = NA,density = -100)
                }
                
        }
        lines(x = QM_report_temp$time, y = QM_report_temp[,QMindex], lty =2, cex = 2, col ="red")
        points(x = QM_report_temp$time, y = QM_report_temp[,QMindex], col = "red",
               pch = 19, cex = 1.3)
        
        axis.POSIXct(side = 1,at=timeline,x= 1:20,format='%Y-%m-%d',labels=T,las=2)
        axis(side = 2, at = round(seq(from=min(y.seq), to =max(y.seq), length.out = 5),0), las =2)
        legend("topright",pch = 15, cex = 1, col = c("navy","lightgreen"), legend = c("Downtime","Operational"))
        legend("topleft",pch = 19, cex = 1.3, col = "red", legend = "JurkatQC")
        }
        
        output$downtime.QC <- renderPlot({
                
                print(plotter())
                
        })
        
        
},ignoreNULL = FALSE)



})