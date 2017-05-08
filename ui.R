################################################################################
# Author: Ozan Aygun
# Date: 01/27/2017 
#
# Purpose: this is the ui definition code for the Shiny app "JurkatQC Scraper Ver 0.3"
# 
# The ui interface is established by shinydashboard package in the form of
# a dashbard page. 
################################################################################


library(googleVis)
library(shiny)
library(shinydashboard)


shinyUI( dashboardPage( 
        
        dashboardHeader(title = "JurkatQC Scraper Version 0.1.0", titleWidth = 350),
       
               
       
        dashboardSidebar(
            sidebarMenu(id="tabitems",  
                h5(icon("power-off"),"Powered by:"),
                tags$img(src='BroadProteomicsLogo.png', height = 90, width =220 ),        
                hr(color = "blue"),
                menuItem("Welcome!",tabName = "welcome",icon = icon("thumbs-o-up"),badgeLabel = "start here",badgeColor = "blue"),
                menuItem("Daily QC summary", tabName = "dailysummary", icon = icon("bar-chart")),
                menuItem("Time-series monitoring", tabName = "timeseries", icon = icon("line-chart")),
                menuItem("Current MS Conditions", tabName = "current", icon = icon("dashboard")),
                menuItem("MS Downtime", tabName = "downtime", icon = icon("clock-o"), badgeLabel = "new", badgeColor = "green"),
                menuItem("The latest QC",tabName = "thelatest", icon = icon("table")),
                menuItem("Facts and Figures",tabName = "facts", icon = icon("users")),
                menuItem("Instrument vs user analytics", tabName = "instrument_centric", icon = icon("cogs")),
                menuItem("LC vs MS analytics",tabName = "LC_centric",icon = icon("fire"), badgeLabel = "new", badgeColor = "green"),
                hr(),
                menuItem("Watch Tutorial", icon = icon("film"), tabName = "tutorial",badgeLabel = "new", badgeColor = "green"),
                menuItem("Version Evolution",icon = icon("code-fork"), tabName = "evolution"),
                menuItem("Get the Sourcecode", icon = icon("github"), tabName = "sourcecode"),
                menuItem("Support and bug reports",icon=icon("medkit"),tabName = "support"),
            
                hr()
                         
                )# End of sidebarMenu structure 
        ), #End of dashboardSidebar
        
        dashboardBody(
                tags$head(
                        tags$script(
                                HTML("
                                     window.onload = function() {
                                        resize();
                                     }
                                     window.onresize = function() {
                                     resize();
                                     }
                                     Shiny.addCustomMessageHandler ('triggerResize',function (val) {
                                     window.dispatchEvent(new Event('resize'));
                                     });
                                     function resize(){
                                     var h = window.innerHeight - $('.navbar').height() - 150; // Get dashboardBody height
                                     $('#box').height(h); 
                                     }"
                                        )
                                )
                                ),
                tabItems(
                
          
                  #Welcome-tab content  
                  tabItem(tabName = "welcome",class = "active",
                   fluidRow( 
                    infoBoxOutput("latest_update",width = 4),
                    infoBoxOutput("QC_size",width = 4),
                    infoBoxOutput("user_size",width = 4)
                   ),
                  
                   
                   box(title = "Welcome to JurkatQC Scraper!",
                   width = 12,solidHeader = TRUE,status ="primary",background = "navy",

                   h4("Use data analytics to monitor mass spectrometer performance"),
                   tags$hr(),
                  
                   h5(icon("thumbs-o-up"),"Mass Spectrometry users inject
                      1 ug tryptic Jurkat cell peptide mixture to instruments and run a 110 min 
                      gradient with a standardized LC-MS/MS run method.",br(),br(),
                      icon("thumbs-o-up"),"This provides an objective quality control
                      that helps to monitor instrument performance by using powerful
                      Quality Metrics provided by Spectrum Mill.",br(),br(),
                      icon("thumbs-o-up"),"JurkatQC Scraper actively monitors 
                      Broad Proteomics servers to detect new Jurkat Quality Metrics,
                      provided that the raw files were searched in Spectrum Mill.",br(),br(),
                      icon("thumbs-o-up"),"JurkatQC Scraper archives Quality Metrics data,
                      extracts useful LC labels, time and user attributes, and present helpful data analytics.",br(),br(),
                      icon("thumbs-o-up"),"JurkatQC Scraper also provides a user interface to obtain, store and present
                      mass spectrometry user comments and maintanence records.",br(),br(),
                      icon("thumbs-o-up"),"Effective from Version 0.1.0, JurkatQC Scraper also monitors
                      instrument active archive records to estimate Mass Spectrometer downtime. It also integrates
                      the instruments' operational status with Jurkat QC quality metrics."),
                  
                   
                   
                   fluidRow(
                   
                   column(1,{}),                      
                        
                   actionLink("link_to_downtime",
                              label = uiOutput("downtime_box"),width = 3),
                      
                   infoBox(title="Sourcecode",color = "purple",width = 4,
                            icon = icon("github"), value = "Find us on GitHub!",subtitle = "Access the sourcecode",
                            href = "https://github.com/ayguno/JurkatQCScraper"),
                   
                   actionLink("link_to_tutorial",
                              label = uiOutput("tutorial_box"),width = 3),
                   
                   column(1,{}) 
                   
                   )
                   
                   
                   
                   
                   ),

                   
                   box(title = "Choose an app to explore the latest Jurkat QC data
                       generated by the mass spectrometers in the Broad Proteomics Platform",
                   width = 12,solidHeader = TRUE, status = "danger",
                   background = "navy",
                   
                   fluidRow(
                   
                   # Generating dynamic InfoBoxes as action links
                   actionLink("link_to_QC_summary",
                              label = uiOutput("QC_summary_box"),width =4),
                   actionLink("link_to_timeseries",
                              label = uiOutput("timeseries_box"),width =4),
                   actionLink("link_to_current",
                              label = uiOutput("current_box"),width =4)
                   
                   ),
                   
                   fluidRow(
                           
                           # Generating dynamic InfoBoxes as action links
                           actionLink("link_to_thelatest",
                                      label = uiOutput("thelatest_box"),width =4),
                           actionLink("link_to_facts",
                                      label = uiOutput("facts_box"),width =4),
                           actionLink("link_to_instrument_centric",
                                      label = uiOutput("instrument_centric_box"),width =4)
                           
                   ),
                   
                   fluidRow(
                           column(2,{}),
                           actionLink("link_to_thelatest2",
                                      label = uiOutput("comment_box"),width =3),
                           actionLink("link_to_support",
                                      label = uiOutput("support_box"),width =2)
                   )
                   

                   
                   )
                   
                          
                   
                ),  
                    
                    
                    
                    
                    
                # First tab content
                tabItem(tabName = "dailysummary",class= "active",

                    box(width= 12,title = "Daily summary of Mass Spectrometer Quality Controls",
                        status = "primary",solidHeader = TRUE,
                    htmlOutput("motionplot")
                    ),
     
                                box(title= "Tips for using this APP" ,
                                    width = 12,background = "navy",solidHeader = TRUE,
                                    status = "success",
                                      
                                column(6,         
                                          h5(icon("calendar")," The data describes the daily summary of
                                          the JurkatQC data obtained real-time.
                                          Each parameter is described as the 'Median'
                                          of the daily records observed for each instrument.
                                          ", br(),br(),
                                          icon("bar-chart")," The default view is a bar chart, which displays
                                          the median value of the selected quality metrics for a given date.",br(),br(),
                                          icon("sliders")," Use the sliding bar on the x-axis to change the dates.",br(),br(),
                                          icon("gears")," Click Y-axis label to switch between different quality metrics.")#, br(),br(),
                                        ),
                                       
                                       column(6,
                                          h5(icon("play")," For bar-chart only: click to play button on the x-axis to monitor changes over time in motion!",br(),br(),
                                          icon("line-chart")," Use the chart switch on the top right to change into a line-chart summary.", br(), br(),
                                          icon("thumbs-o-up"), " You can log transform the data by clicking top of the Y-axis.",br(),br(),
                                          icon("check-square-o"), " You can choose to display the data for all or some of the 
                                          instruments from the menu on the left side of the chart."))  
                                
                                )     
                       
                ),
                
                # Second tab content
                tabItem(tabName = "timeseries",
                        
                        box(id= "box",title = "Follow all Jurkat Quality Metrics recorded in the history."
                            ,width = 12,status = "primary",solidHeader = TRUE,height = 500,
                        plotlyOutput("timeplot", width = "100%", height = 400)
                        ),
                        
                        fluidRow(
                                
                                box(title= "Select a Quality Meric to Monitor",width=3, 
                                    status = "primary",solidHeader = TRUE,
                                    background = "navy",
                                       
                                       selectInput(inputId = "QM2",
                                                   label = "Select a quality metric:",
                                                   choices = names(QM_report)[2:15]
                                       ),
                                       h6("Re-plot the data for the selected parameter:"),
                                       actionButton(inputId = "action2", label = "Plot data")
                                ), 
                                
                                box(title = "Tips for using this APP",width=6, status = "success",
                                    solidHeader = TRUE,background = "navy",
                                   
                                    column(6,
                                       h5(icon("hand-o-left"),"Select a Quality metric to follow.",br(),br(),
                                          icon("mouse-pointer"),"Scroll over individual data points to display information.",br(),br(),
                                          icon("mouse-pointer"),"Click the names on the legend to display/hide specific instruments.",br(),br(),
                                          icon("hand-o-right"),"Note that multiple QC might be recorded for each day.")
                                    ),
                                    column(6,
                                       h5( icon("zoom-in",lib = "glyphicon"),"Crop over the plot to zoom into a particular range.",br(),br(),
                                          icon("zoom-out",lib = "glyphicon"),"Double click to plot to zoom out.",br(),br(),
                                          icon("calendar"),"You can also use the calendar on the right to define a custom date range.") 
                                    )
                                        
                                ),
                                box(title = "Select a custom date range",width=3, status = "info",
                                    solidHeader = TRUE,background = "navy",
                                    h5(icon("calendar"),"Use this calendar to choose a new date range."),
                                    dateRangeInput("range",label = NULL, min = min(QM_report$date),
                                                   max = max(QM_report$date),start = min(QM_report$date),
                                                   end = max(QM_report$date),width = 250),
                                    h6("Re-plot the data for the selected range:"),
                                    actionButton(inputId = "action3", label = "Re-plot data")
                                    
                                    
                                    
                                )
                                
                        )
                        
                ),
                
                # Third tab content
                tabItem(tabName = "current",
                        
                        fluidRow(   
                                
                                box(title = "Check the distinct peptide gauges for each instrument relative to its historical performance",
                                    width = 10, status = "success", solidHeader = TRUE,
                                            
                                            htmlOutput("Gauge_plot")
                                    
                                ),
                                
                                
                                box(title = "Tips for using this APP",width=5, status = "success",
                                    solidHeader = TRUE,background = "navy",
                                
                                column(6,   
                                  h5(icon("tachometer"),"Each gauge reflects the current distinct peptide counts for a given instrument, relative to its historical performance.",br(),br(),
                                    icon("calendar-check-o"),"Data is obtained from the most recent quality metrics recorded.",br(),br(),
                                    icon("hand-peace-o"), "The green zone reflects the distinct peptide counts that are above the instrument's specific historical median.",br(),br())
                                ),
                                
                                column(6,      
                                  h5(icon("calendar-check-o"),"The time record for the data is given in the table on the right.", br(),br(),
                                    icon("history"),"You can also compare the latest value with the historical median.", icon("hand-o-right"), br(),br(),
                                    icon("sort-alpha-asc"),icon("sort-numeric-asc"),"You can sort the table by clicking to header.",icon("hand-o-right"))
                                )    
                                  ),
                                            
                                      box(title = "Mini-record table",width = 5, status = "info",
                                          solidHeader = TRUE, 
                                          htmlOutput("gaugetable")
                                          )
                        ) 
                        
                        
                ),
                
                # Fourth tab content
                tabItem(tabName = "thelatest",
                       box(title = "Detailed latest quality metrics record for each instrument",
                        
                         width = 12, status = "primary", solidHeader = TRUE,
                          dataTableOutput("latestQC")
                       ),
                 
                      
                       
                       box(title = "Tips for using this APP (Click + on the right to expand)",width=12, status = "success",
                          solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                          background = "navy",
                       column(6,    
                          h5(icon("file-excel-o"),"If you like reading spreadsheets, this is for you.",br(),br(),
                              icon("table"),"Data is obtained from the most recent quality metrics recorded.",br(),br(),
                              icon("list-alt"), "Detailed quality metric parameters are presented along with the date, time and file name attributes.",br(),br(),
                              icon("columns"), "For your convenience the first 5 columns in the table remain frozen
                             as you scroll towards the right.")
                       ),
                       
                       column(6,
                           h5 (icon("sort-alpha-asc"),icon("sort-numeric-asc"),"You can sort the table by clicking to header.",br(),br(),
                              icon("calendar-check-o"),"You can also monitor alternative days by using the calendar below.",br(),br(),
                              icon("commenting-o"),"You can enter your comments about the instruments from the space provided below.
                              Your comments will be archived in the internal database and will be immediately posted along with the
                              date of comment and your initials. Your comments will be visible to all users. Multiple comments per instrument,
                              per date is also supported.")
                       )
                             
                       ),

                   
                  
                      
                    box(title = "Monitor an earlier date",width=3, status = "info",
                        solidHeader = TRUE,background = "navy",
                        collapsible = TRUE,
                        h5(icon("calendar"),"Use this calendar to choose an alternative date."),
                        dateInput("alternativedate",label = NULL,
                                  min = min(QM_report$date),width = 100
                        ),
                        h6("When you choose a new date, JurkatQC Scraper
                                will display the latest QC,
                                as well as instrument comments
                                recorded by that date."),
                        actionButton(inputId = "action4", label = "Monitor another date")
                    ),
                       
                    uiOutput("Reactive_comments"),
                    
                    box(title = "All quality metrics records for all instruments",width=12, status = "primary",
                        solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE, 
                        
                        selectInput("Instrument_choice_allQC",
                                    label= " Choose an Instrument",
                                    choices = unique(QM_report$instrument),
                                    selected = "Franklin",width = 150),
                        dataTableOutput("allQC")
                    )
                                
                ),
                
                # 5th tab content
                tabItem(tabName = "facts",
                       
                        box(title = "Select a focus and let data to speak!",
                            width = 4, solidHeader = TRUE, 
                            status = "danger", background = "navy",
                            h4("Select a focus for the entire page:"),
                            selectInput("centric", label = NULL, 
                                        choices = c("MS_user","weekdays","instrument"),
                                        selected = "instrument"),
                            actionButton("action5",label = "Switch the gears!"),br(),
                            h5("Based on your choice, JurkatQCscraper will present the
                               entire data for you.",br()),
                            hr(),
                            h5( "You can learn:", br(),br(),
                               "-Which instrument has been checked more often?",br(),
                               "-What are the prefered days for QCs?", br(),
                               "-How do quality metrics differ amongst instruments,
                               users or weekdays?", br(),
                               "-What are the user tendencies in terms of
                               instruments and weekdays?")
                            ),
                       
                        box(title = "Distribution of the total number of Quality Controls performed over time",
                            width = 8, solidHeader = TRUE, status = "primary",
                            collapsible = TRUE,background = "navy",
                           htmlOutput("nQM_centric"),
                         column(6,  
                           h5(icon("sliders")," Use the sliding bar on the x-axis to change the dates.",br(),br(),
                              "Click ", icon("play"), " to monitor changes over time in motion!"
                              )
                              ),
                         column(6,
                            h5(icon("line-chart")," Use the chart switch on the top right to change into a line-chart summary.", br(), br(),
                               icon("calendar-plus-o"),"The chart displays the total amount of QCs performed up to a selected date.")
                               )
                        ),
                        
                        box(title = "Distribution of the Quality Metrics",
                            width = 12, solidHeader = TRUE, status = "primary",
                            collapsible = TRUE,background = "navy",id="box",height = "100%",
                            plotlyOutput("QM_centric", width = "100%", height = 250),
                            hr(),
                            column(4,
                            selectInput("QM_choice5",
                                        label = "Choose a Quality Metric to compare",
                                        choices = names(QM_report)[2:15],
                                        selected = "DistinctPeps_CS_Total_number_of",width = 300),
                            actionButton("action6", label = "Compare a different metric")
                               ),
                            
                            column(8,
                                   h5(icon("cogs"),"This analytics tool is designed to provide information
                                    about Quality Metrics distribution of the selected analytics focus.",br(),br(),
                                      icon("thumbs-o-up"),"You can analyze potential instrument-to-instrument, day-to-day, user-to-user variability of 
                                      a given Quality Metric.", br(),br(),
                                      icon("thumbs-o-up"),"Select a Quality Metric to compare.")
                                   
                                   
                                   )
                          ),
                        # Stacked bar charts for the interaction of the selected
                        # Factor with the other factors
                        box(title = "Mass spectrometry user tendencies Part 1",
                            width = 6,  status = "primary", id="box",height = "100%",
                            collapsible = TRUE, solidHeader = TRUE, background = "navy",
                            
                            plotlyOutput("Interaction1",width = "100%", height = 275)
                            
                          ),
                        box(title = "Mass spectrometry user tendencies Part 2",
                            width = 6,  status = "primary",id="box",height = "100%",
                            collapsible = TRUE, solidHeader = TRUE, background = "navy",
                            plotlyOutput("Interaction2", width = "100%",height = 275)
                            )
                        
                ),
                
                #6th tab content
                tabItem(tabName = "instrument_centric",
                        box(title = "Time-series analysis of Mass Spectrometer Quality Metrics along with MS Users",
                            width = 12, status = "primary",id="box",height = "100%",
                            collapsible = TRUE, solidHeader = TRUE, background = "navy",
                            
                            plotlyOutput("time_series_USER", width = "100%", height = 300),
                            
                            hr(),
                            column(4,  
                                   selectInput("Instrument_choice6_2",
                                               label= " Choose an Instrument",
                                               choices = unique(QM_report$instrument),
                                               selected = "Franklin",width = 150),
                                   selectInput("QM_choice6_2",
                                               label = "Choose a Quality Metric to compare",
                                               choices = names(QM_report)[2:15],
                                               selected = "DistinctPeps_CS_Total_number_of",width = 400),
                                   actionButton("action6_2", label = "Display report!")
                            ),
                            column(5,
                                   h5(icon("line-chart"),"This analytics tool is designed to provide time-series information
                                      about Quality Metrics of a particular instrument and its User association.",br(),br(),
                                      icon("thumbs-o-up"),"Each data point represents a single JurkatQC run, scroll over data points to see the information.",br(),br(),
                                      icon("thumbs-o-up"),"You can explore potential user association of the instrument performance
                                      for a given Quality Metric.", br(),br(),
                                      icon("thumbs-o-up"),"Select an instrument to monitor, then a Quality Metric to compare.",br(),br(),
                                      icon("mouse-pointer"),"Click the names on the legend to display/hide associated with specific users.",br(),br(),
                                      icon("calendar"),"You can also select custom date ranges from the calendar provided on the right for your convenience.")
                            ),
                            column(3,
                                   h5(icon("calendar"),"Use this calendar to choose a new date range."),
                                   dateRangeInput("range6_2",label = NULL, min = min(QM_report$date),
                                                  max = max(QM_report$date),start = min(QM_report$date),
                                                  end = max(QM_report$date),width = 350),
                                   h6("Re-plot the data for the selected range:"),
                                   actionButton(inputId = "action6_2_2", label = "Re-plot data")
                            )
                            
                        ),
                        box(title = "Quality metrics conditioned by Instrument, Weekdays and Mass Spectrometry User",
                            width = 12, status = "primary",id="box",height = "100%",
                            collapsible = TRUE, solidHeader = TRUE, background = "navy",
                            plotlyOutput("Interaction3", width = "100%", height = 300),
                            hr(),
                          column(6,  
                            selectInput("Instrument_choice6",
                                        label= " Choose an Instrument",
                                        choices = unique(QM_report$instrument),
                                        selected = "Franklin",width = 150),
                            selectInput("QM_choice6",
                                        label = "Choose a Quality Metric to compare",
                                        choices = names(QM_report)[2:15],
                                        selected = "DistinctPeps_CS_Total_number_of",width = 350),
                            actionButton("action7", label = "Display report!")
                          ),
                          column(6,
                                 
                                 h4(icon("cogs"),"This analytics tool is designed to provide information
                                    about Quality Metrics of a particular instrument.",br(),br(),
                                    icon("thumbs-o-up"),"You can analyze for day-to-day and user-to-user variability of the instruments
                                    for a given Quality Metric.", br(),br(),
                                    icon("thumbs-o-up"),"Select an instrument to monitor, then a Quality Metric to compare.")
                                 
                                 
                                 
                                 
                                 )
                          
                                    
                            )
                        
                                
                                   
                ),
                
                #7th tab content
                tabItem(tabName = "LC_centric",
                        box(title = "Time-series analysis of Mass Spectrometer Quality Metrics along with LC choice",
                            width = 12, status = "primary",id="box",height = "100%",
                            collapsible = TRUE, solidHeader = TRUE, background = "navy",
                            
                            plotlyOutput("time_series_LC", width = "100%", height = 300),
                            
                            hr(),
                            column(4,  
                                   selectInput("Instrument_choice7",
                                               label= " Choose an Instrument",
                                               choices = unique(QM_report$instrument),
                                               selected = "Franklin",width = 150),
                                   selectInput("QM_choice7",
                                               label = "Choose a Quality Metric to compare",
                                               choices = names(QM_report)[2:15],
                                               selected = "DistinctPeps_CS_Total_number_of",width = 370),
                                   actionButton("action9", label = "Display report!")
                            ),
                            column(5,
                                   h5(icon("line-chart"),"This analytics tool is designed to provide time-series information
                                    about Quality Metrics of a particular instrument and its LC-association.",br(),br(),
                                      icon("thumbs-o-up"),"Each data point represents a single JurkatQC run, scroll over data points to see the information.",br(),br(),
                                      icon("thumbs-o-up"),"You can explore potential LC-association of the instrument performance
                                    for a given Quality Metric.", br(),br(),
                                      icon("thumbs-o-up"),"Select an instrument to monitor, then a Quality Metric to compare.",br(),br(),
                                      icon("mouse-pointer"),"Click the names on the legend to display/hide associated with specific LCs.",br(),br(),
                                      icon("calendar"),"You can also select custom date ranges from the calendar provided on the right for your convenience.")
                                   ),
                            column(3,
                                   h5(icon("calendar"),"Use this calendar to choose a new date range."),
                                   dateRangeInput("range7",label = NULL, min = min(QM_report$date),
                                                  max = max(QM_report$date),start = min(QM_report$date),
                                                  end = max(QM_report$date),width = 300),
                                   h6("Re-plot the data for the selected range:"),
                                   actionButton(inputId = "action10", label = "Re-plot data")
                                   )
                              
                        ),
                        
                        box(title = "Compare LC performance when linked to different Mass Spectrometers",
                            width = 12, status = "primary",id="box",height = "100%",
                            collapsible = TRUE, solidHeader = TRUE, background = "navy",
                            
                            plotlyOutput("LC_MS_interaction", width = "100%", height = 300),
                            
                            hr(),
                            column(3, 
                            selectInput("QM_choice7_4",
                                        label = "Choose a Quality Metric to compare",
                                        choices = names(QM_report)[2:15],
                                        selected = "DistinctPeps_CS_Total_number_of",width = 370),
                            actionButton("action13", label = "Re-plot data")
                            ),
                            column(9,
                                   h5(icon("dot-circle-o"),"This tool is designed to provide the distribution of Quality Metrics data
                                      associated with a particular LC.",br(),br(),
                                      icon("thumbs-o-up"),"Each data point represents a single JurkatQC run, scroll over data points to see the information.",br(),br(),
                                      icon("thumbs-o-up"),"You can explore potential variability of LC-performance
                                      for a given Quality Metric and Mass Spectrometers (colored differentially).", br(),br(),
                                      icon("thumbs-o-up"),"Select a Quality Metric to compare LCs.",br(),br(),
                                      icon("mouse-pointer"),"Click the names on the legend to display/hide data associated with specific Mass Spectrometers.")    
                            )
                        
                        ),
                        
                        box(title = "Explore LC performance in a given Mass Spectrometer",
                            width = 6, status = "primary",id="box",height = "100%",
                            collapsible = TRUE, solidHeader = TRUE, background = "navy",
                            
                            plotlyOutput("LC_MS_QM", width = "100%", height = 300),
                            
                            hr(),
                            
                            column(4,
                                   selectInput("Instrument_choice7_2",
                                               label= " Choose an Instrument",
                                               choices = unique(QM_report$instrument),
                                               selected = "Franklin",width = 150)
                                   ),
                            column(7,
                                   selectInput("QM_choice7_2",
                                               label = "Choose a Quality Metric to compare",
                                               choices = names(QM_report)[2:15],
                                               selected = "DistinctPeps_CS_Total_number_of",width = 450)
                                   ),
                            
                            column(4,     
                                   actionButton("action11", label = "Display analysis!")
                            )
                        ),
                        
                        box(title = "Explore the overall LC performance",
                            width = 6, status = "primary",id="box",height = "100%",
                            collapsible = TRUE, solidHeader = TRUE, background = "navy",
                            
                            plotlyOutput("LC_QM", width = "100%", height = 300),
                            
                            hr(),
                            
                            selectInput("QM_choice7_3",
                                        label = "Choose a Quality Metric to compare",
                                        choices = names(QM_report)[2:15],
                                        selected = "DistinctPeps_CS_Total_number_of",width = 350),
                            actionButton("action12", label = "Display analysis!") 
                            
                            ),
                        
                        column(2,{}),
                        
                        box(title = "LC/Mass spectrometer/User tendencies",
                            width = 8,  status = "primary", id="box",height = "100%",
                            collapsible = TRUE, solidHeader = TRUE, background = "navy",
                            
                            plotlyOutput("LC_MS_User",width = "100%", height = 275),
                            hr(),
                           
                          column(3,  
                            h5("Select a focus:"),
                            selectInput("centric7", label = NULL, 
                                        choices = c("MS_user","weekdays","instrument"),
                                        selected = "instrument", width = 150),
                            actionButton("action14",label = "Switch the gears!")
                          ),
                          column(9,
                                 h5(icon("bar-chart"),"This tool is designed to provide data
                                    about the utilization of particular LC.",br(),br(),
                                    icon("thumbs-o-up"),"You can focus on either Mass Spectrometers (instrument),
                                    weekdays or MS users to display LC utilization information.")
                                 
                                 )
                            
                        )
                        
                        
                        
                ),
                
                
                
                
                #Support tab content
                tabItem(tabName = "support",
                        
                        
                        br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                        column(3,{}),
                        box(title = "Contact for support",status = "danger", width = 6,background = "navy",
                            h4(icon("ambulance"),"Do you need support?", br(),br(),
                               icon("bug")," Have you found a bug?", br(), br(),
                               icon("envelope")," Contact me at: ozan[at]broadinstitute.org")
                            )
                        
                
                ),
                
                tabItem(tabName = "evolution",
                        
                        
                        br(),
                        
                        box(id= "box",title = "Evolution of the JurkatQC Scraper",
                            status = "primary", width = 12,background = "navy",height = "100%",
                            h3(icon("code-fork"),"Version 0.1.0"), hr(),
                            h4(icon("thumbs-o-up"),"Incorporated the new feature for monitoring Mass Spectrometer
                               Downtime and integrating JurkatQC information "),br(),
                            h4(icon("thumbs-o-up"),"Updated the design of the Latest Quality Metrics Table, 
                            also incorporated a new table that allows retrieval of all Quality Metrics records,
                            for all instruments"),br(),
                            h4(icon("thumbs-o-up"),"Fixed a bug in the LC dot plot due to the increasing number of
                            LC instruments"),br(),
                            
                            h3(icon("code-fork"),"Version 0.3"), hr(),
                            h4(icon("thumbs-o-up"),"Calibrated Distinct Peptide Gauges with the historical performance of each instrument"),br(),
                            h4(icon("thumbs-o-up"),"Included a color key to interpret peptide gauges"),br(),
                            h4(icon("thumbs-o-up"),"Included a new app to monitor Instrument-specific QC performance along with users, in a defined time window"),br(),
                            h4(icon("thumbs-o-up"),"Completed integration of the new instrument: Yoda"),br(),
                            h4(icon("thumbs-o-up"),"Included a video tutorial for using JurkatQC Scraper"),br(),
                            
                            h3(icon("code-fork"),"Version 0.2"), hr(), 
                            h4(icon("thumbs-o-up"),"Included a new app to monitor LC-specific analytics"),br(),
                            h4(icon("thumbs-o-up"),"Incorporated Java Script definition for the dynamic resizing of the most apps based on screen size, aimed to improve user performance"),br(),
                            h4(icon("thumbs-o-up"),"Improved the server logic for faster launching of the user interface during data scanning"),br(),
                            h4(icon("thumbs-o-up"),"Fixed a bug that arises during the simultaneous access of multiple users"),br(),
                            h4(icon("thumbs-o-up"),"Started integrating new instrument Yoda and fixed bugs arising from this addition"),br(),
                            
                            h3(icon("code-fork"),"Version 0.1"), hr(), 
                            h4(icon("thumbs-o-up"),"Beta-version for limited use")   
                        )
                
                ),
                
                tabItem(tabName = "sourcecode",
                        
                        
                        br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                        column(3,{}),
                        valueBox(color = "purple",width = 5,
                            icon = icon("github"), value = "Find us on GitHub!",subtitle = "Access the most recent sourcecode",
                            href = "https://github.com/ayguno/JurkatQCScraper"
                        )
                        
                        
                ),
                
                tabItem(tabName = "tutorial",
               
                br(),
               
                
                box(id= "box",title = "Watch the video tutorial to explore JurkatQC Scraper",
                    status = "primary",background = "navy",height = "100%",width = 10,
                tags$video(src="JurkatQCscraper_tutorial.m4v",
                                                type = "video/m4v",width = 800, height = 400,
                                                height = "700px", controls = "controls")
                        
                    ),
                
                box(id= "box",title = "Recommended internet browsers:", 
                    status = "danger",background = "navy",height = "100%",width = 10,
                   icon("firefox"),"Firefox","and", icon("chrome"),"Google chrome",
                   "(some versions of Safari or Internet Explorer might not display the video properly).")
                
                ),
                
                # "downtime" tab
                tabItem(tabName = "downtime",
                        
                        box(title = "Mass Spectrometer downtime and JurkatQC",
                            width = 12, status = "primary",id="box",height = "100%",
                            collapsible = TRUE, solidHeader = TRUE, background = "navy",
                            
                            plotOutput("downtime.QC"),
                            h5(column(4,{}),"Raw File Acquistion Time (Format: Year-Month-Day)"),
                            hr(),
                            column(3,  
                                   selectInput("Instrument_choice.downtime.QC",
                                               label= " Choose an Instrument",
                                               choices = unique(active.archive$instrument),
                                               selected = "Franklin",width = 150),
                                   selectInput(inputId = "downtime.QM", selected = "DistinctPeps_CS_Total_number_of",
                                               label = "Select a quality metric:",
                                               choices = names(QM_report)[2:15],width = 400),
                                   
                                   h5(icon("clock-o"),"Any time difference that is equal to or more than
                                      5h between two raw files is counted as Mass Spectrometer downtime.")
                                   
                            ),
                            column(6,
                                   h5(icon("line-chart"),"This analytics tool is designed to provide time-series information
                                      about the downtime or operational status of a given mass spectrometer along with 
                                      JurkatQC measured in the same time window.",br(),br(),
                                      icon("thumbs-o-up"),"Any time gap that is more than 5 hours between two consequtive
                                      raw files is considered as DOWNTIME.",br(),br(),
                                      icon("thumbs-o-up"),"You can explore the blocks of instrument downtime and overlay with 
                                      quality metrics obtained from JurkatQC.", br(),br(),
                                      icon("thumbs-o-up"),"Select an instrument to monitor operational status.",br(),br(),
                                      icon("thumbs-o-up"),"Select a quality metric to monitor.",br(),br(),
                                      icon("calendar"),"You can also select custom date ranges from the calendar provided on the right for your convenience.")
                            ),
                            column(3,
                                   h5(icon("calendar"),"Use this calendar to choose a new date range."),
                                   dateRangeInput("range.downtime.QC",label = NULL, min = min(active.archive$date),
                                                  max = max(active.archive$date),start = min(active.archive$date),
                                                  end = max(active.archive$date),width = 300),
                                   h6("Focus on the data for the selected range"),
                                   h5(icon("exclamation-triangle"),"Note that the instrument downtime is estimated
                                      by hourly scan of ALL raw files in the proteomics active archive directory.")
                                   
                            )
                            
                            
                        ),# End of downtime.QC box      
                        
                        box(title = "Mass Spectrometer ESTIMATED cumulative downtime report",
                            width = 12, status = "primary",id="box",height = "100%",
                            collapsible = TRUE, solidHeader = TRUE, background = "navy",
                            
                            plotlyOutput("cumulative.downtime"),
                            hr(),
                            
                            h5(icon("calendar"),"Use this calendar to choose a new date range."),
                            dateRangeInput("range.cumulative.downtime",label = NULL, min = min(active.archive$date),
                                           max = max(active.archive$date),start = (Sys.Date()-30),
                                           end = Sys.Date()),
                            h5("Focus on the data for the selected range")
                           
                                
                        ),
                        
                       

                        box(title = "Mass Spectrometer active archive cumulative file size",
                            width = 12, status = "primary",id="box",height = "100%",
                            collapsible = TRUE, solidHeader = TRUE, background = "navy",
                            
                            plotlyOutput("cumulative.filesize")
                            
                            
                        )
                        
                        
                )# End of "downtime" tab
                  
            )# End of tabItems structure      
        )  # End of dashboard body
        
)  # End of dashboard page     
)  # end of UI      

        