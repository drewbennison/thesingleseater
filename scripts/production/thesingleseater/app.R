library(shiny)
library(DT)
#library(data.table)
library(tidyverse)
library(lubridate)
library(plotly)
library(scales)
library(reshape2)
library(shinythemes)

ui <- navbarPage(title="The Single Seater",
                 navbarMenu(title = "Statistics",
                            
                                         #width = 10,
                            tabPanel(title = "Season Statistics",
                                     fluidRow(column(
                                         width = 10,
                                         offset = 0,
                                         tags$h4("Season Statistics"),
                                         radioButtons("trackb", "Track Type:",
                                                      c("All Tracks" = "all",
                                                        "Oval" = "oval",
                                                        "Road and Street" = "road and street")),
                                         
                                         selectInput("selectyear", "Season:",
                                                     c("2022"="2022",
                                                         "2021"= "2021",
                                                       "2020" = "2020",
                                                       "2019" = "2019",
                                                       "2018" = "2018")),
                                         DT::dataTableOutput("seasonTable")
                                     ))),
                            
                            tabPanel(title = "Track Statistics",
                                     fluidRow(column(
                                         width = 10,
                                         offset = 0,
                                         h4("Track Statistics (2018-Present)"),
                                         selectInput("selecttrack", "Track:", 
                                                     choices = NULL, 
                                                     selected = 1),
                                         DT::dataTableOutput("trackTable")
                                         ))),
                 
                             tabPanel(title = "Race Statistics",
                                      fluidRow(column(
                                          width = 10,
                                          offset = 0,
                                          tags$h4("Race Statistics"),
                                          selectInput("selectrace", "Round Number:",
                                                      c("1"= "1",
                                                        "2" = "2",
                                                        "3" = "3",
                                                        "4" = "4",
                                                        "5" = "5",
                                                        "6" = "6",
                                                        "7" = "7",
                                                        "8" = "8",
                                                        "9" = "9",
                                                        "10" = "10",
                                                        "11" = "11",
                                                        "12" = "12",
                                                        "13" = "13",
                                                        "14" = "14",
                                                        "15" = "15",
                                                        "16" = "16",
                                                        "17" = "17")),
                                          
                                          selectInput("selectyear2", "Season:",
                                                      c("2022"="2022",
                                                          "2021"= "2021",
                                                        "2020" = "2020",
                                                        "2019" = "2019",
                                                        "2018" = "2018")),
                                          DT::dataTableOutput("raceTable")
                                      )))),
                 
                 navbarMenu(title = "Elo Ratings",
                            tabPanel(title = "Current Elo Ratings",
                                     fluidRow(column(
                                                    width = 10,
                                                    offset = 0,
                                                    h4("Current Elo Ratings"))),
                                     DT::dataTableOutput("eloTable")),
                            
                            tabPanel(title = "Historical Elo Ratings",
                                     fluidRow(column(
                                                      width = 10,
                                                      offset = 0,
                                                      h4("Historical Elo Ratings"),
                                                      #wellPanel(
                                                          selectInput("selectedDrivers", choices = NULL, label = "Drivers:", selected = NULL,
                                                                      multiple = TRUE)
                                                      #)
                                                      ,
                                                      plotlyOutput("eloGraph")
                                    )))),
                 
                 navbarMenu(title = "Championship Projections",
                            tabPanel(title = "2022 Championship Projections",
                                     fluidRow(column(
                                         width = 6,
                                         offset = 0,
                                         h4("Championship Projections"),
                                         h5(textOutput("date_text")),
                                         selectInput("selectchampdriver", "Select a driver to view their championship projection:", choices = NULL, selected = 1),
                                         DT::dataTableOutput("champTable")),
                                         
                                         column(
                                             width = 6,
                                             offset = 0,
                                             plotOutput("champGraph"))
                                         ))),
                 
                 navbarMenu(title= "More",
                            tabPanel(title = "Links",
                                     fluidRow(column(
                                         width = 10,
                                         offset = 0,
                                         tags$ul(
                                             tags$li(h4(HTML("<a href=\"https://thesingleseater.com/\">thesingleseater.com</a>"))),
                                             tags$li(h4(HTML("<a href=\"https://thesingleseater.com/glossary//\">Statistic glossary</a>")))
                                              )))
                            ))
                 )

server <- function(input, output,session) {
    
    #### load in data sources ####W
    data <- read.csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/master_backup/indycar_results.csv")
    
    elo_ratings <- read.csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/elo_ratings/elo_tracker.csv") %>% 
        mutate(date=ymd(date))
    
    champ_projections <- read.csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/champPredictions/current_champ.csv")
    
    champ_projections_date <- champ_projections %>% pull(current_date) %>% 
        unique()
    
    champ_projections_date <- paste("Last updated: ",champ_projections_date)
    
    champ_projections_season <- max(champ_projections$season)
    
    champ_projections <- champ_projections %>% 
        filter(season!=0) %>% 
        select(driver, totalPoints, chamPos, season) %>%
        group_by(driver, chamPos) %>% 
        add_count() %>% 
        mutate(prob = 100*(round((n/champ_projections_season),3)),
               exp = round(chamPos*.01*prob,2)) %>% 
        select(driver, chamPos, n, prob, exp) %>% 
        distinct() %>%
        rename("Driver" = "driver")
        
    champ_projections_exp <- champ_projections %>% 
        group_by(Driver) %>% summarise(x=sum(exp))
    
    champ_projections <- champ_projections %>% select(-exp)
    
    choices_champ_projections <- champ_projections %>% 
        select(Driver) %>% 
        distinct()
    
    #### dynamically populate selections ####
    updateSelectInput(session=session, inputId="selectchampdriver", choices=choices_champ_projections$Driver)
    
    #dynamically populate choices for track selection
    choices_tracks <- data %>%
        select(track) %>% 
        distinct() %>% 
        arrange(track)
    
    updateSelectInput(session = session, inputId = "selecttrack", choices = choices_tracks$track)
    
    #dynamically populate choices for driver selection
    choices_drivers <- elo_ratings %>% 
        group_by(driver) %>% 
        filter(n()>10) %>% 
        select(driver) %>% 
        distinct() %>% 
        arrange(driver)
    
    updateSelectInput(session = session, inputId = "selectedDrivers", choices=choices_drivers$driver, selected = "Josef Newgarden")
    
    #### season table ####
    output$seasonTable = DT::renderDataTable({
        
        #Calculate averages for adjusted pass efficiency
        avgPE <- data %>% filter(!is.na(passesFor)) %>% 
            select(driver, st, passesFor, passesAgainst) %>% 
            mutate(passEff = passesFor/(passesFor+passesAgainst),
                   #check if they have no passes for or against
                   passEff = ifelse(is.na(passEff),.5,passEff)) %>% 
            group_by(st) %>% 
            summarise(avgPE = mean(passEff)) %>% 
            select(st, avgPE)
        
        #Calculate expected finishing position from every starting position
        afp <- data %>%
            group_by(st) %>% 
            summarise(xFP = mean(fin))
        
        #Left join data with expected finishing position for every driver's results
        data <- data %>%
            left_join(afp, by=c("st" = "st")) %>% 
            mutate(xFPDifference=xFP-fin)
        
        #calculate adjusted passing efficiency
        data <- data %>% 
            mutate(passEff = passesFor/(passesFor+passesAgainst),
                   passEff = ifelse(is.na(passEff), .5, passEff)) %>% 
            left_join(avgPE, by="st") %>% 
            mutate(AdjPassEff = passEff-avgPE)
        
        if(input$trackb=="all") {
            driver_season_stats <- data %>%
                filter(year==input$selectyear) 
        } else if (input$trackb=="oval") {
            driver_season_stats <- data %>%
                filter(year==input$selectyear, type=="oval") 
        } else if (input$trackb=="road and street") {
            driver_season_stats <- data %>%
                filter(year==input$selectyear, type %in% c("road", "street")) 
        }
        
        driver_season_stats <- driver_season_stats %>%
            mutate(favorableStart = ifelse(lapOneChange >= 0, 1,
                                           ifelse(lapOneChange < 0, 0, NA)),
                   RunningCheck = ifelse(status=="running",1,0)) %>% 
            group_by(driver) %>% 
            summarise(StartRetention = 100*mean(favorableStart),
                      StartPM = sum(lapOneChange),
                      Races = n(),
                      PMperStart = StartPM/Races,
                      Pts=sum(pts),
                      xPoints = sum(xPts),
                      AFP = mean(fin),
                      DevFP = sd(fin),
                      ASP = mean(st),
                      DevSP = sd(st),
                      ATP = mean(atp),
                      DevATP = sd(atp),
                      ATP25 = mean(atp25, na.rm = TRUE),
                      DevATP25 = sd(atp25, na.rm = TRUE),
                      PassEff = 100*mean(passEff),
                      AdjPassEff = 100*mean(AdjPassEff),
                      RunPerc = 100*mean(RunningCheck),
                      AFS = mean(fastLapRank, na.rm = TRUE),
                      Top5Perc = 100*(sum(inTopFive)/sum(laps)),
                      AEP = mean(xFPDifference),
                      Wins = sum(ifelse(fin==1,1,0)),
                      Poles = sum(ifelse(st==1,1,0)),
                      TopFives = sum(ifelse(fin<6,1,0)),
                      Podiums = sum(ifelse(fin<4,1,0)),
                      LapsLed = sum(led)) %>% 
            mutate(Difference = Pts-xPoints) %>% 
            select(driver, Races, Pts, xPoints, Difference, Wins, Poles, Podiums, TopFives, LapsLed, AFP, DevFP, ASP, DevSP, ATP, DevATP, ATP25, DevATP25, PassEff, AdjPassEff, RunPerc, Top5Perc, AEP, AFS, StartRetention, StartPM, PMperStart) %>%
            rename("Driver"="driver") %>% 
            #round
            mutate_at(vars(Difference, AFP, DevFP, ASP, DevSP, ATP, DevATP, ATP25, DevATP25, PassEff, AdjPassEff, RunPerc, Top5Perc, AEP, StartRetention, PMperStart, AFS), list(~ round(.,1))) %>% 
            arrange(-Pts)
        
    }, options=list(pageLength=50, scrollX = TRUE))
    
    #### track history table ####
    output$trackTable = DT::renderDataTable({
        
        avgPE <- data %>% filter(!is.na(passesFor)) %>% 
            select(driver, st, passesFor, passesAgainst) %>% 
            mutate(passEff = passesFor/(passesFor+passesAgainst),
                   #check if they have no passes for or against
                   passEff = ifelse(is.na(passEff),.5,passEff)) %>% 
            group_by(st) %>% 
            summarise(avgPE = mean(passEff)) %>% 
            select(st, avgPE)
        
        #Calculate expected finishing position from every starting position
        afp <- data %>%
            group_by(st) %>% 
            summarise(xFP = mean(fin))
        
        #Left join data with expected finishing position for every driver's results
        data <- data %>%
            left_join(afp, by=c("st" = "st")) %>% 
            mutate(xFPDifference=xFP-fin)
        
        #calculate adjusted passing efficiency
        data <- data %>% 
            mutate(passEff = passesFor/(passesFor+passesAgainst),
                   passEff = ifelse(is.na(passEff), .5, passEff)) %>% 
            left_join(avgPE, by="st") %>% 
            mutate(AdjPassEff = passEff-avgPE)
        
        driver_season_stats <- data %>% 
            filter(track==input$selecttrack,
                   year>=2018) %>% 
            mutate(favorableStart = ifelse(lapOneChange >= 0, 1,
                                           ifelse(lapOneChange < 0, 0, NA)),
                   RunningCheck = ifelse(status=="running",1,0)) %>% 
            group_by(driver) %>% 
            summarise(StartRetention = 100*mean(favorableStart),
                      StartPM = sum(lapOneChange),
                      Races = n(),
                      PMperStart = StartPM/Races,
                      Pts=sum(pts),
                      xPoints = round(sum(xPts, na.rm = TRUE)),
                      AFP = mean(fin),
                      DevFP = sd(fin),
                      ASP = mean(st),
                      DevSP = sd(st, na.rm = TRUE),
                      ATP = mean(atp, na.rm = TRUE),
                      DevATP = sd(atp, na.rm = TRUE),
                      ATP25 = mean(atp25, na.rm = TRUE),
                      DevATP25 = sd(atp25, na.rm = TRUE),
                      PassEff = 100*mean(passEff),
                      AdjPassEff = 100*mean(AdjPassEff),
                      RunPerc = 100*mean(RunningCheck),
                      AFS = mean(fastLapRank, na.rm = TRUE),
                      Top5Perc = 100*(sum(inTopFive)/sum(laps)),
                      AEP = mean(xFPDifference),
                      Wins = sum(ifelse(fin==1,1,0)),
                      Poles = sum(ifelse(st==1,1,0)),
                      TopFives = sum(ifelse(fin<6,1,0)),
                      Podiums = sum(ifelse(fin<4,1,0)),
                      LapsLed = sum(led)) %>% 
            mutate(Difference = Pts-xPoints) %>% 
            select(driver, Races, Pts,xPoints, Difference, Wins, Poles, Podiums, TopFives, LapsLed, AFP, DevFP, ASP, DevSP, ATP, DevATP, ATP25, DevATP25, PassEff, AdjPassEff, RunPerc, Top5Perc, AEP, AFS, StartRetention, StartPM, PMperStart) %>%
            rename("Driver"="driver") %>% 
            #round
            mutate_at(vars(Difference, AFP, DevFP, ASP, DevSP, ATP, DevATP, ATP25, DevATP25, PassEff, AdjPassEff, RunPerc, Top5Perc, AEP, StartRetention, PMperStart, AFS), list(~ round(.,1))) %>% 
            arrange(-Pts)
        
    }, options=list(pageLength=50, scrollX = TRUE))
    
    
    #### race stats table ####
    output$raceTable = DT::renderDataTable({
        
        #Calculate averages for adjusted pass efficiency
        avgPE <- data %>% filter(!is.na(passesFor)) %>% 
            select(driver, st, passesFor, passesAgainst) %>% 
            mutate(passEff = passesFor/(passesFor+passesAgainst),
                   #check if they have no passes for or against
                   passEff = ifelse(is.na(passEff),.5,passEff)) %>% 
            group_by(st) %>% 
            summarise(avgPE = mean(passEff)) %>% 
            select(st, avgPE)
        
        #Calculate AFP from every starting position
        afp <- data %>%
            group_by(st) %>% 
            summarise(xFP = mean(fin))
        
        #Left join data with xFP for every driver's results
        data <- data %>%
            left_join(afp, by=c("st" = "st")) %>% 
            mutate(xFPDifference=xFP-fin)
        
        #calculate adjusted passing efficiency using averages from above
        data <- data %>% 
            mutate(passEff = passesFor/(passesFor+passesAgainst),
                   passEff = ifelse(is.na(passEff), .5, passEff)) %>% 
            left_join(avgPE, by="st") %>% 
            mutate(AdjPassEff = passEff-avgPE)
        
            driver_season_stats <- data %>%
                filter(year==input$selectyear2, 
                       raceNumber==input$selectrace) 
            
            driver_season_stats <- driver_season_stats %>% 
                mutate(xPoints = round(xPts),
                       PassEff = 100*passEff,
                       AdjPassEff = 100*AdjPassEff,
                       AEP = xFPDifference,
                       LapsLed = led,
                       Top5Perc = 100*(inTopFive/laps),
                       Pts = pts,
                       StartPM = lapOneChange,
                       ATP = atp,
                       ATP25 = atp25) %>% 
                rename("Driver"="driver",
                       "Start"="st",
                       "Finish"="fin",
                       "DevATP"=atp_deviation) %>% 
                mutate(Difference = Pts-xPoints) %>% 
                select(Driver, Finish, Start, Pts, xPoints, Difference, LapsLed, ATP, DevATP, ATP25, PassEff, AdjPassEff, Top5Perc, AEP, StartPM) %>%
                #round
                mutate_at(vars(Difference, ATP, DevATP, ATP25, PassEff, AdjPassEff, Top5Perc, AEP), list(~ round(.,1))) %>% 
                arrange(-Pts)
            
    }, options=list(pageLength=50, scrollX = TRUE))

            #### current elo ratings table ####
    output$eloTable <- DT::renderDataTable({
        elo_ratings %>% filter(year>2020) %>%
           group_by(driver) %>%
            slice(which.max(as.Date(date, '%m/%d/%Y'))) %>%
            mutate(EloRating = round(EloRating),
                   PreviousEloRating = round(PreviousEloRating),
                   `1 Race Change` = EloRating-PreviousEloRating) %>%
            arrange(-EloRating) %>%
            select(-year) %>%
            select(-PreviousEloRating) %>%
            rename(Driver = driver) %>%
            rename(LastUpdated = date)

        
    }, options=list(pageLength=50, scrollX = TRUE))
    
    #### historical elo ratings graph ####
    output$eloGraph <- renderPlotly({
        dg <- elo_ratings %>% filter(year!=2000, driver %in% input$selectedDrivers)
        g <- ggplot(data = dg, aes(x=date, y=EloRating, color=driver)) + geom_line() +
            labs(x="Date", title = "Elo Rating Over Time", subtitle = "Minimum 10 starts",
                 color="Driver", y="EloRating") +
            theme_bw() #+ theme(plot.title = element_text(size=22))
        ggplotly(g)
    })
 
    #### championship projections table ####
    output$champTable <- DT::renderDataTable({
        
        champ_projections_final <-reshape2::dcast(champ_projections, Driver~chamPos, sum, value.var = "prob")
        champ_projections_final <- champ_projections_final %>% left_join(champ_projections_exp) %>% 
            rename("Expected Champ Pos" = "x") %>% arrange(`Expected Champ Pos`)
        
    }, options=list(pageLength=50, scrollX = TRUE)) 
    
    output$date_text <- renderText({
        champ_projections_date
    })
    
    #### championship projections graph
    output$champGraph <- renderPlot({
        champ_projections %>%
            filter(Driver == input$selectchampdriver) %>% 
            ggplot() + geom_col(aes(x=chamPos, y=prob, fill=Driver)) + 
            theme(legend.position = "none") +
            scale_y_continuous(breaks=c(0,25,50,75,100), limits = c(0,100)) +
            labs(y="% probability of finishing the season in this position",
                 x="Championship finishing position",
                 title=paste0("2022 IndyCar Championship Projection for ", input$selectchampdriver),
                 subtitle = paste0("After simulating the remaining races ",champ_projections_season," times"),
                 caption = "www.thesingleseater.com")
    }, width = 600, height = 500)
}

shinyApp(ui, server)
