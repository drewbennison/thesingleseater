#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(data.table)
library(tidyverse)
library(lubridate)

ui <- fluidPage(
    
    tags$head(
        tags$style(HTML("

     .multicol {

       -webkit-column-count: 5; /* Chrome, Safari, Opera */

       -moz-column-count: 5; /* Firefox */

       column-count: 5;

     }

   "))),
    
    
    
    
    
    title = "The Single Seater: IndyCar Stats",
    # Application title
    titlePanel("The Single Seater: IndyCar Stats"),
    tags$h3(tags$a(href="https://thesingleseater.com/", "Visit the blog")),
    
    conditionalPanel(
        condition = "input.tabpan == 'Season Stats'",
        fluidRow(column(
            width = 10,
            offset = 0,
            tags$h4("Driver season stats"),
            radioButtons("trackb", "Sort by track type:",
                         c("All Tracks" = "all",
                           "Oval" = "oval",
                           "Road and Street" = "road and street")),
            
            selectInput("selectyear", "Select a season:",
                        c("2020" = "2020",
                          "2019" = "2019")),
            tags$h5(tags$em(tags$a(href="https://thesingleseater.com/glossary/", "View the stat glossary")))
            ))),
    
    conditionalPanel(
        condition = "input.tabpan == 'Track Stats'",
        fluidRow(column(
            width = 10,
            offset = 0,
            h4("Track historical results (2008-2020)"),
            selectInput("selecttrack", "Select a track:", 
                        choices = NULL, 
                        selected = 1),
            tags$h5(tags$em(tags$a(href="https://thesingleseater.com/glossary/", "View the stat glossary")))
            ))),
    
    conditionalPanel(
        condition = "input.tabpan == 'Current Elo Ratings'",
        fluidRow(column(
            width = 10,
            offset = 0,
            h4("Current Elo Ratings")))),
    
    conditionalPanel(
        condition = "input.tabpan == 'Historical Elo Ratings'",
        fluidRow(column(
            width = 10,
            offset = 0,
            h4("Historical Elo Ratings"),
            wellPanel(
                
                tags$div(class = "multicol", checkboxGroupInput("selectedDrivers", choices = NULL, label = "Select drivers to show", selected = NULL)))
            ))),
    
    conditionalPanel(
        condition = "input.tabpan == 'Championship Projections'",
        fluidRow(column(
            width = 10,
            offset = 0,
            h4("Championship Projections"),
            h5("Last updated: 6/16/2020"),
            selectInput("selectchampdriver", "Select a driver to view their championship projection:", 
                        choices = NULL, 
                        selected = 1),))),
    
    mainPanel(
            tabsetPanel(id="tabpan", type="tabs",
                    tabPanel("Season Stats", DT::dataTableOutput("seasonTable")),
                    tabPanel("Track Stats", DT::dataTableOutput("trackTable")),
                    tabPanel("Current Elo Ratings", DT::dataTableOutput("eloTable")),
                    tabPanel("Historical Elo Ratings", plotOutput("eloGraph")),
                    tabPanel("Championship Projections", DT::dataTableOutput("champTable"), plotOutput("champGraph"))),
    )
)

server <- function(input, output,session) {
    
    #global data source
    data<- read.csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/master_backup/indycar_results.csv")
    
    caution_stats <- read.csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/master_backup/restartdata.csv")
    
    elo_ratings <- read.csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/elo_ratings/elo_tracker.csv") %>% 
        mutate(date=ymd(date))
    
    champ_projections <- read.csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/champPredictions/6_15_2020_champ.csv")
    
    champ_projections <- champ_projections %>% 
        filter(season!=0) %>% 
        select(driver, totalPoints, chamPos, season) %>% 
        group_by(driver, chamPos) %>% 
        add_count() %>% 
        mutate(prob = round((n/max(season)),2)) %>% 
        select(driver, chamPos, n, prob) %>% 
        distinct()
    
    choices_champ_projections <- champ_projections %>% 
        select(driver) %>% 
        distinct()
    
    updateSelectInput(session=session, inputId="selectchampdriver", choices=choices_champ_projections$driver)
    
    #dynamically populate choices for track selection
    choices_tracks <- data %>%
        select(track) %>% 
        distinct()
    
    updateSelectInput(session = session, inputId = "selecttrack", choices = choices_tracks$track)
    
    
    choices_drivers <- elo_ratings %>% 
        group_by(driver) %>% 
        filter(n()>10) %>% 
        select(driver) %>% 
        distinct()
    
    updateCheckboxGroupInput(session = session, inputId = "selectedDrivers", choices=choices_drivers$driver)
    
    
    output$seasonTable = DT::renderDataTable({
        
        
        
        #Calculate AFP from every starting position
        afp <- data %>%
            group_by(st) %>% 
            summarise(xFP = mean(fin))
        
        #Left join data with xFP for every driver's results
        data <- data %>%
            filter(year==input$selectyear) %>% 
            left_join(afp, by=c("st" = "st")) %>% 
            mutate(xFPDifference=xFP-fin)
        
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
            group_by(driver) %>% 
            mutate(percFavorablePass = 100*(sum(passesFor)/(sum(passesFor)+sum(passesAgainst))),
                   favorableStart = ifelse(lapOneChange>=0, 1,
                                           ifelse(lapOneChange<0, 0, NA)),
                   StartRetention = 100*mean(favorableStart),
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
                   ATP25 = mean(atp25),
                   DevATP25 = sd(atp25),
                   PassEff = 100*(sum(passesFor)/ (sum(passesFor)+sum(passesAgainst))),
                   RunningCheck = ifelse(status=="running",1,0),
                   RunPerc = 100*mean(RunningCheck),
                   AFS = mean(fastLapRank),
                   Top5Perc = 100*(sum(inTopFive)/sum(laps)),
                   #Average Surplus Position
                   AEP = mean(xFPDifference)) %>% 
            #SELECT DRIVER AND ANY VARIBLES BEFORE YOU SELECT DISTINCT
            distinct(driver, StartRetention, StartPM, Races, PMperStart, Pts, xPoints, AFP, DevFP, ASP, DevSP, ATP, DevATP, ATP25, DevATP25, PassEff, RunPerc, Top5Perc, AEP, AFS)
        
        season1<- driver_season_stats %>%
            mutate(Difference = Pts-xPoints) %>% 
            select(driver, Races, Pts, xPoints, Difference, AFP, DevFP, ASP, DevSP, ATP, DevATP, ATP25, DevATP25, PassEff, RunPerc, Top5Perc, AEP, AFS, StartRetention, StartPM, PMperStart) %>% 
            rename(Driver=c("Driver"="driver")) %>% 
            mutate_at(vars(Difference, AFP, DevFP, ASP, DevSP, ATP, DevATP, ATP25, DevATP25, PassEff, RunPerc, Top5Perc, AEP, StartRetention, PMperStart, AFS), list(~ round(.,1))) %>% 
            mutate_at(vars(xPoints, Difference), list(~ round(.,0))) %>% 
            arrange(-Pts)
        
        
    }, options=list(pageLength=50))
    
    
    output$trackTable = DT::renderDataTable({
        
        #Calculate AFP from every starting position
        afp <- data %>%
            group_by(st) %>% 
            summarise(xFP = mean(fin))
        
        #Left join data with xFP for every driver's results
        data <- data %>%
            left_join(afp, by=c("st" = "st")) %>% 
            mutate(xFPDifference=xFP-fin)
        
        driver_season_stats <- data %>% 
            filter(track==input$selecttrack) %>% 
            group_by(driver) %>% 
            mutate(percFavorablePass = 100*(sum(passesFor)/(sum(passesFor)+sum(passesAgainst))),
                   favorableStart = ifelse(lapOneChange>=0, 1,
                                           ifelse(lapOneChange<0, 0, NA)),
                   StartRetention = 100*mean(favorableStart),
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
                   ATP25 = mean(atp25),
                   PassEff = 100*(sum(passesFor)/ (sum(passesFor)+sum(passesAgainst))),
                   RunningCheck = ifelse(status=="running",1,0),
                   RunPerc = 100*mean(RunningCheck),
                   AvgFastSpeed = mean(fastLapRank),
                   Top5Perc = 100*(sum(inTopFive)/sum(laps)),
                   #Average Surplus Position
                   AEP = mean(xFPDifference),
                   #new additions
                   Wins = sum(ifelse(fin==1,1,0)),
                   Poles = sum(ifelse(st==1,1,0)),
                   TopFives = sum(ifelse(fin<6,1,0)),
                   LapsLed = sum(led)) %>% 
            #SELECT DRIVER AND ANY VARIBLES BEFORE YOU SELECT DISTINCT
            distinct(driver, StartRetention, StartPM, Races, PMperStart, Pts, xPoints, AFP, DevFP, ASP, DevSP, ATP, ATP25, PassEff, RunPerc, Top5Perc, AEP, Wins, Poles, TopFives, LapsLed)
        
        season1<- driver_season_stats %>%
            mutate(Difference = Pts-xPoints) %>% 
            select(driver, Races, Wins, Poles, TopFives, Pts, AFP, DevFP, ASP, DevSP, RunPerc,AEP, LapsLed) %>% 
            rename(Driver=c("Driver"="driver")) %>% 
            mutate_at(vars(AFP, DevFP, ASP, DevSP, RunPerc,AEP), list(~ round(.,1))) %>% 
            arrange(-Pts)
        
    }, options=list(pageLength=50))
        
    
    
    output$eloTable <- DT::renderDataTable({
        elo_ratings %>% filter(year>2019) %>% 
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

        
    }, options=list(pageLength=50))
    
    output$eloGraph <- renderPlot({
        
        elo_ratings %>% filter(year!=2000, driver %in% input$selectedDrivers) %>%
            ggplot(aes(x=date, y=EloRating, color=driver)) + geom_line() +
            labs(x="Date", title = "Elo Rating Over Time", subtitle = "Minimum 10 starts",
                 color="Driver", y="EloRating") +
            theme_bw() + theme(plot.title = element_text(size=22))
    }, height="auto", width="auto")
 
    #### championship projections table ####
    output$champTable <- DT::renderDataTable({
        
        
        champ_projections_final <-dcast(champ_projections, driver~chamPos, sum, value.var = "prob")
        champ_projections_final <- champ_projections_final %>% arrange_at(2:32, desc)
        
    }, options=list(pageLength=50))   
    
    output$champGraph <- renderPlot({
        champ_projections %>%
            filter(driver == input$selectchampdriver) %>% 
            ggplot() + geom_col(aes(x=chamPos, y=prob, fill=driver)) + 
            theme(legend.position = "none")+
            scale_x_discrete(limits = c(1, 5, 10, 15, 20, 25, 31)) +
            scale_y_continuous(breaks=c(0,.25,.50,.75,1), limits = c(0,1)) +
            labs(y="Probability of finishing the season in position",
                 x="Championship finishing position",
                 title=paste0("2020 IndyCar Championship Projection for ", input$selectchampdriver),
                 subtitle = "After simulating the remaining races 4,000 times",
                 caption = "www.thesingleseater.com")
    }, width = 600, height = 600)
}

shinyApp(ui, server)
