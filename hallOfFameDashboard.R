#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny)
library(ggplot2)
library(Lahman)
library(dplyr)
library(tidyverse)
library(MASS)
library(gam)
library(car)
library(pROC)
library(profileModel)
library(shiny)
library(ggplot2)
library(caret)
library(nnet)


### original data scraping to get the proper datatable

hof <- data.frame(HallOfFame)
hofy <- hof[hof$inducted == "Y",]
hofy <- hofy[hofy$category == "Player",]
hofy <- hofy[,c("playerID","inducted")]
names(hofy)[names(hofy) == 'inducted'] <- 'HoF'
hofy$HoF <- 1

hofn <- hof[hof$inducted == "N",]
hofn <- hofn[hofn$category == "Player",]
hofn <- hofn[,c("playerID","inducted")]
names(hofn)[names(hofn) == 'inducted'] <- 'HoF'
hofn$HoF <- 0
hofn <- hofn[!duplicated(hofn),]

hofe <- merge(hofy,hofn,by = "playerID", all = T)

hofe$HoF <- ifelse(hofe$HoF.x == 1,1)
hofe[is.na(hofe)] <- 0
hofe <- hofe[,c("playerID","HoF")]

batting <- Batting %>% group_by(playerID) %>% summarize(G = sum(G), AB = sum(AB), R = sum(R), H = sum(H), X2B = sum(X2B), X3B = sum(X3B), HR = sum(HR),
                                                        RBI = sum(RBI), SB = sum(SB), CS = sum(CS), BB = sum(BB), SO = sum(SO),IBB = sum(IBB), HBP = sum(HBP),
                                                        SH = sum(SH), SF = sum(SF), GIDP = sum(GIDP), seasons = n(),last = max(yearID))

stats <- battingStats(data = batting, idvars = c("playerID","last","seasons"))
stats[is.na(stats)] <- 0

#eligible <- stats[stats$seasons >= 10,]
#eligible <- eligible[eligible$last <= 2012,]

awardCounts <- AwardsPlayers %>% group_by(playerID) %>% summarize(awards = n())

ASGcounts <- AllstarFull %>% group_by(playerID) %>% summarize(ASG = n())

#eligible <- merge(eligible, awardCounts, by = "playerID", all.x = T, all.y = F)
#eligible[is.na(eligible)] <- 0

position <- Appearances %>% group_by(playerID) %>% summarize(G = sum(G_all), P = sum(G_p), first = sum(G_1b), second = sum(G_2b), third = sum(G_3b),
                                                             short = sum(G_ss), left = sum(G_lf), center = sum(G_cf), right = sum(G_rf), out = sum(G_of),
                                                             dh = sum(G_dh), catch = sum(G_c))

position[is.na(position)] <- 0

position$pitcher <- apply(position[,c(3:13)],1,max)

i <- 1

for (i in 1:nrow(position)) {
    if (position[i,3] == position[i,14]) {
        position[i,14] <- 1
    } else {
        position[i,14] <- 0
    }
    i <- i+1
}

position <- position[,c("playerID","pitcher")]


# merge
eligible <- merge(hofe,stats, by = "playerID", all.x = T, all.y = F)
eligible <- merge(eligible,ASGcounts, by = "playerID", all.x = T, all.y = F)
eligible <- merge(eligible,awardCounts, by = "playerID", all.x = T, all.y = F)

eligible <- merge(eligible,position,by = "playerID", all.x = T, all.y = F)
eligible <- eligible[eligible$pitcher == 0,]

eligible <- eligible[!is.na(eligible$pitcher),]

# <- merge(eligible,hof,by = "playerID",all.x = T, all.y = F)
eligible[is.na(eligible)] <- 0

drops <- c("pitcher")

hofData <- eligible[ , !(names(eligible) %in% drops)]

hofData$HoF <- ifelse(hofData$HoF == 1, "Inducted", "Not Inducted")

hofData$HoF <- as.factor(hofData$HoF)
hofData <- hofData[!(hofData$seasons < 10),]

noPlayers <- hofData[,-1]
rownames(noPlayers) <- hofData[,1]
noPlayers$PA <- NULL
noPlayers$TB <- NULL
noPlayers$OPS <- NULL
noPlayers$SF <- NULL
noPlayers$CS <- NULL
noPlayers$IBB <- NULL
noPlayers$GIDP <- NULL
noPlayers$HoF <- as.factor(noPlayers$HoF)

noLast <- noPlayers
noLast$last <- NULL # I don't want the Last Season to be a part of the logit, but I need it for the time series viz

noLastTrim <- noLast # just a table with the predictors from the final model

noLastTrim$G <- NULL
noLastTrim$HR <- NULL
noLastTrim$SB <- NULL
noLastTrim$SO <- NULL
noLastTrim$seasons <- NULL
noLastTrim$BA <- NULL
noLastTrim$BABIP <- NULL

dataset <- noPlayers
################
# Define UI for application that draws a histogram
ui <- navbarPage("R Shiny Dashboard",
                 tabPanel(
                     "Welcome",
                     tabName = "welcome",
                     icon=icon("door-open"),
                     
                     fluidPage(theme=shinytheme("cerulean"),
                               h1("Welcome to my Shiny Dashboard!"),
                               br(),
                               p(strong(tags$u("What is this dashboard all about?"))),
                               p("I will show visualizations for the Hall of Fame dataset"), 
                               br(),
                               p(strong(tags$u("How can I use this dashboard?"))),
                               p("You can click on any of the tabs above to see a different analysis of the data.")
                     )
                 ),
                 tabPanel("Explore",
                          tabname="Explore",
                          icon=icon("play"),
                          pageWithSidebar(
                              
                              headerPanel("Exploring the Dataset"),
                              
                              sidebarPanel(
                                  
                                  sliderInput('sampleSize', 'Sample Size', min=1, max=nrow(dataset),
                                              value=min(10, nrow(dataset)), step=10, round=0),
                                  
                                  selectInput('x', 'X', names(dataset[,-1])),
                                  selectInput('y', 'Y', names(dataset[,-1]), names(dataset[,-1])[[2]]),
                                  selectInput('color', 'Color', c('None', names(dataset[,-1]))),
                                  
                                  checkboxInput('jitter', 'Jitter'),
                                  checkboxInput('smooth', 'Smooth')
                                  ),
                              
                              mainPanel(
                                  plotOutput('plot')
                                  )
                              )
                          ),
                 
                 tabPanel("Time Series",
                          tabname="Time Series",
                          icon=icon("forward"),
                          pageWithSidebar(
                              
                              headerPanel("Change in Predictors Over Time"),
                              
                              sidebarPanel(
                                  
                                  selectInput('tsy', 'Y', names(dataset[,-1]), names(dataset[,-1])[[2]]),
                              ),
                              
                              mainPanel(
                                  plotOutput('tseries')
                              )
                          )
                 ),
                 
                 tabPanel("Logistic Regression",
                          tabname="Logistic Regression",
                          icon=icon("signal"),
                          fluidPage(
                              
                              headerPanel("Logistic Regression Model"),
                              
                              #sidebarPanel(
                              #    
                              #    selectInput('tsy', 'Y', names(dataset[,-1]), names(dataset[,-1])[[2]]),
                              #),
                              
                              mainPanel(
                                  h2("Summary of the Final Logistic Regression Model"),
                                  verbatimTextOutput("model"),
                                  verbatimTextOutput("matrix")
                              )
                          )
                 ),
                 
                 tabPanel("Compare",
                          tabname="Comapre",
                          icon=icon("search"),
                          fluidPage(
                              headerPanel("Comparing the Variables in the Logit Model"),
                              
                              column(6,
                                     "Full Model Predictors",
                                     plotOutput("radar")),
                              column(6,
                                     "Trimmed Model Predictors",
                                     plotOutput("radarTrim"))
                          )
                 ),
                 
                 tabPanel("Predict",
                          tabname="Predict",
                          icon=icon("flag"),
                          pageWithSidebar(
                              headerPanel("New Data to Predict Hall of Fame Induction"),
                              sidebarPanel(
                                  sliderInput("pAB", "At Bats: ", min = 661, max = 14053, value = 6030, step=1),
                                  sliderInput("pR", "Runs Scored: ", min = 54, max = 2295, value = 891, step=1),
                                  sliderInput("pH", "Hits: ", min = 153, max = 4256, value = 1702, step=1),
                                  sliderInput("pX2B", "Double: ", min = 18, max = 792, value = 295, step=1),
                                  sliderInput("pX3B", "Triples: ", min = 1, max = 309, value = 60, step=1),
                                  sliderInput("pRBI", "Runs Batted In: ", min = 1, max = 2297, value = 827, step=1),
                                  sliderInput("pBB", "Walks: ", min = 24, max = 2558, value = 635, step=1),
                                  sliderInput("pHBP", "Hit By Pitch: ", min = 0, max = 287, value = 42, step=1),
                                  sliderInput("pSH", "Sacrifice Hits: ", min = 0, max = 512, value = 67, step=1),
                                  sliderInput("pSlugPct", "Slugging Percentage: ", min = 0.260, max = 0.690, value = 0.420, step=0.001),
                                  sliderInput("pOBP", "On Base Percentage: ", min = 0.240, max = 0.483, value = 0.348, step=0.001),
                                  sliderInput("pASG", "All Star Game Nominations: ", min = 0, max = 26, value = 3, step=1),
                                  sliderInput("pawards", "Awards Earned: ", min = 0, max = 47, value = 5, step=1)
                              ),
                              mainPanel(
                                  tableOutput("pred")
                              )
                              
                          )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    dataset <- reactive({
        noPlayers[sample(nrow(noPlayers), input$sampleSize),]
    })
    
    output$plot <- renderPlot({
        
        p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point() + facet_grid(HoF ~ .)
        
        if (input$color != 'None')
            p <- p + aes_string(color=input$color)
        
        #facets <- paste(input$facet_row, '~', input$facet_col)
        #if (facets != '. ~ .')
        #    p <- p + facet_grid(facets)
        
        if (input$jitter)
            p <- p + geom_jitter()
        if (input$smooth)
            p <- p + geom_smooth()
        
        print(p)
        
    }, height=700)
    
    ### Radar Plots
    
    output$radar <- renderPlot({
        ggRadar(data = noLast, aes(color = HoF),
                legend.position = "bottom",
                main = "Radar Plot, Inducted v Not Inducted")
    })
    
    output$radarTrim <- renderPlot({
        ggRadar(data = noLastTrim, aes(color = HoF),
                legend.position = "bottom",
                main = "Radar Plot, Inducted v Not Inducted")
    })
    
    ### Time Series
    
    output$tseries <- renderPlot({
        
        tseries <- ggplot(data = noPlayers, aes_string(x=noPlayers$last, y=input$tsy)) + geom_point() + geom_smooth() + xlab("Year of Final Season")
        
        tseries <- tseries + aes_string(color=noPlayers$HoF)

        print(tseries)
        
    }, height=700)
    
    ### logistic regression model
    
    set.seed(11)
    train_ind <- sample(seq_len(nrow(noLast)), size = floor(0.7 * nrow(noLast)))
        
    train1 <- noLast[train_ind, ]
    test1 <- noLast[-train_ind, ]
        
    model <- glm(HoF ~., data = train1, family = "binomial") %>% 
        stepAIC(trace = FALSE)
    
    output$model <- renderPrint({
        print(summary(model))
    })
    
    
    preds1 <- predict(model, test1, type = "response")
    predicted.classes <- ifelse(preds1 > 0.5, "Not Inducted", "Inducted")
    predicted.classes <- as.factor(predicted.classes)
        
        
    output$matrix <- renderPrint({
        confusionMatrix(predicted.classes,test1$HoF, mode = "everything", positive = "Inducted")
    })
    
    ### Predict
    
    predsSlider <- reactive({
        #data.frame(Name = c("AB","R","H","X2B","X3B","RBI","BB","HBP","SH","SlugPct","OBP","ASG","awards"),
        #           Value = as.character(c(input$pAB, input$pR, input$pH, input$pX2B, input$pX3B, input$pRBI, input$pBB, input$pHBP, input$pSH, input$pSlugPct,
        #                                  input$pOBP, input$pASG, input$pawards)))
        data.frame(AB = as.numeric(input$pAB),
                   R = as.numeric(input$pR),
                   H = as.numeric(input$pH),
                   X2B = as.numeric(input$pX2B),
                   X3B = as.numeric(input$pX3B),
                   RBI = as.numeric(input$pRBI),
                   BB = as.numeric(input$pBB),
                   HBP = as.numeric(input$pHBP),
                   SH = as.numeric(input$pSH),
                   SlugPct = as.numeric(input$pSlugPct),
                   OBP = as.numeric(input$pOBP),
                   ASG = as.numeric(input$pASG),
                   awards = as.numeric(input$pawards)
        )
    })
    
    #classSlider <- reactive({
    #    data.frame(Induction_Odds = 1- predict(model, predsSlider(), type = "response"))
    #})
    #classSlider <- ifelse(classSlider > 0.5, "Not Inducted", "Inducted")
    #classSlider <- as.factor(classSlider)
    
    output$pred <- renderTable({
        #classSlider
        #predsSlider()
        data.frame(Induction_Odds = 1- predict(model, predsSlider(), type = "response")) # since the base is 
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
