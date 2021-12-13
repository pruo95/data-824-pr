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
