server <- function(input, output) {
    
    dataset <- reactive({
        noPlayers[sample(nrow(noPlayers), input$sampleSize),]
    })
    
    output$plot <- renderPlot({
        
        p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point() + facet_grid(HoF ~ .)
        
        if (input$color != 'None')
            p <- p + aes_string(color=input$color)
        
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
    
    output$pred <- renderTable({
        data.frame(Induction_Odds = 1- predict(model, predsSlider(), type = "response")) # since the base is 
    })
    
}
