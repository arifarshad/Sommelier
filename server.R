# shiny functions
library(shiny)



shinyServer(function(input, output) {
 
  ## wine data
  red <- read.csv("winequality-red.csv", sep=";")
  white <- read.csv("winequality-white.csv", sep=";")
  
  red$kind <- rep("red", length(red$fixed.acidity))
  red$kind <- as.factor(red$kind)
  white$kind <- rep("white", length(white$fixed.acidity))
  white$kind <- as.factor(white$kind)
  red$quality <- as.factor(red$quality)
  white$quality <- as.factor(white$quality)
  full.data <- rbind(red, white)
  
  na.omit(full.data)
  
  # training and testing sets
  library(caret)
  
  
  ## configure parallel processing
  library(parallel)
  library(doParallel)
  
  # cross-validation by configuring trainControl object
  library(caret)
  require(randomForest)
  library(randomForest)
  fitControl <- trainControl(method="cv", number=10, allowParallel = TRUE)
  
  # the model
  fit <- load("fit_lda.RData")
  
  
  
  
  
  
  pq <- reactive({
    one <- input$slider1
    two <- input$slider2
    three <- input$slider3
    four <- input$slider4
    five <- input$slider5
    six <- input$slider6
    seven <- input$slider7
    eight <- input$slider8
    nine <- input$slider9
    ten <- input$slider10
    eleven <- input$slider11
    twelve <- input$kind 
    
    
    predict(get(fit), newdata=data.frame(fixed.acidity=one,
                                    volatile.acidity=two,
                                    citric.acid=three,
                                    residual.sugar=four,
                                    chlorides=five,
                                    free.sulfur.dioxide=six,
                                    total.sulfur.dioxide=seven,
                                    density=eight,
                                    pH=nine,
                                    sulphates=ten,
                                    alcohol=eleven,
                                    kind=twelve))
  })
  output$text <- renderText({
    pq()
    
  })
  library(ggplot2)
  
  
  output$plot <- renderPlot({
    f <- input$variable
    if(f=="Fixed.Acidity"){
      ggplot(data=full.data, aes(x=quality, y=fixed.acidity,fill=kind)) + geom_boxplot() + scale_fill_manual(values = c("firebrick4","lightyellow1"))
    } else if(f=="Volatile.Acidity"){
      ggplot(data=full.data, aes(x=quality, y=volatile.acidity,fill=kind)) + geom_boxplot() + scale_fill_manual(values = c("firebrick4","lightyellow1"))
    } else if(f=="Citric.Acid"){
      ggplot(data=full.data, aes(x=quality, y=citric.acid,fill=kind)) + geom_boxplot() + scale_fill_manual(values = c("firebrick4","lightyellow1"))
    } else if(f=="Residual.Sugar"){
      ggplot(data=full.data, aes(x=quality, y=residual.sugar,fill=kind)) + geom_boxplot() + scale_fill_manual(values = c("firebrick4","lightyellow1"))
    } else if(f=="Chlorides"){
      ggplot(data=full.data, aes(x=quality, y=chlorides,fill=kind)) + geom_boxplot() + scale_fill_manual(values = c("firebrick4","lightyellow1"))
    } else if(f=="Free.Sulfur.Dioxide"){
      ggplot(data=full.data, aes(x=quality, y=free.sulfur.dioxide, fill=kind)) + geom_boxplot() + scale_fill_manual(values = c("firebrick4","lightyellow1"))
    } else if(f=="Total.Sulfur.Dioxide"){
      ggplot(data=full.data, aes(x=quality, y=total.sulfur.dioxide, fill=kind)) + geom_boxplot() + scale_fill_manual(values = c("firebrick4","lightyellow1"))
    } else if(f=="Density"){
      ggplot(data=full.data, aes(x=quality, y=density,fill=kind)) + geom_boxplot() + scale_fill_manual(values = c("firebrick4","lightyellow1"))
    } else if(f=="pH"){
      ggplot(data=full.data, aes(x=quality, y=pH,fill=kind)) + geom_boxplot() + scale_fill_manual(values = c("firebrick4","lightyellow1"))
    } else if(f=="Sulphates"){
      ggplot(data=full.data, aes(x=quality, y=sulphates,fill=kind)) + geom_boxplot() + scale_fill_manual(values = c("firebrick4","lightyellow1"))
    } else if(f=="Alcohol"){
      ggplot(data=full.data, aes(x=quality, y=alcohol,fill=kind)) + geom_boxplot() + scale_fill_manual(values = c("firebrick4","lightyellow1"))
    }
    
  })
  
  
})


