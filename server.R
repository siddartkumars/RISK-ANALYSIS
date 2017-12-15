library(shiny)
library(shinythemes)
library("RCurl")
library("rjson")
library(plotrix)
library(shinyBS)
library(shinydashboard)
library(ggplot2)

library(readr)
sample <- read.csv('~/Banking/sample.csv',stringsAsFactors = FALSE)


## Factoring
sample$job = as.factor(sample$job)
sample$marital = as.factor(sample$marital)
sample$education = as.factor(sample$education)
sample$default = as.factor(sample$default)
sample$housing = as.factor(sample$housing)
sample$loan = as.factor(sample$loan)
sample$contact = as.factor(sample$contact)
sample$month = as.factor(sample$month)
sample$poutcome = as.factor(sample$poutcome)
#sample$output = ifelse(sample$y==1,1,0)
#sample$output = as.factor(sample$output)

## Binning age variable
sample$age_0_30 = ifelse(sample$age<=30,1,0)
sample$age_30_60 = ifelse(sample$age>30 & sample$age<=60,1,0)
sample$age_60_95 = ifelse(sample$age>60 & sample$age<=95,1,0)

#sample = sample[,-18]

##Splitting training and validation set

library(caret)
inTrain <- createDataPartition(y=sample$y, p=0.70, list=FALSE,times = 1) 

train = sample[inTrain,]
val = sample[-inTrain,]


## XGboost
library(xgboost)

#train$output = as.numeric(train$output)
#val$output = as.numeric(val$output)

dtrain = xgb.DMatrix(data= data.matrix(train[,-c(1,2,18)]),label=train$y)
dtest = xgb.DMatrix(data=data.matrix(val[,-c(1,2,18)]),label=val$y)

watch = list(train=dtrain,test = dtest) #Simultaneous error listing when runnning cg boost on trained data

xgb = xgb.train(data= dtrain,
                eta = 0.05,
                max_depth = 6,
                nround = 200,
                set.seed = 1,
                eval_metric = 'error',
                watchlist = watch,
                early_stopping_rounds = 10,
                subsample = 0.95,
                colsample = 0.85,
                objective = 'binary:logistic'
)

## Testing on validation set
val$pred = predict(xgb,data.matrix(val[,-c(1,2,18)]))

## Taking 0.5 as cut off for Yes (Give loan)
val$pred = ifelse(val$pred>0.45,1,0)

## Checking accuracy
accuracy <- table(val$pred, val$y)
sum(diag(accuracy))/sum(accuracy)



shinyServer(function(input, output,session) {
  v <- reactiveValues()
  v1 <- reactiveValues()
  v2 <- reactiveValues()
  v3 <- reactiveValues()
  count <- reactiveValues()
  new_data1 <- data.frame()
  new_data2 <- data.frame()
  new_data3 <- data.frame()
  new_data4 <- data.frame()
  azure <- data.frame()
  count$data = 0
  #  azure <- 1
  js$disableTab("Result")
  js$disableTab("Tabulation")
  js$disableTab("Visualization")
  
  
  
  output$contents <- renderTable({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    #azure <-  read.csv('testing.csv',stringsAsFactors = F) directly giving file name
    
    
    azure <-  read.csv(inFile$datapath, stringsAsFactors = F)
    
    ntext <- observeEvent(input$submit1, {
      
      
      js$enableTab("Result")
      js$enableTab("Tabulation")
      js$enableTab("Visualization")
      #azure <- read.csv("azure.csv",sep=",")
      # print(names(azure))
      
      #different variable means different factors to be created
      azure$job = as.factor(azure$job)
      azure$marital = as.factor(azure$marital)
      azure$education = as.factor(azure$education)
      azure$default = as.factor(azure$default)
      azure$housing = as.factor(azure$housing)
      azure$loan = as.factor(azure$loan)
      azure$contact = as.factor(azure$contact)
      azure$month = as.factor(azure$month)
      azure$poutcome = as.factor(azure$poutcome)
      
      
      ## Binning age variable
      azure$age_0_30 = ifelse(azure$age<=30,1,0)
      azure$age_30_60 = ifelse(azure$age>30 & azure$age<=60,1,0)
      azure$age_60_95 = ifelse(azure$age>60 & azure$age<=95,1,0)
      
      azure$y = predict(xgb,data.matrix(azure[,-c(1,2)]))
      
      ## Taking 0.5 as cut off for Yes (Give loan)
      azure$y = ifelse(azure$y>0.45,1,0)
      
      #azure$Probability_Range = ifelse(azure$pred <0.5,'Low',ifelse(azure$pred >=0.5 & azure$pred< 0.75,'Medium','High'))
      
      updateTabsetPanel(session, "navbar", "tab1")
      updateTabsetPanel(session, "navbar", "tab2")
      updateTabsetPanel(session, "navbar", "tab3")
      shinyjs::info("Please check the adjacent tab for the results")
      
      v1$data = azure
    })
    
    
    azure
  })
  
  
  
  
  
  output$table1 <- renderDataTable({
    new_data1<- rbind(new_data1,v1$data)
    #names(new_data1) <- c("Age","Job_Type","Marital_Status","Education","Default","Balance","Housing_Loan","Personal_Loan","Contact_Type","Day","Month","Duration","Campaign","Pdays","Previous","Poutcome","Converted","Converted(Yes/No)","Probability_Value","Probability_Range","ID")
    new_data1[,c(1,21)]
  })
  
  
  datasetInput <- reactive({
    
    new_data2<- rbind(new_data2,v1$data)
    
    #names(new_data2) <- c("Age","Job_Type","Marital_Status","Education","Default","Balance","Housing_Loan","Personal_Loan","Contact_Type","Day","Month","Duration","Campaign","Pdays","Previous","Poutcome","Converted","Converted(Yes/No)","Probability_Value","Probability_Range","ID")
    
    #new_data2[,c(21,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18,19,20)]
    new_data2[,-c(18:20)]
    
  })
  output$table2 <- renderDataTable({
    
    
    
    output$downloadData <- downloadHandler(
      filename = function() { paste('output', '.csv', sep=' ') },
      content = function(file) {
        write.csv(datasetInput(), file)
      }
    )
    
    datasetInput()
  })
  
  
  selectedData <- reactive({
    new_data4 = rbind(new_data4,v1$data)
    new_data4$job = as.numeric(new_data4$job)
    new_data4$marital = as.numeric(new_data4$marital)
    new_data4$education = as.numeric(new_data4$education)
    new_data4$contact = as.numeric(new_data4$contact)
    
    #new_data4 = new_data4[,c(1:17,21,18,19,20)]
    
    if(input$xcol=="job")
    {
      subset(new_data4[,(input$xcol)],new_data4$job>=input$job[1] & new_data4$job<=input$job[2])
    }
    else if(input$xcol=="age")
    {
      subset(new_data4[,(input$xcol)],new_data4$age>=input$age[1] & new_data4$age<=input$age[2])
    }
    else if(input$xcol=="marital")
    {
      subset(new_data4[,(input$xcol)],new_data4$marital>=input$marriage[1] & new_data4$marital<=input$marriage[2])
    }
    else if(input$xcol=="education")
    {
      subset(new_data4[,(input$xcol)],new_data4$education>=input$edu[1] & new_data4$education<=input$edu[2])
    }
    else if(input$xcol=="contact")
    {
      subset(new_data4[,(input$xcol)],new_data4$contact>=input$contact[1] & new_data4$contact<=input$contact[2])
    }
    else
    {
      new_data4[,(input$xcol)]
    }
    
    
  })
  
  selectedData1 <- reactive({
    #new_data4 = sample
    new_data4 = rbind(new_data4,v1$data)
    new_data4$job = as.numeric(new_data4$job)
    new_data4$marital = as.numeric(new_data4$marital)
    new_data4$education = as.numeric(new_data4$education)
    new_data4$contact = as.numeric(new_data4$contact)
    
    #new_data4 = new_data4[,c(1:17,21,18,19,20)]
    
    if(input$xcol=="job")
    {
      subset(new_data4[,(input$ycol)],new_data4$job>=input$job[1] & new_data4$job<=input$job[2])
    }
    
    else if(input$xcol=="age")
    {
      subset(new_data4[,(input$ycol)],new_data4$age>=input$age[1] & new_data4$age<=input$age[2])
    }
    else if(input$xcol=="marital")
    {
      subset(new_data4[,(input$ycol)],new_data4$marital>=input$marriage[1] & new_data4$marital<=input$marriage[2])
    }
    else if(input$xcol=="education")
    {
      subset(new_data4[,(input$ycol)],new_data4$education>=input$edu[1] & new_data4$education<=input$edu[2])
    }
    else if(input$xcol=="contact")
    {
      subset(new_data4[,(input$ycol)],new_data4$contact>=input$contact[1] & new_data4$contact<=input$contact[2])
    }
    else
    {
      new_data4[,(input$ycol)]
    }
    
    
    
    
  })
  
  
  
  
  
  
  
  ranges2 <- reactiveValues(x=NULL,y=NULL)
  output$plot1 <- renderPlot({
    
    if(input$plot_type=="barplot")
    {
      
      count <- (table(selectedData1()[1:input$range],selectedData()[1:input$range]))
      barplot(count,col=c("red","blue"),legend=(rownames(count)),xlab=input$xcol,ylab=input$ycol,beside=TRUE)
      
      
      
    }
    
    
    
    
  })
  
  output$plot2 <- renderPlot({
    if(input$plot_type=="barplot")
    {
      count <- table(selectedData1()[1:input$range],selectedData()[1:input$range])
      barplot(count,col=c("red","lightblue"),legend=rownames(count),xlim=ranges2$x,ylim=ranges2$y,xlab=input$xcol,ylab=input$ycol,beside=TRUE)
    }
    
    
    
  })
  
  
  
  
  
  observe({
    brush <- input$plot_brush
    if(!is.null(brush)){
      ranges2$x <- c(brush$xmin,brush$xmax)
      ranges2$y <- c(brush$ymin,brush$ymax)
    }
    
    else
    {  ranges2$x <- NULL
    ranges2$y <- NULL
    }
  })
  
  
  
  
  
  
  
  
  
})




