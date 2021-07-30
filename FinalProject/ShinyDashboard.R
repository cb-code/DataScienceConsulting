# install.packages(c("shiny", "ggplot2", "dplyr", "prophet", 'loo', "forecast", "DT", "lubridate", "zoo", "shinythemes"))

library(shiny)
library(ggplot2)
library(dplyr)
library(prophet)
library(forecast)
library(DT)
library(lubridate)
library(zoo)
library(loo)
library(shinythemes)


DJI <- read.csv("../data/DJI.csv")
SP500 <- read.csv("../data/SP500.csv")
VIX <- read.csv("../data/VIX.csv")

choices <- c("DJI","SP500","VIX")

data <- read.csv("../data/yield curve.csv", na.strings = "N/A")

colnames(data)[1] <- c("Date")

data$Date <- as.Date(data$Date, "%m/%d/%y")


colnames(data)[c(4,6,11,13)] <- c("3 Mo","1 Yr","10 Yr","30 Yr")

yield <- data[,c(4,6,11,13)]

colnames(yield) <- c("3 Mo","1 Yr","10 Yr","30 Yr")

choices2 <- colnames(yield)

dates <- as.Date(DJI$Date)

preds <-  seq(tail(dates,1) + days(1), tail(dates,1) + days(700), 1)

Sys.setlocale("LC_TIME","C")

weekdays <- weekdays.Date(preds) 

preds2 <- preds[!weekdays %in% c("Saturday","Sunday")]

wholedates <- c(dates, preds2[1:365])

ind <- index(ts(1:length(wholedates ), frequency = 30))

dates <- as.Date(data$Date)

preds <-  seq(tail(dates,1) + days(1), tail(dates,1) + days(700), 1)

weekdays <- weekdays.Date(preds) 

preds2 <- preds[!weekdays %in% c("Saturday","Sunday")]


wholedates2 <- c(dates, preds2[1:365])

ind2 <- index(ts(1:length(wholedates2), frequency = 90))




m1 <-   auto.arima(data$`3 Mo`, max.p = 5, max.d = 1, max.q = 5, seasonal = F)
m2 <-   auto.arima(data$`1 Yr`, max.p = 5, max.d = 1, max.q = 5, seasonal = F)
m3 <-   auto.arima(data$`10 Yr`, max.p = 5, max.d = 1, max.q = 5, seasonal = F)
m4 <-   auto.arima(data$`30 Yr`, max.p = 5, max.d = 1, max.q = 5, seasonal = F)
f1 <- forecast(m1, 365)
f2 <- forecast(m2, 365)
f3 <- forecast(m3, 365)
f4 <- forecast(m4, 365)
f <- data.frame(f1$mean,f2$mean,f3$mean,f4$mean)
f[f < 0] <- 0
f <- data.frame(Date =  wholedates2[(nrow(data)+1):(nrow(data)+365)], f)
colnames(f)[-1] <- c("3 Mo","1 Yr","10 Yr","30 Yr")

day.end.mo <- seq(as.Date("2020-12-01"),length=17,by="months") - 1

ff2 <- f[f$Date %in% day.end.mo,]

row.names(ff2) <- 1:nrow(ff2)


choices3 <- ff2$Date


m1 <-  HoltWinters(ts(data$`3 Mo`[!is.na(data$`3 Mo`)],frequency = 90))
m2 <-   HoltWinters(ts(data$`1 Yr`[!is.na(data$`1 Yr`)],frequency = 90))
m3 <-   HoltWinters(ts(data$`10 Yr`[!is.na(data$`10 Yr`)],frequency = 90))
m4 <-   HoltWinters(ts(data$`30 Yr`[!is.na(data$`30 Yr`)],frequency = 90))
f1 <- forecast(m1, 365)
f2 <- forecast(m2, 365)
f3 <- forecast(m3, 365)
f4 <- forecast(m4, 365)
f <- data.frame(f1$mean,f2$mean,f3$mean,f4$mean)
f[f < 0] <- 0
f <- data.frame(Date =  wholedates2[(nrow(data)+1):(nrow(data)+365)], f)
colnames(f)[-1] <- c("3 Mo","1 Yr","10 Yr","30 Yr")
ff3 <- f[f$Date %in% day.end.mo,]

row.names(ff3) <- 1:nrow(ff3)

m1 <-  ets(ts(data$`3 Mo`[!is.na(data$`3 Mo`)],frequency = 90))
m2 <-   ets(ts(data$`1 Yr`[!is.na(data$`1 Yr`)],frequency = 90))
m3 <-   ets(ts(data$`10 Yr`[!is.na(data$`10 Yr`)],frequency = 90))
m4 <-   ets(ts(data$`30 Yr`[!is.na(data$`30 Yr`)],frequency = 90))
f1 <- forecast(m1, 365)
f2 <- forecast(m2, 365)
f3 <- forecast(m3, 365)
f4 <- forecast(m4, 365)
f <- data.frame(f1$mean,f2$mean,f3$mean,f4$mean)
f[f < 0] <- 0
f <- data.frame(Date =  wholedates2[(nrow(data)+1):(nrow(data)+365)], f)
colnames(f)[-1] <- c("3 Mo","1 Yr","10 Yr","30 Yr")
ff4 <- f[f$Date %in% day.end.mo,]

row.names(ff4) <- 1:nrow(ff4)

ui <- fluidPage(
  theme = shinytheme("journal"),
  themeSelector(),
  
  navbarPage(
  "Shiny App",
  tabPanel(
    "Stock market index forecasting",
    sidebarLayout(
      sidebarPanel(
        
        selectInput("x", "Select a stock market index",
                    choices = choices
        ),
        
        
        
        radioButtons(inputId = "mod",
                     label = "Select a model",
                     choices = c("Holt-Winters", "ARIMA", "Prophet"),
                     selected = "Holt-Winters"),
        
        sliderInput("per", "Forecasting days ahead", 1, 365, value = 100)
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Time series trend", plotOutput("plot1")),
          tabPanel("Forecasting results", plotOutput("plot2")),
          tabPanel("Model details", verbatimTextOutput("model1")),
          tabPanel("Data details", dataTableOutput("data1"))
        )
      )
    )
    
  ),
  
  tabPanel(
    
    "Yield curve forecasting",
    
    sidebarLayout(
      
      sidebarPanel(
        
        selectInput("x2", "select a desired period", choices = choices2),
        
        radioButtons(inputId = "mod2",
                     
                     label = "select a model",
                     
                     choices = c("ETS", "ARIMA", "Holt-Winters"),
                     
                     selected = "ETS"),
        
        sliderInput("per2", "Forecasting Days Ahead", 1, 365, value = 100)
        
      ),
      
      mainPanel(
        
        tabsetPanel(
          
          tabPanel("Time series", plotOutput("plot12")),
          tabPanel("Forecasting Plot", plotOutput("plot22")),
          tabPanel("Forecasting Data", dataTableOutput("data22")),
          tabPanel("Model", verbatimTextOutput("model12")),
          tabPanel("Data", dataTableOutput("data12"))
        )
      )
    )
    
  ),
  
  
  tabPanel(
    
    "Yield curves comparison",
    
    sidebarLayout(
      
      sidebarPanel(
        
        selectInput("x3", "select a desired day", choices = choices3),
        
        radioButtons(inputId = "mod3",
                     
                     label = "select a model",
                     
                     choices = c("Holt-Winters", "ARIMA", "ETS"),
                     
                     selected = "Holt-Winters")
        
      ),
      
      mainPanel(
        
        tabsetPanel(
          tabPanel("Time series", plotOutput("plot23")),
          tabPanel("Forecasting Data", dataTableOutput("data23"))
        )
      )
    )
    
  )
  
  
)
)



server <- function(input, output) {
  
  myfulldata <- reactive({
    data <- get(input$x)
    data
  })
  
  mydata <- reactive({
    data <- get(input$x)
    dt <-  data[,c("Date","Close")]
    dt
  })
  
  myseries <- reactive({
    input$x
  })
  
  mymodel <- reactive({
    input$mod
  })
  
  myperiod <- reactive({
    input$per
  })
  
  output$data1 <- renderDataTable({
    datatable(myfulldata())
  })
  
  output$plot1 <- renderPlot( {
    
    dt <-  mydata()
    dt$Date <- as.Date(dt$Date)
    ggplot(dt, aes(Date, Close)) + geom_line() + theme_classic() +
      ggtitle(paste0("Time series plot of ",myseries()))
  }
  )
  
  output$model1 <- renderPrint ( {
    
    dt <-  mydata()
    nt <- nrow(dt)
    #use recent series to make predictions
    dt$Date <- as.Date(dt$Date)
    x <- dt[,"Close"]
    price <- x
    price.ts <- ts(price,  frequency = 30)
    if(mymodel() == "ARIMA") {
      m <-   auto.arima(price.ts, max.p = 3, max.d = 1, max.q = 3)
    } else if (mymodel() == "Holt-Winters")  {
      m <- HoltWinters(price.ts)
    } else {
      colnames(dt) <- c("ds","y")
      m <- prophet(dt)
      m <- str(m)
    }
    
    m
  }
  )
  output$plot2 <- renderPlot( {
    
    dt <-  mydata()
    nt <- nrow(dt)
    #use recent series to make predictions
    dt$Date <- as.Date(dt$Date)
    x <- dt[,"Close"]
    price <- x
    price.ts <- ts(price,frequency = 30)
    
    if(mymodel() == "ARIMA") {
      m <-   auto.arima(price.ts, max.p = 3, max.d = 1, max.q = 3)
      f <- forecast(m, myperiod())
      usedates <- wholedates[1:(nrow(DJI)+ myperiod())]
      plot(f, xlab = "years", xaxt='n')
      axis(1, at=  seq(ind[1], ind[length( usedates)],  length.out=7), 
           labels=seq(usedates[1], tail(usedates,1), length.out=7))
    } else if (mymodel() == "Holt-Winters")  {
      m <- HoltWinters(price.ts)
      f <- forecast(m, myperiod())
      usedates <- wholedates[1:(nrow(DJI)+ myperiod())]
      plot(f, xlab = "years", xaxt='n')
      axis(1, at=  seq(ind[1], ind[length( usedates)],  length.out=7), 
           labels=seq(usedates[1], tail(usedates,1), length.out=7))
      
    } else {
      colnames(dt) <- c("ds","y")
      m <- prophet(dt)
      future <- make_future_dataframe(m, myperiod())
      forecast <- predict(m, future)
      usedates <- wholedates[1:(nrow(DJI)+ myperiod())]
      plot(1:length(forecast$ds), forecast$yhat, xlab = "years", ylab = "close",
           type = "l",xaxt='n')
      axis(1, at = seq(1, length(usedates),  length.out=7), 
           labels=seq(usedates[1], tail(usedates,1), length.out=7))
      lines(1:length(forecast$ds), forecast$yhat[1:length(forecast$ds)],
            col = "red")
      lines(1:length(forecast$ds), forecast$yhat_lower[1:length(forecast$ds)],
            col = "blue")
      lines(1:length(forecast$ds), forecast$yhat_upper[1:length(forecast$ds)],
            col = "blue")
    }
  }
  )
  
  
  
  myfulldata2 <- reactive({
    
    data <- data[, c("Date", input$x2)]
    data
    
  })
  
  mydata2 <- reactive({
    data[, c(input$x2)]
  })
  
  myseries2 <- reactive({
    input$x2
  })
  
  mymodel2 <- reactive({
    input$mod2
  })
  
  myperiod2 <- reactive({
    input$per2
  })
  
  output$data12 <- renderDataTable({
    datatable(myfulldata2())
  })
  
  output$plot12 <- renderPlot( {
    dt <-  mydata2()
    usedates <- wholedates2[which(!is.na(dt))]
    dt <- dt[!is.na(dt)] 
    x <- ts(dt, frequency = 90)
    plot(1:length(x), dt, xlab = "years", xaxt='n', ylab = myseries2(),  main = paste0("Time series plot of ",myseries2()),
         type = "l", col = "indianred", lwd = 2)
    axis(1, at=  seq(1, length( usedates),  length.out=7), 
         labels=seq(usedates[1], tail(usedates,1), length.out=7))
    
  }
  )
  
  output$model12 <- renderPrint ({
    
    price <- mydata2()
    price <- price[!is.na(price)] 
    price.ts <- ts(price,frequency = 90)
    if(mymodel2() == "ARIMA") {
      m <-   auto.arima(price.ts, max.p =5, max.d = 1, max.q = 5, seasonal = F)
      
    } else if (mymodel2() == "Holt-Winters")  {
      m <- HoltWinters(price.ts)
    } else {
      m <-   ets(price.ts)
    }
    
    m
  }
  )
  output$plot22 <- renderPlot( {
    
    price <-  mydata2()
    price <- price [!is.na(price)] 
    price.ts <- ts(price, frequency = 90)
    if(mymodel2() == "ARIMA") {
      m <-   auto.arima(price.ts, max.p = 5, max.d = 1, max.q = 5, seasonal = F)
      f <- forecast(m, myperiod2())
      
      usedates <- wholedates2[1:(nrow(data)+ myperiod2())]
      plot(f, xlab = "years", xaxt='n')
      axis(1, at = seq(ind2[1], ind2[length(usedates)],  length.out=7), 
           labels=seq(usedates[1], tail(usedates,1), length.out=7))
      
    } else if (mymodel2() == "Holt-Winters")  {
      m <- HoltWinters(price.ts)
      f <- forecast(m, myperiod2())
      usedates <- wholedates2[1:(nrow(data)+ myperiod2())]
      plot(f, xlab = "years", xaxt='n')
      axis(1, at = seq(ind2[1], ind2[length(usedates)],  length.out=7), 
           labels=seq(usedates[1], tail(usedates,1), length.out=7))
    } else {
      m <- ets(price.ts)
      f <- forecast(m, myperiod2())
      usedates <- wholedates2[1:(nrow(data)+ myperiod2())]
      plot(f, xlab = "years", xaxt='n')
      axis(1, at = seq(ind2[1], ind2[length(usedates)],  length.out=7), 
           labels=seq(usedates[1], tail(usedates,1), length.out=7))
    }
  }
  )
  
  output$data22 <- renderDataTable( {
    
    price <-  mydata2()
    price <- price [!is.na(price)] 
    price.ts <- ts(price, frequency = 90)
    if(mymodel2() == "ARIMA") {
      m <-   auto.arima(price.ts, max.p = 5, max.d = 1, max.q = 5, seasonal = F)
      f <- forecast(m, myperiod2())
      f1 <- f$mean
      f2 <- f$upper[,2]
      f3 <- f$lower[,2]
      d <- data.frame(Date =  wholedates2[(nrow(data)+1):(nrow(data)+myperiod2())],   mean = f1, `95%Lower` =f3, `95%Upper` = f2)
      colnames(d)[-1] <- c("Forecasting","95%Lowerbound","95%Upperbound")
      datatable(d)
    } else if (mymodel2() == "Holt-Winters")  {
      m <- HoltWinters(price.ts)
      f <- forecast(m, myperiod2())
      f1 <- f$mean
      f2 <- f$upper[,2]
      f3 <- f$lower[,2]
      d <- data.frame(Date =  wholedates2[(nrow(data)+1):(nrow(data)+myperiod2())],mean = f1, `95%Lower` =f3, `95%Upper` =f2)
      colnames(d)[-1] <- c("Forecasting","95%Lowerbound","95%Upperbound")
      datatable(d)
      
    } else {
      m <- ets(price.ts)
      f <- forecast(m, myperiod2())
      f1 <- f$mean
      f2 <- f$upper[,2]
      f3 <- f$lower[,2]
      d <- data.frame(Date =  wholedates2[(nrow(data)+1):(nrow(data)+myperiod2())],mean = f1, `95%Lower` =f3, `95%Upper` =f2)
      colnames(d)[-1] <- c("Forecasting","95%Lowerbound","95%Upperbound")
      datatable(d)
    }
  }
  )
  
  mymodel3 <- reactive({
    input$mod3
  })
  myday <- reactive({
    input$x3
  })
  
  output$data23 <- renderDataTable( {
    
    if(mymodel3() == "ARIMA") {
      
      datatable(ff2)
      
    } else if (mymodel3() == "Holt-Winters")  {
      datatable(ff3)
      
    } else {
      datatable(ff4)
    }
  }
  )
  
  
  output$plot23 <- renderPlot( {
    
    if( mymodel3() == "ARIMA") {
      
      d <- ff2[ff2$Date == myday(), -1]
      plot(1:4, as.numeric(d[1,]), xaxt = "n", type = "l", lwd = 2, 
           col = "darkorange", ylab = "predicted rate",xlab = myday())
      points(cbind(1:4),as.numeric(d[1,]), pch = 16, col = "blue")
      axis(1, at = 1:4, labels = colnames(yield))
      
    } else if (mymodel3() == "Holt-Winters")  {
      
      d <- ff3[ff3$Date == myday(), -1]
      plot(1:4, as.numeric(d[1,]), xaxt = "n", type = "l", lwd = 2, 
           col = "darkorange", ylab = "predicted rate",xlab = myday())
      points(cbind(1:4),as.numeric(d[1,]), pch = 16, col = "blue")
      axis(1, at = 1:4, labels = colnames(yield))
    } else {
      d <- ff4[ff4$Date == myday(), -1]
      plot(1:4, as.numeric(d[1,]), xaxt = "n", type = "l", lwd = 2, 
           col = "darkorange", ylab = "predicted rate",xlab = myday())
      points(cbind(1:4),as.numeric(d[1,]), pch = 16, col = "blue")
      axis(1, at = 1:4, labels = colnames(yield))
    }
  }
  )
  
  
  
  
  
}


shinyApp(ui, server)
