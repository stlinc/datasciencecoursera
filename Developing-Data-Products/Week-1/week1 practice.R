install.packages("shiny")
library(shiny)

# ui.R
library(shiny)
shinyUI(pageWithSidebar(
    headerPanel("Data science FTW!"),
    sidebarPanel(
        h3('Sidebar text')
    ),
    mainPanel(
        h3('Main Panel text')
    )
))

# server.r
library(shiny)
shinyServer(
    function(input, output) {
    }
)

# R functions for HTML markup
# ui.R
shinyUI(pageWithSidebar(
    headerPanel("Illustrating markup"),
    sidebarPanel(
        h1('Sidebar panel'),
        h1('H1 text'),
        h2('H2 Text'),
        h3('H3 Text'),
        h4('H4 Text')
        
    ),
    mainPanel(
        h3('Main Panel text'),
        code('some code'),
        p('some ordinary text')
    )
))

# Part of ui.R
mainPanel(
    h3('Illustrating outputs'),
    h4('You entered'),
    verbatimTextOutput("oid1"),
    h4('You entered'),
    verbatimTextOutput("oid2"),
    h4('You entered'),
    verbatimTextOutput("odate")
)

# server.R
shinyServer(
    function(input, output) {
        output$oid1 <- renderPrint({input$id1})
        output$oid2 <- renderPrint({input$id2})
        output$odate <- renderPrint({input$date})
    }
)

#Prediction
#ui.R
shinyUI(
    pageWithSidebar(
        # Application title
        headerPanel("Diabetes prediction"),
        
        sidebarPanel(
            numericInput('glucose', 'Glucose mg/dl', 90, min = 50, max = 200, step = 5),
            submitButton('Submit')
        ),
        mainPanel(
            h3('Results of prediction'),
            h4('You entered'),
            verbatimTextOutput("inputValue"),
            h4('Which resulted in a prediction of '),
            verbatimTextOutput("prediction")
        )
    )
)
# server.R
diabetesRisk <- function(glucose) glucose / 200

shinyServer(
    function(input, output) {
        output$inputValue <- renderPrint({input$glucose})
        output$prediction <- renderPrint({diabetesRisk(input$glucose)})
    }
)

# Interactive image
# ui.R
shinyUI(pageWithSidebar(
    headerPanel("Example plot"),
    sidebarPanel(
        sliderInput('mu', 'Guess at the mean',value = 70, min = 62, max = 74, step = 0.05)
    ),
    mainPanel(
        plotOutput('newHist')
    )
))

# server.R
library(UsingR)
data(galton)

shinyServer(
    function(input, output) {
        output$newHist <- renderPlot({
            hist(galton$child, xlab='child height', col='lightblue',main='Histogram')
            mu <- input$mu
            lines(c(mu, mu), c(0, 200),col="red",lwd=5)
            mse <- mean((galton$child - mu)^2)
            text(63, 150, paste("mu = ", mu))
            text(63, 140, paste("MSE = ", round(mse, 2)))
        })
        
    }
)

# shinyapps.io
# https://www.shinyapps.io/admin/#/dashboard?referral=coursera
install.packages('rsconnect')
rsconnect::setAccountInfo(name='stlinc', token='0A1F6C931CCA083CACED0777007185A2', secret='ZBHlVr2flGvYQ6b0QEHyDl61SuX9Ppi5QiGcmZLu')

# Deploy shiny apps.
library(rsconnect)
# rsconnect::deployApp('path/to/your/app')

# Experiment (code in the slidify document)

# ui.R
library(shiny)

shinyUI(pageWithSidebar(
    headerPanel("Hello Shiny!"),
    sidebarPanel(
        textInput(inputId="text1", label = "Input Text1"),
        textInput(inputId="text2", label = "Input Text2")
    ),
    mainPanel(
        p('Output text1'),
        textOutput('text1'),
        p('Output text2'),
        textOutput('text2'),
        p('Output text3'),
        textOutput('text3'),
        p('Outside text'),
        textOutput('text4'),
        p('Inside text, but non-reactive'),
        textOutput('text5')
    )
))

# server.R Set x <- 0 before running

library(shiny)
x <<- x + 1
y <<- 0

shinyServer(
    function(input, output) {
        y <<- y + 1
        output$text1 <- renderText({input$text1})
        output$text2 <- renderText({input$text2})
        output$text3 <- renderText({as.numeric(input$text1)+1})
        output$text4 <- renderText(y)
        output$text5 <- renderText(x)
    }
)

# Run app with code displayed
runApp(display.mode='showcase')

# Reactive expressions (ui.R same as Experiment)
# server.R
shinyServer(
    function(input, output) {
        x <- reactive({as.numeric(input$text1)+100})
        output$text1 <- renderText({x()                          })
        output$text2 <- renderText({x() + as.numeric(input$text2)})
    }
)

# Non-reactive reactivity (what?)
# ui.R
shinyUI(pageWithSidebar(
    headerPanel("Hello Shiny!"),
    sidebarPanel(
        textInput(inputId="text1", label = "Input Text1"),
        textInput(inputId="text2", label = "Input Text2"),
        actionButton("goButton", "Go!")
    ),
    mainPanel(
        p('Output text1'),
        textOutput('text1'),
        p('Output text2'),
        textOutput('text2'),
        p('Output text3'),
        textOutput('text3')
    )
))

# Server.R
shinyServer(
    function(input, output) {
        output$text1 <- renderText({input$text1})
        output$text2 <- renderText({input$text2})
        output$text3 <- renderText({
            input$goButton
            isolate(paste(input$text1, input$text2))
        })
    }
)

# Add more codes to server.R to count number of times "Go" button gets pressed.
# server.R
output$text3 <- renderText({
    if (input$goButton == 0) "You have not pressed the button"
    else if (input$goButton == 1) "you pressed it once"
    else "OK quit pressing it"
})

# manipulate
library(manipulate)
manipulate(plot(1:x), x = slider(1, 100))

# Example from the regression class
library(UsingR)
library(manipulate)
myHist <- function(mu){
    hist(galton$child,col="blue",breaks=100)
    lines(c(mu, mu), c(0, 150),col="red",lwd=5)
    mse <- mean((galton$child - mu)^2)
    text(63, 150, paste("mu = ", mu))
    text(63, 140, paste("MSE = ", round(mse, 2)))
}
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))

# rCharts
install.packages("devtools")
require(devtools)
# options(download.file.method = "wininet") # for Windows
# library("devtools")
# install_github('rCharts', 'ramnathv')
install.packages("downloader")
library(downloader)
download("https://github.com/ramnathv/rCharts/archive/master.tar.gz", "rCharts.tar.gz")
require(rCharts)
haireye = as.data.frame(HairEyeColor)
n1 <- nPlot(Freq ~ Hair, group = 'Eye', type = 'multiBarChart',
            data = subset(haireye, Sex == 'Male')
)
n1$save('fig/n1.html', cdn = TRUE)
cat('<iframe src="fig/n1.html" width=100%, height=600></iframe>')
n1

# Deconstructing another example
## Example 1 Facetted Scatterplot
names(iris) = gsub("\\.", "", names(iris))
r1 <- rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = 'Species', type = 'point')
r1$save('fig/r1.html', cdn = TRUE)
cat('<iframe src="fig/r1.html" width=100%, height=600></iframe>')
r1

# Example 2 Facetted Barplot
hair_eye = as.data.frame(HairEyeColor)
r2 <- rPlot(Freq ~ Hair | Eye, color = 'Eye', data = hair_eye, type = 'bar')
r2$save('fig/r2.html', cdn = TRUE)
cat('<iframe src="fig/r2.html" width=100%, height=600></iframe>')
r2

# How to get the js/html or publish an rChart
r1 <- rPlot(mpg ~ wt | am + vs, data = mtcars, type = "point", color = "gear")
r1$print("chart1") # print out the js 
r1$save('myPlot.html') #save as html file
r1$publish('myPlot', host = 'gist') # save to gist, rjson required
r1$publish('myPlot', host = 'rpubs') # save to rpubs

# morris package for time-series
data(economics, package = "ggplot2")
econ <- transform(economics, date = as.character(date))
m1 <- mPlot(x = "date", y = c("psavert", "uempmed"), type = "Line", data = econ)
m1$set(pointSize = 0, lineWidth = 1)
m1$save('fig/m1.html', cdn = TRUE)
cat('<iframe src="fig/m1.html" width=100%, height=600></iframe>')
m1

# xCharts
require(reshape2)
uspexp <- melt(USPersonalExpenditure)
names(uspexp)[1:2] = c("category", "year")
x1 <- xPlot(value ~ year, group = "category", data = uspexp, type = "line-dotted")
x1$save('fig/x1.html', cdn = TRUE)
cat('<iframe src="fig/x1.html" width=100%, height=600></iframe>')
x1


# Leaflet for maps
map3 <- Leaflet$new()
map3$setView(c(51.505, -0.09), zoom = 13)
map3$marker(c(51.5, -0.09), bindPopup = "<p> Hi. I am a popup </p>")
map3$marker(c(51.495, -0.083), bindPopup = "<p> Hi. I am another popup </p>")
map3$save('fig/map3.html', cdn = TRUE)
cat('<iframe src="fig/map3.html" width=100%, height=600></iframe>')
map3

# Rickshaw
usp = reshape2::melt(USPersonalExpenditure)
# get the decades into a date Rickshaw likes
usp$Var2 <- as.numeric(as.POSIXct(paste0(usp$Var2, "-01-01")))
p4 <- Rickshaw$new()
p4$layer(value ~ Var2, group = "Var1", data = usp, type = "area", width = 560)
# add a helpful slider this easily; other features TRUE as a default
p4$set(slider = TRUE)
p4$save('fig/p4.html', cdn = TRUE)
cat('<iframe src="fig/p4.html" width=100%, height=600></iframe>')
p4

# Rickshaw charts
h1 <- hPlot(x = "Wr.Hnd", y = "NW.Hnd", data = MASS::survey, type = c("line", "bubble", "scatter"), group = "Clap", size = "Age")
h1$save('fig/h1.html', cdn = TRUE)
cat('<iframe src="fig/h1.html" width=100%, height=600></iframe>')
h1

# GoogleVis
library(ggvis)
library(dplyr)
install.packages("googleVis")
suppressPackageStartupMessages(library(googleVis))
M <- gvisMotionChart(Fruits, "Fruit", "Year", options=list(width=600, height=400))
# print(M,"chart")
plot(M)

# Plots on maps
G <- gvisGeoChart(Exports, locationvar="Country", colorvar="Profit",options=list(width=600, height=400))
# print(G,"chart")
plot(G)

# Specifying a region
G2 <- gvisGeoChart(Exports, locationvar="Country", colorvar="Profit",options=list(width=600, height=400,region="150"))
# print(G2,"chart")
plot(G2)

# Setting more options
df <- data.frame(label=c("US", "GB", "BR"), val1=c(1,3,4), val2=c(23,12,32))
Line <- gvisLineChart(df, xvar="label", yvar=c("val1","val2"),
                      options=list(title="Hello World", legend="bottom",
                                   titleTextStyle="{color:'red', fontSize:18}",                         
                                   vAxis="{gridlines:{color:'red', count:3}}",
                                   hAxis="{title:'My Label', titleTextStyle:{color:'blue'}}",
                                   series="[{color:'green', targetAxisIndex: 0}, 
                         {color: 'blue',targetAxisIndex:1}]",
                                   vAxes="[{title:'Value 1 (%)', format:'##,######%'}, 
                                  {title:'Value 2 (\U00A3)'}]",                          
                                   curveType="function", width=500, height=300                         
                      ))
# print(Line,"chart")
plot(Line)

# Combining multiple plots together
G <- gvisGeoChart(Exports, "Country", "Profit",options=list(width=200, height=100))
T1 <- gvisTable(Exports,options=list(width=200, height=270))
M <- gvisMotionChart(Fruits, "Fruit", "Year", options=list(width=400, height=370))
GT <- gvisMerge(G,T1, horizontal=FALSE)
GTM <- gvisMerge(GT, M, horizontal=TRUE,tableOptions="bgcolor=\"#CCCCCC\" cellspacing=10")

# Combining multiple plots together
# print(GTM,"chart")
plot(GTM)

# Seeing the HTML code
M <- gvisMotionChart(Fruits, "Fruit", "Year", options=list(width=600, height=400))
print(M)
print(M, 'chart', file='FruitsMotionChart.html')

# For more info
demo(googleVis)

# Quiz
# Q 1. This function plots distance versus speed, each de-meaned and an associated line of slope s.
# Which of the following code will make a manipulate plot that creates a slider for the slope?

library(manipulate)
myPlot <- function(s) {
    plot(cars$dist - mean(cars$dist), cars$speed - mean(cars$speed))
    abline(0, s)
}
manipulate(myPlot(s), s = slider(0, 2, step = 0.1))

# Q 2.Which of the following code uses the rCharts package to create a sortable and searchable data table for the airquality data set? Assume the rCharts package and the airquality data set have already been loaded into R.
dTable(airquality, sPaginationType = "full_numbers")

# Q 4. What is incorrect about the followig syntax in ui.R?
library(shiny)
shinyUI(pageWithSidebar(  
    headerPanel("Data science FTW!"),  
    sidebarPanel(    
        h2('Big text')    
        h3('Sidebar')  
    ),  
    mainPanel(      
        h3('Main Panel text')  
    )
))
# Missing a comma in the sidebar panel

# Q 5.Consider the following code in ui.R And the following server.R code. Why isn't it doing what we want?
# ui.R
shinyUI(pageWithSidebar(  
    headerPanel("Example plot"),  
    sidebarPanel(    
        sliderInput('mu', 'Guess at the mu',value = 70, min = 60, max = 80, step = 0.05,)  ), 
    mainPanel(    
        plotOutput('newHist')  
    )
))

# server.R
library(UsingR)
data(galton)

shinyServer(  
    function(input, output) {    
        output$myHist <- renderPlot({      
            hist(galton$child, xlab='child height', col='lightblue',main='Histogram')      
            mu <- input$mu      
            lines(c(mu, mu), c(0, 200),col="red",lwd=5)      
            mse <- mean((galton$child - mu)^2)      
            text(63, 150, paste("mu = ", mu))      
            text(63, 140, paste("MSE = ", round(mse, 2)))      
        })      }
)

# The server.R output name isn't the same as the plotOutput command used in ui.R.
