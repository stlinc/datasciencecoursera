#Download csv file
if(!file.exists("./data")){dir.create("./data/")}
fileUrl1 <- "https://raw.githubusercontent.com/jtleek/modules/master/04_ExploratoryAnalysis/exploratoryGraphs/data/avgpm25.csv"
download.file(fileUrl1, destfile="./data/avgpm25.csv", method="wininet")
pollution <- read.csv("./data/avgpm25.csv", colClasses=c("numeric", "character", "factor", "numeric", "numeric"))
head(pollution)
#Summary
summary(pollution$pm25)
#boxplot
boxplot(pollution$pm25, col = "blue")
#Use colors() to check colors available for plotting.
#Overlaying features
abline(h = 12)
#Histogram
hist(pollution$pm25, col = "green")
#Plot all data points along the histogram axis
rug(pollution$pm25)
#Finer histogram, breaks denotes number of buckets
hist(pollution$pm25, col = "green", breaks = 100)
rug(pollution$pm25)
#Overlaying features, lwd means line width
abline(v = 12, lwd = 2)
abline(v = median(pollution$pm25), col = "magenta", lwd = 2)
#Bar plot
barplot(table(pollution$region), col = "wheat", main = "Number of Counties in Each Region")
#Multiple boxplots
boxplot(pm25 ~ region, data=pollution, col = "red")
#Multiple histograms
#Define plot parameters: par
#mfrow: A vector of the form c(nr, nc). Subsequent figures will be drawn in an nr-by-nc array on the device by columns (mfcol), or rows (mfrow), respectively.
#mar: A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot. 
par(mfrow = c(1,1), mar = c(4,4,2,1))
hist(subset(pollution, region == "east")$pm25, col = "green")
hist(subset(pollution, region == "west")$pm25, col = "green")
#Scatter plot
with(pollution, plot(latitude, pm25))
#identical plot as with, but the labels are different.
plot(pollution$latitude, pollution$pm25)
#lty: line type, 2 means dashed
abline(h=12, lwd=2, lty=2)
with(pollution, plot(longitude, pm25))
abline(h=12, lwd=2, lty=2)
#Color by regions
with(pollution, plot(latitude, pm25, col = region))
abline(h=12, lwd=2, lty=2)
#Multiple scatter plots
par(mfrow = c(1,1), mar = c(5,4,2,1))
with(subset(pollution, region == "west"), plot(latitude, pm25, main = "West"))
with(subset(pollution, region == "east"), plot(latitude, pm25, main = "East"))

#Base plot
library(datasets)
data(cars)
with(cars, plot(speed, dist))

#Lattice plot
library(lattice)
state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4,1))

#ggplot2
library(ggplot2)
data(mpg)
qplot(displ, hwy, data=mpg)
#Checking mpg depenecy on engine drive (4WD, front-, rear-drive)
xyplot(hwy ~ displ | mpg$drv, data = mpg, layout = c(3,1))
#Base plot
library(datasets)
hist(airquality$Ozone)
with(airquality, hist(Ozone))
with(airquality, plot(Wind,Ozone))
airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab= "Ozone(ppb)")
#Check defaults or current settings for plot parameters
par("lty")
#[1] "solid"
par("lwd")
#[1] 1
par("col")
#[1] "black"
par("pch")
#[1] 1
par("mar")
par("mfrow")
par("bg","mfcol")
#Adjusting plots
with(airquality, plot(Wind, Ozone))
title(main="Ozone and Wind in New York City")
#Plot May data in blue color
with(airquality, plot(Wind, Ozone, main="Ozone and Wind in New York City"))
with(subset(airquality, Month==5), points(Wind, Ozone, col = "blue"))
#Plot with 2 data sets with different colors
with(airquality, plot(Wind, Ozone, main="Ozone and Wind in New York City", type = "n"))
with(subset(airquality, Month==5), points(Wind, Ozone, col = "blue"))
with(subset(airquality, Month!=5), points(Wind, Ozone, col = "red"))
legend("topright", pch = 1, col=c("blue", "red"), legend = c("May", "Other Months"))
#Plotting with linear regression model
with(airquality, plot(Wind, Ozone, main="Ozone and Wind in New York City", pch = 20))
model <- lm(Ozone ~ Wind, airquality)
abline(model, lwd = 2)
#Multiple plots
par(mfrow=c(1,2))
with(airquality, {
  plot(Wind, Ozone, main = "Ozone and Wind")
  plot(Solar.R, Ozone, main="Ozone and Solar Radiation")
})
#Mutiple plots with main title
par(mfrow=c(1,3), mar=c(4,4,2,1), oma= c(0,0,2,0))
with(airquality, {
  plot(Wind, Ozone, main = "Ozone and Wind")
  plot(Solar.R, Ozone, main="Ozone and Solar Radiation")
  plot(Temp, Ozone, main="Ozone and Temperature")
  mtext("Ozone and Weather in New York City", outer=TRUE)
})
#Find examples of points
example(points)

#Print devices
?Devices
## Create plot on screen device
with(faithful, plot(eruptions, waiting))
## Add a main title
title(main = "Old Faithful Geyser data")
#Plot to a PDF file
pdf(file = "myplot.pdf")
with(faithful, plot(eruptions, waiting, title(main = "Old Faithful Geyser data")))
dev.off() #Close PDF device
## Copy my plot (plot on current device) to a PNG file
dev.copy(png, file = "ozone.png")
## Don't forget to close the PNG device!
dev.off()
