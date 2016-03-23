#Lattice plot
library(lattice)
state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4,1))
#Another example
library(lattice)
library(datasets)
xyplot(Ozone ~ Wind, data = airquality)
#Yet another example
#Convert Month to a factor variable
airquality <- transform(airquality, Month = factor(Month))
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5,1))
p <- xyplot(Ozone ~ Wind, data = airquality) #Nothing happens
print(p)

#ggplot

#Control panels
set.seed(10)
x <- rnorm(100)
f <- rep(0:1, each = 50)
y <- x + f - f * x+ rnorm(100, sd = 0.5)
f <- factor(f, labels = c("Group 1", "Group 2"))
## Plot with 2 panels with custom panel function
xyplot(y ~ x | f, panel = function(x, y, ...) {
  # call the default panel function for xyplot
  panel.xyplot(x, y, ...)
  # adds a horizontal line at the median
  panel.abline(h = median(y), lty = 2)
  # overlays a simple linear regression line
  panel.lmline(x, y, col = 2)
})
#1 panel
xyplot(y ~ x, panel = function(x, y, ...) {
  # call the default panel function for xyplot
  panel.xyplot(x, y, ...)
  # adds a horizontal line at the median
  panel.abline(h = median(y), lty = 2)
  # overlays a simple linear regression line
  panel.lmline(x, y, col = 2)
})

#ggplot
library(ggplot2)
str(mpg)
library(ggplot2)
qplot(displ, hwy, data=mpg)
#Classify drv by different colors
qplot(displ, hwy, data=mpg, color = drv)
#Smooth line over points
qplot(displ, hwy, data=mpg, geom=c("point", "smooth"))
#Histograms
qplot(hwy, data=mpg, fill=drv)
#Facets in ggplot is similar to panels in lattice
#left-hand side of the tilt indicates the row variables, and right-hand side indicates the column variables
qplot(displ, hwy, data=mpg, facets = .~drv)
qplot(hwy, data=mpg, facets = drv~., binwidth = 2)
#MAACS data
str(maacs) #MAACS data not available
#Try mpg instead
qplot(displ, hwy, data=mpg, facets = .~drv, geom=c("point", "smooth"))
g <- ggplot(mpg, aes(displ, hwy))
summary(g)
print(g) #No data printed
#Add geom_point()
p <- g + geom_point()
print(p)
g + geom_point()
#Smooth line, default vs. lm
g + geom_point() + geom_smooth()
g + geom_point() + geom_smooth(method="lm")
#Categorize plots by drv
g + geom_point() + geom_smooth(method="lm") + facet_grid(.~drv)
#Adjust appearances, alpha specifies transparency of points
g + geom_point(color = "steelblue", size = 4, alpha = 0.5)
#Color by drv
g + geom_point(aes(color=drv), size = 4, alpha = 0.5)
#Add labels
g + geom_point(aes(color=drv), size = 4, alpha = 0.5) + labs(x = "Cylinder Displacement", y = "Milage/gallon")
#Modify smooth,disable confidence interval (se)
g + geom_point(aes(color=drv), size = 4, alpha = 0.5) + labs(x = "Cylinder Displacement", y = "Milage/gallon")+geom_smooth(size=4, linetype=3,method="lm", se=FALSE)
#Modify font, doesn't work on Windows
g + geom_point(aes(color=drv), size = 4, alpha = 0.5) + labs(x = "Cylinder Displacement", y = "Milage/gallon")+geom_smooth(size=4, linetype=3,method="lm", se=FALSE)+theme_bw(base_family="Times")

#Control axis limit
testdata <- data.frame(x=1:100, y=rnorm(100))
#Introducing outlier
testdata[50,2] <- 100
plot(testdata$x, testdata$y, type="l", ylim =c(-3, 3))
#ggplot
g <- ggplot(testdata, aes(x=x, y=y))
g+ geom_line()
g+ geom_line() + ylim(-3, 3) #limit points satifying the range, outlier is missing
g+ geom_line() + coord_cartesian(ylim=c(-3, 3)) # display all points, but cut off thechart range

#Quiz
#No.2
library(nlme)
library(lattice)
xyplot(weight ~ Time | Diet, BodyWeight)

#No.4
library(lattice)
library(datasets)
data(airquality)
p <- xyplot(Ozone ~ Wind | factor(Month), data = airquality)
print(p)

#No. 7
library(datasets)
data(airquality)
class(airquality$Month)
airquality = transform(airquality, Month = factor(Month))
qplot(Wind, Ozone, data = airquality, facets = . ~ Month)

#No.9
library(ggplot2)
install.packages("ggplot2movies")
library(ggplot2movies)
names(movies)
head(movies)
g <- ggplot(movies, aes(votes, rating))
g+geom_point()
print(g)

#No. 10
qplot(votes, rating, data = movies)
qplot(votes, rating, data = movies) + geom_smooth()
