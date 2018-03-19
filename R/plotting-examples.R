# install.packages("corplot")
# install.packages("forecast")
# install.packages("ggplot2")

data("iris")
iris[1:10,]
summary(iris)

# Let's focus on analyzing the numeric columns
numericColumns <- c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width')
plot(iris[, numericColumns])

# This plot is great for visual purposes, but what if we want to numerically summarize the pairwise interactions?
cor(iris[, numericColumns])
# Default correlation is Pearson. Kendall and Spearman are also available.
cor(iris[, numericColumns], method = 'kendall')
cor(iris[, numericColumns], method = 'spearman')

# What if we want to visualize these correlation matrices?
library(corrplot)
# Set plot space to have three rows and one column of plots
par(mfrow = c(1, 3))
corrplot(cor(iris[, numericColumns]), main = 'Pearson Correlation')
corrplot(cor(iris[, numericColumns], method = 'kendall'), main = 'Kendall Correlation')
corrplot(cor(iris[, numericColumns], method = 'spearman'), main = 'Spearman Correlation')

data("PlantGrowth")
boxplot(weight ~ group, data = PlantGrowth)

library(forecast)
data("USAccDeaths")
plot(USAccDeaths)
plot(decompose(USAccDeaths))
fitAccDeaths <- auto.arima(USAccDeaths)
plot(forecast(fitAccDeaths))
# Under the hood "plot" is calling the unexported plot method for plotting objects of class "forecast"
forecast:::plot.forecast(forecast(fitAccDeaths))

library(ggplot2)
p <- autoplot(forecast(fitAccDeaths))
p
# Notice the title in the previous plot is left justified instead of centered. Let's fix that.
p + theme(plot.title = element_text(hjust = 0.5))
# Now what if we want our plot to be dark?
p +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_dark()
# The plot is now dark, but our title reverted... Try reordering the calls.
p +
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5))

