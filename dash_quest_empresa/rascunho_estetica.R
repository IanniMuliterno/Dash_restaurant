# Libraries
library(ggplot2)
library(dplyr)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/3_TwoNumOrdered.csv", header=T)
data$date <- as.Date(data$date)

# Plot
data %>%
  tail(10) %>%
  ggplot( aes(x=date, y=value)) +
  geom_line() +
  geom_point()

# Plot
library(hrbrthemes)
data %>%
  tail(10) %>%
  ggplot( aes(x=date, y=value)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  theme_ipsum() +
  ggtitle("Evolution of bitcoin price")



#################################################