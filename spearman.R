# Spearman rank stuff
library(plotly)
library(dplyr)
library(ggplot2)
library(magrittr)

# some faffing around with pipe operators
iris %>% head
iris$Sepal.Length %<>% sqrt


static <- mtcars %>%
  ggplot(aes(x = qsec, y = disp, color = factor(gear))) +
  geom_point()

ggplotly(static)

#test change again aaan 
 

# Plan: Create a plot that shows how correlation and p-values change with changing data.
# Idea for spearman: create dropdown menu to select your data type, e.g. the degree variance in the data
