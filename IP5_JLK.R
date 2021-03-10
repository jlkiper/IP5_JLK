#Individual Project 4
#Author: Jaylon Kiper
#Version: 1.0
#Semester: Spring 2021
#Summary:
#The goal of this project is to use R and TidyVerse to clean up a data set and 
#explore it. Essentially you are re-creating your Pandas analysis in R.

library(datasets)
library(tidyverse)
library(corrplot)
library(ggplot2)
library(dplyr)
library(psych)

#Reading the CSV File.
p <- read.csv("Video_Games_Sales_2016.csv")

#Grabbing all the column names.
colnames(p)

#Change "i..Name" to "Name"
names(p)[1]<-"Name"

#Checking the data types for each column.
sapply(p, class)

#Checking basic summary statistics with the summary function.
summary(p)

#Correlation Martix/Heatmap for all continuous variables.
cor(p[,c("NA_Sales","EU_Sales","JP_Sales","Other_Sales","Global_Sales",
         "Critic_Score","Critic_Count","User_Score","User_Count")])


p.cor = cor(p[,c("NA_Sales","EU_Sales","JP_Sales","Other_Sales","Global_Sales",
                 "Critic_Score","Critic_Count","User_Score","User_Count")])

corrplot(p.cor)

#Value count platforms, years, genres, publishers, developers, and ratings.
count(p, vars = Platform)

count(p, vars = Year_of_Release)

count(p, vars = Genre)

count(p, vars = Publisher)

count(p, vars = Developer)

count(p, vars = Rating)

#Bar chart showing the distinct count for each platform per game.
platcount <- table(p$Platform)
barplot(platcount, main = "Platform Count", xlab = "Gaming Platform", col = c("blue"))

#Horizontal bar chart showing the distinct count for each genre per game.
gencount <- table(p$Genre)
barplot(gencount, main="Genre Count", horiz = TRUE, ylab = "Genre Type", col = c("green"))

#Distribution plot with the critic scores per game.
hist(p$Critic_Score, main = "Average Critic Score",col = "red")

#Distribution plot with the user scores per game.
hist(p$User_Score, main = "Average User Score",col = "yellow")

#Pairplot comparing the relationships between all games sales by genre.
pairs(p[6:10], main = "Sales Pairplot", 
      bg = c("red","coral", "orange", "yellow",
      "green", "darkgreen", "blue", "darkblue", 
      "purple", "black", "saddlebrown", "magenta")[p$Genre],
      pch = 21
      )

#Queries used to grab each game by their rating.
Everyone <- (p$Rating == "E")
E10 <- (p$Rating == "E10+")
Teen <- (p$Rating == "T")
Mature <- (p$Rating == "M")

#Reading the average sales, score, and count for each game by rating.
aggregate(p[6:10], list(Everyone), mean)
aggregate(p[6:10], list(E10), mean)
aggregate(p[6:10], list(Teen), mean)
aggregate(p[6:10], list(Mature), mean)

#Barplot with with ratings count
ratcount <- table(p$Rating)
barplot(ratcount, main = "Rating Count", xlab = "ESRB Rating", col = c("purple"))


#Dr. Kelley, I tried running the scatter subplot from my last assignment,
#it gave me blanks for all the Critic and User score values. This last barplot
#replaces the countplot since it is apart of Seaborn.