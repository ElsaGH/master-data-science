rm(list=ls())
data("mtcars")
View(mtcars)
str(mtcars)

#Q1 cars with 4 cylindres
length(which(mtcars$cyl==4))
sum(mtcars$cyl==4)
rownames(mtcars)[mtcars$cyl==4]

#Q2 cars with more than 4 cyl
rownames(mtcars)[mtcars$cyl>4]

#Q3 of those with more than 4 cyl, how many and which ones are automatic
sum(mtcars$cyl>4 & mtcars$am==0)
rownames(mtcars)[mtcars$cyl>4 & mtcars$am==0]

#Q4 average consumption of those with more than 4 cyl and automatic
mtcars$mpg[mtcars$cyl>4 & mtcars$am==0]
mean(mtcars$mpg[mtcars$cyl>4 & mtcars$am==0])

#Q5 and of those that are manual?
mean(mtcars$mpg[mtcars$cyl>4 & mtcars$am==1])

#Q6 draw boxplot to see if the difference between automatic and manual is significant
boxplot(mtcars$mpg ~ factor(mtcars$am))
boxplot(mpg ~ factor(am), data=mtcars[mtcars$cyl>4,])
