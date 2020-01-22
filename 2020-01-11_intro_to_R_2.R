install.packages(c("devtools",
                   "dplyr",
                   "xlsx",
                   "tidyverse",
                   "readr",
                   "tidyr",
                   "data.table",
                   "dslabs",
                   "rvest"),dependencies = T)

install.packages("pryr")
library(pryr)

parenvs(all=TRUE)
x<-5
where("x")
where("mean")
new<-"hello global"
new<-"hello active"

roll <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE)
  sum(dice)
  return(environment)
}
roll() #returns the environment where I am
die<-1:2
roll()


play <- function() {
  # step 1: generate symbols
  symbols <- get_symbols()
  # step 2: display the symbols
  print(symbols)
  # step 3: score the symbols
  score(symbols)
}

get_symbols <- function() {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  sample(wheel, size = 3, replace = TRUE, 
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}
get_symbols()

symbols<-get_symbols()
symbols
symbols[1]==symbols[2] & symbols[1] == symbols[3]

symbols<-c("7","7","7")
symbols[1] == symbols[2] & symbols[2] == symbols[3]
## TRUE
symbols[1] == symbols[2] & symbols[1] == symbols[3]
## TRUE
all(symbols == symbols[1])
## TRUE

length(unique(symbols) == 1)

symbols<-get_symbols()
symbols
symbols %in% c("B","BB","BBB")
all(symbols %in% c("B","BB","BBB"))

num<-(-1)
if (num < 0) {
  num <- num * -1
}
num
num<-2
num

num <- -1
if (num < 0) {
  print("num is negative.")
  print("Don't worry, I'll fix it.")
  num <- num * -1
  print("Now num is positive.")
}
## "num is negative."
## "Don't worry, I'll fix it."
## "Now num is positive."
num
## 1


a <- 3.14
dec <- a - trunc(a)
dec
if (dec >= 0.5) {
  a <- trunc(a) + 1
} else {
  a <- trunc(a)
}
a
## 3

a <- 1
b <- 1
if (a > b) {
  print("A wins!")
} else if (a < b) {
  print("B wins!")
} else {
  print("Tie.")
}
## "Tie."

symbols <- get_symbols() 
same<-symbols[1]==symbols[2] & symbols[2]==symbols[3]
bars<-symbols %in% c("BBB","BB","B")
if(same & symbols[1]!=0){
    print("prize is 10 or over")
}else if(bars){
    print("prize is 5)")
}else{
    print("prize is 0")
}
  

score <- function (x) {
  # identify case
  same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
  bars <- symbols %in% c("B", "BB", "BBB")
  
  # get prize
  if (same & symbols[1]!=0) {
    payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, 
                 "B" = 10, "C" = 10, "0" = 0)
    prize <- unname(payouts[symbols[1]])
  } else if (all(bars)) {
    prize <- 5
  } else {
    cherries <- sum(symbols == "C")
    if(cherries==2){
      prize<-5
    }else if(cherries==1){
      prize<-2
    }else{
      prize<-0
    }
  }
  
  # adjust for diamonds
  diamonds <- sum(symbols == "DD")
  prize * 2 ^ diamonds
}
score(symbols)


play<-function(){
  symbols<-get_symbols()
  print(symbols)
  print(score(symbols))
}
play()


wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
combos <- expand.grid(wheel, wheel, wheel, stringsAsFactors = FALSE)
View(combos)

prob <- c("DD" = 0.03, "7" = 0.03, "BBB" = 0.06, 
          "BB" = 0.1, "B" = 0.25, "C" = 0.01, "0" = 0.52)

combos$prob1 <- prob[combos$Var1]
combos$prob2 <- prob[combos$Var2]
combos$prob3 <- prob[combos$Var3]
head(combos, 3)

combos$prob <- combos$prob1 * combos$prob2 * combos$prob3
sum(combos$prob)

for (i in 1:nrow(combos)) {
  symbols <- c(combos[i, 1], combos[i, 2], combos[i, 3])
  combos$prize[i] <- score(symbols)
}
head(combos)

sum(combos$prize * combos$prob)


library(dplyr)
library(xlsx)
library(readr)
library(tidyr)
library(data.table)
library(tidyverse)
library(dslabs)
library(devtools)
library(rvest)


?read.delim
data<-read.delim("DataSets/DataSets/murders.csv")
head(data)
typeof(data)
dim(data)
data<-read.delim("DataSets/DataSets/murders.csv",sep=",",stringsAsFactors=F)
head(data)
dim(data)
data<-read.csv("DataSets/DataSets/murders.csv",stringsAsFactors=F)
head(data)
dim(data)
data2<-read.xlsx("DataSets/DataSets/murders.csv",sheetIndex=1)
data3<-read.table("DataSets/DataSets/murders.csv",stringsAsFactors=F,sep=",",header=T)
head(data3)
dim(data3)

install.packages("fread")
library(fread)
counts.rnaseq<-fread("DataSets/DataSets/Counts.genes.DiffAll.genes.limma.random.txt",
                     sep="\t",showProgress = TRUE)
class(counts.rnaseq)

dat3=read.table(file = "DataSets/DataSets/murders.csv",header = T,sep=",")
head(dat3)


class(dat3)
class(data)
class(data.tidy)
head(data.tidy)

tibble(x = letters)
tibble(x=1,y=list(1:5,1:10,1:20))
names(tibble("old alphabet"=letters))
names(data.frame("old alphabet"=letters))

tibble(x=1:5,y=x^2)
data.frame(z=1:5,y=z^2)


df <- data.frame(abc = 1)
df$a
df2 <- tibble(abc = 1)
df2$a

library(dslabs)
data(gapminder)
?gapminder
View(gapminder)
str(gapminder)

gapminder$infant_mortality[gapminder$country=="Sri Lanka" & gapminder$year==2015]
gapminder$infant_mortality[gapminder$country=="Turkey" & gapminder$year==2015]

gapminder$infant_mortality[gapminder$country=="Poland" & gapminder$year==2015]
gapminder$infant_mortality[gapminder$country=="South Korea" & gapminder$year==2015]

gapminder$infant_mortality[gapminder$country=="Pakistan" & gapminder$year==2015]
gapminder$infant_mortality[gapminder$country=="Vietnam" & gapminder$year==2015]

# %>% avoid generating intermediate objects (similar to pip | )

gapminder %>% filter(year==2015 & country %in% c("Sri Lanka","Turkey"))
gapminder %>% filter(year==2015 & country %in% c("Sri Lanka","Turkey")) %>%
  select(country,year,infant_mortality)
gapminder %>% filter(year==2015 & country %in% c("Sri Lanka","Turkey")) %>%
  select(-life_expectancy,-fertility)


library(ggplot2)
years<-c(1962,1980,1990,2000,2012)
continents<-c("Europe","Asia")
gapminder %>% 
  filter(year %in% years & continent %in% continents)

gapminder %>% 
  filter(year %in% years & continent %in% continents) %>% 
  ggplot(aes(fertility,life_expectancy,col=continent)) + geom_point() + 
  facet_wrap(~year)


wide_data<-read_csv("DataSets/DataSets/fertility-two-countries-example.csv")
head(wide_data)         
View(wide_data)
class(wide_data)
?gather


new_tidy_data <- wide_data %>%
  gather(year, fertility, `1960`:`2015`)
View(new_tidy_data)


url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"

