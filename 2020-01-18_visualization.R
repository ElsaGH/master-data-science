library(dslabs)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readr)

data(murders)
View(murders)
plot(murders$total,murders$population/10^5)
plot(murders$total,murders$population/10^5,pch="*")
plot(murders$total,murders$population/10^5,pch="*",col="red")
par(new=T)
plot(murders$total,murders$population/10^5,pch="o")

plot(murders$total,murders$population/10^5,pch="*",col="red")
points(murders$total,murders$population/10^5,lty=2)
abline(a=0,b=1,lty=2)
hist(murders$total)

murders$total.bin=murders$total
murders$total.bin[which(murders$total<=200)]=200
murders$total.bin[which(murders$total>200 & murders$total<=400)]=400
murders$total.bin[which(murders$total>400 & murders$total<=600)]=600
murders$total.bin[which(murders$total>600 & murders$total<=800)]=800
murders$total.bin[which(murders$total>800 & murders$total<=1000)]=1000
murders$total.bin[which(murders$total>1000 & murders$total<=1200)]=1200
murders$total.bin[which(murders$total>1200 & murders$total<=1400)]=1400
table(murders$total.bin)

murders$total.bin=murders$total
for (i in 1:7){
  murders$total.bin[which(murders$total>200*(i-1) & murders$total<=200*i)]=200*i
}
table(murders$total.bin)

hist(murders$total,breaks=10)
hist(murders$total,freq = F)

boxplot(murders$total)
summary(murders$total)
boxplot(murders$total~murders$region)
murders.west<-filter(murders,region=="West")
murders.west[which.max(murders.west$total),]

babies=read.delim("babies.txt",header=T,sep="\t",stringsAsFactors = F)
View(babies)
plot(babies$bwt~babies$gestation)

babies$gestation[which(babies$gestation==999)]=NA
plot(babies$bwt,babies$gestation)

boxplot(babies$bwt~babies$smoke)

hist(babies$bwt)

mean(babies$bwt)
sd(babies$bwt)

hist(murders$total)
mean(murders$total)
sd(murders$total)

median(murders$total)
IQR(murders$total)

summary(murders$total)
summary(babies$bwt)

summary(murders$total)
q1=quantile(murders$total, p=0.25)
q1
q3=quantile(murders$total, p=0.75)
q3
iqr=(q3-q1)
iqr
r <- c(q1 - 1.5*iqr, q3 + 1.5*iqr)
r

which(murders$total<=r[1])
which(murders$total>=r[2])
murders[which(murders$total>=r[2]),]

r2 <- c(q1 - 3*iqr, q3 + 3*iqr)
r2
which(murders$total>=r2[2])
murders[which(murders$total>=r2[2]),]

qqnorm(murders$total)
qqline(murders$total)
qqnorm(babies$bwt)
qqline(babies$bwt)

mad(babies$bwt)
mad(murders$total)


library(dslabs)
data(heights)
head(heights)
str(heights)
View(heights)

s <- heights %>% 
  filter(sex == "Male") %>%
  summarize(average = mean(height), standard_deviation = sd(height))
s
str(s)

s <- murders %>%
  summarize(median = median(total), mad=mad(total),min=min(total),max=max(total))
s
str(s)

s <- murders %>% 
  mutate(rate=total/population*100000) %>%
  summarize(mean(rate))
s
str(s

s <- murders %>% 
      summarize(rate=mean(total)/mean(population)*100000) %>%
      .$rate
s    

babies.new<-babies %>% 
  select(bwt, smoke) %>% 
  group_by(smoke)  
str(babies.new)
babies.new


babies %>% 
  select(bwt, smoke) %>% 
  group_by(smoke)  %>% 
  summarize(mean(bwt))    

murders %>% arrange(population) %>% head()

murders %>% mutate(rate=total/population*100000)%>%
  arrange(rate) %>% 
  head()

murders %>% mutate(rate=total/population*100000)%>%
  arrange(total,rate) %>% 
  head()

murders %>% mutate(rate=total/population*100000)%>%
  arrange(desc(rate)) %>%
  top_n(10)



p <-  murders %>% ggplot(aes(population/10^6, total, label = abb)) +   
  geom_text(nudge_x = 0.05) + 
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") + 
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")+
  geom_point(aes(col=region), size = 3)
p

ggplot(data = murders)

murders %>% ggplot()

p <- ggplot(data = murders)
class(p)
print(p)
p

murders %>% ggplot() + 
  geom_point(aes(x = population/10^6, y = total))

murders %>% ggplot(aes(x = population/10^6, y = total)) + 
  geom_point()

p<-murders %>% ggplot(aes(x = population/10^6, y = total))

p+geom_point(aes(x = population/10^6, y = total))

p+geom_point(aes(x = population/10^6, y = total))+
  geom_text(aes(x = population/10^6, y = total,label=state))

p+geom_point(aes(x = population/10^6, y = total))+
  geom_text(aes(x = population/10^6, y = total,label=abb))

murders %>% 
  ggplot(aes(x = population/10^6, y = total,label=abb))+
  geom_point()+
  geom_text(col="red")

murders %>% ggplot(aes(x = population/10^6, y = total,label=abb))+
  geom_point()+
  geom_text(aes(col="red"))+
  geom_label(aes(x=10,y=800,label="Hello"))

p<-murders %>% ggplot()
p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb))

p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb),nudge_x=1)

p<- murders %>% ggplot(aes(x=population/10^6,y=total,label=abb))
p + geom_point(size = 3) +  
  geom_text(nudge_x = 0.1) + 
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") 

p + geom_point(size = 3) +  
  geom_text(nudge_x = 0.05) + 
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") + 
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")

p<- murders %>% 
  ggplot(aes(x=population/10^6,y=total,label=abb))+
  geom_point(size = 3) +
  geom_text(nudge_x = 0.05) + 
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") + 
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")

p+geom_point(aes(col=region),size=3)


murders %>% 
  ggplot(aes(x = population/10^6, y = total,label=abb))+
  geom_point()+
  geom_text(aes(col="red"))+
  geom_label(aes(x=10,y=800,label="Hello"))

r <- murders %>% 
  summarize(rate = sum(total) /  sum(population) * 10^6) %>% .$rate

p + geom_point(aes(col=region), size = 3) + 
  geom_abline(intercept = log10(r))

p <- p + 
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col=region), size = 3) 
p

install.packages("ggrepel")
install.packages("ggthemes")
library(ggrepel)
library(ggthemes)

p + theme_economist()

murders %>% ggplot(aes(population/10^6, total, label = abb)) +   
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col=region), size = 3) +
  geom_text_repel() + 
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") + 
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") + 
  scale_color_discrete(name = "Region") +
  theme_economist()

p <- heights %>% filter(sex=="Male") %>% ggplot(aes(x = height)) 
p1 <- p + geom_histogram(binwidth = 1, fill = "blue", col="black")
p2 <- p + geom_histogram(binwidth = 2, fill = "blue", col="black")
p3 <- p + geom_histogram(binwidth = 3, fill = "blue", col="black")

p
p1
p2
p3

