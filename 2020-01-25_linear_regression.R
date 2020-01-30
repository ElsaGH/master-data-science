library(dplyr)
advertising <- read.csv('advertising.csv', sep = ';', header = T, fileEncoding = 'utf-8')
glimpse(advertising)
summary(advertising)
#sublimetext.com para ver archivos de texto plano

pairs(advertising, col = 'red')

lm_fit_sales_TV <- lm(Sales ~ TV, data = advertising)

lm_fit_sales_TV

summary(lm_fit_sales_TV)

names(lm_fit_sales_TV)

confint(lm_fit_sales_TV, level = 0.95)

new_advertising <- data.frame(TV = c(100, 150, 200, 250))
predicted_values <- predict(lm_fit_sales_TV, new_advertising, interval = 'confidence')
new_advertising <- cbind(new_advertising, predicted_values)
new_advertising

plot(advertising$TV, advertising$Sales, type = 'p', col = 'red', xlab = 'TV', ylab = 'Sales')
abline(lm_fit_sales_TV, col = 'blue')

plot(advertising$TV, lm_fit_sales_TV$residuals, type = 'p', col = 'red', xlab = 'TV', ylab = 'Sales')
#puede ser que la media sea 0, no vemos varianza constante, depende de las X, a valores altos
#hay más error, va a predecir mejor con valores bajos
#buscaríamos una forma de mejorar el modelo eg, regresión múltiple



auto <- read.csv('auto.csv', sep = ';', header = T, fileEncoding = 'utf-8')
View(auto)
summary(auto)
str(auto)

pairs(auto, col="red")

cor(x=auto$horsepower, y=auto$mpg)

lm_fit_mpg_horsepower <- lm(mpg ~ horsepower, data = auto)
lm_fit_mpg_horsepower

summary(lm_fit_mpg_horsepower)
confint(lm_fit_mpg_horsepower, level = 0.95)


new_auto <- data.frame(horsepower = 98)
predicted_values2 <- predict(lm_fit_mpg_horsepower, new_auto, interval = 'confidence', level=0.99)
predicted_values2

plot(auto$horsepower, auto$mpg, type = 'p', col = 'red', xlab = 'horsepower', ylab = 'mpg')
abline(lm_fit_mpg_horsepower, col = 'blue')

plot(auto$horsepower, lm_fit_mpg_horsepower$residuals, type = 'p', col = 'red', 
     xlab = 'horsepower', ylab = 'mpg')



pairs(auto, col="red")

par(mfrow = c(1,2))
plot(auto$mpg, lm_fit_mpg_horsepower$fitted.values, type = 'p', col = 'red', 
     xlab = 'mpg', ylab = 'Predicted mpg')
plot(auto$mpg, lm_fit_mpg_horsepower$residuals, type = 'p', col = 'red', 
     xlab = 'mpg', ylab = 'Residuals')

