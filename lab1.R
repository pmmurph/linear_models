library(tidyverse)
library(car)
library(broom)

#play ground for linear models with numeric response

glimpse(mpg)

attach(mpg)

qplot(hwy,model, col=factor(year), size=displ)
mod1 <- lm(price~carat)
coef(mod1)

points(carat,fitted(mod1), col= 'red')


mod2<- lm(price~carat + I(carat^2))
coef(mod2)

points(carat, fitted(mod2), col='green')


s<- seals %>%
  mutate(lat2= lat+delta_lat,
         long2= long+delta_long) %>% 
  mutate(len= sqrt( (lat-lat2)^2 + (long-long2)^2 ))
  

ggplot()+
  geom_segment(data= s, aes(x= long, y= lat, xend= long2, yend= lat2, col=len),
               arrow = arrow(angle= 20,type='closed',length = unit(1.5, 'mm')), size=.5)


