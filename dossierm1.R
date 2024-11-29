library(readxl)
library(dplyr)
library(stargazer)
library(lmtest)
library(sandwich)
library(whitestrap)
library(corrplot)

library(plotly)

data = read_excel("C:/Users/hassan/Desktop/econometrie/M1 Econometrie/combined_data.xlsx")
data = data %>% 
  mutate(pibmilliard = pib / 1000000000)



stargazer(as.data.frame(data), 
          type = "text",
          title = "Table: Descriptive statistics of some variables",
          covariate.labels = c("Esperance de vie", "Part urbanistation","Concentration pm2,5", "pib","pib en milliard"))


corrplot(cor(data[-1]), method = 'square',  diag = F, addCoef.col ='black', number.cex = 0.7)


ggplot(data) +
  aes(x = pm25, y = espevie) +
  geom_point(size = 2L, colour = "#B22222") +
  theme_minimal()+
  geom_smooth(method = "lm", se = F, size = 1)



ggplot(data) +
  aes(x = log(pm25), y = log(espevie)) +
  geom_point(size = 2L, colour = "#B22222") +
  theme_minimal()+
  geom_smooth(method = "lm", se = FALSE, size = 1)









modlogl <- lm(log(espevie)~log(pm25), data = data)
summary(modlogl)

ggplot(data) +
  aes(x = log(pm25), y = log(espevie)) +
  geom_point(size = 2L, colour = "#B22222") +
  theme_minimal()+
  geom_smooth(method = "lm", se = FALSE, size = 1)

plot(modlogl, which = 1)  
plot(modlogl, which = 2) 

dwtest(modlogl) #Legere autocorrelation mais pas statistiquement significative
bptest(modlogl)
white_test(modlogl)


modlogl6 <- lm(log(espevie)~log(pm25)+ parturb + log(pib), data = data)
summary(modlogl6)
bptest(modlogl6)
dwtest(modlogl6)
coeftest(modlogl6,vcov=vcovHC(modlogl6,type="HC0"))


modlogl7 <- lm(log(espevie)~log(pm25)+ parturb + log(pibmilliard), data = data)
summary(modlogl7)
bptest(modlogl7)
dwtest(modlogl7)
coeftest(modlogl6,vcov=vcovHC(modlogl6,type="HC0"))

stargazer(modlogl , modlogl6, modlogl7, type = "text", align = TRUE)



corrplot(cor(xxx), method = 'square',  diag = F, addCoef.col ='black', number.cex = 0.7)


plot_ly (data= data,
  x = data$pm25, y = data$espevie,
  type = "scatter",
  mode = "markers",
  text = ~`Country Name`
  )

sapply(data, length)


