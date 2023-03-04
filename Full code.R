# Data Extraction from web 
library(rvest)
library(dplyr)
online_data <- "https://prsindia.org/budgets/states/karnataka-budget-analysis-2022-23#:~:text=sources%3A%20Karnataka%20Budget%20Document%202022%2D23%3B%20PRS.,-Expenditure%20in%202022&text=Revenue%20expenditure%20in%202022%2D23,pensions%2C%20interest%2C%20and%20subsidies."
online_data<- read_html(online_data)
View(online_data)
table <- online_data%>% html_nodes("table")%>%.[6]%>%html_table()%>%.[[1]]
View(table)

# Data cleaning 
colnames(table)<-table[1,]
table <- table [-1,]
library(janitor)
clean_names(table,case = 'snake')
colnames(table)

# Converting as numeric 
table$2020-21 Actuals<- as.numeric(table$2020-21 Actuals)
str(table$`2020-21 Actuals`)

colnames(table)
# Diagrams to capture the budget allocation 
ggplot(table, aes(x=Sectors, y= 2020-21 Actuals)) +
  geom_segment(aes(x=Sectors, xend=Sectors, y=2020-21 Actuals, yend= 2020-21 Actuals), color="green") +
  geom_point( color="green", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+ ggtitle('Lolipop chart',subtitle = "Budget")+
  theme(plot.title = element_text(colour = 'red',face = 'bold',size=10))


# Extraction of data
library(dplyr)
library(rvest)
money_control<- "https://www.moneycontrol.com/mutual-funds/icici-prudential-bluechip-fund/portfolio-overview/MPI392"
moneycontrol_page<- read_html(money_control)
stock_table<- moneycontrol_page %>% html_nodes("table") %>%.[4] %>%html_table() %>%.[[1]]
View(stock_table)
library(ggplot2)
ggplot(stock_table, aes(x = '', Value(Mn), fill = Sector)) + 
  geom_line() + 
  geom_text(aes(label = paste0(Value(Mn), "%")), position = position_stack(vjust = 0.5)) +
  coord_polar('y', start = 0) + 
  theme(legend.position='bottom')

fortify.zoo(stock_table)
View(stock_table)
which(is.na(stock_table))

ggplot(data = stock_table,aes(x=Sector, y=Quantity)) + 
  xlab('sector')+ylab('Quantity')+
  geom_bar(stat = 'identity',color = 'blue')+
  ggtitle('preparing for shatesh exam')+
  theme(plot.title = element_text(colour = 'red',face = 'bold',size=10))+coord_polar()



library(rvest)
library(dplyr)
library(ggplot2)
data <- "https://www.moneycontrol.com/mutual-funds/sbi-long-term-equity-fund-direct-plan/portfolio-overview/MSB499"
data1<- read_html(data)
View(data1)
table <- data1%>% html_nodes("table")%>%.[5]%>%html_table()%>%.[[1]]
View(table)
library(janitor)
table <- clean_names(table)
ggplot(table,aes(x=sector,y= sector_total))+ geom_bar(stat = 'identity')

devtools::install_github('thomasp85/gganimate')



p <- ggplot(table, aes(x=as.factor(id), y=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("blue", 0.3)) +
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-100,120) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm")     # This remove unnecessary margin around plot
  ) +
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0)
p

setwd("C:\\Users\\samhi\\Downloads\\End Semester")
library(readxl)
Cars <- read_excel("C:\\Users\\samhi\\Downloads\\End Semester\\R programming\\car_data.xlsx")
View(Cars)
str(Cars)
head(Cars)
summary(Cars)

# Setting Dummy variables 
Cars$Fuel_Type <- ifelse(Cars$Fuel_Type == "Petrol",1,0)
Cars$Transmission <- ifelse(Cars$Transmission == "Manual",1,0)
Cars$Seller_Type <- ifelse(Cars$Seller_Type == "Individual",1,0)
Cars<- transform(Cars, Car_Age=(2023-(Cars$Year)))

# Regression 
library(lmtest)
Regression <- lm(Present_Price ~ Car_Age+Selling_Price+Kms_Driven+Fuel_Type+Seller_Type+Transmission+Owner,Cars)
print(Regression)
summary(Regression)
library(jtools)
summ(Regression)

# To check for outlier 
  # If there are numbers then they are the outliers
library(car)
outlierTest(Regression)
#To check for heteroskedasticity 
  # Null - No Heteroskedasticity
bptest(Regression)
#To check for autocorrelation 
  # No serial correlation 
dwtest(Regression)
# Testing for normality (Shapiro-Wilk test)
   #Null - normally distributed
shapiro.test(residuals(Regression))

# Exporting to word doc 

library(textreg)
library(stargazer)
stargazer(Cars,type ="text",title ="Regression_Result",digits =2,out='tablel.text')
stargazer(Regression,type ="text",title ="Regression_Result",digits =2,out='tablel.text')


#   Regression Trial 
library(readxl)
heart_data <- read.csv("C:\\Users\\samhi\\Downloads\\End Semester\\R programming\\heart.data.csv")
View(heart_data)

rownames(heart_data)<- heart_data[,1]
heart_data<- heart_data[,-1]
library(lmtest)
Reg <- lm(heart_disease~ biking+smoking,heart_data)
summary(Reg)
summ(Reg)

# Diagnostics checking 
library(car)
outlierTest(Reg)
bptest(Reg)
library(car)
vif(Reg)


#ARIMA Model 
# Import data 
# library tseries and forecast
#stationarity test 
#best fit model 
#create the model 
#diagnostic check 
library(readxl)
exdata <- read_excel("C:\\Users\\samhi\\Downloads\\End Semester\\R programming\\ARIMA.xlsx")
View(exdata)
rownames(exdata)<- exdata[,1]
library(tseries)
library(forecast)
View(exdata)
# Set as timeseries data 
exdata.ts=ts(exdata,start = c(2007,10),frequency = 12)
View(exdata.ts)
colnames(exdata.ts)
adf.test(exdata,ts)





# Cointegration 
coint <- read_excel("C:\\Users\\samhi\\Downloads\\End Semester\\R programming\\coint.xlsx")
View(coint)
library(tseries)
lr.ts <- ts(data$LR,start = c(2000,1),frequency = 12)
sr.ts <- ts(data$SR,start = c(2000,1),frequency = 12)
ggplot()+geom_line(data = data, aes(Month, LR), color = 'blue') + geom_line(data = data, aes(Month, SR), color = 'red')
# Checking for stationarity 
adf.test(lr.ts)
adf.test(diff(lr.ts))
adf.test(sr.ts)
adf.test(diff(sr.ts))

library(urca)
adfsr =ur.df(sr.ts,type = "trend",selectlags ="AIC")
summary(adfsr)
adflr =ur.df(lr.ts,type = "trend",selectlags ="AIC")
summary(adflr)


# step of Cointegration 
library(lmtest)
cointegration <- lm(lr.ts~sr.ts)
summary(cointegration)
summ(cointegration)
residual<-cointegration$residuals
plot.ts(residual)
adf.test(residual)
adfres =ur.df(residual,type = "trend",selectlags ="AIC")
summary(adfres)

library(aTSA)
coint.test(lr.ts,sr.ts,d=1)

# VECM 

vecm<-read.csv("C:\\Users\\samhi\\Downloads\\End Semester\\R programming\\VECM_1.csv")
View(vecm)
colnames(vecm)
vecm<-vecm%>%clean_names(case="snake")
library(tseries)
gdp<-ts(vecm$ln_gdp, frequency = 4, start= c(2003,1,31) )
cpi<-ts(vecm$ln_cpi, frequency = 4, start= c(2003,1,31) )
m3<-ts(vecm$ln_m3, frequency = 4, start= c(2003,1,31))

# Creating a data frame 
df1<-data.frame(gdp, cpi, m3)
# Plotting using TSstudio package
library(TSstudio)
ts_plot(gdp, title="GDP")
ts_plot(cpi, title="CPI")
ts_plot(m3, title="M3")

# Checking for stationarity 
pp.test(gdp)
pp.test(cpi)
pp.test(m3)

# Select the number of lags 
library(vars)
lagselect = VARselect(df1, lag.max=10)
lagselect$selection

#Joansen test 
cointest1 <- ca.jo(df1, type = "trace", K = 4, ecdet = "const")
summary(cointest1)

cointest2 <- ca.jo(df1, type = "eigen", K = 4, ecdet = "const")
summary(cointest2)

# Interpretation 
#Need to look at first table with test, 10pct, 5pct, 1pct
#r = number of cointegrating relationships
#test lower than 10pct, 5pct --> There exists either 1/2 cointegrating relationships
#Null hypothesis - There is atleast
#VECM(data, lags, r=, estim ="")
library(tsDyn)
vecm1 = VECM(df1, 4, r = 1, estim="2OLS")
summary(vecm1)

# Vector Autoregression
library(readxl)
data_VAR <- read_excel("C:\\Users\\samhi\\Downloads\\End Semester\\R programming\\VAR.xlsx")
View(data_VAR)
library(tseries)
lcal_ts<-ts(data_VAR$CAL,start= c(1996,1),frequency = 4)
linf_ts<-ts(data_VAR$INF,start= c(1996,1),frequency = 4)
lgap_ts<-ts(data_VAR$GAP,start= c(1996,1),frequency = 4)

# Checking Stationarity 
adf.test(lcal_ts)
adf.test(diff(lcal_ts))
adf.test(lcal_ts)
adf.test(linf_ts)
adf.test(lgap_ts)

adf.test(diff(lcal_ts))
adf.test(diff(linf_ts))
adf.test(diff(lgap_ts))
library(urca)
adfcal =ur.df(lcal_ts,type = "trend",selectlags ="AIC")
summary(adfcal)

vardata <- data.frame(lcal_ts,linf_ts,lgap_ts)
library(vars)
# Selecting the lags 
varlags=VARselect(vardata,lag.max = 10)
varlags$selection
# VAR model 
varmodel=VAR(vardata,p=2, type = 'both')
summary(varmodel)

irf1=irf(varmodel,impulse = "lgap_ts",response = "lcal_ts",n.ahead = 10)
plot(irf1)
irf2=irf(varmodel,impulse = "lcal_ts",response = "linf_ts",n.ahead = 10)
plot(irf2)


# ARDL Model 
library(readxl)
data_ARDL<- read_excel("C:\\Users\\samhi\\Downloads\\End Semester\\R programming\\ARDL (1).xlsx")
View(data_ARDL)
data_ARDL <- data_ARDL %>% clean_names(case="snake")
colnames(data_ARDL)
# Connverting variables to timeseries 
gr = ts(data_ARDL$gr, start = 1971, frequency = 1)
infl = ts(data_ARDL$infl, start = 1971, frequency = 1)
fd = ts(data_ARDL$fd, start = 1971, frequency = 1)
ardl.ts <- data.frame(gr, infl, fd)

# Stationarity 
adf.test(gr)
#Stationary
adf.test(infl)
#Stationary
adf.test(fd)
adf.test(diff(fd))


# Plotting time series data
library(TSstudio)
ts_plot(gr)
ts_plot(infl)
ts_plot(fd)
plot.ts(data_ARDL)


# ARDL Model 
library(dynamac)
ardlmodel1 = dynardl(gr ~ fd + infl, lags = list("gr" = 1, "fd" = 1, "infl" = 1), diffs = "fd")
summary(ardlmodel1)

# Null - possible cointegration
library(ARDL)
autoardl = auto_ardl(gr ~ fd + infl, ardl.ts, max_order = 6)
autoardl
autoardl$best_order
bestardl = autoardl$best_model
bounds_f_test(bestardl, 2)


# ARIMA 
data_ARIMA <- read.csv("C:\\Users\\samhi\\Downloads\\End Semester\\R programming\\cpidata.csv")
library(janitor)
data_ARIMA <- clean_names(data_ARIMA,case = "snake")
colnames(data_ARIMA)
ARIMA.ts <- ts(data_ARIMA$cpi,frequency = 12, start=c(2000,1))

library(TSstudio)
ts_plot(ARIMA.ts)

# Decomposing components 
components.ts = decompose(ARIMA.ts)
plot(components.ts)
# autoplot 
library(ggfortify)
autoplot(decompose(ARIMA.ts))


# Staionarity 
adf.test(ARIMA.ts)
adf.test(diff(ARIMA.ts))
kpss.test(ARIMA.ts)
pp.test(ARIMA.ts)

#Finding ACF and PACF of data
acf(ARIMA.ts, lag.max=20)
pacf(ARIMA.ts, lag.max=20)

library(forecast)
model <- auto.arima(ARIMA.ts,ic='aicc',trace = TRUE)
summary(model)
tsdiag(model)
tsdiag(model)
tsdisplay(residuals(model))
qqnorm(model$residuals)
qqline(model$residuals)
predict(model,n.ahead = 20)
plot(forecast(model,h=20))



# Logit model
data_logit <- read.csv("C:\\Users\\samhi\\Downloads\\End Semester\\R programming\\logit_model.csv")
data_logit <- data_logit %>% clean_names(case="snake")
model <- glm(low ~ age + lwt + race + smoke, family = binomial(link="logit"), data = data_logit)
summ(model)
coef(model)
logit_or<-exp(coef(model))
logit_or


library(ggplot2)
library(maptools)
library(rgeos)
library(ggmap)
library(scales)
library(RColorBrewer)
library(readxl)
shp<- readShapeSpatial("C:/Users/samhi/Downloads/india_shapefile/Admin2.shp")
plot(shp)
names(shp)
print(shp$ST_NM)
pop<-read_excel("C:/Users/samhi/Downloads/poverty rate.xlsx")
head(pop)
library(ggplot2)
shp.f <- fortify(shp,region = "ST_NM")
merge.shp.coef<-merge(shp.f,pop, by="id",all.x=TRUE)
final.plot<-merge.shp.coef[order(merge.shp.coef$order),]
library(mapproj)
ggplot()+geom_polygon(data=final.plot,aes(x=long,y=lat, group=group, fill=count),color="black",size= 0.25)+coord_map()
ggsave("india_poverty.png",dpi = 300,width = 20,height = 20,units = "cm")
