data <- read.csv("owid-covid-data.csv", header = T)
View(data)
library(tidyverse)
library(ggplot2)
uk_data <- data %>%
  filter(iso_code == 'GBR')%>%
  View()

total_cases_per_million = subset(uk_data, select=c(iso_code, date, total_cases_per_million))
cw_data = subset(uk_data, select=c(iso_code, date, total_cases_per_million, weekly_hosp_admissions_per_million, total_deaths_per_million, reproduction_rate, stringency_index, people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred, total_boosters_per_hundred))


#fig0
ggplot(total_cases_per_million, aes(x=as.Date(uk_data$date), y=total_cases_per_million))+
         geom_point(colour="red", size=2)+
         labs(x="Date", y="UK total cases(per million)",
              title="covid-19 total cases",
              caption="in the UK")

cw_data_July <- cw_data %>% filter(as.Date(date) <= as.Date('2021-07-01'))
#fig1
ggplot(cw_data_July, aes(x=as.Date(cw_data_July$date), y=total_cases_per_million))+
  geom_point(colour="red", size=2)+
  labs(x="Date", y="UK total cases(per million)",
       title="covid-19 total cases",
       caption="in the UK")
#fig2
ggplot(cw_data_July, aes(x=as.Date(cw_data_July$date), y=weekly_hosp_admissions_per_million))+
  geom_point(colour="blue", size=2)+
  labs(x="Date", y="UK weekly_hosp_admissions(per million)",
       title="covid-19 weekly_hosp_admissions",
       caption="in the UK")
#fig3
ggplot(cw_data_July, aes(x=as.Date(cw_data_July$date), y=total_deaths_per_million))+
  geom_point(colour="yellow", size=2)+
  labs(x="Date", y="UK total deaths(per million)",
       title="covid-19 total deaths",
       caption="in the UK")

cw_data_January <- cw_data %>% filter(as.Date(date) <= as.Date('2022-01-01'))
#fig4
ggplot(cw_data_January, aes(x=as.Date(cw_data_January$date), y=total_cases_per_million))+
  geom_point(colour="red", size=2)+
  labs(x="Date", y="UK total cases(per million)",
       title="covid-19 total cases",
       caption="in the UK")
#fig5
ggplot(cw_data_January, aes(x=as.Date(cw_data_January$date), y=weekly_hosp_admissions_per_million))+
  geom_point(colour="blue", size=2)+
  labs(x="Date", y="UK weekly_hosp_admissions(per million)",
       title="covid-19 weekly_hosp_admissions",
       caption="in the UK")
#fig6
ggplot(cw_data_January, aes(x=as.Date(cw_data_January$date), y=total_deaths_per_million))+
  geom_point(colour="yellow", size=2)+
  labs(x="Date", y="UK total deaths(per million)",
       title="covid-19 total deaths",
       caption="in the UK")

#fig0.1
ggplot(cw_data, aes(x=as.Date(cw_data$date), y=reproduction_rate))+
  geom_point(colour="red", size=2)+
  labs(x="Date", y="reproduction_rate",
       title="covid19 reproduction_rate",
       caption="in the UK")

#dealing with missing data
sum(is.na(cw_data))
install.packages('Hmisc')
library(Hmisc)
cw_data$total_cases_per_million <- with(cw_data, impute(total_cases_per_million, mean))
cw_data$weekly_hosp_admissions_per_million <- with(cw_data, impute(weekly_hosp_admissions_per_million, mean))
cw_data$total_deaths_per_million <- with(cw_data, impute(total_deaths_per_million, mean))
cw_data$reproduction_rate <- with(cw_data, impute(reproduction_rate, mean))

#time-series
install.packages('lubridate')
install.packages('forecast')
library(lubridate)
library(forecast)
ts <- ts(cbind(cw_data$total_cases_per_million, cw_data$weekly_hosp_admissions_per_million, cw_data$total_deaths_per_million),
                           start = decimal_date(ymd("2020-01-31")),
                           frequency = 365.25 / 7)
#fig7
plot(ts, xlab ="Date",
     main ="COVID-19 in the UK",
     col.main ="darkblue")
reproduction_rate_ts <- ts(cbind(cw_data$reproduction_rate, cw_data$total_cases_per_million),
          start = decimal_date(ymd("2020-01-31")),
          frequency = 365.25 / 7)
#fig8
plot(reproduction_rate_ts, xlab ="Date",
     main ="COVID-19 reproduction_rate",
     col.main ="darkblue")
reproduction_rate_forecast <- ts(cw_data$reproduction_rate, start = decimal_date(ymd("2022-01-31")),
          frequency = 365.25 / 7)
fit <- auto.arima(reproduction_rate_forecast)
# Next 5 forecasted values
forecast(fit, 5)
# plotting the graph with next 5 weekly forecasted values
#fig9
plot(forecast(fit, 5), xlab ="Date",
     ylab ="reproduction_rate",
     main ="COVID-19 reproduction_rate", col.main ="darkblue")


##mapping
worldmap = subset(data, select=c(iso_code, date, total_cases_per_million))
world <-map_data("world")
ggplot() + geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="grey")
ggplot()+
  geom_polygon(data=world,aes(x=long,y=lat,group=group),fill="grey",alpha=0.3) +
  geom_point(data=world,aes(x=long,y=lat,size='total_cases_per_million',color='total_cases_per_million'),alpha=0.5)

##world map
library(maps)
library(ggmap)
world_map <- map_data("world")
map2020 = subset(data, select=c(location, date, total_cases_per_million, weekly_hosp_admissions_per_million, total_deaths_per_million))
Map2020 <- map2020 %>%
  filter(as.Date(date) == as.Date("2020-12-01"))
  
map2020 <- left_join(world_map, Map2020, by = c("region"="location"))
world_map_2020 <- map2020 %>% 
  ggplot(aes(long,lat,group=group))+
  geom_polygon(aes(fill=(total_cases_per_million)))+ 
  scale_fill_viridis_c()+
  theme_void()+
  labs(fill="total_cases_per_million",
       title="World map coloured by total cases in 2020")
world_map_2020

map2022 = subset(data, select=c(location, date, total_cases_per_million, weekly_hosp_admissions_per_million, total_deaths_per_million))
Map2022 <- map2022 %>%
  filter(as.Date(date) == as.Date("2022-07-03"))

map2022 <- left_join(world_map, Map2022, by = c("region"="location"))
world_map_2022 <- map2022 %>% 
  ggplot(aes(long,lat,group=group))+
  geom_polygon(aes(fill=(total_cases_per_million)))+ 
  scale_fill_viridis_c()+
  theme_void()+
  labs(fill="total_cases_per_million",
       title="World map coloured by total case in 2022")
world_map_2022

library(gridExtra)
grid.arrange(world_map_2020, world_map_2022)


#fig10
f1 <- ggplot(cw_data,aes(x=as.Date(cw_data$date), y= people_vaccinated_per_hundred))  +  
         geom_point(colour="pink", size=3) +  
         geom_smooth(method="loess", se=F) + 
         labs(x="Year", y="people_vaccinated_per_hundred",
            title='people_vaccinated_per_hundred', subtitle = 'in the UK')
f1
f2 <- ggplot(cw_data,aes(x=as.Date(cw_data$date), y= people_fully_vaccinated_per_hundred))  +  
         geom_point(colour="orange", size=3) +  
         geom_smooth(method="loess", se=F) + 
         labs(x="Year", y="people_fully_vaccinated_per_hundred",
            title='people_fully_vaccinated_per_hundred', subtitle = 'in the UK')
f2
f3 <- ggplot(cw_data,aes(x=as.Date(cw_data$date), y= total_boosters_per_hundred))  +  
         geom_point(colour="green", size=3) +  
         geom_smooth(method="loess", se=F) + 
         labs(x="Year", y="total_boosters_per_hundred",
            title='total_boosters_per_hundred', subtitle = 'in the UK')
f3
library(gridExtra)
grid.arrange(f1, f2, f3)


#fig 11
library(GGally)
cor_cw_data = subset(cw_data, select=c(total_cases_per_million, weekly_hosp_admissions_per_million, total_deaths_per_million, stringency_index))
cor(cor_cw_data) 
ggpairs(cor_cw_data, title="correlogram with ggpairs()")+
  theme(panel.border = element_rect(fill=NA),
        axis.text =  element_text(color='blue'))
