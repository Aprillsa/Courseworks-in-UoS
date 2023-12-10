install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
employment_data <- read.csv("Data.csv", header=TRUE)

install.packages("WDI")
library(WDI)
new_wdi_cache <- WDIcache()
education_data <- WDI(country="all",
                  indicator = "SE.TER.CUAT.MS.ZS",     ##Educational attainment, at least Master's or equivalent, population 25+, total (%) (cumulative)
                  start = 2010,
                  end = 2022,
                  cache = new_wdi_cache)
View(education_data)
country <- subset(education_data, country=="United Kingdom" | country=="China")
View(country)
education <- WDI(country=c("GB","FR","ES","IT","NL", "CN","AE","IN","JO","US"),
                   indicator = c("SE.TER.CUAT.BA.MA.ZS","SE.TER.CUAT.BA.FE.ZS","SE.TER.CUAT.DO.FE.ZS","SE.TER.CUAT.DO.MA.ZS","SE.TER.CUAT.MS.FE.ZS","SE.TER.CUAT.MS.MA.ZS"),
                    start = 2010, end = 2022,
                    extra=TRUE,
                    cache = new_wdi_cache)

View(education)


###the No.1 pic
ggplot(education,aes(year, SE.TER.CUAT.BA.MA.ZS, colour=country, size=SE.TER.CUAT.BA.MA.ZS, shape = region))  +  
  geom_point() +  
  labs(x="Year", y="Educational attainment,at least Master's or equivalent,population 25+(%)")

###the No.2 pic
ggplot(education,aes(year, SE.TER.CUAT.BA.MA.ZS) ) +
  geom_point(aes(colour =country)) +
  facet_grid(region ~ .) +
  labs(x="Year", y="Educational attainment(%)",
       title='Educational attainment,at least Masters or equivalent,population 25+(%)', subtitle = 'Facet on the Y axis',
       colour="Country",
       caption='World Development Indicators, World Bank')

###the No.3 pic???????????????????????????????
ggplot(education, aes(region))+geom_bar(aes(fill=))+
  labs(x="Regions", y="Educational attainment(%)",
       title="Educational attainment(%),population 25+(%)",
       caption='World Development Indicators, World Bank')

###the No.4 pic?????????????????
ggplot(education, aes(factor(1), fill= region))+geom_bar()+
  coord_polar(theta="y")+
  theme(axis.line = element_blank(), panel.background = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  labs(x=NULL, y=NULL, fill="Regions",
       title="Educational attainment(%),population 25+(%)",
       caption="World Development Indicators, World Bank")


internet <- WDI(country=c("GB","FR","ES","IT","NL", "CN","AE","IN","JO","US"),
                 indicator = c("IT.NET.USER.ZS","NY.GDP.MKTP.KD.ZG","SH.XPD.CHEX.GD.ZS","NV.SRV.TOTL.ZS"),
                 start = 2010, end = 2022,
                 extra=TRUE,
                 cache = new_wdi_cache)
###the No.5 pic
ggplot(internet,aes(year, IT.NET.USER.ZS, colour=country, size=IT.NET.USER.ZS, shape = region))  +  
  geom_point() +  
  labs(x="Year", y="Individuals using the Internet (% of population)")

###the No.6 pic
ggplot(internet,aes(year, IT.NET.USER.ZS) ) +
  geom_point(aes(colour =country)) +
  facet_grid(region ~ .) +
  labs(x="Year", y="Individuals using the Internet (% of population)",
       title='Individuals using the Internet (% of population)', subtitle = 'Facet on the Y axis',
       colour="Country",
       caption='World Development Indicators, World Bank')

###the No.7 pic??????????????????
ggplot(internet, aes(factor(1), fill= region))+geom_bar()+
  coord_polar(theta="y")+
  theme(axis.line = element_blank(), panel.background = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  labs(x=NULL, y=NULL, fill="Regions",
       title="Individuals using the Internet (% of population)",
       caption="World Development Indicators, World Bank")

###the No.8 pic######good one
ggplot(internet,aes(year, IT.NET.USER.ZS))  +  
  geom_point(aes(colour=region, size=IT.NET.USER.ZS)) +  
  geom_smooth(method="loess", se=F) + 
  labs(x="Year", y="Individuals using the Internet (% of population)",
       title='Individuals using the Internet (% of population)', subtitle = 'in different regions',
       caption='World Development Indicators, World Bank')

###the No.9 pic?????
install.packages("ggcorrplot")
library(ggcorrplot)
corr <- round(cor(internet), 1)
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of internet", 
           ggtheme=theme_bw)

###the No.10 pic###good one
install.packages("dplyr")
library(dplyr)
#ggplot(internet, aes(year, IT.NET.USER.ZS, size = NY.GDP.MKTP.KD.ZG)) +
#  geom_point(alpha=0.7)
#ggplot(internet, aes(NY.GDP.MKTP.KD.ZG, NV.SRV.TOTL.ZS, size = IT.NET.USER.ZS)) +
#  geom_point(alpha=0.7)
internet %>%
  arrange(desc(IT.NET.USER.ZS)) %>%
  ggplot(aes(x=NY.GDP.MKTP.KD.ZG, y=NV.SRV.TOTL.ZS, size = IT.NET.USER.ZS, color=region)) +
  geom_point(alpha=0.3) +
  scale_size(range = c(.1, 24), name="Individuals using the Internet")+
  labs(x="GDP growth (annual %)", y="Services, value added (% of GDP)",
       title='Individuals using the Internet (% of population)', subtitle = 'in different regions',
       colour="region",
       caption='World Development Indicators, World Bank')


###the No.11 pic###good one
install.packages("GGally")
library(GGally)
cor_internet = subset(internet, select=c(IT.NET.USER.ZS, NY.GDP.MKTP.KD.ZG, SH.XPD.CHEX.GD.ZS, NV.SRV.TOTL.ZS))
cor(cor_internet) 
ggpairs(cor_internet, title="correlogram with ggpairs()")+
  theme(panel.border = element_rect(fill=NA),
        axis.text =  element_text(color='blue'))



###the No.12 pic###good one
install.packages("maps")
library(maps)
install.packages("ggmap")
library(ggmap)
world_map <- map_data("world")
countryDataWDI_2000<-WDI(indicator=c("IT.NET.USER.ZS", 
                                     "NY.GDP.MKTP.KD.ZG",
                                     "SH.XPD.CHEX.GD.ZS"),
                         start=2000,
                         end=2000,
                         extra= TRUE,
                         cache=new_wdi_cache)
countryDataWDI_00 <- countryDataWDI_2000 %>% filter(region != "Aggregation" & income != "NA")
countryDataWDI_00 <- na.omit(countryDataWDI_00)

getPalette = colorRampPalette(brewer.pal(12, "Set3"))

countryDataWDIMap_00 <- left_join(world_map, countryDataWDI_00, by = c("region"="country"))

colourCount_00 = length(unique(countryDataWDIMap_00$SP.DYN.LE00.IN))

world_map_00 <- countryDataWDIMap_00 %>% 
  ggplot(aes(long,lat,group=group))+
  geom_polygon(aes(fill=IT.NET.USER.ZS),color="White")+ 
  scale_fill_viridis_c()+
  theme_void()+
  labs(fill="Individuals using the Internet",
       title="World map coloured by Individuals using the Internet in 2000",
       caption="World Development Indicators, World Bank")
world_map_00

countryDataWDI_2019<-WDI(indicator=c("IT.NET.USER.ZS", 
                                     "NY.GDP.MKTP.KD.ZG",
                                     "SH.XPD.CHEX.GD.ZS"),
                         start=2019,
                         end=2019,
                         extra= TRUE,
                         cache=new_wdi_cache)
countryDataWDI_19 <- countryDataWDI_2019 %>% filter(region != "Aggregation" & income != "NA")
countryDataWDI_19 <- na.omit(countryDataWDI_19)
countryDataWDIMap_19 <- left_join(world_map,countryDataWDI_19, by = c("region"="country"))
world_map_19 <- countryDataWDIMap_19 %>% 
  ggplot(aes(long,lat,group=group))+
  geom_polygon(aes(fill=IT.NET.USER.ZS),color="White")+ 
  scale_fill_viridis_c()+
  theme_void()+
  labs(fill="Individuals using the Internet",
       title="World map coloured by Individuals using the Internet in 2019",
       caption="World Development Indicators, World Bank")
world_map_19
install.packages("gridExtra")
library(gridExtra)
grid.arrange(world_map_00, world_map_19)
