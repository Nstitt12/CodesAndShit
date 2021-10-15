

#install.packages("wooldridge")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("comptradr")
#install.packages("quantmod")
#install.packages("WDI")
#install.packages("zoo") # Makes working with time series easier
#install.packages("lubridate") # Makes working with dates easier

install.packages("devtools")
devtools::install_github("ropensci/comtradr")



#====================================================================================
# ===== Downloading several basic data sets 
library(comtradr)
library(quantmod) #https://www.quantmod.com/ describes the package and https://fred.stlouisfed.org/ contains information about the Federal Reserve data depository
library(WDI) # Please see https://cran.r-project.org/web/packages/WDI/WDI.pdf for documentation about the package and https://data.worldbank.org/indicator for general information about the data

library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(ggplot2)



NDexports <- ct_search(reporters = "Netherlands", 
                       partners = "All", 
                       trade_direction = "exports", 
                       start_date = 2015, 
                       end_date = 2019)
NDimports <- ct_search(reporters = "Netherlands", 
                       partners = "All", 
                       trade_direction = "imports", 
                       start_date = 2015, 
                       end_date = 2019)
NDimports <- NDimports %>% 
  select(year, partner, partner_iso, trade_value_usd) %>% 
  rename(imports=trade_value_usd)
NDexports <- NDexports %>% 
  select(year, partner, partner_iso, trade_value_usd) %>% 
  rename(exports=trade_value_usd)
NDtrade <- full_join(NDexports, NDimports) #
# I used full join so no data was left out
# I will choose to filter out world from the partners. 
# Decribe data  below
names(NDtrade)
ncol(NDtrade)
nrow(NDtrade)
any(is.na(NDtrade))
# create new columns
NDtrade %>% 
  group_by(year) %>% 
  mutate(imp_share = round(imports/sum(imports , na.rm=T)*100 , digits=1)) %>%
  filter(rank(-imp_share)<=3)
# 
table <- NDtrade %>% group_by(year) %>%
  filter(partner!="World")  %>%
  mutate(imports=replace(imports,is.na(imports),0)) %>%
  mutate(imp_share = round(imports/sum(imports , na.rm=T)*100 , digits=1)) %>%
  mutate(rank.imp_share = rank(-imp_share)) %>%
  arrange(year, -imp_share) %>%
  mutate(sum.imp_share=cumsum(imp_share)) %>% 
  filter(rank.imp_share<6) %>%
  select(year, partner_iso, rank.imp_share) %>%
  pivot_wider(names_from=year, values_from=c(partner_iso)) 
table
# The imports in the Netherlands has not changed from 2015-2019. The same countries have ranked the same every year. 




# WDI Data Import and Cleaning
tech <- WDI(country = "all",
    indicator =  "TX.VAL.TECH.CD",
    start = 2016,
    end = 2018,
    extra = TRUE,
    cache = NULL)
tech <- tech[tech$region!="Aggregates",]
tech1 <- WDI(country = "all",
            indicator =  "NY.ADJ.NNTY.PC.CD",
            start = 2016,
            end = 2018,
            extra = TRUE,
            cache = NULL)
tech1 <- tech1[tech1$region!="Aggregates",]
tech <- tech %>%
  select(year, country, TX.VAL.TECH.CD,)%>%
  rename(tech_exports=TX.VAL.TECH.CD)
tech1 <- tech1 %>%
  select(year, country, NY.ADJ.NNTY.PC.CD)%>%
  rename(ipc=NY.ADJ.NNTY.PC.CD)
tech <- full_join(tech, tech1,)

# Calculate average of indicator

aveexports <- tech %>% 
  group_by(country) %>% 
  summarize ( ave.exports = mean(tech_exports, na.rm=TRUE),
              ave.ipc = mean(ipc), na.rm=TRUE)  
# Average Growth of Tech Exports
tech <- tech %>% 
  arrange(country, year) %>% 
  group_by(country)  %>% 
  mutate(tech_exports_growth=((tech_exports-lag(tech_exports))/lag(tech_exports)))              
# Break countries into groups based on income per capita
aveexports <- aveexports %>% filter(!is.na(ave.ipc)) %>% 
  mutate(inc.quintile=ceiling(rank(ave.ipc)/(n()/5)))
# Table with 5 equal groups
aveexports %>% group_by(inc.quintile) %>% summarize(mean(ave.exports, na.rm=TRUE))  
                                                      
# Summary: THis data shows that the more exports in the tech industry a country has, the more income per capita they will posess.  
 
 
 
 
 
 # Time Series for FRED

getSymbols('PCEPILFE',src='FRED') #
getSymbols('UNRATE',src='FRED') 
df1 <- data.frame(PCEPILFE, date=index(PCEPILFE))
df2 <- data.frame(UNRATE, date=index(UNRATE))
# Join Data Sets together

inflation <- full_join(df1, df2, by="date")
# I wanted to keep all data so I used full join
# Calculate inflation rates

inflation$PCEPILFEinfl <- round(
  (inflation$PCEPILFE-lag(inflation$PCEPILFE))/(lag(inflation$PCEPILFE))*100, 
  digits=2)
inflation$UNRATEinfl <- round(
  (inflation$UNRATE-lag(inflation$UNRATE))/(lag(inflation$UNRATE))*100, 
  digits=2)
# Break into periods of recession
inflation <- inflation %>%
          filter(between(date, as.Date("2001-03-01"), as.Date("2001-08-01")) |
                         between(date, as.Date("2007-12-01"), as.Date("2008-05-01")) |
                                 between(date, as.Date("2019-11-01"), as.Date("2020-04-01")))
 
 inflation <- inflation %>% 
  mutate(period=cut(
    1:18,
    breaks=3, 
    labels=c("Recession 1", "Recession 2", "Recession 3")))
 inflation %>% group_by(period) %>% summarize(mean(UNRATEinfl),mean(PCEPILFEinfl))
# Summary : This shows that personal consumption will diminish in a recession. 