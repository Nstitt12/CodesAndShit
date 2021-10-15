# Install required packages

#install.packages("wooldridge")
#install.packages("foreign")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("tradestatistics")
#install.packages("quantmod")
#install.packages("WDI")
#install.packages("zoo") # Makes working with time series easier
#install.packages("lubridate") # Makes working with dates easier

#====================================================================================
# ===== Downloading several basic data sets 

library(quantmod) #https://www.quantmod.com/ describes the package and https://fred.stlouisfed.org/ contains information about the Federal Reserve data depository
library(WDI) # Please see https://cran.r-project.org/web/packages/WDI/WDI.pdf for documentation about the package and https://data.worldbank.org/indicator for general information about the data
library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(ggplot2) # https://www.r-graph-gallery.com/index.html for graph examples

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

# Creating Graphs 
inflation <- inflation %>%
mutate(year_month=paste(year(inflation$date),"/", month(inflation$date), sep="") ) %>%
arrange(year_month, date) %>% mutate(first=1:n()==1, second=1:n()==7, third=1:n()==13)

SUbinflation <- inflation %>%
  subset(first=="TRUE")

                                                   
inflation %>% arrange(year_month, date) %>% mutate(second=1:n()==7)


ggplot(data=subset(inflation, UNRATE<6), aes(x=PCEPILFEinfl, y=UNRATE)) +
  geom_path(aes(color=period)) +
  geom_text(data=subset(inflation, first=="TRUE" | second=="TRUE" | third=="TRUE", UNRATE<6), aes(label=year_month,)) +
  labs(x="Inflation Rate", y="Unemployment Rate", title= "Last Three Recessions")         
         
       
       