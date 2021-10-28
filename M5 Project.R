#install packages
Install required packages

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
#install.packages("plotly")
#install.packages("stargazer")
#install.packages("ggcorrplot")
#install.packages("comptradr")
library(wooldridge)
library(quantmod) #https://www.quantmod.com/ describes the package and https://fred.stlouisfed.org/ contains information about the Federal Reserve data depository
library(WDI) # Please see https://cran.r-project.org/web/packages/WDI/WDI.pdf for documentation about the package and https://data.worldbank.org/indicator for general information about the data
library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(ggplot2) # https://www.r-graph-gallery.com/index.html for graph examples
library(plotly)
library(foreign)
library(stargazer)
library(ggcorrplot) # explores correlations on the graph
library(comtradr)
#import data

#WDI Data 
WDI <- WDI(country = 'all', indicator =c(rgdp="NY.GDP.MKTP.PP.CD", pop="SP.POP.TOTL"), start=2019, end=2019,
           extra = TRUE, cache = NULL) %>%
            filter(region!="Aggregates") %>%
           filter(!is.na(rgdp) & !is.na(pop))

#Export data
USexports <- ct_search(reporters = "USA", 
                       partners = "All", 
                       trade_direction = "exports", 
                       start_date = 2019, 
                       end_date = 2019)
FRexports <- ct_search(reporters = "France", 
                       partners = "All", 
                       trade_direction = "exports", 
                       start_date = 2019, 
                       end_date = 2019)
USexports <- USexports %>% 
  select(year, partner, partner_iso, trade_value_usd) %>% 
  rename(exports=trade_value_usd)
FRexports <- FRexports %>% 
  select(year, partner, partner_iso, trade_value_usd) %>% 
  rename(exports=trade_value_usd)
            