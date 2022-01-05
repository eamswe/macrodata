library("reshape2")
library("zoo")
library("xts")
library("rio")
library("parsedate")
library("janitor")
library("dplyr")
library("tidyverse")
library("openxlsx")
library('csodata')
#get the data from the CSO's API
NQQ47 <- cso_get_data(
  "NQQ47",
  wide_format = "long",
  include_ids = FALSE,
  id_list = NULL,
  use_factors = TRUE,
  cache = FALSE
)
#Clean and subset to seasonally adjusted index
NQQ47 <- clean_names(NQQ47)
NQQ47$statistic <- as.factor(NQQ47$statistic)
#this creates a column for every time the 'statistic' column has SA in it. Same for prices.
sa <- grepl('Seasonally Adjusted', NQQ47$statistic) 
con <- grepl('Constant', NQQ47$statistic) 
#make new blank columns in our dataset
NQQ47$sa <- NA
NQQ47$prices <- NA
#merge in the columns we made with grepl above
NQQ47$sa[sa] <- '_sa'
NQQ47$prices[con] <- 'constant'
#fill in the blanks in these columns i.e non sa and current prices
NQQ47 <- NQQ47 %>% dplyr::mutate(prices = replace_na(prices, 'current'))
NQQ47 <- NQQ47 %>% dplyr::mutate(sa = replace_na(sa, '_non_sa'))
#make a column that makes one word out of these
NQQ47$price_adj <- paste(NQQ47$prices, NQQ47$sa)
#get rid of extra columns we dont need
NQQ47<- subset(NQQ47,
               select=c(quarter, sector, value, price_adj))
#make this dataframe into list of dataframes based on the price adjustment

NQQ47_split <- split(NQQ47, NQQ47$price_adj)
#for all of these select the relevant columns 
NQQ47_split <- lapply(NQQ47_split, function(x) {subset(x, select=c(quarter, sector, value))})
#make the data wide (rather than long)
nqq47_split <- lapply(NQQ47_split, function(x) {spread(x, sector, value)})
#make an Excel sheet
write.xlsx(nqq47_split, "NQQ_47.xlsx")


NQQ48 <- cso_get_data(
  "NQQ48",
  wide_format = "long",
  include_ids = FALSE,
  id_list = NULL,
  use_factors = TRUE,
  cache = FALSE
)
NQQ48 <- clean_names(NQQ48)
NQQ48$statistic <- as.factor(NQQ48$statistic)
sa <- grepl('Seasonally Adjusted', NQQ48$statistic) 
con <- grepl('chain linked', NQQ48$statistic) 
NQQ48$sa <- NA
NQQ48$prices <- NA
NQQ48$sa[sa] <- '_sa'
NQQ48$prices[con] <- 'constant'

NQQ48 <- NQQ48 %>% dplyr::mutate(prices = replace_na(prices, 'current'))
NQQ48 <- NQQ48 %>% dplyr::mutate(sa = replace_na(sa, '_non_sa'))

NQQ48$price_adj <- paste(NQQ48$prices, NQQ48$sa)
NQQ48<- subset(NQQ48,
               select=c(quarter, statistic, value, price_adj)) 

NQQ48_split <- split(NQQ48, NQQ48$price_adj)
NQQ48_split <- lapply(NQQ48_split, function(x) {subset(x, select=c(quarter, statistic, value))})

nqq48_split <- lapply(NQQ48_split, function(x) {spread(x, statistic, value)})
write.xlsx(nqq48_split, "NQQ_48.xlsx")

NQQ49 <- cso_get_data(
  "NQQ49",
  wide_format = "long",
  include_ids = FALSE,
  id_list = NULL,
  use_factors = TRUE,
  cache = FALSE
)
#Clean and subset to seasonally adjusted index
NQQ49 <- clean_names(NQQ49)
NQQ49$statistic <- as.factor(NQQ49$statistic)
sa <- grepl('Seasonally Adjusted', NQQ49$statistic) 
con <- grepl('Constant', NQQ49$statistic) 
NQQ49$sa <- NA
NQQ49$prices <- NA
NQQ49$sa[sa] <- '_sa'
NQQ49$prices[con] <- 'constant'

NQQ49 <- NQQ49 %>% dplyr::mutate(prices = replace_na(prices, 'current'))
NQQ49 <- NQQ49 %>% dplyr::mutate(sa = replace_na(sa, '_non_sa'))

NQQ49$price_adj <- paste(NQQ49$prices, NQQ49$sa)
NQQ49<- subset(NQQ49,
               select=c(quarter, sector, value, price_adj)) 

NQQ49_split <- split(NQQ49, NQQ49$price_adj)
NQQ49_split <- lapply(NQQ49_split, function(x) {subset(x, select=c(quarter, sector, value))})

nqq49_split <- lapply(NQQ49_split, function(x) {spread(x, sector, value)})
write.xlsx(nqq49_split, "NQQ_49.xlsx")

NQQ50 <- cso_get_data(
  "NQQ50",
  wide_format = "long",
  include_ids = FALSE,
  id_list = NULL,
  use_factors = TRUE,
  cache = FALSE
)
#Clean and subset to seasonally adjusted index
NQQ50 <- clean_names(NQQ50)
NQQ50$statistic <- as.factor(NQQ50$statistic)
sa <- grepl('Seasonally Adjusted', NQQ50$statistic) 
con <- grepl('Constant', NQQ50$statistic) 
NQQ50$sa <- NA
NQQ50$prices <- NA
NQQ50$sa[sa] <- '_sa'
NQQ50$prices[con] <- 'constant'

NQQ50 <- NQQ50 %>% dplyr::mutate(prices = replace_na(prices, 'current'))
NQQ50 <- NQQ50 %>% dplyr::mutate(sa = replace_na(sa, '_non_sa'))

NQQ50$price_adj <- paste(NQQ50$prices, NQQ50$sa)
NQQ50<- subset(NQQ50,
               select=c(quarter, sector, value, price_adj)) 

NQQ50_split <- split(NQQ50, NQQ50$price_adj)
NQQ50_split <- lapply(NQQ50_split, function(x) {subset(x, select=c(quarter, sector, value))})

nqq50_split <- lapply(NQQ50_split, function(x) {spread(x, sector, value)})
write.xlsx(nqq50_split, "NQQ_50.xlsx")
