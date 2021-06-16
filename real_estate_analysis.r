library(tidyverse)
library(lubridate)
library(caret)
setwd("C:/Users/Katherine/Desktop/RealEstateResearch")

# # load data (and append the data I had forgotten to add earlier...)
# df <- read_csv("ZillowAPIQuery_2021-01-09_.csv")
# queryDF <- read_csv("redfin_2021-01-09-10-27-31.csv")
# queryDF$daysOnMarket <- queryDF$`DAYS ON MARKET`
# appendDF <- queryDF %>% select(ADDRESS, PRICE, daysOnMarket) %>% rename(street=ADDRESS, price=PRICE)
# df <- merge(x = df, y = appendDF, by = "street", all.x = TRUE)

df <- read_csv("ZillowAPIQuery_2021-01-10_.csv")

#df$regionName <- revalue(df$regionName, c("High Pt"="High Point", "Winston salem"="Winston Salem"))
#data$scode[data$sex=="M"] <- "1"

# define function(s)
getMortgage <- function(homeValue, rate=0.036){
  N <- 12*30
  rate <- rate / 12
  a <- rate * ((1+rate)^N)
  b <- ((1+rate)^N) - 1
  q <- a/b
  res <- homeValue * q
  return(res)
}

# impute price as zestimate where price is unavailable
df$price = ifelse(is.na(df$price), df$zestimate, df$price)

# impute HOA as zero for now, when unknown
df$HOA = ifelse(is.na(df$HOA), 0, df$HOA)


# remove things that aren't actually properties we're interested in
df <- df %>% filter(useCode != "VacantResidentialLand")

# some feature engineering....
df$percentZestimateChange <- df$zestimateChange / df$zestimate
df$percentRentChange <- df$rentZestimateChange / df$rentZestimate
df$percentRentZestimate <- df$rentZestimate / df$zestimate
df$downPayment <- df$price * 0.2
df$mortgage <- sapply(df$price, getMortgage)
df$estTaxes <- df$taxAssessment * 0.015 / 12
df$netIncome <- df$rentZestimate - df$mortgage - df$HOA - df$estTaxes
df$cashOnCash <- df$netIncome / df$downPayment
df <- df %>% arrange(-cashOnCash)
df.high <- df %>% filter(price > 200000)

# get summaries by region and city

regionDF <- df %>% 
  group_by(regionName) %>% 
  summarize(
    regionMeanDaysOnMarket=mean(daysOnMarket, na.rm=T),
    regionPercentZestimateChange=mean(percentZestimateChange, na.rm=T),
    regionPercentRentChange=mean(percentRentChange, na.rm=T),
    regionPercentRentZestimate=mean(percentRentZestimate, na.rm=T),
    regionCashOnCash=mean(cashOnCash, na.rm=T),
    regionCount=n(),
  )
# df <- merge(x=df, y=regionDF, by="regionName", all.x=T)

cityDF <- df %>%
  group_by(city) %>%
  summarize(
    cityMeanDaysOnMarket=mean(daysOnMarket, na.rm=T),
    cityPercentZestimateChange=mean(percentZestimateChange, na.rm=T),
    cityPercentRentChange=mean(percentRentChange, na.rm=T),
    cityPercentRentZestimate=mean(percentRentZestimate, na.rm=T),
    cityCashOnCash=mean(cashOnCash, na.rm=T),
    cityCount=n()
  )
# df <- merge(x=df, y=cityDF, by="city", all.x=T)

# # just some subsets
# df.test <- df %>% filter(regionName == 'High Point')
# df.test2 <- df %>% filter(regionName == "Greensboro")
# df.test3 <- df %>% filter(regionName == "Northeast Raleigh")
# df.test4 <- df %>% filter(regionName == "Raleigh")

# save results
thisday <- strftime(today(), "%Y-%m-%d")
outfilename1 <- paste(c("analysis_", thisday, "_.csv"), sep="", collapse="")
outfilename2 <- paste(c("regionDF_", thisday, "_.csv"), sep="", collapse="")
outfilename3 <- paste(c("cityDF_", thisday, "_.csv"), sep="", collapse="")

write_csv(df, outfilename1)
write_csv(regionDF, outfilename2)
write_csv(cityDF, outfilename3)

# ggplot of latitude and longitude - cash on cash hot spots
ggplot(df, aes(x=longitude, y=latitude)) + geom_point(aes(color=cashOnCash))
ggplot(df, aes(x=longitude, y=latitude)) + geom_point(aes(color=cashOnCash^.2)) # adjust contrast

