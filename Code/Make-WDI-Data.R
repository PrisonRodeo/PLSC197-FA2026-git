#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Intro things                                    ####
#
# PLSC 197 / SODA 110N -- Fall 2026
#
# Social Data, Technology, and Artificial Intelligence
# Prof. Christopher Zorn
#
# This is some code to grab & create data from 
# the World Bank's _World Development Indicators_ (WDI).
# These are the data that we'll use for the "data
# visualization" lab February 10 & 12, 2026. The WDI 
# data schema is detailed here:
#
# https://databank.worldbank.org/home
#
# This code creates a dataframe called "wdi." It then
# cleans up those data, and outputs them to a .CSV file
# on your computer (in the "working directory"). Your
# computer must have a working internet connection
# for this code to function properly.
#
# NOTE: This code can take a hot second to
# run, depending on how fast the World Bank's
# API is operating on any given day...so expect
# to wait 1-2 minutes (or more) for it to
# run.
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Packages: This code checks to see if the packages
# needed are installed. If not, it installs them;
# if so, it prints a little smiley face. :)
# Either way, it loads all the required packages
# when it's done.

P<-c("RCurl","readr","data.table","countrycode","WDI")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# NOTE: You probably want to run the first 45 lines or so of 
# this file 4-5 times, to make sure everything gets loaded.
# If the console window looks like this:
#
#[1] ":)"
#[1] ":)"
#[1] ":)"
#[1] ":)"
#[1] ":)"
#
# ...then you know you're in good shape.
#
# Also, be sure to set a working directory in here
# someplace, a la:
#
# setwd("~/Dropbox (Personal)/SDTAI/Data")
#
# or whatever.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Grab the data...                              ####
#
# Get the relevant data / indicators (add variables
# as you wish...), and limit data to one year (2019):

wdi<-WDI(country="all",
         indicator=c("LandArea"="AG.LND.TOTL.K2", # Land area (sq. km)
                     "ArablePercent"="AG.LND.ARBL.ZS", # Arable Land (% of total land area)
                     "Population"="SP.POP.TOTL", # Popluation (in, like, people)
                     "PopGrowth"="SP.POP.GROW", # Population Growth (%)
                     "RuralPopulation"="SP.RUR.TOTL.ZS", # Rural Population (% of total)
                     "UrbanPopulation"="SP.URB.TOTL.IN.ZS", # Urban Population (% of total)
                     "BirthRatePer1K"="SP.DYN.CBRT.IN", # Birth Rate (births per 1K people)
                     "FertilityRate"="SP.DYN.TFRT.IN", # Fertility Rate (births per woman)
                     "PrimarySchoolAge"="SE.PRM.AGES", # Primary school starting age (years)
                     "LifeExpectancy"="SP.DYN.LE00.IN", # Life Expectancy at birth (years)
                     "AgeDepRatioOld"="SP.POP.DPND.OL", # Age Dependency Ratio (old), % working age population
                     "CO2Emissions"="EN.GHG.CO2.PC.CE.AR5", # CO2 Emissions (metric tons per capita)
                     "GDP"="NY.GDP.MKTP.KD", # GDP, constant 2015 $US
                     "GDPPerCapita"="NY.GDP.PCAP.KD", # GDP per capita (constant 2010 $US)
                     "GDPPerCapGrowth"="NY.GDP.PCAP.KD.ZG", # GDP Per Capita Growth (%)
                     "Inflation"="FP.CPI.TOTL.ZG", # Inflation (CPI, annual %)
                     "TotalTrade"="NE.TRD.GNFS.ZS", # Total trade, % of GDP
                     "Exports"="NE.EXP.GNFS.ZS", # Exports, % of GDP
                     "Imports"="NE.IMP.GNFS.ZS", # Imports, % of GDP
                     "FDIIn"="BX.KLT.DINV.WD.GD.ZS", # FDI in, % of GDP
                     "AgriEmployment"="SL.AGR.EMPL.ZS", # Percent of total employment in agriculture
                     "MobileCellSubscriptions"="IT.CEL.SETS.P2", # Mobile / cellular subscriptions per 100 people
                     "NaturalResourceRents"="NY.GDP.TOTL.RT.ZS", # Total natural resource rents (% of GDP)
                     "GovtExpenditures"="NE.CON.GOVT.ZS", # Government Expenditures, % of GDP
                     "PublicHealthExpend"="SH.XPD.GHED.GD.ZS", # Public expenditure on health (% of GDP)
                     "PoliticalStability"="GOV_WGI_PV.SC", # Political Stability and Absence of Violence/Terrorism
                     "RuleOfLaw"="GOV_WGI_RL.SC", # Rule of Law
                     "ControlOfCorruption"="GOV_WGI_CC.SC"), # Control of Corruption
                start=2019,end=2019,extra=FALSE)

# Get a couple more things, and merge:

country_info <- WDI::WDI_data$country[, c("iso3c", "region", "income")]
wdi <- merge(wdi, country_info, by = "iso3c", all.x = TRUE)
rm(country_info)

# Create discrete variables:

wdi$Region<-wdi$region      # Region
wdi$region<-NULL
wdi$IncomeLevel<-wdi$income # Income level
wdi$income<-NULL
wdi$RuralLevel<-ifelse(wdi$RuralPopulation>median(wdi$RuralPopulation,na.rm=TRUE),
                       paste("High"),paste("Low"))
wdi$UrbanLevel<-ifelse(wdi$UrbanPopulation>median(wdi$UrbanPopulation,na.rm=TRUE),
                       paste("High"),paste("Low"))
wdi$RuralLevel<-ifelse(wdi$RuralPopulation>median(wdi$RuralPopulation,na.rm=TRUE),
                       paste("High"),paste("Low"))
wdi$PopGrowthLevel<-ifelse(wdi$PopGrowth>median(wdi$PopGrowth,na.rm=TRUE),
                      paste("High"),paste("Low"))
wdi$EconGrowthLevel<-ifelse(wdi$GDPPerCapGrowth>median(wdi$GDPPerCapGrowth,na.rm=TRUE),
                       paste("High"),paste("Low"))
wdi$TradeLevel<-ifelse(wdi$TotalTrade>median(wdi$TotalTrade,na.rm=TRUE),
                       paste("High"),paste("Low"))
wdi$AgricultureLevel<-ifelse(wdi$AgriEmployment>median(wdi$AgriEmployment,na.rm=TRUE),
                       paste("High"),paste("Low"))


# Remove aggregates (e.g., "World," "Arab World," etc.):

wdi<-wdi[wdi$Region!="Aggregates",]

# Rename ISO3:

wdi$ISO3<-wdi$iso3c
wdi$iso3c<-NULL

# Fix Region labels:

wdi$Region<-ifelse(wdi$Region=="Middle East, North Africa, Afghanistan & Pakistan",
                   paste("Middle East & North Africa"),wdi$Region)

# Rename year:

wdi$Year<-wdi$year
wdi$year<-NULL

# remove iso2c:

wdi$iso2c<-NULL

# Get rid of unidentified rows:

wdi<-wdi[is.na(wdi$Year)==FALSE,] 

# Put ISO3 + Year + Region at the front of the data:

wdi<-wdi[,c("ISO3","Year","Region",setdiff(names(wdi),c("ISO3","Year","Region")))]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Output the CSV file:

write.csv(wdi,"Data/WDI2019.csv",row.names=FALSE,na="")

# FIN!