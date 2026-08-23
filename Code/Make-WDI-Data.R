#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Intro things                                    ####
#
# PLSC 197 / SODA 197N -- Spring 2026
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
# to wait 5-10 minutes (or more) for it to
# run.
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
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
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
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
                     "NetAidReceived"="DT.ODA.ALLD.KD", # Net official dev. aid received (constant 2018 $US)
                     "MobileCellSubscriptions"="IT.CEL.SETS.P2", # Mobile / cellular subscriptions per 100 people
                     "NaturalResourceRents"="NY.GDP.TOTL.RT.ZS", # Total natural resource rents (% of GDP)
                     "MilitaryExpenditures"="MS.MIL.XPND.GD.ZS", # Military expenditures, % of GDP
                     "GovtExpenditures"="NE.CON.GOVT.ZS", # Government Expenditures, % of GDP
                     "PublicEdExpend"="SE.XPD.TOTL.GD.ZS", # Public expenditure on education (% of GDP)
                     "PublicHealthExpend"="SH.XPD.GHED.GD.ZS", # Public expenditure on health (% of GDP)
                     "HIVDeaths"="SH.DYN.AIDS.DH", # Deaths due to HIV/AIDS (UNAIDS estimate)
                     "MalariaRate"="SH.MLR.INCD.P3", # Incidence of malaria (per 1K at risk)
                     "PoliticalStability"="PV.EST", # Political Stability and Absence of Violence/Terrorism
                     "RuleOfLaw"="RL.EST", # Rule of Law
                     "PaidParentalLeave"="SH.PAR.LEVE.AL", # Paid Parental Leave (0=no,1=yes)
                     "WomenEmployDiscrim"="SG.LAW.NODC.HR", # Law prohibits employment discrimination based on sex
                     "WomenEqualPay"="SG.LAW.EQRM.WK", # Law mandates equal remuneration by sex
                     "WomenDivorce"="SG.OBT.DVRC.EQ"), # A woman can obtain a divorce in the same way as a man
                start=2019,end=2019)

# Remove aggregates (e.g., "World," "Arab World," etc.):

wdi$ISO3<-countrycode(wdi$iso2c,origin="iso2c",destination="iso3c")
wdi<-wdi[is.na(wdi$ISO3)==FALSE,]

# Relabel year:

wdi$Year<-wdi$year
wdi$year<-NULL

# Delete ISO2:

wdi$iso2c<-NULL

# Create a "region" variable

wdi$Region<-countrycode(wdi$ISO3,origin="iso3c",destination="region")

# Put ISO3 + Year + Region at the front of the data:

nc<-ncol(wdi)
sb<-seq(nc-2,nc)
se<-seq(1,(nc-3))
wdi<-wdi[,c(sb,se)]
rm(nc,sb,se)

# remove the "iso3c" variable:

wdi$iso3c<-NULL

# Output the CSV file:

write.csv(wdi,"WDI2019.csv",row.names=FALSE)

# FIN!