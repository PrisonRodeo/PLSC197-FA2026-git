#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SODA 110N: Fall 2026
#
# Some code for some pictures...
#
# Date: 9/14/2026
#
# Each plot is turned into a .PDF file, like this:
#
# pdf("Name-of-PDF.pdf",7,6)
#   [code to draw the plot]
# dev.off()
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This reads the data off the Github repo:

df<-read.csv("https://github.com/PrisonRodeo/PLSC197-FA2026-git/raw/refs/heads/main/Class%20Data/WDI2019.csv")

# Subset the data to sub-saharan Africa only:

ssa<-df[df$Region=="Sub-Saharan Africa",]

wdi <- read.csv("https://github.com/PrisonRodeo/PLSC197-FA2026-git/raw/refs/heads/main/Class%20Data/WDI2019.csv",
                stringsAsFactors = FALSE)

ssa <- subset(wdi, Region == "Sub-Saharan Africa")

# Dotplot

pdf("DotPlot-26.pdf",12,9)
ssa<-ssa[order(ssa$PoliticalStability),]
dotchart(ssa$PoliticalStability, labels = ssa$country,
         pch=19,xlab = "Political Stability",
         main = "Political Stability, Sub-Saharan Africa")
abline(v=mean(ssa$PoliticalStability,na.rm=TRUE),lwd=1,lty=2)
dev.off()

# Histogram

pdf("Histogram-26.pdf",8,6)
hist(ssa$PoliticalStability,
     xlab = "Political Stability",
     main = "Political Stability, Sub-Saharan Africa")
dev.off()

# Scatterplot with ISO3 codes as the plotting characters

line<-lm(PoliticalStability~RuleOfLaw,data=ssa)
pdf("Scatterplot-26.pdf",8,6)
plot(ssa$RuleOfLaw, ssa$PoliticalStability, type = "n",
     xlab = "Rule of Law", ylab = "Political Stability",
     main = "Political Stability vs. Rule of Law, Sub-Saharan Africa")
text(ssa$RuleOfLaw, ssa$PoliticalStability, labels = ssa$ISO3, cex = 0.7)
abline(line)
dev.off()

# Finally, a fast map...
#
# Load a package:

install.packages("rworldmap")   # only needed once
library(rworldmap)

# Draw a map:

map_data <- joinCountryData2Map(ssa, joinCode = "ISO3", nameJoinColumn = "ISO3")

pdf("Choropleth-26.pdf",8,6)
mapCountryData(map_data,
               nameColumnToPlot = "PoliticalStability",
               mapRegion = "Africa",
               mapTitle = "Political Stability, Sub-Saharan Africa (2019)",
               colourPalette = c("darkorange","navy"))
dev.off()
