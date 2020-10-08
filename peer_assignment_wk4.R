# Exploratory Data Analysis>Week 4>Peer-graded Assignment:Course Project 2>

# Gregg Powell
# 5OCT2020

# Instructions:
# Fine particulate matter (PM2.5) is an ambient air pollutant for which there is strong evidence that it is harmful to human health. 
# In the United States, the Environmental Protection Agency (EPA) is tasked with setting national ambient air quality standards for fine PM and for 
# tracking the emissions of this pollutant into the atmosphere. Approximatly every 3 years, the EPA releases its database on emissions #
# of PM2.5. This database is known as the National Emissions Inventory (NEI). You can read more information about the NEI at the EPA National 
# Emissions Inventory web site.

# For each year and for each type of PM source, the NEI records how many tons of PM2.5 were emitted from that source over the course of the 
# entire year. The data that you will use for this assignment are for 1999, 2002, 2005, and 2008.


# PM2.5 Emissions Data (\color{red}{\verb|summarySCC_PM25.rds|}summarySCC_PM25.rds): 
# This file contains a data frame with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008. 
# For each year, the table contains number of tons of PM2.5 emitted from a specific type of source for the entire year. 
# Here are the first few rows.

# fips: A five-digit number (represented as a string) indicating the U.S. county
# SCC: The name of the source as indicated by a digit string (see source code classification table)
# Pollutant: A string indicating the pollutant
# Emissions: Amount of PM2.5 emitted, in tons
# type: The type of source (point, non-point, on-road, or non-road)
# year: The year of emissions recorded

# Source Classification Code Table (\color{red}{\verb|Source_Classification_Code.rds|}Source_Classification_Code.rds): 
# This table provides a mapping from the SCC digit strings in the Emissions table to the actual name of the PM2.5 source. 
# The sources are categorized in a few different ways from more general to more specific and you may choose to explore whatever 
# categories you think are most useful. For example, source “10100101” is known as “Ext Comb /Electric Gen /Anthracite Coal /Pulverized Coal”.

# You can read each of the two files using the \color{red}{\verb|readRDS()|}readRDS() function in R. 
# For example, reading in each file can be done with the following code:

# as long as each of those files is in your current working directory (check by calling \color{red}{\verb|dir()|}dir() and see 
# if those files are in the listing).

#__________________________________________________________________________________________________#
# The overall goal of this assignment is to explore the National Emissions Inventory database and see what it say about fine particulate 
# matter pollution in the United states over the 10-year period 1999–2008. You may use any R package you want to support your analysis.

# Questions
# You must address the following questions and tasks in your exploratory analysis. For each question/task you will need to make a single plot. 
# Unless specified, you can use any plotting system in R to make your plot.








#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

setwd("C:/_TEMP/_R_WORK_TEMP/Johns_Hopkins_Data_Science/Module 4-Exploratory_Data_Analysis/week4/peer_assignment")
library("ggplot2")
library("data.table")
library("dplyr")

PM2.5ED <- readRDS("summarySCC_PM25.rds")
SCCT <- readRDS("Source_Classification_Code.rds")
summary(PM2.5ED)
head(PM2.5ED)
class(PM2.5ED)
PM2.5ED
summary(SCCT)
head(SCCT)
class(SCCT)
SCCT
class(SCCT)

#____________________________________Part 1__________________________________________________________________________complete
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, 
# make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

Year99 <- subset(PM2.5ED, PM2.5ED$year==1999)
Year02 <- subset(PM2.5ED, PM2.5ED$year==2002)
Year05 <- subset(PM2.5ED, PM2.5ED$year==2005)
Year08 <- subset(PM2.5ED, PM2.5ED$year==2008)

SumEm_Year99 <- sum(Year99$Emissions)/1000
SumEm_Year02 <- sum(Year02$Emissions)/1000
SumEm_Year05 <- sum(Year05$Emissions)/1000
SumEm_Year08 <- sum(Year08$Emissions)/1000

SumEm
SumEm <- data.frame(SumEm_Year99, SumEm_Year02, SumEm_Year05, SumEm_Year08)

SumEmTr <- transpose(SumEm, keep.names = NULL)

SumEmTr
Year <- c(1999, 2002, 2005, 2008)

SumEmNew <- cbind(Year, SumEmTr)

names(SumEmNew)[2] <- "Emissions"

png(file="C:/_TEMP/_R_WORK_TEMP/Johns_Hopkins_Data_Science/Module 4-Exploratory_Data_Analysis/week4/peer_assignment/peer_assignment_plot1.png",
    width=600, height=350)
plot(SumEmNew$Year, SumEmNew$Emissions, col="red", xlab="Year", ylab="Emissions", main="total PM2.5 Emissions x1000")
dev.off()

#___________________________________________Part 2_________________________________________________________complete
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.

Baltimore_Year99 <- subset(PM2.5ED, PM2.5ED$year==1999 & fips==24510)
Baltimore_Year08 <- subset(PM2.5ED, PM2.5ED$year==2008 & fips==24510)
SumEm_Balt_Year99 <- sum(Baltimore_Year99$Emissions)/1000
SumEm_Balt_Year08 <- sum(Baltimore_Year08$Emissions)/1000

SumEm_Balt <- data.frame(SumEm_Balt_Year99, SumEm_Balt_Year08)

Sum_BaltTr <- transpose(SumEm_Balt, keep.names = NULL)
 Year2 <- c(1999, 2008)

 Sum_BaltTrNew <- cbind(Year2, Sum_BaltTr) 
 names(Sum_BaltTrNew)[2] <- "Emissions"
 names(Sum_BaltTrNew)[1] <- "Year"

 png(file="C:/_TEMP/_R_WORK_TEMP/Johns_Hopkins_Data_Science/Module 4-Exploratory_Data_Analysis/week4/peer_assignment/peer_assignment_plot2.png",
     width=600, height=350)
 plot(Sum_BaltTrNew$Year, Sum_BaltTrNew$Emissions, col="red", xlab="Year", ylab="Emissions", main="total PM2.5 Baltimore Emissions x1000")
 dev.off()
 
 #Answer-Yes. Emissions in 1999 exceeded 3.2K, in 2008, < 2k. 
 
 
 #___________________________________________Part 3_________________________________________________________complete


 # Of the four types of sources indicated by the (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen 
 # decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions 
 # from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

 Sum_BaltTrNew2_grp <- subset(PM2.5ED, PM2.5ED$fips == "24510")
 Sum_BaltTrNew2_grp
 Sum_BaltTrNew2_grp_sub <- aggregate(Emissions ~ year + type, Sum_BaltTrNew2_grp, sum)
 Sum_BaltTrNew2_grp_sub
 
 plot3 <- ggplot(Sum_BaltTrNew2_grp_sub, aes(year, Emissions, col = type)) + geom_line() + geom_point() + ggtitle(expression("Total Emissions in Baltimore by Year/Source-Type")) + xlab("Year") +
   ylab(expression("Total Baltimore Emissions")) + scale_colour_discrete(name = "source groups") + xlim(1998,2010) + ylim(0,3000)
 
 ggsave("peer_assignment_plot3.png", plot = plot3)
 
 #___________________________________________Part 4_________________________________________________________complete
 
  # Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
 
 SCCT_Coal <- SCCT[grepl("coal", SCCT$Short.Name, ignore.case = T),]
 SCCT_Coal
 PM2.5ED_coal <- PM2.5ED[PM2.5ED$SCC %in% SCCT_Coal$SCC,]
 PM2.5ED_coal
 total_Coal_Emissions <- aggregate(Emissions ~ year + type, PM2.5ED_coal, sum)
 total_Coal_Emissions
 
 plot4 <- ggplot(total_Coal_Emissions, aes(year, Emissions, col = type)) + geom_line() + geom_point() + ggtitle(expression("Total Coal Emission in USA by Source-Type and Year")) +
   xlab("Year") + ylab(expression("US Coal Emission")) + scale_colour_discrete(name = "source groups") + xlim(1998,2010) + ylim(0,600000) + 
    ggsave("total_coal_emissions_99.10.png", width = 8, height = 8, units = "in")
 
 ggsave("peer_assignment_plot4.png", plot = plot4)
 
 
 #______________________________________________Part 5_________________________________________________________complete
 #  How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
 
 SCCT_Vehicles <- SCCT [grepl('vehicle', SCCT$SCC.Level.Two, ignore.case = T),] 
 SCCT_Vehicles
 
 Tot_Emi_Balt_Veh <- PM2.5ED %>%
   filter(fips == "24510") %>%
   select(SCC, fips, Emissions, year) %>%
   inner_join(SCCT_Vehicles, by = "SCC") %>%
   group_by(year) %>%
   summarise(Total_Emissions = sum(Emissions, na.rm = TRUE)) %>%
   select(year, Total_Emissions)
 
 Tot_Emi_Balt_Veh
 
 plot5 <- ggplot(Tot_Emi_Balt_Veh, aes(factor(year), Total_Emissions)) +
   geom_bar(stat = "identity", fill = "blue", width = 0.5) +
   labs(x = "Years", y = "Motor Vehicle Emissions (Tons)", title = "Total Motor Vehicle Emissions 1999-2008 Baltimore") +
   theme(plot.title = element_text(size = 17),
         axis.title.x = element_text(size = 13),
         axis.title.y = element_text(size = 13))
 
   ggsave("peer_assignment_plot5.png", plot=plot5)
 
 
 #______________________________________________Part 6___________________________________________________________complete
 #  Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources 
 # in Los Angeles County, California fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?
 
 Balt_LA_Veh <- subset(PM2.5ED, PM2.5ED$fips %in% c("24510","06037") & PM2.5ED$type == "ON-ROAD")
 Balt_LA_Veh_Tot <- aggregate(Emissions ~ year + fips, Balt_LA_Veh, sum)
 
 plot6 <- ggplot(Balt_LA_Veh_Tot, aes(year, Emissions, col = fips)) + geom_line() + geom_point() +
    ggtitle(expression("Baltimore and Los Angeles County Motor Vehicle Emissions")) +
    labs(x = "Years", y = expression("Motor Vehicle Emissions") ) +
    scale_colour_discrete(name = "City", labels = c("Los Angeles", "Baltimore")) +
    theme(legend.title = element_text(face = "bold"))
 
 ggsave("peer_assignment_plot6.png", plot=plot6)
 
 
 
 
  
