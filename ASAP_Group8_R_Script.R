#-------------------------------------------------------------------------
# BM01BAM Group project - Data preparation 
#-------------------------------------------------------------------------

# Empty the memory
remove(list=ls())
cat("\f")

#-------------------------------------------------------------------------
# Load libraries
#-------------------------------------------------------------------------
library(stargazer)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(plyr)

#--------------------------------------------------------------------
# Define folder structure
#--------------------------------------------------------------------
dir <- "/Users/yuansiqi/Downloads/BAM/advanced statistics & programming/group project/"

dirData <- paste0(dir, "Data/")
dirProg <- paste0(dir, "Programs/")
dirRslt <- paste0(dir, "Results/")

#-------------------------------------------------------------------------
# Read the data
#-------------------------------------------------------------------------
movData2020 <- read.csv(file=paste0(dirData,"2020_NL_Region_Mobility_Report.csv"))
movData2021 <- read.csv(file=paste0(dirData,"2021_NL_Region_Mobility_Report.csv"))
strData <- read.csv(file=paste0(dirData,"covid-stringency-index.csv"))
VacciData <- read.csv(file=paste0(dirData,"owid-covid-data.csv"), header = TRUE, sep = ",")
caseData <- read.csv(file=paste0(dirData,"COVID-19_aantallen_gemeente_per_dag.csv"), 
                     header = TRUE, sep = ";")

#-------------------------------------------------------------------------
# prepare mobility data
#-------------------------------------------------------------------------
provinces <- c("North Holland", "South Holland", "Zeeland", "North Brabant", "Utrecht", 
               "Flevoland", "Friesland", "Groningen", "Drenthe", "Overijssel", 
               "Gelderland", "Limburg")

movData2020$date <- as.Date(movData2020$date)
movData2021$date <- as.Date(movData2021$date)

movData <- rbind(movData2020, movData2021) %>%
        filter(sub_region_1 %in% provinces) %>%
        filter(sub_region_2 == "") %>%
        filter(date >= as.Date("2020-09-11")) %>%
        filter(date <= as.Date("2021-09-10"))

colnames(movData)[colnames(movData) == "sub_region_1"]<- "province"
colnames(movData)[colnames(movData) == "retail_and_recreation_percent_change_from_baseline"]<- 
        "retail_and_recreation"
colnames(movData)[colnames(movData) == "grocery_and_pharmacy_percent_change_from_baseline"]<- 
        "grocery_and_pharmacy"
colnames(movData)[colnames(movData) == "parks_percent_change_from_baseline"]<- "parks"
colnames(movData)[colnames(movData) == "transit_stations_percent_change_from_baseline"]<- 
        "transit_stations"
colnames(movData)[colnames(movData) == "workplaces_percent_change_from_baseline"]<- "workplaces"

#-------------------------------------------------------------------------
# prepare case ratio data
#-------------------------------------------------------------------------
#case data
str(caseData) 
caseData$date <- as.Date(caseData$Date_of_publication)

caseData <- aggregate(caseData$Total_reported,
                      by = list(caseData$date, caseData$Province),
                      FUN = sum)
colnames(caseData) <- c("date", "province", "newcase")

caseData <- caseData %>%
        filter(date >= as.Date("2020-08-28")) %>%
        filter(date <= as.Date("2021-09-24")) 

caseData$province[caseData$province == "Fryslân"] <- "Friesland"
caseData$province[caseData$province == "Noord-Brabant"] <- "North Brabant"
caseData$province[caseData$province == "Noord-Holland"] <- "North Holland"
caseData$province[caseData$province == "Zuid-Holland"] <- "South Holland"

# the data is already sorted, so we can run following syntax directly
caseRatio <- {}
for (i in 1:(nrow(caseData)-14)){
        case14days <- 0
        for (j in 1:14){
                case14days <- case14days + caseData[j+i-1, "newcase"]
        }
        Ratio <- caseData[i+14, "newcase"]/(case14days/14)
        caseRatio <- c (caseRatio, Ratio)
}

caseRatio <- c(caseRatio, rep(0, 14))
caseData2 <- mutate(caseData, caseRatio = caseRatio)
caseLag <- filter(caseData2, date <= as.Date("2021-09-10")) %>%
        filter(date >= as.Date("2020-09-11"))
caseNoLag <- filter(caseData2, date <= as.Date("2021-08-27"))
colnames(caseNoLag)[colnames(caseNoLag) == "caseRatio"]<- "caseRatio0"
caseNoLag$date <- caseNoLag$date+14
#-------------------------------------------------------------------------
# prepare control variables
#-------------------------------------------------------------------------
# stringency index
strData <- strData %>%
        filter(Entity == "Netherlands") %>%
        filter(Day >= as.Date("2020-09-11")) %>%
        filter(Day <= as.Date("2021-09-10")) 
strData$date <- as.Date(strData$Day)

# vaccination index
VacciData <- VacciData %>%
        filter(location == "Netherlands") %>%
        filter(date >= as.Date("2020-09-11")) %>%
        filter(date <= as.Date("2021-09-10")) 
VacciData$date <- as.Date(VacciData$date)

VacciData[115,"people_vaccinated"] <- 0
VacciData[136,"people_fully_vaccinated"] <- 0
VacciData$VacciPartly[1]<-0
VacciData$VacciFully[1]<-0

library(zoo)
VacciData2 <- data.frame(VacciData$date, na.approx(VacciData$people_vaccinated, rule = 2), 
                         na.approx(VacciData$people_fully_vaccinated, rule = 2))

colnames(VacciData2)[colnames(VacciData2) == "na.approx.VacciData.people_vaccinated..rule...2."]<- 
        "VacciPartly"
colnames(VacciData2)[colnames(VacciData2) == "na.approx.VacciData.people_fully_vaccinated..rule...2."]<- 
        "VacciFully"
colnames(VacciData2)[colnames(VacciData2) == "VacciData.date"]<- "date"


VacciData2$VacciPartly <- VacciData2$VacciPartly/174746.77
VacciData2$VacciFully <- VacciData2$VacciFully/174746.77


# season
dfSeason <- data.frame(season = c(rep("Winter", 3), rep("Summer", 3), 
                                  rep("Spring", 3), rep("Fall", 3)),
                       month = month.name[c(11,12,1, 5:7, 2:4, 8:10)],
                       stringsAsFactors = F)

#-------------------------------------------------------------------------
# combining
#-------------------------------------------------------------------------
jointdataset <- movData %>%
        merge(caseNoLag, by = c("date","province")) %>%
        merge(caseLag, by = c("date","province")) %>%
        merge(strData, by = "date") %>%
        merge(VacciData2, by = "date")

finalData <- jointdataset[, c("date", "province", "retail_and_recreation", "grocery_and_pharmacy", 
                      "parks", "transit_stations", "workplaces", "caseRatio", "caseRatio0", "stringency_index",
                      "VacciPartly", "VacciFully")]

finalData$season <- dfSeason$season[match(format(finalData$date, "%B"), dfSeason$month)]

#-------------------------------------------------------------------------
# summary of data
#-------------------------------------------------------------------------

summary(finalData)
stargazer(finalData)

write.csv(finalData, file=paste0(dirRslt,'finalData.csv'))

#-------------------------------------------------------------------------
# Testing linear regression assumptions
#-------------------------------------------------------------------------
V2data <- read.csv(file=paste0(dirRslt,'finalData.csv'))

library(ggplot2)
library(GGally)
library(car)
library(MASS)
library(AER)

mdl1 <- lm(caseRatio ~ retail_and_recreation + grocery_and_pharmacy + parks + transit_stations + 
                   workplaces + stringency_index + VacciPartly + VacciFully + season, 
           data = V2data)
mdl2 <- lm(retail_and_recreation ~ caseRatio0 + stringency_index + VacciPartly + VacciFully 
           + season, data = V2data)
mdl3 <- lm(grocery_and_pharmacy ~ caseRatio0 + stringency_index + VacciPartly + VacciFully 
           + season, data = V2data)
mdl4 <- lm(parks ~ caseRatio0 + stringency_index + VacciPartly + VacciFully 
           + season, data = V2data)
mdl5 <- lm(transit_stations ~ caseRatio0 + stringency_index + VacciPartly + VacciFully 
           + season, data = V2data)
mdl6 <- lm(workplaces ~ caseRatio0 + stringency_index + VacciPartly + VacciFully 
           + season, data = V2data)

###
#####Linearity#####
###

ggpairs(V2data, columns = 4:12)

ggplot(V2data, aes(retail_and_recreation, caseRatio)) + geom_point(color="grey") + stat_smooth(method="loess") + stat_smooth(method="lm", color="red", fill="red", alpha=.25)
ggplot(V2data, aes(grocery_and_pharmacy, caseRatio)) + stat_smooth(method="loess") + stat_smooth(method="lm", color="red", fill="red", alpha=.25) + geom_point()
ggplot(V2data, aes(parks, caseRatio)) + stat_smooth(method="loess") + stat_smooth(method="lm", color="red", fill="red", alpha=.25) + geom_point()
ggplot(V2data, aes(transit_stations, caseRatio)) + stat_smooth(method="loess") + stat_smooth(method="lm", color="red", fill="red", alpha=.25) + geom_point()
ggplot(V2data, aes(workplaces, caseRatio)) + geom_point(color="grey") + stat_smooth(method="loess") + stat_smooth(method="lm", color="red", fill="red", alpha=.25)
ggplot(V2data, aes(stringency_index, caseRatio)) + geom_point(color="grey") + stat_smooth(method="loess") + stat_smooth(method="lm", color="red", fill="red", alpha=.25)
ggplot(V2data, aes(VacciPartly, caseRatio)) + geom_point(color="grey") + stat_smooth(method="loess") + stat_smooth(method="lm", color="red", fill="red", alpha=.25)
ggplot(V2data, aes(VacciFully, caseRatio)) + geom_point(color="grey") + stat_smooth(method="loess") + stat_smooth(method="lm", color="red", fill="red", alpha=.25)

#Since the loess smoother (blue) roughly approximates the linear line (red), the linearity assumption has been met
#Note: the VacciFully variable in on the border of meeting the assumption

###
#####Homoscedasticity test: Breusch-pagan test #####

#for double check
lmtest::bptest(mdl1)
lmtest::bptest(mdl2)
lmtest::bptest(mdl3)
lmtest::bptest(mdl4)
lmtest::bptest(mdl5)
lmtest::bptest(mdl6)

## all the models violate homoscedasticity assumption

###
#####Normality#####
###

qqPlot(mdl1, main="QQ Plot")
qqPlot(mdl2, main="QQ Plot")
qqPlot(mdl3, main="QQ Plot")
qqPlot(mdl4, main="QQ Plot")
qqPlot(mdl5, main="QQ Plot")
qqPlot(mdl6, main="QQ Plot")

#All models violated because the points fall far outside the main lined area with a large curve.
#This is not a major issue due to our large sample size.

###
#####Multicollinearity#####
###
vif1 <- vif(mdl1)
stargazer(vif1, type = "text")
vif2 <- vif(mdl2)
stargazer(vif2, type = "text")
vif3 <- vif(mdl3)
stargazer(vif3, type = "text")
vif4 <- vif(mdl4)
stargazer(vif4, type = "text")
vif5 <- vif(mdl5)
stargazer(vif5, type = "text")
vif6 <- vif(mdl6)
stargazer(vif6, type = "text")

#In all models the variables VacciPartly and VacciFully violate the assumption (GVIFDf > 5).
#This could mean we should remove one of the vaccination variables. 

# variable VacciPartly is removed 
V2data$season <- relevel(factor(V2data$season), ref= 3)        

mdl1 <- caseRatio ~ retail_and_recreation + grocery_and_pharmacy + parks + transit_stations + 
                   workplaces + stringency_index + VacciFully + season
mdl2 <- retail_and_recreation ~ caseRatio0 + stringency_index + VacciFully + season
mdl3 <- grocery_and_pharmacy ~ caseRatio0 + stringency_index +  VacciFully + season
mdl4 <- parks ~ caseRatio0 + stringency_index + VacciFully + season
mdl5 <- transit_stations ~ caseRatio0 + stringency_index +  VacciFully + season
mdl6 <- workplaces ~ caseRatio0 + stringency_index + VacciFully + season

vif1 <- vif(lm(mdl1, data = V2data))
stargazer(vif1, type = "text")
vif2 <- vif(lm(mdl2, data = V2data))
stargazer(vif2, type = "text")
vif3 <- vif(lm(mdl3, data = V2data))
stargazer(vif3, type = "text")
vif4 <- vif(lm(mdl4, data = V2data))
stargazer(vif4, type = "text")
vif5 <- vif(lm(mdl5, data = V2data))
stargazer(vif5, type = "text")
vif6 <- vif(lm(mdl6, data = V2data))
stargazer(vif6, type = "text")

# Multicollinearity issue solved

#-------------------------------------------------------------------------
# Data Analysis
#-------------------------------------------------------------------------
library(plm)
rsltFE1 <- plm(mdl1, data = V2data, index=c("province", "date"), model = "within")
rsltFE2 <- plm(mdl2, data = V2data, index=c("province", "date"), model = "within")
rsltFE3 <- plm(mdl3, data = V2data, index=c("province", "date"), model = "within")
rsltFE4 <- plm(mdl4, data = V2data, index=c("province", "date"), model = "within")
rsltFE5 <- plm(mdl5, data = V2data, index=c("province", "date"), model = "within")
rsltFE6 <- plm(mdl6, data = V2data, index=c("province", "date"), model = "within")

rsltPO1 <- plm(mdl1, data = V2data, index=c("province", "date"), model = "pooling")
rsltPO2 <- plm(mdl2, data = V2data, index=c("province", "date"), model = "pooling")
rsltPO3 <- plm(mdl3, data = V2data, index=c("province", "date"), model = "pooling")
rsltPO4 <- plm(mdl4, data = V2data, index=c("province", "date"), model = "pooling")
rsltPO5 <- plm(mdl5, data = V2data, index=c("province", "date"), model = "pooling")
rsltPO6 <- plm(mdl6, data = V2data, index=c("province", "date"), model = "pooling")

rsltRE1 <- plm(mdl1, data = V2data, index=c("province", "date"), model = "random")
rsltRE2 <- plm(mdl2, data = V2data, index=c("province", "date"), model = "random")
rsltRE3 <- plm(mdl3, data = V2data, index=c("province", "date"), model = "random")
rsltRE4 <- plm(mdl4, data = V2data, index=c("province", "date"), model = "random")
rsltRE5 <- plm(mdl5, data = V2data, index=c("province", "date"), model = "random")
rsltRE6 <- plm(mdl6, data = V2data, index=c("province", "date"), model = "random")

# Evaluate the fixed effects model versus the pooled regression model
pooltest(rsltPO1, rsltFE1)
pooltest(rsltPO2, rsltFE2)
pooltest(rsltPO3, rsltFE3)
pooltest(rsltPO4, rsltFE4)
pooltest(rsltPO5, rsltFE5)
pooltest(rsltPO6, rsltFE6)
# Hausman test : compare random and fixed effects models .
phtest (rsltFE1 , rsltRE1 )
phtest ( rsltFE2 , rsltRE2)
phtest ( rsltFE3 , rsltRE3 )
phtest ( rsltFE4 , rsltRE4 )
phtest ( rsltFE5 , rsltRE5 )
phtest ( rsltFE6, rsltRE6 )

#applying robust standard errors 

seWhite1 <- sqrt (diag (vcovHC (rsltRE1, type = "HC0")))
seWhite2 <- sqrt (diag (vcovHC (rsltRE2, type = "HC0")))
seWhite3 <- sqrt (diag (vcovHC (rsltRE3, type = "HC0")))
seWhite4 <- sqrt (diag (vcovHC (rsltRE4, type = "HC0")))
seWhite5 <- sqrt (diag (vcovHC (rsltRE5, type = "HC0")))
seWhite6 <- sqrt (diag (vcovHC (rsltRE6, type = "HC0")))

rsltOLS <- stargazer(rsltRE1, rsltRE2, rsltRE3, rsltRE4, rsltRE5, rsltRE6, 
                     se = list(seWhite1, seWhite2, seWhite3,seWhite4, seWhite5, seWhite6),
                  align = TRUE, no.space = TRUE, intercept.bottom = FALSE, type="text")


##visualization 
ggplot(V2data, aes(x = retail_and_recreation, y = caseRatio)) +
        geom_point(aes(color = province), size = 2) +
        geom_smooth(method = "lm", se = FALSE, color = "black") +
        xlab("Visits frequency at retail_and_recreation locations") +
        ylab("Case ratio with 14 days lag") + 
        theme(axis.title = element_text(size = rel(1.5)),
              axis.text = element_text(size = rel(1.2))) +
        guides(color = guide_legend(override.aes = list(size = 5)))

ggplot(V2data, aes(x = caseRatio0 , y = retail_and_recreation)) +
        geom_point(aes(color = province), size = 2) +
        geom_smooth(method = "lm", se = FALSE, color = "black") +
        xlab("Present case ratio") +
        ylab("Visits frequency at retail_and_recreation locations") + 
        theme(axis.title = element_text(size = rel(1.5)),
              axis.text = element_text(size = rel(1.2))) +
        guides(color = guide_legend(override.aes = list(size = 5)))
