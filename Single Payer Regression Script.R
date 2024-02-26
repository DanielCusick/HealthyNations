# Set up paths and Import data
# Libraries
library(data.table)
library(car)
library(gam)
library(faraway)
library(kableExtra)
library(zoo)
library(fpp3)
# Paths
projpath <- "C:/Users/dancu/Documents/Fall2022_ADEC743002"
# does this exist?
dir.exists(projpath)

# establish subfolders
rawdata <- file.path(projpath, "RawData")
finaldata <- file.path(projpath, "FinalData")
code <- file.path(projpath, "Code")
output <- file.path(projpath, "Output")
interdata <- file.path(projpath, "IntermediateData")
docs <- file.path(projpath, "Docs")

# Read in and review Avoidable Mortality
avoid<-read.csv(file.path(rawdata, "OECD_AvoidMort.csv"))
avoid<-data.table(avoid)
names(avoid)
nrow(avoid)
sapply(avoid, function(x) sum(is.na(x)))
unique(avoid$Country)
unique(avoid$Variable)
names(avoid)[names(avoid) == "Value"] <- "am"
summary(avoid$am)

# Read in and Review Life Expectancy
life<-read.csv(file.path(rawdata, "OECD_LE.csv"))
life<-data.table(life)
names(life)
nrow(life)
sapply(life, function(x) sum(is.na(x)))
unique(life$Country)
unique(life$Variable)
names(life)[names(life) == "Value"] <- "le"
summary(life$le)
life

# Columns 6 Country, 8 Year, 9 Value have what we need
avoid2<-avoid[,c(6,8,9)]
life2<-life[,c(6,8,9)]
# merge
health<-merge(avoid2,life2, all=TRUE)

# Read in and review lifestyle data
style<-read.csv(file.path(rawdata, "OECD_lifestyle.csv"))
style<-data.table(style)
names(style)
nrow(style)
sapply(style, function(x) sum(is.na(x)))
unique(style$Country)
unique(style$Variable)

# Trim & rename columns, merge data
sugar<-style[Variable=="Sugar supply"]
fruit<-style[Variable=="Fruits supply"]
veggies<-style[Variable=="Vegetables supply"]
alcohol<-style[Variable=="Alcohol consumption"]
tobacco<-style[Variable=="Tobacco consumption"]
obesepop<-style[Variable=="Obese population, self-reported"]
sugar2<-sugar[,c(6,8,9)]
names(sugar2)[names(sugar2) == "Value"] <- "sugkilospc"
sugar2
fruit2<-fruit[,c(6,8,9)]
names(fruit2)[names(fruit2) == "Value"] <- "frukilospc"
fruit2
veggies2<-veggies[,c(6,8,9)]
names(veggies2)[names(veggies2) == "Value"] <- "vegkilospc"
veggies2
tobacco2<-tobacco[,c(6,8,9)]
names(tobacco2)[names(tobacco2) == "Value"] <- "pctdailysmokers"
tobacco2
alcohol2<-alcohol[,c(6,8,9)]
names(alcohol2)[names(alcohol2) == "Value"] <- "alcliterspc"
alcohol2
obesepop2<-obesepop[,c(6,8,9)]
names(obesepop2)[names(obesepop2) == "Value"] <- "obspct"
obesepop2

health2<-merge(health, sugar2, all=TRUE)
health3<-merge(health2,fruit2, all=TRUE)
health4<-merge(health3,veggies2, all=TRUE)
health5<-merge(health4,tobacco2, all=TRUE)
sapply(health5, function(x) sum(is.na(x)))
health6<-merge(health5,alcohol2, all=TRUE)
health7<-merge(health6,obesepop2, all=TRUE)
health7
sapply(health7, function(x) sum(is.na(x)))
health8<-health7

# Impute missing obesity % with mean by country
round(mean(health8[Country=="United States"]$obspct, na.rm=TRUE),1)
round(mean(health8[Country=="Australia"]$obspct, na.rm=TRUE),1)

for (i in unique(health8$Country)){
  health8$obspct[is.na(health8$obspct)&health8$Country==i]<-round(mean(health8[Country==i]$obspct, na.rm=TRUE),1)
}

# Impute missing % of daily smokers with mean by country
round(mean(health8[Country=="United States"]$pctdailysmokers, na.rm=TRUE),1)
round(mean(health8[Country=="Australia"]$pctdailysmokers, na.rm=TRUE),1)

for (i in unique(health8$Country)){
  health8$pctdailysmokers[is.na(health8$pctdailysmokers)&health8$Country==i]<-round(mean(health8[Country==i]$pctdailysmokers, na.rm=TRUE),1)
}

# Impute missing alcohol consumption with mean by country
health8[is.na(health8$alcliterspc)]

for (i in unique(health8$Country)){
  health8$alcliterspc[is.na(health8$alcliterspc)&health8$Country==i]<-round(mean(health8[Country==i]$alcliterspc, na.rm=TRUE),1)
}

# Continue to inspect missing values
sapply(health8, function(x) sum(is.na(x)))
health8
health8[is.na(health8$vegkilospc)]
sapply(health8[Year<2020], function(x) sum(is.na(x)))
# Data before 2020 is more complete
health8[is.na(health8$am)]
health8[is.na(health8$le)]

# Stop table after 2019
health9<-health8[Year<2020]
health9[is.na(health9$am)]
health9[is.na(health9$le)]

# Impute missing avoidable mortality with mean by country
for (i in unique(health8$Country)){
  health9$am[is.na(health9$am)&health9$Country==i]<-round(mean(health9[Country==i]$am, na.rm=TRUE),1)
}

health9[Country=="Norway"]
health9[Country=="France"]

# Read in and review other Well Being data 

well<-read.csv(file.path(rawdata, "OECD_wellbeing.csv"))
well<-data.table(well)
names(well)
nrow(well)
sapply(well, function(x) sum(is.na(x)))
unique(well$Country)
unique(well$Indicator)

# Clean up and merge 
homicide<-well[Indicator=="Homicides"]
pollution<-well[Indicator=="Air pollution"]
safety<-well[Indicator=="Feeling safe at night"]
safety2<-safety[TYPE_VAR=="AVERAGE"]

homicide2<-homicide[,c(2,16,17)]
names(homicide2)[names(homicide2) == "Value"] <- "homper100K"
names(homicide2)[names(homicide2) == "Time"] <- "Year"
homicide2
pollution2<-pollution[,c(2,16,17)]
names(pollution2)[names(pollution2) == "Value"] <- "pollutpoppct"
names(pollution2)[names(pollution2) == "Time"] <- "Year"
pollution2
unique(pollution2$Country)
safety3<-safety2[,c(2,16,17)]
names(safety3)[names(safety3) == "Value"] <- "feelsafepct"
names(safety3)[names(safety3) == "Time"] <- "Year"
safety3

health9
health10<-merge(health9, homicide2, all=TRUE)
health11<-merge(health10,pollution2)
health12<-merge(health11, safety3, all=TRUE)
sapply(health12, function(x) sum(is.na(x)))

# Impute missing homicides with with mean by country
health12[is.na(health12$homper100K)]

for (i in unique(health12$Country)){
  health12$homper100K[is.na(health12$homper100K)&health12$Country==i]<-round(mean(health12[Country==i]$homper100K, na.rm=TRUE),1)
}

for (i in unique(health12$Country)){
  health12$feelsafepct[is.na(health12$feelsafepct)&health12$Country==i]<-round(mean(health12[Country==i]$feelsafepct, na.rm=TRUE),1)
}

# Read in, review, trim, merge Road Injuries
roads<-read.csv(file.path(rawdata, "OECD_road.csv"))
roads<-data.table(roads)
names(roads)
nrow(roads)
sapply(roads, function(x) sum(is.na(x)))
unique(roads$Country)
unique(roads$Variable)
names(roads)[names(roads) == "Value"] <- "roadinjpm"
summary(roads$roadinjpm)
roads
roads2<-roads[,c(6,8,9)]
roads2

health13<-merge(health12,roads2, all=TRUE)
health13
sapply(health13, function(x) sum(is.na(x)))
health13[is.na(health13$roadinjpm)]
# Impute missing road injuries with mean by country
health13$roadinjpm[is.na(health13$roadinjpm)&health13$Country=="Australia"]<-round(mean(health13[Country=="Australia"]$roadinjpm, na.rm=TRUE),1)
health13[Country=="Australia"]

health[Country=="Norway"]
health2[Country=="Norway"]
health3[Country=="Norway"]
health4[Country=="Norway"]
health5[Country=="Norway"]
health6[Country=="Norway"]
health7[Country=="Norway"]
health8[Country=="Norway"]
health9[Country=="Norway"]
health10[Country=="Norway"]
health11[Country=="Norway"]
health12[Country=="Norway"]
health13[Country=="Norway"]

# Read in, review trim, merge Health Expenditure per capita
spend<-read.csv(file.path(rawdata, "OECD_spend.csv"))
spend<-data.table(spend)
names(spend)
unique(spend$Country)
unique(spend$Measure)
unique(spend$Function)
nrow(spend)
summary(spend$Year)
names(spend)[names(spend) == "Value"] <- "healthexppc"
summary(spend$healthexppc)
spend
spend2<-spend[,c(10,12,19)]
spend2

# Create expenditure per life-year column
health14<-merge(health13, spend2)
health14$expperle<-health14$healthexppc/health14$le
health14
sapply(health14, function(x) sum(is.na(x)))

# 9/22/23 I am going to update this data table to include OECD External causes of death figure
# This includes accidents, suicides, homicides and other violent causes of death
# After loading this in, I am going to remove homicide rate and road injury rate 
ext<-read.csv(file.path(rawdata, "OECD_external.csv"))
ext<-data.table(ext)
names(ext)
unique(ext$Country)
unique(ext$Measure)
nrow(ext)
summary(ext$Year)
names(ext)[names(ext) == "Value"] <- "extdeathper100k"
ext
ext2<-ext[,c(6,8,9)]
ext2
health15<-merge(health14, ext2, all=TRUE)
health15
sapply(health15, function(x) sum(is.na(x)))

#remove redundant homicide rate and road injuries columns
health15<-health15[,-c(11,14)]

# Impute missing external deaths with mean by country
health15[is.na(health15$extdeathper100k)]

for (i in unique(health15$Country)){
  health15$extdeathper100k[is.na(health15$extdeathper100k)&health15$Country==i]<-round(mean(health15[Country==i]$extdeathper100k, na.rm=TRUE),1)
}

health15[Country=="France"]
health15[Country=="Norway"]
health15[Country=="Sweden"]

# Read in, review, merge Payment System Classifiers
payers<-read.csv(file.path(rawdata, "Payer_Flags.csv"))
payers<-data.table(payers)
payers
names(payers)
names(payers)[names(payers) == "ï..Country"] <- "Country"

health16<-merge(health15,payers,by="Country")
health16$system<-as.factor(health16$system)
health16
str(health16)

health16[Country=="Germany"]

# View, save, read final health16 data as needed
# print(health16) %>% kbl() %>% kable_classic(full_width=F,html_font="Times New Roman")
# write.csv(health15,file.path(finaldata, "health15.csv"))
# write.csv(health16,file.path(finaldata, "health16.csv"))
# health16<-read.csv(file.path(finaldata, "health16.csv"))
# health16<-data.table(health16)
# str(health16)
# health16$system<-as.factor(health16$system)
# str(health16)
# names(health16)
# health16<-health16[,-1]

# Avoidable Mortality Linear Regression
lmod<-lm(am ~ . - Country - Year -le - expperle - healthexppc, health16)
summary(lmod)
plot(lmod, which=1)
plot(lmod, which=2)

# Life Expectancy Linear Regression
lmod2<-lm(le ~ . - Country - Year -am - expperle - healthexppc, health16)
summary(lmod2)
plot(lmod2, which=1)
plot(lmod2, which=2)

# Spot check data per slides
health16[Country=="Sweden"&Year==2017]
health16[Country=="Germany"&Year==2011]
health16[Country=="France"&Year==2013]
health16[Country=="United States"&Year==2013]
health16[Country=="France"&Year==2013]
