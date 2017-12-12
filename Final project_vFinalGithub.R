dir <- "C:/Users/Fabian/Box Sync/Uni/2017_Wintersemester/Data_Management/Final Project"
setwd(dir)
library(RSQLite)
library(readxl)
library(tidyverse)
library(dbplyr)
library(dplyr)
library(readr)
library(DBI)
library(dummies)
library(dummy)
library(plyr)
library(stargazer)
library(plm)
library(plotly)

### renaming column names
colnames_refugee <- c("Year", "Country", "Origin", "RSD.procedure","Total pending start",
                  "UNCHR assisted","Applied during year", "decision_recognized",
                  "decision_other", "Rejected", "Otherwise closed", "Total decisions",
                  "Total pending end year", "of.which.UNCHR.assisted")

colnames_population <- c("Year", "Country","Origin","Population.type","Value")
colnames_GDP <- c("GDP", "Code", "Country", "Country_Code", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")
colnames_GDPPC <- c("GDPPC", "Code", "Country", "Country_Code", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")


### loading data sets
refugee <- read.csv("unhcr_popstats_asylum_seekers.csv", 
                     skip=3, 
                     na.string=c("","-","*"),
                     col.names=colnames_refugee)

population <- read.csv("unhcr_popstats_population_type.csv",
                       skip=3,
                       na.string=c("","-","*"),
                       col.names=colnames_population)

GDP <- read.csv("World_Bank_GDP.csv", 
                na.strings = c("","-", "*", ".."),
                col.names = colnames_GDP)
  
GDPPC <- read.csv("World_Bank_GDPPC.csv",
                  na.strings = c("","-", "*", ".."),
                  col.names = colnames_GDPPC)

countries <- read.csv("Countries-Continents-csv.csv")



###############################################
##### Data cleaning and merging 
###############################################
### collapsing to only country and year 
## for population
pop <- population %>% 
  select(-Origin) %>% 
  group_by(Year, Country, Population.type) %>% 
  summarize_all(sum)
# spreadin the data 
pop <- pop %>% 
  spread(key=Population.type, value=Value)

## for refugee
ref <- refugee %>% 
  select(-Origin, -RSD.procedure) %>% 
  group_by(Year, Country) %>%
  summarize_all(sum, na.rm=TRUE)
  

### calculating acceptance rates 
#Create the acceptance rate
# At T-1 there are 265 pending decisions
# At T there are 2156 applications
# At T+1 there are 1235 pending decisions

# (T-1 + T) - T+1 = Total applications processed, which is column total decisions
# decision_recognized = all decisions recognized
# decision_other and Rejected = all decisions that were not recognized
# Acceptance rate is then decision_recognized divided by total.decisions
ref <- ref %>% 
  mutate(share_accept = decision_recognized/Total.decisions)

###### merging population and refugee, dropping all countries where we cannot calculate acceptance rates (i.e. inner join)
refpop <- inner_join(ref, pop, by=c("Year", "Country"))

############ adding GDP
## "gathering" the GDPs Dataset to be combined with population dataset
GDP_new <- GDP %>% 
  select(-GDP, -Code, -X2017, -Country_Code) %>% 
  gather(key = Year, value = GDP, X2000:X2016, na.rm = TRUE) %>% 
  mutate(Year = readr::parse_number(Year))

GDPPC_new <- GDPPC %>% 
  select(-GDPPC, -Code, -X2017, -Country_Code) %>% 
  gather(key = Year, value = GDPPC, X2000:X2016, na.rm = TRUE) %>% 
  mutate(Year = readr::parse_number(Year))

GDPS <- full_join(GDP_new, GDPPC_new, by=c("Country", "Year"))

###################### merging refpop and GDPS

# renaming countries to merge later   
GDPS$Country <- stringr::str_replace_all(GDPS$Country, "Cote d'Ivoire", "Côte d'Ivoire")
GDPS$Country <- stringr::str_replace_all(GDPS$Country, "Czech Republic", "Czech Rep.")
GDPS$Country <- stringr::str_replace_all(GDPS$Country, "United States", "United States of America")
GDPS$Country <- stringr::str_replace_all(GDPS$Country, "Slovak Republic", "Slovakia")
GDPS$Country <- stringr::str_replace_all(GDPS$Country, "Syrian Arab Republic", "Syrian Arab Rep.")
GDPS$Country <- stringr::str_replace_all(GDPS$Country, "Lao PDR", "Lao People's Dem. Rep.")
GDPS$Country <- stringr::str_replace_all(GDPS$Country, "Kyrgyz Republic", "Kyrgyzstan")
GDPS$Country <- stringr::str_replace_all(GDPS$Country, "Vietnam", "Viet Nam")
GDPS$Country <- stringr::str_replace_all(GDPS$Country, "Tanzania", "United Rep. of Tanzania")
GDPS$Country <- stringr::str_replace_all(GDPS$Country, "Moldova", "Rep. of Moldova")
GDPS$Country <- stringr::str_replace_all(GDPS$Country, "Central African Republic", "Central African Rep.")
GDPS$Country <- stringr::str_replace_all(GDPS$Country, "Bolivia", "Bolivia (Plurinational State of)")

# adding Kosovo and Serbia (adding the $ to not overwrite the Serbia entries twice )
GDPS$Country <- stringr::str_replace_all(GDPS$Country, "Kosovo", "Serbia and Kosovo (S/RES/1244 (1999))")
GDPS$Country <- stringr::str_replace_all(GDPS$Country, "Serbia$", "Serbia and Kosovo (S/RES/1244 (1999))")
GDPS <- GDPS %>% 
  group_by(Year, Country) %>%
  summarize_all(sum, na.rm=TRUE)

## checking what what the non-congruent states are (using anti-join), showing all countries in refpop which don't have a corresponding country in GDPS
anti <- anti_join(refpop, GDPS, by="Country") %>% 
  filter(Year==2001)
# --> countries which could not be found in GDPS: Macedonia, Iran, Rep. of Korea, Congo, Egypt, Venezuela, Yemen, Dem. Rep. of the Congo, Gambia, "China, Hong Kong SAR", 

# check which countries remain in the GDPS dataset 
anti <- anti_join(GDPS, refpop, by="Country") %>% 
  filter(Year==2001)
# --> the remaining countries in that list are either regional groups of countries, small Islands or micro-states

#################### merging GDPS and refpop
refpopGDPS <- inner_join(refpop, GDPS, by=c("Year", "Country"))
# what happens: observations drop from 2699 (in refpop) to 2400 (in final) 
# The dropped countries are countries which do not show up in the GDPS dataset: Macedonia, Iran, Rep. of Korea, Congo, Egypt, Venezuela, Yemen, Dem. Rep. of the Congo, Gambia, "China, Hong Kong SAR"


#################### creating a continent variable
### using a country-continent database (https://old.datahub.io/dataset/countries-continents/resource/aa08c34c-57e8-4e15-bd36-969bee26aba5)
### check for diverging names
countries$Country <- stringr::str_replace_all(countries$Country, "(.*)Serbia(.*)", "Serbia and Kosovo (S/RES/1244 (1999))")
countries$Country <- stringr::str_replace_all(countries$Country, "(.*)Kosovo(.*)", "Serbia and Kosovo (S/RES/1244 (1999))")
countries$Country <- stringr::str_replace_all(countries$Country, "(.*)Burma(.*)", "Myanmar")
countries$Country <- stringr::str_replace_all(countries$Country, "(.*)Burkina(.*)", "Burkina Faso")
countries$Country <- stringr::str_replace_all(countries$Country, "(.*)Ivory(.*)", "Côte d'Ivoire")
countries$Country <- stringr::str_replace_all(countries$Country, "(.*)Czech Republic(.*)", "Czech Rep.")
countries$Country <- stringr::str_replace_all(countries$Country, "(.*)United States(.*)", "United States of America")
countries$Country <- stringr::str_replace_all(countries$Country, "(.*)Slovak Republic(.*)", "Slovakia")
countries$Country <- stringr::str_replace_all(countries$Country, "(.*)Syria(.*)", "Syrian Arab Rep.")
countries$Country <- stringr::str_replace_all(countries$Country, "(.*)Lao(.*)", "Lao People's Dem. Rep.")
countries$Country <- stringr::str_replace_all(countries$Country, "(.*)Kyrg(.*)", "Kyrgyzstan")
countries$Country <- stringr::str_replace_all(countries$Country, "(.*)Viet(.*)", "Viet Nam")
countries$Country <- stringr::str_replace_all(countries$Country, "(.*)Tanz(.*)", "United Rep. of Tanzania")
countries$Country <- stringr::str_replace_all(countries$Country, "(.*)Mold(.*)", "Rep. of Moldova")
countries$Country <- stringr::str_replace_all(countries$Country, "Central African Republic", "Central African Rep.")
countries$Country <- stringr::str_replace_all(countries$Country, "(.*)Bolivia(.*)", "Bolivia (Plurinational State of)")
countries <- countries %>% 
  select(Continent, Country) %>% 
  distinct()


########### merging countries
final <- full_join(refpopGDPS, countries, by="Country")

final <- final %>% 
  mutate(asylum_seekers =`Asylum-seekers`) %>% 
  mutate(IDP =`Internally displaced persons`) %>% 
  mutate(others =`Others of concern`) %>% 
  mutate(ref =`Refugees (incl. refugee-like situations)`) %>% 
  mutate(returnidp =`Returned IDPs`) %>% 
  mutate(returnref =`Returned refugees`) %>% 
  mutate(stateless =`Stateless persons`) %>% 
  select(-`Stateless persons`, -`Returned refugees`, `Returned IDPs`, `Refugees (incl. refugee-like situations)`, `Others of concern`, `Internally displaced persons`, `Asylum-seekers` )

  
final_clean <- final %>% 
  gather(key=Population.Type, value = rank, asylum_seekers:stateless, na.rm=TRUE) %>% 
  arrange(Year, Country)


###########################################
#Descriptive analysis
###########################################

#Create log variables 
final$ln.GDP = log(final$GDP)
final$ln.GDPPC=log(final$GDPPC)
final$ln.apply = log(final$Applied.during.year)
final$ln.asylum=log(final$`asylum_seekers`)
final$ln.IDP=log(final$`IDP`)
final$ln.others=log(final$`others`)
final$ln.ref=log(final$`ref`)
final$ln.returnidp=log(final$`returnidp`)
final$ln.returnref=log(final$`returnref`)
final$ln.stateless=log(final$`stateless`)
final2015 <- final %>% 
  filter(Year=="2015")

# Plot 1
p <- ggplot(data = final_clean,
            mapping = aes(x = Year, fill=Population.Type ))
p + geom_bar() +  + 
  ggtitle ("Plot 1: Refugees by population type across time") +
  xlab ("Time") +
  ylab("Count (x 10,000)")

#Plot 2
refugeesbar<- final %>% 
  select(Year,Country,ref,Continent) %>% 
  filter(Year=="2015") %>% 
  filter(ref>75000)
ggplot(data=refugeesbar, mapping= aes(x=reorder(Country,ref),
                                      y=ref,fill=Continent))+
  geom_bar(stat="identity") + 
  coord_flip() +   
  ggtitle ("Plot 2: Countries with highest refugees influx in 2015") +
  xlab ("Number of refugees") +
  ylab("Countries")

#Plot 3
final2015asylum <- final %>% 
  select(Year,Country,asylum_seekers,Continent) %>% 
  filter(Year=="2015") %>% 
  filter(asylum_seekers>10000)

Plot_3<- ggplot(data=final2015asylum, mapping= aes(x=reorder(Country,asylum_seekers),y=asylum_seekers,fill=Continent))+
  geom_bar(stat="identity") + 
  coord_flip() + 
  ggtitle ("Plot 3: Countries with most asylum seekers in 2015") +
  xlab ("Asylum seekers") +
  ylab("Countries")
Plot_3


#Plot 4
final2015apply<- final %>% 
  select(Year,Country,Applied.during.year,Continent) %>% 
  filter(Year=="2015") %>% 
  filter(Applied.during.year>10000)

Plot4 <- ggplot(data=final2015apply, mapping= aes(x=reorder(Country,Applied.during.year),y=Applied.during.year,fill=Continent))+
  geom_bar(stat="identity") + 
  coord_flip() + 
  ggtitle ("Plot 4: Countries with most asylum applications in 2015") +
  xlab ("Applications throughout 2015") +
  ylab("Countries")
Plot4


#Plot 5

final2015 <- final %>%
  filter(Year=="2015")

Plot5 <- ggplot(data = final) + 
  geom_point(mapping = aes(x = ln.apply, y = share_accept)) +
  theme_bw() + 
  ggtitle ("Plot 5: Relation between number of applications and share of acceptance") +
  xlab ("Acceptance share") +
  ylab("Log of applications")
Plot5

#Plot 6
finalgermany <- final %>% 
  filter(Country=="Germany")

Plot6 <- ggplot(data=finalgermany) +
  geom_smooth(mapping = aes(x = Year, y = share_accept),se=F) +
  theme_bw() + 
  ggtitle ("Plot 6: Germany's acceptance rate across years") +
  xlab ("Time") +
  ylab("Acceptance share")
Plot6

#Plot7
final2015 <- final %>% 
  filter(Year=="2015")
accgdp <- ggplot(data = final2015, mapping = aes(x = ln.GDP, y = share_accept, color = Continent, size=Applied.during.year, label=Country)) +
  geom_point() +
  geom_text(aes(label=Country),hjust=0, vjust=1) + 
  ggtitle("Plot 7: Acceptance rate in relation to GDP across continents") +
  xlab("Log of GDP") +
  ylab("Acceptance share")
accgdp

########################################################
##Running a statistical model
########################

finalEurope <- final %>% 
  filter(Continent=="Europe")

mod.fe <-plm(share_accept ~ ln.GDP + ln.asylum + ln.ref ,
             data = final, model='within', index=c("Country","Year"))

mod.fe2<- coeftest(mod.fe, vcov=vcovHC(mod.fe, cluster="group"))

mod.fe.sub <- plm(share_accept ~ ln.GDP + ln.asylum + ln.ref , 
                  data=finalEurope, model='within', index=c("Country","Year"))


mod.fe.sub.cluster <- coeftest(mod.fe.sub, vcov=vcovHC(mod.fe.sub,cluster="group"))

stargazer(list(mod.fe, mod.fe2, mod.fe.sub, mod.fe.sub.cluster),
          type="html",header=F,float=F,single.row=T, font.size='tiny', 
          dep.var.caption = 'share_accept', 
          title = "Share of refugee acceptance model",
          column.labels = c("Fixed Effects Global", "with clustered SEs",
                            "Fixed Effects Europe", "with clustered SEs"),
          out="model5.html")
















