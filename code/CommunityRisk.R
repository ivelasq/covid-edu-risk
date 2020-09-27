library(tidyverse)
library(ggrepel)
library(viridis)


# ###forming .csv files for needed data calls, will not to keep, but saving in case issues arise
# setwd("~/Dropbox (Partners HealthCare)/R files/covid") ## good practice to set a working directory
# ## MA zipcode listing
# MAtowns<-covidtowns %>%
#   distinct(zipcode, .keep_all = TRUE) %>%
#   dplyr::select(zipcode, Town)
# 
# write_csv(MAtowns, "MAtowns.csv")
# 
# ## Unified MDPH and BPHC data, longitudinal
# #MDPH data
# covidtowns1 <-read_excel("MGB census.xlsx", sheet="Towns", guess_max = 20000)
# covidtowns1$date <-(lubridate::ymd(covidtowns1$date))
# covidtowns1$Count <-as.numeric(covidtowns1$Count)
# covidtowns1$Count <-replace_na(covidtowns1$Count, 3)  #MDPH does not report case counts between 1 and 5, impute as 3
# covidtowns1$Tests <-as.numeric(covidtowns1$Tests)
# covidtowns<-covidtowns1 %>%
#   filter(Town != "Boston") %>%  #remove Boston, as included as neighborhoods instead
#   distinct(date, Town, .keep_all = TRUE) %>%
#   dplyr::select(Town, date, Count, Tests)
# 
# #BPHC data
# boscovidtowns1 <-read_excel("MGB census.xlsx", sheet="Boston")
# boscovidtowns1$date <-(lubridate::ymd(boscovidtowns1$date))
# boscovidtowns1$Count <-as.numeric(boscovidtowns1$Count)
# boscovidtowns1$Tests <-as.numeric(boscovidtowns1$Tests)
# boscovidtowns<- boscovidtowns1 %>%
#   distinct(date, Town, .keep_all = TRUE) %>%
#   dplyr::select(Town, date, Count, Tests)
# 
# allcovidtowns<- full_join(covidtowns, boscovidtowns)
# write_csv(allcovidtowns, "allcovidtowns.csv")
# 
# ##town population table
# townpoptable<- covidtowns1 %>%
#   filter(date=="2020-07-08")%>%  #last date MDPH published rates that can be used to impute population
#   mutate(town_population=round((Count*(100000/as.numeric(Rate)))),
#          town_population = replace_na(town_population, 1500) #for towns unable to calc population, setting at 1500
#   )
# townpoptable<-townpoptable %>% dplyr::select(town_population, Town) %>%
#   filter(Town != "Boston")  #remove Boston, as included as neighborhoods instead
# 
# bostownpoptable<-boscovidtowns1 %>%
#   filter(date=="2020-07-08")%>%  
#   mutate(town_population=round((Count*(100000/as.numeric(Rate)))),
#          town_population = replace_na(town_population, 1500) #for towns unable to calc population, setting at 1500
#   )%>%
#   distinct(date, Town, .keep_all = TRUE)
# bostownpoptable<-bostownpoptable %>% dplyr::select(town_population, Town)
# 
# townpoptable<- full_join(townpoptable,bostownpoptable)
# write_csv(townpoptable, "MAtownpoptable.csv")
# 
# ###end

#read in Town/zipcode list for MA
MAtowns<-read_csv(here::here("data", "MAtowns.csv"))

#read in school/organization name
Org.name<-"School XYZ"

#read in school/organization zipcode listing and create zipcode weights
OrgZips<-read_csv(here::here("data", "NSzips.csv"), col_types = cols(.default = "c"))
numberzip<-OrgZips %>% tally()


ZipWeights<- OrgZips %>%
  group_by(zipcode) %>% 
  tally() %>% 
  mutate(weight=n/numberzip$n)

#read in weekly longitudinal town (and Boston neighborhood) data from MDPH/BPHC for testing and case counts
MAcovid<-read_csv(here::here("data", "allcovidtowns.csv"), col_types = cols(.default = "c"))
  MAcovid$date <-(lubridate::ymd(MAcovid$date))
  MAcovid$Count <-as.numeric(MAcovid$Count)
  MAcovid$Tests <-as.numeric(MAcovid$Tests)
  
#read in town/neighborhood populuation table
MAtownpoptable<-read_csv(here::here("data", "MAtownpoptable.csv"))

#add population to MAcovid
MAcovid<-left_join(MAcovid, MAtownpoptable, by="Town")

#add all zipcodes to MAcovid, multiple zipcode per Town (eg Back Bay as 7)
MAcovid<- left_join(MAtowns, MAcovid, by="Town")

# write the data to use in the app
write_csv(MAcovid, here::here("shiny", "data", "MAcovid.csv"))

##Create school/organization specific dataset and calculate rates

#limit list to organization's zipcodes by adding zips and joining
Org.MAcovid<- dplyr::filter(MAcovid, MAcovid$zipcode %in% ZipWeights$zipcode)
Org.MAcovid<- left_join(Org.MAcovid, ZipWeights, by="zipcode")
  
#calculate town-specific weights and rates
Org<-Org.MAcovid %>%
  distinct(Town, date, .keep_all = TRUE) %>% #need to include otherwise towns with multiple zips have multiple rows
  group_by(Town) %>%
  dplyr::arrange(date, .by_group = TRUE) %>% #avoids the issue with negative incidence for first date for each town due to substracting prior town cases
  mutate(Count.wt=(Count*weight),
         population.wt=town_population*weight,
         Rate=(Count/town_population)*100000,
         Tests.wt=(Tests*weight),
         incidence7day=Rate-(lag(Rate,1)),
         incidence1day=(Rate-lag(Rate,1))/7,
         incidence14day=Rate-lag(Rate,2), 
         incidence1day2wk=(Rate-lag(Rate,2))/14,
         Tests2wk=Tests-(lag(Tests,2)),
         Count2wk=Count-(lag(Count,2)),
         positivity2wk=Count2wk/Tests2wk, 
         town_population=(Count/Rate)*100000,
         testingrate=100000*Tests2wk/ town_population
  )

Org.community<- Org %>%
  arrange(Town,date)   %>% 
  group_by(date) %>% 
  summarise(Count=sum(Count),
            Count.weighted=sum(Count.wt),
            Tests=sum(Tests),
            Tests.weighted=sum(Tests.wt),
            population=sum(town_population),
            population.weighted=sum(population.wt))

Org.community<- Org.community %>% 
  arrange(date)   %>% 
  mutate(incidence14day=((Count.weighted-lag(Count.weighted,2))/population.weighted)*100000,
         incidence1day=incidence14day/14,
         positivity14day=100*((Count.weighted-lag(Count.weighted,2))/(Tests.weighted-lag(Tests.weighted,2)))
         )
  

Org %>%
  mutate(incidence1day=replace(incidence1day,incidence1day > 22, 22)) %>% #overwrite peak to fit in plot
  ggplot( aes(x=date, group=Town, color=Town)) +
  theme_classic() + theme(aspect.ratio=1, plot.title = element_text(size = rel(1.5)),
                          axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  geom_line(aes(y=incidence1day2wk), alpha=.7, size=1) +
  scale_color_viridis(discrete = TRUE, end=0.8, option = "D")+
  geom_label_repel(data=Org %>%
                     filter(date == last(date)), aes(label= Town,
                                                     x = date ,  y = incidence1day2wk), 
                   hjust=0,
                   xlim=c(Sys.Date(), as.Date(NA)))+
  guides(color=FALSE) +
  scale_y_continuous(limits= c(0,22), breaks= c(2,4,6,8,10, 12, 14, 16,18,20))+
  # scale_fill_viridis(discrete = TRUE, end=0.85, option = "D")+
  guides(color=FALSE) +
  labs(x="Date", y="Two-Week Rolling Average of Daily Cases, per 100,000",
       title=paste0("Estimated Daily Covid-19 Cases in\nCommunities of ", Org.name,", 2020"),
       caption="Data source: Massachusetts Department of Public Health and Boston Public Health Commission" )+
  scale_x_date(limits= c(as.Date("2020-05-01"), Sys.Date()+45))

# daily casses plot
Org.community %>%
  filter(date > as.Date("2020-05-07")) %>%  #data not available for all prior 
  mutate(incidence1day=replace(incidence1day,incidence1day > 22, 22)) %>% #overwrite peak average (~23) to fit in plot
  add_row(date=as.Date(Sys.Date()+55), incidence1day=NA) %>% #add row with xmax so that ribbbon plots to max date
  ggplot( aes(x=date)) +
  geom_ribbon(aes(ymin=0, ymax=4), fill="#20854E99")+
  geom_ribbon(aes(ymin=4, ymax=8), fill="#FFDC9199")+
  geom_ribbon(aes(ymin=8, ymax=22), fill="#BC3C2999")+
  theme_classic() + theme(aspect.ratio=1, plot.title = element_text(size = rel(1.5)),
                          axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  geom_line(aes(y=incidence1day), alpha=1, size=2, color="#6F99ADFF") +
  geom_label_repel(data=Org.community %>%
                     filter(date == last(date)), 
                   aes(label=paste0(Org.name, "\nCommunity Average\n","(",round(incidence1day,1),"%)"),
                       x = date ,  y = incidence1day),
                   hjust=0,
                   xlim=c(Sys.Date(), as.Date(NA)))+
  guides(color=FALSE) +
  scale_y_continuous(limits= c(0,22), breaks= c(2,4,6,8,10, 12, 14, 16,18,20))+
  scale_x_date(limits= c(as.Date("2020-05-14"), Sys.Date()+55), date_breaks = "1 month",date_labels = "%b")+
  guides(color=FALSE) +
  labs(x="Date", y="Average Daily Cases, per 100,000",
       title=paste0("Covid-19 Cases in Communities of ", Org.name,", 2020"),
       subtitle = "Green, Yellow, Red risk categorization as per Commonwealth Guidelines",
       caption=paste0("Two-week average incidence in towns/neighborhoods of ", Org.name," members weighted by number of households\nData Source: MA Dept of Public Health and Boston Public Health Commission")
  )

#positivity plot
Org.community %>%
  filter(date > as.Date("2020-05-20")) %>%  #data not available for all prior 
  mutate(positivity14day=replace(positivity14day,positivity14day > 10, 10)) %>% #overwrite peak average  to fit in plot
  add_row(date=as.Date(Sys.Date()+55), incidence1day=NA) %>% #add row with xmax so that ribbbon plots to max date
  ggplot( aes(x=date)) +
  geom_ribbon(aes(ymin=0, ymax=2), fill="#20854E99")+
  geom_ribbon(aes(ymin=2, ymax=10), fill="#BC3C2999")+
  theme_classic() + theme(aspect.ratio=1, plot.title = element_text(size = rel(1.5)),
                          axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  geom_line(aes(y=positivity14day), alpha=1, size=2, color="#6F99ADFF") +
  geom_label_repel(data=Org.community %>%
                     filter(date == last(date)), 
                   aes(label=paste0(Org.name, "\nCommunity Average\n","(",round(positivity14day,2),"%)"),
                       x = date ,  y = positivity14day),
                   hjust=0,
                   xlim=c(Sys.Date(), as.Date(NA)))+
  guides(color=FALSE) +
  scale_y_continuous(limits= c(0,10))+
  scale_x_date(limits= c(as.Date("2020-05-14"), Sys.Date()+55), date_breaks = "1 month",date_labels = "%b")+
  guides(color=FALSE) +
  labs(x="Date", y="Positivity\n(% of molecular tests positive)",
       title=paste0("Positivity in Communities of ", Org.name,", 2020"),
       subtitle = "Green and Red risk categorization as per Commonwealth Guidelines",
       caption=paste0("Molecular test positivity in towns/neighborhoods of ", Org.name," members weighted by number of households\nData Source: MA Dept of Public Health and Boston Public Health Commission")
  )

