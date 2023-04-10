
rm(list=ls())
load("inputs/LAPOP IDB 2018-9 (from merge).rda")
library(hutils)
library(haven)
library(survey)
library(srvyr)
library(sjmisc)
library(sjlabelled)
library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(foreign)
library(stringr)
library(plyr)
lapop <-merge 

options( scipen = 20 )

mean(is.na(lapop$idnum))
mean(is.na(lapop$uniq_id))
mean(is.na(lapop$q10new))
mean(is.na(lapop$pais))
mean(is.na(lapop$psc1))

lapop <- lapop[!is.na(lapop$psc1),]

iso <- tribble(~pais, ~iso3,~pais_no,
               "Mexico"	,"MEX",1,
               "Guatemala"	,"GTM",2,
               "El Salvador"	,"SLV",3,
               "Honduras"	,"HND",4,
               "Nicaragua"	,"NIC",5,
               "Costa Rica"	,"CRI",6,
               "Panama"	,"PAN",7,
               "Colombia"	,"COL",8,
               "Ecuador"	,"ECU",9,
               "Bolivia"	,"BOL",10,
               "Peru"	,"PER",11,
               "Paraguay"	,"PRY",12,
               "Chile"	,"CHL",13,
               "Uruguay"	,"URY",14,
               "Brazil"	,"BRA",15,
               "Venezuela"	,"VEN",16,
               "Argentina"	,"ARG",17,
               "Dominican Republic"	,"DOM",18,
               "Haiti"	,"HTI",19,
               "Jamaica"	,"JAM",20,
               "Guyana"	,"GUY",21,
               "Trinidad and Tobago"	,"TTO",22,
               "Belize"	,"BLZ",23,
               "Suriname"	,"SUR",24,
               "Bahamas"	,"BHS",25,
               "Barbados"	,"BRB",26)

lapop <- 
  lapop %>% 
  left_join(iso, by='pais')


lapop$iso3 <- labelled(c(lapop$iso3), 
                            label="ISO 3 Code")

lapop_income<-read.csv("inputs/lapop_2018_income_table.csv")
xchange_rates<- read_xls("inputs/exchange_rates_wb.xls")
# Convert income information: 
xc <- select(xchange_rates, `Country Code` ,`2018` )

xc2018<-filter(xc, xc$`Country Code` %in% iso$iso3)

income<-left_join(lapop_income, xc2018, by = c("iso3" = "Country Code"))

income$ingreso_min <- as.numeric(income$ingreso_min)
income$ingreso_max <- as.numeric(income$ingreso_max)

income$income_min_usd <- income$ingreso_min/income$`2018`


income$income_max_usd <- income$ingreso_max/income$`2018`

income2<-select(income,iso3, value,income_min_usd, income_max_usd)

lapop<-left_join(lapop, income2, by= c("iso3" = "iso3", "q10new" = "value" ))

lapop_income <- select(lapop, 1:784,789:792)


df <- data.frame(pais = unique(lapop_income$pais),
                 pais_number = 1:18)

lapop_income<-left_join(lapop_income, df, by= c("pais" = "pais"))

write_dta(
  lapop_income,
  "lapop_2019_income.dta",
  label = attr(data, "label")
)


## Impute incomes in stata
## Add imputed incomes back in

files<-list.files("stata_income_generation/")

for(i in 1:length(files)){
  print(files[i])
  data<-read_dta(paste0("stata_income_generation/", files[i]))
  if(i == 1){
    df<-data
  } 
  if(i >1){
    df<-rbind(df, data)
  }
}


lapop<-df

# year
# lapop$year_survey <- format(lapop$fecha,"%Y")
lapop$year_survey <- lapop$wave


# For dimensions we will have country, scope, quintile, gender
# iso	

# scope	
mean(is.na(lapop$ur))
lapop$scope <- ifelse(lapop$ur == 1, "urban", "rural")
mean(is.na(lapop$scope))
#      View(lapop[is.na(lapop$scope),])

#no missing values
#lapop$scope <- ifelse(is.na(lapop$scope), "no scope data", lapop$scope)


# quintile	

# lapop<-mutate_ntile(lapop, "q10new", n=5, weights = "weight1500", by = "iso3",
#                     new.col = "quintile_ranges", character.only = TRUE, overwrite = TRUE,
#                     check.na = FALSE)

lapop<-mutate_ntile(lapop, "iinc", n=5, weights = "weight1500", by = "iso3",
                    new.col = "quintile", character.only = TRUE, overwrite = TRUE,
                    check.na = FALSE)

mean(is.na(lapop$quintile))

#lapop <-select(lapop, 1:795,1099:1102)

save(lapop, file="C:/Users/jesse/Desktop/Data Projects/survey_comparison/lapop_inputs/lapop_2018_19_demos.Rda")

# gender

lapop$gender <- ifelse(lapop$q1 ==1, "male", 
                       ifelse(lapop$q1 ==2, "female",NA ))
mean(is.na(lapop$gender))

lapop$gender <- ifelse(is.na(lapop$q1), "na value",lapop$gender)




# The water access related variables included in the household survey data set will also
# be included here: 


lapop$access_water_piped_home <- ifelse(lapop$psc1 ==1, 1,0)      
lapop$access_water_piped_plot <- ifelse(lapop$psc1 == 2, 1,0)

# psc1n (main drinking water source), psc2n (main source for other purposes), psc2f1 (if other water sources are piped into house or plot)
# psc1n are counted if piped to house, plot, trucked water or rainwater harvesting

lapop$water_on_premises <- ifelse(lapop$psc1 %in% c(1,2,10,12,13), 1,
                                #  ifelse(lapop$psc1 == lapop$psc2n & lapop$psc1 %in% c(3,4,5,6,7,8,14) & lapop$psc2f1 %in% c(2,3),1,
                                         ifelse(is.na(lapop$psc1),NA, 0))
    ### PSC2F1 doesnt exist in lapop 2018/19 so this variable is defined slightly differently, included 12? 


## testing by taking out is na clause 

    lapop$water_distr <- ifelse(lapop$psc1 %in% c(1,2) | lapop$psc2 %in% c(1,2),1,
                                ifelse(is.na(lapop$psc1), NA, 0))

## Water consumption variables

    lapop$consume_distr <- ifelse(lapop$psc1 %in% c(1,2),1,
                                  ifelse(is.na(lapop$psc1), NA,0))
    lapop$consume_irregularconnection<- ifelse(lapop$psc1 %in% c(3),1, 
                                               ifelse(is.na(lapop$psc1),NA,0))
    lapop$consume_publictap<- ifelse(lapop$psc1 %in% c(4),1, 
                                     ifelse(is.na(lapop$psc1),NA,0))
    lapop$consume_impwell<- ifelse(lapop$psc1 %in% c(5,6),1, 
                                    ifelse(is.na(lapop$psc1),NA,0))
    
    lapop$consume_delivered<- ifelse(lapop$psc1 %in% c(12,13),1, 
                                  ifelse(is.na(lapop$psc1),NA,0))
    
    lapop$consume_unimproved <- ifelse(lapop$psc1 %in% c(7,9,14),1,0)
    
    lapop$consume_impspring <- ifelse(lapop$psc1 %in% c(8),1, 
                                      ifelse(is.na(lapop$psc1),NA,0))
    lapop$consume_rain <- ifelse(lapop$psc1 %in% c(10),1, 
                                 ifelse(is.na(lapop$psc1),NA,0))
    
    lapop$consume_bottled <- ifelse(lapop$psc1 == 11,1,
                                    ifelse(is.na(lapop$psc1),NA,0))
    lapop$consume_other <- ifelse(lapop$psc1 == 77,1,
                                  ifelse(is.na(lapop$psc1),NA,0))
    
    lapop$improved_w_access <- ifelse(lapop$psc1 %in% c(1,2,3,4,5,6,8,10,11,12,13),1,
                                  ifelse(is.na(lapop$psc1),NA,0))
    
## General water source variables
    
    
    lapop$ou_distr <- ifelse(lapop$psc2 %in% c(1,2),1,
                                  ifelse(is.na(lapop$psc2), NA,0))
    lapop$ou_irregularconnection<- ifelse(lapop$psc2 %in% c(3),1, 
                                               ifelse(is.na(lapop$psc2),NA,0))
    lapop$ou_publictap<- ifelse(lapop$psc2 %in% c(4),1, 
                                     ifelse(is.na(lapop$psc2),NA,0))
    lapop$ou_impwell<- ifelse(lapop$psc2 %in% c(5,6),1, 
                                   ifelse(is.na(lapop$psc2),NA,0))
    
    lapop$ou_delivered<- ifelse(lapop$psc2 %in% c(12,13),1, 
                                     ifelse(is.na(lapop$psc2),NA,0))
    
    lapop$ou_unimproved <- ifelse(lapop$psc2 %in% c(7,9,14),1,0)
    
    lapop$ou_impspring <- ifelse(lapop$psc2 %in% c(8),1, 
                                      ifelse(is.na(lapop$psc2),NA,0))
    lapop$ou_rain <- ifelse(lapop$psc2 %in% c(10),1, 
                                 ifelse(is.na(lapop$psc2),NA,0))
    
    lapop$ou_bottled <- ifelse(lapop$psc2 == 11,1,
                                    ifelse(is.na(lapop$psc2),NA,0))
    lapop$ou_other <- ifelse(lapop$psc2 == 77,1,
                                  ifelse(is.na(lapop$psc2),NA,0))

# Daily 
    lapop$water_daily <- ifelse(lapop$psc7 %in% c(4:7) ,1,
                                ifelse(is.na(lapop$psc2), NA,
                                       ifelse(lapop$psc7 %in% c(0:3),0,0))) ## This is not comparible -- lapop 2021 asks if they have sufficient water
    

# The sanitation access related variables included in the household survey data set will also
# be included here:
    
    

lapop$san_exclusive <-  ifelse(lapop$psc12 == 1, 0,
                                      ifelse(lapop$psc12 ==2, 1, NA))

lapop$psc11<-zap_missing(lapop$psc11)
lapop$psc11a<-zap_missing(lapop$psc11a)

lapop$san_sewer <- ifelse(lapop$psc11 %in% c(1),1,
                             ifelse((is.na(lapop$psc11) & is.na(lapop$psc11a)),NA, 0))


lapop$san_septic <- ifelse(lapop$psc11 %in% c(2,7),1,
                              ifelse((is.na(lapop$psc11) & is.na(lapop$psc11a)),NA, 0))

lapop$san_other <- ifelse(lapop$psc11 %in% c(4,5)|(lapop$psc11 %in% c(6)&(lapop$psc11a == 77| is.na(lapop$psc11a))),1,
                           ifelse((is.na(lapop$psc11) & is.na(lapop$psc11a)),NA, 0))

## combo with psc11 and psc11a
lapop$san_implatrine <- ifelse(lapop$psc11 %in% c(6) & lapop$psc11a%in% c(1,2) ,1,
                               ifelse((is.na(lapop$psc11) & is.na(lapop$psc11a)),NA,0))  ## 6 is latrine in argentina survey but pozo not connected to anything in other countries 

lapop$san_ecolatrine <- ifelse(lapop$psc11 %in% c(6) & lapop$psc11a%in% c(4) ,1,
                                  ifelse((is.na(lapop$psc11) & is.na(lapop$psc11a)),NA,0))  ## 6 is latrine in argentina survey but pozo not connected to anything in other countries 


lapop$san_unimp <- ifelse((lapop$psc11 %in% c(6) & lapop$psc11a%in% c(3,5,6))|lapop$psc11 %in% c(3)  ,1,
                               ifelse((is.na(lapop$psc11) & is.na(lapop$psc11a)),NA,0))  ## 6 is latrine in argentina survey but pozo not connected to anything in other countries 

lapop$no_san_access <- ifelse(lapop$psc11 %in% c(6) & lapop$psc11a %in% c(7),1,
                              ifelse((is.na(lapop$psc11) & is.na(lapop$psc11a)),NA,0))




lapop$improved_san <- ifelse((lapop$psc11 %in% c(6) & lapop$psc11a%in% c(1,2,6)) |lapop$psc11 %in% c(1,2,7),1,0)

data_improved_san_ex <- select(lapop, improved_san,san_exclusive )
data_improved_san_ex$improved_ex<- +(rowSums(data_improved_san_ex, na.rm = TRUE) * NA ^ (rowSums(!is.na(data_improved_san_ex)) == 0) > 0)
lapop$improved_san_exclusive <-data_improved_san_ex$improved_ex

lapop$sewage_contaminates <- ifelse(lapop$psc11 %in% c(3)| lapop$psc11a %in% c(5,6,7),1,0) 



# We correct the dummy variables for the svyby function, replacing NAs with 0 for the calculations. The original NA
# values are important to keep in the lapop data set. For cleanliness we work with a data set dedicated to processing.
lapop_processing <- select(lapop,pais,iso3, strata, upm, weight1500,wt, scope, gender, quintile,
                           access_water_piped_home ,
                           access_water_piped_plot ,
                           consume_bottled ,
                           consume_delivered,
                           consume_distr ,
                           consume_impspring ,
                           consume_impwell,
                           consume_irregularconnection,
                           consume_other ,
                           consume_publictap,
                           consume_rain ,
                           consume_unimproved ,
                           improved_san ,
                           improved_san_exclusive ,
                           improved_w_access ,
                           no_san_access ,
                           ou_bottled ,
                           ou_delivered,
                           ou_distr ,
                           ou_impspring ,
                           ou_impwell,
                           ou_irregularconnection,
                           ou_other ,
                           ou_publictap,
                           ou_rain ,
                           ou_unimproved ,
                           san_ecolatrine ,
                           san_exclusive ,
                           san_implatrine ,
                           san_other ,
                           san_septic ,
                           san_sewer ,
                           san_unimp ,
                           sewage_contaminates ,
                           water_daily ,
                           water_distr ,
                           water_on_premises,
                           uniq_id, q2, q12c, q10new, income_min_usd, income_max_usd)


lapop_processing<-lapop_processing[!is.na(lapop_processing$wt),]

library(srvyr)
lapop2019_sd <- as_survey_design(lapop_processing, 
                                 ids = upm,
                                 strata = strata,
                                 weight = wt,
                                 nest=TRUE)


dimensions <- list("iso3", c("iso3", "scope"), c("iso3","gender"), c("iso3","quintile"),  c("iso3","scope", "quintile"))


#test <- lapop[!is.na(lapop$weight1500),]
#test$iws <- ifelse(is.na(test$psc1n), NA, 
#                  ifelse(test$psc1n %in% c(1,2,3,4,5,6,8,10,11,12,13), 1,0))

#lapop2021_sd <- as_survey_design(test, 
#                                ids = upm,
#                                 strata = strata,
#                                 weight = weight1500,
#                                 nest=TRUE)
#test2 <- test[test$iso3== "MEX",]  
#summary_d <- lapop2021_sd%>%                     
#  dplyr::group_by(iso3) %>%                            
#  srvyr::summarise(iws = survey_mean(iws, na.rm = T,  vartype = c("ci")))
#consume_other = survey_mean(consume_other, na.rm = T,  vartype = c("ci")),
#consume_bottled = survey_mean(consume_bottled, na.rm = T,  vartype = c("ci")),
#improved_w_access = survey_mean(improved_w_access, na.rm = T,  vartype = c("ci")))

for (i in 1:length(dimensions)){
  
  print(paste0("Dimension: ",dimensions[i]," ", "Num"," ", i, "/",length(dimensions))) 

  # summary_d <- lapop2019_sd%>%
  #   dplyr::group_by_at(vars(one_of(dimensions[[i]]))) %>%
  # srvyr::summarise(access_water_piped_home = survey_mean(access_water_piped_home, na.rm = T,  vartype = c("ci")),
  #                  access_water_piped_plot = survey_mean(access_water_piped_plot, na.rm = T,  vartype = c("ci")),
  #                  water_on_premises = survey_mean(water_on_premises, na.rm = T,  vartype = c("ci")),
  #                  water_distr = survey_mean(water_distr, na.rm = T,  vartype = c("ci")),
  #                  consume_distr = survey_mean(consume_distr, na.rm = T,  vartype = c("ci")),
  #                  consume_delivered= survey_mean(consume_delivered, na.rm = T,  vartype = c("ci")),
  #                  consume_impspring  = survey_mean(consume_impspring , na.rm = T,  vartype = c("ci")),
  #                  consume_impwell = survey_mean(consume_impwell, na.rm = T,  vartype = c("ci")),
  #                  consume_irregularconnection = survey_mean(consume_irregularconnection, na.rm = T,  vartype = c("ci")),
  #                  consume_rain  = survey_mean(consume_rain , na.rm = T,  vartype = c("ci")),
  #                  consume_unimproved  = survey_mean(consume_unimproved , na.rm = T,  vartype = c("ci")),
  #                  consume_publictap = survey_mean(consume_publictap, na.rm = T,  vartype = c("ci")),
  #                  consume_other = survey_mean(consume_other, na.rm = T,  vartype = c("ci")),
  #                  consume_bottled = survey_mean(consume_bottled, na.rm = T,  vartype = c("ci")),
  #                  improved_w_access = survey_mean(improved_w_access, na.rm = T,  vartype = c("ci")),
  #                  water_daily = survey_mean(water_daily, na.rm = T,  vartype = c("ci")),
  #                  
  #                  ou_distr= survey_mean(ou_distr, na.rm = T,  vartype = c("ci")),
  #                  ou_bottled= survey_mean(ou_bottled, na.rm = T,  vartype = c("ci")),
  #                  ou_delivered= survey_mean(ou_delivered, na.rm = T,  vartype = c("ci")),
  #                  ou_impspring = survey_mean(ou_impspring , na.rm = T,  vartype = c("ci")),
  #                  ou_impwell= survey_mean(ou_impwell, na.rm = T,  vartype = c("ci")),
  #                  ou_irregularconnection= survey_mean(ou_irregularconnection, na.rm = T,  vartype = c("ci")),
  #                  ou_publictap= survey_mean(ou_publictap, na.rm = T,  vartype = c("ci")),
  #                  ou_rain = survey_mean(ou_rain , na.rm = T,  vartype = c("ci")),
  #                  ou_other = survey_mean(ou_other , na.rm = T,  vartype = c("ci")),
  #                  ou_unimproved = survey_mean(ou_unimproved , na.rm = T,  vartype = c("ci")),
  #                  
  #                  improved_san = survey_mean(improved_san , na.rm = T,  vartype = c("ci")),
  #                  improved_san_exclusive = survey_mean(improved_san_exclusive , na.rm = T,  vartype = c("ci")),
  #                  no_san_access = survey_mean(no_san_access , na.rm = T,  vartype = c("ci")),
  #                  san_ecolatrine  = survey_mean(san_ecolatrine , na.rm = T,  vartype = c("ci")),
  #                  san_exclusive = survey_mean(san_exclusive , na.rm = T,  vartype = c("ci")),
  #                  san_implatrine = survey_mean(san_implatrine , na.rm = T,  vartype = c("ci")),
  #                  san_other = survey_mean(san_other , na.rm = T,  vartype = c("ci")),
  #                  san_septic  = survey_mean(san_septic , na.rm = T,  vartype = c("ci")),
  #                  san_sewer = survey_mean(san_sewer , na.rm = T,  vartype = c("ci")),
  #                  san_unimp = survey_mean(san_unimp , na.rm = T,  vartype = c("ci")),
  #                  sewage_contaminates= survey_mean(sewage_contaminates, na.rm = T,  vartype = c("ci")),
  #                  count_respondents = length(unique(uniq_id)))

  # Without CIs
   summary_d <- lapop2019_sd%>%
     dplyr::group_by_at(vars(one_of(dimensions[[i]]))) %>%
       srvyr::summarise(access_water_piped_home = survey_mean(access_water_piped_home, na.rm = T,  vartype = NULL),
            access_water_piped_plot = survey_mean(access_water_piped_plot, na.rm = T,  vartype = NULL),
            water_on_premises = survey_mean(water_on_premises, na.rm = T,  vartype = NULL),
            water_distr = survey_mean(water_distr, na.rm = T,  vartype = NULL),
            consume_distr = survey_mean(consume_distr, na.rm = T,  vartype = NULL),
            consume_delivered= survey_mean(consume_delivered, na.rm = T,  vartype = NULL),
            consume_impspring  = survey_mean(consume_impspring , na.rm = T,  vartype = NULL),
            consume_impwell = survey_mean(consume_impwell, na.rm = T,  vartype = NULL),
            consume_irregularconnection = survey_mean(consume_irregularconnection, na.rm = T,  vartype = NULL),
            consume_rain  = survey_mean(consume_rain , na.rm = T,  vartype = NULL),
            consume_unimproved  = survey_mean(consume_unimproved , na.rm = T,  vartype = NULL),
            consume_publictap = survey_mean(consume_publictap, na.rm = T,  vartype = NULL),
            consume_other = survey_mean(consume_other, na.rm = T,  vartype = NULL),
            consume_bottled = survey_mean(consume_bottled, na.rm = T,  vartype = NULL),
            improved_w_access = survey_mean(improved_w_access, na.rm = T,  vartype = NULL),
            water_daily = survey_mean(water_daily, na.rm = T,  vartype = NULL),
            
            ou_distr= survey_mean(ou_distr, na.rm = T,  vartype = NULL),
            ou_bottled= survey_mean(ou_bottled, na.rm = T,  vartype = NULL),
            ou_delivered= survey_mean(ou_delivered, na.rm = T,  vartype = NULL),
            ou_impspring = survey_mean(ou_impspring , na.rm = T,  vartype = NULL),
            ou_impwell= survey_mean(ou_impwell, na.rm = T,  vartype = NULL),
            ou_irregularconnection= survey_mean(ou_irregularconnection, na.rm = T,  vartype = NULL),
            ou_publictap= survey_mean(ou_publictap, na.rm = T,  vartype = NULL),
            ou_rain = survey_mean(ou_rain , na.rm = T,  vartype = NULL),
            ou_other = survey_mean(ou_other , na.rm = T,  vartype = NULL),
            ou_unimproved = survey_mean(ou_unimproved , na.rm = T,  vartype = NULL),
            
            improved_san = survey_mean(improved_san , na.rm = T,  vartype = NULL),
            improved_san_exclusive = survey_mean(improved_san_exclusive , na.rm = T,  vartype = NULL),
            no_san_access = survey_mean(no_san_access , na.rm = T,  vartype = NULL),
            san_ecolatrine  = survey_mean(san_ecolatrine , na.rm = T,  vartype = NULL),
            san_exclusive = survey_mean(san_exclusive , na.rm = T,  vartype = NULL),
            san_implatrine = survey_mean(san_implatrine , na.rm = T,  vartype = NULL),
            san_other = survey_mean(san_other , na.rm = T,  vartype = NULL),
            san_septic  = survey_mean(san_septic , na.rm = T,  vartype = NULL),
            san_sewer = survey_mean(san_sewer , na.rm = T,  vartype = NULL),
            san_unimp = survey_mean(san_unimp , na.rm = T,  vartype = NULL),
            sewage_contaminates= survey_mean(sewage_contaminates, na.rm = T,  vartype = NULL),
            count_respondents = length(unique(uniq_id)))
            
  
  if(i == 1){
    summary <-summary_d                        # Create summary data frame if it doesnt exist
  }else{
    summary <-rbind.fill(summary, summary_d)        # append the current data frame
  }
  
}

lapop_summary<-summary


lapop_summary$quintile <- ifelse(is.na(lapop_summary$quintile), "total",lapop_summary$quintile)
lapop_summary$gender <- ifelse(is.na(lapop_summary$gender), "all", lapop_summary$gender)
lapop_summary$scope <- ifelse(is.na(lapop_summary$scope), "country", lapop_summary$scope)



## reorder columns
lapop_summary <- lapop_summary[lapop_summary$gender != "na value",]
lapop_summary_filtered<-lapop_summary[lapop_summary$scope != "no scope data",]

# for CIs included
#lapop_2018_19<- select(lapop_summary_filtered, 1,116:119, 2:115)

# CIs not included

lapop_2018_19<- select(lapop_summary_filtered, 1,39:42, 2:38)

test<-select(lapop_2018_19,"iso3","scope","quintile","gender","san_ecolatrine","no_san_access","san_implatrine","san_other","san_septic","san_sewer","san_unimp")
test$sum<-test$san_ecolatrine+test$no_san_access+test$san_implatrine+test$san_other+test$san_septic+test$san_sewer+test$san_unimp

a<-select(lapop, iso3, quintile, scope, gender, psc11, psc11a, san_ecolatrine,no_san_access,san_implatrine,san_other,san_septic,san_sewer,san_unimp)
View(a[a$iso3 == "BRA" & a$quintile == 5 & a$scope == "rural",])
a<-select(lapop_2018_19, iso3, quintile, scope, gender, san_unimp, sewage_contaminates)


write.csv(lapop_2018_19, "outputs/lapop_2018_19_wide.csv", row.names = F)

lapop_2018_19_long<- pivot_longer(lapop_2018_19, 6:42, names_to = "indicator", values_to = "value")

lapop_2018_19_long$value <- lapop_2018_19_long$value*100

write.csv(lapop_2018_19_long, "outputs/lapop_2018_19_long.csv", row.names = F)


save(lapop_2018_19, file ="outputs/lapop_2018_19.Rda")

