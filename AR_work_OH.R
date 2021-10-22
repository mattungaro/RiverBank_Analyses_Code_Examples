

# 1. GOAL: SUMMARIZE IMPACTS ON WATER RESOURCES (STREAMS AND WETLANDS) AND MITIGATION OF WATER RESOURCES ########
# (STREAMS AND WETLANDS)

# 2a. DATA COLLECTION AND DATA UNDERSTANDING #####################################################################

library(tidyverse)

# First, get all of the datasets
csv2015 <- read_csv("C:\\Users\\Owner\\Documents\\Fort Worth Research\\Research\\3.14_research_redone_R_code\\ORM\\2015_Oct_to_Sep.csv") #%>% filter(Action == "Impact")
csv2016 <- read_csv("C:\\Users\\Owner\\Documents\\Fort Worth Research\\Research\\3.14_research_redone_R_code\\ORM\\2016_Oct_to_Sep.csv") #%>% filter(DISTRICT == "SWF" & ACTION == "Impact")
csv2017 <- read_csv("C:\\Users\\Owner\\Documents\\Fort Worth Research\\Research\\3.14_research_redone_R_code\\ORM\\2017_Oct_to_Sep.csv") #%>% filter(DISTRICT == "SWF" & ACTION == "Impact")
csv2018 <- read_csv("C:\\Users\\Owner\\Documents\\Fort Worth Research\\Research\\3.14_research_redone_R_code\\ORM\\2018_Oct_to_Sep.csv") #%>% filter(DISTRICT == "SWF" & ACTION == "Impact")
csv2019 <- read_csv("C:\\Users\\Owner\\Documents\\Fort Worth Research\\Research\\3.14_research_redone_R_code\\ORM\\2019_Oct_to_Sep.csv") #%>% filter(DISTRICT == "SWF" & ACTION == "Impact")
csv2020 <-read_csv("C:\\Users\\Owner\\Documents\\Fort Worth Research\\FY2020 FOIA Report r19Mar2021.csv")
colnames(csv2020)

# 3a. DATA PREPARATION ##################################################################

# Second, get the filter that we'll run the csv2015-2020 through to get unique 
# HUCs.
csv2015_filter <- csv2015 %>% filter(State == "OH") 
csv2016_filter <- csv2016 %>% filter(STATE == "OH") 
csv2017_filter <- csv2017 %>% filter(STATE == "OH") 
csv2018_filter <- csv2018 %>% filter(STATE == "OH") 
csv2019_filter <- csv2019 %>% filter(STATE == "OH") 
csv2020_filter <- csv2020 %>% filter(State == "OH") 

hucs2015 <- as.numeric(unique(csv2015_filter$HUC8))
hucs2016 <- as.numeric(unique(csv2016_filter$HUC))
hucs2017 <- as.numeric(unique(csv2017_filter$HUC))
hucs2018 <- as.numeric(unique(csv2018_filter$HUC))
hucs2019 <- as.numeric(unique(csv2019_filter$HUC))
hucs2020 <- as.numeric(unique(csv2020_filter$HUC8))
hucs <- c(hucs2015,hucs2016,hucs2017, hucs2018, hucs2019, hucs2020)
unique_hucs <- unique(hucs)
csv2015_huc = csv2015 %>% filter(HUC8 %in% unique_hucs)
csv2016_huc = csv2016 %>% filter(HUC %in% unique_hucs)
csv2017_huc = csv2017 %>% filter(HUC %in% unique_hucs)
csv2018_huc = csv2018 %>% filter(HUC %in% unique_hucs)
csv2019_huc = csv2019 %>% filter(HUC %in% unique_hucs)
csv2020_huc = csv2020 %>% filter(HUC8 %in% unique_hucs)
rm(csv2015, csv2016, csv2020 ,csv2019, csv2018, csv2017,csv2015_filter,csv2016_filter,
   csv2017_filter,csv2018_filter,csv2019_filter,csv2020_filter)

csv2015_huc <- csv2015_huc %>% mutate_at(c('IMPACT_ID',
                                           'MITIGATION_ID',
                                           'HUC8', 'HUC10', 'HUC12', 'AUTH_LINEAR_FT', 'AUTH_FILL_ACRES',
                                           'MIT_RQD_ACRES', 'MIT_RQD_LINEAR_FT', 'PROJ_LATITUDE', 'PROJ_LONGITUDE',
                                           'WATERS_LATITUDE', 'WATERS_LONGITUDE'), as.numeric) 

csv2016_huc <- csv2016_huc %>% mutate_at(c( 'HUC10', 'HUC12'), as.numeric) %>% select(HUC8 = HUC, everything())
csv2017_huc <- csv2017_huc %>% mutate_at(c( 'HUC10', 'HUC12'), as.numeric)  %>% select(HUC8 = HUC, everything())
csv2018_huc <- csv2018_huc %>% mutate_at(c( 'HUC10', 'HUC12'), as.numeric)  %>% select(HUC8 = HUC, everything())
csv2019_huc <- csv2019_huc %>% mutate_at(c( 'HUC10', 'HUC12'), as.numeric)  %>% select(HUC8 = HUC, everything())


csv2020_huc <- csv2020_huc %>% mutate_at(c('IMpaCT_ID',
                                           'MITIGATION_ID',
                                           'HUC8', 'HUC10', 'HUC12', 'AUTH_FILL_LENGTH_FT', 
                                           'AUTH_FILL_ACRES', 'MIT_RQD_LENGTH_FT', 'MIT_RQD_ACRES',
                                           'PROJ_LATITUDE', 'PROJ_LONGITUDE',
                                           'WATERS_LATITUDE', 'WATERS_LONGITUDE'), as.numeric) 


csv2015_huc$COMPENSATORY_MITIGATION_RQD <- (csv2015_huc$COMP_MIT_RQD)
csv2020_huc$COMPENSATORY_MITIGATION_RQD <- (csv2020_huc$COMP_MIT_RQD)

select0 <- function(df) {
  df <- df %>% select('IMpaCT_ID',
                      'MITIGATION_ID',
                      'HUC8', 'HUC10', 'HUC12', 'AUTH_LINEAR_FT', 
                      'AUTH_FILL_ACRES', 
                      'PROJ_LATITUDE', 'PROJ_LONGITUDE', 
                      'WATERS_LATITUDE', 'WATERS_LONGITUDE', 'DA_NUMBER', 'ACTION', 'ACTION_TYPE', 
                      'DISTRICT', 'COMPENSATORY_MITIGATION_RQD', 'COUNTY', 'STATE',  'COWARDIN_CLASS'
  )
}
select2 <- function(df) {
  df <- df %>% select('IMpaCT_ID',
                      'MITIGATION_ID',
                      'HUC8', 'HUC10', 'HUC12', 'AUTH_LINEAR_FT', 
                      'AUTH_FILL_ACRES', 
                      'PROJ_LATITUDE', 'PROJ_LONGITUDE', 
                      'WATERS_LATITUDE', 'WATERS_LONGITUDE', 'DA_NUMBER', 'ACTION', 'ACTION_TYPE', 
                      'DISTRICT', 'COMPENSATORY_MITIGATION_RQD', 'COUNTY', 'STATE',  'COWARDIN_NAME'
  )
}
select1 <- function(df) {
  df <- df %>% select('IMpaCT_ID',
                      'MITIGATION_ID',
                      'HUC8', 'HUC10', 'HUC12', 'AUTH_LINEAR_FT', 'AUTH_FILL_LENGTH_FT', 
                      'AUTH_FILL_ACRES', 
                      'PROJ_LATITUDE', 'PROJ_LONGITUDE',
                      'WATERS_LATITUDE', 'WATERS_LONGITUDE', 'DA_NUMBER', 'ACTION', 'ACTION_TYPE', 
                      'DISTRICT', 'COMPENSATORY_MITIGATION_RQD', 'COUNTY', 'STATE', 'COWARDIN_CLASS'
  )
}
huc15 <- select0(csv2015_huc)
huc16 <- select2(csv2016_huc)
huc17 <- select2(csv2017_huc)
huc18 <- select2(csv2018_huc)
huc19 <- select2(csv2019_huc)
huc20 <- select1(csv2020_huc)

huc20$AUTH_LINEAR_FT <- huc20$AUTH_LINEAR_FT %>% as.numeric()

test3 <- bind_rows(huc15, huc16, huc17, huc18, huc19, huc20)

test3 <- test3 %>% mutate(updated_huc8 = substr(HUC10, 1, nchar(HUC10)-2))
test3$updated_huc8 <- as.numeric(test3$updated_huc8)


test3 %>% select(contains("AUTH_FI")) %>% colnames() #"AUTH_FILL_ACRES"     "AUTH_FILL_LENGTH_FT"
test3 %>% select(contains("AUTH_LI")) %>% colnames() #"AUTH_LINEAR_FT"

# Fourth, mutate types of impacts - aka COWARDIN
## three cowardin classes, because there's three ways they do this in the dataset
unique(grep("RIV", test3$COWARDIN_CLASS, value = T))

unique(grep("RIV", test3$COWARDIN_NAME, value = T))

test4 <- test3 %>% mutate(cow_class1= ifelse(grepl("PER",COWARDIN_CLASS), "per", 
                                                           ifelse(grepl("paL", COWARDIN_CLASS), "pal", 
                                                                  ifelse(grepl("INTERM", COWARDIN_CLASS), "int",
                                                                         ifelse(grepl("EPH", COWARDIN_CLASS), "eph",
                                                                                ifelse(grepl("R-", COWARDIN_CLASS), "unclass_riv",
                                                                                       NA))))))
unique(test4[, 97]) # TEST. LOOKS GOOD!

test4 <- test4 %>% mutate(cow_class2= ifelse(grepl("PER",COWARDIN_NAME), "per", 
                                             ifelse(grepl("paL", COWARDIN_NAME), "pal", 
                                                    ifelse(grepl("INTERM", COWARDIN_NAME), "int",
                                                           ifelse(grepl("EPH", COWARDIN_NAME), "eph",
                                                                  ifelse(grepl("R-", COWARDIN_NAME), "unclass_riv",
                                                                         NA))))))
unique(test4[, 98]) # TEST. LOOKS GOOD!

test4 <- test4 %>% mutate(cow_mit= ifelse(grepl("PER",`MIT_COWARDIN CLASS`), "per", 
                                          ifelse(grepl("paL", `MIT_COWARDIN CLASS`), "pal", 
                                                 ifelse(grepl("INTERM", `MIT_COWARDIN CLASS`), "int",
                                                        ifelse(grepl("EPH", `MIT_COWARDIN CLASS`), "eph",
                                                               ifelse(grepl("R-", `MIT_COWARDIN CLASS`), "unclass_riv",
                                                                      NA))))))
test4$`MIT_COWARDIN CLASS` %>% unique()
unique(test4[, 99]) # TEST. LOOKS GOOD!


# Fifth, split into ACTION - mitigation and ACTION - Impact
#write_csv(test4, 'C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\AR_work_12_Aug\\pa_2015_2020.csv
pa_mit <- test4 %>% filter(ACTION == "Mitigation")
pa_impact <- test4 %>% filter(ACTION == "Impact")

pa_impact %>% count(cow_class1)
pa_impact %>% count(cow_class2)

pa_impact2 =unite(pa_impact, cow_class0, c(cow_class1, cow_class2), na.rm = T)

pa_impact2 %>% count(cow_class0)
pa_impact2$cow_class0[pa_impact2$cow_class0 == ""] <- NA
pa_impact2


## FILTER DATA ################################################
n_distinct(pa_impact2$DA_NUMBER) #7072 # wait really?
pa_impact_rqd <- pa_impact2 %>% filter(COMPENSATORY_MITIGATION_RQD == "Y")
n_distinct(pa_impact_rqd$DA_NUMBER)
#1015
#### BIG DIFFERENCE! ##

pa_impact2_summary <- pa_impact2 %>% group_by(updated_huc8, cow_class0) %>% 
  summarise(sum_lf1 = sum(AUTH_LINEAR_FT, na.rm = T), sum_lf2 = sum(AUTH_FILL_LENGTH_FT, na.rm = T), 
            sum_ac1 = sum(AUTH_FILL_ACRES, na.rm = T),
            count_permits = n_distinct(DA_NUMBER, na.rm = T)) # this is what I want-tested it with excel


pa_impact2_summary2 <- pa_impact2_summary %>% mutate(true_sum_lf = sum_lf1 + sum_lf2 )
pa_impact2_summary2$true_sum_lf %>% sum()
# checked with my excel spreadsheet- the numbers match.

##Summarise  by types of resources, District(?), HUC8, make sure to get linear ft and length, cowardin 1 and 2.

pa_summary <- pa_impact2_summary2 %>% dplyr::select(updated_huc8, cowardin = cow_class0, sum_LF = true_sum_lf, sum_AC = sum_ac1, count_permits)
pa_summary <- pa_summary %>% mutate(simple_cow = ifelse(cowardin == "pal", "pal", ifelse(cowardin == "per"| cowardin == "unclass_riv"|cowardin == "int"|cowardin == "eph", "riv", NA)))
#write_csv(pa_summary, 'C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\AR_work_12_Aug\\pa_summary.csv')

# Then, set up group_by and summarise cases by HUC8s
all_LF = pa_summary %>% group_by(updated_huc8) %>% summarise(x = sum(sum_LF, na.rm  = T))

all_LF_riv = pa_summary %>% filter(simple_cow == "riv") %>% group_by(updated_huc8) %>% summarise(x = sum(sum_LF, na.rm = T))

int = pa_summary %>% filter(cowardin == "int") %>% group_by(updated_huc8) %>% summarise(x = sum(sum_LF))

eph = pa_summary %>% filter(cowardin == "eph") %>% group_by(updated_huc8) %>% summarise(x = sum(sum_LF))

per = pa_summary %>% filter(cowardin == "per") %>% group_by(updated_huc8) %>% summarise(x = sum(sum_LF))

all_AC = pa_summary %>% group_by(updated_huc8) %>% summarise(x = sum(sum_AC))

all_AC_pal = pa_summary %>%filter(simple_cow == "pal") %>%  group_by(updated_huc8) %>% summarise(x = sum(sum_AC))

all_pa  = full_join(all_LF_riv, eph, by = "updated_huc8")
all_pa  = full_join(all_pa, int, by = "updated_huc8")
all_pa  = full_join(all_pa, per, by = "updated_huc8")
all_pa  = full_join(all_pa, all_AC_pal, by = "updated_huc8")
all_pa <- all_pa %>% dplyr::select(updated_huc8, all_riv = x.x, eph = x.y, int = x.x.x, per = x.y.y, pal = x)
# Top impacts in pa

#write_csv(all_pa, "C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\pa\\pa_top_HUC.csv" )

top_10_int = int %>% arrange(desc(x)) %>% dplyr::select(huc8_int = updated_huc8, int1 = x)# top 10 HUCs for LF of streams
top_10_eph = eph %>% arrange(desc(x)) %>% dplyr::select(huc8_eph = updated_huc8, eph1 = x)# top 10 HUCs for LF of streams
top_10_per = per %>% arrange(desc(x)) %>% dplyr::select(huc8_per = updated_huc8, per1 = x)# top 10 HUCs for LF of streams
top10 = cbind(top_10_eph[1:10, ],  top_10_int[1:10, ],top_10_per[1:10, ])
# Take the top 10 and put it all together
all_top <- c(top10$huc8_eph, top10$huc8_int, top10$huc8_per)
all_top <- unique(all_top)

top_10_int = int %>% arrange(desc(x)) %>% dplyr::select(huc = updated_huc8, int1 = x)# top 10 HUCs for LF of streams
top_10_eph = eph %>% arrange(desc(x)) %>% dplyr::select(huc = updated_huc8, eph1 = x)# top 10 HUCs for LF of streams
top_10_per = per %>% arrange(desc(x)) %>% dplyr::select(huc = updated_huc8, per1 = x)# top 10 HUCs for LF of streams

desc_int <- top_10_int[1:10, ]
desc_eph <- top_10_eph[1:10, ]
desc_per <- top_10_per[1:10, ]


top <- full_join(desc_eph, desc_int, by = "huc")
top <- full_join(top, desc_per, by = "huc")
write_csv(top, "C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\pa\\pa_top_10_HUC_wo_mit_rqd.csv")

## NOW FILTER FOR ONLY REQUIRED MITIGATION ##################


pa_impact2_summary <- pa_impact_rqd %>% group_by(updated_huc8, cow_class0) %>% 
  summarise(sum_lf1 = sum(AUTH_LINEAR_FT, na.rm = T), sum_lf2 = sum(AUTH_FILL_LENGTH_FT, na.rm = T), 
            sum_ac1 = sum(AUTH_FILL_ACRES, na.rm = T),
            count_permits = n_distinct(DA_NUMBER, na.rm = T)) # this is what I want-tested it with excel


pa_impact2_summary2 <- pa_impact2_summary %>% mutate(true_sum_lf = sum_lf1 + sum_lf2 )
pa_impact2_summary2$true_sum_lf %>% sum()
# checked with my excel spreadsheet- the numbers match.

## Summarise  by types of resources, HUC8, make sure to get linear ft and length, cow 1 and 2.

pa_summary <- pa_impact2_summary2 %>% select(updated_huc8, cowardin = cow_class0, sum_LF = true_sum_lf, sum_AC = sum_ac1, count_permits)
pa_summary <- pa_summary %>% mutate(simple_cow = ifelse(cowardin == "pal", "pal", ifelse(cowardin == "per"| cowardin == "unclass_riv"|cowardin == "int"|cowardin == "eph", "riv", NA)))
#write_csv(pa_summary, 'C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\AR_work_12_Aug\\pa_summary.csv')

# Then, set up group_by and summarise cases by HUC8s
all_LF = pa_summary %>% group_by(updated_huc8) %>% summarise(x = sum(sum_LF, na.rm  = T))

all_LF_riv = pa_summary %>% filter(simple_cow == "riv") %>% group_by(updated_huc8) %>% summarise(x = sum(sum_LF, na.rm = T))

int = pa_summary %>% filter(cowardin == "int") %>% group_by(updated_huc8) %>% summarise(x = sum(sum_LF, na.rm  = T))

eph = pa_summary %>% filter(cowardin == "eph") %>% group_by(updated_huc8) %>% summarise(x = sum(sum_LF, na.rm  = T))

per = pa_summary %>% filter(cowardin == "per") %>% group_by(updated_huc8) %>% summarise(x = sum(sum_LF, na.rm  = T))

pal= pa_summary %>% filter(cowardin == "pal") %>% group_by(updated_huc8) %>% summarise(x = sum(sum_AC, na.rm  = T))



all_AC = pa_summary %>% group_by(updated_huc8) %>% summarise(x = sum(sum_AC))

all_AC_pal = pa_summary %>%filter(simple_cow == "pal") %>%  group_by(updated_huc8) %>% summarise(x = sum(sum_AC))

all_pa  = full_join(all_LF_riv, eph, by = "updated_huc8")
all_pa  = full_join(all_pa, int, by = "updated_huc8")
all_pa  = full_join(all_pa, per, by = "updated_huc8")
all_pa  = full_join(all_pa, all_AC_pal, by = "updated_huc8")
all_pa <- all_pa %>% select(updated_huc8, all_riv = x.x, eph = x.y, int = x.x.x, per = x.y.y, pal = x)
# Top impacts in pa

write_csv(all_pa, "C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\pa\\pa_top_HUC_mit_rqd.csv" )

top_10_all_riv = all_LF_riv %>% arrange(desc(x)) %>% select(huc = updated_huc8, all_riv = x)
top_10_int = int %>% arrange(desc(x)) %>% select(huc = updated_huc8, int1 = x)# top 10 HUCs for LF of streams
top_10_eph = eph %>% arrange(desc(x)) %>% select(huc = updated_huc8, eph1 = x)# top 10 HUCs for LF of streams
top_10_per = per %>% arrange(desc(x)) %>% select(huc = updated_huc8, per1 = x)# top 10 HUCs for LF of streams
top_10_pal = pal %>% arrange(desc(x)) %>% select(huc = updated_huc8, pal = x)# top 10 HUCs for LF of streams

desc_int <- top_10_int[1:10, ]
desc_eph <- top_10_eph[1:10, ]
desc_per <- top_10_per[1:10, ]
desc_pal<- top_10_pal[1:10, ]
desc_riv<- top_10_all_riv[1:10, ]

top <- full_join(desc_eph, desc_int, by = "huc")
top <- full_join(top, desc_per, by = "huc")
top<- full_join(top, desc_pal, by = "huc")
top<- full_join(top, desc_riv, by = "huc")

#top10 = cbind(top_10_eph[1:10, ],  top_10_int[1:10, ],top_10_per[1:10, ])
write_csv(top, "C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\pa\\pa_top_10_HUC.csv")
# Take the top 10 and put it all together
all_top <- c(top10$huc8_eph, top10$huc8_int, top10$huc8_per)
all_top <- unique(all_top)



### COMBINE RQD WITH A SHAPEFILE #####################

# Bring in my Shapefile
library(sf)
file = "C://Users//Owner//Documents//RAships and Riggsbee Work//Data_Analysis_Adam_Riggsbee//huc//huc250k_shp//huc250k.shp"
shp = read_sf(file)
shp$HUC_CODE = as.numeric(shp$HUC_CODE)
attempt1 = filter(shp, HUC_CODE %in% top$huc )
unique(attempt1$HUC_CODE)
# That worked
attempt1 <- attempt1 %>% mutate(huc = HUC_CODE)
attempt1 <- 
  full_join(attempt1, top, by = "huc")

write_sf(attempt1, "C://Users//Owner//Documents//RAships and Riggsbee Work//Data_Analysis_AR_8.12.21//pa//pa_impacts.shp")


# 4a. DATA MODELING - NOT NECESSARY HERE ###################################################

# 5a. DATA EVALUATION ######################################

# CHECK DATA 1:
### EPHEMERAL in 5030106 --- 1707 LF ###################

test3 %>% filter(updated_huc8 == '4100009') -> x
y <- x %>% filter(grepl("PER", COWARDIN_CLASS) | grepl("PER", COWARDIN_NAME))
z <- y %>% filter(COMPENSATORY_MITIGATION_RQD == "Y")
x2 <- z  %>% filter(ACTION == "Impact" )
sum(x2$AUTH_FILL_LENGTH_FT, na.rm = T) + sum(x2$AUTH_LINEAR_FT, na.rm = T)
## THAT WORKED! tested for eph. and per.

hucs <- read_sf("C://Users//Owner//Documents//RAships and Riggsbee Work//Data_Analysis_AR_8.12.21//pa//pa_impacts.shp")

hucs <- hucs %>% dplyr::select(eph1, per1, int1, huc)
hucs %>% mutate(NA_row = ifelse(is.na(eph1) & is.na(per1) & is.na(int1), T, F)) -> hucs
hucs <- hucs %>% dplyr::filter(NA_row == F)
hucs2 <- hucs %>% dplyr::select(huc,geometry)
hucs2 <- unique(hucs2)

write_sf(hucs2, "C://Users//Owner//Documents//RAships and Riggsbee Work//Data_Analysis_AR_8.12.21//pa//top_hucs_for_wdrs.shp")

### STOPPPED HERE FOR IMPACTS ##################

### NOW MITIGATION DATA PREPARATION STARTS HERE ################

library(sf)
library(tidyverse)
library(readr)

# 2.	DATA COLLECTION AND DATA UNDERSTANDING #########################################

louis_huc <- read_sf('C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\pa\\shp\\louisville_hucs.shp')
pitts_huc <- read_sf('C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\pa\\shp\\pittsburgh_hucs.shp')
hunt_huc <- read_sf('C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\pa\\shp\\huntington_hucs.shp')
buff_huc <- read_sf('C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\pa\\shp\\buffalo_hucs.shp')

setwd("C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\pa\\ledgers") 
# this is one above your folder. Replace with your information.
mydir = "Louisville_ledgers" 
#this is your folder. Replace with your information
myfiles = list.files(path=mydir, pattern="*.csv$", full.names=TRUE)
# bind a list of csv files together and add a new naming variable
library(vroom)
louisville_banks <- vroom(myfiles, delim =",",id = "louisville")
## repeat

# this is one above your folder. Replace with your information.
mydir = "Huntington_ledgers" 
#this is your folder. Replace with your information
myfiles = list.files(path=mydir, pattern="*.csv$", full.names=TRUE)
# bind a list of csv files together and add a new naming variable
huntington_banks <- vroom(myfiles, delim =",",id = "huntington")

##repeat
# this is one above your folder. Replace with your information.
mydir = "Buffalo_ledgers" 
#this is your folder. Replace with your information
myfiles = list.files(path=mydir, pattern="*.csv$", full.names=TRUE)
# bind a list of csv files together and add a new naming variable
buffalo_banks <- vroom(myfiles, delim =",",id = "buffalo")

## repeat
# this is one above your folder. Replace with your information.
mydir = "Pittsburgh_ledgers" 
#this is your folder. Replace with your information
myfiles = list.files(path=mydir, pattern="*.csv$", full.names=TRUE)
# bind a list of csv files together and add a new naming variable
pittsburgh_banks <- vroom(myfiles, delim =",",id = "pittsburgh")


# 3. DATA PREPARATION #################################################################################

rbind(l, h, b, p) -> all_banks_pa



#write_csv(all_banks_pa,'C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\PA\\PA_RIBITS_mit.csv')
all_banks_pa2 <- all_banks_pa %>% filter(Type == "Wdr")
all_banks_pa2$`Transaction Date`[1:4]
all_banks_pa2$`Transaction Date` <-  as.Date(all_banks_pa2$`Transaction Date`, format="%m/%d/%Y" )
all_banks_pa2 <- all_banks_pa2 %>% mutate(huc = as.numeric(`Impact HUC`) ) 
all_banks_pa2 <- all_banks_pa2 %>% mutate(updated_huc8 = substr(huc, 1, 7))

all_banks_pa2 <- subset(all_banks_pa2, `Transaction Date` > '2014-08-31' &`Transaction Date` <= '2020-09-01')

sum_hucs <- all_banks_pa2 %>% group_by(updated_huc8,`Credit Classification`) %>% summarise(n_distinct(Permit), sum(Credits, na.rm = T),sum(Acres, na.rm = T),
                                                                                  sum(`Linear Feet`, na.rm = T) )
#write_csv(sum_hucs, 'C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\TX\\TX_RIBITS_mit.csv')
## need to work with this some more. First, new column for different types of riverine impacts. 
## Next, only riverine impacts. Finally, organize based on HUC.

sum_hucs2 <- sum_hucs %>% filter(!grepl( "Wetland",`Credit Classification`))

summed_hucs <- sum_hucs3



z1 <- all_banks_pa2 %>% filter(is.na(huc))
z2 <-  all_banks_pa2 %>% filter(!is.na(huc))
n_distinct(z1$Permit) #225 permits  -- this is the number of NA HUCs
n_distinct(z2$Permit) #247 permits

z3 <- all_banks_pa2 %>% filter(is.na(Permit))
z4 <-  all_banks_pa2 %>% filter(!is.na(Permit))
# Okay this works! 566 out of (566+34). If I can then match the ones without HUCs with 
# impact permits, I should be good. 

# So, z1 is what I need to get filtered through impacts to find out WHERE these permits are doing their thing

rib_no_huc_permits <- unique(z1$Permit) # 225
# These mitigation permits have no HUC - which is my subwatershed and geographic area of concern. Therefore, I 
# attempt to find approximately where the permits are based by comparing them to their corresponding 
# impact permits.

all_impacts <- read_csv("C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\all_impacts.csv")
test4 <- all_impacts %>% filter(DA_NUMBER %in% rib_no_huc_permits)
test5 <- n_distinct(test4$DA_NUMBER) # 46 out of 225

# WARNING - MANY PERMITS WITHOUT DISTINCTIVE LOCATIONS
x <- rib_no_huc_permits %in% test4$DA_NUMBER  
y <- tibble(rib_no_huc_permits, x)
bad_permits <- y %>% filter(x == F)

test_bad <- z1 %>% filter(PERMIT %in% bad_permits$rib_no_huc_permits)
write_csv(test_bad, "C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\OH\\permits_wo_hucs_or_coordinates2.csv")

# WARNING - NOW I"VE MADE A NEW DOC FOR THOSE PERMITS. THEY WILL BE ASSESSED SEPERATELY FROM THE 247 GOOD PERMITS AND THE 46 NEWLY FIXED PERMITS

###### FIXING PERMITS IN QGIS ############################################
# So, I need to take test4 (46 PERMITS), figure out their coordinates, determine which HUC covers their location
write_csv(test4, "C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\OH\\impact_permits_with_corresponding_ribits_permits_missing_HUCs2.csv")
# I brought them into QGIS - now I see a number of the permits outside of our bounds.
# This is good. That means those hucs won't be important. However, I'm still going to attach a huc to them all.

# give those permits HUCs, and then bring them back in and combine permit name and huc with ribits without huc.
permits_with_new_hucs <- read_sf("C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\Shapefiles\\OH_no_huc_points_updated.shp")
permits_with_new_hucs <- `st_geometry<-`(permits_with_new_hucs, NULL)

permits_with_new_hucs2 <- permits_with_new_hucs %>% dplyr::select(HUC_CODE,Permit =  DA_NUMBER)
unique_permits_with_new_hucs2 <- unique(permits_with_new_hucs2)
z1 # needs hucs
z2 # already has the hucs -- problem is, this has wayyy too many hucs. I only need a limited number!

unique_hucs <- read_csv("C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\OH\\top_HUC_only_riv.csv")

## perfect. Now I need to make a new one by combining my permits_with_new_hucs huc and permit name with z1
unique_permits_with_new_hucs2 %<>% mutate(PERMIT = Permit)
z1_mod <- full_join(z1, unique_permits_with_new_hucs2, by = "PERMIT")
z1_mod %>% filter(!is.na(HUC_CODE)) -> test2
unique(test2$PERMIT) 
z1_mod %>% filter(is.na(HUC_CODE)) -> test3
unique(test3$PERMIT)
###################################################################################### 

sum_hucs_good1 <- test2 %>% group_by(huc = as.numeric(HUC_CODE),`CREDIT CLASSIFICATION`) %>% summarise(n_distinct(PERMIT), sum(CREDITS, na.rm = T),
                                                                                                  sum(ACRES, na.rm = T),
                                                                                                 sum(`LINEAR FEET`, na.rm = T) )

sum_hucs3_bad <- test3 %>% group_by(huc = as.numeric(HUC_CODE),`CREDIT CLASSIFICATION`) %>% summarise(n_distinct(PERMIT), sum(CREDITS, na.rm = T),
                                                                                                  sum(ACRES, na.rm = T),
                                                                                                  sum(`LINEAR FEET`, na.rm = T) )
sum_hucs_good2 <- z2 %>% group_by(huc = as.numeric(UPDATED_HUC8),`CREDIT CLASSIFICATION`) %>% summarise(n_distinct(PERMIT), sum(CREDITS, na.rm = T),
                                                                                                       sum(ACRES, na.rm = T),
                                                                                                       sum(`LINEAR FEET`, na.rm = T) )

# So now there's 
# sum_hucs2 = the new one
# sum_hucs3_bad = the new one, but without HUCs
# z2 # already has the hucs


HUC_fixed <- rbind(sum_hucs_good1, sum_hucs_good2, sum_hucs3_bad)
# Problem - now there's too many hucs - many of which I don't care about.
HUC_fixed_filter <- HUC_fixed %>% filter(huc %in% unique_hucs$huc)
# Now that leaves out the NAs
nas <- HUC_fixed %>% filter(is.na(huc))
  
HUC_fixed_filter <- rbind(HUC_fixed_filter, nas)
write_csv(HUC_fixed_filter, 'C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\OH\\huc_summary_filtered2.csv')

### NOW WORK IN EXCEL TO CREATE A TABLE BASED ON COWARDIN CLASSES ##############################

### NOW COMBINE WITH HUC SHAPEFILE AND EXPORT TO QGIS ###################################
hucs <- read_sf("C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\OH\\top_hucs_for_wdrs.shp")
wdr <- read_csv("C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\OH\\huc_summary_filtered.csv")

wdr = wdr %>% mutate(huc = `Row Labels`)
shp = left_join(hucs, wdr , by = "huc")

write_sf(shp, "C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\OH\\wdr_study_area_hucs.shp")

### NOW GET THE INIT AND REL CREDITS (INITAL AND RELEASED CREDITS - SUPPLY OF CREDITS) ######################
library(magrittr)
library(tidyverse)
library(sf)

setwd("C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\OH\\ledgers_with_init_rel") 
# this is one above your folder. Replace with your information.
mydir = "Louisville_ledgers" 
#this is your folder. Replace with your information
myfiles = list.files(path=mydir, pattern="*.csv$", full.names=TRUE)
# bind a list of csv files together and add a new naming variable
library(vroom)
louisville_banks <- vroom(myfiles, delim =",",id = "louisville")
## repeat

# this is one above your folder. Replace with your information.
mydir = "Huntington_ledgers" 
#this is your folder. Replace with your information
myfiles = list.files(path=mydir, pattern="*.csv$", full.names=TRUE)
# bind a list of csv files together and add a new naming variable
huntington_banks <- vroom(myfiles, delim =",",id = "huntington")

##repeat
# this is one above your folder. Replace with your information.
mydir = "Buffalo_ledgers" 
#this is your folder. Replace with your information
myfiles = list.files(path=mydir, pattern="*.csv$", full.names=TRUE)
# bind a list of csv files together and add a new naming variable
buffalo_banks <- vroom(myfiles, delim =",",id = "buffalo")

## repeat
# this is one above your folder. Replace with your information.
mydir = "Pittsburgh_ledgers" 
#this is your folder. Replace with your information
myfiles = list.files(path=mydir, pattern="*.csv$", full.names=TRUE)
# bind a list of csv files together and add a new naming variable
pittsburgh_banks <- vroom(myfiles, delim =",",id = "pittsburgh")

pittsburgh_banks2 <- pittsburgh_banks  %>% mutate(bank = pittsburgh) %>% 
  mutate(location = "Pittsburgh") %>% mutate(total_banks_location = "6") %>% dplyr::select(Type:total_banks_location) 

buffalo_banks2 <- buffalo_banks  %>% mutate(bank = BUFFALO) %>% 
  mutate(location = "buffalo_banks") %>% mutate(total_banks_location = "16") %>% dplyr::select(TYPE:total_banks_location) 

huntington_banks2 <- huntington_banks  %>% mutate(bank = huntington) %>% 
  mutate(location = "huntington_banks") %>% mutate(total_banks_location = "30") %>% dplyr::select(Type:total_banks_location) 

louisville_banks2 <- louisville_banks  %>% mutate(bank = louisville) %>% 
  mutate(location = "louisville_banks") %>% mutate(total_banks_location = "13") %>% dplyr::select(Type:total_banks_location) %>% dplyr::select(-`LRL Noncorps Mitigation`)

OH_banks <- rbind(pittsburgh_banks2,buffalo_banks2, huntington_banks2 ,louisville_banks2)


oh_banks_init <- OH_banks %>% filter(Type == "Init") %>% dplyr::select(everything(), init_credits = Credits)
oh_banks_rel <- OH_banks %>% filter(Type == "Rel") %>% dplyr::select(everything(), rel_credits = Credits)
 
x = oh_banks_init %>% group_by(`Credit Classification`, bank) %>% summarise(sum(init_credits, na.rm = T))

y = oh_banks_rel %>% group_by(`Credit Classification`, bank) %>% summarise(sum(rel_credits, na.rm = T))


write_csv(x, 'C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\OH\\OH_RIBITS_init_fixed.csv')
write_csv(y, 'C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\OH\\OH_RIBITS_rel_fixed.csv')
x2 =read_csv('C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\OH\\OH_RIBITS_init_fixed.csv')
y2 =read_csv('C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\OH\\OH_RIBITS_rel_fixed.csv')

full_join(x2, y2, by = "bank")
z = full_join(x2, y2, by = c("bank", "Credit Classification"))
z <- z %>% mutate(remaining_credits = `sum(init_credits, na.rm = T)` - `sum(rel_credits, na.rm = T)`)
write_csv(z, 'C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\OH\\OH_RIBITS_init_rel.csv')

## NOW I HAVE IMPACTS (WITH REQ MIT; WITHOUT REQ MIT), WDR'S (WITH HUCS ALL DETERMINED(BASED ON GUESSING PARTIALLY))#####
## AND HUCS MOSTLY DETERMINED), INIT, REL, AND EVEN A LIST OF ILF TRANSACTIONS.

#### LOOK INTO  STREAM PERMITS MISSING HUCs TO UNDERSTAND THEM #########################

bad <- read_csv('C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\OH\\bad_permits.csv')
x <- OH_banks %>% filter(PERMIT %in% bad$PERMIT)
x %>% group_by(BANK) %>% summarize(n_distinct(PERMIT)) %>% arrange(desc(`n_distinct(PERMIT)`))
write_csv(x, 'C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_AR_8.12.21\\OH\\bad_permits_summarized.csv')
