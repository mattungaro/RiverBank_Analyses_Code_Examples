# This is an analysis of developers' wetland and stream impacts in a subwatershed 
# in North Carolina, developed and designed for a mitigation firm. 
# It includes a data processing section, where I 
# edit and join several datasets together for my analysis of the subwatershed.

# This will allow the firm to determine if the demand for environmental offset work - 
# stream and wetland restoration - is present within the subwatershed.


# The BEST NEW BITS OF R CODE
pacman::p_load(  # Use p_load function from pacman
  janitor,       # Remove constants
  magrittr,      # Pipes
  pacman,        # Load/unload packages
  psych,         # Descriptive statistics
  rio,           # Import/export data
  tidyverse      # So many reasons
)
#p_unload() # alternative


# 1. GOAL: 

# DATA COLLECTION AND DATA UNDERSTANDING ###############################

library(tidyverse)

# First, get all of the datasets
csv2015 <- read_csv("C:\\Users\\Owner\\Documents\\Fort Worth Research\\Research\\3.14_research_redone_R_code\\ORM\\2015_Oct_to_Sep.csv") #%>% filter(Action == "Impact")
csv2016 <- read_csv("C:\\Users\\Owner\\Documents\\Fort Worth Research\\Research\\3.14_research_redone_R_code\\ORM\\2016_Oct_to_Sep.csv") #%>% filter(DISTRICT == "SWF" & ACTION == "Impact")
csv2017 <- read_csv("C:\\Users\\Owner\\Documents\\Fort Worth Research\\Research\\3.14_research_redone_R_code\\ORM\\2017_Oct_to_Sep.csv") #%>% filter(DISTRICT == "SWF" & ACTION == "Impact")
csv2018 <- read_csv("C:\\Users\\Owner\\Documents\\Fort Worth Research\\Research\\3.14_research_redone_R_code\\ORM\\2018_Oct_to_Sep.csv") #%>% filter(DISTRICT == "SWF" & ACTION == "Impact")
csv2019 <- read_csv("C:\\Users\\Owner\\Documents\\Fort Worth Research\\Research\\3.14_research_redone_R_code\\ORM\\2019_Oct_to_Sep.csv") #%>% filter(DISTRICT == "SWF" & ACTION == "Impact")
csv2020 <-read_csv("C:\\Users\\Owner\\Documents\\Fort Worth Research\\FY2020 FOIA Report r19Mar2021.csv")
colnames(csv2020)

# DATA PROCESSING ################################

# First, turn everything into UPPERCASE (and clean up 2015 and 2020)
up <- function() {
  for (df in ls(.GlobalEnv)[1:(length(ls(.GlobalEnv))-1)]){
    e <<- .GlobalEnv
    if (is.data.frame(e[[df]])) {
      colnames(e[[df]]) <<- toupper(colnames(e[[df]]))
    }
  }
  rm(e,pos=".GlobalEnv")
}
up() ## https://www.roelpeters.be/change-column-names-to-uppercase-in-r/ 

# Second, inject underscore into everything
inject_underscore <- function() {
  for (df in ls(.GlobalEnv)[1:(length(ls(.GlobalEnv))-1)]){
    e <<- .GlobalEnv
    if (is.data.frame(e[[df]])) {
      colnames(e[[df]]) <<- gsub(" ", "_", colnames(e[[df]]))
    }
  }
  rm(e,pos=".GlobalEnv")
}
inject_underscore() 

colnames(csv2019)
# HUC8 or HUC, ACTION, END DATE, AUTH_LINEAR_FT or AUTH_FILL_LENGTH_FT

clean_up <- function(df) {

if ("HUC8"  %in% colnames(df)){ 
 df <-  df %>% mutate(huc = as.numeric(HUC8))%>% select(everything())
} else if ("HUC"  %in% colnames(df)){ 
  df <-   df %>% mutate(huc = as.numeric(HUC)) %>% select(everything())
} else { print("huc fail")}

if( ("AUTH_LINEAR_FT" %in% colnames(df)) && ("AUTH_FILL_LENGTH_FT" %in% colnames(df))){
  df <-   df %>% mutate(lf1 = as.numeric(AUTH_LINEAR_FT), lf2 = AUTH_FILL_LENGTH_FT)%>% select(everything())
} else {print("no length ft")
    }

if( ("AUTH_LINEAR_FT" %in% colnames(df)) && !("AUTH_FILL_LENGTH_FT" %in% colnames(df))){
  df <-   df %>% mutate(lf1 = as.numeric(AUTH_LINEAR_FT))%>% select(everything())
} else {print("probably contains both or only length ft")
}

if( !("AUTH_LINEAR_FT" %in% colnames(df)) && ("AUTH_FILL_LENGTH_FT" %in% colnames(df))){
  df <-  df %>% mutate(lf1 = as.numeric(AUTH_LINEAR_FT))%>% select(everything())
} else {print("probably contains only linear feet")
}

  df2 <- df %>% filter(huc == 5050001 & ACTION == "Impact") %>% 
    select(contains("lf"), contains("COWARDIN"), contains("RQD"), huc,ACTION, END_DATE, IMPACT_ID, DISTRICT,
                                    DA_NUMBER, ACTION, ACTION_TYPE, AUTH_FILL_ACRES)
}
csv2015_updated = clean_up(csv2015)
csv2016_updated = clean_up(csv2016)
csv2017_updated = clean_up(csv2017)
csv2018_updated = clean_up(csv2018)
csv2019_updated = clean_up(csv2019)
csv2020_updated = clean_up(csv2020)

csv2015 <- csv2015_updated %>% mutate( lf2 = 0) %>% 
  select(COMPENSATORY_MITIGATION_RQD = COMP_MIT_RQD, lf1, lf2, 
         cowardin = COWARDIN_CLASS, huc, ACTION, END_DATE, IMPACT_ID, DA_NUMBER, AUTH_FILL_ACRES)

csv2016 <- csv2016_updated %>% mutate( lf2 = 0) %>% 
  select(COMPENSATORY_MITIGATION_RQD,lf1, lf2, cowardin = COWARDIN_NAME, huc, ACTION, END_DATE, IMPACT_ID, DA_NUMBER, AUTH_FILL_ACRES)

csv2017 <- csv2017_updated  %>% 
  select(COMPENSATORY_MITIGATION_RQD,lf1, lf2, cowardin = COWARDIN_NAME, huc, ACTION, END_DATE, IMPACT_ID, DA_NUMBER, AUTH_FILL_ACRES)

csv2018 <- csv2018_updated  %>% 
  select(COMPENSATORY_MITIGATION_RQD,lf1, lf2, cowardin = COWARDIN_NAME, huc, ACTION, END_DATE, IMPACT_ID, DA_NUMBER, AUTH_FILL_ACRES)

csv2019 <- csv2019_updated  %>% 
  select(COMPENSATORY_MITIGATION_RQD,lf1, lf2, cowardin = COWARDIN_NAME, huc, ACTION, END_DATE, IMPACT_ID, DA_NUMBER, AUTH_FILL_ACRES)

csv2020 <- csv2020_updated  %>% 
  select(COMPENSATORY_MITIGATION_RQD = COMP_MIT_RQD,lf1, lf2, cowardin = COWARDIN_CLASS, huc, ACTION, END_DATE, IMPACT_ID, DA_NUMBER, AUTH_FILL_ACRES)

blowing_rock <- rbind(csv2015, csv2016, csv2017, csv2018, csv2019, csv2020)
# A town in NC

summary(blowing_rock)

blowing_rock$lf2 <- as.numeric(blowing_rock$lf2)
blowing_rock$lf1[is.na(blowing_rock$lf1)] <- 0
blowing_rock$AUTH_FILL_ACRES <- as.numeric(blowing_rock$AUTH_FILL_ACRES)
blowing_rock$lf3 <- blowing_rock$lf1 + blowing_rock$lf2
blowing_rock <- blowing_rock %>% 
  select(lf3, cowardin, huc, ACTION, END_DATE, IMPACT_ID, DA_NUMBER, AUTH_FILL_ACRES, COMPENSATORY_MITIGATION_RQD)

# fix dates
library(lubridate)
blowing_rock$END_DATE
blowing_rock$END_DATE2 <- dmy(blowing_rock$END_DATE)
blowing_rock$year <- year(blowing_rock$END_DATE2)
blowing_rock$month <- month(blowing_rock$END_DATE2)

blowing_rock <-
  blowing_rock %>%
  mutate(month_year = floor_date(as_date(END_DATE2), "month"))

riv_summary_not_RQD <- blowing_rock %>% 
  filter(grepl("RIV", cowardin)& COMPENSATORY_MITIGATION_RQD == "N") %>%
  group_by(year) %>%
  summarize(N_notRQD = sum(lf3, na.rm = T)) %>%
  ungroup() # USE THIS FOR GRAPHS --------------------
riv_summary_not_RQD$N_notRQD[is.na(riv_summary_not_RQD$N_notRQD)] <- 0

riv_summary_RQD <- blowing_rock %>% 
  filter(grepl("RIV", cowardin) & COMPENSATORY_MITIGATION_RQD == "Y") %>%
  group_by(year) %>%
  summarize(N_RQD = sum(lf3, na.rm = T)) %>%
  ungroup() # USE THIS FOR GRAPHS --------------------

riv_summary_RQD$N_RQD[is.na(riv_summary_RQD$N_RQD)] <- 0


non_riv_summary_not_RQD <- blowing_rock %>% 
  filter(!grepl("RIV", cowardin)& COMPENSATORY_MITIGATION_RQD == "N") %>%
  group_by(year) %>%
  summarize(N_notRQD = sum(AUTH_FILL_ACRES, na.rm = T)) %>%
  ungroup()
non_riv_summary_not_RQD$N_notRQD[is.na(non_riv_summary_not_RQD$N_notRQD)] <- 0

non_riv_summary_RQD <- blowing_rock %>% 
  filter(!grepl("RIV", cowardin)& COMPENSATORY_MITIGATION_RQD == "Y") %>%
  group_by(year) %>%
  summarize(N_RQD = sum(AUTH_FILL_ACRES, na.rm = T)) %>%
  ungroup()
non_riv_summary_RQD$N_RQD[is.na(non_riv_summary_RQD$N_RQD)] <- 0
# USE THIS FOR GRAPH --------------------------
# will need to also get the RQD part


# MISSING DATA VISUALIZATION

missing.values <- blowing_rock %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)

levels <-
  (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key

percentage.plot <- missing.values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "", 
                    values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
  coord_flip() +
  labs(title = "Percentage of missing values", x =
         'Variable', y = "% of missing values")

percentage.plot

x= blowing_rock %>% filter(is.na(lf3) & is.na(AUTH_FILL_ACRES))
x %>% filter(grepl("RIV", cowardin)) %>% select(DA_NUMBER) %>% unique()
# 10 unique riv permits with no LF or AC


# SHOW DUPLICATES OF EACH ROW

n_distinct(blowing_rock$DA_NUMBER) # 222 permits
n_distinct(blowing_rock$IMPACT_ID) # but 567 unique identifiers

dup_check <- 
  blowing_rock %>% select(-IMPACT_ID)
n_distinct(dup_check) # 55 duplicates without impact id

dups <- subset(dup_check,duplicated(dup_check))
riv_dups <- dups %>% filter(grepl("RIV", cowardin)) 
sum(riv_dups$lf3, na.rm = T)
# 482 LF of streams that are duplicated from the main dataset.
# Keep in the main dataset for now - tell Adam however.
# PotentiaLly remove later
pal_dups <- dups %>% filter(grepl("PAL", cowardin)) 
sum(pal_dups$AUTH_FILL_ACRES, na.rm = T)
#  0.31 AC of wetlands that are duplicated from the main dataset. 

# CHECK FOR DATA IRREGULARITIES/INCONSISTENCIES
blowing_rock <- blowing_rock %>% mutate(cowardin2 = ifelse(grepl("RIV", cowardin), "riv", "other")) 
summary(blowing_rock)

barplot(table(blowing_rock$year))
barplot(table(blowing_rock$cowardin2))
blowing_rock_RQD <- blowing_rock %>% select(COMPENSATORY_MITIGATION_RQD, cowardin2, DA_NUMBER)
blowing_rock_RQD <- unique(blowing_rock_RQD)

counts <- table(blowing_rock_RQD$cowardin2, blowing_rock_RQD$COMPENSATORY_MITIGATION_RQD)
barplot(counts,
        main="Count of Permits",
        xlab="Mitigation Required", col=c("#ef8a62","#67a9cf"),
        legend = rownames(counts))

# outlier graph 
qplot(lf3, data = blowing_rock) 
# lots of low levels of impacts on linear feet
blowing_rock %>% filter(lf3 > 8000)
# one linear foot impact over 8000 ft
qplot(blowing_rock$AUTH_FILL_ACRES, data = blowing_rock) # same with acres
num_permits <- blowing_rock %>% group_by(COMPENSATORY_MITIGATION_RQD, DA_NUMBER) %>%
tally() %>% arrange(desc(n)) 
# approximately 19 permits with required mitigation.
# 207 without. (some overlap - there's only 222 permits in total
# therefore there are 4 permits that have both)
unique(num_permits$DA_NUMBER)


blowing_rock %>% group_by() %>%
  tally() %>% arrange(desc(n))
# just graph each column? Histogram and boxplot

# ANALYSIS AND VISUALIZATION ########################################

# COMPARE ALL THE COLUMNS - CHECK RELATIONSHIPS

br_summary <- blowing_rock %>% 
  group_by(cowardin, COMPENSATORY_MITIGATION_RQD) %>% 
  summarize(all_acres = sum(AUTH_FILL_ACRES, na.rm = T),all_LF = sum(lf3, na.rm = T))


# SUM UP ALL THE IMPACTS BY PERMIT AND MIT RQD
# GRAPH RIV SUMMARY (WITH MIT RQD AND MIT NOT RQD) AND PAL SUMMARY (SAME)
riv_summary_not_RQD <- riv_summary_not_RQD %>% 
  mutate(year, N = N_notRQD, RQD = FALSE) %>% select(year, N, RQD)
riv_summary_RQD <- riv_summary_RQD %>% 
  mutate(year, N = N_RQD, RQD = T) %>% select(year, N, RQD)
riv_summary_RQD <- rbind(riv_summary_RQD, tibble(year = as.numeric(2018), 
                              N = as.numeric(0), 
                              RQD = as.logical(TRUE)))
# add a row for a missing year. 

riv <- rbind(riv_summary_RQD, riv_summary_not_RQD)
riv %>% ggplot(aes(x = year,
                   y = N, fill = RQD))+
  geom_bar(stat="identity", position = "dodge") + ggtitle("Riverine Impacts")+
  ylab("Linear Feet")+xlab("Year")+
  theme(plot.title = element_text(size=20, face="bold.italic"),
        axis.title.x = element_text(size=20, face="bold"),
        axis.title.y = element_text(size=20, face="bold"), 
        axis.text=element_text(size=18)) +
  scale_fill_discrete(breaks=c("TRUE", "FALSE"), type = c("#5ab4ac", "#d8b365")) 

# GRAPH PAL SUMMARY (WITH MIT RQD AND MIT NOT RQD) AND PAL SUMMARY (WITH MIT RQD AND MIT NOT RQD)
non_riv_summary_not_RQD <- non_riv_summary_not_RQD %>% 
  mutate(year, N = N_notRQD, RQD = FALSE) %>% select(year, N, RQD)
non_riv_summary_RQD <- non_riv_summary_RQD %>% 
  mutate(year, N = N_RQD, RQD = T) %>% select(year, N, RQD)
non_riv_summary_RQD <- rbind(non_riv_summary_RQD, tibble(year = as.numeric(2019), 
                                                 N = as.numeric(0), 
                                                 RQD = as.logical(TRUE)))
non_riv_summary_RQD <- rbind(non_riv_summary_RQD, tibble(year = as.numeric(2020), 
                                                         N = as.numeric(0), 
                                                         RQD = as.logical(TRUE)))
# add two rows for missing years

pal <- rbind(non_riv_summary_RQD, non_riv_summary_not_RQD)
pal %>% ggplot(aes(x = year,
                   y = N, fill = RQD))+
  geom_bar(stat="identity", position = "dodge") + ggtitle("Non-Riverine Impacts")+
  ylab("Acres")+xlab("Year")+
  theme(plot.title = element_text(size=20, face="bold.italic"),
        axis.title.x = element_text(size=20, face="bold"),
        axis.title.y = element_text(size=20, face="bold"), 
        axis.text=element_text(size=18)) +
  scale_fill_discrete(breaks=c("TRUE", "FALSE"), type = c("#5ab4ac", "#d8b365")) 

pal <- pal %>% arrange((year))
riv <- riv %>% arrange((year))

pal_riv <- (cbind(pal, riv))
write_csv(pal_riv, "pal_riv.csv")


## Count of Permits

blowing_rock3 <- blowing_rock %>% filter(COMPENSATORY_MITIGATION_RQD == "Y") %>%
  group_by(year, cowardin2) %>% summarize(number_permits = n_distinct(DA_NUMBER, na.rm = T))

blowing_rock3

z = blowing_rock%>% filter(COMPENSATORY_MITIGATION_RQD == "Y") %>% 
  filter(cowardin2 == "riv" & year == 2020)

write_csv(blowing_rock3, "pal_riv_count.csv")


## CHECK WORK

## check if in 2018, other impacts (MIT REQ) sum to 0.328
blowing_rock %>% filter(year == 2018 & COMPENSATORY_MITIGATION_RQD == "Y")
# TRUE
## Check if in 2015, riverine impacts (not mit req) sum to about 6000 linear feet
blowing_rock %>% filter(year == 2015 & COMPENSATORY_MITIGATION_RQD == "N") %>% 
  select(lf3) %>% sum(na.rm = T)
# TRUE


## Finally, let's break this into eph, int, per etc. impacts per year (mit req and not required)

blowing_rock_cow <- blowing_rock %>% 
  mutate(cowardin_new= ifelse(grepl("PER",cowardin), "per", 
         ifelse(grepl("PAL", cowardin), "pal", 
              ifelse(grepl("INTERM", cowardin), "int",
                   ifelse(grepl("EPH", cowardin), "eph",
                        ifelse(grepl("R-", cowardin), "unclass_riv",
                 NA))))))
blowing_rock_cow_w_RQD <-
  blowing_rock_cow %>% filter(COMPENSATORY_MITIGATION_RQD == "Y")
blowing_rock_cow_wo_RQD <- 
  blowing_rock_cow %>% filter(COMPENSATORY_MITIGATION_RQD == "N")

RQD_summary <- blowing_rock_cow_w_RQD %>% 
  group_by(cowardin_new,year) %>% 
  summarize(LF = sum(lf3, na.rm = T), 
            AC = sum(AUTH_FILL_ACRES, na.rm = T))
not_RQD_summary <- blowing_rock_cow_wo_RQD %>% 
  group_by(cowardin_new,year) %>% 
  summarize(LF = sum(lf3, na.rm = T), 
            AC = sum(AUTH_FILL_ACRES, na.rm = T))

write_csv(RQD_summary, "summary_mit_rqd.csv")
write_csv(not_RQD_summary, "summary_mit_not_rqd.csv")


# CLEAN UP #################################################

# Clear console
# Clear data
rm(list = ls())  # Removes all objects from the environment

# Clear packages
detach("package:datasets", unload = T)  # For base packages
p_unload(all)    # Remove all contributed packages

# Clear plots
graphics.off()   # Clears plots, closes all graphics devices

cat("\014")      # Mimics ctrl+L