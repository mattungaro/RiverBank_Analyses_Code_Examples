
## This was the first project that I worked on for RiverBank.
## We estimated how many linear feet of intermittent and ephermal streams that
## developers from 2010-2015 had impacted, which, under
## the Trump Administration's new mitigation policy, 
## would continue with previous mtigation requirements removed.

library(tidyverse)
icc2 <- read_csv("C:\\Users\\Owner\\Documents\\RAships and Riggsbee Work\\Data_Analysis_Adam_Riggsbee\\ICC_RBC_eph_int_per.csv")

icc2 %>% filter(cowardin == "intermittent") -> ee

total <- c(30979.02, 22426.01, 52.04)

icc2 %>% ggplot(mapping = aes(x = cowardin, y = credits, fill = ICC))+geom_col()+xlab("Cowardin Classes")+ylab("Total Credits")+ggtitle("Credit Withdrawals from 2014-2019")+
  ggthemes::scale_color_calc()+ggthemes::theme_calc()+ggthemes::scale_fill_few()+theme(text = element_text(size = 12))+
  scale_fill_manual(name="Classification",
            labels=c("RBC", "ICC"), values = c("#253494", "#41b6c4"))+
geom_label(
  label="sum = 30979.02", 
  x=1,
  y=31979.02,
  label.size = 0.05,
  color = "black", fill="#FFFFFF"
)+geom_label(
  label="sum = 22426.01", 
  x=2,
  y=23426.01,
  label.size = 0.05,
  color = "black", fill="#FFFFFF"
)+geom_label(
  label="sum = 52.04", 
  x=3,
  y=1052.04,
  label.size = 0.05,
  color = "black", fill="#FFFFFF"
)+scale_y_continuous(sec.axis = sec_axis(~(. / 53457.07),labels = scales::percent, name = "Percent"))
