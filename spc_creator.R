#filename to be changed manually
filename = "Feb PIPG UEC 76% MSIT data.xlsx"


#install packages
if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(Rcpp, tidyverse,readxl,dplyr,lubridate,ggplot2,NHSRplotthedots,magrittr,ggthemes,gdtools,hms,ggpubr,scales)


#load data from file - File name needs to be manually changed
data_tbl <- read_excel(filename, sheet = "SE provider level data")
colnames(data_tbl) <- c("ICB_1","Provider_1","month_1","Attendances","Breaches","Performance","Type 1 attendances","Type 1 breaches"
                         ,"Type 1 performance","Emergency admissions","4+ hour delays from decision to admit","12+ hour delays from decision to admit"
                         ,"Provider","ICB","Month")


data_tbl <- data_tbl %>%
  filter(Month >= "2021-10-01") %>% 
  #mutate(Month = ymd(Month)) %>%
  #mutate(Month = format(Month,'%b-%y')) %>% 
  select(-c("ICB_1","Provider_1","month_1")) %>% 
  filter(!is.na(Performance))


systems_tbl <- data_tbl %>% 
  filter(!is.na(ICB)) %>% 
  group_by(ICB,Month) %>% 
  summarise(Attendances = sum(Attendances)
            ,Breaches = sum(Breaches)) %>% 
  ungroup() %>% 
  mutate(Performance = 1-(Breaches/Attendances))


#Create SPC for providers
x1 <- ptd_spc(.data = data_tbl
              ,value_field = Performance
              ,date_field = Month
              ,facet_field = Provider
              ,improvement_direction = "increase"
              #,target = ptd_target(0.76)
              #,rebase = ptd_rebase(as.Date("2023-01-09"))
              )

c1 <- ptd_create_ggplot(x1
                        ,point_size = 1.5
                        ,main_title = NULL
                        ,x_axis_label = "Month"
                        ,y_axis_label = "All Type"
                        ,x_axis_breaks = "1 year"
                        ,fixed_y_axis_multiple = FALSE
                        ,icons_position = "top left"
                        )


windowsFonts(A = windowsFont("Arial"))
c1 <- c1 + theme_tufte()+
  #scale_x_date(date_labels =  '%b-%y')+
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text (angle = 90))+
  theme(text = element_text(family = "A"))+
  theme(legend.position = "bottom")+
  theme(legend.title=element_blank())

c1


#Create SPC for systems
x2 <- ptd_spc(.data = systems_tbl
              ,value_field = Performance
              ,date_field = Month
              ,facet_field = ICB
              ,improvement_direction = "increase"
              ,target = ptd_target(0.76)
              #,rebase = ptd_rebase(as.Date("2023-01-01"))
              )

c2 <- ptd_create_ggplot(x2
                        ,point_size = 1.5
                        ,main_title = NULL
                        ,x_axis_label = "Month"
                        ,y_axis_label = "All Type"
                        ,x_axis_breaks = "1 year"
                        ,fixed_y_axis_multiple = FALSE
                        ,icons_position = "top left"
                        )


windowsFonts(A = windowsFont("Arial"))
c2 <- c2 + theme_tufte()+
  #scale_x_date(date_labels =  '%b-%y')+
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text (angle = 90))+
  theme(text = element_text(family = "A"))+
  theme(legend.position = "bottom")+
  theme(legend.title=element_blank())

c2