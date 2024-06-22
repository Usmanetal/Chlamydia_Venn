library(readxl)
library(janitor)
library(tidyverse)
library(finalfit)
library(dplyr)
library(MASS)
library(factoextra)
library(rstatix)
library(ggpubr)
#RESEARCH DATA-1.xlsx
Infertile_type <- read_excel("C:/Users/Usman Ola/Documents
            /Chlamydia-Host_Immunology/RESEARCH DATA-1.xlsx",sheet="Sheet1")

Infertile_type<-RESEARCH_DATA_1 <- read_excel("RESEARCH DATA-1.xlsx", 
                              sheet = "Sheet1")
Infertile_type <- clean_names(Chlamydia_Venn)
names(Infertile_type) # All variables names are automagically cleaned!

Infertile_newdata<-Chlamydia_logistic_reg[!Chlamydia_logistic_reg$how_long_7=="Nil",]%>%
  mutate(age_cat=cut(age,
                     breaks = c(0,35,Inf),
                     labels = c("<35",">35")),
         Marital_dur=ifelse(how_long_7=="1","First_Child","Children"),
         Marital_cat=cut(as.numeric(how_long_7),
                         c(0,20,Inf),
                         c("≤20",">20")),
         Times_mar=case_when(how_times=="Fouth"~"second",
                             how_times=="Third"~"second",
                             how_times=="First"~"first",
                             how_times=="Second"~"second"),
         wom_edu=str_replace(level_edu,"p","P"),
         husband_occ=case_when(
           husb_occ=="Leccturer"~"Lecturer",
           husb_occ=="Teacher"~"Lecturer",
           husb_occ=="Civil Serv"~"Civil_ser",
           husb_occ=="Lecturer"~"Lecturer",
           husb_occ=="Civil serv"~"Civil_ser",
           husb_occ=="Publicserv"~"Civil_ser",
           husb_occ=="Civilserv"~"Civil_ser",
           husb_occ=="Bussiness"~"Bussines",
           husb_occ=="Business"~"Bussines",
           husb_occ=="Carpenter"~"Artisan",
           husb_occ=="Artisan"~"Artisan",
           husb_occ=="Farmer"~"Farmer",
           TRUE~"others"),
         fam_type=str_replace_all(family_typ,c("mono"="Mono")),
         woman_occ=case_when(
           occup=="Teacher"~"Teacher",
           occup=="teacher"~"Teacher",
           occup=="Civil Serv"~"Civil_ser",
           occup=="House wife"~"House_wife",
           occup=="Civil serv"~"Civil_ser", 
           occup=="Civilserv"~"Civil_ser",
           occup=="Student"~"Student",
           occup=="Tailor"~"Artisan",
           occup=="Artisan"~"Artisan", 
           occup=="Farmer"~"Farmer",
           TRUE~"others"),woman_occ1=ifelse(woman_occ=="Teacher","Teacher","other"),
         woman_occ2=str_replace_all(woman_occ,c("Farmer"="others","Student"="others","Teacher"="others","Artisan"="others")),
         woman_occ2f=factor(woman_occ2,levels=c("others","House_wife","Civil_ser")),
         any_preg=recode(any_preg,"No"="Primary_infert",
                         "Yes"="secondary_infert"),
         how_many=recode(
           how_many,
           "Eight"=8,
           "Five"=5,
           "Four"=4,
           "Nil"=0,
           "Once"=1,
           "One"=1,
           "Seven"=7,
           "Six"=6,
           "Ten"=10,
           "Three"=3,
           "Thrice"=3,
           "Twice"=2,
           "Twince"=2,
           "Two"=2,
           "10"=10,
           "2"=2,
           "3"=3),
         Pregnancy=cut(how_many,c(-Inf,0.9,1,5,10),
                       c("No_preg","once","2-5","6-10")), 
         to_term=recode(
           to_term,
           "Eight"=8,
           "Five"=5,
           "Four"=4,
           "Nil"=0,
           "Once"=1,
           "One"=1,"0ne"=1,"one"=1,
           "Seven"=7,
           "Six"=6,
           "Ten"=10,
           "Three"=3,
           "Thrice"=3,
           "Twice"=2,
           "Twince"=2,
           "Two"=2,
           "10"=10,
           "2"=2,
           "3"=3,"Nine"=9),
         term_cat=cut(to_term,c(-Inf,0.9,1,5,10),right=TRUE,
                      c("no_child","One_child","2-5","6-10"),.drop=FALSE) ,
         age_of_last=as.numeric(str_replace(age_of_last,"Nil","0")),
         ageoflast_cat=cut(age_of_last,c(-Inf,0.9,1,5,10,15,20),
                           c("none","One_year","1-5","6-10","11-15","16-20")),
         family_pla,
         which_one,
         how_long_20=recode(
           how_long_20,"1"=1,"2"=2,"3"=3,"5"=5,"6"=6,"Nil"=0 ,"<6M"=1,"NiL"=0),
         Vaginal_discharge=str_replace_all(color,c("NIL"="Nil","Brownish"="Reddish")),
         vaginal_discharge1=str_replace_all(Vaginal_discharge,c("Brownish"="Yellowish","Reddish"="Yellowish")),
         il_10_quatile=ntile(il_10_avrg,3),
         il_10_quatile_cat=recode(il_10_quatile,"1"="firstq","2"="sec","3"="thir"),
         ifn_gam_quatile=ntile(ifn_gamm_avrg,3),
         ifn_gam_quatile_cat=recode(ifn_gam_quatile,"1"="firstq","2"="sec","3"="thir"),
         hsp_60_quatile=ntile(hsp_60_avrg,3),chlamydia_pos=ifelse(ig_g=="Pos"|ig_m=="Pos","Pos","Neg"),
         hsp_60_quatile_cat=recode(hsp_60_quatile,"1"="firstq","2"="sec","3"="thir")
         
  )
