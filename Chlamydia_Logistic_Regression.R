#Data wrangling of code for SecioDemographic Characteristics
# median and IQR for Marital Duration of couples
#Correlation and Logistic regression code chunk
#To syntactically clean our data we use "janitor" package
library(ggpubr)
library(tidyverse)
library(janitor)
Chlamydia_logistic_reg <- clean_names(Chlamydia_Venn)
names(Chlamydia_logistic_reg) # All variables names are automagically cleaned!
glimpse(Chlamydia_logistic_reg)

# Age categorization

age_cat<-Chlamydia_logistic_reg%>%group_by(group)%>%
  mutate(age1=cut(age,
                  breaks = c(0,35,Inf),
                  labels = c("<35",">35")))%>%
  count(age1)%>% 
  pivot_wider(names_from=group,values_from=n)%>%
  column_to_rownames(var="age1")
#%>%chisq.test(correct=FALSE)

#R code 1
Marital_Duration_couples_mmm<-Chlamydia_logistic_reg[!Chlamydia_logistic_reg$how_long_7=="Nil",]%>%
  reframe(median=median(as.numeric(how_long_7)),
          median=median(as.numeric(how_long_7)),mode=mode(as.numeric(how_long_7)),
          qs=list(quantile(as.numeric(how_long_7))))%>%
  unnest_wider(qs)

Marital_Duration_couples_mmm<-Chlamydia_logistic_reg%>%
  mutate(how_long_7=ifelse(how_long_7=="1","First_Child","Children"))%>%
  group_by(group)%>%
  count(how_long_7)%>%
  pivot_wider(names_from=group,values_from=n)%>%
  column_to_rownames(var="how_long_7")

#R code 2
#Chlamydia_logistic_reg[-100,Chlamydia_logistic_reg$how_long_7]%>%
 # select((as.numeric(Chlamydia_logistic_reg$how_long_7)))%>%
  #summarise_at(
   # vars(.),list(min=min,max=max,
    #             median=median,Q1=quantile(.,0.25),Q3=quantile(.,0.75)))
# the above code works only when cleaned-up as numeric

Marital_Duration_Couples_Categ<-Chlamydia_logistic_reg[!Chlamydia_logistic_reg$how_long_7=="Nil",]%>%
  group_by(group)%>%
  mutate(categ=cut(as.numeric(how_long_7),c(0,10,20,Inf),c("<10","11-20",">20")))%>%
  count(categ)%>%
  pivot_wider(names_from=group,values_from=n)%>%
  column_to_rownames(var="categ")


#no. of life time sex partner
no_times_married<-Chlamydia_logistic_reg[!Chlamydia_logistic_reg$how_times=="Nil",]%>%
  group_by(group)%>%
  mutate(Times_mar=case_when(how_times=="Fouth"~"second",how_times=="Third"~"second",how_times=="First"~"first",how_times=="Second"~"second"))%>%
  count(Times_mar)%>%
  pivot_wider(names_from=group,values_from=n)%>%
  column_to_rownames(var="Times_mar")

woman_Education<-Chlamydia_logistic_reg%>%
  group_by(group)%>%
  mutate(wom_edu=str_replace(level_edu,"p","P"))%>%count(wom_edu)%>%
  pivot_wider(names_from=group,values_from=n)%>%
  column_to_rownames(var="wom_edu")

Husban_Occupation<-Chlamydia_logistic_reg%>%group_by(group)%>%
  mutate(husband_occ=case_when(
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
TRUE~"others"))%>%
  count(husband_occ)%>%
  pivot_wider(names_from=group,values_from=n)%>%
  column_to_rownames(var="husband_occ")

Fam_type<-Chlamydia_logistic_reg%>%
  group_by(group)%>%
  mutate(fam_type=str_replace_all(family_typ,c("mono"="Mono")))%>%
  count(fam_type)%>%
  pivot_wider(names_from=group,values_from=n)%>%
  column_to_rownames(var="fam_type")

Woman_occupation<-Chlamydia_logistic_reg%>%group_by(group)%>%
  mutate(woman_occ=case_when(
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
    TRUE~"others"))%>%
  count(woman_occ)%>%
  pivot_wider(names_from=group,values_from=n)%>%
  column_to_rownames(var="woman_occ")%>%mutate(Fertile=replace_na(Fertile,0))

Infertility_cat<-Chlamydia_logistic_reg%>%
  group_by(group)%>%
  count(any_preg)%>%
  mutate(any_preg=recode(any_preg,"No"="Primary_infert",
                         "Yes"="secondary_infert"))%>%
  pivot_wider(names_from=group,values_from=n)%>%
  column_to_rownames(var="any_preg")



History_pregnancy<-Chlamydia_logistic_reg%>%
 group_by(group)%>%
  mutate(how_many=recode(
    how_many,
    "Eight"=8,
    "Five"=5,
    "Four"=4,
    "Nil"=0, # changed from 0 to 12 to allow for analysis
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
    grp=cut(how_many,c(-Inf,0.9,1,5.0,10),
    c("no_Child","one_child","1-5","6-10")))%>%count(grp)%>%
  pivot_wider(names_from=group,values_from=n)%>%
  column_to_rownames(var="grp")%>%mutate(Fertile=replace_na(Fertile,0))
# the 'Nil' has been changed to for 0 to 12 to allow for chi-sq.test analysis

To_term<-Chlamydia_logistic_reg%>%
  group_by(group)%>%
  mutate(to_term=recode(
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
    grp=cut(to_term,c(-Inf,0.9,1,5,10),right = TRUE,ordered_result = TRUE,
            c("no_Child","one_child","1-5","6-10")))%>%count(grp)%>%
  pivot_wider(names_from=group,values_from=n)%>%
  column_to_rownames(var="grp")%>%mutate(Fertile=replace_na(Fertile,0))
#%>%chisq.test()

Last_age_of_child<-Chlamydia_logistic_reg%>%
  group_by(group)%>%
  mutate(age_of_last=as.numeric(str_replace(age_of_last,"Nil","0")),
         grp=cut(age_of_last,c(-Inf,0.9,1,5,10,15,20),
                 c("none","One_year","1-5","6-10","11-15","16-20"),.drop=FALSE))%>%
  count(grp)%>%
  pivot_wider(names_from=group,values_from=n)%>%
  column_to_rownames(var="grp")%>%
  mutate(Fertile=replace_na(Fertile,0),Infertile=replace_na(Infertile,0))
#%>%chisq.test()

Family_plan<-Chlamydia_logistic_reg%>%
  group_by(group)%>%count(family_pla)%>%
  pivot_wider(names_from=group,values_from=n)%>%
  column_to_rownames(var="family_pla")%>%
  mutate(Fertile=replace_na(Fertile,0),
         Infertile=replace_na(Infertile,0))

What_type_of_contraceptive<-Chlamydia_logistic_reg%>%
  group_by(group)%>%count(which_one)%>%
  pivot_wider(names_from=group,values_from=n)%>%
  column_to_rownames(var="which_one")%>%
  mutate(Fertile=replace_na(Fertile,0),
         Infertile=replace_na(Infertile,0))

How_long_use_contra<-Chlamydia_logistic_reg%>%group_by(group)%>%
  mutate(how_long_20=recode(how_long_20,"1"=1,"2"=2,"3"=3,"5"=5,"6"=6,"Nil"=0 ,"<6M"=1,"NiL"=0))%>%
  mutate(how_long_20=cut(how_long_20,c(-Inf,0.9,1,6),c("none","one_year","2-6")))%>%
 count(how_long_20)%>%pivot_wider(names_from=group,values_from=n)%>%
  column_to_rownames(var="how_long_20")

#Vaginal disc or menstrual Disc/cup originally used to prevent menstrual bleeding
#as an Eco-friendly alternative to tampon is increasing used for fertility
#(prevention of sperm backflow from the vagina)
# as a second-use
Vaginal_disc<-Chlamydia_logistic_reg%>%
  group_by(group)%>%
  count(vaginal_disc)%>%
  pivot_wider(names_from=group,values_from=n)%>% remove_rownames %>%
  column_to_rownames(var="vaginal_disc") 

Vaginal_discharge<-Chlamydia_logistic_reg%>%group_by(group)%>%
  count(color=str_replace_all(color,c("NIL"="Nil")))%>%
  pivot_wider(names_from=group,values_from=n)%>% remove_rownames %>%
  column_to_rownames(var="color") 

Odour<-Chlamydia_logistic_reg%>%
  group_by(group)%>%
  count(odour)%>%
  pivot_wider(names_from=group,values_from=n)%>% remove_rownames %>%
  column_to_rownames(var="odour")

#Pelvic Pain and discomfort
Discomfort<-Chlamydia_logistic_reg%>%
  group_by(group)%>%
  count(discomfort)%>%
  pivot_wider(names_from=group,values_from=n)%>% remove_rownames %>%
  column_to_rownames(var="discomfort")%>%mutate(Infertile=replace_na(Infertile,0))

## Quatiles categorization of Interleukin 10

Inter_10<-Chlamydia_logistic_reg%>%group_by(group)%>%
  mutate(il_10_quatile=ntile(il_10_avrg,3))%>%
  count(il_10_quatile)%>%
  pivot_wider(names_from=group,values_from=n)%>%
  column_to_rownames(var="il_10_quatile")# %>%
  # select(Infertile,Fertile) # alternative to "column_to_rownames"
  #chisq.test()
 # xtabs(~Infertile+Fertile,data=.) # create a matrix table

## Quatiles to categorize Interferon Alpha

Inter_Alpha<-Chlamydia_logistic_reg%>%group_by(group)%>%
  mutate(ifn_gamm_avrg=ntile(ifn_gamm_avrg,3))%>%
  count(ifn_gamm_avrg)%>%
  pivot_wider(names_from=group,values_from=n)%>%
  column_to_rownames(var="ifn_gamm_avrg")#%>%
  # select(Infertile,Fertile) # alternative to "column_to_rownames"
  #chisq.test()
# xtabs(~Infertile+Fertile,data=.) # create a matrix table
 
## Categorization of cHSP-60 (Chlamydia Heat Shock Protein)
  
Chsp_60<-Chlamydia_logistic_reg%>%group_by(group)%>%
  mutate(hsp_60_avrg=ntile(hsp_60_avrg,3))%>%
  count(hsp_60_avrg)%>%
  pivot_wider(names_from=group,values_from=n)%>%
  column_to_rownames(var="hsp_60_avrg")#%>%
  # select(Infertile,Fertile) # alternative to "column_to_rownames"
  #chisq.test()
# xtabs(~Infertile+Fertile,data=.) # create a matrix table

# Chlamydia anitgenemia

Chlamydia_logistic_reg%>%
  group_by(group)%>%
  mutate(new=ifelse(ig_g=="Pos"|ig_m=="Pos","Pos","Neg"))%>%count(new)

summary(Chlamydia_logistic_reg$il_10_avrg)
summary(Chlamydia_logistic_reg$ifn_gamm_avrg)
summary(Chlamydia_logistic_reg$hsp_60_avrg) 

# Initializing for Loggistic regression
newdata<-Chlamydia_logistic_reg[!Chlamydia_logistic_reg$how_long_7=="Nil",]%>%
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
#%>%
 # select(group,age_cat,il_10_quatile,age,il_10_quatile_cat,ifn_gam_quatile_cat,
  #       il_10_avrg,ifn_gamm_avrg, hsp_60_avrg,hsp_60_quatile_cat,
   #      ifn_gam_quatile,hsp_60_quatile,Vaginal_discharge,hsp_60_quatile_cat,
    #     term_cat,Pregnancy,any_preg,fam_type,wom_edu,
     #    Times_mar,Marital_cat,Marital_dur,
      #   ageoflast_cat,vaginal_disc,chlamydia_pos,
       #  discomfort,odour,vaginal_disc,which_one,family_pla,age_of_last,
        # how_long_20)#%>%
 # view()

# Final build up of the Socio-demographic factors surrounding reproductive pathology

Socio_eco<-bind_rows(list(
  age=age_cat,Marital=Marital_Duration_Couples_Categ,
  no_times_mar=no_times_married,
  wom_Edu=woman_Education,
  Husban_Occupation=Husban_Occupation,
  Fam_type=Fam_type,
  Woman_occupation=Woman_occupation,
  Infertility_cat=Infertility_cat,
  History_pregnancy=History_pregnancy,
  To_term=To_term,
  Last_age_of_child=Last_age_of_child,
  Family_plan=Family_plan,
  What_type_of_contraceptive=What_type_of_contraceptive,
  How_long_use_contra=How_long_use_contra,
  Vaginal_disc=Vaginal_disc,
  Vaginal_discharge=Vaginal_discharge,
  Odour=Odour,
  Discomfort=Discomfort,
  Inter_10=Inter_10,
  Inter_Alpha=Inter_Alpha,
  Chsp_60=Chsp_60),.id="Variable")%>%
  rownames_to_column()

# Final sociodemographic data of our study group

Socio_eco%>%mutate(P_Value=recode
                   (Variable,
                     "age"="0.09419",
                     "Marital"="0.0078736",
                     "no_times_mar"="0.0005033",
                     "wom_Edu"="0.009383",
                     "Husban_Occupation"="0.07422",
                     "Fam_type"="0.08756",
                     "Woman_occupation"="<0.0001",
                     "Infertility_cat"="-",
                     "History_pregnancy"="<0.0001",
                     "To_term"="<0.0001",
                     "Last_age_of_child"="=0.001",
                     "Family_plan"="<0.0001",
                     "What_type_of_contraceptive"="0.0378",
                     "How_long_use_contra"="0.02004",
                     "Vaginal_disc"="<0.0001",
                     "Vaginal_discharge"="0.0001",
                     "Odour"="<0.0001","Discomfort"="<0.0001",
                     "Inter_10"="n.s","Inter_Alpha"="n.s","Chsp_60"="n.s"))%>%
 mutate(rowname=str_replace_all(rowname,c("...69"="Quatile1",
                                          "...70"="Quatile2",
                                          "...71"="Quatile3",
                                          "...72"="Quatile_1",
                                          "...73"="Quatile_2",
                                          "...74"="Quatile_3",
                                          "...75"="Quatile.1",
                                          "...76"="Quatile.2",
                                          "...77"="Quatile.3"))
)
  #write.csv(.,"socio_demof.csv")


  #Quick exploratory data analysis
library(DataExplorer)
plot_missing(Chlamydia_logistic_reg) #The missing value exploration plot shows,
#we don’t have missing value of more than 25% for each variable. We don’t have 
#any variable that will be dropped using a complete deletion. In fact, the 
#missing values are tolerable and performing imputation strategies is 
#practically possible.

#Chlamydia_logistic_reg <- Chlamydia_logistic_reg[complete.cases(Chlamydia_logistic_reg), ] 
# This deletes all rows with missing values

plot_histogram(Chlamydia_logistic_reg) # Almost all variables are skewed

logis<-glm(group~marital_s,family=binomial)

logistic_reg<-glm(as.factor(Chlamydia_logistic_reg$group)~Chlamydia_logistic_reg$age+
                    Chlamydia_logistic_reg$marital_status+
                    Chlamydia_logistic_reg$husb_occ,family=binomial)
summary(logistic_reg)

require(MASS)
or_CI <- round(exp(cbind(coef(logistic_reg), confint(logistic_reg))), digits=3) %>% 
  as.data.frame()

or_CI <- or_CI %>% 
  mutate(variable=rownames(or_CI)) # extract the variables from rownames


or_CI <- rename(or_CI, c("AOR" = "V1",
                         "lower_bound"= "2.5 %",
                         "upper_bound"="97.5 %" ))
# Reorder variables
col_order <- c("variable", "AOR", "lower_bound", "upper_bound")
or_CI <- or_CI[, col_order] #reorder variables in the data frame


plot_logit_model <- or_CI[-1,] %>%  #remove row number 1 (The intercept) 
  ggplot(aes(x = reorder(variable, AOR), y = AOR)) +
  geom_point(shape = 15,
             size  = 4, width = 0.1,
             position = "dodge", color="black") + 
  geom_errorbar(aes(ymin  = lower_bound,
                    ymax  = upper_bound),
                width = 0.2,
                size  = 0.7,
                position = "dodge", color="turquoise4") +
  theme(axis.title = element_text(face = "bold")) +
  xlab("Variables") + ylab("Adjusted odds ratios with 95% CI") +
  coord_flip(ylim = c(0, 2.5)) + 
  geom_hline(yintercept = 1, color = "red", size = 1) +
  theme(axis.title = element_text(size = 17)) + 
  theme(axis.text = element_text(size = 14)) 
plot_logit_model

for(i in 1:215){
  x<- Chlamydia_logistic_reg$to_term[i]
  if(x=="one")
    print(0)
} | if(x!="one") {
  print(1)
}

Chlamydia_logistic_reg%>%
  select(age_of_last,to_term)%>%
  mutate(last=ifelse(to_term=="one",1,0))

Chlamydia_logistic_reg%>%
  select(age_of_last,to_term)%>%
  mutate(last=ifelse(to_term %in% c("one","Two","Three"),1,0))%>% 
  tail()

x<-vector(mode = "integer")

for(i in 1:215){
  x<- Chlamydia_logistic_reg$to_term[i]
  if(x=="one")
    print("0")
  x[i]<-x
} | if(x!="one"|x!="four") {
  print("1")
  
}

sn<-vector()
for(i in 1:215){
  x<- Chlamydia_logistic_reg$to_term[i]
  
  if(x=="one")
    print("0")
} | if(x!="one"|x!="Four") {
  print("1")
  
}

for(i in 1:215){
  output<- Chlamydia_logistic_reg$to_term[i]
  if(output=="one"){
    print(0)
  }|if(output=="Three"){
    print(0)
    
  }
  output
}
for(i in 1:215){
  output[i]<- Chlamydia_logistic_reg$to_term[i]
  if(output[i]=="Three"){
    print(0)
  }
  print(output[i])
}



library(finalfit) 
explanatory = c( "age",
                 "vaginal_disc",
                 "Pregnancy",
                 "wom_edu",
                 "odour")
dependent = "group"
newdata %>%
  coefficient_plot(dependent, explanatory, table_text_size=3, 
                   title_text_size=12,
                   plot_opts=list(xlab("Beta, 95% CI"), 
                                  theme(axis.title = element_text(size=12))))


require(MASS)
or_CI <- round(exp(cbind(coef(Model), confint(Model))), digits=3) %>% 
  as.data.frame()
or_CI <- or_CI %>% 
  mutate(variable=rownames(or_CI)) # extract the variables from rownames

or_CI <- rename(or_CI, c("AOR"="V1" ,
                         "lower_bound"=`2.5 %` ,
                         "upper_bound"= `97.5 %`))
# We don't need to plot the intercept. We can remove it from our data
# Reorder variables
col_order <- c("variable", "AOR", "lower_bound", "upper_bound")
or_CI <- or_CI[, col_order] #reorder variables in the data frame

plot_logit_model <- or_CI[-1,] %>%  #remove row number 1 (The intercept) 
  ggplot(aes(x = reorder(variable, AOR), y = AOR)) +
  geom_point(shape = 15,
             size  = 4, width = 0.1,
             position = "dodge", color="black") + 
  geom_errorbar(aes(ymin  = lower_bound,
                    ymax  = upper_bound),
                width = 0.2,
                size  = 0.7,
                position = "dodge", color="turquoise4") +
  theme(axis.title = element_text(face = "bold")) +
  xlab("Variables") + ylab("Adjusted odds ratios with 95% CI") +
  coord_flip(ylim = c(0, 2.5)) + 
  geom_hline(yintercept = 1, color = "red", size = 1) +
  theme(axis.title = element_text(size = 17)) + 
  theme(axis.text = element_text(size = 14)) 
plot_logit_model




library(finalfit) 
explanatory = c( "age",
                 "how_long_20", 
                 "term_cat"
                 )
dependent = "group"
newdata %>%
  or_plot(dependent, explanatory, table_text_size=3, 
                   title_text_size=12,
                   plot_opts=list(xlab("Beta, 95% CI"), 
                                  theme(axis.title = element_text(size=12))))


# first model
Model1<-newdata%>%glm(
  as.factor(group)~ig_m+
    ig_g+
    vaginal_disc+
    odour+
    Marital_cat+
    Times_mar+
    woman_occ1+age_cat+wom_edu+
    how_many,
  data = .,family="binomial")%>%
  summary()

# Logistic regression Plot for model1
explanatory = c( "ig_m",
                   "ig_g",
                   "vaginal_disc",
                   "odour",
                   "Marital_cat",
                   "Times_mar",
                   "woman_occ1","age_cat","wom_edu",
                   "how_many"
)
dependent = "group"
newdata %>%
  or_plot(dependent, explanatory, table_text_size=3, 
          title_text_size=12,
          plot_opts=list(xlab("Beta, 95% CI"), 
                         theme(axis.title = element_text(size=12))))

# Logistic regression Plot for model2 (including Vaginal_discharge and removing Vaginal disc)
explanatory = c( "ig_m",
                 "ig_g",
                 "Vaginal_discharge",
                 "odour",
                 "Marital_cat",
                 "Times_mar",
                 "woman_occ1","age_cat","wom_edu",
                 "how_many"
)
dependent = "group"
newdata %>%
  or_plot(dependent, explanatory, table_text_size=3, 
          title_text_size=12,
          plot_opts=list(xlab("Beta, 95% CI"), 
                         theme(axis.title = element_text(size=12))))

# Socio-demographic risk factors for Infertility type in the study population

newdata[newdata$group=="Infertile",]%>%
  glm(factor(any_preg)~family_pla+
        age+hsp_60_quatile_cat+
        fam_type+odour+
        chlamydia_pos,
      data = .,family="binomial")%>%
  summary()
# model to predict Secondary infertility
explanatory = c( "family_pla",
                   "age",
                   "hsp_60_quatile_cat",
                   "fam_type",
                   "odour",
                   "chlamydia_pos"
)

newdata$any_preg<-factor(newdata$any_preg,levels=c("Primary_infert","secondary_infert"))
dependent = "any_preg"

newdata[newdata$group=="Infertile",] %>%
  or_plot(dependent, explanatory, table_text_size=7, 
          title_text_size=15,
          plot_opts=list(xlab("Beta, 95% CI"), 
                         theme(axis.title = element_text(size=15))))

# Model (sociodemographic) to predict Primary Infertility

# model to predict Secondary infertility
explanatory = c( "family_pla",
                 "age",
                 "hsp_60_quatile_cat",
                 "fam_type",
                 "odour",
                 "chlamydia_pos"
)

newdata$any_preg<-factor(newdata$any_preg,levels=c("secondary_infert","Primary_infert"))
dependent = "any_preg"

newdata[newdata$group=="Infertile",] %>%
  or_plot(dependent, explanatory, table_text_size=4.5, 
          title_text_size=12,
          plot_opts=list(xlab("Beta, 95% CI"), 
                         theme(axis.title = element_text(size=12))))
