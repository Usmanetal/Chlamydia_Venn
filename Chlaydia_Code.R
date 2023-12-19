library(readxl)
Chlamydia_Venn <- read_excel("C:/Users/Usman Ola/Downloads/Chlamydia Trichomatis.xlsx")
head(Chlamydia_Venn)


#library(BiocManager)
library(limma)
#library(VennDiagram)
library(ggVennDiagram)
library(tidyverse)
Chlamydia_Venn%>%select(IgG,IgM)%>%
  mutate(IgG=recode(IgG,Neg=0,Pos=1), IgM=recode(IgM,Neg=0,Pos=1))%>%
  vennCounts()%>%vennDiagram()
Chlamydia_Venn

Chla_Tris_Venn<-Chlamydia_Venn%>%select(IgG,IgM)%>%
  mutate(IgG=recode(IgG,Neg=0,Pos=1), IgM=recode(IgM,Neg=0,Pos=1))
Chla_Tris_Venn

Chla_Tris_Venn_Inf<-Chlamydia_Venn%>%
  filter(Group=="Infertile")%>%select(IgG,IgM)%>%
  mutate(IgG=recode(IgG,Neg=0,Pos=1), IgM=recode(IgM,Neg=0,Pos=1))
Chla_Tris_Venn_Inf

Chla_Tris_Venn_Fer<-Chlamydia_Venn%>%
  filter(Group=="Fertile")%>%
  select(IgG,IgM)%>%
  mutate(IgG=recode(IgG,Neg=0,Pos=1), IgM=recode(IgM,Neg=0,Pos=1))
Chla_Tris_Venn_Fer

ggVennDiagram(lapply(Chla_Tris_Venn, function(x)which(x==1)))
ggVennDiagram(lapply(Chla_Tris_Venn_Inf, function(x)which(x==1)))
ggVennDiagram(lapply(Chla_Tris_Venn_Fer, function(x)which(x==1)))

