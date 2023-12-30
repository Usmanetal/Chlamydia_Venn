
library(rstatix)
pwc <- newdata %>%group_by(chlamydia_pos)%>%
  dunn_test(il_10_avrg~group,p.adjust.method = "none")
pwc <- pwc %>% add_y_position()
pwc
ggplot(data=newdata,aes(x=any_preg,y=hsp_60_avrg))+
  geom_violin(trim = FALSE,col="blue",size=1.5)+
  facet_wrap(~group,nrow=1)

Interleukin_10<-ggplot(data=newdata,aes(x=group,y=il_10_avrg))+
  geom_violin(trim = FALSE,col="blue",size=1.5)+
  geom_boxplot(width=0.1)+
  # geom_dotplot(binaxis='y', stackdir='center', dotsize=0.4)+
  stat_pvalue_manual(pwc, tip.length = 0.02,hide.ns = FALSE,
  bracket.size = 1,size=7,label = "p.adj.signif",color="darkblue")+facet_wrap(~chlamydia_pos)

pwc1 <- newdata %>%group_by(chlamydia_pos)%>%
  dunn_test(ifn_gamm_avrg~group,p.adjust.method = "none")
pwc1 <- pwc1 %>% add_y_position()
pwc1
Interferon_alpha<-ggplot(data=newdata,aes(x=group,y=ifn_gamm_avrg))+
  geom_violin(trim = FALSE,col="blue",size=1.5)+
  geom_boxplot(width=0.1)+facet_wrap(~chlamydia_pos)+
  # geom_dotplot(binaxis='y', stackdir='center', dotsize=0.4)+
  stat_pvalue_manual(pwc1, tip.length = 0.02,hide.ns = FALSE,
                     bracket.size = 1,size=7,label = "p.adj.signif",color="darkblue" )

pwc2 <- newdata %>%group_by(chlamydia_pos)%>%
  dunn_test(hsp_60_avrg~group,p.adjust.method = "none")
pwc2 <- pwc2 %>% add_y_position()
pwc2
Heat_shock_Prot<-newdata%>%group_by(group)%>%ggplot(aes(x=group,y=hsp_60_avrg))+
  geom_violin(trim = FALSE,col="blue",size=1.5)+
  geom_boxplot(width=0.1)+facet_wrap(~chlamydia_pos)+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=0.4)+
  stat_pvalue_manual(pwc2, tip.length = 0.02,hide.ns = TRUE,
                     bracket.size = 1,size=7,label = "p.adj.signif",color="darkblue" )

# theme(legend.position = "top",
 #     axis.title.x=element_text(colour="darkblue", size=11.4,face = "bold"),
  #    axis.title.y=element_text(colour="darkblue",size=17,face = "bold"),
   #   axis.line = element_line(colour="black",size = 1),
   #   axis.text.x = element_text(size=13,face = "bold"),axis.ticks = element_line(size = 1))+
  #labs(x = "HIV RNA Category (Viral non-Suppression= >1000 cp/mL, Viral Supression= <1000 cp/mL)",
   #    y="")+
  # theme(strip.text=element_text(size = 13.7,face = "bold"))

# Cytokine Levels of Fertile and Infertile women 
ggarrange(Interleukin_10,Interferon_alpha,Heat_shock_Prot,nrow = 1)



# Analysis for Interferon-gamma using the Infertility data subset by Primary or
# Secondary Infertility

pwc3 <- newdata[newdata$group=="Infertile",] %>%
  group_by(chlamydia_pos)%>%
  pairwise_wilcox_test(il_10_avrg~any_preg,p.adjust.method = "none")
pwc3 <- pwc %>% add_y_position()
pwc3
pwc3 <- newdata[newdata$group=="Infertile",] %>%
  group_by(chlamydia_pos)%>%
  dunn_test(il_10_avrg~any_preg,p.adjust.method = "none")
pwc3 <- pwc3 %>% add_y_position()
pwc3
Interleukin<-newdata[newdata$group=="Infertile",]%>%
  ggplot(aes(x=any_preg,y=il_10_avrg))+
  geom_violin(trim = FALSE,col="blue",size=1.5)+
  geom_boxplot(width=0.1)+facet_wrap(~chlamydia_pos,nrow=1)
  # geom_dotplot(binaxis='y', stackdir='center', dotsize=0.4)+
  stat_pvalue_manual(pwc3, tip.length = 0.02,hide.ns = FALSE,
                     bracket.size = 1,size=7,label = "p.adj.signif",color="darkblue" )

  pwc5 <- newdata[newdata$group=="Infertile",] %>%
    group_by(chlamydia_pos)%>%
    dunn_test(ifn_gamm_avrg~any_preg,p.adjust.method = "none")
  pwc5 <- pwc5 %>% add_y_position()
  pwc5
  Interferon<-newdata[newdata$group=="Infertile",]%>%
    ggplot(aes(x=any_preg,y=ifn_gamm_avrg))+
    geom_violin(trim = FALSE,col="blue",size=1.5)+
    geom_boxplot(width=0.1)+facet_wrap(~chlamydia_pos,nrow=1)
  # geom_dotplot(binaxis='y', stackdir='center', dotsize=0.4)+
  stat_pvalue_manual(pwc5, tip.length = 0.02,hide.ns = FALSE,
                     bracket.size = 1,size=7,label = "p.adj.signif",color="darkblue" )

  pwc6 <- newdata[newdata$group=="Infertile",] %>%
    group_by(chlamydia_pos)%>%
    dunn_test(hsp_60_avrg~any_preg,p.adjust.method = "none")
  pwc6 <- pwc6 %>% add_y_position()
  pwc6
  Hsp<-newdata[newdata$group=="Infertile",]%>%
    ggplot(aes(x=any_preg,y=hsp_60_avrg))+
    geom_violin(trim = FALSE,col="blue",size=1.5)+
    geom_boxplot(width=0.1)+facet_wrap(~chlamydia_pos,nrow=1)
  # geom_dotplot(binaxis='y', stackdir='center', dotsize=0.4)+
  stat_pvalue_manual(pwc6, tip.length = 0.02,hide.ns = FALSE,
                     bracket.size = 1,size=7,label = "p.adj.signif",color="darkblue" )
 
  ggarrange(Interleukin,Interferon,Hsp,nrow = 1)
  
# Analysis for Interferon-gamma using the Infertility data subset by Primary or
# Secondary Infertility

pwc1 <- newdata %>%
  dunn_test(ifn_gamm_avrg~group,p.adjust.method = "none")
pwc1 <- pwc1 %>% add_y_position()
Interferon_alpha<-ggplot(data=newdata,aes(x=group,y=ifn_gamm_avrg))+
  geom_violin(trim = FALSE,col="blue",size=1.5)+
  geom_boxplot(width=0.1)+
  # geom_dotplot(binaxis='y', stackdir='center', dotsize=0.4)+
  stat_pvalue_manual(pwc1, tip.length = 0.02,hide.ns = FALSE,
                     bracket.size = 1,size=7,label = "p.adj.signif",color="darkblue" )

pwc2 <- newdata %>%
  dunn_test(hsp_60_avrg~group,p.adjust.method = "none")
pwc2 <- pwc2 %>% add_y_position()

Heat_shock_Prot<-ggplot(data=newdata,aes(x=group,y=hsp_60_avrg))+
  geom_violin(trim = FALSE,col="blue",size=1.5)+
  geom_boxplot(width=0.1)+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=0.4)+
  stat_pvalue_manual(pwc2, tip.length = 0.02,hide.ns = TRUE,
                     bracket.size = 1,size=7,label = "p.adj.signif",color="darkblue" )

newdata3[newdata3$group=="Infertile",]%>%
  ggplot(aes(x=any_preg,y=ifn_gamm_avrg))+facet_wrap(~ifn_gam_quatile_cat,scales = "free")+
  geom_violin(trim = FALSE,col="blue",size=1.5)+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.4)+
  stat_pvalue_manual(pwc, tip.length = 0.02,hide.ns = TRUE,
  bracket.size = 1,size=7,label = "p.adj.signif",color="darkblue" )+
  stat_pvalue_manual(pwc, tip.length = 0.02,
  hide.ns = TRUE,bracket.size = 1,size=7,label = "p.adj.signif",
  color="darkblue" )+
  labs(x=element_blank(),y="Interferon_gamma")+
  theme(axis.title.y=element_text("Interferon-gamma",colour="darkblue",
  size=10,face = "bold"),axis.line = element_line(colour="black",size = 1),
  axis.ticks = element_line(size = 1),
  axis.text.x = element_text(angle = 45,face="bold",vjust = 1,hjust = 1))

# Distribution of hsp_60_avrg variable
ggdensity(Chlamydia_logistic_reg, x = "hsp_60_avrg", fill = "lightgray", title = "Heat_Shock_Protein") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

# Distribution of il_10_avrg variable
ggdensity(Chlamydia_logistic_reg, x = "il_10_avrg", fill = "lightgray", title = "interleukin_10") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
