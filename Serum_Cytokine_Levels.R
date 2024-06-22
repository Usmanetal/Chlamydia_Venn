
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

ggplot(newdata,aes(x=chlamydia_pos,y=il_10_avrg,color=group))+
  geom_boxplot()+scale_color_manual(name="",values = c("blue","black"))+
  geom_point(position=position_jitterdodge())+
  stat_pvalue_manual(pwc, tip.length = 0.02,hide.ns = FALSE,
  y.position = c(500,620),bracket.size = 1,size=7,
  label = "p={format.pval(pv=p.adj,digits = 2)}",color="darkblue")+
  geom_line(data = tibble(x=c(1.2,2.18),y=c(600,600)),aes(x=x,y=y),
  inherit.aes = FALSE,size=1,color="darkblue")+
  geom_line(data = tibble(x=c(1.2,1.2),y=c(600,585)),aes(x=x,y=y),
  inherit.aes = FALSE,size=1,color="darkblue")+
  geom_line(data = tibble(x=c(0.81,1.8),y=c(400,400)),aes(x=x,y=y),
  inherit.aes = FALSE,size=1,color="darkblue")+
  geom_line(data = tibble(x=c(0.81,0.81),y=c(400,385)),aes(x=x,y=y),
  inherit.aes = FALSE,size=1,color="darkblue")+
  geom_line(data = tibble(x=c(1.8,1.8),y=c(400,385)),aes(x=x,y=y),
  inherit.aes = FALSE,size=1,color="darkblue")+
  geom_line(data = tibble(x=c(2.18,2.18),y=c(600,585)),aes(x=x,y=y),
            inherit.aes = FALSE,size=1,color="darkblue")

Il_10<-ggplot(newdata,aes(x=chlamydia_pos,y=il_10_avrg,col=group))+
  geom_boxplot()+scale_color_manual(name="",values = c("blue","black"))+
  geom_point(position=position_jitterdodge())+
  stat_pvalue_manual(pwc, tip.length = 0.02,hide.ns = FALSE,
  y.position = c(500,620),bracket.size = 1,size=7,
  label = "{format.pval(pv=p.adj,digits = 2)}",color="darkblue")+
  stat_pvalue_manual(pwc1,y.position = c(650,585), 
  tip.length = 0.02,hide.ns = FALSE,,bracket.size = 1,size=7, 
    label = "p.adj.signif",color="darkblue")+
 # label = "p={format.pval(pv=p.adj,digits = 2)}",color="darkblue")+
  scale_y_continuous(expand=expansion(mult=c(0.05,0.1)))+theme_minimal()+
  theme(legend.position = "null",
    axis.title=element_text(colour="darkblue",        
    size=15,face = "bold"),axis.title.y=element_text(colour="darkblue"),
    axis.text.x = element_text(size=10,face = "bold"),
    axis.ticks = element_line(size = 0.1),axis.line = element_line(size=1))+
  scale_x_discrete(label=c("Ct-negative","Ct-positive"))+
  labs(y = "Interleukin-10 (pg/mL)",x=expression(bold(Serum~italic(Ct)~"-immoglubulin response")))

  pwc <- newdata %>%
  group_by(chlamydia_pos)%>%
  dunn_test(il_10_avrg~group,p.adjust.method = "none")
pwc <- pwc %>% add_xy_position(x="chlamydia_pos")

pwc1 <- newdata %>%
  group_by(group)%>%
  dunn_test(il_10_avrg~chlamydia_pos,p.adjust.method = "none")
pwc1 <- pwc1 %>% add_xy_position(x="chlamydia_pos",group = "group")

# Interferon gamma levels in Ct-immunoglobulin Positive or Negative participants
pwc_ifn <- newdata %>%
  group_by(chlamydia_pos)%>%
  dunn_test(ifn_gamm_avrg~group,p.adjust.method = "none")
pwc_ifn <- pwc %>% add_xy_position(x="chlamydia_pos")

pwc_ifn1 <- newdata %>%
  group_by(group)%>%
  dunn_test(ifn_gamm_avrg~chlamydia_pos,p.adjust.method = "none")
pwc_ifn1 <- pwc_ifn1 %>% add_xy_position(x="chlamydia_pos",group = "group")

Ifn_gam<-ggplot(newdata,aes(x=chlamydia_pos,y=ifn_gamm_avrg,col=group))+
  geom_boxplot()+scale_color_manual(name="",values = c("blue","black"))+
  geom_point(position=position_jitterdodge())+
  stat_pvalue_manual(pwc_ifn, tip.length = 0.02,hide.ns = FALSE,
  y.position = c(350,350),bracket.size = 1,size=7,
  label = "p.adj.signif",color="darkblue")+
  stat_pvalue_manual(pwc_ifn1,y.position = c(400,385), tip.length = 0.02,
  hide.ns = FALSE,bracket.size = 1,size=7, label = "p.adj.signif",color="darkblue")+
  scale_y_continuous(expand=expansion(mult=c(0.05,0.1)))+theme_minimal()+
  theme(legend.position = "null",
    axis.title=element_text(colour="darkblue",        
   size=15,face = "bold"),axis.title.y=element_text(colour="darkblue"),
   axis.text.x = element_text(size=10,face = "bold"),
   axis.ticks = element_line(size = 0.1),axis.line = element_line(size=1))+
  scale_x_discrete(label=c("Ct-negative","Ct-positive"))+
  labs(y = expression(bold(Interferon-gamma~(pg/mL))),x=expression(bold(Serum~italic(Ct)~"-immoglubulin response")))


# Chlamydia Heat Shock Protein (cHSP60)

pwc_Hsp60 <- newdata %>%
  group_by(chlamydia_pos)%>%
  dunn_test(hsp_60_avrg~group,p.adjust.method = "none")
pwc_Hsp60 <- pwc_Hsp60 %>% add_xy_position(x="chlamydia_pos")

pwc_Hsp601 <- newdata %>%
  group_by(group)%>%
  dunn_test(hsp_60_avrg~chlamydia_pos,p.adjust.method = "none")
pwc_Hsp601 <- pwc_Hsp601 %>% add_xy_position(x="chlamydia_pos",group = "group")

hsp_60<-ggplot(newdata,aes(x=chlamydia_pos,y=hsp_60_avrg,col=group))+
  geom_boxplot()+scale_color_manual(name="",values = c("blue","black"))+
  geom_point(position=position_jitterdodge())+
  stat_pvalue_manual(pwc_Hsp60, tip.length = 0.02,hide.ns = FALSE,
                     y.position = c(450,450),bracket.size = 1,size=7,
                     label = "p.adj.signif",color="darkblue")+
  stat_pvalue_manual(pwc_Hsp601,y.position = c(560,515), tip.length = 0.02,
                     hide.ns = FALSE,bracket.size = 1,size=7, label = "p.adj.signif",color="darkblue")+
  scale_y_continuous(expand=expansion(mult=c(0.05,0.1)))+theme_minimal()+
  theme(legend.position = "null",
        axis.title=element_text(colour="darkblue",        
                                size=15,face = "bold"),axis.title.y=element_text(colour="darkblue"),
        axis.text.x = element_text(size=10,face = "bold"),
        axis.ticks = element_line(size = 0.1),axis.line = element_line(size=1))+
  scale_x_discrete(label=c("Ct-negative","Ct-positive"))+
  labs(y = expression(bold(italic(Ct)-heat~shock~protein~(pg/mL))),x=expression(bold(Serum~italic(Ct)~"-immoglubulin response")))

# Th1/Th2 expression Ifn-gamm/In-10
newdata<-newdata%>%mutate(ratio=ifn_gamm_avrg/il_10_avrg)
pwc_th1_2 <- newdata %>%
  group_by(chlamydia_pos)%>%
  dunn_test(ratio~group,p.adjust.method = "none")
pwc_th1_2 <- pwc_th1_2 %>% add_xy_position(x="chlamydia_pos")

pwc_th1_21 <- newdata %>%
  group_by(group)%>%
  dunn_test(ratio~chlamydia_pos,p.adjust.method = "none")
pwc_th1_21 <- pwc_th1_21 %>% add_xy_position(x="chlamydia_pos",group = "group")

Il_Ifn<-ggplot(newdata,aes(x=chlamydia_pos,y=ratio,col=group))+
  geom_boxplot()+scale_color_manual(name="",values = c("blue","black"))+
  geom_point(position=position_jitterdodge())+
  stat_pvalue_manual(pwc_th1_2, tip.length = 0.02,hide.ns = FALSE,
                     y.position = c(1.4,1.25),bracket.size = 1,size=7,
                     label = "p.adj.signif",color="darkblue")+
  stat_pvalue_manual(pwc_th1_21,y.position = c(1.7,1.5), tip.length = 0.02,
                     hide.ns = FALSE,bracket.size = 1,size=7, label = "p.adj.signif",color="darkblue")+
  scale_y_continuous(expand=expansion(mult=c(0.05,0.1)))+theme_minimal()+
  theme(legend.position = "bottom",legend.direction = "horizontal",
        axis.title=element_text(colour="darkblue",        
                                size=15,face = "bold"),axis.title.y=element_text(colour="darkblue"),
        axis.text.x = element_text(size=10,face = "bold"),
        axis.ticks = element_line(size = 0.1),axis.line = element_line(size=1))+
  scale_x_discrete(label=c("Ct-negative","Ct-positive"))+
  labs(y = expression(bold(Th1/Th2)),x=expression(bold(Serum~italic(Ct)~"-immoglubulin response")))


figure<-ggarrange(Il_10,Ifn_gam,hsp_60,Il_Ifn,labels = c("A","B","C","D")
                  ,common.legend = TRUE,legend = "bottom")
figure
