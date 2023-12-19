
library(rstatix)
pwc <- newdata %>%
  dunn_test(il_10_avrg~group,p.adjust.method = "none")
pwc <- pwc %>% add_y_position()
pwc
ggplot(data=newdata,aes(x=any_preg,y=hsp_60_avrg))+
  geom_violin(trim = FALSE,col="blue",size=1.5)+
  facet_wrap(~group,nrow=1)

Interleukin_10<-ggplot(data=newdata,aes(x=any_preg,y=il_10_avrg))+
  geom_violin(trim = FALSE,col="blue",size=1.5)+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.4)+
  stat_pvalue_manual(pwc, tip.length = 0.02,hide.ns = TRUE,
  bracket.size = 1,size=7,label = "p.adj.signif",color="darkblue" )

Interferon_alpha<-ggplot(data=newdata,aes(x=any_preg,y=ifn_gamm_avrg))+
  geom_violin(trim = FALSE,col="blue",size=1.5)+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.4)+
  stat_pvalue_manual(pwc, tip.length = 0.02,hide.ns = TRUE,
                     bracket.size = 1,size=7,label = "p.adj.signif",color="darkblue" )

Heat_shock_Prot<-ggplot(data=newdata,aes(x=any_preg,y=hsp_60_avrg))+
  geom_violin(trim = FALSE,col="blue",size=1.5)+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.4)+
  stat_pvalue_manual(pwc, tip.length = 0.02,hide.ns = TRUE,
                     bracket.size = 1,size=7,label = "p.adj.signif",color="darkblue" )

theme(legend.position = "top",
      axis.title.x=element_text(colour="darkblue", size=11.4,face = "bold"),
      axis.title.y=element_text(colour="darkblue",size=17,face = "bold"),
      axis.line = element_line(colour="black",size = 1),
      axis.text.x = element_text(size=13,face = "bold"),axis.ticks = element_line(size = 1))+
  labs(x = "HIV RNA Category (Viral non-Suppression= >1000 cp/mL, Viral Supression= <1000 cp/mL)",
       y="")+
  theme(strip.text=element_text(size = 13.7,face = "bold"))

ggarrange(Interleukin_10,Interferon_alpha,Heat_shock_Prot,nrow = 1)
ggar


# Analysis for Interon-gamma using the Infertility data subsetted by Primary or
# Secondary Infertility

pwc <- newdata3[newdata3$group=="Infertile",] %>%
  group_by(ifn_gam_quatile_cat)%>%
  pairwise_wilcox_test(ifn_gamm_avrg~any_preg,p.adjust.method = "none")
pwc <- pwc %>% add_y_position()
pwc

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