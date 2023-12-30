library(MASS)
library(factoextra)
library(ggplot2)



cytokine_response<-newdata[newdata$group=="Infertile",c("il_10_avrg","ifn_gamm_avrg","hsp_60_avrg")]

# Run PCA
cytokine_pca<- prcomp(cytokine_response,scale=TRUE)

# Scree plot of variance

fviz_eig(cytokine_pca,
         addlabels = TRUE,
         ylim=c(0,70),
         main = "Cytokine screePlot"
         )

# Biplot with Default Settings
fviz_pca_biplot(cytokine_pca)

# Biplot with labeled variables
fviz_pca_biplot(cytokine_pca,
                label = "var")

# Biplot with customized colored groups and variables
fviz_pca_biplot(cytokine_pca,
                label = "var",
                habillage = newdata[newdata$group=="Infertile",]$chlamydia_pos)

# Biplot with customized colored groups and variables
fviz_pca_biplot(cytokine_pca,
                 label="var",addEllipses = TRUE,title="Cytokine PCA-Biplot",
                 habillage=newdata[newdata$group=="Infertile",]$chlamydia_pos,
                 col.var="black")+scale_color_manual(values=c("orange","purple"))+
                 labs(x="PCA1 (55.6%)",y="PCA2 (29.1%)")

# Cytokine Response in fertile and infertile women
cytokine_response_g<-newdata[,c("il_10_avrg","ifn_gamm_avrg","hsp_60_avrg")]

# Run PCA
cytokine_pca_g<- prcomp(cytokine_response_g,scale=TRUE)

# Scree plot of variance

fviz_eig(cytokine_pca_g,
         addlabels = TRUE,
         ylim=c(0,70),
         main = "Cytokine screePlot"
)

# Biplot with Default Settings
fviz_pca_biplot(cytokine_pca_g)

# Biplot with labeled variables
fviz_pca_biplot(cytokine_pca_g,
                label = "var")

# Biplot with customized colored groups and variables
fviz_pca_biplot(cytokine_pca_g,
                label = "var",
                habillage = newdata$chlamydia_pos)

# Biplot with customized colored groups and variables
fviz_pca_biplot(cytokine_pca_g,
                label="var",addEllipses = TRUE,title="Cytokine PCA-Biplot",
                habillage=newdata$chlamydia_pos,
                col.var="black")+scale_color_manual(values=c("orange","purple"))+
  labs(x="PCA1 (55.6%)",y="PCA2 (29.1%)")
