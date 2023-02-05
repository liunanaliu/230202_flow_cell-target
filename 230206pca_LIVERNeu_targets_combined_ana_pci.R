##############20230205 nanaliu
######pca for cells -4, combined with target for neutrophils 
##specific for tissue
load('230205_Scombined_neu_targets_wide.rda')
Scombined_LIVERneu_targets_wide<- Scombined_neu_targets_wide[
  Scombined_neu_targets_wide$Tissue=='LIVER',]

pca_Scombined_LIVERneu_targets <- PCA(Scombined_LIVERneu_targets_wide[,c(8:13)],
                                 graph = F)###
###
get_eigenvalue(pca_Scombined_LIVERneu_targets)
fviz_eig(pca_Scombined_LIVERneu_targets,addlabels = T,
         ylim=c(0,60))
var <- get_pca_var(pca_Scombined_LIVERneu_targets)
head(var$coord)
#####
fviz_pca_ind(pca_Scombined_LIVERneu_targets, #col.ind = "cos2",
             #color='Sick',shape='Analgesic',
             #gradient.cols = c("blue", "orange", "red"),
             geom.ind = c('point'),
             col.ind = Scombined_LIVERneu_targets_wide$Group,
             #shape.ind = 'Tissue',
             palette = 'ucscgb',##jco
             #habillage = c('Group'),#,'Tissue'),
             addEllipses = T,
             repel = TRUE,
             legend.title='Groups',
             ggtheme=theme_classic())+
  labs(title = 'Principal component for neutrophil targets')+
  theme(legend.position = 'right',
        title = element_text(face = 'bold',size = 18),
        axis.title = element_text(face = 'bold',size = 16),
        axis.text.x = element_blank(),#element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))

fviz_pca_biplot(pca_Scombined_LIVERneu_targets,
                col.ind = Scombined_LIVERneu_targets_wide$Group,
                #shape.ind = 'Tissue',
                palette = 'ucscgb',
                geom.ind = c('point'),
                addEllipses = T,
                repel = TRUE,
                legend.title='Groups',
                ggtheme=theme_classic())+
  labs(title = 'Principal component for neutrophil targets')+
  theme(legend.position = 'right',
        title = element_text(face = 'bold',size = 18),
        axis.title = element_text(face = 'bold',size = 16),
        axis.text.x = element_blank(),#element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))












