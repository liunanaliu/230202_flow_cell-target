##############20230205 nanaliu
######pca for cells -2, combined with target for neutrophils 
##
library(FactoMineR)
library(factoextra)##http://www.sthda.com/english/web/tag/factoextra/#google_vignette
load('230202_Scombined_tussie_cells.rda')
##https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/Principal-Component-Analysis/Principal-component-analysis-in-R/index.html
table(Scombined_tussie_cells$CellType)
Scombined_tussie_neu <- Scombined_tussie_cells[
  Scombined_tussie_cells$CellType=='Neutrophil (CD11b + Ly6G +)',]
names(Scombined_tussie_neu)
Scombined_tussie_neu$Tissue <- toupper(Scombined_tussie_neu$Tissue)
Scombined_tussie_neu$Targets <- 'None'

#################
load('230202_Scombined_tussie_targets.rda')
Scombined_tussie_neu_targets <- Scombined_tussie_targets[
  Scombined_tussie_targets$CellType=='Neutrophil',]
table(Scombined_tussie_neu_targets$CellType)

Scombined_tussie_neu_targets$Tissue <- toupper(Scombined_tussie_neu_targets$Tissue)
Scombined_tussie_neu_targets$Cell_Targets <- NULL
names(Scombined_tussie_neu_targets)
#############

##########
Scombined_neu_targets <- rbind(Scombined_tussie_neu,Scombined_tussie_neu_targets)
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
names(Scombined_neu_targets)
library(tidyverse)
Scombined_neu_targets_wide <- Scombined_neu_targets
Scombined_neu_targets_wide$CellType <- 'Neutrophil'
table(Scombined_neu_targets_wide$CellType)
table(Scombined_neu_targets_wide$Tissue)

Scombined_neu_targets_wide$Animal <- str_sub(Scombined_neu_targets$Animal,
        str_length(Scombined_neu_targets$Animal)-4,
        str_length(Scombined_neu_targets$Animal))

Scombined_neu_targets_wide <- spread(Scombined_neu_targets_wide,
                                     key = 'Targets',
                                     value = 'Percent')
save(Scombined_neu_targets_wide,file = '230205_Scombined_neu_targets_wide.rda')
#select numerbic rows for pca
names(Scombined_neu_targets_wide)
pca_Scombined_neu_targets <- PCA(Scombined_neu_targets_wide[,c(8:13)],
    graph = F)###
###
get_eigenvalue(pca_Scombined_neu_targets)
fviz_eig(pca_Scombined_neu_targets,addlabels = T,
         ylim=c(0,60))
var <- get_pca_var(pca_Scombined_neu_targets)
head(var$coord)
#####
fviz_pca_ind(pca_Scombined_neu_targets, #col.ind = "cos2",
              #color='Sick',shape='Analgesic',
              #gradient.cols = c("blue", "orange", "red"),
              geom.ind = c('point'),
              col.ind = Scombined_neu_targets_wide$Group,
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

fviz_pca_biplot(pca_Scombined_neu_targets,
                col.ind = Scombined_neu_targets_wide$Group,
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

ggsave('230205_pca_neu_targets.png',
       units = 'cm',
       dpi = 600,
       width = 36,height = 20)




###############for run2
#select numerbic rows for pca
table(Scombined_neu_targets_wide$Run)
Srun2_neu_targets_wide <- Scombined_neu_targets_wide[
  Scombined_neu_targets_wide$Run==2,]
pca_Srun2_neu_targets <- PCA(Srun2_neu_targets_wide[,c(8:13)],
                                 graph = F)###
###
get_eigenvalue(pca_Srun2_neu_targets)
fviz_eig(pca_Srun2_neu_targets,addlabels = T,
         ylim=c(0,60))
var <- get_pca_var(pca_Srun2_neu_targets)
head(var$coord)
#####
fviz_pca_ind(pca_Srun2_neu_targets, #col.ind = "cos2",
             #color='Sick',shape='Analgesic',
             #gradient.cols = c("blue", "orange", "red"),
             geom.ind = c('point'),
             col.ind = Srun2_neu_targets_wide$Group,
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

fviz_pca_biplot(pca_Srun2_neu_targets,
                col.ind = c(Srun2_neu_targets_wide$Group),
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

ggsave('230205_pcaRun2_neu_targets.png',
       units = 'cm',
       dpi = 600,
       width = 36,height = 20)
