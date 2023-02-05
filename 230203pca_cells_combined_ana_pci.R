#######20230203 nanaliu
######pca for cells -1 
##
library(FactoMineR)
library(factoextra)##http://www.sthda.com/english/web/tag/factoextra/#google_vignette
load('230202_Scombined_tussie_cells.rda')
##https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/Principal-Component-Analysis/Principal-component-analysis-in-R/index.html
table(Scombined_tussie_cells$CellType)
Scombined_tussie_neu <- Scombined_tussie_cells[Scombined_tussie_cells$CellType=='Neutrophil (CD11b + Ly6G +)',]

##FactoMineR::MCA for data with all categorical 
famd_Scombined_tussie_neu <- FactoMineR::FAMD(Scombined_tussie_neu[,c(4,6,8)],
                #sup.var = 3,
                graph = F)

fviz_famd_ind(famd_Scombined_tussie_neu, #col.ind = "cos2",
              #color='Sick',shape='Analgesic',
              #gradient.cols = c("blue", "orange", "red"),
              geom = c('point'),
              col.ind = 'Group',
              #shape.ind = 'Tissue',
              palette = 'ucscgb',##jco
              habillage = c('Group'),#,'Tissue'),
              addEllipses = T,
              repel = TRUE,
              ggtheme=theme_classic())+
  labs(title = 'Principal component for neutrophil percentage')+
  theme(legend.position = 'right',
        title = element_text(face = 'bold',size = 18),
        axis.title = element_text(face = 'bold',size = 16),
        axis.text.x = element_blank(),#element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))
ggsave('230203_famd_neutrophil.png',
       units = 'cm',
       dpi = 600,
       width = 36,height = 20)
##,main='Principal component for neutrophil percentage'
##################
table(Scombined_tussie_cells$CellType)
Scombined_tussie_mac <- Scombined_tussie_cells[Scombined_tussie_cells$CellType=='Macrophage (CD11b + Ly6G - F4/80 +)',]

##FactoMineR::MCA for data with all categorical 
famd_Scombined_tussie_mac <- FactoMineR::FAMD(Scombined_tussie_mac[,c(4,6,8)],
                                              #sup.var = 3,
                                              graph = F)

fviz_famd_ind(famd_Scombined_tussie_mac, #col.ind = "cos2",
              #color='Sick',shape='Analgesic',
              #gradient.cols = c("blue", "orange", "red"),
              geom = c('point'),
              palette = 'ucscgb',##jco
              habillage = c('Group'),#,'Tissue'),
              addEllipses = T,
              repel = TRUE,
              ggtheme=theme_classic())+
  labs(title = 'Principal component for macropahe percentage')+
  theme(legend.position = 'right',
        title = element_text(face = 'bold',size = 18),
        axis.title = element_text(face = 'bold',size = 16),
        axis.text.x = element_blank(),#element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))
ggsave('230203_famd_macrophage.png',
       units = 'cm',
       dpi = 600,
       width = 36,height = 20)








