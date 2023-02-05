###############20230201 nanaliu color
library(scales)
hex <- hue_pal()(4)
hex
##4 colors for cell type
####for liver
load('230131_combined_sta_liver_cells.rda')
load('20230131_Ocombined_sta_liver_cells.rda')
combined_sta_liver_cells$Tissue <- 'Liver'
Ocombined_sta_liver_cells$Run <- 3##!!!!!!!!!!!!!!!!!
###3 is a symbol of combination!!!!!!!!!!!!
Ocombined_sta_liver_cells$Tissue <- 'Liver'

##############for spleen
load('230131_combined_sta_spleen_cells.rda')
load('20230131_Ocombined_sta_spleen_cells.rda')
combined_sta_spleen_cells$Tissue <- 'spleen'
Ocombined_sta_spleen_cells$Run <- 3##!!!!!!!!!!!!!!!!!
###3 is a symbol of combination!!!!!!!!!!!!
Ocombined_sta_spleen_cells$Tissue <- 'spleen'


#############for lavage
load('230131_combined_sta_lavage_cells.rda')
load('20230131_Ocombined_sta_lavage_cells.rda')
combined_sta_lavage_cells$Tissue <- 'lavage'
Ocombined_sta_lavage_cells$Run <- 3##!!!!!!!!!!!!!!!!!
###3 is a symbol of combination!!!!!!!!!!!!
Ocombined_sta_lavage_cells$Tissue <- 'lavage'


#############for blood
load('230131_combined_sta_blood_cells.rda')
load('20230131_Ocombined_sta_blood_cells.rda')
combined_sta_blood_cells$Tissue <- 'blood'
Ocombined_sta_blood_cells$Run <- 3##!!!!!!!!!!!!!!!!!
###3 is a symbol of combination!!!!!!!!!!!!
Ocombined_sta_blood_cells$Tissue <- 'blood'

########
combined_sta_tissue_cells <- do.call('rbind',list(combined_sta_liver_cells,combined_sta_spleen_cells,
                     combined_sta_lavage_cells,combined_sta_blood_cells))

save(combined_sta_tissue_cells,file = '230201_combined_sta_tissue_cells.rda')
###4 x 32 = 128
##################
Ocombined_sta_tissue_cells <- do.call('rbind',list(Ocombined_sta_liver_cells,Ocombined_sta_spleen_cells,
                                                  Ocombined_sta_lavage_cells,Ocombined_sta_blood_cells))

save(Ocombined_sta_tissue_cells,file = '230201_Ocombined_sta_tissue_cells.rda')
#####4 x 16 = 64

##############
ggplot(Ocombined_sta_tissue_cells,aes(x=Group, y=mean,fill=CellType))+
  facet_grid(cols = vars(CellType),
             rows=vars(Tissue))+
  geom_bar(alpha=.3,position = position_dodge(),
           stat = 'identity')+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),
                width=.2,
                position = position_dodge(.9))+
  theme_classic()+
  labs(x='',y='Mean biomarkers of cellType in tissue')+
  theme(legend.position = 'none',
        axis.title = element_text(face = 'bold',size = 16),
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))
ggsave('20230201_pAllTissue_Osta_cells.png',
       units = 'cm',
       dpi = 600,
       width = 36,height = 20)

########################
##############
##neutrophil color fill = "#00BFC4" 
##macrophage color fill =  "#C77CFF"
ggplot(Ocombined_sta_tissue_cells[Ocombined_sta_tissue_cells$CellType=='Macrophage (CD11b + Ly6G - F4/80 +)',#'Neutrophil (CD11b + Ly6G +)',
                                  ],
       aes(x=Group, y=mean,fill=CellType))+
  facet_grid(CellType ~ Tissue)+
  #facet_grid(cols = vars(CellType),
  #           rows=vars(Tissue))+
  geom_bar(alpha=.3,position = position_dodge(),
           stat = 'identity', fill =  "#C77CFF"#fill = "#00BFC4" 
           )+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),
                width=.2,
                position = position_dodge(.9))+
  theme_classic()+
  labs(x='',y='Mean biomarkers of cellType in tissue')+
  theme(legend.position = 'none',
        axis.title = element_text(face = 'bold',size = 16),
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))
ggsave('20230201_pAllTissue_Osta_macrophage.png',#'20230201_pAllTissue_Osta_neutrophil.png',
       units = 'cm',
       dpi = 600,
       width = 30,height = 20)

##############
##neutrophil color fill = "#00BFC4" 
##macrophage color fill =  "#C77CFF"
table(combined_sta_tissue_cells$CellType)
ggplot(combined_sta_tissue_cells[combined_sta_tissue_cells$CellType=='Macrophage (CD11b + Ly6G - F4/80 +)',#'Neutrophil (CD11b + Ly6G +)',
                                 ],
       aes(x=Group, y=mean,fill=CellType))+
  facet_grid(Run ~ Tissue)+
  #facet_grid(cols = vars(CellType),
  #           rows=vars(Tissue))+
  geom_bar(alpha=.3,position = position_dodge(),
           stat = 'identity',fill =  "#C77CFF" #fill = "#00BFC4" 
           )+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),
                width=.2,
                position = position_dodge(.9))+
  theme_classic()+
  labs(x='',y='Mean biomarkers of cellType in tissue')+
  theme(legend.position = 'none',
        axis.title = element_text(face = 'bold',size = 16),
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))
ggsave('20230201_pAllTissue_sta_macrophage.png',#'20230201_pAllTissue_sta_neutrophil.png',
       units = 'cm',
       dpi = 600,
       width = 36,height = 20)

##############
ggplot(combined_sta_tissue_cells,
       aes(x=Group, y=mean,fill=CellType))+
  facet_grid(cols = vars(CellType),
             rows=vars(Tissue,Run))+
  geom_bar(alpha=.3,position = position_dodge(),
           stat = 'identity')+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),
                width=.2,
                position = position_dodge(.9))+
  theme_classic()+
  labs(x='',y='Mean biomarkers of cellType in tissue')+
  theme(legend.position = 'none',
        axis.title = element_text(face = 'bold',size = 16),
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))
ggsave('20230201_pAllTissue_sta_cells.png',
       units = 'cm',
       dpi = 600,
       width = 36,height = 20)


##################
Ocombined_sta_tissue_neu_mac <- Ocombined_sta_tissue_cells[Ocombined_sta_tissue_cells$CellType=='Neutrophil (CD11b + Ly6G +)' | 
  Ocombined_sta_tissue_cells$CellType=='Macrophage (CD11b + Ly6G - F4/80 +)',]
