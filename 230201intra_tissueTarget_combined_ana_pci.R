###############20230201 nanaliu color
library(scales)
hex <- hue_pal()(4)
hex
##4 colors for cell type
####for liver
load('230131_combined_sta_liver_targets.rda')
load('20230131_Ocombined_sta_liver_targets.rda')
combined_sta_liver_targets$Tissue <- 'Liver'
Ocombined_sta_liver_targets$Run <- 3##!!!!!!!!!!!!!!!!!
###3 is a symbol of combination!!!!!!!!!!!!
Ocombined_sta_liver_targets$Tissue <- 'Liver'

##############for spleen
load('230131_combined_sta_spleen_targets.rda')
load('20230131_Ocombined_sta_spleen_targets.rda')
combined_sta_spleen_targets$Tissue <- 'Spleen'
Ocombined_sta_spleen_targets$Run <- 3##!!!!!!!!!!!!!!!!!
###3 is a symbol of combination!!!!!!!!!!!!
Ocombined_sta_spleen_targets$Tissue <- 'Spleen'


#############for lavage
load('230131_combined_sta_lavage_targets.rda')
load('20230131_Ocombined_sta_lavage_targets.rda')
combined_sta_lavage_targets$Tissue <- 'Lavage'
Ocombined_sta_lavage_targets$Run <- 3##!!!!!!!!!!!!!!!!!
###3 is a symbol of combination!!!!!!!!!!!!
Ocombined_sta_lavage_targets$Tissue <- 'Lavage'


#############for blood
load('230131_combined_sta_blood_targets.rda')
load('20230131_Ocombined_sta_blood_targets.rda')
combined_sta_blood_targets$Tissue <- 'Blood'
Ocombined_sta_blood_targets$Run <- 3##!!!!!!!!!!!!!!!!!
###3 is a symbol of combination!!!!!!!!!!!!
Ocombined_sta_blood_targets$Tissue <- 'Blood'

########
combined_sta_tissue_targets <- do.call('rbind',list(combined_sta_liver_targets,combined_sta_spleen_targets,
                                                  combined_sta_lavage_targets,combined_sta_blood_targets))

save(combined_sta_tissue_targets,file = '230201_combined_sta_tissue_targets.rda')
###4 x 32 = 128
##################
Ocombined_sta_tissue_targets <- do.call('rbind',list(Ocombined_sta_liver_targets,Ocombined_sta_spleen_targets,
                                                   Ocombined_sta_lavage_targets,Ocombined_sta_blood_targets))

save(Ocombined_sta_tissue_targets,file = '230201_Ocombined_sta_tissue_targets.rda')
#####4 x 16 = 64

##############
ggplot(Ocombined_sta_tissue_targets,aes(x=Group, y=mean,fill=CellType))+
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
##ggsave('20230201_pAllTissue_Osta_targets.png',
       units = 'cm',
       dpi = 600,
       width = 36,height = 20)

########################
############## does not run for visualization!!!!!! 230202
##neutrophil color fill = "#00BFC4" 
##macrophage color fill =  "#C77CFF"
ggplot(Ocombined_sta_tissue_targets[Ocombined_sta_tissue_targets$CellType=='Macrophage'
                                    & Ocombined_sta_tissue_targets$Targets=='CXCR2',#'Neutrophil (CD11b + Ly6G +)',
                                  ],
       aes(x=Group, y=mean,fill=CellType))+
  facet_grid(Targets ~ Tissue)+
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
        axis.text.x = element_blank(),#element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))
##ggsave('20230201_pAllTissue_Osta_macrophage.png',#'20230201_pAllTissue_Osta_neutrophil.png',
       units = 'cm',
       dpi = 600,
       width = 30,height = 20)

##############
##neutrophil color fill = "#00BFC4" 
##macrophage color fill =  "#C77CFF"
table(Scombined_tussie_targets$CellType)
ggplot(Scombined_tussie_targets[Scombined_tussie_targets$CellType=='Macrophage'
                                    & Scombined_tussie_targets$Targets=='CXCR2',#'Neutrophil (CD11b + Ly6G +)',
                                    ],
       aes(x=Group, y=Percent,fill=Run))+
  facet_grid( ~ Tissue)+
  geom_boxplot(fill="#00BFC4",alpha=.3)+
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 8,
               alpha=.8,binpositions = 'all',
               binwidth = .2,stackgroups = T)+
  theme_classic()+
  labs(x='',y='Mean biomarkers of cellType in tissue')+
  theme(legend.position = 'none',
        axis.title = element_text(face = 'bold',size = 16),
        axis.text.x = element_blank(),#element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))
ggsave('20230201_pAllTissue_sta_macrophage.png',#'20230201_pAllTissue_sta_neutrophil.png',
       units = 'cm',
       dpi = 600,
       width = 36,height = 20)

##############
ggplot(combined_sta_tissue_targets,
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
ggsave('20230201_pAllTissue_sta_targets.png',
       units = 'cm',
       dpi = 600,
       width = 36,height = 20)


##################
Ocombined_sta_tissue_neu_mac <- Ocombined_sta_tissue_targets[Ocombined_sta_tissue_targets$CellType=='Neutrophil (CD11b + Ly6G +)' | 
                                                             Ocombined_sta_tissue_targets$CellType=='Macrophage (CD11b + Ly6G - F4/80 +)',]
