###########230202 nanaliu
#####add sample dot for each run intra_tissuetarget_combined
####
library(tidyverse)
getwd()##"/media/l33/My Passport/230124CytoFLex/20230131_analgesic_sepsis_combined_run1+2"
load('230201_Ocombined_sta_tissue_targets.rda')

###get single sample from each tissue
load('20230131_combined_liver_targets.rda')
combined_liver_targets$Tissue <- 'Liver'

load('20230131_combined_spleen_targets.rda')
combined_spleen_targets$Tissue <- 'Spleen'

load('20230131_combined_lavage_targets.rda')
combined_lavage_targets$Tissue <- 'Lavage'

load('20230131_combined_blood_targets.rda')
combined_blood_targets$Tissue <- 'Blood'

Scombined_tussie_targets <- do.call('rbind',list(combined_liver_targets,combined_spleen_targets,
                                               combined_lavage_targets,combined_blood_targets))
####not necessary
Scombined_tussie_targets$Animal <- str_sub(Scombined_tussie_targets$Animal,
                                         str_length(Scombined_tussie_targets$Animal)-4,
                                         str_length(Scombined_tussie_targets$Animal))
##
Scombined_tussie_targets$Run <- factor(Scombined_tussie_targets$Run)
save(Scombined_tussie_targets,file = '230202_Scombined_tussie_targets.rda')

####
##############
Ocombined_sta_tissue_targets$Targets
Ocombined_sta_tissue_targets$CellType

###########do not run!!!!!!!!!!!!!!
ggplot(Ocombined_sta_tissue_targets[Ocombined_sta_tissue_targets$CellType=='Neutrophil (CD11b + Ly6G +)',],
       aes(x=Group, y=mean,fill=CellType))+
  facet_grid(CellType ~ Tissue)+
  geom_bar(alpha=.3,position = position_dodge(),
           stat = 'identity', fill = "#00BFC4" )+
  geom_dotplot(binaxis = 'y',stackdir = 'center')+
  geom_errorbar(aes(ymin=mean,ymax=mean+sd),
                width=.2,
                position = position_dodge(.9))+
  theme_classic()+
  labs(x='',y='MFI of targetType biomarkes in tissue')+
  theme(legend.position = 'none',
        axis.title = element_text(face = 'bold',size = 16),
        axis.text.x = element_blank(),#element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))
#################################################################

#######
table(Scombined_tussie_targets$Tissue)

##########do not run!!!!!!!!!!!
ggplot(Scombined_tussie_targets[Scombined_tussie_targets$CellType=='Neutrophil (CD11b + Ly6G +)',],
       aes(x=Group, y=Percent,fill=Run))+
  facet_grid( ~ Tissue)+
  geom_boxplot(fill="#00BFC4",alpha=.3)+
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 8,
               alpha=.8,binpositions = 'all',
               binwidth = .2,stackgroups = T)+
  #geom_errorbar(aes(ymin=mean,ymax=mean+sd),
  #              width=.2,
  #              position = position_dodge(.9))+
  theme_classic()+
  labs(x='',y='MFI of biomarkes in tissue')+
  theme(legend.position = 'bottom',
        axis.title = element_text(face = 'bold',size = 16),
        axis.text.x = element_blank(),#element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
#######
ggsave('20230202_pAllTissue_Sdot_neutrophil.png',
       units = 'cm',
       dpi = 600,
       width = 36,height = 20)
####################
#do not run!!!!!!!!!!!!!!
ggplot(Scombined_tussie_targets[Scombined_tussie_targets$targetType=='Macrophage (CD11b + Ly6G - F4/80 +)',],
       aes(x=Group, y=Percent,fill=Run))+
  facet_grid(targetType ~ Tissue)+
  geom_boxplot(fill="#C77CFF",alpha=.3)+
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 8,
               alpha=.8,binpositions = 'all',
               binwidth = .2,stackgroups = T)+
  #geom_errorbar(aes(ymin=mean,ymax=mean+sd),
  #              width=.2,
  #              position = position_dodge(.9))+
  theme_classic()+
  labs(x='',y='MFI of biomarkes in tissue')+
  theme(legend.position = 'bottom',
        axis.title = element_text(face = 'bold',size = 16),
        axis.text.x = element_blank(),#element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
#######
str(Scombined_tussie_targets$Run)
load('230202_Scombined_tussie_targets.rda')
library(ggplot2)
neu_il10 <- Scombined_tussie_targets[Scombined_tussie_targets$CellType=='Neutrophil'
                                     & Scombined_tussie_targets$Targets=='IL-10',#'Neutrophil (CD11b + Ly6G +)',
                                     ]
ggplot(Scombined_tussie_targets[Scombined_tussie_targets$CellType=='Neutrophil'
                                & Scombined_tussie_targets$Targets=='IL-10',#'Neutrophil (CD11b + Ly6G +)',
                                ],
       aes(x=Group, y=Percent,fill=Run))+
  facet_grid(Targets ~ Tissue)+
  geom_boxplot(fill="#00BFC4",alpha=.3)+
  geom_dotplot(binaxis = 'y',stackdir = 'center',
               dotsize = .8,alpha=.8,binpositions = 'all',
               #binwidth = 1.6,method = 'histodot',
               stackgroups = T
               )+
  theme_classic()+
  labs(x='',y='MFI biomarkers of cellType in tissue')+
  theme(legend.position = 'bottom',
        axis.title = element_text(face = 'bold',size = 16),
        axis.text.x = element_blank(),#element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

ggsave('20230202_pAllTissue_Sdot_neuIL10.png',
       units = 'cm',
       dpi = 600,
       width = 36,height = 20)
######################
##"C77CFF" macrophage 
### "#00BFC4" neutrophil
ggplot(Scombined_tussie_targets[Scombined_tussie_targets$CellType=='Neutrophil'
                                & Scombined_tussie_targets$Targets=='IL-1 beta',#'Neutrophil (CD11b + Ly6G +)',
                                ],
       aes(x=Group, y=Percent,fill=Run))+
  facet_grid(Targets ~ Tissue)+
  geom_boxplot(fill="#00BFC4",alpha=.3)+###change fill color for cell type
  geom_point(aes(color=Run),alpha=.8,size=4)+
  #geom_jitter()+
  #geom_dotplot(binaxis = 'y',stackdir = 'center',
  #             dotsize = 10,#size=1.5,
  #             alpha=.8,#binpositions = 'all',
  #             binwidth = 1,#method = 'histodot',
   #           stackgroups = T
  #)+
  theme_classic()+
  labs(x='',y='MFI biomarkers of cellType in tissue')+
  theme(legend.position = 'bottom',
        axis.title = element_text(face = 'bold',size = 16),
        axis.text.x = element_blank(),#element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))

ggsave('20230202_pAllTissue_Sdot_neuCXCR2.png',
       units = 'cm',
       dpi = 600,
       width = 36,height = 20)
ggsave('20230202_pAllTissue_Sdot_neuIL1beta.png',
       units = 'cm',
       dpi = 600,
       width = 36,height = 20)
ggsave('20230202_pAllTissue_Sdot_macCXCR2.png',
       units = 'cm',
       dpi = 600,
       width = 36,height = 20)
ggsave('20230202_pAllTissue_Sdot_macIL10.png',
       units = 'cm',
       dpi = 600,
       width = 36,height = 20)
ggsave('20230202_pAllTissue_Sdot_macIL1beta.png',
       units = 'cm',
       dpi = 600,
       width = 36,height = 20)
ggsave('20230202_pAllTissue_Sdot_macIFNgamma.png',
       units = 'cm',
       dpi = 600,
       width = 36,height = 20)
ggsave('20230202_pAllTissue_Sdot_macTNFalpha.png',
       units = 'cm',
       dpi = 600,
       width = 36,height = 20)

