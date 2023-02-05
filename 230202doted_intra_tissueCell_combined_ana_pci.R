######230202 nanaliu
#####add sample dot for each run intra_tissueCell_combined
####
library(tidyverse)
getwd()##"/media/l33/My Passport/230124CytoFLex/20230131_analgesic_sepsis_combined_run1+2"
load('230201_Ocombined_sta_tissue_cells.rda')

###get single sample from each tissue
load('20230131_combined_liver_cells.rda')
combined_liver_cells$Tissue <- 'Liver'

load('20230131_combined_spleen_cells.rda')
combined_spleen_cells$Tissue <- 'spleen'

load('20230131_combined_lavage_cells.rda')
combined_lavage_cells$Tissue <- 'lavage'

load('20230131_combined_blood_cells.rda')
combined_blood_cells$Tissue <- 'blood'

Scombined_tussie_cells <- do.call('rbind',list(combined_liver_cells,combined_spleen_cells,
                     combined_lavage_cells,combined_blood_cells))
####not necessary
Scombined_tussie_cells$Animal <- str_sub(Scombined_tussie_cells$Animal,
                                         str_length(Scombined_tussie_cells$Animal)-4,
                                         str_length(Scombined_tussie_cells$Animal))
##
save(Scombined_tussie_cells,file = '230202_Scombined_tussie_cells.rda')

####
##############
ggplot(Ocombined_sta_tissue_cells[Ocombined_sta_tissue_cells$CellType=='Neutrophil (CD11b + Ly6G +)',],
       aes(x=Group, y=mean,fill=CellType))+
  facet_grid(CellType ~ Tissue)+
  #facet_grid(cols = vars(CellType),
           #  rows=vars(Tissue))+
  geom_bar(alpha=.3,position = position_dodge(),
           stat = 'identity', fill = "#00BFC4" )+
  geom_dotplot(binaxis = 'y',stackdir = 'center')+
  geom_errorbar(aes(ymin=mean,ymax=mean+sd),
                width=.2,
                position = position_dodge(.9))+
  theme_classic()+
  labs(x='',y='MFI of cellType biomarkes in tissue')+
  theme(legend.position = 'none',
        axis.title = element_text(face = 'bold',size = 16),
        axis.text.x = element_blank(),#element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))

#######
Scombined_tussie_cells$Run <- factor(Scombined_tussie_cells$Run)
Scombined_tussie_cells$Tissue
ggplot(Scombined_tussie_cells[Scombined_tussie_cells$CellType=='Neutrophil (CD11b + Ly6G +)',],
       aes(x=Group, y=Percent,fill=Run))+
  facet_grid(CellType ~ Tissue)+
  #facet_grid(cols = vars(CellType),
  #  rows=vars(Tissue))+
  #geom_bar(alpha=.3,position = position_dodge(),
   #        stat = 'identity', fill = "#00BFC4" )+
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
####
ggplot(Scombined_tussie_cells[Scombined_tussie_cells$CellType=='Macrophage (CD11b + Ly6G - F4/80 +)',],
       aes(x=Group, y=Percent,fill=Run))+
  facet_grid(CellType ~ Tissue)+
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
ggsave('20230202_pAllTissue_Sdot_macrophage.png',
       units = 'cm',
       dpi = 600,
       width = 36,height = 20)





