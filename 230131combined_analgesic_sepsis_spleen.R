###############20230131 nanaliu spleen
##analgesic sepsis combinedd run1 and run2
###cd /media/l33/My Passport/230124CytoFLex/20230131_analgesic_sepsis_combined_run1+2
##first for cell type
load("/media/l33/My Passport/230124CytoFLex/20230124_analgesic_sepsis_run1/230131_spleen_run1_cells3.rda")
load("/media/l33/My Passport/230124CytoFLex/20230130_analgesic_sepsis_run2/230131_spleen_run2_cells3.rda")
spleen_run1_cells3$Run <- 1
spleen_run2_cells3$Run <- 2
combined_spleen_cells <- rbind(spleen_run1_cells3,spleen_run2_cells3)
table(combined_spleen_cells$Run)
###run1 = 80, run2 = 112
table(combined_spleen_cells$Analgesic)
table(combined_spleen_cells$Group)
save(combined_spleen_cells,file = '20230131_combined_spleen_cells.rda')

#########################################
library(ggplot2)
library(ggpubr)
ggplot(combined_spleen_cells,aes(x=Group, y=Percent,fill=CellType))+
  facet_grid(cols = vars(CellType))+
  geom_col(alpha=.3)+
  theme_classic()+
  labs(x='',y='Percent of cellType in spleen')+
  theme(legend.position = 'none',
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14),
        strip.text.y = element_text(face = 'bold',size = 14))
ggsave('20230131_p2spleen_Ocelltype_combined_1.png',
       units = 'cm',
       dpi = 600,
       width = 40,height = 30)
###################################################
ggplot(combined_spleen_cells,aes(x=Group, y=Percent,fill=CellType))+
  facet_grid(cols = vars(CellType),
             rows=vars(Run))+
  geom_col(alpha=.3)+
  theme_classic()+
  labs(x='',y='Percent of cellType in spleen')+
  theme(legend.position = 'none',
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14),
        strip.text.y = element_text(face = 'bold',size = 14))

ggsave('20230131_p2spleen_Ocelltype_combined_2.png',
       units = 'cm',
       dpi = 600,
       width = 40,height = 30)
###this one is better that the last one
p1_celltype <- ggplot(combined_spleen_cells,aes(x=Group, y=Percent,fill=CellType))+
  facet_grid(cols = vars(CellType))+
  geom_bar(alpha=.3,stat = 'identity',#color=Run,
           position = position_dodge2())+
  theme_classic()+
  labs(x='',y='Percent of cellType in spleen')+
  theme(legend.position = 'none',
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))

p1_celltype
ggsave('20230131_p1spleen_celltype_combined_1.png',
       units = 'cm',
       dpi = 600,
       width = 40,height = 30)
###################################################
p1_celltype <- ggplot(combined_spleen_cells,aes(x=Group, y=Percent,fill=CellType))+
  facet_grid(cols = vars(CellType),
             rows=vars(Run))+
  geom_bar(alpha=.3,stat = 'identity',#color=Run,
           position = position_dodge2())+
  theme_classic()+
  labs(x='',y='Percent of cellType in spleen')+
  theme(legend.position = 'none',
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))

p1_celltype
ggsave('20230131_p1spleen_celltype_combined_2.png',
       units = 'cm',
       dpi = 600,
       width = 40,height = 30)


########################################
#######second for targets
load("/media/l33/My Passport/230124CytoFLex/20230124_analgesic_sepsis_run1/230131_spleen_run1_targets3.rda")
spleen_run1_targets3$Run <- 1
load("/media/l33/My Passport/230124CytoFLex/20230130_analgesic_sepsis_run2/230131_spleen_run2_targets3.rda")
spleen_run2_targets3$Run <- 2
combined_spleen_targets <- rbind(spleen_run1_targets3,spleen_run2_targets3)
table(combined_spleen_targets$Run)
###run1 = 400, run2 = 560
table(combined_spleen_targets$Analgesic)
table(combined_spleen_targets$Group)
save(combined_spleen_targets,file = '20230131_combined_spleen_targets.rda')
#############################
ggplot(combined_spleen_targets,aes(x=Group, y=Percent,fill=Targets))+
  facet_grid(cols = vars(CellType))+
  geom_col(alpha=.3)+
  theme_classic()+
  labs(x='',y='Percent of cellType in spleen')+
  theme(legend.position = 'none',
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        #strip.text.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))

###this one is better that the last one
p1_celltype <- ggplot(combined_spleen_targets,aes(x=Group, y=Percent,fill=Targets))+
  facet_grid(cols = vars(CellType),
             rows=vars(Run))+
  geom_bar(alpha=.3,stat = 'identity',
           position = position_dodge2())+
  theme_classic()+
  labs(x='',y='Percent of cellType in spleen')+
  theme(legend.position = 'none',
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))

p1_celltype

library(ggbreak)
p1_target <- ggplot(combined_spleen_targets,aes(x=Group, y=Percent,fill=CellType))+
  ##facet_grid(cols = vars(Targets))+
  facet_wrap(~Targets,nrow = 2)+
  #scale_y_break(c(5000,10000)##,ticklabels = c(1000,3000)
  #              )+
  ##scale_y_continuous(limits = c(0,16000))+
  #geom_col(alpha=.3, #stat = 'identity',
  #  position ='dodge')+
  geom_bar(alpha=.3,stat = 'identity',
           position = position_dodge2())+
  theme_classic()+
  labs(x='',y='Mean biomarkers of cellType in spleen')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))
p1_target

ggsave('20230131_p1spleen_target.png',
       units = 'cm',
       dpi = 600,
       width = 40,height = 30)



###############################
###third add statistics
library(tidyverse)
load("/media/l33/My Passport/230124CytoFLex/20230124_analgesic_sepsis_run1/20230131_sta_spleen_cells_1.rda")
sta_spleen_cells_1$Run <- 1
load("/media/l33/My Passport/230124CytoFLex/20230130_analgesic_sepsis_run2/20230131_sta_spleen_cells.rda")
sta_spleen_cells$Run <- 2
combined_sta_spleen_cells <- rbind(sta_spleen_cells_1,sta_spleen_cells)
###16+16 =32
save(combined_sta_spleen_cells,file = '230131_combined_sta_spleen_cells.rda')
################
Ocombined_sta_spleen_cells <- combined_spleen_cells %>% group_by(CellType, Group) %>%
  summarise(mean=mean(Percent),
            sd=sd(Percent))
save(Ocombined_sta_spleen_cells,file = '20230131_Ocombined_sta_spleen_cells.rda')
##############
ggplot(Ocombined_sta_spleen_cells,aes(x=Group, y=mean,fill=CellType))+
  facet_grid(cols = vars(CellType))+
  geom_bar(alpha=.3,position = position_dodge(),
           stat = 'identity')+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),
                width=.2,
                position = position_dodge(.9))+
  theme_classic()+
  labs(x='',y='Mean biomarkers of cellType in spleen')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))
ggsave('20230131_p1spleen_Osta_cells.png',
       units = 'cm',
       dpi = 600,
       width = 40,height = 30)
##########
ggplot(combined_sta_spleen_cells,aes(x=Group, y=mean,fill=CellType))+
  facet_grid(cols = vars(CellType),
             rows=vars(Run))+
  #facet_wrap(~CellType,rows=Run)+
  #scale_y_break(c(5000,10000)##,ticklabels = c(1000,3000)
  #              )+
  ##scale_y_continuous(limits = c(0,16000))+
  #geom_col(alpha=.3, #stat = 'identity',
  #  position ='dodge')+
  geom_bar(alpha=.3,position = position_dodge(),
           stat = 'identity')+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),
                width=.2,
                position = position_dodge(.9))+
  theme_classic()+
  labs(x='',y='Mean biomarkers of cellType in spleen')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))
ggsave('20230131_p1spleen_sta_cells.png',
       units = 'cm',
       dpi = 600,
       width = 40,height = 30)
#############################
load("/media/l33/My Passport/230124CytoFLex/20230124_analgesic_sepsis_run1/20230131_sta_spleen_targets_1.rda")
sta_spleen_targets_1$Run <- 1
load("/media/l33/My Passport/230124CytoFLex/20230130_analgesic_sepsis_run2/20230131_sta_spleen_targets.rda")
sta_spleen_targets$Run <- 2
combined_sta_spleen_targets <- rbind(sta_spleen_targets_1,sta_spleen_targets)
###80 + 80 = 160
save(combined_sta_spleen_targets,file = '230131_combined_sta_spleen_targets.rda')
load("/media/l33/My Passport/230124CytoFLex/20230131_analgesic_sepsis_combined_run1+2/230131_combined_sta_spleen_targets.rda")
################################
################
Ocombined_sta_spleen_targets <- combined_spleen_targets %>% 
  group_by(Targets,CellType, Group) %>%
  summarise(mean=mean(Percent),
            sd=sd(Percent))
save(Ocombined_sta_spleen_targets,file = '20230131_Ocombined_sta_spleen_targets.rda')
load("/media/l33/My Passport/230124CytoFLex/20230131_analgesic_sepsis_combined_run1+2/20230131_Ocombined_sta_spleen_targets.rda")
##############
table(Ocombined_sta_spleen_targets$Targets)
##CXCR2 IL-1 beta     IL-10 TNF alpha IFN gamma 
ggplot(Ocombined_sta_spleen_targets[Ocombined_sta_spleen_targets$Targets=='TNF alpha',],
       aes(x=Group, y=mean,fill=CellType))+
  facet_grid(Targets~CellType)+
  #facet_wrap(~Targets,nrow = 2)+
  geom_bar(alpha=.3,position = position_dodge(),
           stat = 'identity')+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),
                width=.2,
                position = position_dodge(.9))+
  theme_classic()+
  labs(x='',y='Mean biomarkers of cellType in spleen')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14),
        strip.text.y = element_text(face = 'bold',size = 14))
ggsave('20230131_p1spleen_Osta_TNFalpha.png',
       units = 'cm',
       dpi = 600,
       width = 40,height = 30)

##############
table(Ocombined_sta_spleen_targets$CellType)
##IC    Myeloid Neutrophil Macrophage
##Neutrophil color fill = "#00BFC4" 
##Macrophage color fill =  "#C77CFF"
ggplot(Ocombined_sta_spleen_targets[Ocombined_sta_spleen_targets$CellType=='Neutrophil',#'Macrophage'
                                      ],
       aes(x=Group, y=mean,fill=CellType))+
  facet_grid(CellType ~ Targets)+
  #facet_wrap(~Targets,nrow = 2)+
  geom_bar(alpha=.3,position = position_dodge(),
           stat = 'identity', fill = "#00BFC4" #fill =  "#C77CFF"
           )+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),
                width=.2,
                position = position_dodge(.9))+
  theme_classic()+
  labs(x='',y='Mean biomarkers of cellType in spleen')+
  theme(legend.position = 'none',
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14),
        strip.text.y = element_text(face = 'bold',size = 14))
ggsave('20230201_p1spleen_Osta_neutrophil.png',
       units = 'cm',
       dpi = 600,
       width = 30,height = 20)
ggsave('20230201_p1spleen_Osta_macrophage.png',
       units = 'cm',
       dpi = 600,
       width = 30,height = 20)
###############
table(combined_sta_spleen_targets$CellType)
##Neutrophil color fill = "#00BFC4" 
##Macrophage color fill =  "#C77CFF"
ggplot(combined_sta_spleen_targets[combined_sta_spleen_targets$CellType=='Neutrophil',#'Macrophage',
                                   ],
       aes(x=Group, y=mean,fill=CellType))+
  #facet_grid(Targets~CellType)+
  facet_grid(cols = vars(Targets),
             rows=vars(Run))+
  #facet_wrap(~Targets,nrow = 2)+
  geom_bar(alpha=.3,position = position_dodge(),
           stat = 'identity',fill = "#00BFC4" #,fill =  "#C77CFF"
           )+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),
                width=.2,
                position = position_dodge(.9))+
  theme_classic()+
  labs(x='',y='Mean biomarkers of cellType in spleen')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14),
        strip.text.y = element_text(face = 'bold',size = 14))
ggsave('20230201_p1spleen_Osta_targets_neutrophil.png',
       units = 'cm',
       dpi = 600,
       width = 30,height = 20)
ggsave('20230201_p1spleen_Osta_targets_macrophage.png',
       units = 'cm',
       dpi = 600,
       width = 30,height = 20)
##########
ggplot(combined_sta_spleen_cells,aes(x=Group, y=mean,fill=CellType))+
  facet_grid(cols = vars(CellType),
             rows=vars(Run))+
  #facet_wrap(~CellType,rows=Run)+
  #scale_y_break(c(5000,10000)##,ticklabels = c(1000,3000)
  #              )+
  ##scale_y_continuous(limits = c(0,16000))+
  #geom_col(alpha=.3, #stat = 'identity',
  #  position ='dodge')+
  geom_bar(alpha=.3,position = position_dodge(),
           stat = 'identity')+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),
                width=.2,
                position = position_dodge(.9))+
  theme_classic()+
  labs(x='',y='Mean biomarkers of cellType in spleen')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))
ggsave('20230131_p1spleen_sta_cells.png',
       units = 'cm',
       dpi = 600,
       width = 40,height = 30)






