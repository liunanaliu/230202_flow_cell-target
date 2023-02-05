###############20230131 nanaliu liver
##analgesic sepsis combinedd run1 and run2
###cd /media/l33/My Passport/230124CytoFLex/20230131_analgesic_sepsis_combined_run1+2
##first for cell type
load("/media/l33/My Passport/230124CytoFLex/20230124_analgesic_sepsis_run1/230131_liver_run1_cells3.rda")
load("/media/l33/My Passport/230124CytoFLex/20230130_analgesic_sepsis_run2/230131_liver_run2_cells3.rda")
liver_run1_cells3$Run <- 1
liver_run2_cells3$Run <- 2
combined_liver_cells <- rbind(liver_run1_cells3,liver_run2_cells3)
table(combined_liver_cells$Run)
###run1 = 84, run2 = 112
table(combined_liver_cells$Analgesic)
table(combined_liver_cells$Group)
save(combined_liver_cells,file = '20230131_combined_liver_cells.rda')

#########################################
library(ggplot2)
library(ggpubr)
ggplot(combined_liver_cells,aes(x=Group, y=Percent,fill=CellType))+
  facet_grid(cols = vars(CellType))+
  geom_col(alpha=.3)+
  theme_classic()+
  labs(x='',y='Percent of cellType in liver')+
  theme(legend.position = 'none',
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14),
        strip.text.y = element_text(face = 'bold',size = 14))
ggsave('20230131_p1liver_Ocelltype_combined_1.png',
       units = 'cm',
       dpi = 600,
       width = 40,height = 30)
###################################################
ggplot(combined_liver_cells,aes(x=Group, y=Percent,fill=CellType))+
  facet_grid(cols = vars(CellType),
             rows=vars(Run))+
  geom_col(alpha=.3)+
  theme_classic()+
  labs(x='',y='Percent of cellType in liver')+
  theme(legend.position = 'none',
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14),
        strip.text.y = element_text(face = 'bold',size = 14))

ggsave('20230131_p1liver_Ocelltype_combined_2.png',
       units = 'cm',
       dpi = 600,
       width = 40,height = 30)
###this one is better that the last one
p1_celltype <- ggplot(combined_liver_cells,aes(x=Group, y=Percent,fill=CellType))+
  facet_grid(cols = vars(CellType))+
  geom_bar(alpha=.3,stat = 'identity',#color=Run,
           position = position_dodge2())+
  theme_classic()+
  labs(x='',y='Percent of cellType in liver')+
  theme(legend.position = 'none',
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))

p1_celltype
ggsave('20230131_p1liver_celltype_combined_1.png',
       units = 'cm',
       dpi = 600,
       width = 40,height = 30)
###################################################
p1_celltype <- ggplot(combined_liver_cells,aes(x=Group, y=Percent,fill=CellType))+
  facet_grid(cols = vars(CellType),
             rows=vars(Run))+
  geom_bar(alpha=.3,stat = 'identity',#color=Run,
           position = position_dodge2())+
  theme_classic()+
  labs(x='',y='Percent of cellType in liver')+
  theme(legend.position = 'none',
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))

p1_celltype
ggsave('20230131_p1liver_celltype_combined_2.png',
       units = 'cm',
       dpi = 600,
       width = 40,height = 30)


########################################
#######second for targets
load("/media/l33/My Passport/230124CytoFLex/20230124_analgesic_sepsis_run1/230131_liver_run1_targets3.rda")
liver_run1_targets3$Run <- 1
load("/media/l33/My Passport/230124CytoFLex/20230130_analgesic_sepsis_run2/230131_liver_run2_targets3.rda")
liver_run2_targets3$Run <- 2
combined_liver_targets <- rbind(liver_run1_targets3,liver_run2_targets3)
table(combined_liver_targets$Run)
###run1 = 420, run2 = 560
table(combined_liver_targets$Analgesic)
table(combined_liver_targets$Group)
save(combined_liver_targets,file = '20230131_combined_liver_targets.rda')

#############################
ggplot(combined_liver_targets,aes(x=Group, y=Percent,fill=Targets))+
  facet_grid(cols = vars(CellType))+
  geom_col(alpha=.3)+
  theme_classic()+
  labs(x='',y='Percent of cellType in liver')+
  theme(legend.position = 'none',
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        #strip.text.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))

###this one is better that the last one
p1_celltype <- ggplot(combined_liver_targets,aes(x=Group, y=Percent,fill=Targets))+
  facet_grid(cols = vars(CellType),
             rows=vars(Run))+
  geom_bar(alpha=.3,stat = 'identity',
           position = position_dodge2())+
  theme_classic()+
  labs(x='',y='Percent of cellType in liver')+
  theme(legend.position = 'none',
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))

p1_celltype

library(ggbreak)
p1_target <- ggplot(combined_liver_targets,aes(x=Group, y=Percent,fill=CellType))+
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
  labs(x='',y='Mean biomarkers of cellType in liver')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))
p1_target

ggsave('20230131_p1liver_target.png',
       units = 'cm',
       dpi = 600,
       width = 40,height = 30)



###############################
###third add statistics
library(tidyverse)
load("/media/l33/My Passport/230124CytoFLex/20230124_analgesic_sepsis_run1/20230131_sta_liver_cells_1.rda")
sta_liver_cells_1$Run <- 1
load("/media/l33/My Passport/230124CytoFLex/20230130_analgesic_sepsis_run2/20230131_sta_liver_cells.rda")
sta_liver_cells$Run <- 2
combined_sta_liver_cells <- rbind(sta_liver_cells_1,sta_liver_cells)
###16+16 =32
save(combined_sta_liver_cells,file = '230131_combined_sta_liver_cells.rda')
################
Ocombined_sta_liver_cells <- combined_liver_cells %>% group_by(CellType, Group) %>%
  summarise(mean=mean(Percent),
            sd=sd(Percent))
save(Ocombined_sta_liver_cells,file = '20230131_Ocombined_sta_liver_cells.rda')
##############
ggplot(Ocombined_sta_liver_cells,aes(x=Group, y=mean,fill=CellType))+
  facet_grid(cols = vars(CellType))+
  geom_bar(alpha=.3,position = position_dodge(),
           stat = 'identity')+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),
                width=.2,
                position = position_dodge(.9))+
  theme_classic()+
  labs(x='',y='Mean biomarkers of cellType in liver')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))
ggsave('20230131_p1liver_Osta_cells.png',
       units = 'cm',
       dpi = 600,
       width = 40,height = 30)
##########
ggplot(combined_sta_liver_cells,aes(x=Group, y=mean,fill=CellType))+
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
  labs(x='',y='Mean biomarkers of cellType in liver')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))
ggsave('20230131_p1liver_sta_cells.png',
       units = 'cm',
       dpi = 600,
       width = 40,height = 30)
#############################
sta_liver_targets_1 <- liver_run1_targets3 %>% group_by(Targets, CellType, Group) %>%
  summarise(mean=mean(Percent),
            sd=sd(Percent))
save(sta_liver_targets_1,file = '20230131_sta_liver_targets_1.rda')
load("/media/l33/My Passport/230124CytoFLex/20230131_analgesic_sepsis_combined_run1+2/20230131_sta_liver_targets_1.rda")
sta_liver_targets_1$Run <- 1
load("/media/l33/My Passport/230124CytoFLex/20230130_analgesic_sepsis_run2/20230131_sta_liver_targets.rda")
sta_liver_targets$Run <- 2
combined_sta_liver_targets <- rbind(sta_liver_targets_1,sta_liver_targets)
###80 + 80 = 160
save(combined_sta_liver_targets,file = '230131_combined_sta_liver_targets.rda')
load('230131_combined_sta_liver_targets.rda')
################################
################
Ocombined_sta_liver_targets <- combined_liver_targets %>% 
  group_by(Targets,CellType, Group) %>%
  summarise(mean=mean(Percent),
            sd=sd(Percent))
save(Ocombined_sta_liver_targets,file = '20230131_Ocombined_sta_liver_targets.rda')
load("/media/l33/My Passport/230124CytoFLex/20230131_analgesic_sepsis_combined_run1+2/20230131_Ocombined_sta_liver_targets.rda")
##############
table(Ocombined_sta_liver_targets$Targets)
ggplot(Ocombined_sta_liver_targets[Ocombined_sta_liver_targets$Targets=='IFN gamma',],
       aes(x=Group, y=mean,fill=CellType))+
  facet_grid(Targets~CellType)+
  #facet_wrap(~Targets,nrow = 2)+
  geom_bar(alpha=.3,position = position_dodge(),
           stat = 'identity')+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),
                width=.2,
                position = position_dodge(.9))+
  theme_classic()+
  labs(x='',y='Mean biomarkers of cellType in liver')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14),
        strip.text.y = element_text(face = 'bold',size = 14))
ggsave('20230131_p1liver_Osta_IFNgamma.png',
       units = 'cm',
       dpi = 600,
       width = 40,height = 30)
##########################################
table(Ocombined_sta_liver_targets$CellType)
##Neutrophil color fill = "#00BFC4" 
##Macrophage color fill =  "#C77CFF"
ggplot(Ocombined_sta_liver_targets[Ocombined_sta_liver_targets$CellType=='Neutrophil',#'Macrophage',
                                   ],
       aes(x=Group, y=mean,fill=CellType))+
  facet_grid(CellType ~ Targets)+
  #facet_wrap(~Targets,nrow = 2)+
  geom_bar(alpha=.3,position = position_dodge(),
           stat = 'identity',fill = "#00BFC4"#fill =  "#C77CFF"
           )+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),
                width=.2,
                position = position_dodge(.9))+
  theme_classic()+
  labs(x='',y='Mean biomarkers of cellType in liver')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14),
        strip.text.y = element_text(face = 'bold',size = 14))
ggsave('20230201_p1liver_Osta_neutrophil.png',#'20230201_p1liver_Osta_macrophage.png',
       units = 'cm',
       dpi = 600,
       width = 30,height = 20)
###############
table(combined_sta_liver_targets$CellType)
##Neutrophil color fill = "#00BFC4" 
##Macrophage color fill =  "#C77CFF"
ggplot(combined_sta_liver_targets[combined_sta_liver_targets$CellType=='Neutrophil',#'Macrophage',
                                  ],
       aes(x=Group, y=mean,fill=CellType))+
  #facet_grid(Targets~CellType)+
  facet_grid(cols = vars(Targets),
             rows=vars(Run))+
  #facet_wrap(~Targets,nrow = 2)+
  geom_bar(alpha=.3,position = position_dodge(),
           stat = 'identity', fill = "#00BFC4" #fill =  "#C77CFF"
           )+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),
                width=.2,
                position = position_dodge(.9))+
  theme_classic()+
  labs(x='',y='Mean biomarkers of cellType in liver')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14),
        strip.text.y = element_text(face = 'bold',size = 14))

ggsave('20230201_p1liver_Osta_targets_neutrophil.png',#'20230201_p1liver_Osta_targets_macrophage.png',
       units = 'cm',
       dpi = 600,
       width = 30,height = 20)
##########
ggplot(combined_sta_liver_cells,aes(x=Group, y=mean,fill=CellType))+
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
  labs(x='',y='Mean biomarkers of cellType in liver')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(face = 'bold',size = 8),
        axis.text.y = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 14),
        strip.text.x = element_text(face = 'bold',size = 14))
ggsave('20230131_p1liver_sta_cells.png',
       units = 'cm',
       dpi = 600,
       width = 40,height = 30)






