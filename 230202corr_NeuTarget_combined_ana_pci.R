##############nanaliu 20230202
##corrlation of cytokines
###cxcr2 and IL-10
load('230202_Scombined_tussie_targets.rda')
if(!require(ggcorrplot)) install.packages("ggcorrplot")
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggcorrplot")

library(ggcorrplot)

#####for neutrophils
Scombined_neu_targets <- Scombined_tussie_targets[Scombined_tussie_targets$CellType=='Neutrophil',]

###https://smin95.github.io/dataviz/basics-of-ggplot2-and-correlation-plot.html
library(tidyverse)
Scombined_neu_targets$Animal <- str_sub(Scombined_neu_targets$Animal,
                                        str_length(Scombined_neu_targets$Animal)-4,
                                        str_length(Scombined_neu_targets$Animal))

Scombined_neu_targets[,c(4,5,7)] <- NULL
corScombined_neu_targets <- spread(Scombined_neu_targets,key = 'Targets',
       value = 'Percent')


###if(!require(smplot2)) install.packages("smplot2") failed!!!
corScombined_neu_targets$`IL-10`
ggplot(corScombined_neu_targets,aes(x=CXCR2,y=`IL-10`))+
  geom_point()

corr_neu_target <- round(cor(corScombined_neu_targets[,c(6:10)]),1)
ggcorrplot(corr_neu_target,hc.order = T,type = 'upper',
           outline.color = 'white',
           lab = T,insig = 'blank')

ggsave('230202_corr_neu_target.png',units = 'cm',
       dpi = 600,width = 20,height = 20)
