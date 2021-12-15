rm(list = ls())

library(ggplot2)
library(plotly)
library(tidyr)
library(dplyr)
library(ggthemes)
library(RColorBrewer)
library(colorspace)
library(stringr)

p <- choose_palette()
p
work_dir <- "/home/samuel/Documentos/iGEM_BioPank/ModellingBiopank/SensitivityAnalysis"   
setwd(work_dir)
getwd()

#Abrir dados


fileList <- list.files(path=work_dir,pattern=".csv", full.names = F)

for (i in fileList){
  iname=sub("\\.csv","",i)
  f=read.csv(i, header = T)
  f[4] = iname
  assign(paste0(iname),f)
}

first_tab <- rbind(first_AMP, first_AMPon, first_Bs, first_Leish) %>% 
  mutate(V4 = case_when(
   V4 == "first_AMP" ~ "AMP",
   V4 == "first_AMPon" ~ "AMPon",
    V4 == "first_Bs" ~ "B. subtilis (Chassis)",
    V4 == "first_Leish" ~ "Leishmania")) 
colnames(first_tab)[1] <- "Parameter"
first <- ggplot(first_tab,aes(x = Parameter, y = S1, fill = V4))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = S1-S1_conf, ymax = S1+S1_conf),
                width = 0.2,
                linetype = "dotted",
                position = position_dodge(width = 0.25),
                color="black", size=1)+
  xlab("")+
  ylab("1st Order Sobol Indices")+
  labs(color = "Output (dy/dt)")+
  facet_wrap(vars(V4))+
  theme( 
    panel.background = element_blank(),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="black" ),
    rect = element_rect(fill = "transparent"),
    axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1),
    strip.text = element_text(face="bold", size = 10),
    legend.position = "none",
    axis.text=element_text(size=8),
    axis.title.y = element_text(size = 13, vjust = 2))+
  scale_fill_viridis_d()
first

ggplotly(first, height = 350, width=700)

############################SECOND ORDER##########################

second_tab <- rbind(second_AMP, second_AMPon, second_Bs, second_Leish) %>% 
  mutate(V4 = case_when(
    V4 == "second_AMP" ~ "AMP",
    V4 == "second_AMPon" ~ "AMPon",
    V4 == "second_Bs" ~ "B. subtilis (Chassis)",
    V4 == "second_Leish" ~ "Leishmania")) 
colnames(second_tab)[1] <- "Parameter"


second_tab2 <- str_split_fixed(second_tab$Parameter, ",",2)
second_tab3 <- cbind(second_tab2,second_tab)
colnames(second_tab3)[1] <- "Parameter1"
colnames(second_tab3)[2] <- "Parameter2"
second_tab3
second_tab3$Parameter1 <- gsub("[[:punct:]]", "", as.character(second_tab3$Parameter1))
second_tab3$Parameter2 <- gsub("[[:punct:]]", "", as.character(second_tab3$Parameter2))
second_tab3$Parameter1 <- gsub(" ", "", as.character(second_tab3$Parameter1))
second_tab3$Parameter2 <- gsub(" ", "", as.character(second_tab3$Parameter2))

second_tab4 <- second_tab3[c("Parameter2", "Parameter1","Parameter", "S2", "S2_conf", "V4")] 
colnames(second_tab4)[1] <- "Parameter1"
colnames(second_tab4)[2] <- "Parameter2"
second_tab4
second_tab5 <- rbind(second_tab3, second_tab4)

second <- ggplot(data = second_tab5,aes(x=Parameter1, y=Parameter2, fill = S2)) + 
  geom_tile() + 
  xlab("")+
  ylab("")+
  labs( fill = "2nd Order \n Sobol Indices")+
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.5),
        strip.text = element_text(face="bold", size = 10),
        axis.text=element_text(size=6),
        axis.title.y = element_text(size = 13))+
  scale_fill_viridis_c()+
  facet_wrap(vars(V4))

second

ggplotly(second, height = 600, width=700)





#################TOTAL################################
total_tab <- rbind(total_AMP, total_AMPon, total_Bs, total_Leish) %>% 
  mutate(V4 = case_when(
    V4 == "total_AMP" ~ "AMP",
    V4 == "total_AMPon" ~ "AMPon",
    V4 == "total_Bs" ~ "B. subtilis (Chassis)",
    V4 == "total_Leish" ~ "Leishmania")) 
colnames(total_tab)[1] <- "Parameter"
total_tab
total <- ggplot(total_tab,aes(x = Parameter, y = ST, fill = V4))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = ST-ST_conf, ymax = ST+ST_conf),
                width = 0.2,
                linetype = "dotted",
                position = position_dodge(width = 0.25),
                color="black", size=1)+
  xlab("")+
  ylab("Total-Order Sobol Indices")+
  labs(color = "Output (dy/dt)")+
  facet_wrap(vars(V4))+
  theme( 
    panel.background = element_blank(),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="black" ),
    rect = element_rect(fill = "transparent"),
    axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1),
    strip.text = element_text(face="bold", size = 10),
    legend.position = "none",
    axis.text=element_text(size=8),
    axis.title.y = element_text(size = 13, vjust = 2))+
  scale_fill_viridis_d()
total

ggplotly(total, height = 350, width=700)
