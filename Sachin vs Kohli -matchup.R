getwd()

# data processing for total runs

Kohli_era=read.csv('C:\\Users\\Desktop\\Desktop\\Sachin vs Kohli ; Case Study\\Kohli_era.csv',header=TRUE)
View(Kohli_era)
Kohli_era=Kohli_era[,c("Year","Player","Mat","Inns","NO","Runs",
                       "BF","HS","Ave","SR","RPI")]
View(Kohli_era)
Kohli_yearly_runs=Kohli_era[Kohli_era$Player=="V Kohli (IND)",]
View(Kohli_yearly_runs)

sachin_era=read.csv('C:\\Users\\Desktop\\Desktop\\Sachin vs Kohli ; Case Study\\sachin_era.csv',header=TRUE)
View(sachin_era)
sachin_era=sachin_era[,c("Year","Player","Mat","Inns","NO","Runs",
                         "BF","HS","Ave","SR","RPI")]
View(sachin_era)
sachin_yearly_runs=sachin_era[sachin_era$Player=="SR Tendulkar (IND)",]
View(sachin_yearly_runs)

# normality check
shapiro.test(Kohli_yearly_runs$Runs)
shapiro.test(sachin_yearly_runs$Runs)
# homoscedasticity check
var.test(Kohli_yearly_runs$Runs,sachin_yearly_runs$Runs)
# independent t test
t.test(Kohli_yearly_runs$Runs,sachin_yearly_runs$Runs,
       var.equal=TRUE)

# RPI t test of total runs
#normality check
shapiro.test(Kohli_yearly_runs$RPI)
shapiro.test(sachin_yearly_runs$RPI)
#Being non normal,we will conduct wilcoxon rank test
wilcox.test(Kohli_yearly_runs$RPI,sachin_yearly_runs$RPI,
            exact=FALSE)

# SR t test of total runs
#normality test
shapiro.test(Kohli_yearly_runs$SR)
shapiro.test(sachin_yearly_runs$SR)
#Being non normal,we will conduct wilcoxon rank test
wilcox.test(Kohli_yearly_runs$SR,sachin_yearly_runs$SR,
            exact=FALSE)

# data processing for chasing runs

Kohli_chase=read.csv('C:\\Users\\Desktop\\Desktop\\Sachin vs Kohli ; Case Study\\Kohli_2ndIngs.csv')
Kohli_chase=Kohli_chase[,c("Year","Player","Mat","Inns","NO","Runs",
                       "BF","HS","Ave","SR","RPI")]
Kohli_chase_runs=Kohli_chase[Kohli_chase$Player=="V Kohli (IND)",]
View(Kohli_chase_runs)

sachin_chase=read.csv('C:\\Users\\Desktop\\Desktop\\Sachin vs Kohli ; Case Study\\sachin_2ndIngs.csv',header=TRUE)
sachin_chase=sachin_chase[,c("Year","Player","Mat","Inns","NO","Runs",
                           "BF","HS","Ave","SR","RPI")]
sachin_chase_runs=sachin_chase[sachin_chase$Player=="SR Tendulkar (IND)",]
View(sachin_chase_runs)

# normality check
shapiro.test(Kohli_chase_runs$Runs)
shapiro.test(sachin_chase_runs$Runs)
# Being non normal,we will conduct wilcoxon rank test
wilcox.test(Kohli_chase_runs$Runs,sachin_chase_runs$Runs,
            exact=FALSE)

# RPI t test while chasing
#normality check
shapiro.test(Kohli_chase_runs$RPI)
shapiro.test(sachin_chase_runs$RPI)
#homoscedasticity check
var.test(Kohli_chase_runs$RPI,sachin_chase_runs$RPI)
#independent t test
t.test(Kohli_chase_runs$RPI,sachin_chase_runs$RPI,
       var.equal =TRUE )
t.test(Kohli_chase_runs$RPI,sachin_chase_runs$RPI,
       var.equal =TRUE,alternative="greater" )

# SR t test while chasing
#normality test
shapiro.test(Kohli_chase_runs$SR)
shapiro.test(sachin_chase_runs$SR)
# Being non normal,we will conduct wilcoxon rank test
wilcox.test(Kohli_chase_runs$SR,sachin_chase_runs$SR,
            exact=FALSE)

# data processing for winning contribution

Kohli_win=read.csv('C:\\Users\\Desktop\\Desktop\\Sachin vs Kohli ; Case Study\\Kohli_era_win.csv')
Kohli_win=Kohli_win[,c("Year","Player","Mat","Inns","NO","Runs",
                           "BF","HS","Ave","SR","RPI")]
Kohli_win_runs=Kohli_win[Kohli_win$Player=="V Kohli (IND)",]
View(Kohli_win_runs)

sachin_win=read.csv('C:\\Users\\Desktop\\Desktop\\Sachin vs Kohli ; Case Study\\sachin_era_win.csv',header=TRUE)
sachin_win=sachin_win[,c("Year","Player","Mat","Inns","NO","Runs",
                             "BF","HS","Ave","SR","RPI")]
sachin_win_runs=sachin_win[sachin_win$Player=="SR Tendulkar (IND)",]
View(sachin_win_runs)

# normality check
shapiro.test(Kohli_win_runs$Runs)
shapiro.test(sachin_win_runs$Runs)
# Being non normal,we will conduct wilcoxon rank test
wilcox.test(Kohli_win_runs$Runs,sachin_win_runs$Runs,
            exact=FALSE)

# RPI t test while winning
#normality check
shapiro.test(Kohli_win_runs$RPI)
shapiro.test(sachin_win_runs$RPI)
#homoscedasticity check
var.test(Kohli_win_runs$RPI,sachin_win_runs$RPI)
#independent t test
t.test(Kohli_win_runs$RPI,sachin_win_runs$RPI,
       var.equal =TRUE )

# SR t test while winning
#normality test
shapiro.test(Kohli_win_runs$SR)
shapiro.test(sachin_win_runs$SR)
#Being non normal,we will conduct wilcoxon rank test
wilcox.test(Kohli_win_runs$SR,sachin_win_runs$SR,
            exact=FALSE)
#------------------------------------------------------------------------------------------------ 
#Visualisation

#installing tidyverse packages

install.packages("tidyverse")
library(tidyverse)
names(Kohli_era)
library(dplyr)

# Top 10 players during VKera
VKera_player=group_by(Kohli_era,Player)
VKera_pR=summarise(VKera_player,total_runs=sum(Runs))
VKera_top=head((VKera_pR[order(-VKera_pR$total_runs),]),n=10)
VKera_top
VKera_pRPI=summarise(VKera_player,RPI=mean(RPI))
View(VKera_pRPI)
VKera_tRPI=VKera_pRPI[c(1288,993,445,622,30,704,753,1249,786,1266),]
VKera_pSR=summarise(VKera_player,Strike_rate=mean(SR))
View(VKera_pSR)
VKera_tSR=VKera_pSR[c(1288,993,445,622,30,704,753,1249,786,1266),]
VKera_tSR
VKera_top_p=merge(VKera_top,VKera_tRPI,by.x="Player",by.y="Player")
VKera_top_f=merge(VKera_top_p,VKera_tSR,by.x="Player",by.y="Player")
VKera_top_f
write.csv(VKera_top_f,"C:\\Users\\Desktop\\Desktop\\Sachin vs Kohli ; Case Study\\VKera_top_f.csv",row.names=FALSE)

# Top 10 players during SRTera
SRTera_player=group_by(sachin_era,Player)
SRTera_pR=summarise(SRTera_player,total_runs=sum(Runs))
SRTera_top=head((SRTera_pR[order(-SRTera_pR$total_runs),]),n=10)
SRTera_top
SRTera_pRPI=summarise(SRTera_player,RPI=mean(RPI))
View(SRTera_pRPI)
SRTera_tRPI=SRTera_pRPI[c(1336,1226,1348,608,652,199,1272,1124,726,382),]
SRTera_tRPI
SRTera_pSR=summarise(SRTera_player,Strike_rate=mean(SR))
View(SRTera_pSR)
SRTera_tSR=SRTera_pSR[c(1336,1226,1348,608,652,199,1272,1124,726,382),]
SRTera_tSR
SRTera_top_p=merge(SRTera_top,SRTera_tRPI,by.x="Player",by.y="Player")
SRTera_top_f=merge(SRTera_top_p,SRTera_tSR,by.x="Player",by.y="Player")
SRTera_top_f
write.csv(SRTera_top_f,"C:\\Users\\Desktop\\Desktop\\Sachin vs Kohli ; Case Study\\SRTera_top_f.csv",row.names=FALSE)

# Top 10 players during VK_Chase


VKchase_player=group_by(Kohli_chase,Player)
VKchase_pR=summarise(VKchase_player,total_runs=sum(Runs))
VKchase_top=head((VKchase_pR[order(-VKchase_pR$total_runs),]),n=10)
VKchase_top
VKchase_pRPI=summarise(VKchase_player,RPI=mean(RPI))
View(VKchase_pRPI)
VKchase_tRPI=VKchase_pRPI[c(1151,889,1133,944,676,1117,554,313,1057,707),]
VKchase_tRPI
VKchase_pSR=summarise(VKchase_player,Strike_rate=mean(SR))
View(VKchase_pSR)
VKchase_tSR=VKchase_pSR[c(1151,889,1133,944,676,1117,554,313,1057,707),]
VKchase_tSR
VKchase_top_p=merge(VKchase_top,VKchase_tRPI,by.x="Player",by.y="Player")
VKchase_top_f=merge(VKchase_top_p,VKchase_tSR,by.x="Player",by.y="Player")
VKchase_top_f
write.csv(VKchase_top_f,"C:\\Users\\Desktop\\Desktop\\Sachin vs Kohli ; Case Study\\VKchase_top_f.csv",row.names=FALSE)

# Top 10 players during SRT_Chase

SRTchase_player=group_by(sachin_chase,Player)
SRTchase_pR=summarise(SRTchase_player,total_runs=sum(Runs))
SRTchase_top=head((SRTchase_pR[order(-SRTchase_pR$total_runs),]),n=10)
SRTchase_top
SRTchase_pRPI=summarise(SRTchase_player,RPI=mean(RPI))
View(SRTchase_pRPI)
SRTchase_tRPI=SRTchase_pRPI[c(1183,1194,172,573,1123,1080,34,1097,534,1176),]
SRTchase_tRPI
SRTchase_pSR=summarise(SRTchase_player,Strike_rate=mean(SR))
View(SRTchase_pSR)
SRTchase_tSR=SRTchase_pSR[c(1183,1194,172,573,1123,1080,34,1097,534,1176),]
SRTchase_tSR
SRTchase_top_p=merge(SRTchase_top,SRTchase_tRPI,by.x="Player",by.y="Player")
SRTchase_top_f=merge(SRTchase_top_p,SRTchase_tSR,by.x="Player",by.y="Player")
SRTchase_top_f
write.csv(SRTchase_top_f,"C:\\Users\\Desktop\\Desktop\\Sachin vs Kohli ; Case Study\\SRTchase_top_f.csv",row.names=FALSE)

# Top 10 players during Vk_win

VKwin_player=group_by(Kohli_win,Player)
VKwin_pR=summarise(VKwin_player,total_runs=sum(Runs))
VKwin_top=head((VKwin_pR[order(-VKwin_pR$total_runs),]),n=10)
VKwin_top
VKwin_pRPI=summarise(VKwin_player,RPI=mean(RPI))
View(VKwin_pRPI)
VKwin_tRPI=VKwin_pRPI[c(942,723,322,552,21,771,509,916,929,450),]
VKwin_tRPI
VKwin_pSR=summarise(VKwin_player,Strike_rate=mean(SR))
View(VKwin_pSR)
VKwin_tSR=VKwin_pSR[c(942,723,322,552,21,771,509,916,929,450),]
VKwin_tSR
VKwin_top_p=merge(VKwin_top,VKwin_tRPI,by.x="Player",by.y="Player")
VKwin_top_f=merge(VKwin_top_p,VKwin_tSR,by.x="Player",by.y="Player")
VKwin_top_f
write.csv(VKwin_top_f,"C:\\Users\\Desktop\\Desktop\\Sachin vs Kohli ; Case Study\\VKwin_top_f.csv",row.names=FALSE)

#Top 10 players during SRT_win

SRTwin_player=group_by(sachin_win,Player)
SRTwin_pR=summarise(SRTwin_player,total_runs=sum(Runs))
SRTwin_top=head((SRTwin_pR[order(-SRTwin_pR$total_runs),]),n=10)
SRTwin_top
SRTwin_pRPI=summarise(SRTwin_player,RPI=mean(RPI))
View(SRTwin_pRPI)
SRTwin_tRPI=SRTwin_pRPI[c(918,834,926,444,30,415,136,931,870,573),]
SRTwin_tRPI
SRTwin_pSR=summarise(SRTwin_player,Strike_rate=mean(SR))
View(SRTwin_pSR)
SRTwin_tSR=SRTwin_pSR[c(918,834,926,444,30,415,136,931,870,573),]
SRTwin_tSR
SRTwin_top_p=merge(SRTwin_top,SRTwin_tRPI,by.x="Player",by.y="Player")
SRTwin_top_f=merge(SRTwin_top_p,SRTwin_tSR,by.x="Player",by.y="Player")
SRTwin_top_f
write.csv(SRTwin_top_f,"C:\\Users\\Desktop\\Desktop\\Sachin vs Kohli ; Case Study\\SRTwin_top_f.csv",row.names=FALSE)




install.packages("ggplot2")
library(ggplot2)


install.packages("reshape2")
library(reshape2)

#SRT bowling match up every innings(since 2002)

SRT_matchup=read.csv("C:\\Users\\Desktop\\Desktop\\Sachin vs Kohli ; Case Study\\SRT_matchup.csv")
SRT_matchup=SRT_matchup[,-c(2,3,4)]
SRT_data=melt(SRT_matchup, id = "Year")
SRT_plot=ggplot(SRT_data,            
                aes(x = Year,
                    y = value,
                    color = variable),) +  geom_line(size=1) + geom_point()
SRT_plot=SRT_plot+labs(y="Runs")
SRT_plot=SRT_plot+theme_classic()
SRT_plot=SRT_plot+ggtitle("SRT vs Bowlers (Matchup)")+theme(plot.title = element_text(hjust = 0.5,size = 20,color="black"),
                                                            panel.background =element_rect(fill="black"),
                                                            legend.title = element_blank(), 
                                                            legend.text= element_text(size=14),
                                                            axis.title.x = element_text(size=18),
                                                            axis.text.x = element_text(size=12),
                                                            axis.ticks = element_blank(),
                                                            axis.title.y = element_text(size=18),
                                                            axis.text.y = element_text(size=12))
SRT_plot

#VK bowling match up every innings

VK_matchup=read.csv("C:\\Users\\Desktop\\Desktop\\Sachin vs Kohli ; Case Study\\VK_matchup.csv")
VK_matchup=VK_matchup[,-c(2,3,4)]
VK_data=melt(VK_matchup, id = "Year")
VK_plot=ggplot(VK_data,            
             aes(x = Year,
                 y = value,
                 color = variable)) +  geom_line(size=1) + geom_point()
VK_plot=VK_plot+labs(y="Runs")
VK_plot=VK_plot+theme_classic()
VK_plot=VK_plot+ggtitle("VK vs Bowlers (Matchup)")+theme(plot.title = element_text(hjust = 0.5,size = 20,color="black"),
                                                         panel.background =element_rect(fill="black"),
                                                         legend.title = element_blank(), 
                                                         legend.text= element_text(size=14),
                                                         axis.title.x = element_text(size=18),
                                                         axis.text.x = element_text(size=12),
                                                         axis.ticks = element_blank(),
                                                         axis.title.y = element_text(size=18),
                                                         axis.text.y = element_text(size=12))
VK_plot

setwd("C:\\Users\\Desktop\\Desktop\\Sachin vs Kohli ; Case Study")




