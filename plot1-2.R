# Візуалізації 1-2

library (tidyverse)

  
merge_year<-data.frame()


pgo_year<- summarise(group_by(pgo_raw, Year), 
                         QNT=sum(ACC),
                         Type="обліковано проваджень")
merge_year<-pgo_year

pgo_year<- summarise(group_by(pgo_raw, Year), 
                     QNT=sum(INDICM,REL,MED,EDU),
                     Type="проваджень направлено до суду")
merge_year<-rbind(merge_year,pgo_year)

court_year<- summarise(group_by(court_raw, Year), 
                       QNT=sum(CONVIC),
                       Type="засуджено осіб")
merge_year<-rbind(merge_year,court_year)


gr<-ggplot(merge_year, 
           aes(x=factor(Year), y=QNT,
           fill=factor(Type, levels=c("засуджено осіб",
                                      "проваджень направлено до суду",
                                      "обліковано проваджень") )))
gr+geom_histogram(stat="identity")+scale_fill_brewer(palette = "Set1")+
labs(title = "Обліковані правопорушення, провадження направлені до суду,
кількість засуджених")+
theme (axis.title=element_blank(), 
       legend.title = element_blank(), legend.position="bottom")

mysave (vis)
vis<-vis+1


mean_merge<-summarise(group_by(merge_year,Type),
                      mean_v=mean(QNT))

gr2<-ggplot (merge_year, aes(x=factor(Year), y=QNT,label=QNT,fill=Type))
gr2+geom_histogram(stat="identity",show.legend = FALSE)+
  scale_fill_brewer(palette = "Set1")+
  geom_text(size=3,angle=90, position = position_stack(vjust = 0.5))+
  geom_hline(data = mean_merge, color="chartreuse", show.legend = FALSE,
             aes(yintercept=mean_v,
                 size=1,
                 alpha=1/2))+
  geom_text(data=mean_merge,aes(x=7,
            y=mean_v, label=round (mean_v)))+
    facet_grid(.~factor(Type,levels = c("обліковано проваджень",
                                      "проваджень направлено до суду",
                                      "засуджено осіб")))+
  labs(title = 
"Обліковані правопорушення, провадження направлені до суду,кількість засуджених")+
  theme(axis.text=element_text(), axis.title=element_blank(),
        axis.text.x = element_text(angle=90, vjust=0.5))+
  scale_y_continuous(labels = point)

mysave (vis)
vis<-vis+1

#merge_year_tab<-data.frame(
#  Year = unique(merge_year$Year),
#  ACC = merge_year$QNT[merge_year$Type=="обліковано проваджень"],
#  COURT = merge_year$QNT[merge_year$Type=="проваджень направлено до суду"],
#  CONVIC =merge_year$QNT[merge_year$Type=="засуджено осіб"])
#library (grid)
#library (gridExtra)
#tableGrob(merge_year_tab, cols = c("Рік","обліковано проваджень",
 #                  "проваджень направлено до суду",
  #                 "засуджено осіб"))

#grid.newpage()
#grid.table(merge_year_tab, cols = c("Рік","обліковано проваджень",
           # "проваджень направлено до суду",
          #  "засуджено осіб"))
#dev.copy (png, file="table.png") 
#dev.off ()

#library (htmlTable)
#library (magrittr)
#htmlTable(merge_year_tab) #Очень важно тут есть группировки строк напр 2018, 2019


#merge_year_tab %>% 
 # addHtmlTableStyle(col.rgroup = c("none", "#F7F7F7")) %>% 
  #htmlTable
#library (xtable)
#xtable(merge_year_tab)
