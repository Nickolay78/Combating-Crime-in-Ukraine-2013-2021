# Візуалізації що Розділів Особливої частини КК (24-143, по 6 на Розділ)

library (stringi)

chapter_txt<-c(
"Злочини проти основ національної безпеки України",
"Кримінальні правопорушення проти життя та здоров'я особи",
"Кримінальні правопорушення проти волі, честі та гідності особи",
"Кримінальні правопорушення проти статевої свободи
та статевої недоторканності особи",
"Кримінальні правопорушення проти виборчих, трудових та 
інших особистих прав і свобод людини і громадянина",
"Кримінальні правопорушення проти власності",
"Кримінальні правопорушення у сфері господарської діяльності",
"Кримінальні правопорушення проти довкілля",
"Кримінальні правопорушення проти громадської безпеки",					
"Кримінальні правопорушення проти безпеки виробництва",
"Кримінальні правопорушення проти безпеки руху та експлуатації транспорту",
"Кримінальні правопорушення проти громадського порядку та моральності",
"Кримінальні правопорушення у сфері обігу наркотичних засобів, психотропних
речовин, їх аналогів або прекурсорів та інші кримінальні правопорушення
проти здоров'я населення",
"Кримінальні правопорушення у сфері охорони державної таємниці, 
недоторканності державних кордонів, забезпечення призову та мобілізації",
"Кримінальні правопорушення проти авторитету органів державної влади, 
органів місцевого самоврядування, об’єднань громадян та кримінальні 
правопорушення проти журналістів",
"Кримінальні правопорушення у сфері використання 
електронно-обчислювальних машин (комп'ютерів), систем 
та комп'ютерних мереж і мереж електрозв'язку",
"Кримінальні правопорушення у сфері службової діяльності та 
професійної діяльності, пов'язаної з наданням публічних послуг",
"Кримінальні правопорушення проти правосуддя",
"Кримінальні правопорушення проти встановленого порядку несення
військової служби (військові кримінальні правопорушення)",
"Кримінальні правопорушення проти миру, безпеки людства 
та міжнародного правопорядку")



#Processing data for first chapter graph
ch1<-summarise(group_by(pgo_raw,Year,Chapter),
                Cur_Chpt=sum(ACC),
               Prob_c=NA)

for (year_c in ymin:ymax)
  for (chaptr_c in 1:20)
  {ch1$Prob_c[ch1$Year==year_c&
ch1$Chapter==as.roman(chaptr_c)]<-(ch1$Cur_Chpt[ch1$Year==year_c&
ch1$Chapter==as.roman(chaptr_c)]/sum(ch1$Cur_Chpt[ch1$Year==year_c])*100)}

ch1_court<-summarise(group_by(court_raw,Year,Chapter),
               Cur_Chpt=sum(CONVIC),
               Prob_c=NA)

ch1_court<-subset(ch1_court,!is.na(Chapter))
for (year_c in ymin:ymax)
  for (chaptr_c in 1:20)
{ch1_court$Prob_c[ch1$Year==year_c&
ch1_court$Chapter==as.roman(chaptr_c)]<-(ch1_court$Cur_Chpt[ch1_court$Year==year_c&
ch1_court$Chapter==as.roman(chaptr_c)]/sum(ch1_court$Cur_Chpt[ch1_court$Year==year_c])*100)}


ch1_t<-select (ch1,1:3)
ch1_t$category<-"обліковано проваджень"
names(ch1_t)<-c("Year", "Chapter", "QNT", "category")

ch1_tt<-select(ch1,1,2,4)
ch1_tt$category<-"% серед облікованих проваджень"
names(ch1_tt)<-c("Year", "Chapter", "QNT", "category")
ch1_t<-rbind (ch1_t,ch1_tt)

ch1_tt<-select(ch1_court,1:3)
ch1_tt$category<-"засуджено осіб"
names(ch1_tt)<-c("Year", "Chapter", "QNT", "category")
ch1_t<-rbind (ch1_t,ch1_tt)

ch1_tt<-select(ch1_court,1,2,4)
ch1_tt$category<-"% серед засуджених осіб"
names(ch1_tt)<-c("Year", "Chapter", "QNT", "category")
ch1_t<-rbind (ch1_t,ch1_tt)


#Processing data for second chapter graph
ch2<-summarise(group_by(pgo_raw,Year, Chapter),
               ART=Article,
               ACC=ACC)
ch2<-subset(ch2,!(ART=="Others"))

for (i in 1:nrow(ch2))
  if (stri_detect_fixed (ch2$ART[i], "*"))
    ch2$ART[i]<-gsub('.$', '',ch2$ART[i])
  
  

ch2$ART<-sub("Article", "",ch2$ART)
ch2$ART<-gsub(" ","",ch2$ART)

#Processing data for third chapter graph

ch3<-summarise (group_by (pgo_raw, Year, Chapter),
                     ART=Article,
                     ACC=ACC,
                     INDICM=INDICM,
                     REL=REL,
                     MED=MED,
                     EDU=EDU,
                    ZAKIN=0,
                RATIO=0)
for (i in 1:nrow(ch3))
     {ch3$RATIO[i]<-(sum(ch3$INDICM[i],ch3$REL[i],ch3$MED[i],ch3$EDU[i])/ch3$ACC[i])*100
     ch3$ZAKIN[i]<-sum(ch3$INDICM[i],ch3$REL[i],ch3$MED[i],ch3$EDU[i])
}
for (i in 1:nrow(ch3))
  if (stri_detect_fixed (ch3$ART[i], "*"))
    ch3$ART[i]<-gsub('.$', '',ch3$ART[i])


ch3$ART<-sub("Article", "",ch3$ART)
ch3$ART<-gsub(" ","",ch3$ART)
ch3<-subset(ch3,!(ART=="Others"))

#Processing data for forth chapter graph

ch4_pre<-court_raw
ch4_pre<-subset(ch4_pre,!is.na(Chapter))
ch4_pre$Article<-sub("Article", "",ch4_pre$Article)
ch4_pre$Article<-gsub(" ","",ch4_pre$Article)

ch4<-summarise(group_by(ch4_pre,Year,Chapter,Article),
                      CRTOT=sum(CRTOT),
                      CONVIC=sum(CONVIC),
                      PROB=sum(PROB),
                      RELAMN=sum(RELAMN),
                      RELOTHR=sum(RELOTHR),
                     RATIO=NA,
                      REPRES=NA)
for (i in 1:nrow(ch4))
  {ch4$RATIO[i]<-(ch4$CONVIC[i]/ch4$CRTOT[i])*100
ch4$REPRES[i]<-((ch4$CONVIC[i]-sum(ch4$PROB[i],ch4$RELAMN[i],ch4$RELOTHR[i]))/ch4$CONVIC[i])*100
}
ch4$RATIO[is.na(ch4$RATIO)]<-0
ch4$REPRES[is.na(ch4$REPRES)]<-0








ch5<-summarise (group_by (pgo_raw, Year, Chapter),
                ART=Article,
                CACC=INDICM+REL+MED+EDU,
                RECID=RECID,
                GROUP=GROUP,
                INTOX=INTOX,
                JUVEN=JUVEN,
                RATIO=(RECID/CACC)*100,
                RATIO_G=(GROUP/CACC)*100,
                RATIO_INT=(INTOX/CACC)*100,
                RATIO_J=(JUVEN/CACC)*100)
for (i in 1:nrow(ch5))
  if (stri_detect_fixed (ch5$ART[i], "*"))
    ch5$ART[i]<-gsub('.$', '',ch5$ART[i])


ch5$ART<-sub("Article", "",ch5$ART)
ch5$ART<-gsub(" ","",ch5$ART)
ch5<-subset(ch5,!(ART=="Others"))

ch5$RATIO[ch5$RATIO==0]<-NA
ch5$RATIO_G[ch5$RATIO_G==0]<-NA
ch5$RATIO_INT[ch5$RATIO_INT==0]<-NA
ch5$RATIO_J[ch5$RATIO_J==0]<-NA


ch5$RECID[ch5$RECID==0]<-NA
ch5$GROUP[ch5$GROUP==0]<-NA
ch5$INTOX[ch5$INTOX==0]<-NA
ch5$JUVEN[ch5$JUVEN==0]<-NA


#Here must cycle starts


for (chx in chapter_ord)
{
print (paste("Обробка даних щодо Розділу ",chx))
  vis<-1
  data_ch_1<-subset(ch1_t,Chapter==as.roman(chx))
  ch1_g<-ggplot (data_ch_1, aes (x=as.factor(Year), y=QNT,label=round(QNT,2)))
  ch1_g+geom_bar(aes (fill=category),stat="identity", show.legend = FALSE)+
    geom_label(size=3,position = position_stack(vjust = 0.97))+
    facet_wrap(.~category, scales="free_y",ncol = 2, dir = "v")+
    labs(title=chapter_txt[as.integer(as.roman(chx))],
      subtitle = paste("Розділ ",chx," Особливої частини КК України"))+
    theme (axis.title = element_blank(),axis.text.y = element_blank(),
           axis.ticks.y = element_blank())
  
  name_s<-paste("rozdil",as.integer(as.roman(chx)),sep="")
  mysave (vis,suf=name_s)
  vis<-vis+1  
  
  
ch2_x=subset(ch2, Chapter==as.roman(chx))

ch2_x<-subset (ch2_x,ACC>0)
horizon<-round(max(ch2_x$ACC)/10)+1
ch2_g<-ggplot(ch2_x,aes(x=as.factor(ART),y=ACC, label = ACC))
ch2_g+geom_bar (aes (fill=ART),stat = "identity", show.legend = FALSE)+facet_wrap(Year~.,ncol=2,dir="v")+
  theme(axis.text.x = element_text(angle=90, vjust=0.5))+
  geom_text(size=2, angle=90, y=horizon, hjust=0)+
  labs(title = "Обліковані кримінальні провадження",
       subtitle= paste("Розділ ", chx," Особливої частини КК"),
       x="Стаття КК", y = "кількість")+
  scale_y_continuous(labels = point)    

mysave (vis,suf=name_s)
vis<-vis+1


ch3_x=subset(ch3, Chapter==as.roman(chx))
ch3_x<-subset (ch3_x,!(RATIO==0))

ch3_g<-ggplot(ch3_x,aes(x=as.factor(ART),y=ZAKIN,label=round(ZAKIN,0)))
ch3_g+geom_bar (aes (fill=ART),stat = "identity", show.legend = FALSE)+facet_wrap(Year~.,ncol=2,dir="v")+
  geom_text(size=2,hjust = 0, y=10, angle =90)+theme(axis.text.x = element_text(angle=90, vjust=0.5))+
  labs(title = "Кількість закінчених проваджень (матеріали по яким направлені до суду)",
       subtitle= paste("Розділ ", chx," Особливої частини КК"),
       x="Стаття КК", y = "кількість")

mysave (vis,suf=name_s)
vis<-vis+1


ch3_g<-ggplot(ch3_x,aes(x=as.factor(ART),y=RATIO,label=round(RATIO,0)))
ch3_g+geom_bar (aes (fill=ART),stat = "identity", show.legend = FALSE)+facet_wrap(Year~.,ncol=2,dir="v")+
  geom_text(size=2,hjust = 0, y=10, angle =90)+theme(axis.text.x = element_text(angle=90, vjust=0.5))+
  labs(title = "Частка закінчених проваджень серед облікованих",
       subtitle= paste("Розділ ", chx," Особливої частини КК"),
       x="Стаття КК", y = "%")+
  geom_hline(aes(yintercept=common_zakin), color="red",
             show.legend = FALSE)

mysave (vis,suf=name_s)
vis<-vis+1

ch5_x=subset(ch5, Chapter==as.roman(chx))
ch5_x<-subset (ch5_x,!(RATIO==0))

horizon<-round(max(ch5_x$RATIO, na.rm = TRUE)/10)+1
ch5_g<-ggplot(ch5_x,aes(x=as.factor(ART),y=RATIO,label=round(RATIO,1)))
ch5_g+geom_bar (aes (fill=ART),stat = "identity", show.legend = FALSE)+facet_wrap(Year~.,ncol=2,dir="v")+
  geom_text(size=2,hjust = 0, y=horizon, angle =90)+theme(axis.text.x = element_text(angle=90, vjust=0.5))+
  labs(title = "Частка проваджень щодо правопорушень, вчинених особами, які раніше вчиняли, 
кримінальні правопорушення, серед загальної кількості закінчених",
       subtitle= paste("Розділ ", chx," Особливої частини КК"),
       x="Стаття КК", y = "%")+
geom_hline(aes(yintercept=common_recid), color="red",
           show.legend = FALSE)

mysave (vis,suf=name_s)
vis<-vis+1



horizon<-round(max(ch5_x$RECID, na.rm = TRUE)/10)+1
ch5_g<-ggplot(ch5_x,aes(x=as.factor(ART),y=RECID,label=RECID))
ch5_g+geom_bar (aes (fill=ART),stat = "identity", show.legend = FALSE)+facet_wrap(Year~.,ncol=2,dir="v")+
  geom_text(size=2,hjust = 0, y=horizon, angle =90)+theme(axis.text.x = element_text(angle=90, vjust=0.5))+
  labs(title = "Кількість закінчених проваджень щодо правопорушень, вчинених особами, 
які раніше вчиняли, кримінальні правопорушення",
       subtitle= paste("Розділ ", chx," Особливої частини КК"),
       x="Стаття КК", y = "кількість проваджень")

mysave (vis,suf=name_s)
vis<-vis+1


horizon<-round(max(ch5_x$RATIO_G, na.rm = TRUE)/10)+1
ch5_g<-ggplot(ch5_x,aes(x=as.factor(ART),y=RATIO_G,label=round(RATIO_G,1)))
ch5_g+geom_bar (aes (fill=ART),stat = "identity", show.legend = FALSE)+facet_wrap(Year~.,ncol=2,dir="v")+
  geom_text(size=2,hjust = 0, y=horizon, angle =90)+theme(axis.text.x = element_text(angle=90, vjust=0.5))+
  labs(title = "Частка проваджень щодо правопорушень, вчинених групою осіб, 
серед загальної кількості закінчених",
       subtitle= paste("Розділ ", chx," Особливої частини КК"),
       x="Стаття КК", y = "%")+
  geom_hline(aes(yintercept=common_group), color="red",
             show.legend = FALSE)


mysave (vis,suf=name_s)
vis<-vis+1



horizon<-round(max(ch5_x$GROUP, na.rm = TRUE)/10)+1
ch5_g<-ggplot(ch5_x,aes(x=as.factor(ART),y=GROUP,label=GROUP))
ch5_g+geom_bar (aes (fill=ART),stat = "identity", show.legend = FALSE)+facet_wrap(Year~.,ncol=2,dir="v")+
  geom_text(size=2,hjust = 0, y=horizon, angle =90)+theme(axis.text.x = element_text(angle=90, vjust=0.5))+
  labs(title = "Кількість закінчених проваджень щодо правопорушень, вчинених групою осіб",
       subtitle= paste("Розділ ", chx," Особливої частини КК"),
       x="Стаття КК", y = "кількість проваджень")

mysave (vis,suf=name_s)
vis<-vis+1




horizon<-round(max(ch5_x$RATIO_INT, na.rm = TRUE)/10)+1
ch5_g<-ggplot(ch5_x,aes(x=as.factor(ART),y=RATIO_INT,label=round(RATIO_INT,1)))
ch5_g+geom_bar (aes (fill=ART),stat = "identity", show.legend = FALSE)+facet_wrap(Year~.,ncol=2,dir="v")+
  geom_text(size=2,hjust = 0, y=horizon, angle =90)+theme(axis.text.x = element_text(angle=90, vjust=0.5))+
  labs(title = "Частка проваджень щодо правопорушень, вчинених у стані сп'яніння, 
серед загальної кількості закінчених",
       subtitle= paste("Розділ ", chx," Особливої частини КК"),
       x="Стаття КК", y = "%")+
geom_hline(aes(yintercept=common_intox), color="red",
           show.legend = FALSE)


mysave (vis,suf=name_s)
vis<-vis+1



horizon<-round(max(ch5_x$INTOX, na.rm = TRUE)/10)+1
ch5_g<-ggplot(ch5_x,aes(x=as.factor(ART),y=INTOX,label=INTOX))
ch5_g+geom_bar (aes (fill=ART),stat = "identity", show.legend = FALSE)+facet_wrap(Year~.,ncol=2,dir="v")+
  geom_text(size=2,hjust = 0, y=horizon, angle =90)+theme(axis.text.x = element_text(angle=90, vjust=0.5))+
  labs(title = "Кількість закінчених проваджень щодо правопорушень, 
вчинених у стані сп'яніння",
       subtitle= paste("Розділ ", chx," Особливої частини КК"),
       x="Стаття КК", y = "кількість проваджень")

mysave (vis,suf=name_s)
vis<-vis+1



check_j<-sum(ch5_x$JUVEN,na.rm=TRUE)
if ((check_j>0)){
horizon<-round(max(ch5_x$RATIO_J,na.rm = TRUE)/10)+1
ch5_g<-ggplot(ch5_x,aes(x=as.factor(ART),y=RATIO_J,label=round(RATIO_J,1)))
ch5_g+geom_bar (aes (fill=ART),stat = "identity", show.legend = FALSE)+facet_wrap(Year~.,ncol=2,dir="v")+
  geom_text(size=2,hjust = 0, y=horizon, angle =90)+theme(axis.text.x = element_text(angle=90, vjust=0.5))+
  labs(title = "Частка проваджень щодо правопорушень, вчинених неповнолітніми або, 
за їх участі серед загальної кількості закінчених",
       subtitle= paste("Розділ ", chx," Особливої частини КК"),
       x="Стаття КК", y = "%")+
  geom_hline(aes(yintercept=common_juven), color="red",
             show.legend = FALSE)

mysave (vis,suf=name_s)
vis<-vis+1


horizon<-round(max(ch5_x$JUVEN, na.rm = TRUE)/10)+1
ch5_g<-ggplot(ch5_x,aes(x=as.factor(ART),y=JUVEN,label=JUVEN))
ch5_g+geom_bar (aes (fill=ART),stat = "identity", show.legend = FALSE)+facet_wrap(Year~.,ncol=2,dir="v")+
  geom_text(size=2,hjust = 0, y=horizon, angle =90)+theme(axis.text.x = element_text(angle=90, vjust=0.5))+
  labs(title = "Кількість закінчених проваджень щодо правопорушень, 
вчинених неповнолітніми або за їх участі",
       subtitle= paste("Розділ ", chx," Особливої частини КК"),
       x="Стаття КК", y = "кількість проваджень")

mysave (vis,suf=name_s)
vis<-vis+1


}else vis<-vis+2






ch4_x=subset(ch4, Chapter==as.roman(chx))
ch4_x<-subset (ch4_x,!(CONVIC==0))
horizon<-round(max(ch4_x$CONVIC)/10)+1
ch4_g<-ggplot(ch4_x,aes(x=as.factor(Article),y=CONVIC,label=CONVIC))
ch4_g+geom_bar (aes (fill=Article),stat = "identity", show.legend = FALSE)+
  facet_wrap(Year~.,ncol=2,dir="v")+
  theme(axis.text.x = element_text(angle=90, vjust=0.5))+
  geom_text(size=2,hjust = 0, y = horizon, angle=90)+
  labs(title = 
"Кількість осіб, засуджених за вчинення кримінальних правопорушень, 
передбачених розділом",
       subtitle= paste("Розділ ", chx," Особливої частини КК"),
       x="Стаття КК", y = "кількість")

mysave (vis,suf=name_s)
vis<-vis+1

ch5_g<-ggplot(ch4_x,aes(x=as.factor(Article), y=RATIO, label=round(RATIO,0)))
ch5_g+geom_bar (aes (fill=Article),stat = "identity", show.legend = FALSE)+
  facet_wrap(Year~.,ncol=2,dir="v")+geom_text(size=2,hjust = 0, y = 10, angle=90)+
   theme(axis.text.x = element_text(angle=90, vjust=0.5))+
  labs(title = "Частка засуджених осіб серед осіб, судові рішення щодо яких набрали законної сили",
       subtitle= paste("Розділ ", chx," Особливої частини КК"),
              x="Стаття КК", y = "%")

mysave (vis,suf=name_s)
vis<-vis+1

ch6_g<-ggplot(ch4_x,aes(x=as.factor(Article), y=REPRES,label=round(REPRES,0)))
ch6_g+geom_bar (aes (fill=Article),stat = "identity", show.legend = FALSE)+
    facet_wrap(Year~.,ncol=2,dir="v")+geom_text(size=2,hjust = 0, y = 10, angle=90)+
  theme(axis.text.x = element_text(angle=90, vjust=0.5))+
  labs(title = "Частка репресивної форми реалізації кримінальної відповідальності",
       subtitle= paste("Розділ ", chx," Особливої частини КК"),
              x="Стаття КК", y = "%")


mysave (vis,suf=name_s)
vis<-vis+1

}
