

gr2<-summarise(group_by(main.frame),
               AS=sum (G46,G47,G48),
               A14_16=sum(G46),
               A16_18=sum(G47),
               A18_25=sum(G48),
               TOT=sum(G1)
               
)


gr2<-as.data.frame (gr2)
gr2<-reshape(gr2,
             timevar = "Comment", 
             times=c("AS", "A14_16","A16_18","A18_25"), 
             v.names = "QNT", 
             varying = c("AS","A14_16","A16_18","A18_25"), 
             direction = "long")

bylo<-c("AS","A14_16","A16_18","A18_25")
stalo<-c(
  "Мали судимість",
  "Одна судимість",
  "Дві судимості",
  "Три і більше
судимостей"
)
for (i in 1:length(bylo))
{gr2$Comment[gr2$Comment==bylo[i]]<-stalo[i]}
gr2<-as.data.frame (gr2)

age1<-ggplot(gr2,aes(x=factor(Comment,levels = stalo),fill=Comment, y=QNT, 
                     label=paste(QNT,"-",round(QNT/TOT*100,1),"%")   )  )
age1+geom_bar(stat="identity")+
  labs(y="кількість засуджених",
       title="Розподіл засуджених, які на момент вчинення кримінального 
правопорушення мали судимість та частка таких осіб серед всіх засуджених",
       subtitle=paste("за період з",ymin,"по", ymax, "рік"))+
  theme (legend.title = element_blank(),
         axis.title.x=element_blank(),
         axis.text=element_blank(),
         axis.ticks =element_blank(),
         axis.title.y=element_blank())+geom_text(y=max(gr2$QNT)/8,hjust=0,angle=90)+
  scale_fill_brewer(palette="Set1",breaks=stalo)



mysave(vis)
vis<-vis+1



gr2<-summarise(group_by(main.frame, Year),
               AS=sum (G46,G47,G48),
               A14_16=sum(G46),
               A16_18=sum(G47),
               A18_25=sum(G48),
               TOT=sum(G1)
)


gr2<-as.data.frame (gr2)
gr2<-reshape(gr2,
             timevar = "Comment", 
             times=bylo, 
             v.names = "QNT", 
             varying = bylo, 
             direction = "long")

for (i in 1:length(bylo))
{gr2$Comment[gr2$Comment==bylo[i]]<-stalo[i]}

age1<-ggplot(gr2,aes(x=factor(Comment,levels = stalo),fill=Comment, y=QNT,
                     label=paste(QNT,"-",round(QNT/TOT*100,1),"%"  )   ))
age1+geom_bar(stat="identity")+facet_wrap(.~Year,ncol=5,dir="h")+
  labs(y="кількість засуджених",
       title="Розподіл засуджених осіб що мали судимість на момент вчинення правопорушення,
за роками та частка таких осіб серед всіх засуджених",
       subtitle=paste("за період з",ymin,"по", ymax, "рік"))+
  theme (legend.title = element_blank(),legend.position = "bottom",
         axis.title.x=element_blank(),
         axis.text=element_blank(),
         axis.ticks =element_blank())+geom_text(y=max(gr2$QNT)/8,hjust=0,angle=90)+
  scale_fill_brewer(palette="Set1",breaks=stalo)

mysave(vis)
vis<-vis+1


gr2<-summarise(group_by(main.frame, Chapter),
               AS=sum (G46,G47,G48),
               A14_16=sum(G46),
               A16_18=sum(G47),
               A18_25=sum(G48),
               TOT=sum(G1))

gr2<-subset(gr2,!is.na(Chapter))

gr2<-as.data.frame (gr2)
gr2<-reshape(gr2,
             timevar = "Comment", 
             times=bylo, 
             v.names = "QNT", 
             varying = bylo, 
             direction = "long")

for (i in 1:length(bylo))
{gr2$Comment[gr2$Comment==bylo[i]]<-stalo[i]}
horizon_a<-c(1:20)

for (i in 1:20)
{gr2$HOR[gr2$Chapter==chapter_ord[i]]<-max(gr2$QNT[gr2$Chapter==chapter_ord[i]])/8}
###
age1<-ggplot(gr2,aes(x=factor(Comment,levels = stalo),fill=Comment, y=QNT, 
                     label=paste(QNT,"-",round(QNT/TOT*100,1),"%"  )   ))
age1+geom_bar(stat="identity")+facet_wrap(.~factor(Chapter,levels=chapter_ord),
                                          ncol=5,dir="h",scales="free_y")+
  labs(y="кількість засуджених",
       title="Розподіл засуджених осіб що мали судимість на момент вчинення правопорушення,
за розділами Особливої частини КК та частка таких осіб серед засуджених
за правопорушення, передбачені певними розділами",
       subtitle=paste("за період з",ymin,"по", ymax, "рік"))+
  theme (legend.title = element_blank(),legend.position = "bottom",
         axis.title.x=element_blank(),
         axis.text = element_blank(),
         axis.ticks =element_blank(),
         axis.title.y=element_blank())+geom_text(y=gr2$HOR,hjust=0,angle=90,size=2)+
  scale_fill_brewer(palette="Set1",breaks=stalo)

mysave(vis)
vis<-vis+1



bylo<-c("A14_16","A16_18","A18_25")
stalo<-c(
  
  "Одна судимість",
  "Дві судимості",
  "Три і більше
судимостей")

gr2<-summarise(group_by(main.frame,Year, Chapter),
               A14_16=sum(G46),
               A16_18=sum(G47),
               A18_25=sum(G48))

gr2<-subset(gr2,!is.na(Chapter))
gr2<-as.data.frame (gr2)
gr2<-reshape(gr2,
             timevar = "Comment", 
             times=bylo, 
             v.names = "QNT", 
             varying = bylo, 
             direction = "long")

for (i in 1:length(bylo))
{gr2$Comment[gr2$Comment==bylo[i]]<-stalo[i]}
horizon_a<-c(1:20)
years<-c(ymin:ymax)
for (i in 1:20)
{gr2$HOR[gr2$Chapter==chapter_ord[i]]<-max(gr2$QNT[gr2$Chapter==chapter_ord[i]])/8}
gr2<-as.data.frame (gr2)
###
age1<-ggplot(gr2,aes(x=factor(Year),fill=Comment, y=QNT))
age1+geom_bar(stat="identity")+facet_wrap(.~factor(Chapter,levels = chapter_ord),
                                          ncol=5,dir="h",scales="free_y")+
  labs(y="кількість засуджених",title="Розподіл засуджених осіб, які мають судимість, за кількістю судимостей",
       subtitle=paste("за розділами Особливої частини КК, за період з",ymin,"по", ymax, "рік"))+
  theme (legend.title = element_blank(),legend.position = "bottom",
         axis.text.x=element_text(angle=90),
         axis.title=element_blank())+
  scale_x_discrete(breaks=c(2013,2014,2015,2016,2017,2018,2019,2020,2021))+
  scale_fill_brewer(palette="Set1",breaks=stalo)

mysave(vis)
vis<-vis+1


gr2<-summarise(group_by(main.frame,Year, Chapter),
               TOT=sum(G1),
               A14_16=sum(G46),
               A16_18=sum(G47),
               A18_25=sum(G48),
               AS=sum (G46,G47,G48),
               pA14_16=A14_16/AS*100,
               pA16_18=A16_18/AS*100,
               pA18_25=A18_25/AS*100,
               PTOT=AS/TOT*100)

gr2<-subset(gr2,!is.na(Chapter))
gr2<-as.data.frame (gr2)
gr2<-subset(gr2,PTOT>0)
gr2_t<-gr2
for (i in 1:20)
{gr2_t$HOR[gr2_t$Chapter==chapter_ord[i]]<-max(gr2_t$PTOT[gr2_t$Chapter==chapter_ord[i]])/8}

age1<-ggplot(gr2_t,aes(x=factor(Year),fill="red", y=PTOT, 
        label=paste(round(PTOT,1),"% -",AS) ))
age1+geom_bar(stat="identity",show.legend = FALSE)+facet_wrap(.~factor(Chapter,
                                                                       levels = chapter_ord),
                                                              ncol=5,dir="h")+
  labs(y="%",title="Частка засуджених осіб з судимістю серед всіх засуджених",
       subtitle=paste("за розділами Особливої частини КК, за період з",ymin,"по", ymax, "рік"))+
  theme (legend.title = element_blank(),legend.position = "bottom",
         axis.text.x=element_text(angle=90),
         axis.title=element_blank())+geom_text(y=3,size=2,hjust=0,angle=90)

mysave(vis)

vis<-vis+1


gr2<-reshape(gr2,
             timevar = "Comment", 
             times=c("pA14_16","pA16_18","pA18_25"), 
             v.names = "QNT", 
             varying = c("pA14_16","pA16_18","pA18_25"), 
             direction = "long")

bylo<-c("pA14_16","pA16_18","pA18_25")

for (i in 1:length(bylo))
{gr2$Comment[gr2$Comment==bylo[i]]<-stalo[i]}
horizon_a<-c(1:20)
years<-c(ymin:ymax)
for (i in 1:20)
{gr2$HOR[gr2$Chapter==chapter_ord[i]]<-max(gr2$QNT[gr2$Chapter==chapter_ord[i]])/8}
gr2<-as.data.frame (gr2)

age1<-ggplot(gr2,aes(x=factor(Year),fill=Comment, y=QNT, label=round(QNT,1)))
age1+geom_bar(stat="identity")+facet_wrap(.~factor(Chapter,
                                                   levels = chapter_ord),
                                          ncol=5,dir="h",scales="free_y")+
  labs(y="%",title="Відсотковий розподіл засуджених з судимістю 
за кількістю судимостей",
       subtitle=paste("за розділами Особливої частини КК, за період з",ymin,"по", ymax, "рік"))+
  theme (legend.title = element_blank(),legend.position = "bottom",
         axis.text.x=element_text(angle=90),
         axis.title=element_blank())+
  scale_fill_brewer(palette="Set1",breaks=stalo)


mysave(vis)

vis<-vis+1

###GRAY THEME


tsum<-summarise (group_by(main.frame,Chapter),
                 Total=sum(G1),
                 Female=sum (G46,G47,G48))
tsum$FemPart<-tsum$Female/tsum$Total*100
horizon_sudim<-sum(tsum$Female)/sum(tsum$Total)*100
common_sudim123<-horizon_sudim
tsum<-subset(tsum,!is.na(Chapter))
tot1<-ggplot(tsum,aes(x=factor(Chapter,levels = invert_chapter), y=FemPart, label=round(FemPart,1)))
tot1+geom_bar(stat="identity")+labs(y="%",title="Частка засуджених з судимістю серед усіх засуджених
за розділами Особливої частини",
                                    subtitle=paste("за період з",ymin,"по", ymax, "рік"))+
  theme (axis.title.y=element_blank())+geom_label()+coord_flip()+
  geom_hline(aes(yintercept=common_sudim123), color="red", show.legend = FALSE)+
  geom_text (y=common_sudim123+1, hjust=0, x=11, color="red", 
             label = paste("середній рівень - ",round(common_sudim123,1),"%",sep="") )

mysave(vis)
vis<-vis+1



