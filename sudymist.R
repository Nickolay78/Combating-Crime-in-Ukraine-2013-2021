


gr2<-summarise(group_by(main.frame),
               A14_16=sum(G42),
               A16_18=sum(G43),
               A18_25=sum(G44),
               A25_30=sum(G45),
               AS=sum(G1))


gr2<-as.data.frame (gr2)
gr2<-reshape(gr2,
             timevar = "Comment", 
             times=c("A14_16","A16_18","A18_25","A25_30"), 
             v.names = "QNT", 
             varying = c("A14_16","A16_18","A18_25","A25_30"), 
             direction = "long")

bylo<-c("A14_16","A16_18","A18_25","A25_30")
stalo<-c(
  "Раніше вчинили злочин, 
але були звільнені від 
кримінальної відповідальності",
  "Судилися, але визнані такими,
що не мають судимості",
  "Судилися, але судимість
погашена чи знята",
  "Мають судимість"
)
for (i in 1:length(bylo))
{gr2$Comment[gr2$Comment==bylo[i]]<-stalo[i]}
gr2<-as.data.frame (gr2)

age1<-ggplot(gr2,aes(x=factor(Comment,levels = stalo),fill=Comment, y=QNT, 
                     label=paste(QNT,"-",round(QNT/AS*100,1),"%" ) ))
age1+geom_bar(stat="identity")+
  labs(y="кількість засуджених",
       title="Кількість засуджених з попередньою кримінальною протиправною поведінкою
та частки відповідних категорій серед всіх засуджених",
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
               SUMSUD=sum(G42,G43,G44,G45),
               A14_16=sum(G42),
               A16_18=sum(G43),
               A18_25=sum(G44),
               A25_30=sum(G45),
               tot=sum(G1),
               PSUD=sum(A14_16,A16_18,A18_25,A25_30)/tot*100,
               PS1=A14_16/tot*100,
               PS2=A16_18/tot*100,
               PS3=A18_25/tot*100,
               PS4=A25_30/tot*100
               
)
bylo_2<-c("SUMSUD","A14_16","A16_18","A18_25","A25_30")
stalo_2<-c(
  "Загальна кількість засуджених
з попередньою кримінальною
протиправною поведінкою",
  "Раніше вчинили злочин, 
але були звільнені від 
кримінальної відповідальності",
  "Судилися, але визнані такими,
що не мають судимості",
  "Судилися, але судимість
погашена чи знята",
  "Мають судимість"
)
gr2_1<-as.data.frame (gr2)
gr2_1<-reshape(gr2_1,
               timevar = "Comment", 
               times=bylo_2, 
               v.names = "QNT", 
               varying = bylo_2, 
               direction = "long")

for (i in 1:length(bylo_2))
{gr2_1$Comment[gr2_1$Comment==bylo_2[i]]<-stalo_2[i]}
gr2_1<-as.data.frame (gr2_1)
age1<-ggplot(gr2_1,aes(x=factor(Comment,levels = stalo_2),fill=Comment, y=QNT,
                       label=paste(QNT,"-",round(QNT/tot*100),"%"  )    )  )
age1+geom_bar(stat="identity")+facet_wrap(.~Year,ncol=5,dir="h")+
  labs(y="%",title=
         "Кількість засуджених, які характеризуються попередньою кримінальною протиправною
поведінкою, та частки відповідних категорій серед усіх засуджених",
       subtitle=paste("за період з",ymin,"по", ymax, "рік"))+
  theme (legend.title = element_blank(),legend.position = "bottom",
         legend.text = element_text(size=6),
         axis.title.x=element_blank(),
         axis.text=element_blank(),
         axis.ticks =element_blank()
  )+geom_text(y=max(gr2_1$QNT)/8,hjust=0,angle=90)+
  scale_fill_brewer(palette="Set1",breaks=stalo_2)

mysave(vis)
vis<-vis+1


gr2<-summarise(group_by(main.frame, Chapter),
               SUMSUD=sum(G42,G43,G44,G45),
               A14_16=sum(G42),
               A16_18=sum(G43),
               A18_25=sum(G44),
               A25_30=sum(G45),
               tot=sum(G1),
               PSUD=sum(A14_16,A16_18,A18_25,A25_30)/tot*100,
               PS1=A14_16/tot*100,
               PS2=A16_18/tot*100,
               PS3=A18_25/tot*100,
               PS4=A25_30/tot*100
               
)

gr2<-subset(gr2,!is.na(Chapter))

gr2_1<-as.data.frame (gr2)
gr2_1<-reshape(gr2_1,
               timevar = "Comment", 
               times=bylo_2, 
               v.names = "QNT", 
               varying = bylo_2, 
               direction = "long")

for (i in 1:length(bylo_2))
{gr2_1$Comment[gr2_1$Comment==bylo_2[i]]<-stalo_2[i]}
gr2_1<-as.data.frame (gr2_1)


horizon_a<-c(1:20)

for (i in 1:20)
{gr2_1$HOR[gr2$Chapter==chapter_ord[i]]<-max(gr2_1$QNT[gr2_1$Chapter==chapter_ord[i]])/8}               
###
age1<-ggplot(gr2_1,aes(x=factor(Comment,levels = stalo_2),fill=Comment, y=QNT,
                       label=paste(QNT,"-",round(QNT/tot*100),"%"  )    )  )
age1+geom_bar(stat="identity")+facet_wrap(.~factor(Chapter,levels = chapter_ord),
                                          ncol=5,dir="h",scales = "free_y")+
  labs(y="%",
       title="Кількість засуджених з попередньою кримінальною протиправною поведінкою
та частки відповідних категорій серед всіх засуджених",
       subtitle=paste("за розділами Особливої частини КК, за період з",ymin,"по", ymax, "рік"))+
  theme (legend.title = element_blank(),legend.position = "bottom",
         axis.title.x=element_blank(),
         legend.text = element_text(size=6),axis.text.x = element_blank(),
         axis.ticks =element_blank())+geom_text(y=gr2_1$HOR,hjust=0,angle=90,size=3)+
  scale_fill_brewer(palette="Set1",breaks=stalo_2)

mysave(vis)
vis<-vis+1               









gr2<-summarise(group_by(main.frame,Year, Chapter),
               A14_16=sum(G42),
               A16_18=sum(G43),
               A18_25=sum(G44),
               A25_30=sum(G45) )

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
  labs(y="кількість засуджених",title="Розподіл засуджених осіб з попередньою кримінальною 
протиправню поведінкою",
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
               A14_16=sum(G42),
               A16_18=sum(G43),
               A18_25=sum(G44),
               A25_30=sum(G45),
               AS=sum (G42,G43,G44,G45),
               pA14_16=A14_16/AS*100,
               pA16_18=A16_18/AS*100,
               pA18_25=A18_25/AS*100,
               pA25_30=A25_30/AS*100,
               PTOT=AS/TOT*100)

gr2<-subset(gr2,!is.na(Chapter))
gr2<-subset(gr2,PTOT>0)

gr2<-as.data.frame (gr2)

gr2_t<-gr2
for (i in 1:20)
{gr2_t$HOR[gr2$Chapter==chapter_ord[i]]<-max(gr2_t$PTOT[gr2_t$Chapter==chapter_ord[i]],na.rm=TRUE)/8}

age1<-ggplot(gr2_t,aes(x=factor(Year),fill="red", y=PTOT, 
      label=paste(round(PTOT,1),"% -",AS) ))
age1+geom_bar(stat="identity",show.legend = FALSE)+facet_wrap(.~factor(Chapter,
                                                                       levels = chapter_ord),
                                                              ncol=5,dir="h")+
  labs(y="%",title="Частка засуджених осіб з попередньою кримінальною протиправною поведінкою
серед всіх засуджених та кількість таких осіб",
       subtitle=paste("за розділами Особливої частини КК, за період з",ymin,"по", ymax, "рік"))+
  theme (legend.title = element_blank(),legend.position = "bottom",
         axis.text.x=element_text(angle=90),
         axis.title=element_blank())+geom_text(y=3,size=2, hjust=0,angle=90)

mysave(vis)

vis<-vis+1


gr2<-reshape(gr2,
             timevar = "Comment", 
             times=c("pA14_16","pA16_18","pA18_25","pA25_30"), 
             v.names = "QNT", 
             varying = c("pA14_16","pA16_18","pA18_25","pA25_30"), 
             direction = "long")

bylo<-c("pA14_16","pA16_18","pA18_25","pA25_30")

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
  labs(y="%",title="Відсотковий розподіл засуджених осіб з попередньою 
кримінальною протиправною поведінкою",
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
                 Female=sum (G42,G43,G44,G45))
tsum$FemPart<-tsum$Female/tsum$Total*100
horizon_sudim<-sum(tsum$Female)/sum(tsum$Total)*100
common_sudim<-horizon_sudim
tsum<-subset(tsum,!is.na(Chapter))
tot1<-ggplot(tsum,aes(x=factor(Chapter,levels = invert_chapter), y=FemPart, label=round(FemPart,1)))
tot1+geom_bar(stat="identity")+labs(y="%",title="Частка засуджених з попередньою кримінальною правовою поведінкою 
серед усіх засуджених за розділами Особливої частини",
                                    subtitle=paste("за період з",ymin,"по", ymax, "рік"))+
  theme (axis.title.y=element_blank())+geom_label()+coord_flip()+
  geom_hline(aes(yintercept=horizon_sudim), color="red", show.legend = FALSE)+
  geom_text (y=horizon_sudim+1, hjust=0, x=11, color="red", label = paste("середній рівень - ",round(horizon_sudim,1),"%",sep="") )

mysave(vis)
vis<-vis+1


