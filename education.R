


gr2<-summarise(group_by(main.frame),
               A14_16=sum(G35),
               A16_18=sum(G36),
               A18_25=sum(G37),
               A25_30=sum(G38),
               A30_50=sum(G39),
               A50_65=sum(G40),
               A65=sum(G41),
               AS=sum(G35,G36,G37,G38,G39,G40,G41))


gr2<-as.data.frame (gr2)
gr2<-reshape(gr2,
             timevar = "Comment", 
             times=c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65"), 
             v.names = "QNT", 
             varying = c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65"), 
             direction = "long")

bylo<-c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65")
stalo<-c(
  "повна вища",
  "базова вища",
  "професійнотехнічна",
  "повна загальна середня",
  "базова загальна середня",
  "початкова загальна",
  "без освіти"
)
for (i in 1:length(bylo))
{gr2$Comment[gr2$Comment==bylo[i]]<-stalo[i]}



age1<-ggplot(gr2,aes(x=factor(Comment,levels = stalo),fill=Comment, 
                     y=QNT, label=paste(QNT,"-",round(QNT/AS*100,1),"%" )))
age1+geom_bar(stat="identity")+
  labs(y="кількість засуджених",title="Розподіл засуджених осіб за освітою на момент вчинення кримінального правопорушення",
       subtitle=paste("за період з",ymin,"по", ymax, "рік"))+
  theme (legend.title = element_blank(),
         axis.title.x=element_blank(),
         axis.text=element_blank(),
         axis.ticks =element_blank(),
         axis.title.y=element_blank())+geom_text(y=max(gr2$QNT)/8,hjust=0,angle=90)+
  scale_fill_brewer(breaks=stalo,palette = "Set1")

mysave(vis)
vis<-vis+1

gr2<-summarise(group_by(main.frame, Year),
               A14_16=sum(G35),
               A16_18=sum(G36),
               A18_25=sum(G37),
               A25_30=sum(G38),
               A30_50=sum(G39),
               A50_65=sum(G40),
               A65=sum(G41),
               AS=sum(G35,G36,G37,G38,G39,G40,G41))

gr2<-as.data.frame (gr2)
gr2<-reshape(gr2,
             timevar = "Comment", 
             times=c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65"), 
             v.names = "QNT", 
             varying = c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65"), 
             direction = "long")

bylo<-c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65")
stalo<-c(
  "повна вища",
  "базова вища",
  "професійнотехнічна",
  "повна загальна середня",
  "базова загальна середня",
  "початкова загальна",
  "без освіти"
)
for (i in 1:length(bylo))
{gr2$Comment[gr2$Comment==bylo[i]]<-stalo[i]}

age1<-ggplot(gr2,aes(x=factor(Comment,levels = stalo),fill=Comment, y=QNT, 
                     label=paste(QNT,"-",round(QNT/AS*100,1),"%" )   ))
age1+geom_bar(stat="identity")+facet_wrap(.~Year,ncol=5,dir="v")+
  labs(y="кількість засуджених",title="Розподіл засуджених осіб за освітою на момент вчинення кримінального правопорушення",
       subtitle=paste("за період з",ymin,"по", ymax, "рік"))+
  theme (legend.title = element_blank(),legend.position = "bottom",
         axis.title.x=element_blank(),
         axis.text=element_blank(),
         axis.ticks =element_blank(),
         axis.title.y=element_blank())+geom_text(y=max(gr2$QNT)/8,hjust=0,angle=90)+
  scale_fill_brewer(breaks=stalo,palette = "Set1")

mysave(vis)
vis<-vis+1


gr2<-summarise(group_by(main.frame, Chapter),
               A14_16=sum(G35),
               A16_18=sum(G36),
               A18_25=sum(G37),
               A25_30=sum(G38),
               A30_50=sum(G39),
               A50_65=sum(G40),
               A65=sum(G41),
               AS=sum(G35,G36,G37,G38,G39,G40,G41))

gr2<-subset(gr2,!is.na(Chapter))
gr2<-as.data.frame (gr2)
gr2<-reshape(gr2,
             timevar = "Comment", 
             times=c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65"), 
             v.names = "QNT", 
             varying = c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65"), 
             direction = "long")

bylo<-c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65")

for (i in 1:length(bylo))
{gr2$Comment[gr2$Comment==bylo[i]]<-stalo[i]}
horizon_a<-c(1:20)

for (i in 1:20)
{gr2$HOR[gr2$Chapter==chapter_ord[i]]<-max(gr2$QNT[gr2$Chapter==chapter_ord[i]])/8}
###
age1<-ggplot(gr2,aes(x=factor(Comment,levels = stalo),fill=Comment, y=QNT, 
                     label=paste(QNT,"-",round(QNT/AS*100,1),"%" )  ))
age1+geom_bar(stat="identity")+facet_wrap(.~Chapter,ncol=5,dir="h",scales="free_y")+
  labs(y="кількість засуджених",title="Розподіл засуджених осіб за освітою на момент вчинення кримінального правопорушення",
       subtitle=paste("за розділами Особливої частини КК, за період з",ymin,"по", ymax, "рік"))+
  theme (legend.title = element_blank(),legend.position = "bottom",
         axis.title.x=element_blank(),
         axis.text.x = element_blank(),
         axis.ticks =element_blank(),
         axis.title.y=element_blank())+geom_text(y=gr2$HOR,hjust=0,size=2,angle=90)+
  scale_fill_brewer(breaks=stalo,palette = "Set1")

mysave(vis)
vis<-vis+1


gr2<-summarise(group_by(main.frame,Year, Chapter),
               A14_16=sum(G35),
               A16_18=sum(G36),
               A18_25=sum(G37),
               A25_30=sum(G38),
               A30_50=sum(G39),
               A50_65=sum(G40),
               A65=sum(G41))

gr2<-subset(gr2,!is.na(Chapter))
gr2<-as.data.frame (gr2)
gr2<-reshape(gr2,
             timevar = "Comment", 
             times=c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65"), 
             v.names = "QNT", 
             varying = c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65"), 
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
age1+geom_bar(stat="identity")+facet_wrap(.~Chapter,ncol=5,dir="h",scales="free_y")+
  labs(y="кількість засуджених",title="Розподіл засуджених осіб за освітою на момент вчинення кримінального правопорушення",
       subtitle=paste("за розділами Особливої частини КК, за період з",ymin,"по", ymax, "рік"))+
  theme (legend.title = element_blank(),legend.position = "bottom",
         axis.ticks.y =element_blank(),
         axis.text.x=element_text(angle=90),
         axis.title=element_blank())+
  scale_fill_brewer(breaks=stalo,palette = "Set1")
scale_x_discrete(breaks=c(2013,2014,2015,2016,2017,2018,2019,2020,2021))

mysave(vis)
vis<-vis+1


gr2<-summarise(group_by(main.frame,Year, Chapter),
               A14_16=sum(G35),
               A16_18=sum(G36),
               A18_25=sum(G37),
               A25_30=sum(G38),
               A30_50=sum(G39),
               A50_65=sum(G40),
               A65=sum(G41),
               AS=sum (G35,G36,G37,G38,G39,G40,G41),
               pA14_16=A14_16/AS*100,
               pA16_18=A16_18/AS*100,
               pA18_25=A18_25/AS*100,
               pA25_30=A25_30/AS*100,
               pA30_50=A30_50/AS*100,
               pA50_65=A50_65/AS*100,
               pA65=A65/AS*100)

gr2<-subset(gr2,!is.na(Chapter))
gr2<-as.data.frame (gr2)



gr2<-reshape(gr2,
             timevar = "Comment", 
             times=c("pA14_16","pA16_18","pA18_25","pA25_30","pA30_50","pA50_65","pA65"), 
             v.names = "QNT", 
             varying = c("pA14_16","pA16_18","pA18_25","pA25_30","pA30_50","pA50_65","pA65"), 
             direction = "long")

bylo<-c("pA14_16","pA16_18","pA18_25","pA25_30","pA30_50","pA50_65","pA65")

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
  labs(y="кількість засуджених",title="Відсотковий розподіл засуджених осіб за освітою на момент вчинення кримінального правопорушення",
       subtitle=paste("за розділами Особливої частини КК, за період з",ymin,"по", ymax, "рік"))+
  theme (legend.title = element_blank(),legend.position = "bottom",
         axis.ticks.y =element_blank(),
         axis.text.x=element_text(angle=90),
         axis.title=element_blank())+
  scale_x_discrete(breaks=c(2013,2014,2015,2016,2017,2018,2019,2020,2021))+
  scale_fill_brewer(breaks=stalo,palette = "Set1")

mysave(vis)

vis<-vis+1