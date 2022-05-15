
gr2<-summarise(group_by(main.frame),
               A14_16=sum(G56),
               A16_18=sum(G58),
               A18_25=sum(G59),
               A25_30=sum(G60),
               A30_50=sum(G61),
               A50_65=sum(G63),
               A65=sum(G64),
               AS=sum(G45)
)




gr2<-as.data.frame (gr2)
gr2<-reshape(gr2,
             timevar = "Comment", 
             times=c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65"), 
             v.names = "QNT", 
             varying = c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65"), 
             direction = "long")

bylo<-c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65")
stalo<-c(
  "відбули покарання повністю",
  "умовно-дострокове звільнення",
  "амністія",
  "звільнені від покарання
з інших підстав",
  "скоїли злочин, не відбувши
покарання",
  "учинили злочин в місцях 
виконання арешту, обмеження
або позбавлення волі",
  "учинили злочин під час 
іспитового строку"
)

for (i in 1:length(bylo))
{gr2$Comment[gr2$Comment==bylo[i]]<-stalo[i]}



age1<-ggplot(gr2,aes(x=factor(Comment,levels = stalo),fill=Comment, y=QNT, 
                     label=paste(QNT,"-",round(QNT/AS*100,1),"%" )    ))
age1+geom_bar(stat="identity")+
  labs(y="кількість засуджених",
       title="Окремі категорії засуджених осіб, які мали судимість на момент вчинення
кримінального правопорушення. Кількість та частка серед всіх таких засуджених",
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
               A14_16=sum(G56),
               A16_18=sum(G58),
               A18_25=sum(G59),
               A25_30=sum(G60),
               A30_50=sum(G61),
               A50_65=sum(G63),
               A65=sum(G64),
               AS=sum (G45))

gr2<-as.data.frame (gr2)
gr2<-reshape(gr2,
             timevar = "Comment", 
             times=c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65"), 
             v.names = "QNT", 
             varying = c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65"), 
             direction = "long")

for (i in 1:length(bylo))
{gr2$Comment[gr2$Comment==bylo[i]]<-stalo[i]}

age1<-ggplot(gr2,aes(x=factor(Comment,levels = stalo),fill=Comment, y=QNT, 
                     label=paste(QNT,"-",round(QNT/AS*100,1),"%"  )    ))
age1+geom_bar(stat="identity")+facet_wrap(.~Year,ncol=5,dir="h")+
  labs(y="кількість засуджених",
       title="Окремі категорії засуджених осіб, які мали судимість на момент вчинення
кримінального правопорушення. Кількість та частка серед всіх таких засуджених",
       subtitle=paste("за період з",ymin,"по", ymax, "рік"))+
  theme (legend.title = element_blank(),legend.position = "bottom",
         axis.title.x=element_blank(),
         axis.text=element_blank(),
         axis.ticks =element_blank(),
         axis.title.y=element_blank())+geom_text(y=max(gr2$QNT)/8,hjust=0,angle=90)+
  scale_fill_brewer(palette="Set1",breaks=stalo)

mysave(vis)
vis<-vis+1


gr2<-summarise(group_by(main.frame, Chapter),
               A14_16=sum(G56),
               A16_18=sum(G58),
               A18_25=sum(G59),
               A25_30=sum(G60),
               A30_50=sum(G61),
               A50_65=sum(G63),
               A65=sum(G64),
               AS=sum (G45),
               pA14_16=A14_16/AS*100,
               pA16_18=A16_18/AS*100,
               pA18_25=A18_25/AS*100,
               pA25_30=A25_30/AS*100,
               pA30_50=A30_50/AS*100,
               pA50_65=A50_65/AS*100,
               pA65=A65/AS*100)

gr2<-subset(gr2,!is.na(Chapter))
gr2<-as.data.frame (gr2)


gr2_n<-gr2

gr2<-as.data.frame (gr2)
gr2<-reshape(gr2,
             timevar = "Comment", 
             times=bylo, 
             v.names = "QNT", 
             varying = bylo, 
             direction = "long")

for (i in 1:length(bylo))
{gr2$Comment[gr2$Comment==bylo[i]]<-stalo[i]}

gr2<-as.data.frame (gr2)



for (i in 1:20)
{gr2$HOR[gr2$Chapter==chapter_ord[i]]<-max(gr2$QNT[gr2$Chapter==chapter_ord[i]])/8}
###
age1<-ggplot(gr2,aes(x=factor(Comment,levels = stalo),fill=Comment, y=QNT, 
                     label=paste(QNT,"-",round(QNT/AS*100,1),"%"  )    ))
age1+geom_bar(stat="identity")+
  facet_wrap(.~factor(Chapter,levels = chapter_ord),
             ncol=5,dir="h",scales="free_y")+
  labs(y="%",title="Окремі категорії засуджених осіб, які мали судимість на момент вчинення
кримінального правопорушення. Кількість та частка серед всіх таких засуджених",
       subtitle=paste("За розділами Особливої частини КК, за період з",ymin,"по", ymax, "рік"))+
  theme (legend.title = element_blank(),legend.position = "right",
         axis.title.x=element_blank(),
         axis.text = element_blank(),
         axis.ticks =element_blank(),
         axis.title.y=element_blank())+geom_text(y=gr2$HOR,hjust=0,size=3,angle=90)+
  scale_fill_brewer(palette="Set1",breaks=stalo)

mysave(vis)
vis<-vis+1



gr2<-summarise(group_by(main.frame,Year, Chapter),
               A14_16=sum(G56),
               A16_18=sum(G58),
               A18_25=sum(G59),
               A25_30=sum(G60),
               A30_50=sum(G61),
               A50_65=sum(G63),
               A65=sum(G64),
               AS=sum (G45),
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
             times=c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65"), 
             v.names = "QNT", 
             varying = c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65"), 
             direction = "long")


for (i in 1:length(bylo))
{gr2$Comment[gr2$Comment==bylo[i]]<-stalo[i]}

age1<-ggplot(gr2,aes(x=factor(Year),fill=Comment, y=QNT))
age1+geom_bar(stat="identity")+
  facet_wrap(.~factor(Chapter, levels = chapter_ord),ncol=5,dir="h",scales="free_y")+
  labs(y="кількість засуджених",title="Окремі категорії засуджених осіб, які мали судимість на момент вчинення
кримінального правопорушення",
       subtitle=paste("за розділами Особливої частини КК, за період з",ymin,"по", ymax, "рік"))+
  theme (legend.title = element_blank(),legend.position = "bottom",
         axis.ticks.y =element_blank(),
         axis.text.x=element_text(angle=90),
         axis.title=element_blank())+
  scale_x_discrete(breaks=c(2013,2014,2015,2016,2017,2018,2019,2020,2021))+
  scale_fill_brewer(palette="Set1",breaks=stalo)

mysave(vis)
vis<-vis+1


gr2<-summarise(group_by(main.frame,Year),
               A14_16=sum(G56),
               A16_18=sum(G58),
               A18_25=sum(G59),
               A25_30=sum(G60),
               A30_50=sum(G61),
               A50_65=sum(G63),
               A65=sum(G64))



gr2<-as.data.frame (gr2)
gr2<-reshape(gr2,
             timevar = "Comment", 
             times=c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65"), 
             v.names = "QNT", 
             varying = c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65"), 
             direction = "long")


for (i in 1:length(bylo))
{gr2$Comment[gr2$Comment==bylo[i]]<-stalo[i]}




age1<-ggplot(gr2,aes(x=factor(Year),fill=Comment, y=QNT, label=QNT))
age1+geom_bar(stat="identity")+
  labs(y="кількість засуджених",title="Окремі категорії засуджених осіб, які мали судимість на момент вчинення
кримінального правопорушення. Загальні дані",
       subtitle=paste("за період з",ymin,"по", ymax, "рік"))+
  theme (legend.title = element_blank(),legend.position = "bottom",
         axis.ticks.y =element_blank(),
         axis.text.x=element_text(angle=90),
         axis.title=element_blank())+
  scale_x_discrete(breaks=c(2013,2014,2015,2016,2017,2018,2019,2020,2021))+
  scale_fill_brewer(palette="Set1",breaks=stalo)

mysave(vis)
vis<-vis+1

