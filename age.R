
gr2<-summarise(group_by(main.frame),
               A14_16=sum(G10),
               A16_18=sum(G11),
               A18_25=sum(G12),
               A25_30=sum(G13),
               A30_50=sum(G14),
               A50_65=sum(G15),
               A65=sum(G16),
               AS=sum(A14_16,A16_18,A18_25,A25_30,A30_50,A50_65,A65))


gr2<-as.data.frame (gr2)
gr2<-reshape(gr2,
             timevar = "Comment", 
             times=c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65"), 
             v.names = "QNT", 
             varying = c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65"), 
             direction = "long")

bylo<-c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65")
stalo<-c("від 14 до 16 років","від 16 до 18 років","від 18 до 25 років","від 25 до 30 років",
         "від 30 до 50 років","від 50 до 65 років","від 65 років і старше")
for (i in 1:length(bylo))
{gr2$Comment[gr2$Comment==bylo[i]]<-stalo[i]}



age1<-ggplot(gr2,aes(x=factor(Comment,levels = stalo),fill=Comment, y=QNT, 
                     label=paste(QNT,"-",round(QNT/AS*100,1),"%")))
age1+geom_bar(stat="identity")+
  labs(y="кількість засуджених",title="Розподіл засуджених осіб за віком на момент 
вчинення кримінального правопорушення",
       subtitle=paste("за період з",ymin,"по", ymax, "рік"))+
  theme (legend.title = element_blank(),
         axis.title.x=element_blank(),
         axis.text=element_blank(),
         axis.ticks =element_blank(),
         axis.title.y=element_blank())+geom_text(y=max(gr2$QNT)/8,hjust=0,angle=90)

mysave(vis)
vis<-vis+1

gr2<-summarise(group_by(main.frame, Year),
               A14_16=sum(G10),
               A16_18=sum(G11),
               A18_25=sum(G12),
               A25_30=sum(G13),
               A30_50=sum(G14),
               A50_65=sum(G15),
               A65=sum(G16),
               AS=sum (G10,G11,G12,G13,G14,G15,G16))

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
                     label=paste(QNT,"-",round(QNT/AS*100,1),"%")))
age1+geom_bar(stat="identity")+facet_wrap(.~Year,ncol=5,dir="h")+
  labs(y="%",title="Розподіл засуджених осіб за віком 
на момент вчинення кримінального правопорушення",
       subtitle=paste("за період з",ymin,"по", ymax, "рік"))+
  theme (legend.title = element_blank(),legend.position = "bottom",
         axis.title.x=element_blank(),
         axis.text=element_blank(),
         axis.ticks =element_blank(),
         axis.title.y=element_blank())+geom_text(y=max(gr2$QNT)/8,hjust=0,angle=90)

mysave(vis)
vis<-vis+1





gr2<-summarise(group_by(main.frame, Chapter),
               A14_16=sum(G10),
               A16_18=sum(G11),
               A18_25=sum(G12),
               A25_30=sum(G13),
               A30_50=sum(G14),
               A50_65=sum(G15),
               A65=sum(G16),
               AS=sum (G10,G11,G12,G13,G14,G15,G16))

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


for (i in 1:20)
{gr2$HOR[gr2$Chapter==chapter_ord[i]]<-max(gr2$QNT[gr2$Chapter==chapter_ord[i]])/10}
###
age1<-ggplot(gr2,aes(x=factor(Comment,levels = stalo),fill=Comment, y=QNT,
                     label=paste(QNT,"-",round(QNT/AS*100,1),"%")))
age1+geom_bar(stat="identity")+
  facet_wrap(.~factor(Chapter,levels=chapter_ord),
             ncol=5,dir="h",scales="free_y")+
  labs(y="%",title="Розподіл засуджених осіб за віком 
на момент вчинення кримінального правопорушення",
       subtitle=paste("за розділами Особливої частини КК, за період з",ymin,"по", ymax, "рік"))+
  theme (legend.title = element_blank(),legend.position = "bottom",
         axis.title.x=element_blank(),
         axis.text.x = element_blank(),
         axis.ticks =element_blank(),
         axis.title.y=element_blank(),
  )+geom_text(y=gr2$HOR,hjust=0,angle=90,size=2)

mysave(vis)
vis<-vis+1




gr2<-summarise(group_by(main.frame,Year, Chapter),
               A14_16=sum(G10),
               A16_18=sum(G11),
               A18_25=sum(G12),
               A25_30=sum(G13),
               A30_50=sum(G14),
               A50_65=sum(G15),
               A65=sum(G16))

gr2<-subset(gr2,!is.na(Chapter))
gr2<-as.data.frame (gr2)
gr2<-reshape(gr2,
             timevar = "Comment", 
             times=c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65"), 
             v.names = "QNT", 
             varying = c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65"), 
             direction = "long")

bylo<-c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65")
stalo<-c("від 14 до 16 років","від 16 до 18 років","від 18 до 25 років","від 25 до 30 років",
         "від 30 до 50 років","від 50 до 65 років","від 65 років і старше")
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
  scale_fill_discrete(breaks=stalo,
                      type=c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02","#a6761d"))+
  labs(y="кількість засуджених",title="Розподіл засуджених осіб за віком 
на момент вчинення кримінального правопорушення",
       subtitle=paste("за розділами Особливої частини КК, за період з",ymin,"по", ymax, "рік"))+
  theme (legend.title = element_blank(),legend.position = "bottom",
         axis.ticks.y =element_blank(),
         axis.text.x=element_text(angle=90),
         axis.title=element_blank())+
  scale_x_discrete(breaks=c(2013,2014,2015,2016,2017,2018,2019,2020,2021))

mysave(vis)
vis<-vis+1


gr2<-summarise(group_by(main.frame,Year, Chapter),
               A14_16=sum(G10),
               A16_18=sum(G11),
               A18_25=sum(G12),
               A25_30=sum(G13),
               A30_50=sum(G14),
               A50_65=sum(G15),
               A65=sum(G16),
               AS=sum (G10,G11,G12,G13,G14,G15,G16),
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
stalo<-c("від 14 до 16 років","від 16 до 18 років","від 18 до 25 років","від 25 до 30 років",
         "від 30 до 50 років","від 50 до 65 років","від 65 років і старше")
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
  scale_fill_discrete(breaks=stalo,
                      type=c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02","#a6761d"))+
  labs(y="кількість засуджених",title="Відсотковий розподіл засуджених осіб за віком 
на момент вчинення кримінального правопорушення",
       subtitle=paste("за розділами Особливої частини КК, за період з",ymin,"по", ymax, "рік"))+
  theme (legend.title = element_blank(),legend.position = "bottom",
         axis.ticks.y =element_blank(),
         axis.text.x=element_text(angle=90),
         axis.title=element_blank())+
  scale_x_discrete(breaks=c(2013,2014,2015,2016,2017,2018,2019,2020,2021))

mysave(vis)
vis<-vis+1