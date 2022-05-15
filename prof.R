stalo<-c(
  "робітники",
  "державні службовці",
  "інші службовці",
  "військовослужбовці",
  "лікарі, фармацевти",
  "вчителі, викладачі",
  "працівники засобів
масової інформації",
  "приватні підприємці",
  "працівники господарських
товариств",
  "учні шкіл, ліцеїв,
коледжів, гімназій",
  "студенти навчальних
закладів",
  "інші заняття",
  "пенсіонери, 
у т.ч. інваліди",
  "безробітні",
  "працездатні, які не
працювали і не навчалися",
  "неповнолітні до 16 років, 
які не працювали і не навчалися",
  "утримувалися в установі 
виконання покарань, під вартою")
bylo<-c(
  "AG17","AG18","AG19","AG20","AG21","AG22",
  "AG23","AG24","AG25","AG26","AG27","AG28",
  "AG29","AG30","AG31","AG33","AG34")



#"G17","G18","G19","G20","G21",
#"G22","G23","G24","G25","G26",
#"G27","G28","G29","G30","G31","G33","G34"

gr2<-summarise(group_by(main.frame),
               AG17=sum(G17),
               AG18=sum(G18),
               AG19=sum(G19),
               AG20=sum(G20),
               AG21=sum(G21),
               AG22=sum(G22),
               AG23=sum(G23),
               AG24=sum(G24),
               AG25=sum(G25),
               AG26=sum(G26),
               AG27=sum(G27),
               AG28=sum(G28),
               AG29=sum(G29),
               AG30=sum(G30),
               AG31=sum(G31),
               AG33=sum(G33),
               AG34=sum(G34),
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


gr2<-arrange(gr2,desc(QNT))
stalo<-gr2$Comment
age1<-ggplot(gr2,aes(x=factor(Comment,levels = stalo),fill=Comment, y=QNT,
                     label=paste (QNT," - ",round(QNT/TOT*100,3),"%")))
age1+geom_bar(stat="identity", show.legend = FALSE)+
  scale_fill_discrete(breaks=stalo,
                      type=c("#1b9e77",
                             "#d95f02",
                             "#7570b3",
                             "#e7298a",
                             "#66a61e",
                             "#e6ab02",
                             "#a6761d",
                             "#a6cee3",
                             "#1f78b4",
                             "#b2df8a",
                             "#33a02c",
                             "#fb9a99",
                             "#e31a1c",
                             "#fdbf6f",
                             "#ff7f00",
                             "#cab2d6",
                             "#6a3d9a"
                      ))+   
  labs(y="кількість засуджених",
       title="Розподіл засуджених осіб за видом занять 
на момент вчинення кримінального правопорушення",
       subtitle=paste("за період з",ymin,"по", ymax, "рік"))+
  theme (legend.position="bottom",legend.title = element_blank(),
         axis.title.x=element_blank(),
         axis.text.y=element_blank(),
         axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),
         axis.ticks =element_blank(),
         axis.title.y=element_blank())+geom_text(y=max(gr2$QNT)/8,hjust=0,angle=90)


mysave(vis)
vis<-vis+1
####### TOTAL BY CHAPTER

stalo<-c(
  "робітники",
  "державні службовці",
  "інші службовці",
  "військовослужбовці",
  "лікарі, фармацевти",
  "вчителі, викладачі",
  "працівники засобів
масової інформації",
  "приватні підприємці",
  "працівники господарських
товариств",
  "учні шкіл, ліцеїв,
коледжів, гімназій",
  "студенти навчальних
закладів",
  "інші заняття",
  "пенсіонери, 
у т.ч. інваліди",
  "безробітні",
  "працездатні, які не
працювали і не навчалися",
  "неповнолітні до 16 років, 
які не працювали і не навчалися",
  "утримувалися в установі 
виконання покарань, під вартою")
bylo<-c(
  "AG17","AG18","AG19","AG20","AG21","AG22",
  "AG23","AG24","AG25","AG26","AG27","AG28",
  "AG29","AG30","AG31","AG33","AG34")



#"G17","G18","G19","G20","G21",
#"G22","G23","G24","G25","G26",
#"G27","G28","G29","G30","G31","G33","G34"

gr2<-summarise(group_by(main.frame, Chapter),
               AG17=sum(G17),
               AG18=sum(G18),
               AG19=sum(G19),
               AG20=sum(G20),
               AG21=sum(G21),
               AG22=sum(G22),
               AG23=sum(G23),
               AG24=sum(G24),
               AG25=sum(G25),
               AG26=sum(G26),
               AG27=sum(G27),
               AG28=sum(G28),
               AG29=sum(G29),
               AG30=sum(G30),
               AG31=sum(G31),
               AG33=sum(G33),
               AG34=sum(G34),
               TOT=sum(G1)
)

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


gr2<-subset(gr2,QNT>0)


for (i in 1:20)
{gr2$HOR[gr2$Chapter==chapter_ord[i]]<-max(gr2$QNT[gr2$Chapter==chapter_ord[i]])/10}


age1<-ggplot(gr2,aes(x=factor(Comment,levels = stalo),fill=Comment, y=QNT, 
                     label=paste (QNT," - ",round(QNT/TOT*100,2),"%")))
age1+geom_bar(stat="identity")+facet_wrap(.~factor(Chapter, levels=chapter_ord),
                                          ncol=5,
                                          dir="h",
                                          scales = "free_y")+
  labs(y="кількість засуджених",title="Розподіл засуджених осіб за розділами Особливої частини,
за видом занять на момент вчинення кримінального правопорушення",
       subtitle=paste("за період з",ymin,"по", ymax, "рік"))+
  theme (legend.title = element_blank(),legend.position = "bottom",
         legend.text=element_text(size=7),legend.direction = "horizontal",
         axis.title.x=element_blank(),
         axis.text=element_blank(),
         axis.ticks =element_blank(),
         axis.title.y=element_blank())+geom_text(y=gr2$HOR,size=2,hjust=0,angle=90)+
  scale_fill_discrete(breaks=stalo,
                      type=c("#1b9e77",
                             "#d95f02",
                             "#7570b3",
                             "#e7298a",
                             "#66a61e",
                             "#e6ab02",
                             "#a6761d",
                             "#a6cee3",
                             "#1f78b4",
                             "#b2df8a",
                             "#33a02c",
                             "#fb9a99",
                             "#e31a1c",
                             "#fdbf6f",
                             "#ff7f00",
                             "#cab2d6",
                             "#6a3d9a"
                      ))



mysave(vis)
vis<-vis+1
