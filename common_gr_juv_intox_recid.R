ch5<-summarise (group_by (pgo_raw,Year,Chapter),
                SACC=sum(ACC),
                CACC=sum(INDICM)+sum(REL)+sum(MED)+sum(EDU),
                SRECID=sum(RECID),
                SGROUP=sum(GROUP),
                SINTOX=sum(INTOX),
                SJUVEN=sum (JUVEN),
                PZAKIN=CACC/SACC*100,
                PRECID=SRECID/CACC*100,
                PGROUP=SGROUP/CACC*100,
                PINTOX=SINTOX/CACC*100,
                PJUVEN=SJUVEN/CACC*100
)

ch5<-subset(ch5,!is.na(Chapter))
ch5<-as.data.frame(ch5)
ch5$Chapter<-as.character(ch5$Chapter)
ch5_t<-ch5
ch5$HOR<-0
for (i in 1:20)
{ch5$HOR[ch5$Chapter==chapter_ord[i]]<-max(ch5$SACC[ch5$Chapter==chapter_ord[i]])/10}

any1<-ggplot(ch5,aes(x=factor(Year,levels = c(ymin:ymax)), fill=Chapter, y=SACC, label=SACC))
any1+geom_bar(stat="identity", show.legend = FALSE)+
  facet_wrap(.~factor(Chapter, levels = chapter_ord),ncol=5,dir="h",scales="free_y")+
  labs(y="кількість проваджень",
       title="Кількість облікованих проваджень",
       subtitle=paste("За розділами Особливої частини КК, за період з",ymin,"по", ymax, "рік"))+
  theme (axis.ticks.y =element_blank(),axis.text.y=element_blank(),
         axis.text.x=element_text(angle=90),
         axis.title=element_blank())+geom_text (angle=90, y=ch5$HOR, hjust=0)


mysave (vis)
vis<-vis+1

ch5$HOR<-0
for (i in 1:20)
{ch5$HOR[ch5$Chapter==chapter_ord[i]]<-max(ch5$CACC[ch5$Chapter==chapter_ord[i]])/10}

any1<-ggplot(ch5,aes(x=factor(Year,levels = c(ymin:ymax)), fill=Chapter, y=CACC, 
    label=paste(CACC,"-",round(CACC/SACC*100,1),"%"  )   ))
any1+geom_bar(stat="identity", show.legend = FALSE)+
  facet_wrap(.~factor(Chapter, levels = chapter_ord),ncol=5,dir="h",scales="free_y")+
  labs(y="кількість проваджень",
       title="Кількість закінчених проваджень та їх частка серед облікованих",
       subtitle=paste("За розділами Особливої частини КК, за період з",ymin,"по", ymax, "рік"))+
  theme (axis.ticks.y =element_blank(),axis.text.y=element_blank(),
         axis.text.x=element_text(angle=90),
         axis.title=element_blank())+geom_text (angle=90, size=2,y=ch5$HOR, hjust=0)


mysave (vis)
vis<-vis+1

ch5_t<-ch5

top3<-ggplot(ch5, aes(x=Year,y=PZAKIN))
top3+geom_point()+geom_line()+
  facet_wrap(.~factor(Chapter,levels = chapter_ord),ncol = 5,dir = "h")+
  labs(title="Частка закінчених проваджень серед облікованих",
       subtitle = paste("За розділами Особливої частини КК, за період з",ymin,"по", ymax, "рік"))+
  scale_x_continuous(breaks = c(ymin:ymax))+
  theme (axis.title = element_blank(), axis.text.x = element_text (angle = 90),
         axis.text.y = element_text(size=6))


mysave (vis)
vis<-vis+1






ch5<-summarise (group_by (pgo_raw),
                CACC=sum(INDICM)+sum(REL)+sum(MED)+sum(EDU),
                SRECID=sum(RECID),
                SGROUP=sum(GROUP),
                SINTOX=sum(INTOX),
                SJUVEN=sum (JUVEN))



stalo<-c(
"вчинені особами, які раніше 
вчиняли, кримінальні правопорушення",
"вчинені групою осіб",
"вчинені у стані сп'яніння",
"вчинених неповнолітніми або 
за їх участі")
bylo<-c("SRECID","SGROUP","SINTOX","SJUVEN")

ch5<-as.data.frame (ch5)
ch5<-reshape(ch5,
             timevar = "Comment", 
             times=bylo, 
             v.names = "QNT", 
             varying = bylo, 
             direction = "long")

for (i in 1:length(bylo))
{ch5$Comment[ch5$Comment==bylo[i]]<-stalo[i]}

totconv<-ch5$CACC[1]
any1<-ggplot(ch5,aes(x=factor(Comment,levels = stalo),fill=Comment, y=QNT, 
                     label=paste(QNT, " - ", round(QNT/totconv*100,1),"%")))
any1+geom_bar(stat="identity", show.legend = TRUE)+
  labs(y="кількість правопорушень",
       title="Кримінальні правопорушення, вчинені окремими категоріями осіб, провадження за
якими закінчені.",
       subtitle=paste("За період з",ymin,"по", ymax, "рік"))+
  theme (legend.position="right",legend.title = element_blank(),
         axis.title.x=element_blank(),
         axis.text=element_blank(),
                  axis.ticks =element_blank(),
         )+geom_text(y=max(ch5$QNT)/8,hjust=0,angle=90)+
  scale_fill_viridis_d(breaks=stalo)

mysave (vis)
vis<-vis+1





ch5<-ch5_t

ch5$HOR<-0
for (i in 1:20)
{ch5$HOR[ch5$Chapter==chapter_ord[i]]<-max(ch5$SRECID[ch5$Chapter==chapter_ord[i]])/7}
ch5<-subset(ch5,SRECID>0)
any1<-ggplot(ch5,aes(x=factor(Year,levels = c(ymin:ymax)), fill=Chapter, y=SRECID, 
                     label=paste(SRECID,"-",round(PRECID,1),"%")))
any1+geom_bar(stat="identity", show.legend = FALSE)+
  facet_wrap(.~factor(Chapter, levels = chapter_ord),ncol=5,dir="h",scales="free_y")+
    labs(y="кількість правопорушень",
         title="Закінчені провадження щодо правопорушень, вчинених особами, які раніше вчиняли 
кримінальні правопорушення та частка таких проваджень серед закінчених",
       subtitle=paste("За розділами Особливої частини КК, за період з",ymin,"по", ymax, "рік"))+
  theme (axis.ticks.y =element_blank(),
         axis.text.x=element_text(angle=90),axis.text.y=element_blank(),
         axis.title=element_blank())+geom_text (angle=90,size=2, y=ch5$HOR, hjust=0)


mysave (vis)
vis<-vis+1


top3<-ggplot(ch5, aes(x=Year,y=PRECID))
top3+geom_point()+geom_line()+
  facet_wrap(.~factor(Chapter,levels = chapter_ord),ncol = 5,dir = "h")+
  labs(title="Частка проваджень щодо правопорушень, вчинених особами, які раніше вчиняли 
кримінальні правопорушення, серед закінчених.",
       subtitle = paste("За розділами Особливої частини КК, за період з",ymin,"по", ymax, "рік"))+
scale_x_continuous(breaks = c(ymin:ymax))+
  theme (axis.title = element_blank(), axis.text.x = element_text (angle = 90),
         axis.text.y = element_text(size=6))


mysave (vis)
vis<-vis+1





for (i in 1:20)
{ch5$HOR[ch5$Chapter==chapter_ord[i]]<-max(ch5$SGROUP[ch5$Chapter==chapter_ord[i]])/7}
ch5<-subset(ch5,SGROUP>0)

any1<-ggplot(ch5,aes(x=factor(Year,levels = c(ymin:ymax)), fill=Chapter, y=SGROUP, 
                     label=paste(SGROUP,"-",round(PGROUP,1),"%")))
any1+geom_bar(stat="identity", show.legend = FALSE)+
  facet_wrap(.~factor(Chapter, levels = chapter_ord),ncol=5,dir="h",scales="free_y")+
  labs(y="кількість правопорушень",
       title="Закінчені провадження щодо правопорушень, вчинених групою осіб
та частка таких проваджень серед закінчених",
       subtitle=paste("За розділами Особливої частини КК, за період з",ymin,"по", ymax, "рік"))+
  theme (axis.ticks.y =element_blank(),axis.text.y=element_blank(),
         axis.text.x=element_text(angle=90),
         axis.title=element_blank())+geom_text (angle=90,size=2, y=ch5$HOR, hjust=0)


mysave (vis)
vis<-vis+1


top3<-ggplot(ch5, aes(x=Year,y=PGROUP))
top3+geom_point()+geom_line()+
  facet_wrap(.~factor(Chapter,levels = chapter_ord),ncol = 5,dir = "h")+
  labs(title="Частка проваджень щодо правопорушень, вчинених групою осіб, серед закінчених.",
       subtitle = paste("За розділами Особливої частини КК, за період з",ymin,"по", ymax, "рік"))+
  scale_x_continuous(breaks = c(ymin:ymax))+
  theme (axis.title = element_blank(), axis.text.x = element_text (angle = 90),
         axis.text.y = element_text(size=6))


mysave (vis)
vis<-vis+1



for (i in 1:20)
{ch5$HOR[ch5$Chapter==chapter_ord[i]]<-max(ch5$SINTOX[ch5$Chapter==chapter_ord[i]])/7}
ch5<-subset(ch5,SINTOX>0)
any1<-ggplot(ch5,aes(x=factor(Year,levels = c(ymin:ymax)), fill=Chapter, y=SINTOX, 
          label=paste(SINTOX,"-",round(PINTOX,1),"%" )   ))
any1+geom_bar(stat="identity", show.legend = FALSE)+
  facet_wrap(.~factor(Chapter, levels = chapter_ord),ncol=5,dir="h",scales="free_y")+
  labs(y="кількість правопорушень",
       title="Закінчені провадження щодо правопорушень, вчинених у стані сп'яніння
та частка таких проваджень серед закінчених",
       subtitle=paste("За розділами Особливої частини КК, за період з",ymin,"по", ymax, "рік"))+
  theme (axis.ticks.y =element_blank(),axis.text.y=element_blank(),
         axis.text.x=element_text(angle=90),
         axis.title=element_blank())+geom_text (angle=90,size=2, y=ch5$HOR, hjust=0)


mysave (vis)
vis<-vis+1


top3<-ggplot(ch5, aes(x=Year,y=PINTOX))
top3+geom_point()+geom_line()+
  facet_wrap(.~factor(Chapter,levels = chapter_ord),ncol = 5,dir = "h")+
  labs(title="Частка проваджень щодо правопорушень, вчинених у стані сп'яніння, серед закінчених.",
       subtitle = paste("За розділами Особливої частини КК, за період з",ymin,"по", ymax, "рік"))+
  scale_x_continuous(breaks = c(ymin:ymax))+
  theme (axis.title = element_blank(), axis.text.x = element_text (angle = 90),
         axis.text.y = element_text(size=6))


mysave (vis)
vis<-vis+1



for (i in 1:20)
{ch5$HOR[ch5$Chapter==chapter_ord[i]]<-max(ch5$SJUVEN[ch5$Chapter==chapter_ord[i]])/7}
ch5<-subset(ch5,SJUVEN>0)

any1<-ggplot(ch5,aes(x=factor(Year,levels = c(ymin:ymax)), fill=Chapter, 
          y=SJUVEN, label=paste(SJUVEN,"-",round(PJUVEN,1),"%" )  ))
any1+geom_bar(stat="identity", show.legend = FALSE)+
  facet_wrap(.~factor(Chapter, levels = chapter_ord),ncol=5,dir="h",scales="free_y")+
  labs(y="кількість правопорушень",
       title="Закінчені провадження щодо правопорушень, вчинених неповнолітніми 
або за їх участю та частка таких проваджень серед закінчених",
       subtitle=paste("За розділами Особливої частини КК, за період з",ymin,"по", ymax, "рік"))+
  theme (axis.ticks.y =element_blank(),axis.text.y=element_blank(),
         axis.text.x=element_text(angle=90),
         axis.title=element_blank())+geom_text (angle=90, size=2, y=ch5$HOR, hjust=0)


mysave (vis)
vis<-vis+1


top3<-ggplot(ch5, aes(x=Year,y=PJUVEN))
top3+geom_point()+geom_line()+
  facet_wrap(.~factor(Chapter,levels = chapter_ord),ncol = 5,dir = "h")+
  labs(title="Частка проваджень щодо правопорушень, вчинених
неповнолітніми або за їх участю, серед закінчених",
       subtitle = paste("За розділами Особливої частини КК, за період з",ymin,"по", ymax, "рік"))+
  scale_x_continuous(breaks = c(ymin:ymax))+
  theme (axis.title = element_blank(), axis.text.x = element_text (angle = 90),
         axis.text.y = element_text(size=6))


mysave (vis)
vis<-vis+1

invert_chapter<-rep("",20)
for (i in 1:20)
  invert_chapter[i]<-chapter_ord[21-i]

tsum<-summarise (group_by (pgo_raw,Chapter),
                SACC=sum(ACC),
                 CACC=sum(INDICM)+sum(REL)+sum(MED)+sum(EDU),
                SRECID=sum(RECID),
                SGROUP=sum(GROUP),
                SINTOX=sum(INTOX),
                SJUVEN=sum (JUVEN),
                PRECID=SRECID/CACC*100,
                PGROUP=SGROUP/CACC*100,
                PINTOX=SINTOX/CACC*100,
                PJUVEN=SJUVEN/CACC*100,
                PZAKIN=CACC/SACC*100)

horizon_y<-sum(tsum$CACC)/sum(tsum$SACC)*100
tot1<-ggplot(tsum,aes(x=factor(Chapter,levels = invert_chapter), y=PZAKIN, label=round(PZAKIN,1)))
tot1+geom_bar(stat="identity")+labs(y="%",
                                    title="Частка закінчених проваджень, серед облікованих",
                                    subtitle=paste("за період з",ymin,"по", ymax, "рік"))+
  theme (axis.title.y=element_blank())+geom_label()+coord_flip()+
  geom_hline(aes(yintercept=horizon_y), color="red", show.legend = FALSE)+
  geom_text (y=horizon_y+1, hjust=0, x=11, color="red", label = paste("середній рівень - ",round(horizon_y,1),"%",sep="") )

common_zakin<-horizon_y
mysave (vis)
vis<-vis+1

horizon_y<-sum(tsum$SRECID)/sum(tsum$CACC)*100
tot1<-ggplot(tsum,aes(x=factor(Chapter,levels = invert_chapter), y=PRECID, label=round(PRECID,1)))
tot1+geom_bar(stat="identity")+labs(y="%",
title="Частка проваджень щодо правопорушень, вчинених особами, які раніше вчиняли 
кримінальні правопорушення, серед закінчених",
                                    subtitle=paste("за період з",ymin,"по", ymax, "рік"))+
  theme (axis.title.y=element_blank())+geom_label()+coord_flip()+
  geom_hline(aes(yintercept=horizon_y), color="red", show.legend = FALSE)+
  geom_text (y=horizon_y+1, hjust=0, x=11, color="red", label = paste("середній рівень - ",round(horizon_y,1),"%",sep="") )

common_recid<-horizon_y
mysave (vis)
vis<-vis+1

horizon_y<-sum(tsum$SGROUP)/sum(tsum$CACC)*100
tot1<-ggplot(tsum,aes(x=factor(Chapter,levels = invert_chapter), y=PGROUP, label=round(PGROUP,1)))
tot1+geom_bar(stat="identity")+labs(y="%",
 title="Частка проваджень щодо правопорушень, вчинених групою осіб, серед закінчених",
                                    subtitle=paste("за період з",ymin,"по", ymax, "рік"))+
  theme (axis.title.y=element_blank())+geom_label()+coord_flip()+
  geom_hline(aes(yintercept=horizon_y), color="red", show.legend = FALSE)+
  geom_text (y=horizon_y+1, hjust=0, x=11, color="red", label = paste("середній рівень - ",round(horizon_y,1),"%",sep="") )

common_group<-horizon_y
mysave (vis)
vis<-vis+1

horizon_y<-sum(tsum$SINTOX)/sum(tsum$CACC)*100
tot1<-ggplot(tsum,aes(x=factor(Chapter,levels = invert_chapter), y=PINTOX, label=round(PINTOX,1)))
tot1+geom_bar(stat="identity")+labs(y="%",
                                    title="Частка проваджень щодо правопорушень, 
вчинених у стані сп'яніння, серед закінчених",
                                    subtitle=paste("за період з",ymin,"по", ymax, "рік"))+
  theme (axis.title.y=element_blank())+geom_label()+coord_flip()+
  geom_hline(aes(yintercept=horizon_y), color="red", show.legend = FALSE)+
  geom_text (y=horizon_y+1, hjust=0, x=11, color="red", label = paste("середній рівень - ",round(horizon_y,1),"%",sep="") )

common_intox<-horizon_y
mysave (vis)
vis<-vis+1

horizon_y<-sum(tsum$SJUVEN)/sum(tsum$CACC)*100
tot1<-ggplot(tsum,aes(x=factor(Chapter,levels = invert_chapter), y=PJUVEN, label=round(PJUVEN,1)))
tot1+geom_bar(stat="identity")+labs(y="%",
                                    title="Частка проваджень щодо правопорушень, 
вчинених неповнолітніми або за їх участю, серед закінчених",
                                    subtitle=paste("за період з",ymin,"по", ymax, "рік"))+
  theme (axis.title.y=element_blank())+geom_label()+coord_flip()+
  geom_hline(aes(yintercept=horizon_y), color="red", show.legend = FALSE)+
  geom_text (y=horizon_y+1, hjust=0, x=11, color="red", label = paste("середній рівень - ",round(horizon_y,1),"%",sep="") )

common_juven<-horizon_y
mysave (vis)
vis<-vis+1
