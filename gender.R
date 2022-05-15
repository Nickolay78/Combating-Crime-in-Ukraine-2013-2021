
gr1<-summarise(group_by(main.frame,Year),
               Total=sum(G1),
               Female=sum(G4))
gr1$Fempart<-round(gr1$Female/gr1$Total*100,1)




gr1<-as.data.frame (gr1)
gr1<-reshape(gr1,
             timevar = "Comment", 
             times=c("Total","Female","Fempart"), 
             v.names = "QNT", 
             varying = c("Total","Female","Fempart"), 
             direction = "long")

bylo<-c("Total","Female","Fempart")
stalo<-c("всього засуджено","всього засуджено жінок","частка жінок
серед засуджених (%)")
for (i in 1:length(bylo))
{gr1$Comment[gr1$Comment==bylo[i]]<-stalo[i]}



top_1<-ggplot (gr1,aes(x=Year,label=QNT, y=QNT))
top_1+geom_bar(aes(fill=Comment),stat = "identity",show.legend = FALSE)+
  facet_wrap(ncol=1,scales="free_x",.~factor(Comment, levels=stalo))+
  labs (title = "Частка жінок серед засуджених осіб")+
  geom_text(size=4,position = position_stack(vjust = 0.5))+
  theme (axis.ticks.x=element_blank(),
         axis.text.x=element_blank(),axis.title = element_blank())+
  scale_y_continuous(labels = point)+
  scale_x_continuous (breaks=c(ymin:ymax))+coord_flip()   

mysave (vis)
vis<-vis+1



gr2<-summarise(group_by(main.frame,Year,Chapter),
               Total=sum(G1),
               Female=sum(G4))
gr2<-subset(gr2,Female>0)
gr2<-subset(gr2,!is.na(Chapter))
gr2$Fempart<-round(gr2$Female/gr2$Total*100,1)              
top2<-ggplot(gr2, aes(x=factor(Chapter,levels = invert_chapter),
                      label=paste(round(Fempart,1),"% - ",Female,sep=""),y=Fempart))
top2+geom_bar(fill="green",stat = "identity")+geom_text(y=0,hjust=0,size=3)+
  facet_wrap(.~Year,ncol = 5,dir = "h")+coord_flip()+
  labs(title="Частка жінок серед засуджених за правопорушення, передбачені певними
розділами Особливої частини КК, та кількість засуджених жінок за розділами",
       subtitle = paste("за період з ", ymin," по ",ymax," роки",sep=""))+
  theme (axis.title = element_blank(),axis.text.y = element_text(size=6))

mysave (vis)
vis<-vis+1


tsum2<-summarise (group_by(main.frame,Year,Chapter),
                  Total=sum(G1),
                  Female=sum (G4))
tsum2<-subset(tsum2,!is.na(Chapter))
tsum2$FemPart<-0
for (y in ymin:ymax)
  for (i in 1:20)
    tsum2$FemPart[tsum2$Year==y & tsum2$Chapter==chapter_ord[i]]<-
  tsum2$Female[tsum2$Year==y & tsum2$Chapter==chapter_ord[i]]/sum(tsum2$Female[tsum2$Year==y])*100


tsum2<-subset(tsum2,Female>0)

top2<-ggplot(tsum2, aes(x=factor(Chapter,levels = invert_chapter),
                        label=paste(Female," -",round(FemPart,2),"%"),y=FemPart))
top2+geom_bar(fill="green",stat = "identity")+geom_text(y=0,hjust=0,size=3)+
  facet_wrap(.~Year,ncol = 5,dir = "h")+coord_flip()+
  labs(title="Кількість засуджених жінок за розділами Особливої частини КК та
відсотковий розподіл засуджених жінок за розділами")+
  theme (axis.title = element_blank(),axis.text.y = element_text(size=6))

mysave (vis)
vis<-vis+1



gr2$Fempart<-round(gr2$Female/gr2$Total*100,1)              
top3<-ggplot(gr2, aes(x=Year,y=Fempart))
top3+geom_point()+geom_line()+
  facet_wrap(.~factor(Chapter,levels = chapter_ord),ncol = 5,dir = "h")+
  labs(title="Частка жінок серед засуджених осіб за розділами Особливої частини КК,
за роками у відсотках",
       subtitle = paste(ymin,"-",ymax," роки",sep=""))+scale_x_continuous(breaks = c(ymin:ymax))+
  theme (axis.title = element_blank(), axis.text.x = element_text (angle = 90),
         axis.text.y = element_text(size=6))

mysave (vis)
vis<-vis+1

gr2$Fempart<-round(gr2$Female/gr2$Total*100,1)              
top4<-ggplot(gr2, aes(x=Year,y=Female))
top4+geom_point()+geom_line()+
  facet_wrap(.~factor(Chapter,levels = chapter_ord),scales="free_y", ncol = 5,dir = "h")+
  labs(title="Кількість засуджених жінок за розділами Особливої частини КК, за роками",
       subtitle = paste(ymin,"-",ymax," роки",sep=""))+
  scale_x_continuous (breaks=c(ymin:ymax))+
  theme (axis.title = element_blank(), axis.text.x = element_text (angle = 90),
         axis.text.y = element_text(size=6))

mysave (vis)
vis<-vis+1


tsum<-summarise (group_by(main.frame,Chapter),
                 Total=sum(G1),
                 Female=sum (G4))
tsum$FemPart<-tsum$Female/tsum$Total*100
horizon_fem<-sum(tsum$Female)/sum(tsum$Total)*100
common_fem<-horizon_fem
tsum<-subset(tsum,!is.na(Chapter))
tot1<-ggplot(tsum,aes(x=factor(Chapter,levels = invert_chapter), y=FemPart, label=round(FemPart,1)))
tot1+geom_bar(stat="identity")+labs(y="%",title="Частка засуджених жінок серед усіх засуджених за розділами Особливої частини",
                                    subtitle=paste("за період з",ymin,"по", ymax, "рік"))+
  theme (axis.title.y=element_blank())+geom_label()+coord_flip()+
  geom_hline(aes(yintercept=horizon_fem), color="red", show.legend = FALSE)+
  geom_text (y=horizon_fem+1, hjust=0, x=11, color="red", label = paste("середній рівень - ",round(horizon_fem,1),"%",sep="") )

mysave(vis)
vis<-vis+1


