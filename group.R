art28<-select (main.frame,1:5,9:11)
art28abs<-select (main.frame,1:5,9:11)

art28g1<-summarise(group_by(art28abs,Year,Chapter),
                   tsum=sum(G1),
                   group=sum(G5),
                   ozg=sum(G6),
                   crsoc=sum(G7))
art28g1<-subset(art28g1,!is.na(Chapter))

art28g2<-summarise(group_by(art28g1, Year),
                   group=sum(group),
                   ozg=sum(ozg),
                   crsoc=sum(crsoc))

art28g2<-as.data.frame(art28g2)                   
art28g2<-reshape(art28g2,
                 timevar = "Comment", 
                 times=c("group","ozg","crsoc"), 
                 v.names = "QNT", 
                 varying = c("group","ozg","crsoc"), 
                 direction = "long")

bylo<-c("group","ozg","crsoc")
stalo<-c("у складі групи","у складі організованої групи","у складі злочинної організації")
for (i in 1:length(bylo))
{art28g2$Comment[art28g2$Comment==bylo[i]]<-stalo[i]}


top_1<-ggplot (art28g2,aes(x=Year,label=QNT, y=QNT))
top_1+geom_bar(aes(fill=Comment),stat = "identity",show.legend = FALSE)+
  facet_wrap(ncol=1,scales="free_x",.~factor(Comment, levels=stalo))+
  labs (title = "Вчинення кримінальних правопорушень у співучасті")+
  geom_text(size=4,position = position_stack(vjust = 0.5))+
  theme (axis.ticks.x=element_blank(),
         axis.text.x=element_blank(),axis.title = element_blank())+
  scale_y_continuous(labels = point)+
  scale_x_continuous (breaks=c(ymin:ymax))+coord_flip()   
vis<-60
mysave (vis)
vis<-vis+1



gr2<-summarise(group_by(main.frame,Year,Chapter),
               Total=sum(G1),
               Group=sum(G5))

gr2<-subset(gr2,!is.na(Chapter))
gr2$Grpart<-round(gr2$Group/gr2$Total*100,1)              
top2<-ggplot(gr2, aes(x=factor(Chapter,levels = invert_chapter),
                      label=paste(round(Grpart,1),"% -",Group),
                      y=Grpart))
top2+geom_bar(fill="red",stat = "identity")+geom_text(y=0,hjust=0,size=3)+
  facet_wrap(.~Year,ncol = 5,dir = "h")+coord_flip()+
  labs(title=
         "Частка засуджених за правопорушення, вчинені у складі груп, 
серед всіх засуджених за правопорушення, передбачені певними розділами Особливої
частини, та кількість засуджениих за такі правопорушення за розділами",
       subtitle = paste(ymin,"-",ymax," роки",sep=""))+
  theme (axis.title = element_blank(),axis.text.y = element_text(size=6))

mysave (vis)
vis<-vis+1

#FEMALE AS GROUP G4 as G5

tsum2<-summarise (group_by(main.frame,Year,Chapter),
                  Total=sum(G1),
                  Female=sum (G5))
tsum2<-subset(tsum2,!is.na(Chapter))
tsum2$FemPart<-0
for (y in ymin:ymax)
  for (i in 1:20)
    tsum2$FemPart[tsum2$Year==y & tsum2$Chapter==chapter_ord[i]]<-
  tsum2$Female[tsum2$Year==y & tsum2$Chapter==chapter_ord[i]]/sum(tsum2$Female[tsum2$Year==y])*100

top2<-ggplot(tsum2, aes(x=factor(Chapter,levels = invert_chapter),
                        label=paste(Female,"-",round(FemPart,1),"%"),
                        y=FemPart))
top2+geom_bar(fill="red",stat = "identity")+geom_text(y=0,hjust=0,size=3)+
  facet_wrap(.~Year,ncol = 5,dir = "h")+coord_flip()+
  labs(title=
         "Кількість засуджених за правопорушення, вчинені у складі груп, за розділами 
Особливої частини КК та відсотковий розподіл засуджених 
за такі правопорушення за розділами",
       subtitle =paste(ymin,"-",ymax," роки",sep=""))+
  theme (axis.title = element_blank(),axis.text.y = element_text(size=6))

mysave (vis)
vis<-vis+1




gr2<-summarise(group_by(main.frame,Year,Chapter),
               Total=sum(G1),
               Female=sum(G5))#Here we change for group G5

gr2<-subset(gr2,!is.na(Chapter))
gr2$Fempart<-round(gr2$Female/gr2$Total*100,1)              




gr2$Fempart<-round(gr2$Female/gr2$Total*100,1)              
top3<-ggplot(gr2, aes(x=Year,y=Fempart))
top3+geom_point()+geom_line()+
  facet_wrap(.~factor(Chapter,levels = chapter_ord),ncol = 5,dir = "h")+
  labs(title="Частка кримінальних правопорушень, вчинених у складі груп,
за роками, за розділами Особливої частини КК у відсотках",
       subtitle = paste(ymin,"-",ymax," роки",sep=""))+scale_x_continuous(breaks = c(ymin:ymax))+
  theme (axis.title = element_blank(), axis.text.x = element_text (angle = 90),
         axis.text.y = element_text(size=6))

mysave (vis)
vis<-vis+1

gr2$Fempart<-round(gr2$Female/gr2$Total*100,1)              
top4<-ggplot(gr2, aes(x=Year,y=Female))
top4+geom_point()+geom_line()+
  facet_wrap(.~factor(Chapter,levels = chapter_ord),scales="free_y", ncol = 5,dir = "h")+
  labs(title="Кількість кримінальних правопорушень, вчинених у складі груп,
за роками, за розділами Особливої частини КК",
       subtitle = paste(ymin,"-",ymax," роки",sep=""))+
  scale_x_continuous (breaks=c(ymin:ymax))+
  theme (axis.title = element_blank(), axis.text.x = element_text (angle = 90),
         axis.text.y = element_text(size=6))

mysave (vis)
vis<-vis+1



tsum<-summarise (group_by(main.frame,Chapter),
                 Total=sum(G1),
                 Female=sum (G5))
tsum$FemPart<-tsum$Female/tsum$Total*100
horizon_group<-sum(tsum$Female)/sum(tsum$Total)*100




tsum<-subset(tsum,!is.na(Chapter))
tot1<-ggplot(tsum,aes(x=factor(Chapter,levels = invert_chapter), y=FemPart, label=round(FemPart,1)))
tot1+geom_bar(stat="identity")+labs(y="%",title="Частка правопорушень, вчинених у складі груп,
за розділами Особливої частини",
                                    subtitle=paste("за період з",ymin,"по", ymax, "рік"))+
  theme (axis.title.y=element_blank())+geom_label()+coord_flip()+
  geom_hline(aes(yintercept=horizon_group), color="red", show.legend = FALSE)+
  geom_text (y=horizon_group+1, hjust=0, x=12, color="red", label = paste("середній рівень - ",round(horizon_group,1),"%",sep="") )

mysave (vis)
vis<-vis+1
