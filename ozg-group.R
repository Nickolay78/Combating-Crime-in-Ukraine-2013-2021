


gr2<-summarise(group_by(main.frame,Year,Chapter),
               Total=sum(G1),
               Group=sum(G6))

gr2<-subset(gr2,!is.na(Chapter))
gr2<-subset(gr2,Group>0)
gr2$Grpart<-round(gr2$Group/gr2$Total*100,2)              
top2<-ggplot(gr2, aes(x=factor(Chapter,levels = invert_chapter),
                      label=paste(round(Grpart,2),"% -",Group),
                      y=Grpart))
top2+geom_bar(fill="red",stat = "identity")+geom_text(y=0,hjust=0,size=3)+
  facet_wrap(.~Year,ncol = 5,dir = "h")+coord_flip()+
  labs(title="Частка засуджених за правопорушення, вчинені у складі організованої групи, 
серед всіх засуджених за правопорушення, передбачені певними розділами Особливої
частини, та кількість засуджениих за такі правопорушення за розділами",
       subtitle = paste(ymin,"-",ymax," роки",sep=""))+
  theme (axis.title = element_blank(),axis.text.y = element_text(size=6))

mysave (vis)
vis<-vis+1

#FEMALE AS GROUP G4 as G5

tsum2<-summarise (group_by(main.frame,Year,Chapter),
                  Total=sum(G1),
                  Female=sum (G6))
tsum2<-subset(tsum2,!is.na(Chapter))
tsum2<-subset(tsum2,Female>0)

tsum2$FemPart<-0
for (y in ymin:ymax)
  for (i in 1:20)
    tsum2$FemPart[tsum2$Year==y & tsum2$Chapter==chapter_ord[i]]<-
  tsum2$Female[tsum2$Year==y & tsum2$Chapter==chapter_ord[i]]/sum(tsum2$Female[tsum2$Year==y])*100



top2<-ggplot(tsum2, aes(x=factor(Chapter,levels = invert_chapter),
                        label=paste(Female,"-",round(FemPart,2),"%"),y=FemPart))
top2+geom_bar(fill="red",stat = "identity")+geom_text(y=0,hjust=0,size=3)+
  facet_wrap(.~Year,ncol = 5,dir = "h")+coord_flip()+
  labs(title="Кількість засуджених за правопорушення, вчинені у складі організованої групи,
за розділами Особливої частини КК та відсотковий розподіл засуджених 
за такі правопорушення за розділами",
       subtitle =paste(ymin,"-",ymax," роки",sep=""))+
  theme (axis.title = element_blank(),axis.text.y = element_text(size=6))

mysave (vis)
vis<-vis+1




gr2<-summarise(group_by(main.frame,Year,Chapter),
               Total=sum(G1),
               Female=sum(G6))#Here we change for group G5

gr2<-subset(gr2,!is.na(Chapter))
gr2$Fempart<-round(gr2$Female/gr2$Total*100,1)              


gr2$Fempart<-round(gr2$Female/gr2$Total*100,1)              
top3<-ggplot(gr2, aes(x=Year,y=Fempart))
top3+geom_point()+geom_line()+
  facet_wrap(.~factor(Chapter,levels = chapter_ord),ncol = 5,dir = "h")+
  labs(title="Частка кримінальних правопорушень, вчинених у складі організованої групи,
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
  labs(title="Кількість кримінальних правопорушень, вчинених у складі організованої групи,
за роками, за розділами Особливої частини КК",
       subtitle = paste(ymin,"-",ymax," роки",sep=""))+
  scale_x_continuous (breaks=c(ymin:ymax))+
  theme (axis.title = element_blank(), axis.text.x = element_text (angle = 90),
         axis.text.y = element_text(size=6))

mysave (vis)
vis<-vis+1



tsum<-summarise (group_by(main.frame,Chapter),
                 Total=sum(G1),
                 Female=sum (G6))

tsum<-subset(tsum,Female>0)
tsum$FemPart<-tsum$Female/tsum$Total*100
horizon_group<-sum(tsum$Female)/sum(tsum$Total)*100




tsum<-subset(tsum,!is.na(Chapter))
tot1<-ggplot(tsum,aes(x=factor(Chapter,levels = invert_chapter), y=FemPart, 
                      label=round(FemPart,2)))
tot1+geom_bar(stat="identity")+labs(y="%",title="Частка правопорушень, вчинених у складі організованої групи,
за розділами Особливої частини",
                                    subtitle=paste("за період з",ymin,"по", ymax, "рік"))+
  theme (axis.title.y=element_blank())+geom_label()+coord_flip()+
  geom_hline(aes(yintercept=horizon_group), alpha=0.5,color="red", show.legend = FALSE)+
  geom_text (y=horizon_group+1.5, hjust=0, x=10, 
             color="red", label = paste("середній рівень - ",round(horizon_group,2),"%",sep="") )

mysave (vis)
vis<-vis+1
