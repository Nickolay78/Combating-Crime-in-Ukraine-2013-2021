# Візуалізації 7-9


court_sub<-court_raw
court_sub<-summarise (group_by(court_sub,Year,Chapter,Article),
                      CONVIC=sum(CONVIC),
                      ACQUIT=sum(ACQUIT),
                      CMED=sum(CMED),
                      CCLOSE=sum(CCLOSE),
                      CSUM=sum(CONVIC,ACQUIT,CMED,CCLOSE))
court_sub<-court_sub[,c("Year","Chapter","Article","CONVIC","ACQUIT","CMED","CCLOSE")]
court_sub<-as.data.frame(court_sub)
court_sub<-reshape(court_sub,
                   timevar = "Comment", 
                   times=c("CONVIC","ACQUIT","CMED","CCLOSE"), 
                   v.names = "QNT", 
                   varying = c("CONVIC","ACQUIT","CMED","CCLOSE"), 
                   direction = "long")


court_sub$Comment[court_sub$Comment=="CONVIC"]<-"засуджено осіб"
court_sub$Comment[court_sub$Comment=="ACQUIT"]<-"виправдано осіб"
court_sub$Comment[court_sub$Comment=="CMED"]<-"неосудних осіб"
court_sub$Comment[court_sub$Comment=="CCLOSE"]<-"закрито справ"

court_sub[court_sub==0]<-NA
court_sub<-subset(court_sub,!is.na(court_sub$QNT))

mean_court<-summarise(group_by(court_sub,Comment,Year),
                      QNTS=sum(QNT))
court_sub<-mean_court
mean_court<-summarise(group_by(mean_court,Comment),
                      mean_v=round(mean(QNTS)))

court_sub$TOT<-0
for (i in ymin:ymax)
{court_sub$TOT[court_sub$Year==i]<-sum(court_sub$QNTS[court_sub$Year==i])}
  
  
  
top_3<-ggplot(court_sub, aes(x=Year,y=QNTS, label=paste(QNTS,"-",round(QNTS/TOT*100,1),"%" )  ))
top_3+geom_bar (stat="identity",aes(fill=Comment),show.legend = FALSE)+
  facet_wrap(.~factor (Comment, 
                       levels=c("засуджено осіб",
                                "виправдано осіб",
                                "неосудних осіб",
                                "закрито справ")),scales = "free_y")+ 
    labs(title = "Структура судових рішень", y="кількість")+
  theme(axis.title.x = element_blank())+
    geom_hline(data = mean_court, color="chartreuse", show.legend = FALSE,
             aes(yintercept=mean_v,
                 size=1,
                 alpha=1/2))+
  scale_x_continuous(breaks = ymin:ymax, labels = ymin:ymax)+
  geom_text(size=3,angle=90, position = position_stack(vjust = 0.5))+
  geom_text(data=mean_court,aes(x=2020,
            y=mean_v,
            label=mean_v))
  
  

mysave (vis)
vis<-vis+1





#Розподіл підстав закриття справ
court_sub<-court_raw
court_sub<-summarise (group_by(court_sub,Year),
                      CONFES=sum(CONFES),
                      RECONC=sum(RECONC),
                      CIRCUMS=sum(CIRCUMS),
                      SPONS=sum(SPONS),
                      AMNESTY=sum(AMNESTY),
                      DEATH=sum(DEATH),
                      COTHER=sum(COTHER),
                      CNOTCR=sum(CNOTCR),
                      CEDU=sum(CEDU),
                      SAMEVERD=sum(SAMEVERD),
                      DENOPR=sum(DENOPR),
                      TOT=sum(CONFES,RECONC,CIRCUMS,
                              SPONS,AMNESTY,DEATH,COTHER,
                              CNOTCR,CEDU,SAMEVERD,DENOPR,na.rm=TRUE))



court_sub<-as.data.frame(court_sub)
court_sub<-reshape(court_sub,
                   timevar = "Comment", 
                   times=c("CONFES","RECONC","CIRCUMS","SPONS","AMNESTY","DEATH","COTHER",
                           "CNOTCR","CEDU","SAMEVERD","DENOPR"), 
                   v.names = "QNT", 
                   varying = c("CONFES","RECONC","CIRCUMS","SPONS","AMNESTY","DEATH","COTHER",
                               "CNOTCR","CEDU","SAMEVERD","DENOPR"), 
                   direction = "long")


court_sub$Comment[court_sub$Comment=="CONFES"]<-"дійове каяття"
court_sub$Comment[court_sub$Comment=="RECONC"]<-"примирення винного з потерпілим"
court_sub$Comment[court_sub$Comment=="CIRCUMS"]<-"зміна обстановки"
court_sub$Comment[court_sub$Comment=="SPONS"]<-"передача на поруки"
court_sub$Comment[court_sub$Comment=="AMNESTY"]<-"амністія"
court_sub$Comment[court_sub$Comment=="DEATH"]<-"смерть"
court_sub$Comment[court_sub$Comment=="COTHER"]<-"інші підстави"
court_sub$Comment[court_sub$Comment=="CNOTCR"]<-"недоведеність обвинувачення 
(до 2017)"
court_sub$Comment[court_sub$Comment=="CEDU"]<-"примусові заходи виховного характеру
(до 2017)"
court_sub$Comment[court_sub$Comment=="SAMEVERD"]<-"рішення по ідентичному
обвинуваченню (з 2018)"
court_sub$Comment[court_sub$Comment=="DENOPR"]<-"відмова від обвинувачення 
(з 2018)"
court_sub[court_sub==0]<-NA
order_p<-c("дійове каяття",
           "примирення винного з потерпілим",
           "зміна обстановки",
           "передача на поруки",
           "амністія",
           "смерть",
           "недоведеність обвинувачення 
(до 2017)",
           "примусові заходи виховного характеру
(до 2017)",
           "рішення по ідентичному
обвинуваченню (з 2018)",
           "відмова від обвинувачення 
(з 2018)",
           "інші підстави")
court_sub<-subset(court_sub,!is.na(QNT))
top_4<-ggplot (court_sub,aes(x=factor(Comment,levels=order_p), y=QNT,
                  label=paste(QNT,"-",round(QNT/TOT*100,2),"%"), fill=Comment))
top_4+scale_fill_discrete(breaks=order_p)+
  geom_bar(stat="identity",)+facet_wrap(.~Year,ncol=3,dir="h")+
  geom_text(size=3,angle=90,hjust = 0, y = max(court_sub$QNT, na.rm = TRUE )/15)+
     theme (legend.position = "bottom",legend.title=element_blank(),
         axis.ticks.x=element_blank(),
          legend.text.align=0,legend.text=element_text(size = 7), axis.text.x=element_blank())+
    labs(title="Розподіл підстав закриття справ",x="підстави",y="кількість")
 
  

mysave (vis)
vis<-vis+1










