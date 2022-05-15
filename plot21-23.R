# Візуалізації 21-23
library (tidyverse)



#Застосовані покарання

court_sub<-court_raw

court_sub<-summarise (group_by(court_sub,Year),
                      LIFEIMP=sum(LIFEIMP),
                      IMP=sum(IMP),
                      IMP1=sum(IMP1),
                      IMP12=sum(IMP12),
                      IMP23=sum(IMP23),
                      IMP35=sum(IMP35),
                      IMP510=sum(IMP510),
                      IMP1015=sum(IMP1015),
                      IMP1525=sum(IMP1525),
                      RESTOL=sum(RESTOL),
                      DISBAT=sum(DISBAT),
                      ARREST=sum(ARREST),
                      CORRW=sum(CORRW),
                      SRVRSTR=sum(SRVRSTR),
                      PUBLW=sum(PUBLW),
                      FINE=sum(FINE),
                      DEPR=sum(DEPR),
                      TS=sum(LIFEIMP,IMP,RESTOL,DISBAT,
                             ARREST,CORRW,SRVRSTR,PUBLW,FINE,DEPR))







court_sub<-select(court_sub,Year:IMP,RESTOL:TS)
court_sub<-as.data.frame(court_sub)


court_sub<-reshape(court_sub,
                   timevar = "Comment", 
                   times=c("LIFEIMP","IMP","RESTOL","DISBAT", "ARREST", "CORRW","SRVRSTR","PUBLW","FINE","DEPR"), 
                   v.names = "QNT", 
                   varying = c("LIFEIMP","IMP","RESTOL","DISBAT", "ARREST", "CORRW","SRVRSTR","PUBLW","FINE","DEPR"), 
                   direction = "long")
punish_ord<-c(
  "довічне позбавлення волі",
  "позбавлення волі",
  "обмеження волі",			
  "тримання в дисциплінарному 
батальйоні",			
  "арешт",		
  "виправні роботи",			
  "службове обмеження для
військовослужбовців",			
  "громадські роботи",			
  "штраф",			
  "позбавлення права займати певні
посади або займатися певною
діяльністю")			




court_sub$Comment[court_sub$Comment=="LIFEIMP"]<-"довічне позбавлення волі"
court_sub$Comment[court_sub$Comment=="IMP"]<-"позбавлення волі"
court_sub$Comment[court_sub$Comment=="RESTOL"]<-"обмеження волі"
court_sub$Comment[court_sub$Comment=="DISBAT"]<-"тримання в дисциплінарному
батальйоні"
court_sub$Comment[court_sub$Comment=="ARREST"]<-"арешт"
court_sub$Comment[court_sub$Comment=="CORRW"]<-"виправні роботи"
court_sub$Comment[court_sub$Comment=="SRVRSTR"]<-"службове обмеження для
військовослужбовців"
court_sub$Comment[court_sub$Comment=="PUBLW"]<-"громадські роботи"
court_sub$Comment[court_sub$Comment=="FINE"]<-"штраф"
court_sub$Comment[court_sub$Comment=="DEPR"]<-"позбавлення права займати певні
посади або займатися певною
діяльністю"
court_sub[court_sub==0]<-NA


top_5<-ggplot (court_sub,aes(x=factor(Comment,levels=punish_ord), y=QNT,
  label=paste(QNT,"-",round(QNT/TS*100,1),"%" ), fill=Comment))
top_5+scale_fill_discrete(breaks=punish_ord)+
  geom_bar(stat="identity",)+facet_wrap(.~Year,ncol=3,dir="h")+
  geom_text(size=3,angle=90,hjust = 0, y = max(court_sub$QNT, na.rm = TRUE )/15)+
    theme (legend.position = "bottom",legend.title=element_blank(),axis.ticks.x = element_blank(),
         legend.text.align=0,legend.text=element_text(size = 7), axis.text.x=element_blank())+
  labs(title="Застосовані покарання",x="Види покарань",y="кількість")


mysave (vis)
vis<-vis+1
#Розподіл інтенсивності позбавлення волі

court_sub<-court_raw
court_sub<-summarise (group_by(court_sub,Year),
                      IMP1=sum(IMP1),
                      IMP12=sum(IMP12),
                      IMP23=sum(IMP23),
                      IMP35=sum(IMP35),
                      IMP510=sum(IMP510),
                      IMP1015=sum(IMP1015),
                      IMP1525=sum(IMP1525),
                      TS=sum(IMP))



court_sub<-as.data.frame(court_sub)


court_sub<-reshape(court_sub,
                   timevar = "Comment", 
                   times=c("IMP1","IMP12","IMP23","IMP35", "IMP510", "IMP1015","IMP1525"), 
                   v.names = "QNT", 
                   varying = c("IMP1","IMP12","IMP23","IMP35", "IMP510", "IMP1015","IMP1525"), 
                   direction = "long")
imp_ord<-c(
  "1 рік",	
  "понад 1 рік
до 2 років включно",	
  "понад 2 роки
до 3 років включно",	
  "понад 3 роки
до 5 років включно",	
  "понад 5 років
до 10 років включно",	
  "понад 10 років
до 15 років включно",	
  "понад 15 років
до 25 років включно")	

court_sub$Comment[court_sub$Comment=="IMP1"]<-"1 рік"
court_sub$Comment[court_sub$Comment=="IMP12"]<-"понад 1 рік
до 2 років включно"
court_sub$Comment[court_sub$Comment=="IMP23"]<-"понад 2 роки
до 3 років включно"
court_sub$Comment[court_sub$Comment=="IMP35"]<-"понад 3 роки
до 5 років включно"
court_sub$Comment[court_sub$Comment=="IMP510"]<-"понад 5 років
до 10 років включно"
court_sub$Comment[court_sub$Comment=="IMP1015"]<-"понад 10 років
до 15 років включно"
court_sub$Comment[court_sub$Comment=="IMP1525"]<-"понад 15 років
до 25 років включно"

court_sub[court_sub==0]<-NA

court_sub<-subset(court_sub,!is.na(QNT))



top_6<-ggplot (court_sub,aes(x=factor(Comment,levels=imp_ord), 
        label=paste(QNT,"-",round(QNT/TS*100,1),"%" ), y=QNT,fill=Comment))
top_6+scale_fill_discrete(breaks=imp_ord,type=c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02","#a6761d"))+
  geom_bar(stat="identity")+facet_wrap(.~Year,ncol=3,dir="h")+
    theme (legend.position = "bottom",axis.ticks.x=element_blank(), legend.title=element_blank(),
         legend.text=element_text(size = 8), axis.text.x=element_blank())+
  geom_text(size=3,angle=90,hjust = 0, y = max(court_sub$QNT, na.rm = TRUE )/15)+
  labs(title="Розподіл строків призначеного покарання у вигляді позбавлення волі",x="Строки",y="кількість")


mysave (vis)
vis<-vis+1




court_s<-data.frame()
y=1
list1<-list.files(path="DATA_COURT/") 
for (y in 1:length (list1))
{
  rr<-read.csv(paste("DATA_COURT/",list1[y],sep=""))
  if (y<=5) 
  {
    rr<-select  (rr,Year:CONVIC,LIFEIMP,IMP, RESTOL, PUBLW, FINE, PROB,RELAMN,RELOTHR)
    rrn<-names(rr)}
  else
  {
    rr<-select  (rr,Year:CONVIC,x3LIFEIMP,x3IMP, x3RESTOL,x3PUBLW, x3FINE, PROB,RELAMN,RELOTHR)
    names (rr)<-rrn}
  court_s<-rbind(court_s,rr)
} 

court_s$Chapter<-as.roman(court_s$Chapter)

court_s4<- summarise(group_by(court_s, Chapter), 
                     PUNISH=sum(CONVIC)-sum(PROB,RELAMN,RELOTHR),
                     REL=sum(PROB,RELAMN,RELOTHR),
                     CONVIC=sum(CONVIC),
                     IMP=sum(LIFEIMP,IMP),
                     
                     PUBLW=sum(PUBLW),
                     FINE=sum(FINE),
                     OTHR=sum(CONVIC)-sum(PROB,RELAMN,RELOTHR,PUBLW,LIFEIMP,IMP,FINE),
                     TS=sum(CONVIC))

court_s4<-subset(court_s4,!is.na(Chapter))
court_s4$Chapter_n<-as.numeric(court_s4$Chapter)
court_s4<-as.data.frame(court_s4)

court_s4<-reshape(court_s4,
                   timevar = "Comment", 
                   times=c("PUNISH","REL","IMP","FINE", "PUBLW", "OTHR"), 
                   v.names = "QNT", 
                   varying = c("PUNISH","REL","IMP","FINE", "PUBLW", "OTHR"), 
                   direction = "long")

bylo<-c("PUNISH","REL","IMP","FINE", "PUBLW", "OTHR","IMP1525")
stalo<-c("застосовано покарання",
"звільнено від покарання",
"позбавлення волі",
"штраф",
"громадські роботи",
"інше покарання")

for (i in 1:length(bylo))
{court_s4$Comment[court_s4$Comment==bylo[i]]<-stalo[i]}

s6_2<-court_s4


gr6_1<-ggplot(s6_2,aes(x=factor(Comment), y=QNT, 

 label=paste(QNT,"-",round(QNT/TS*100,1),"%"  ), fill=Comment))
gr6_1+geom_bar(stat="identity")+
  facet_wrap(ncol=5,
             .~factor(as.character(Chapter),
                      levels=chapter_levels),
             scales="free_y")+
  geom_text(size=3,angle=90,hjust = 0,y=1)+
   scale_fill_discrete(breaks=c("застосовано покарання",
                               "звільнено від покарання",
                               "позбавлення волі",
                               "штраф",
                               "громадські роботи",
                               "інше покарання"),
                       type = c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33"))+
  scale_x_discrete(limits=c("застосовано покарання",
                            "звільнено від покарання",
                            "позбавлення волі",
                            "штраф",
                            "громадські роботи",
                            "інше покарання"))+
  theme (axis.title = element_blank(),
         legend.title=element_blank(),
         axis.text = element_blank(),axis.ticks= element_blank(),
         legend.position = "right")+
  labs(title = "Розподіл форм реалізації кримінальної відповідальності та застосованих покарань
за розділами Особливої частини КК з вказівкою частки серед всіх засуджених осіб",
       subtitle = paste("загальна кількість за період з ",ymin," по ",ymax," рік",sep=""))


mysave (vis)
vis<-vis+1

#COMMON MODEL
#Stat model collect data


library (stringi)
stat.model<-pgo_raw


stat.model<-summarise(stat.model,
                      SACC=sum(ACC),
                      TO_COURT=sum(INDICM,REL,MED,EDU),
                      RATIO1=TO_COURT/SACC
                      
)

stat.modelC<-court_raw

stat.modelC<-summarise(stat.modelC,
                       CTOT=sum(CRTOT),
                       ZASUD=sum(CONVIC),
                       RATIO2=ZASUD/CTOT,
                       ZVILN=sum(PROB,RELAMN,RELOTHR),
                       REAL=ZASUD-ZVILN,
                       RATIO3=REAL/ZASUD,
                       SIMP=sum(LIFEIMP,IMP),
                       SFINE=sum(FINE),
                       SPUBW=sum(PUBLW),
                       SRESTOL=sum(RESTOL),
                       SARREST=sum(ARREST),
                       SOTHR=REAL-SIMP-SFINE-SPUBW-SRESTOL-SARREST,
                       
                       PIMP=SIMP/REAL,
                       PFINE=SFINE/REAL,
                       PPUBW=SPUBW/REAL,
                       PRESTOL=SRESTOL/REAL,
                       PARREST=SARREST/REAL,
                       POTHR=SOTHR/REAL
)

total_a2<-cbind(stat.model,stat.modelC)



total_a2$M_TOCOURT<-100*total_a2$RATIO1
total_a2$M_SUD<-total_a2$CTOT/total_a2$SACC*100

total_a2$M_ZASUD<-total_a2$ZASUD/total_a2$SACC*100
total_a2$M_REAL<-total_a2$REAL/total_a2$SACC*100
total_a2$M_IMP<-total_a2$SIMP/total_a2$SACC*100
total_a2$M_FINE<-total_a2$SFINE/total_a2$SACC*100
total_a2$M_PUBW<-total_a2$SPUBW/total_a2$SACC*100
total_a2$M_RESTOL<-total_a2$SRESTOL/total_a2$SACC*100
total_a2$M_ARREST<-total_a2$SARREST/total_a2$SACC*100
total_a2$M_OTHR<-total_a2$SOTHR/total_a2$SACC*100

total_a2$M_ACC<-NA
total_a2$M_ACC[total_a2$SACC>0]<-100

smodel2<-select(total_a2,SACC, TO_COURT,CTOT,ZASUD,REAL,SIMP,SFINE,SPUBW,SRESTOL,SARREST,SOTHR)

bylo<-c("SACC","TO_COURT","CTOT", "ZASUD",
        "REAL","SIMP",
        "SFINE","SPUBW",
        "SRESTOL","SARREST",
        "SOTHR")

stalo<-c(
  "обліковано проваджень",
  "направлено до суду",
  "осіб, щодо яких набрали
  чинності судові рішення",
  "визнано винними",
  "призначено реальні
покарання",
  "позбавлення волі",
  "штраф",
  "громадські роботи",
  "обмеження волі",
  "арешт",
  "інше покарання")

gr2mQ<-reshape(smodel2,
               timevar = "Comment", 
               times=bylo, 
               v.names = "QNT2", 
               varying = bylo, 
               direction = "long")

for (i in 1:length(bylo))
{gr2mQ$Comment[gr2mQ$Comment==bylo[i]]<-stalo[i]}




smodel<-select(total_a2,
               M_ACC,M_TOCOURT,M_SUD,M_ZASUD,M_REAL,M_IMP,M_FINE,M_PUBW,M_RESTOL,M_ARREST,M_OTHR)


bylo<-c("M_ACC","M_TOCOURT","M_SUD", "M_ZASUD",
        "M_REAL","M_IMP",
        "M_FINE","M_PUBW",
        "M_RESTOL","M_ARREST",
        "M_OTHR")

stalo_m<-stalo
gr2m<-reshape(smodel,
              timevar = "Comment", 
              times=bylo, 
              v.names = "QNT", 
              varying = bylo, 
              direction = "long")


for (i in 1:length(bylo))
{gr2m$Comment[gr2m$Comment==bylo[i]]<-stalo[i]}




gr2m3<-merge(gr2m,gr2mQ,by=c("Comment"))
gr2m<-gr2m3
gr2m<-as.data.frame (gr2m)





gr2m1<-gr2m
gr2m1<-subset(gr2m1,!is.na(QNT))

  mod<-ggplot(gr2m1,aes(x=factor(Comment,levels=stalo_m), fill=Comment, y=QNT, 
                        label=paste(round (QNT,2),"% -", QNT2)))
  mod+geom_bar(stat="identity")+
    geom_text(size=3,angle=90,hjust = 0, y = 15)+
    scale_fill_discrete(breaks=stalo_m)+
    theme(axis.title=element_blank(),axis.text = element_blank(),
          legend.title = element_blank(),axis.ticks = element_blank())+
    labs (title = paste(
"Відношення основних показників кримінально-правового регулювання 
до кількості облікованих проваджень за період з",ymin,"по",ymax,"роки" ) )
  
  mysave (vis)
  vis<-vis+1

###BY YEAR
  
  
  
  stat.model<-pgo_raw
  
  
  stat.model<-summarise(group_by(stat.model,Year),
                        SACC=sum(ACC),
                        TO_COURT=sum(INDICM,REL,MED,EDU),
                        RATIO1=TO_COURT/SACC
                        
  )
  
  stat.modelC<-court_raw
  
  stat.modelC<-summarise(group_by(stat.modelC,Year),
                         CTOT=sum(CRTOT),
                         ZASUD=sum(CONVIC),
                         RATIO2=ZASUD/CTOT,
                         ZVILN=sum(PROB,RELAMN,RELOTHR),
                         REAL=ZASUD-ZVILN,
                         RATIO3=REAL/ZASUD,
                         SIMP=sum(LIFEIMP,IMP),
                         SFINE=sum(FINE),
                         SPUBW=sum(PUBLW),
                         SRESTOL=sum(RESTOL),
                         SARREST=sum(ARREST),
                         SOTHR=REAL-SIMP-SFINE-SPUBW-SRESTOL-SARREST,
                         
                         PIMP=SIMP/REAL,
                         PFINE=SFINE/REAL,
                         PPUBW=SPUBW/REAL,
                         PRESTOL=SRESTOL/REAL,
                         PARREST=SARREST/REAL,
                         POTHR=SOTHR/REAL
  )
  
  total_a2<-merge(stat.model,stat.modelC,by="Year")
  
  
  
  total_a2$M_TOCOURT<-100*total_a2$RATIO1
  total_a2$M_SUD<-total_a2$CTOT/total_a2$SACC*100
  
  total_a2$M_ZASUD<-total_a2$ZASUD/total_a2$SACC*100
  total_a2$M_REAL<-total_a2$REAL/total_a2$SACC*100
  total_a2$M_IMP<-total_a2$SIMP/total_a2$SACC*100
  total_a2$M_FINE<-total_a2$SFINE/total_a2$SACC*100
  total_a2$M_PUBW<-total_a2$SPUBW/total_a2$SACC*100
  total_a2$M_RESTOL<-total_a2$SRESTOL/total_a2$SACC*100
  total_a2$M_ARREST<-total_a2$SARREST/total_a2$SACC*100
  total_a2$M_OTHR<-total_a2$SOTHR/total_a2$SACC*100
  
  total_a2$M_ACC<-NA
  total_a2$M_ACC[total_a2$SACC>0]<-100
  
  smodel2<-select(total_a2,Year,SACC, TO_COURT,CTOT,ZASUD,REAL,SIMP,SFINE,SPUBW,SRESTOL,SARREST,SOTHR)
  
  bylo<-c("SACC","TO_COURT","CTOT", "ZASUD",
          "REAL","SIMP",
          "SFINE","SPUBW",
          "SRESTOL","SARREST",
          "SOTHR")
  
  stalo<-c(
    "обліковано проваджень",
    "направлено до суду",
    "осіб, щодо яких набрали
чинності судові рішення",
    "визнано винними",
    "призначено реальні
покарання",
    "позбавлення волі",
    "штраф",
    "громадські роботи",
    "обмеження волі",
    "арешт",
    "інше покарання")
  
  gr2mQ<-reshape(smodel2,
                 timevar = "Comment", 
                 times=bylo, 
                 v.names = "QNT2", 
                 varying = bylo, 
                 direction = "long")
  
  for (i in 1:length(bylo))
  {gr2mQ$Comment[gr2mQ$Comment==bylo[i]]<-stalo[i]}
  
  
  
  
  smodel<-select(total_a2,Year,
                 M_ACC,M_TOCOURT,M_SUD,M_ZASUD,M_REAL,M_IMP,M_FINE,M_PUBW,M_RESTOL,M_ARREST,M_OTHR)
  
  
  bylo<-c("M_ACC","M_TOCOURT","M_SUD", "M_ZASUD",
          "M_REAL","M_IMP",
          "M_FINE","M_PUBW",
          "M_RESTOL","M_ARREST",
          "M_OTHR")
  
  stalo_m<-stalo
  gr2m<-reshape(smodel,
                timevar = "Comment", 
                times=bylo, 
                v.names = "QNT", 
                varying = bylo, 
                direction = "long")
  
  
  for (i in 1:length(bylo))
  {gr2m$Comment[gr2m$Comment==bylo[i]]<-stalo[i]}
  
  
  
  
  gr2m3<-merge(gr2m,gr2mQ,by=c("Year", "Comment"))
  gr2m<-gr2m3
  gr2m<-as.data.frame (gr2m)
  
  
  
  
  
  gr2m1<-gr2m
  gr2m1<-subset(gr2m1,!is.na(QNT))
  
  mod<-ggplot(gr2m1,aes(x=factor(Comment,levels=stalo_m), fill=Comment, y=QNT, 
                        label=paste(round (QNT,2),"% -", QNT2)))
  mod+geom_bar(stat="identity")+facet_wrap(.~factor(Year),ncol=3)+
    geom_text(size=3,angle=90,hjust = 0, y = 15)+
    scale_fill_discrete(breaks=stalo_m)+
    theme(axis.title=element_blank(),axis.text = element_blank(),
          legend.title = element_blank(),axis.ticks = element_blank())+
    labs (title = paste
("Відношення основних показників кримінально-правового регулювання 
до кількості облікованих проваджень за роками
за період з",ymin,"по",ymax,"роки" ) )
  
  mysave (vis)
  vis<-vis+1
  
  
  
  ###BY CHAPTER
  
  
  
  stat.model<-pgo_raw
  
  
  stat.model<-summarise(group_by(stat.model,Chapter),
                        SACC=sum(ACC),
                        TO_COURT=sum(INDICM,REL,MED,EDU),
                        RATIO1=TO_COURT/SACC
                        
  )
  
  stat.modelC<-court_raw
  
  stat.modelC<-summarise(group_by(stat.modelC,Chapter),
                         CTOT=sum(CRTOT),
                         ZASUD=sum(CONVIC),
                         RATIO2=ZASUD/CTOT,
                         ZVILN=sum(PROB,RELAMN,RELOTHR),
                         REAL=ZASUD-ZVILN,
                         RATIO3=REAL/ZASUD,
                         SIMP=sum(LIFEIMP,IMP),
                         SFINE=sum(FINE),
                         SPUBW=sum(PUBLW),
                         SRESTOL=sum(RESTOL),
                         SARREST=sum(ARREST),
                         SOTHR=REAL-SIMP-SFINE-SPUBW-SRESTOL-SARREST,
                         
                         PIMP=SIMP/REAL,
                         PFINE=SFINE/REAL,
                         PPUBW=SPUBW/REAL,
                         PRESTOL=SRESTOL/REAL,
                         PARREST=SARREST/REAL,
                         POTHR=SOTHR/REAL
  )
  
  total_a2<-merge(stat.model,stat.modelC,by="Chapter")
  
  
  
  total_a2$M_TOCOURT<-100*total_a2$RATIO1
  total_a2$M_SUD<-total_a2$CTOT/total_a2$SACC*100
  
  total_a2$M_ZASUD<-total_a2$ZASUD/total_a2$SACC*100
  total_a2$M_REAL<-total_a2$REAL/total_a2$SACC*100
  total_a2$M_IMP<-total_a2$SIMP/total_a2$SACC*100
  total_a2$M_FINE<-total_a2$SFINE/total_a2$SACC*100
  total_a2$M_PUBW<-total_a2$SPUBW/total_a2$SACC*100
  total_a2$M_RESTOL<-total_a2$SRESTOL/total_a2$SACC*100
  total_a2$M_ARREST<-total_a2$SARREST/total_a2$SACC*100
  total_a2$M_OTHR<-total_a2$SOTHR/total_a2$SACC*100
  
  total_a2$M_ACC<-NA
  total_a2$M_ACC[total_a2$SACC>0]<-100
  
  smodel2<-select(total_a2,Chapter,SACC, TO_COURT,CTOT,ZASUD,REAL,SIMP,SFINE,SPUBW,SRESTOL,SARREST,SOTHR)
  
  bylo<-c("SACC","TO_COURT","CTOT", "ZASUD",
          "REAL","SIMP",
          "SFINE","SPUBW",
          "SRESTOL","SARREST",
          "SOTHR")
  
  stalo<-c(
    "обліковано проваджень",
    "направлено до суду",
    "осіб, щодо яких набрали
чинності судові рішення",
    "визнано винними",
    "призначено реальні
покарання",
    "позбавлення волі",
    "штраф",
    "громадські роботи",
    "обмеження волі",
    "арешт",
    "інше покарання")
  
  gr2mQ<-reshape(smodel2,
                 timevar = "Comment", 
                 times=bylo, 
                 v.names = "QNT2", 
                 varying = bylo, 
                 direction = "long")
  
  for (i in 1:length(bylo))
  {gr2mQ$Comment[gr2mQ$Comment==bylo[i]]<-stalo[i]}
  
  
  
  
  smodel<-select(total_a2,Chapter,
                 M_ACC,M_TOCOURT,M_SUD,M_ZASUD,M_REAL,M_IMP,M_FINE,M_PUBW,M_RESTOL,M_ARREST,M_OTHR)
  
  
  bylo<-c("M_ACC","M_TOCOURT","M_SUD", "M_ZASUD",
          "M_REAL","M_IMP",
          "M_FINE","M_PUBW",
          "M_RESTOL","M_ARREST",
          "M_OTHR")
  
  stalo_m<-stalo
  gr2m<-reshape(smodel,
                timevar = "Comment", 
                times=bylo, 
                v.names = "QNT", 
                varying = bylo, 
                direction = "long")
  
  
  for (i in 1:length(bylo))
  {gr2m$Comment[gr2m$Comment==bylo[i]]<-stalo[i]}
  
  
  
  
  gr2m3<-merge(gr2m,gr2mQ,by=c("Chapter", "Comment"))
  gr2m<-gr2m3
  gr2m<-as.data.frame (gr2m)
  
  
  
  
  
  gr2m1<-gr2m
  gr2m1<-subset(gr2m1,!is.na(QNT))
  
  mod<-ggplot(gr2m1,aes(x=factor(Comment,levels=stalo_m), fill=Comment, y=QNT, 
                        label=paste(round (QNT,2),"% -", QNT2)))
  mod+geom_bar(stat="identity")+
    facet_wrap(.~factor(Chapter,levels = chapter_ord),ncol=4)+
    geom_text(size=2,angle=90,hjust = 0, y = 3)+
    scale_fill_discrete(breaks=stalo_m)+
    theme(axis.title=element_blank(),
          axis.text = element_blank(),legend.position = "right",
          legend.title = element_blank(),axis.ticks = element_blank())+
    labs (title = paste(
"Відношення основних показників кримінально-правового регулювання 
до кількості облікованих проваджень
за розділами Особливої частини КК, за період з",ymin,"по",ymax,"роки" ) )
  
  mysave (vis)
  vis<-vis+1
  
  