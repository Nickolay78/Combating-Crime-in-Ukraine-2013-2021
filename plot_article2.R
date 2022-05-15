nikoly<-0
#Stat model collect data

top_art_names<-result_art$header
top_art_numbers<-result_art$short_n
names_numbers<-data.frame(names=top_art_names, numbers=top_art_numbers)


library (stringi)

stat.model<-pgo_raw

for (i in 1:nrow(stat.model))
{if (stri_detect_fixed (stat.model$Article[i], "*"))
  stat.model$Article[i]<-gsub('.$', '',stat.model$Article[i])}

stat.model<-subset(stat.model,!str_detect(Article,"Others"))
stat.model$Article<-sub("Article", "",stat.model$Article)
stat.model$Article<-gsub(" ","",stat.model$Article)

stat.model<-summarise(group_by(stat.model,Chapter,Article),
                      SACC=sum(ACC),
                      TO_COURT=sum(INDICM,REL,MED,EDU),
                      RATIO1=TO_COURT/SACC
                      
)

stat.modelC<-court_raw

stat.modelC<-subset(stat.modelC,!is.na(Chapter))
stat.modelC$Article<-sub("Article", "",stat.modelC$Article)
stat.modelC$Article<-gsub(" ","",stat.modelC$Article)

stat.modelC<-summarise(group_by(stat.modelC,Chapter,Article),
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
name2<-data.frame(nazva=names_numbers$names,
                  Article=names_numbers$numbers)

#setdiff(stat.model$Article,names_numbers$numbers)

total_a<-merge(stat.model,stat.modelC,by=c("Chapter", "Article"),all = TRUE)
total_a2<-merge(name2,total_a,by="Article",all = TRUE)



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

smodel2<-select(total_a2,Article,Chapter,
               SACC, TO_COURT,CTOT,ZASUD,REAL,SIMP,SFINE,SPUBW,SRESTOL,SARREST,SOTHR)

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




smodel<-select(total_a2,Article,Chapter,nazva,
               M_ACC,M_TOCOURT,M_SUD, M_ZASUD,M_REAL,M_IMP,M_FINE,M_PUBW,M_RESTOL,M_ARREST,M_OTHR)


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




gr2m3<-merge(gr2m,gr2mQ,by=c("Article","Comment"))
gr2m<-gr2m3
gr2m<-as.data.frame (gr2m)

#Stat model collected

frame2<-main.frame

frame2$Article<-sub("Article ","",frame2$Article)
frame2$Article<-gsub(" ","",frame2$Article)

top_art_names<-result_art$header
top_art_numbers<-result_art$short_n
top_all<-data.frame(Article=top_art_numbers)

names_numbers<-data.frame(names=top_art_names, numbers=top_art_numbers)




top<-summarise(group_by(court_raw,Article),
               CONVIC_T=sum(CONVIC),
               prob_art=0,
               round_p=0)
top$prob_art<-(top$CONVIC_T/sum(top$CONVIC_T))*100
top$round_p<-round(top$prob_art,2)



top_a<-arrange(subset(top, prob_art > quantile (prob_art, prob = 0.945)), desc (prob_art))

top_a$Article<-sub("Article ","",top_a$Article)
scale_vector_x<-top_a$Article

top_g<-ggplot(top_a,aes(x=factor(Article, levels = scale_vector_x),
                        y=CONVIC_T, label=CONVIC_T))
top_g+geom_bar(aes(fill=Article), show.legend = FALSE, stat = "identity")+
  labs (x="Стаття Особливої частини КК", y="кількість засуджених з 2013 по 2020 рік",
        title=paste("Кримінальні правопорушення, за вчинення яких засуджено", round ((sum(top_a$prob_art)/sum(top$prob_art))*100), "% осіб,
визнаних винними протягом",ymin,"-",ymax,"років"))+
  scale_y_continuous(labels = point)+
  geom_text(angle=90, hjust=0, y=max(top_a$CONVIC_T)/15)


vis<-0
mysave (vis, suf="st_0")





top_art_names<-result_art$header
top_art_numbers<-result_art$short_n
top_a<-data.frame(Article=top_art_numbers)

#залишити у векторі ті статті які вже візуалізовано в топі

#top_all<-data.frame(Article=top_art_numbers)
#orig<-top_all$Article
#subs<-top_a$Article
#orig<-orig[!orig %in% subs]

#top_a$Article<-orig


pgo_raw2<-pgo_raw
pgo_raw2$Article<-sub("Article ","",pgo_raw2$Article)
pgo_raw2$Article<-gsub(" ","",pgo_raw2$Article)
court_raw$Article<-sub("Article ","",court_raw$Article)
court_raw$Article<-gsub(" ","",court_raw$Article)

for (i in 1:length(top_a$Article))
{
vis<-1  
  print (paste("Working... ",i,"/",length (top_a$Article)))

  
  art_top<-gsub(" ","",top_a$Article[i])

  mark<-ggplot()
  mark+geom_text(aes(x=5,y=5),label=names_numbers$names[names_numbers$numbers==art_top])+
    theme (axis.title = element_blank(),
           axis.text = element_blank(),
           axis.ticks = element_blank())
  
  n_suf<-paste("st_",i,"(",art_top,")",sep="")
    mysave (vis,n_suf)
  vis<-vis+1
  
  
  gr2m1<-subset(gr2m,Article==art_top)
  
  gr2m1<-subset(gr2m1,QNT>0)
  gr2m1<-subset(gr2m1,!is.na(QNT))
  if (nrow(gr2m1)>0)
  {
    mod<-ggplot(gr2m1,aes(x=factor(Comment,levels=stalo_m), fill=Comment, y=QNT, 
                        label=paste(round (QNT,2),"% -", QNT2)))
  mod+geom_bar(stat="identity")+
    geom_text(size=3,angle=90,hjust = 0, y = 15)+
    scale_fill_discrete(breaks=stalo_m)+
    theme(axis.title=element_blank(),axis.text = element_blank(),
          legend.title = element_blank(),axis.ticks = element_blank())+
  labs (title = paste(
"Відношення основних показників кримінально-правового регулювання 
до кількості облікованих проваджень за період з",ymin,"по",ymax,"роки. " ),
        subtitle=paste (names_numbers$names[names_numbers$numbers==art_top]))
  
    mysave (vis,n_suf)
  vis<-vis+1
  } else vis<-vis+1
  
  
  
  
  pgo_sub<-subset (pgo_raw2,Article==art_top)
  #pgo_sub$Article<-art_top
  pgo_sub$TO_COURT<-apply (pgo_sub[,6:9],1,sum)
  pgo_sub1<-select(pgo_sub,"Year","Chapter","Article","ACC","TO_COURT")
  
  court_sub<-subset (court_raw,Article==art_top)
  
  
  
  court_sub_art<-summarise (group_by(court_sub,Year,Chapter,Article),
                            CRTOT=sum(CRTOT),
                            CONVIC=sum(CONVIC))
  court_sub_art$Chapter<-as.roman(court_sub_art$Chapter)
  pgo_sub1<-merge (pgo_sub1,court_sub_art)
  pgo_sub1<-pgo_sub1[,c("Year","Chapter","Article","ACC","TO_COURT","CONVIC")]

  if (nrow(pgo_sub1)>0)
    {
    pgo_sub1<-reshape(pgo_sub1,
          timevar = "Comment", 
          times=c("ACC","TO_COURT","CONVIC"), 
          v.names = "QNT", 
          varying = c("ACC","TO_COURT","CONVIC"), 
          direction = "long")
  
    
    
    if (sum(pgo_sub1$QNT[pgo_sub1$Comment=="ACC"])>0)
      {
  pgo_sub1$Comment[pgo_sub1$Comment=="ACC"]<-"обліковано проваджень"
  pgo_sub1$Comment[pgo_sub1$Comment=="TO_COURT"]<-"передано матеріалів до суду"
  pgo_sub1$Comment[pgo_sub1$Comment=="CONVIC"]<-"засуджено осіб"
  
  
  
  


  
  top_1<-ggplot (pgo_sub1,aes(x=as.factor(Year),y=QNT, label = QNT))
  top_1+geom_bar(aes(fill=Comment),stat = "identity",show.legend = FALSE)+
    facet_grid(.~factor(Comment, levels=c("обліковано проваджень",
                                          "передано матеріалів до суду",
                                        "засуджено осіб")))+
    labs (y="кількість", title = "Кількість облікованих проваджень, переданих до суду матеріалів та засуджених осіб",
          subtitle=paste (names_numbers$names[names_numbers$numbers==art_top]))+
    theme (axis.title.x = element_blank(), axis.text.x = element_text(angle = 90))+
    scale_y_continuous(labels = point)+
    geom_text (size=3,angle=90, position = position_stack(vjust = 0.5))
  
          
  mysave (vis,n_suf)
  vis<-vis+1}  
  } else vis<-vis+1 

   pgo_sub2<-select(pgo_sub,"Year","Article","Chapter","INDICM","REL","MED","EDU")
    
if (nrow(pgo_sub2)>0)
        {pgo_sub2<-reshape(pgo_sub2,
          timevar = "Comment", 
          times=c("INDICM","REL","MED","EDU"), 
          v.names = "QNT", 
          varying = c("INDICM","REL","MED","EDU"), 
          direction = "long",new.row.names = 1:((ymax-ymin+1)*4))
  pgo_sub2$Comment[pgo_sub2$Comment=="INDICM"]<-"з обвинувальним актом"
  pgo_sub2$Comment[pgo_sub2$Comment=="REL"]<-"клопотання про звільнення"
  pgo_sub2$Comment[pgo_sub2$Comment=="MED"]<-"примусові заходи
медичного характеру"
  pgo_sub2$Comment[pgo_sub2$Comment=="EDU"]<-"примусові заходи
виховного характеру"
  
  pgo_sub2[pgo_sub2==0]<-NA
pgo_sub2<-subset(pgo_sub2,!is.na(pgo_sub2$QNT))}

if (nrow(pgo_sub2)>0)
{
    top_2<-ggplot(pgo_sub2, aes(x=as.factor(Year),y=QNT, label=QNT))
  top_2+geom_bar (stat="identity",aes(fill=Comment),show.legend = FALSE)+
    facet_wrap(.~factor (Comment, 
levels=c("з обвинувальним актом",
         "клопотання про звільнення",
         "примусові заходи
медичного характеру",
         "примусові заходи
виховного характеру")),scales = "free_y")+ 
    scale_fill_brewer(palette = "Set1")+
    labs(title = "Структура матеріалів, що передаються до суду",
         subtitle=paste (top_art_names[i]))+
    geom_text(size=3,angle=90, position = position_stack(vjust = 0.5))+
    theme(axis.title=element_blank(),axis.text.y=element_blank(),
          axis.ticks.y=element_blank(), axis.text.x = element_text(angle=90))
  
  mysave (vis,n_suf)
  vis<-vis+1
}else vis<-vis+1
 court_sub<-subset (court_raw,Article==art_top)
  
  
    #court_sub$Article<-art_top
  court_sub<-summarise (group_by(court_sub,Year,Chapter,Article),
                            CONVIC=sum(CONVIC),
                            ACQUIT=sum(ACQUIT),
                            CMED=sum(CMED),
                            CCLOSE=sum(CCLOSE))
  court_sub<-court_sub[,c("Year","Chapter","Article","CONVIC","ACQUIT","CMED","CCLOSE")]
  court_sub<-as.data.frame(court_sub)
  court_sub[court_sub==0]<-NA
  if (nrow(court_sub)>0)
  {
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
}
  if (nrow(court_sub)>0)
  {
    
    top_3<-ggplot(court_sub, aes(x=as.factor(Year),y=QNT, label=QNT))
  top_3+geom_bar (stat="identity",aes(fill=Comment),show.legend = FALSE)+
    facet_wrap(.~factor (Comment, 
                         levels=c("засуджено осіб",
                                  "виправдано осіб",
                                  "неосудних осіб",
                                  "закрито справ")),scales = "free_y")+ 
    labs(title = "Структура судових рішень",
         subtitle=paste (names_numbers$names[names_numbers$numbers==art_top]))+
    geom_text(size=3,angle=90, position = position_stack(vjust = 0.5))+
    theme(axis.title=element_blank(),axis.text.y=element_blank(),
          axis.ticks.y=element_blank(), axis.text.x = element_text(angle=90))
  
  mysave (vis,n_suf)
  vis<-vis+1  
}  else vis<-vis+1
  #Розподіл підстав закриття справ  
  
  court_sub<-subset (court_raw,Article==art_top)
  
 # court_sub$Article<-art_top
  court_sub<-summarise (group_by(court_sub,Year,Chapter,Article),
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
                        AS=sum(CCLOSE))
                        
                        
  court_sub<-as.data.frame(court_sub)
    
  if (nrow(court_sub)>0)
  {
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
}
  if (nrow(court_sub)>0 )
  {  
    
  
    top_4<-ggplot (court_sub,aes(x=factor(Comment,levels=order_p), y=QNT,fill=Comment, 
    label=paste(QNT,"-",paste(round(QNT/AS*100,1),"%"  )     )    ))
  top_4+scale_fill_discrete(breaks=order_p)+
    geom_bar(stat="identity",)+facet_wrap(.~Year,ncol=3,dir="h")+
    geom_text(size=3,angle=90,hjust = 0, y = max(court_sub$QNT)/15)+
    theme (legend.position = "bottom",legend.title=element_blank(),
           axis.ticks.x=element_blank(),legend.direction="horizontal",
           legend.text=element_text(size = 7), axis.text.x=element_blank())+
    labs(title="Розподіл підстав закриття справ",x="Підстави",y="кількість",
         subtitle=paste (names_numbers$names[names_numbers$numbers==art_top]))
    
    
  mysave (vis,n_suf)
  vis<-vis+1
}else vis<-vis+1
#Застосовані покарання
  
  
court_sub<-subset (court_raw,Article==art_top)

court_sub<-summarise (group_by(court_sub,Year,Chapter,Article),
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
                      AS=sum(CONVIC))


court_sub<-select(court_sub,Year:IMP,RESTOL:AS)
court_sub<-as.data.frame(court_sub)

if (nrow(court_sub)>0)  
{court_sub<-reshape(court_sub,
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
посади або займатися певною діяльністю")			

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
посади або займатися певною діяльністю"
court_sub[court_sub==0]<-NA

court_sub<-subset(court_sub,!is.na(QNT))
}

if (nrow(court_sub)>0)
{
  
  top_5<-ggplot (court_sub,aes(x=factor(Comment,levels=punish_ord), y=QNT,fill=Comment,
          label=paste(QNT,"-",round(QNT/AS*100,1),"%"  )   ))
top_5+scale_fill_discrete(breaks=punish_ord)+
  geom_bar(stat="identity",)+facet_wrap(.~Year,ncol=3,dir="h")+
  geom_text(size=3,hjust = 0, angle = 90, y=max(court_sub$QNT)/15)+
  theme (legend.position = "bottom",axis.ticks.x=element_blank(), legend.title=element_blank(),
         legend.text.align=0,legend.text=element_text(size = 7), axis.text.x=element_blank())+
  labs(title="Застосовані покарання. 
Кількість та частка від визнаних винними осіб",x="Види покарань",y="кількість",
       subtitle=paste (names_numbers$names[names_numbers$numbers==art_top]))


mysave (vis,n_suf)
vis<-vis+1
}else vis<-vis+1


#Розподіл інтенсивності позбавлення волі

court_sub<-subset (court_raw,Article==art_top)

court_sub<-summarise (group_by(court_sub,Year,Chapter,Article),
                      IMP1=sum(IMP1),
                      IMP12=sum(IMP12),
                      IMP23=sum(IMP23),
                      IMP35=sum(IMP35),
                      IMP510=sum(IMP510),
                      IMP1015=sum(IMP1015),
                      IMP1525=sum(IMP1525),
                      AS=sum(IMP))



court_sub<-as.data.frame(court_sub)

if (nrow(court_sub)>0)
{
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
}

if (nrow(court_sub>0))
{
  
top_6<-ggplot (court_sub,aes(x=factor(Comment,levels=imp_ord), y=QNT,fill=Comment,
        label=paste(QNT,"-",round(QNT/AS*100,1),"%"   )    ))
top_6+scale_fill_discrete(breaks=imp_ord,type=c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02","#a6761d"))+
  geom_bar(stat="identity",)+facet_wrap(.~Year,ncol=3,dir="h")+
  geom_text(size=3,angle=90,hjust = 0, y = max(court_sub$QNT)/15)+
  theme (legend.position = "bottom",axis.ticks.x=element_blank(), legend.title=element_blank(),
         legend.text=element_text(size = 7), axis.text.x=element_blank())+
  labs(title="Розподіл строків призначеного покарання у вигляді позбавлення волі",x="Строки",y="кількість",
       subtitle=paste (names_numbers$names[names_numbers$numbers==art_top]))


mysave (vis,n_suf)
vis<-vis+1
}else vis<-vis+1

###AGE




gr2<-summarise(group_by(frame2,Year, Chapter, Article),
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
             times=c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65"), 
             v.names = "QNT", 
             varying = c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65"), 
             direction = "long")

bylo<-c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65")
stalo<-c("від 14 до 16 років","від 16 до 18 років","від 18 до 25 років","від 25 до 30 років",
         "від 30 до 50 років","від 50 до 65 років","від 65 років і старше")
for (i in 1:length(bylo))
{gr2$Comment[gr2$Comment==bylo[i]]<-stalo[i]}

gr2<-as.data.frame (gr2)

gr2$HOR<-0
for (y in ymin:ymax)
{gr2$HOR[gr2$Year==y]<-max(gr2$QNT[gr2$Year==y])/8}

gr2_sub<-subset(gr2,Article==art_top)
if (sum(gr2_sub$QNT,na.rm = TRUE)>0)
{gr2_sub<-subset(gr2_sub,!QNT==0)
  age1<-ggplot(gr2_sub,aes(x=factor(Comment,levels = stalo),fill=Comment, y=QNT, 
                          label=paste(QNT,"-",round(QNT/AS*100,1),"%")  )    )
age1+geom_bar(stat="identity")+facet_wrap(.~factor(Year),
                                          ncol=3,dir="v")+
  scale_fill_discrete(breaks=stalo,
                      type=c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02","#a6761d"))+
  labs(y="кількість засуджених",title=paste("Розподіл засуджених осіб за віком на 
момент вчинення кримінального правопорушення"),
       subtitle=paste (names_numbers$names[names_numbers$numbers==art_top]))+
  theme (legend.title = element_blank(),legend.position = "right",
         axis.ticks.y =element_blank(),
         axis.text.x=element_blank(),
         axis.title=element_blank())+
  geom_text(angle=90,y=max(gr2_sub$QNT)/8,size=3, hjust=0)

mysave (vis,n_suf)
vis<-vis+1}else vis<-vis+1




###PROF

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

gr2<-summarise(group_by(frame2, Year, Chapter,Article),
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

gr2<-subset(gr2,Article==art_top)

gr2<-subset(gr2,QNT>0)

if (sum(gr2$QNT,na.rm = TRUE)>0)  
{
gr2$HOR<-0

for (i in ymin:ymax)
{gr2$HOR[gr2$Year==i]<-max(gr2$QNT[gr2$Year==i])/8}

age1<-ggplot(gr2,aes(x=factor(Comment,levels = stalo),fill=Comment, y=QNT, 
                      label=paste (QNT," - ",round(QNT/TOT*100,2),"%")))
age1+geom_bar(stat="identity")+facet_wrap(.~factor(Year),
                                          ncol=5,
                                          dir="h",
                                          scales = "free_y")+
  labs(y="кількість засуджених",
       title=paste("Розподіл засуджених осіб за видом занять на момент вчинення 
кримінального правопорушення"),
       subtitle=paste (names_numbers$names[names_numbers$numbers==art_top]))+
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



mysave (vis,n_suf)
vis<-vis+1
}  else vis<-vis+1


###EDUCATION



stalo<-c(
  "повна вища",
  "базова вища",
  "професійнотехнічна",
  "повна загальна середня",
  "базова загальна середня",
  "початкова загальна",
  "без освіти"
)
gr2<-summarise(group_by(frame2,Year, Chapter, Article),
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
             times=c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65"), 
             v.names = "QNT", 
             varying = c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65"), 
             direction = "long")

bylo<-c("A14_16","A16_18","A18_25","A25_30","A30_50","A50_65","A65")

for (i in 1:length(bylo))
{gr2$Comment[gr2$Comment==bylo[i]]<-stalo[i]}

gr2<-as.data.frame (gr2)

gr2_sub<-subset(gr2,Article==art_top)
gr2_sub<-subset(gr2_sub,QNT>0)

if (sum(gr2_sub$QNT,na.rm = TRUE)>0)  
{

gr2_sub$HOR<-0
for (i in ymin:ymax)
{gr2_sub$HOR[gr2_sub$Year==i]<-max(gr2_sub$QNT[gr2_sub$Year==i])/8}  


age1<-ggplot(gr2_sub,aes(x=factor(Comment,levels = stalo),fill=Comment, y=QNT, 
                          label=paste(QNT,"-",round(QNT/AS*100,1),"%")         ))
age1+geom_bar(stat="identity")+facet_wrap(.~factor(Year),
                                          ncol=3,dir="v")+
  labs(title=paste("Розподіл засуджених осіб за освітою на 
момент вчинення кримінального правопорушення"),
       subtitle=paste (names_numbers$names[names_numbers$numbers==art_top]))+
  theme (legend.title = element_blank(),legend.position = "right",
         axis.ticks.y =element_blank(),
         axis.text.x=element_blank(),
         axis.title=element_blank())+
  scale_fill_brewer(breaks=stalo,palette = "Set1")+
  geom_text(angle=90,y=gr2_sub$HOR,size=3, hjust=0)

mysave (vis,n_suf)  
vis<-vis+1}else vis<-vis+1



###SUDYMIST



bylo<-c("A14_16","A16_18","A18_25","A25_30")
stalo<-c(
  "Раніше вчинили злочин, 
але були звільнені від 
кримінальної відповідальності",
  "Судилися, але визнані такими,
що не мають судимості",
  "Судилися, але судимість
погашена чи знята",
  "Мають судимість"
)
gr2<-summarise(group_by(frame2,Year, Chapter, Article),
               TOT=sum(G1),
               A14_16=sum(G42),
               A16_18=sum(G43),
               A18_25=sum(G44),
               A25_30=sum(G45),
               AS=sum (G42,G43,G44,G45),
               pA14_16=A14_16/AS*100,
               pA16_18=A16_18/AS*100,
               pA18_25=A18_25/AS*100,
               pA25_30=A25_30/AS*100,
               PTOT=AS/TOT*100)



gr2<-subset(gr2,!is.na(Chapter))
gr2<-as.data.frame (gr2)
gr2$Article<-sub("Article ","",gr2$Article)
gr2$Article<-gsub(" ","",gr2$Article)


gr2<-reshape(gr2,
             timevar = "Comment", 
             times=bylo, 
             v.names = "QNT", 
             varying = bylo, 
             direction = "long")


for (i in 1:length(bylo))
{gr2$Comment[gr2$Comment==bylo[i]]<-stalo[i]}

gr2<-as.data.frame (gr2)


gr2_sub<-subset(gr2,Article==art_top)
gr2_sub<-subset(gr2_sub,QNT>0)

if (sum(gr2_sub$QNT,na.rm = TRUE)>0)
{
gr2_sub$HOR<-0
for (i in ymin:ymax)
{gr2_sub$HOR[gr2_sub$Year==i]<-max(gr2_sub$QNT[gr2_sub$Year==i])/8}  

age1<-ggplot(gr2_sub,aes(x=factor(Comment,levels = stalo),
                          fill=Comment, y=QNT, 
                          label=paste(QNT,"-",round(QNT/TOT*100,1),"%")   ))
age1+geom_bar(stat="identity")+facet_wrap(.~factor(Year),
                                          ncol=3,dir="v")+
  
  labs(title=paste("Розподіл засуджених осіб за попередньою протиправною поведінкою
та частка відповідних категорій серед всіх засуджених"),
       subtitle=paste(names_numbers$names[names_numbers$numbers==art_top]))+
  theme (legend.title = element_blank(),legend.position = "right",
         axis.ticks.y =element_blank(),
         axis.text.x=element_blank(),
         axis.title=element_blank())+
  scale_fill_brewer(palette="Set1",breaks=stalo)+
  geom_text(angle=90,y=gr2_sub$HOR,size=3, hjust=0)


mysave (vis,n_suf)  
vis<-vis+1
}else vis<-vis+1



###SUDYMIST123




bylo<-c("A14_16","A16_18","A18_25")
stalo<-c(
  
  "Одна судимість",
  "Дві судимості",
  "Три і більше
судимостей")


gr2<-summarise(group_by(frame2,Year, Chapter, Article),
               TOT=sum(G1),
               A14_16=sum(G46),
               A16_18=sum(G47),
               A18_25=sum(G48),
               AS=sum (G46,G47,G48),
               pA14_16=A14_16/AS*100,
               pA16_18=A16_18/AS*100,
               pA18_25=A18_25/AS*100,
               PTOT=AS/TOT*100)


gr2<-subset(gr2,AS>0)
gr2<-subset(gr2,!is.na(Chapter))
gr2<-as.data.frame (gr2)
gr2$Article<-sub("Article ","",gr2$Article)
gr2$Article<-gsub(" ","",gr2$Article)

gr2_t<-gr2


gr2<-reshape(gr2,
             timevar = "Comment", 
             times=bylo, 
             v.names = "QNT", 
             varying = bylo, 
             direction = "long")


for (i in 1:length(bylo))
{gr2$Comment[gr2$Comment==bylo[i]]<-stalo[i]}

gr2<-as.data.frame (gr2)

gr2_sub<-subset(gr2,Article==art_top)
gr2_sub<-subset(gr2_sub,QNT>0)

if (sum(gr2_sub$QNT,na.rm = TRUE)>0)
{
gr2_sub$HOR<-0
for (i in ymin:ymax)
{gr2_sub$HOR[gr2_sub$Year==i]<-max(gr2_sub$QNT[gr2_sub$Year==i])/8}   

age1<-ggplot(gr2_sub,aes(x=factor(Comment,levels = stalo),fill=Comment, y=QNT, 
                          label=paste(QNT,"-",round(QNT/AS*100,1),"%")    ))
age1+geom_bar(stat="identity")+facet_wrap(.~factor(Year),
                                          ncol=3,dir="v")+
  scale_fill_discrete(breaks=stalo,
                      type=c("#1b9e77","#d95f02","#7570b3"))+
  labs(title=paste("Розподіл засуджених з судимістю за кількістю судимостей та частка категорій
таких осіб серед всіх, які мали судимість на момент вчинення правопорушення"),
       subtitle=paste(names_numbers$names[names_numbers$numbers==art_top]))+        
  
  theme (legend.title = element_blank(),legend.position = "right",
         axis.ticks.y =element_blank(),
         axis.text.x=element_blank(),
         axis.title=element_blank())+
  geom_text(angle=90,y=gr2_sub$HOR,size=3, hjust=0)


mysave (vis,n_suf) 
vis<-vis+1}else vis<-vis+1





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

gr2<-summarise(group_by(frame2, Year,Chapter,Article),
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



gr2_sub<-subset(gr2,Article==art_top)
gr2_sub<-subset(gr2_sub,QNT>0)

if (sum(gr2_sub$QNT,na.rm = TRUE)>0)  
{
gr2_sub$HOR<-0
for (i in ymin:ymax)
{gr2_sub$HOR[gr2_sub$Year==i]<-max(gr2_sub$QNT[gr2_sub$Year==i])/8} 


age1<-ggplot(gr2_sub,aes(x=factor(Comment,levels = stalo),fill=Comment, y=QNT, 
                          label=paste(QNT,"-",round(QNT/AS*100,1),"%"  )    ))
age1+geom_bar(stat="identity")+facet_wrap(.~Year,ncol=3,dir="v")+
  labs(title="Окремі категорії засуджених осіб, які мали судимість на момент вчинення
кримінального правопорушення. Кількість та частка серед всіх таких засуджених",
       subtitle=paste(names_numbers$names[names_numbers$numbers==art_top]))+        
  
  theme (legend.title = element_blank(),legend.position = "right",
         axis.title.x=element_blank(),
         axis.text=element_blank(),
         axis.ticks =element_blank(),
         axis.title.y=element_blank())+geom_text(y=max(gr2_sub$QNT)/8,hjust=0,angle=90)+
  scale_fill_brewer(palette="Set1",breaks=stalo)

mysave(vis,n_suf)
vis<-vis+1}else vis<-vis+1


if (vis==2)
  {
  vis<-1
  mark<-ggplot()
  mark+geom_text(aes(x=5,y=5),label=paste(names_numbers$names[names_numbers$numbers==art_top],"
--- У СТАТИСТИЧНИХ ОБЛІКАХ ДАНІ ЩОДО ЗАСТОСУВАННЯ ЦІЄЇ НОРМИ ВІДСУТНІ ---"))+
        theme (axis.title = element_blank(),
           axis.text = element_blank(),
           axis.ticks = element_blank())
  
    mysave (vis,n_suf)
    nikoly<-nikoly+1
  
}


}







