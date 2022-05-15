interest<-chapter_ord


chtit<-c(
  "Злочини проти основ національної безпеки України",
  "Кримінальні правопорушення проти життя та здоров'я особи",
  "Кримінальні правопорушення проти волі, честі та гідності особи",
  "Кримінальні правопорушення проти статевої свободи
та статевої недоторканності особи",
  "Кримінальні правопорушення проти виборчих, трудових та 
інших особистих прав і свобод людини і громадянина",
  "Кримінальні правопорушення проти власності",
  "Кримінальні правопорушення у сфері господарської діяльності",
  "Кримінальні правопорушення проти довкілля",
  "Кримінальні правопорушення проти громадської безпеки",					
  "Кримінальні правопорушення проти безпеки виробництва",
  "Кримінальні правопорушення проти безпеки руху та експлуатації транспорту",
  "Кримінальні правопорушення проти громадського порядку та моральності",
  "Кримінальні правопорушення у сфері обігу наркотичних засобів, психотропних
речовин, їх аналогів або прекурсорів та інші кримінальні правопорушення
проти здоров'я населення",
  "Кримінальні правопорушення у сфері охорони державної таємниці, 
недоторканності державних кордонів, забезпечення призову та мобілізації",
  "Кримінальні правопорушення проти авторитету органів державної влади, 
органів місцевого самоврядування, об’єднань громадян та кримінальні 
правопорушення проти журналістів",
  "Кримінальні правопорушення у сфері використання 
електронно-обчислювальних машин (комп'ютерів), систем 
та комп'ютерних мереж і мереж електрозв'язку",
  "Кримінальні правопорушення у сфері службової діяльності та 
професійної діяльності, пов'язаної з наданням публічних послуг",
  "Кримінальні правопорушення проти правосуддя",
  "Кримінальні правопорушення проти встановленого порядку несення
військової служби (військові кримінальні правопорушення)",
  "Кримінальні правопорушення проти миру, безпеки людства 
та міжнародного правопорядку") 

##GENDER

for (ch in 1:length(interest))
{
vis<-16  
  ch1<-subset(main.frame,Chapter==interest[ch])
  ch1gr<-summarise(group_by(ch1,Year,Chapter,Article),
                   TOTAL=sum(G1),
                   Female=sum(G4),
                   PartFem=sum(G4)/sum(G1)*100)  
  ch1gr<-subset(ch1gr,TOTAL>0)
  ch1gr$Article<-sub("Article ","",ch1gr$Article)
  ch1gr$Article<-gsub(" ","",ch1gr$Article)
  
  subt<-paste ("Розділ ",interest[ch],". ",chtit[ch], sep="")
  ch1gr<-subset(ch1gr,Female>0)
  horizon<-round(max(ch1gr$Female)/5+1,1)
  grch<-ggplot(ch1gr,aes(x=factor(Article),fill=Article,y=Female, label=Female))
  grch+geom_bar(stat="identity",show.legend = FALSE)+
    geom_text(angle=90,y=horizon,size=2)+
    facet_wrap(.~Year,ncol=2,dir="v")+
    labs (title = paste("Кількість засуджених жінок (",ymin,"-",ymax,")",sep=""),
          subtitle = subt, x="Стаття", y="Кількість засуджених")+
    theme(axis.text.x=element_text(angle = 90))
 
  name_s<-paste("rozdil",ch,sep="")
  mysave (vis,suf=name_s)  
  vis<-vis+1
  
  horizon<-round(max(ch1gr$PartFem)/5+1,1)
  if (ch==4) {horizon<-(-5)}
  grch<-ggplot(ch1gr,aes(x=factor(Article),fill=Article, y=round(PartFem,0),label=round(PartFem,1)))
  grch+geom_bar(stat="identity",show.legend = FALSE)+
    geom_hline(aes(yintercept=common_fem), alpha=0.7, color="red",
               show.legend = FALSE)+geom_text(angle=90,y=horizon+10, size=2)+
    facet_wrap(.~Year,ncol=2,dir="v")+
    labs (title = paste("Частка жінок серед засуджених (",ymin,"-",ymax,")",sep=""),
          subtitle = subt, x="Стаття", y="%")+
    theme(axis.text.x=element_text(angle = 90),legend.title = element_blank())
  
 
  mysave (vis,suf=name_s)  
  vis<-vis+1
  
}


####GROUP

for (ch in 1:length(interest))
{
  vis<-18
  ch1<-subset(main.frame,Chapter==interest[ch])
  ch1gr<-summarise(group_by(ch1,Year,Chapter,Article),
                   TOTAL=sum(G1),
                   Female=sum(G5),####G5 means group (conspiracy)
                   PartFem=sum(G5)/sum(G1)*100)  
  ch1gr<-subset(ch1gr,TOTAL>0)
  ch1gr$Article<-sub("Article ","",ch1gr$Article)
  ch1gr$Article<-gsub(" ","",ch1gr$Article)
  
  subt<-paste ("Розділ ",interest[ch],". ",chtit[ch], sep="")
  ch1gr<-subset(ch1gr,Female>0)
  horizon<-round(max(ch1gr$Female)/5+1,1)
  grch<-ggplot(ch1gr,aes(x=factor(Article),fill=Article,y=Female, label=Female))
  grch+geom_bar(stat="identity",show.legend = FALSE)+
    geom_text(angle=90,y=horizon,size=2)+
    facet_wrap(.~Year,ncol=2,dir="v")+
    labs (title = paste("Кількість засуджених за правопорушення, вчинені у складі груп (",ymin,"-",ymax,")",sep=""),
          subtitle = subt, x="Стаття", y="Кількість засуджених")+
    theme(axis.text.x=element_text(angle = 90))
  
  name_s<-paste("rozdil",ch,sep="")
  mysave (vis,suf=name_s)  
  vis<-vis+1
  
  horizon<-round(max(ch1gr$PartFem)/5+1,1)
  #if (ch==4) {horizon<-(-5)}
  
  
  grch<-ggplot(ch1gr,aes(x=factor(Article),fill=Article, y=round(PartFem,0),label=round(PartFem,1)))
  grch+geom_bar(stat="identity",show.legend = FALSE)+
    geom_hline(aes(yintercept=horizon_group), color="red",
               show.legend = FALSE)+geom_text(angle=90,y=horizon+10, size=2)+
    facet_wrap(.~Year,ncol=2,dir="v")+
    labs (title = paste("Частка засуджених за правопорушення, вчинені у складі груп,
серед загальної кількості засуджених (",ymin,"-",ymax,")",sep=""),
          subtitle = subt, x="Стаття", y="%")+
    theme(axis.text.x=element_text(angle = 90),legend.title = element_blank())
  
  mysave (vis,suf=name_s)  
  vis<-vis+1
  
}

###OZG

for (ch in 1:length(interest))
{
  vis<-20
  ch1<-subset(main.frame,Chapter==interest[ch])
  ch1gr<-summarise(group_by(ch1,Year,Chapter,Article),
                   TOTAL=sum(G1),
                   Female=sum(G6),####G5 means group (conspiracy)
                   PartFem=sum(G6)/sum(G1)*100)  
  ch1gr<-subset(ch1gr,TOTAL>0)
  ch1gr$Article<-sub("Article ","",ch1gr$Article)
  ch1gr$Article<-gsub(" ","",ch1gr$Article)
  
  subt<-paste ("Розділ ",interest[ch],". ",chtit[ch], sep="")
  ch1gr<-subset(ch1gr,Female>0)
  horizon<-round(max(ch1gr$Female)/5+1,1)
  if (nrow(ch1gr)>0) 
  { 
    grch<-ggplot(ch1gr,aes(x=factor(Article),fill=Article,y=Female, label=Female))
    grch+geom_bar(stat="identity",show.legend = FALSE)+
      geom_text(angle=90,y=horizon,size=2)+
      facet_wrap(.~Year,ncol=2,dir="v")+
      labs (title = paste("Кількість засуджених за правопорушення, 
вчинені у складі організованої групи (",ymin,"-",ymax,")",sep=""),
            subtitle = subt, x="Стаття", y="Кількість засуджених")+
      theme(axis.text.x=element_text(angle = 90))
    
    name_s<-paste("rozdil",ch,sep="")
    mysave (vis,suf=name_s) 
    vis<-vis+1
    
    horizon<-round(max(ch1gr$PartFem)/2,1)
    #if (ch==4) {horizon<-(-5)}
    
    
    grch<-ggplot(ch1gr,aes(x=factor(Article),fill=Article, y=round(PartFem,0),label=round(PartFem,1)))
    grch+geom_bar(stat="identity",show.legend = FALSE)+
      geom_hline(aes(yintercept=horizon_group), color="red",
                 show.legend = FALSE)+geom_text(angle=90,y=horizon, size=2)+
      facet_wrap(.~Year,ncol=2,dir="v")+
      labs (title = paste("Частка засуджених за правопорушення, вчинені у складі 
організованої групи (",ymin,"-",ymax,")",sep=""),
            subtitle = subt, x="Стаття", y="%")+
      theme(axis.text.x=element_text(angle = 90),legend.title = element_blank())
    
    mysave (vis,suf=name_s) 
    vis<-vis+1
  }
}

###AGE




gr2<-summarise(group_by(main.frame,Year, Chapter, Article),
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
gr2$Article<-sub("Article ","",gr2$Article)
gr2$Article<-gsub(" ","",gr2$Article)



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

gr2<-as.data.frame (gr2)
###

for (ch in 1:length(interest))
{
  vis<-22
  gr2_sub<-subset(gr2,Chapter==chapter_ord[ch])
  subt<-chtit[ch]
  age1<-ggplot(gr2_sub,aes(x=factor(Article),fill=Comment, y=QNT, label=round(QNT,1)))
  age1+geom_bar(stat="identity")+facet_wrap(.~factor(Year),
                                            ncol=2,dir="v")+
    scale_fill_discrete(breaks=stalo,
                        type=c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02","#a6761d"))+
    labs(y="кількість засуджених",title=paste("Відсотковий розподіл засуджених осіб за віком на 
момент вчинення кримінального правопорушення (за період з ",ymin," по ", ymax, " рік)", sep=""),
         subtitle=paste("Розділ ",chapter_ord[ch],". ",subt,sep="") )+
    theme (legend.title = element_blank(),legend.position = "bottom",
           axis.ticks.y =element_blank(),
           axis.text.x=element_text(angle=90,size = 4,vjust=0),
           axis.title=element_blank())
  
  name_s<-paste("rozdil",ch,sep="")
  mysave (vis,suf=name_s) 
 
  
}

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

gr2<-summarise(group_by(main.frame, Year, Chapter),
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

for (ch in 1:length(interest))
{
  vis<-23
  gr2_sub<-subset(gr2,Chapter==chapter_ord[ch])
  
  for (i in ymin:ymax)
  {gr2_sub$HOR[gr2_sub$Year==i]<-max(gr2_sub$QNT[gr2_sub$Year==i])/9}
  
  age1<-ggplot(gr2_sub,aes(x=factor(Comment,levels = stalo),fill=Comment, y=QNT, 
                           label=paste (QNT," - ",round(QNT/TOT*100,2),"%")))
  age1+geom_bar(stat="identity")+facet_wrap(.~factor(Year),
                                            ncol=5,
                                            dir="h",
                                            scales = "free_y")+
    labs(y="кількість засуджених",
         title=paste("Розподіл засуджених осіб за видом занять на момент вчинення 
кримінального правопорушення. За період з ",ymin," по ", ymax, " рік.", sep=""),
         subtitle=paste("Розділ ",chapter_ord[ch],".",chtit[ch],sep=""))+
    theme (legend.title = element_blank(),legend.position = "bottom",
           legend.text=element_text(size=7),legend.direction = "horizontal",
           axis.title.x=element_blank(),
           axis.text=element_blank(),
           axis.ticks =element_blank(),
           axis.title.y=element_blank())+geom_text(y=gr2_sub$HOR,size=2,hjust=0,angle=90)+
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
  
  
  name_s<-paste("rozdil",ch,sep="")
  mysave (vis,suf=name_s)  
  
}

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
gr2<-summarise(group_by(main.frame,Year, Chapter, Article),
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
gr2$Article<-sub("Article ","",gr2$Article)
gr2$Article<-gsub(" ","",gr2$Article)



gr2<-reshape(gr2,
             timevar = "Comment", 
             times=c("pA14_16","pA16_18","pA18_25","pA25_30","pA30_50","pA50_65","pA65"), 
             v.names = "QNT", 
             varying = c("pA14_16","pA16_18","pA18_25","pA25_30","pA30_50","pA50_65","pA65"), 
             direction = "long")

bylo<-c("pA14_16","pA16_18","pA18_25","pA25_30","pA30_50","pA50_65","pA65")

for (i in 1:length(bylo))
{gr2$Comment[gr2$Comment==bylo[i]]<-stalo[i]}

gr2<-as.data.frame (gr2)
###

for (ch in 1:length(interest))
{
  vis<-24
  gr2_sub<-subset(gr2,Chapter==chapter_ord[ch])
  subt<-chtit[ch]
  age1<-ggplot(gr2_sub,aes(x=factor(Article),fill=Comment, y=QNT, label=round(QNT,1)))
  age1+geom_bar(stat="identity")+facet_wrap(.~factor(Year),
                                            ncol=2,dir="v")+
    labs(y="кількість засуджених",title=paste("Відсотковий розподіл засуджених осіб за освітою на 
момент вчинення кримінального правопорушення (за період з ",ymin," по ", ymax, " рік)", sep=""),
         subtitle=paste("Розділ ",chapter_ord[ch],". ",subt,sep="") )+
    theme (legend.title = element_blank(),legend.position = "bottom",
           axis.ticks.y =element_blank(),
           axis.text.x=element_text(angle=90,size = 4,vjust=0),
           axis.title=element_blank())+
    scale_fill_brewer(breaks=stalo,palette = "Set1")
  
  name_s<-paste("rozdil",ch,sep="")
  mysave (vis,suf=name_s)  
  
}

###SUDYMIST



bylo<-c("pA14_16","pA16_18","pA18_25","pA25_30")
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
gr2<-summarise(group_by(main.frame,Year, Chapter, Article),
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
###

for (ch in 1:length(interest))
{
  vis<-25
  gr2_sub<-subset(gr2_t,Chapter==chapter_ord[ch])
  subt<-chtit[ch]
  gr2_sub<-subset(gr2_sub,PTOT>0)
  font_s<-6
  font_s2<-2
  if (ch==7) {font_s<-4
  font_s2<-2}
  
  age1<-ggplot(gr2_sub,aes(x=factor(Article),fill="red", y=PTOT, 
                           label=paste(round(PTOT,1),"% -",AS) ) )
  age1+geom_bar(stat="identity", show.legend = FALSE)+facet_wrap(.~factor(Year),
                                                                 ncol=2,dir="v")+
    labs(y="%",title=paste("Частка засуджених з попередньою кримінальною протиправною поведінкою
та кількість таких осіб (за період з ",ymin," по ", ymax, " рік)", sep=""),
         subtitle=paste("Розділ ",chapter_ord[ch],". ",subt,sep="") )+
    geom_hline(aes(yintercept=horizon_sudim), color="red", show.legend = FALSE)+
    theme (legend.title = element_blank(),legend.position = "bottom",
           axis.ticks.y =element_blank(),
           axis.text.x=element_text(angle=90,size = font_s,vjust=0),
           axis.title=element_blank())+geom_text(y=13,hjust=0,size=font_s2,angle=90)
  
  
  name_s<-paste("rozdil",ch,sep="")
  mysave (vis,suf=name_s)  
  vis<-vis+1
  
  
  gr2_sub<-subset(gr2,Chapter==chapter_ord[ch])
  
  age1<-ggplot(gr2_sub,aes(x=factor(Article),fill=Comment, y=QNT, label=round(QNT,1)))
  age1+geom_bar(stat="identity")+facet_wrap(.~factor(Year),
                                            ncol=2,dir="v")+
    
    labs(y="кількість засуджених",title=paste("Відсотковий розподіл засуджених осіб за попередньою протиправною поведінкою
  (за період з ",ymin," по ", ymax, " рік)", sep=""),
         subtitle=paste("Розділ ",chapter_ord[ch],". ",subt,sep="") )+
    theme (legend.title = element_blank(),legend.position = "bottom",
           axis.ticks.y =element_blank(),
           axis.text.x=element_text(angle=90,size = font_s,vjust=0),
           axis.title=element_blank())+
    scale_fill_brewer(palette="Set1",breaks=stalo)
  
  
  mysave (vis,suf=name_s)  
  vis<-vis+1
  
}



###SUDYMIST123




bylo<-c("pA14_16","pA16_18","pA18_25")
stalo<-c(
  
  "Одна судимість",
  "Дві судимості",
  "Три і більше
судимостей")


gr2<-summarise(group_by(main.frame,Year, Chapter, Article),
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
###

for (ch in 1:length(interest))
{
  vis<-27
  gr2_sub<-subset(gr2_t,Chapter==chapter_ord[ch])
  subt<-chtit[ch]
  font_s<-6
  font_s2<-2
  if (ch==7) {font_s<-4
  font_s2<-2}
  
  age1<-ggplot(gr2_sub,aes(x=factor(Article),fill="red", y=PTOT, 
                           label=paste(round(PTOT,1),"% -", AS)   )   )
  age1+geom_bar(stat="identity", show.legend = FALSE)+facet_wrap(.~factor(Year),
                                                                 ncol=2,dir="v")+
    labs(y="%",title=paste("Частка та кількість засуджених, які мали судимість 
на момент вчинення кримінального правопорушення (за період з ",ymin," по ", ymax, " рік)", sep=""),
         subtitle=paste("Розділ ",chapter_ord[ch],". ",subt,sep="") )+
    geom_hline(aes(yintercept=common_sudim123), color="red", show.legend = FALSE)+
    theme (legend.title = element_blank(),legend.position = "bottom",
           axis.ticks.y =element_blank(),
           axis.text.x=element_text(angle=90,size = font_s,vjust=0),
           axis.title=element_blank())+geom_text(y=13,hjust=0,size=font_s2,angle=90)
  
  name_s<-paste("rozdil",ch,sep="")
  mysave (vis,suf=name_s) 
  vis<-vis+1
  
  
  gr2_sub<-subset(gr2,Chapter==chapter_ord[ch])
  
  age1<-ggplot(gr2_sub,aes(x=factor(Article),fill=Comment, y=QNT, label=round(QNT,1)))
  age1+geom_bar(stat="identity")+facet_wrap(.~factor(Year),
                                            ncol=2,dir="v")+
    scale_fill_discrete(breaks=stalo,
                        type=c("#1b9e77","#d95f02","#7570b3","#e7298a"))+
    labs(y="кількість засуджених",title=paste("Відсотковий розподіл засуджених з судимістю за кількістю судимостей
  (за період з ",ymin," по ", ymax, " рік)", sep=""),
         subtitle=paste("Розділ ",chapter_ord[ch],". ",subt,sep="") )+
    theme (legend.title = element_blank(),legend.position = "bottom",
           axis.ticks.y =element_blank(),
           axis.text.x=element_text(angle=90,size = font_s,vjust=0),
           axis.title=element_blank())
  
  
  mysave (vis,suf=name_s) 
  vis<-vis+1
  
}









