
#Creating Data for new project

erdr<-pgo_raw
erdr$Chapter<-as.character(erdr$Chapter)

erdr<-subset(erdr,!is.na(Chapter))
erdr<-subset(erdr,!str_detect(Article,"Others"))
erdr$Article<-sub("Article", "",erdr$Article)
erdr$Article<-gsub(" ","",erdr$Article)



court1<-summarise(group_by(court_raw,Year,Article,Chapter),
                  CRTOT=sum(CRTOT),
                  CONVIC=sum(CONVIC),
                  ACQUIT=sum(ACQUIT),
                  CMED=sum(CMED),
                  CCLOSE=sum(CCLOSE),
                  CONFES=sum(CONFES),
                  RECONC=sum(RECONC),
                  CIRCUMS=sum(CIRCUMS),
                  SPONS=sum(SPONS),
                  AMNESTY=sum(AMNESTY),
                  DEATH=sum(DEATH),
                  COTHER=sum(COTHER),
                  SAMEVERD=sum(SAMEVERD),
                  CNOTCR=sum(CNOTCR),
                  DENOPR=sum(DENOPR),
                  CEDU=sum(CEDU),
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
                  PROB=sum(PROB),
                  RELAMN=sum(RELAMN),
                  RELOTHR=sum(RELOTHR))
                  
                  
                  
                  
court1<-subset(court1,!is.na(Chapter))
court1<-subset(court1,!str_detect(Article,"Others"))
court1$Article<-sub("Article", "",court1$Article)
court1$Article<-gsub(" ","",court1$Article)

court2<-summarise(group_by(main.frame,Year,Article,Chapter),
                  G1=sum(G1),
                  G2=sum(G2),
                  G3=sum(G3),
                  G4=sum(G4),
                  G5=sum(G5),
                  G6=sum(G6),
                  G7=sum(G7),
                  G8=sum(G8),
                  G9=sum(G9),
                  G10=sum(G10),
                  G11=sum(G11),
                  G12=sum(G12),
                  G13=sum(G13),
                  G14=sum(G14),
                  G15=sum(G15),
                  G16=sum(G16),
                  G17=sum(G17),
                  G18=sum(G18),
                  G19=sum(G19),
                  G20=sum(G20),
                  G21=sum(G21),
                  G22=sum(G22),
                  G23=sum(G23),
                  G24=sum(G24),
                  G25=sum(G25),
                  G26=sum(G26),
                  G27=sum(G27),
                  G28=sum(G28),
                  G29=sum(G29),
                  G30=sum(G30),
                  G31=sum(G31),
                  G32=sum(G32),
                  G33=sum(G33),
                  G34=sum(G34),
                  G35=sum(G35),
                  G36=sum(G36),
                  G37=sum(G37),
                  G38=sum(G38),
                  G39=sum(G39),
                  G40=sum(G40),
                  G41=sum(G41),
                  G42=sum(G42),
                  G43=sum(G43),
                  G44=sum(G44),
                  G45=sum(G45),
                  G46=sum(G46),
                  G47=sum(G47),
                  G48=sum(G48),
                  G49=sum(G49),
                  G50=sum(G50),
                  G51=sum(G51),
                  G52=sum(G52),
                  G53=sum(G53),
                  G54=sum(G54),
                  G55=sum(G55),
                  G56=sum(G56),
                  G57=sum(G57),
                  G58=sum(G58),
                  G59=sum(G59),
                  G60=sum(G60),
                  G61=sum(G61),
                  G62=sum(G62),
                  G63=sum(G63),
                  G64=sum(G64),
                  G65=sum(G65))
court2<-subset(court2,!is.na(Chapter))
court2<-subset(court2,!str_detect(Article,"Others"))
court2$Article<-sub("Article", "",court2$Article)
court2$Article<-gsub(" ","",court2$Article)

court1$Chapter<-as.character(court1$Chapter)
court2$Chapter<-as.character(court2$Chapter)


name2<-data.frame(nazva=names_numbers$names,
                  Article=names_numbers$numbers)


total_data<-merge(court1,court2,by=c("Year","Chapter", "Article"),all = TRUE)
total_data<-merge(erdr,total_data,by=c("Year","Chapter", "Article"),all = TRUE)

total_data<-merge(name2,total_data,by="Article")
total_data<-arrange(total_data,Year,Article)

crimes<-total_data
save (crimes,file="crimes.RData")
write.csv(names(crimes),"names_crimes.csv")


