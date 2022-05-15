#Скрипт для отримання та очищення статистичних даних Державної судової адміністрації
library (RColorBrewer)
library (xlsx)
library (stringr)
library(readxl)
library (tidyverse)

colT13_17<-"numeric text text text"
for (ct in 1:65) colT13_17<-paste (colT13_17,"numeric", sep = " ")

colT18_20<-"numeric text text text"
for (ct in 1:67) colT18_20<-paste (colT18_20,"numeric", sep = " ")

TNames13_17<-"Year Article Part Chapter G1 G2 G3 G4 G5 G6 G7 G8 G9 G10 G11 G12 G13 G14 G15 G16 G17 G18 G19 G20 G21 G22 G23 G24 G25 G26 G27 G28 G29 G30 G31 G32 G33 G34 G35 G36 G37 G38 G39 G40 G41 G42 G43 G44 G45 G46 G47 G48 G49 G50 G51 G52 G53 G54 G55 G56 G57 G58 G59 G60 G61 G62 G63 G64 G65"
TNames18_20<-"Year Article Part Chapter G1 G2 G3 G4 G5 G6 G7 G8 G9 G10 G11 G12 G13 G14 G15 G16 G17 G18 G185 G187 G19 G20 G21 G22 G23 G24 G25 G26 G27 G28 G29 G30 G31 G32 G33 G34 G35 G36 G37 G38 G39 G40 G41 G42 G43 G44 G45 G46 G47 G48 G49 G50 G51 G52 G53 G54 G55 G56 G57 G58 G59 G60 G61 G62 G63 G64 G65"



chapter_ord<-c("I","II","III","IV","V","VI","VII","VIII","IX","X",
               "XI","XII","XIII","XIV","XV","XVI","XVII","XVIII","XIX","XX")   
invert_chapter<-rep("",20)
for (i in 1:20)
  invert_chapter[i]<-chapter_ord[21-i]

ymin<-2013
ymax<-2021


special_court<-data.frame (
  Year=ymin:ymax,
  http=c("https://court.gov.ua/userfiles/7_2013.xls",
         "https://court.gov.ua/userfiles/7_2014.xls",
         "https://court.gov.ua/userfiles/7_2015.xls",
         "https://court.gov.ua/userfiles/file/DSA/DSA_2017_all_docs/FEBRUARY_17/statistika_16/Copi_7_7_16.xls",
         "https://court.gov.ua/userfiles/file/DSA/2018_DSA_docs/7_2017.xlsx",
         "https://court.gov.ua/userfiles/media/media/7_2018.xlsx",
         "https://court.gov.ua/userfiles/media/dsa_pres_slujba_2019/dsa_pres_slujba_2020/7_2019.xlsx",
         "https://court.gov.ua/userfiles/media/new_folder_for_uploads/main_site/7_2020.xlsx",
         "https://court.gov.ua/userfiles/media/new_folder_for_uploads/main_site/7_2021.xlsx"),
  Trange=c("A13:BQ888",
           "A14:BQ909",
           "A14:BQ962",
           "A13:BQ964",
           "A14:BQ967",
           "A13:BS971",
           "A13:BS990",
           "A13:BS1013",
           "A13:BS1041"),
  Trange2=c("A974:BQ974",
            "A983:BQ983",
            "A963:BQ963",
            "A1050:BQ1050",
            "A968:BQ968",
            "A972:BS972",
            "A991:BS991",
            "A1014:BS1014",
            "A1042:BS1042"),
  TNames=c(TNames13_17,
           TNames13_17,
           TNames13_17,
           TNames13_17,
           TNames13_17,
           TNames18_20,
           TNames18_20,
           TNames18_20,
           TNames18_20),
  colTypes=c(colT13_17,
             colT13_17,
             colT13_17,
             colT13_17,
             colT13_17,
             colT18_20,
             colT18_20,
             colT18_20,
             colT18_20)
)



#Завантаження статистичних звітів з офіційних джерел


for (yr in ymin:ymax) 
{
  if (yr<2017) {ext<-".xls"} else {ext<-".xlsx"}
  
  filename<-paste("RAW7/raw_court",as.character(yr),ext,sep="")
  if (!file.exists(filename))
  {
    fileURL <-special_court$http[special_court$Year==yr] 
    download.file(fileURL, filename, method = "curl")
  }
}


#Очищення даних, генерація CSV файлів
check.frame=data.frame()
check.counter<-1
listcourt<-list.files(path="RAW7/")

main.frame<- data.frame(matrix(NA, ncol = 71, nrow = 0))

for (iy in 1:length(listcourt))
{
  
  readxl<-paste("RAW7/",listcourt[iy],sep="")
  Year<-special_court$Year[iy]
  names_y<-unlist(str_split(special_court$TNames[iy], pattern = " "))
  colT<-unlist(str_split(special_court$colTypes[iy],pattern = " "))
  
  
  court_y <- read_excel(readxl, sheet = "Форма 7", 
                        range = special_court$Trange[iy], col_names = FALSE, 
                        col_types = colT) 
  
  court_y2 <- read_excel(readxl, sheet = "Форма 7", 
                         range = special_court$Trange2[iy], col_names = FALSE, 
                         col_types = colT) 
  
  court_y<-rbind (court_y,court_y2)
  
  
  
  names(court_y)<-names_y
  court_y$Year<-Year
  court_y$Article[nrow(court_y)]<-"Criminal Code 1960"
  court_y$Part[nrow(court_y)]<-0
  court_y$Chapter[nrow(court_y)]<-0
  ch<-1
  for (i in 2:nrow(court_y))
  {
    if (str_detect(court_y$Article[i],"ст.")&!str_detect(court_y$Article[i-1],"ст."))
    {ch<-ch+1
    } else
      
    {
      if ((str_detect(court_y$Article[i],"ч.")))
      {
        court_y$Article[i]<-sub ("ч.","",court_y$Article[i])
        court_y$Part[i]<-word (court_y$Article[i],-1)
      }
      
      if (i<nrow(court_y)) 
      {court_y$Article[i]<-paste ("Article ",word (court_y$Article[i],1))
      court_y$Chapter[i]<-chapter_ord[ch]}
      
    }
  }
  
  court_y<-subset(court_y,!str_detect(court_y$Article,"ст."))
  court_y$Part<-as.numeric(court_y$Part)
  court_y[is.na(court_y)]<-0
  
  
  
  court_y$Part[nrow(court_y)]<-NA
  court_y$Chapter[nrow(court_y)]<-NA
  court_y$Article[court_y$Article=="Article  364-01"]<-"Article  364-1"
  fileread<-paste("DATA/",as.character(Year),"Court.csv",sep="")
  write.csv(court_y,file=fileread,row.names = FALSE)
  
  #Створення фрейму для контрольної перевірки сум
  
  if (Year<2018) colnumb<-65 else colnumb<-67 
  
  for (chkp in 1:colnumb) 
  {
    check.frame[check.counter,1]<-Year
    check.frame[check.counter,2]<-paste("G",chkp)
    check.frame[check.counter,3]<-sum(court_y[,chkp+4])
    check.counter<-check.counter+1
  }
  
  #Формування головного фрейму даних  
  if (Year==2013) {main.names<-c(names(court_y),"G185","G187")
  names(main.frame)<-main.names}
  if (Year<2018){court_y$G185<-0
  court_y$G187<-0
  } else
  {court_yp1<-select (court_y,1:22,25:71)
  court_yp2<-select (court_y, 23:24)
  court_y<-cbind(court_yp1, court_yp2)}
  
  main.frame<-rbind (main.frame,court_y)
  
  
}

names (check.frame)<-c("Year","Column","Sum")
fileread<-"DATA/check.csv"
write.csv(check.frame,file=fileread,row.names = FALSE)

fileread<-"DATA/dataframe.csv"
write.csv(main.frame,file=fileread,row.names = FALSE)

