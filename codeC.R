library (textreadr)
library (stringr)

crime_doc<-read_docx("kodeks-ukrajini-2341-iii-vid-05_04_2001-kriminalnij-kodeks-ukrajini.docx")

number_rows<-c(1:length(crime_doc))

crime_doc2<-data.frame(number=number_rows,
                       cont=crime_doc)

result_art<-data.frame(count=c(1,2),
                      text=c("2","2"),
                      short_n=c("1","1"),
                      header=c("d","d"))
ind<-1
isosob<-FALSE

for (ir in 1:nrow(crime_doc2))
{if (str_detect(crime_doc2[ir,]$cont,"ОСОБЛИВА")) isosob<-TRUE
  
  
  if (isosob&substr(crime_doc2[ir,]$cont,1,6)=="Стаття")
  {result_art[ind,]$count<-ind
  result_art[ind,]$text<-crime_doc2[ir,]$cont
  if (substr(crime_doc2[ir,]$cont,11,11)==".") 
    result_art[ind,]$short_n<-substr(crime_doc2[ir,]$cont,8,10)
      else 
        result_art[ind,]$short_n<-paste(substr(crime_doc2[ir,]$cont,8,10),"-",substr(crime_doc2[ir,]$cont,11,11),sep = "")
  
  ind<-ind+1}

}  


#общая идея - бежит курсор до позиции пока нк обнаруживает нужный пробел и сбрасывает 
#вырезанную подстроку в вектор, потом опять вектор добавляется append


for (ir in 1:nrow(result_art))
{
  
  if (nchar(result_art[ir,2])>81)
    
  {ind<-1
  ind2<-80
  res<-c("")
  
  while (ind2<nchar(result_art[ir,2]))
  {found<-FALSE
  while (!found)
        {
            if (substr(result_art[ir,2],ind2,ind2)==" ")
            {res<-append(res,substr(result_art[ir,2],ind,ind2-1) )
            ind<-ind2
            ind2<-ind2+80
            found<-TRUE}
                else ind2<-ind2-1  
        }
  
  }
  res<-append(res,substr(result_art[ir,2],ind+1,nchar(result_art[ir,2])))
  res<-paste(res[2:length(res)], collapse ="
")
    
      } else res<-result_art[ir,2]

      
      
      if (str_detect(result_art[ir,3],"-")) 
        res<-paste (substr (res,1,10),"-",substr (res,11,nchar(res)),sep="")
      
      result_art[ir,4]<- res
  } 
 



