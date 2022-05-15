# Скрипт визначає послідовність виконання всіх скриптів проєкту

library (beepr)
library (stringi)
library(cowplot)

exlist<-c("get-proceed-COURT.R",
           "get-proceed-PG0.R",
           "get.R",
           "load5.R",
           "plot1-2.R",
           "plot3.R",
           "plot4-6.R",
           "plot7-9.R",
           "plot10-17.R", 
           "common_gr_juv_intox_recid.R",
           "plot18-20.R",
           "plot21-23.R",
           "gender.R",
           "group.R",
           "ozg-group.R",
           "age.R",
           "prof.R",
           "education.R",
           "sudymist.R",
           "sudymist123.R",
           "sudymist-othr.R",
           "plot_chapter2.R",
           "chapter-gender.R",
          "codeC.R",
          "plot_article2.R")
 
  
                           
start<-Sys.time()
  
for (pow in 1:length(exlist))
    {source(exlist[pow],encoding = "UTF-8",echo = TRUE)}

beep(5)
print (c(paste("Роботу розпочато: ",start," Роботу закінчено ",Sys.time())))

