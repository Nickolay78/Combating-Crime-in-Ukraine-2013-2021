# Карчевський М.В. Інтерактивний довідник  "Протидія злочинності в Україні (2013-2021)"
## Вхідні дані, очищені дані, скрипти збирання, очищення, обробки даних та побудови візуалізацій для веб-застосунку  

Доступ до веб-застосунку за адресою https://karchevskiy.org/i-dovidnyk/


## Зміст репозитарію
### Скрипти в R
+ **first.R** - забезпечує послідовне виконання всіх скритів проєкту  
+ **get-proceed-COURT.R** - збір та "очищення" даних Державної судової адміністрації України (форма 6) 
+ **get.R** - збір та "очищення" даних Державної судової адміністрації України (форма 7)  
+ **get-proceed-PG0.R** - збір та "очищення" даних Офісу Генерального Прокурора України (форма 1)  
+ **load5.R** - завантаження робочої бази даних  
+ **plot1-2.R, plot3.R, plot4-6.R, plot7-9.R, plot10-17.R, common_gr_juv_intox_recid.R, plot18-20.R, plot21-23.R, gender.R, group.R, ozg-group.R, age.R, prof.R, education.R, sudymist.R, sudymist123.R, sudymist-othr.R, plot_chapter2.R, chapter-gender.R, codeC.R, plot_article2.R** - скрипти побудови візуалізацій
+ **Combating-Crime-in-Ukraine-2013-2021.Rproj** - файл проєкту для середовища R
### Довідники
+ **vocab_court.pdf** - довідник змісту «очищених» даних, отриманих з звітів Державної судової адміністрації України (форма 6)
+ **vocab7.pdf** - довідник змісту «очищених» даних, отриманих з звітів Державної судової адміністрації України (форма 7)
+ **vocab_pgo.pdf** - довідник змісту «очищених» даних, отриманих з звітів Офісу Генерального Прокурора України (форма 1)
### Первинні дані
+ **RAW_COURT** - звіти ДСА України за 2013-2020 роки (форма 6)
+ **RAW7** - звіти ДСА України за 2013-2020 роки (форма 7)
+ **RAW_PGO** - звіти Офісу Генерального Прокурора про обліковані провадження за 2013-2020 роки (форма 1)
### "Очищені" дані
+ **DATA_COURT** - придатні до автоматизованої обробки дані ДСА (форма 6, CSV)
+ **DATA** - придатні до автоматизованої обробки дані ДСА (форма 7, CSV) 
+ **DATA_PGO** - придатні до автоматизованої обробки дані Офісу Генерального Прокурора (форма 1, CSV)
## Як відтворити дослідження
1. Завантажити R (весія не нижче 4.0.5)
2. Завантажити RStudio
3. Розмістити вміст репозитарію до локальної, на Вашому комп'ютері :), теки нового проєкту. Інший спосіб - використовуйте Git Version Control.
4. Завантажити (перевірити наявність) наступних бібліотек R: ***beepr, xlsx, stringr, readxl, tidyverse, scales, stingi, cowplot, magick***
5. Видалити вміст тек з первинними, "очищеними" даними у локальній теці нового проєкту
6. Виконати скрипт **first.R**
7. Після закінчення виконання теки вхідних та "очищених" даних будуть містити відповідні дані, тека GRAPH - створені візуалізації

### Бажаю успіхів та приємної роботи!  
### Микола Карчевський  
### **comcriminal@gmail.com**
### **https://karchevskiy.org/**
#### (c) Карчевський М.В., 2022
