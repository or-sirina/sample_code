library(xml2)
library(rvest)  
#загружаем необходимые библиотеки для парсинга
library(tibble)
library (tidyverse)
library(stringr)
library(httr)
library(dplyr)

baseurl1 <- "https://opendatabot.ua/open/large-tax-payers?page=" #базовая ссылка для извлечения ссылок
baseurl2 <- "https://opendatabot.ua" #базования ссылка для парсинга данных предприятия
df <- tibble() #создаем пустую таблицу для записывания результатов 
 #создаем сводную таблицу. Для каждого столбца прописываем команду для парсинга. Переменные для команд называем по предполагаемому названию столбцов
df2 <- data.frame(matrix(ncol = 1, nrow = 0)) #формируем таблицу

#цикл скачивания ссылок на предприятия
for (i in 1:14) { #задаем страницы (их 14, поэтому цикл идет от 1 до 14). Альтернативный способ (более красивый), посчитать количество элементов в тэге перехода страниц
  download.file(paste (baseurl1, i), destfile = "C:\\Users\\User\\scrapedpage.html", quiet=TRUE) #это хитрый прием обхода блокировки proxy. Мы скачиваем веб-страницу и потом уже работаем с файлом, так меньше вероятность ошибки. Команда paste позволяет переключиться между страницами благодаря общей части
  page <- read_html("C:\\Users\\User\\scrapedpage.html") #читаем скачанный html файл
  links <- page %>% html_nodes("table.table a") %>% html_attr("href") #извлекаем только ссылки на предприятия
  links <- data.frame(links) #создаем таблицу из полученных ссылок
  vlinks <- tibble::rowid_to_column(links, "ID") #переводим стандартную таблицу в формат тиббла (таже таблица, только более гибкая в работе) и добавляем нумирацию. Просто для удобства 
  df <- rbind(df, vlinks) #добавляем результаты к пустой таблице
}

j <- nrow(df) #считаем количество строк для определения верхнего потолка второго цикла
chang1 <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(chang1) <- "ID"
colnames(df2) <- "ID"

for (z in 1:j) { #второй цикл для парсинга данных предприятий
  tryCatch({
  download.file(paste0(baseurl2, paste (df[z,2])), destfile = "C:\\Users\\User\\scrapedpage2.html", quiet=TRUE) #это хитрый прием обхода блокировки proxy. Мы скачиваем веб-страницу и потом уже работаем с файлом, так меньше вероятность ошибки. Команда paste позволяет переключиться между страницами благодаря общей части
  page2 <- read_html("C:\\Users\\User\\scrapedpage2.html") #читаем скачанный html файл
  own <- length(page2 %>% html_nodes("div.container .col-sm-4.col-6.col p") %>% html_text()) #у компаний разное количество владельцев, поэтому мы считаем количество элементов вектора, отнимаем 2 (это название и адрес) и пишем цикл внутри цикла для формирования нужного количества полей для владельцев
  chan <- data.frame((page2 %>% html_nodes("div.d-flex table tbody") %>% html_table())) #заранее экспортируем уже готовую таблицу с изменениями
  ch <- nrow(chan) #считаем количество 
  df2[nrow(df2) + 1,] = paste(z)
  chang1[nrow(chang1) + 1,] = paste(z)
  
  for(d in 1:ch) { 
    nam1 <- paste("Date_changes", d, sep = "_")
    nam2 <- paste("Changes", d, sep = "_")
    nam3 <- paste("Before", d, sep = "_")
    nam4 <- paste("After", d, sep = "_")
    chang2 <- tibble( 
      ID = paste(j),
      nam1 = (chan[d,1]),
      nam2 = (chan[d,2]),
      nam3  = (chan[d,3]),
      nam4 = (chan[d,4])
    )
    colnames(chang2) <- c("ID", nam1, nam2, nam3, nam4)
    chang1 <- right_join(chang1, chang2, by = "ID")
  }
  
  fin <- tibble( #создаем сводную таблицу. Для каждого столбца прописываем команду для парсинга. Переменные для команд называем по предполагаемому названию столбцов
    ID = paste(z),
    Name = paste((page2 %>% html_nodes("div.container .col-12.col p") %>% html_text())[1]),
    Adress = paste((page2 %>% html_nodes("div.container .col-12.col p") %>% html_text())[2]),
    Founded = paste((page2 %>% html_nodes("div.container .col-sm-4.col-6.col p") %>% html_text())[1]),
    Director = paste((page2 %>% html_nodes("div.container .col-sm-4.col-6.col p") %>% html_text())[2]),
    Status = paste((page2 %>% html_nodes("div.container .col-sm-4.col-6.col p") %>% html_text())[3]),
    Code = paste((page2 %>% html_nodes("div.container .col-sm-4.col-6.col p") %>% html_text())[4]),
    Auth_Capital = paste((page2 %>% html_nodes("div.container .col-sm-4.col-6.col p") %>% html_text())[5]),
    Activity = paste((page2 %>% html_nodes("div.container .col-sm-4.col-6.col p") %>% html_text())[6]),
    Owner1 = if (own <= 1) {
      paste((page2 %>% html_nodes("div.container .col-12.col p") %>% html_text())[3])
      } else {
        paste("-")
    },
    Owner2 = if (own <= 2) {
      paste((page2 %>% html_nodes("div.container .col-12.col p") %>% html_text())[4])
      } else {
        paste("-")
      },
    Owner3 = if (own <= 3) {
      paste((page2 %>% html_nodes("div.container .col-12.col p") %>% html_text())[5])
      } else {
        paste("-")
      },
    Owner4 = if (own <= 4) {
      paste((page2 %>% html_nodes("div.container .col-12.col p") %>% html_text())[6])
      } else {
        paste("-")
      },
    Owner5 = if (own <= 5) {
      paste((page2 %>% html_nodes("div.container .col-12.col p") %>% html_text())[7])
      } else {
        paste("-")
      },
    Owner6 = if (own <= 6) {
      paste((page2 %>% html_nodes("div.container .col-12.col p") %>% html_text())[8])
      } else {
        paste("-")
      },
    Owner7 = if (own <= 7) {
      paste((page2 %>% html_nodes("div.container .col-12.col p") %>% html_text())[9])
      } else {
        paste("-")
      },
    Owner8 = if (own <= 8) {
      paste((page2 %>% html_nodes("div.container .col-12.col p") %>% html_text())[10]) 
      } else {
        paste("-")
      },
    Owner9 = if (own <= 9) {
      paste((page2 %>% html_nodes("div.container .col-12.col p") %>% html_text())[11])
      } else {
        paste("-")
      },
    Owner10 = if (own <= 10) {
      paste((page2 %>% html_nodes("div.container .col-12.col p") %>% html_text())[12])
      } else {
        paste("-")
      },
    Owner11 = if (own <= 11) {
      paste((page2 %>% html_nodes("div.container .col-12.col p") %>% html_text())[13])
    } else {
      paste("-")
    },
    Owner12 = if (own <= 12) {
      paste((page2 %>% html_nodes("div.container .col-12.col p") %>% html_text())[14])
    } else {
      paste("-")
    },
    Owner13 = if (own <= 13) {
      paste((page2 %>% html_nodes("div.container .col-12.col p") %>% html_text())[15])
    } else {
      paste("-")
    },
    Owner14 = if (own <= 14) {
      paste((page2 %>% html_nodes("div.container .col-12.col p") %>% html_text())[16])
    } else {
      paste("-")
    },
    Owner15 = if (own <= 15) {
      paste((page2 %>% html_nodes("div.container .col-12.col p") %>% html_text())[17])
    } else {
      paste("-")
    },
    Owner16 = if (own <= 16) {
      paste((page2 %>% html_nodes("div.container .col-12.col p") %>% html_text())[18])
    } else {
      paste("-")
    },
    Owner17 = if (own <= 17) {
      paste((page2 %>% html_nodes("div.container .col-12.col p") %>% html_text())[19])
    } else {
      paste("-")
    },
    Owner18 = if (own <= 18) {
      paste((page2 %>% html_nodes("div.container .col-12.col p") %>% html_text())[20])
    } else {
      paste("-")
    },
    Owner19 = if (own <= 19) {
      paste((page2 %>% html_nodes("div.container .col-12.col p") %>% html_text())[21])
    } else {
      paste("-")
    },
    Owner20 = if (own <= 20) {
      paste((page2 %>% html_nodes("div.container .col-12.col p") %>% html_text())[22])
    } else {
      paste("-")
    },
    Income_2022 = paste((page2 %>% html_nodes(".table td") %>% html_text())[2]),
    Netprofit_2022 = paste((page2 %>% html_nodes(".table td") %>% html_text())[4]),
    Actives = paste((page2 %>% html_nodes(".table td") %>% html_text())[6]),
    Obligation = paste((page2 %>% html_nodes(".table td") %>% html_text())[8]),
    PDV = paste((page2 %>% html_nodes("div.row.px-4.vat .col p") %>% html_text()))
  )
  df2 <- inner_join(df2, fin, by = "ID")
  df2 <- inner_join(df2, chang2, by = "ID")
  })}