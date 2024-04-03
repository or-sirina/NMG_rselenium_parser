library(httr)
library(rvest)
library(stringr)
library(dplyr)
library(RSelenium)

#ссылка на сайт
url_base <- "https://nation.africa/kenya"
webarch <- "https://web.archive.org/web/20230201000000*/"
url <- paste(webarch, url_base, sep = "")

#функция проверки работы браузера
check_browser <- function(u){
  check <- try(suppressMessages(remDr$getCurrentUrl()), silent = TRUE)
  if ("try-error" %in% class(check)) {
    remDr$open(silent = TRUE)
    remDr$navigate(u)
  }
}

#запускаем один раз за сессию
rD <- rsDriver(browser = "firefox", chromever = "114.0.5735.90")
remDr <- rD[["client"]]

#счетчик бекапов
zzz <- 4
#df <- data.frame()
while (TRUE) {
  cur_url <- url
  print('проверяю работу браузера')
  check_browser(cur_url)
  
  while (TRUE) {
    tryCatch({
      print('перехожу на веб архив')
      remDr$navigate(url)
      Sys.sleep(5)
      print('смотрю количество бекапов')
      address_element <- remDr$findElements(using = 'css',
                                            value = 'div.calendar-day')
    }, error = function(e){
      zzz <- zzz + 1
    })
    if(length(address_element) != 0) {
      break
    }
  }
  
  #выгружаем бекапы по дням
  for (i in 55:length(address_element)) {
    error <- 1
    #бекапы по дню
    while (TRUE) {
      tryCatch({
        remDr$navigate(url)
        error <- 1
        #получаем текущий юрл для дебаггинга
        cur_url <- remDr$getCurrentUrl() %>% unlist()
        print('проверяю работу браузера')
        check_browser(cur_url)
        while (TRUE) {
          tryCatch({
            address_element <- remDr$findElements(using = 'css',
                                                  value = 'div.calendar-day')
          }, error = function(e){
            Sys.sleep(5)
          })
          if(length(address_element) != 0) {
            error <- error + 1
            break
          }
        }
        print('проверяю работу браузера')
        check_browser(cur_url)
        print(paste("выгружаю бекапы по дню", i))
        if (error > 5) {
          remDr$navigate(url)
        }
        Sys.sleep(6)
        remDr$mouseMoveToLocation(webElement = address_element[[i]])
        Sys.sleep(4)
        links <- remDr$findElements(using = 'css',
                                    value = 'a.s2xx')
        links <- lapply(links, function(x) { x$getElementAttribute('href') }) %>%
          unlist()
      }, error = function(e){
        if (error > 5) {
          remDr$navigate(url)
        }
        print("жду прогрузки страницы")
      })
      if(length(links) != 0) {
        break
      }
      print('проверяю работу браузера')
      check_browser(cur_url)
    }
    #открываем каждый бекап по дню
    for (y in 1:1) {
      print(y)
      error <- 1
      cur_url <- links[y]
      print('проверяю работу браузера')
      check_browser(cur_url)
      while (TRUE) {
        tryCatch({
          print("открываю ссылки на кеш")
          remDr$navigate(links[y])
          Sys.sleep(5)
          webElem <- remDr$findElement("partial link text", "News")
          cur_url <- webElem$getElementAttribute('href') %>% unlist()
          webElem$clickElement()
          print("перехожу к вкладке новости")
          Sys.sleep(5)
          if (length(webElem) != 0) {
            break
          }
          if (error > 10) {
            break
          }
        }, error = function(e){
          error <- error + 1
          print("жду прогрузки страницы")
        })
      } 
      if (error > 10) {
        next
      }
      while (TRUE) {
        tryCatch({
          news <- remDr$findElements(using = 'css',
                                     value = 'a[tentacle-id]')
          news <- lapply(news, function(x) { x$getElementAttribute('href') }) %>%
            unlist()
          if (length(news) != 0) {
            break
          }
        }, error = function(e) {
          print('проверяю работу браузера')
          check_browser(cur_url)
          remDr$navigate(cur_url)
        })
      }
      news <- news[which(str_detect(news, "news"))]
      for (nn in 1:length(news)) {
        error <- 1
        print(paste('извлекаю новость', nn))
        while (TRUE) {
          tryCatch({
            remDr$navigate(news[nn])
            error <- error + 1
            print(error)
            if (error > 10) {
              break
            }
            Sys.sleep(4)
            texts <- remDr$findElements("css", ".paragraph-wrapper p")
            if(length(texts) == 0) {
              text <- NA
            } else {
              text <- c()
              for (x in 1:length(texts)) {
                text <- c(text, texts[[x]]$getElementText() %>% unlist())
              }
              text <- text[text != ""] %>% paste(collapse = " ")
            }
            date <- remDr$findElement("css", ".date")$getElementAttribute("datetime") %>%
              unlist()
            source <- remDr$getCurrentUrl() %>% unlist()
            df <- rbind(df, data.frame(date,
                                       source,
                                       text))
            write.csv(df, "df.csv")
            break
            
          }, error = function(e) {
            if (error > 10) {
              break
            }
          })
        }
      }
    }
  }
}





