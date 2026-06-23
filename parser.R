###############################################################################
# Сбор новостей nation.africa/kenya из снапшотов Wayback Machine
#
# Логика: открываем календарь снапшотов за нужный период -> для каждого дня
# наводимся на ячейку, забираем ссылки на снапшоты -> в снапшоте переходим
# на вкладку News -> собираем ссылки на статьи -> из каждой статьи извлекаем
# дату, источник и текст. Результат пишется в CSV инкрементально.
#
# Запуск идемпотентен: уже собранные URL (по полю source) пропускаются,
# поэтому после падения можно просто перезапустить скрипт.
###############################################################################

suppressPackageStartupMessages({
  library(RSelenium)
  library(stringr)
})

# ----------------------------------------------------------------------------
# Конфигурация
# ----------------------------------------------------------------------------
cfg <- list(
  base_url        = "https://nation.africa/kenya",
  wayback_prefix  = "https://web.archive.org/web/20230201000000*/",
  out_csv         = "df.csv",

  start_day       = 1L,    # с какого дня календаря начинать (для дозабора)
  snapshots_per_day = 1L,  # сколько снапшотов брать на день

  page_wait       = 5,     # пауза после navigate(), сек
  hover_wait      = 4,     # пауза после наведения на ячейку дня
  article_wait    = 4,     # пауза на прогрузку статьи

  max_tries       = 10,    # попыток на один шаг
  retry_wait      = 3      # пауза между попытками, сек
)

cfg$calendar_url <- paste0(cfg$wayback_prefix, cfg$base_url)

# ----------------------------------------------------------------------------
# Утилиты
# ----------------------------------------------------------------------------

log_msg <- function(...) {
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), sprintf(...)))
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x[1])) y else x

# Универсальный ретрай. Заменяет все вложенные while(TRUE)+tryCatch.
# fn   - функция без аргументов, выполняющая шаг
# valid - предикат: считается ли результат успешным
# Возвращает результат или NULL, если попытки исчерпаны.
retry <- function(fn,
                  valid = function(x) !is.null(x) && length(x) > 0,
                  max_tries = cfg$max_tries,
                  wait = cfg$retry_wait,
                  label = "step") {
  for (attempt in seq_len(max_tries)) {
    result <- tryCatch(fn(), error = function(e) {
      log_msg("[%s] попытка %d/%d — ошибка: %s",
              label, attempt, max_tries, conditionMessage(e))
      NULL
    })
    if (!is.null(result) && isTRUE(valid(result))) return(result)
    if (attempt < max_tries) Sys.sleep(wait)
  }
  log_msg("[%s] не удалось за %d попыток — пропускаю", label, max_tries)
  NULL
}

# Безопасное чтение атрибута/текста элемента (RSelenium возвращает list()).
attr_or_na <- function(el, name) {
  v <- el$getElementAttribute(name)
  if (length(v) == 0) NA_character_ else v[[1]]
}

hrefs_of <- function(elements) {
  vapply(elements, function(e) attr_or_na(e, "href"), character(1))
}

# Дозапись одной строки в CSV: заголовок пишется только при создании файла.
append_row <- function(row, path) {
  new_file <- !file.exists(path)
  write.table(row, path, sep = ",", append = !new_file,
              col.names = new_file, row.names = FALSE,
              qmethod = "double", fileEncoding = "UTF-8")
}

# Уже собранные источники — чтобы не дублировать при перезапуске.
load_seen <- function(path) {
  if (!file.exists(path)) return(character(0))
  existing <- tryCatch(read.csv(path, stringsAsFactors = FALSE),
                       error = function(e) NULL)
  if (is.null(existing) || !"source" %in% names(existing)) {
    return(character(0))
  }
  unique(existing$source)
}

# ----------------------------------------------------------------------------
# Управление драйвером
# ----------------------------------------------------------------------------

start_driver <- function() {
  # browser = "firefox" -> chromever не используется (это была ошибка в исходнике).
  rD <- rsDriver(browser = "firefox", verbose = FALSE, check = FALSE)
  rD
}

# Проверяем, что сессия жива; если нет — переоткрываем.
ensure_browser <- function(remDr, fallback_url) {
  alive <- tryCatch({ remDr$getCurrentUrl(); TRUE },
                    error = function(e) FALSE)
  if (!alive) {
    log_msg("сессия упала — переоткрываю браузер")
    remDr$open(silent = TRUE)
    remDr$navigate(fallback_url)
  }
  invisible(alive)
}

# ----------------------------------------------------------------------------
# Шаги парсинга
# ----------------------------------------------------------------------------

# Ссылки на снапшоты конкретного дня календаря.
# Календарь приходится перезагружать на каждый день: после navigate() хэндлы
# элементов протухают, а mouseMove работает только со свежими.
day_snapshot_links <- function(remDr, day_index) {
  ensure_browser(remDr, cfg$calendar_url)

  days <- retry(function() {
    remDr$navigate(cfg$calendar_url)
    Sys.sleep(cfg$page_wait)
    d <- remDr$findElements("css", "div.calendar-day")
    if (length(d) == 0) stop("calendar-day не найдены")
    d
  }, label = "calendar")

  if (is.null(days) || day_index > length(days)) return(character(0))

  links <- retry(function() {
    remDr$mouseMoveToLocation(webElement = days[[day_index]])
    Sys.sleep(cfg$hover_wait)
    a <- remDr$findElements("css", "a.s2xx")
    if (length(a) == 0) stop("ссылки снапшотов не появились")
    hrefs_of(a)
  }, label = sprintf("день %d", day_index))

  links %||% character(0)
}

# Ссылки на статьи внутри одного снапшота (через вкладку News).
news_links_from_snapshot <- function(remDr, snapshot_url) {
  ensure_browser(remDr, snapshot_url)

  ok <- retry(function() {
    remDr$navigate(snapshot_url)
    Sys.sleep(cfg$page_wait)
    tab <- remDr$findElement("partial link text", "News")
    tab$clickElement()
    Sys.sleep(cfg$page_wait)
    TRUE
  }, valid = isTRUE, label = "вкладка News")

  if (is.null(ok)) return(character(0))

  links <- retry(function() {
    a <- remDr$findElements("css", "a[tentacle-id]")
    if (length(a) == 0) stop("список новостей пуст")
    hrefs_of(a)
  }, label = "список новостей")

  links <- links %||% character(0)
  unique(links[str_detect(links, "news")])
}

# Извлечение одной статьи -> data.frame(date, source, text).
scrape_article <- function(remDr, article_url) {
  res <- retry(function() {
    remDr$navigate(article_url)
    Sys.sleep(cfg$article_wait)

    paras <- remDr$findElements("css", ".paragraph-wrapper p")
    text <- if (length(paras) == 0) {
      NA_character_
    } else {
      chunks <- vapply(paras,
                       function(p) p$getElementText()[[1]] %||% "",
                       character(1))
      chunks <- chunks[chunks != ""]
      if (length(chunks) == 0) NA_character_ else paste(chunks, collapse = " ")
    }

    date <- tryCatch(
      attr_or_na(remDr$findElement("css", ".date"), "datetime"),
      error = function(e) NA_character_
    )

    data.frame(
      date   = date %||% NA_character_,
      source = remDr$getCurrentUrl()[[1]],
      text   = text,
      stringsAsFactors = FALSE
    )
  }, valid = function(x) is.data.frame(x), label = "статья")

  res
}

# ----------------------------------------------------------------------------
# Основной сценарий
# ----------------------------------------------------------------------------

main <- function() {
  rD <- start_driver()
  remDr <- rD[["client"]]

  # Гарантированная очистка драйвера при любом выходе.
  on.exit({
    log_msg("закрываю драйвер")
    try(remDr$close(), silent = TRUE)
    try(rD[["server"]]$stop(), silent = TRUE)
  }, add = TRUE)

  seen <- load_seen(cfg$out_csv)
  log_msg("уже собрано источников: %d", length(seen))

  days <- day_snapshot_links(remDr, cfg$start_day)  # прогрев + узнаём, что календарь жив
  # Перебираем дни; число дней определяем заново внутри day_snapshot_links,
  # поэтому идём до тех пор, пока возвращаются ссылки.
  day_index <- cfg$start_day
  repeat {
    snapshots <- day_snapshot_links(remDr, day_index)
    if (length(snapshots) == 0) {
      log_msg("день %d: снапшотов нет — останавливаюсь", day_index)
      break
    }

    snapshots <- head(snapshots, cfg$snapshots_per_day)
    for (snap in snapshots) {
      log_msg("день %d: снапшот %s", day_index, snap)
      articles <- news_links_from_snapshot(remDr, snap)
      log_msg("найдено статей: %d", length(articles))

      for (i in seq_along(articles)) {
        url <- articles[i]
        log_msg("статья %d/%d", i, length(articles))

        row <- scrape_article(remDr, url)
        if (is.null(row)) next

        if (row$source %in% seen) {
          log_msg("уже собрана — пропускаю")
          next
        }

        append_row(row, cfg$out_csv)
        seen <- c(seen, row$source)
      }
    }

    day_index <- day_index + 1L
  }

  log_msg("готово. Всего источников в файле: %d", length(seen))
}

if (sys.nframe() == 0) {
  main()
}library(httr)
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





