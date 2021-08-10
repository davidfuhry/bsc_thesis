library(ggplot2)
library(lubridate)
library(dplyr)
library(magrittr)

dir.create("report/graphics")


corpus$is_conspiracy <- features_html$is_conspiracy

### Stats for corpus table

stats <- corpus %>%
    filter(is_conspiracy == TRUE) %>%
    mutate(nchar = nchar(text)) %>%
    group_by(site) %>%
    add_tally() %>%
    summarise(start_date = min(date), end_date = max(date), avg_length = mean(nchar), n_articles = n_distinct(url)) %>%
    mutate(pub_amount = round(n_articles / (interval(start_date, end_date) %/% months(1)), digits = 1)) %>%
    mutate(avg_length = round(avg_length)) %>%
    mutate(start_date = format(start_date, "%d.%m.%Y")) %>%
    mutate(end_date = format(end_date, "%d.%m.%Y")) %>%
    t %>%
    data.frame

colnames(stats) <- stats[1, ]
stats <- stats[-1, ]

kableExtra::kbl(t(stats), format = "latex", booktabs = TRUE)

### Stats for comparitive Corpus

stats <- corpus %>%
    filter(is_conspiracy == FALSE) %>%
    mutate(nchar = nchar(text)) %>%
    group_by(site) %>%
    add_tally() %>%
    summarise(start_date = min(date, na.rm = TRUE), end_date = max(date, na.rm = TRUE), avg_length = mean(nchar), n_articles = n_distinct(url)) %>%
    mutate(pub_amount = round(n_articles / (interval(start_date, end_date) %/% months(1)), digits = 1)) %>%
    mutate(avg_length = round(avg_length)) %>%
    mutate(start_date = format(start_date, "%d.%m.%Y")) %>%
    mutate(end_date = format(end_date, "%d.%m.%Y")) %>%
    t %>%
    data.frame

colnames(stats) <- stats[1, ]
stats <- stats[-1, ]

kableExtra::kbl(t(stats), format = "latex", booktabs = TRUE)

### Table for features



#### Frequency plot of publishing dates for conspiracy articles

consp_data <- corpus %>%
    filter(is_conspiracy == TRUE) %>%
    mutate(date = format(date, "%Y-%m")) %>%
    count(date) %>%
    mutate(date = lubridate::ym(date))

ggplot(consp_data, aes(x = date, y = n)) +
    geom_line() +
    labs(x = "Veröffentlichungsdatum", y = "Anzahl Artikel")

ggsave("report/graphics/cons_freq_time.jpg")
