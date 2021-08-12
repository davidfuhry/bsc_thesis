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

wilcox.test(features$sentiments_sum[features$is_conspiracy == 1], features$sentiments_sum[features$is_conspiracy == 0])
mean(features$sentiments_sum[features$is_conspiracy == 1], na.rm = TRUE)
sd(features$sentiments_sum[features$is_conspiracy == 1], na.rm = TRUE)
mean(features$sentiments_sum[features$is_conspiracy == 0], na.rm = TRUE)
sd(features$sentiments_sum[features$is_conspiracy == 0], na.rm = TRUE)

#### Frequency plot of publishing dates for conspiracy articles

consp_data <- corpus %>%
    filter(is_conspiracy == 1) %>%
    mutate(date = format(date, "%Y-%m")) %>%
    count(date) %>%
    mutate(date = lubridate::ym(date))

ggplot(consp_data, aes(x = date, y = n)) +
    geom_line() +
    labs(x = "Veröffentlichungsdatum", y = "Anzahl Artikel") +
    theme(text = element_text(size = 18))

ggsave("report/graphics/cons_freq_time.jpg")


# Importance plot

top_10_imp <- importance[1:10, ] %>%
    mutate(Feature = forcats::fct_reorder(Feature, Gain))
ggplot(top_10_imp, aes(x = Gain, y = Feature)) + geom_bar(stat = "identity") + theme(text = element_text(size = 18))

ggsave("report/graphics/top_10_features.jpg")

# Make confusion matrix

predictedClass <- factor(c(0,0,1,1))
actualClass <- factor(c(0,1,0,1))
Y <- c(33288, 368, 634, 16468)
df <- data.frame(predictedClass, actualClass, Y)
ggplot(data =  df, mapping = aes(x = predictedClass, y = actualClass)) +
    geom_tile(aes(fill = Y), colour = "white") +
    geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 0.5, size = 8) +
    theme(legend.position = "none", text = element_text(size = 18)) +
    scale_fill_gradient(low = "#D6EAF8", high = "#2E86C1") +
    xlab("Vorhergesagte Klasse") +
    ylab("Tatsächliche Klasse")

ggsave("report/graphics/confusion_matrix.jpg")


