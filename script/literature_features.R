library(tidyverse)
library(stringr)
library(magrittr)

base_dir <- rprojroot::find_root("README.md")

if (!exists("corpus")) corpus <- readRDS(file.path(base_dir, "data", "corpus.RDS"))

if (!exists("annotated_corpus")) annotated_corpus <- readRDS(file.path(base_dir, "data", "annotated_corpus.RDS"))

if (!exists("features")) features <- tibble(id = corpus$id, text_length = nchar(corpus$text), is_conspiracy = ifelse(corpus$site %in% c("conrebbi", "deutschlandpranger", "fm-tv", "hinterderfichte", "Watergate.tv", "Alles Schall und Rauch", "recentr"), 1, 0))

#### Sentiments

sentiments <- bind_rows(read_tsv(file.path(base_dir, "data", "SentiWS_v2.0_Negative.txt"), col_names = FALSE),
                        read_tsv(file.path(base_dir, "data", "SentiWS_v2.0_Positive.txt"), col_names = FALSE))

sentiments %<>% mutate(pos = str_replace(str_extract(.data$X1, "\\|.*$"), fixed("|"), "")) %>%
    mutate(X1  = gsub("\\|.*$", "", .data$X1))

sentiments <- bind_rows(sentiments %>%
                            select(.data$X1, .data$X2, .data$pos),
                        separate_rows(sentiments[, c("X2", "X3", "pos")], X3, sep = ",") %>%
                            rename(X1 = X3))

sentiments <- inner_join(annotated_corpus$token %>%
                             select(.data$doc_id, .data$lemma, .data$xpos) %>%
                             mutate(lemma = tolower(.data$lemma)) %>%
                             mutate(xpos = recode(.data$xpos,
                                                  ADJA = "ADJX",
                                                  ADJD = "ADJX")),
                         sentiments %>%
                             mutate(X1 = tolower(.data$X1)),
                         by = c("lemma" = "X1"),
                                "xpos" = "pos")

sentiments %<>% select(doc_id, X2) %>%
    group_by(doc_id) %>%
    summarise(sentiments_sum = sum(X2),
              sentiments_absolute_sum = sum(abs(X2)),
              sentiments_positive_sum = sum(X2[X2 > 0]),
              sentiments_negative_sum = sum(X2[X2 < 0]))

features %<>% left_join(sentiments, by = c("id" = "doc_id"))

#### Negationen

negations <- c("kein", "keiner", "nein", "nicht", "nichts", "nie",
               "niemals", "niemand", "nirgends", "nirgendwo", "nirgendwoher",
               "nirgendwohin", "keinesfalls", "keineswegs", "mitnichten")

negations <- annotated_corpus$token %>%
    select(doc_id, lemma) %>%
    filter(lemma %in% negations) %>%
    count(doc_id, name = "negations_count")

features %<>% left_join(negations, by = c("id" = "doc_id"))

#### Question marks

question_marks <- annotated_corpus$token %>%
    select(doc_id, lemma) %>%
    filter(lemma == "?") %>%
    count(doc_id, name = "question_marks")

features %<>% left_join(question_marks, by = c("id" = "doc_id"))

#### Take a closer look at quoted stuff

features$quotation_marks_count <- str_count(corpus$text, "['\"“”‘’„”«»]")


quote_regex <- paste0(c("\"", "'", "“", "‘", "„", "«"),
                      ".+?",
                      c("\"", "'", "”", "’", "”", "»"))

quoted_text <- lapply(corpus$text, function(x) {
    quotes <- unlist(str_extract_all(x, quote_regex))

    # The question presenting itself is how we want to deal with nested quotes.
    # Most likely it depends. To calculate the share of quoted text we probably want to
    # only look at top level quotes, for things like scare quotes we may want to look at everything

    # We define scare quotes as a quote that does not contain a whitespace
    # That's not highly accuarate but should be good enough

    number_scare_quotes <- sum(!str_detect(quotes, "[[:space:]]"))

    number_quotes_total <- length(quotes)

    # We check if every string is a substring of another. This is not ide

    is_substr <- sapply(seq_along(quotes), function(x) {
        any(str_detect(quotes[-x], fixed(quotes[x])))
    })

    quotes <- quotes[!is_substr]

    number_toplevel_quotes <- length(quotes)

    characters <- nchar(quotes)

    average_quote_length <- mean(characters)

    number_quoted_characters <- sum(characters)

    data.frame(number_scare_quotes, number_quotes_total, number_toplevel_quotes, average_quote_length, number_quoted_characters)

}) %>% bind_rows

features <- cbind(features, quoted_text)

# Count numbers

features$numbers_count <- str_count(corpus$text, "[:digit:]+")

# Scale everything and stuff

features$average_quote_length[is.nan(features$average_quote_length)] <- 0

features %<>% replace(is.na(.), 0) %>%
    mutate(across(!any_of(c("id", "text_length", "is_conspiracy")), ~ .x / text_length * 100))

# saveRDS(features, file.path(base_dir, "data", "features_literatur.RDS"))

rm(sentiments, negations, question_marks, quote_regex, quoted_text)
gc()
