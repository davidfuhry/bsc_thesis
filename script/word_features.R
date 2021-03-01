library(tidyverse)
library(magrittr)
library(tidytext)

base_dir <- rprojroot::find_root("README.md")

if (!exists("corpus")) corpus <- readRDS(file.path(base_dir, "data", "corpus.RDS"))

if (!exists("annotated_corpus")) annotated_corpus <- readRDS(file.path(base_dir, "data", "annotated_corpus.RDS"))

if (!exists("features")) features <- tibble(id = corpus$id, text_length = nchar(corpus$text), is_conspiracy = ifelse(corpus$site %in% c("conrebbi", "deutschlandpranger", "fm-tv", "hinterderfichte", "Watergate.tv", "Alles Schall und Rauch", "recentr"), 1, 0))

stopwords <- readLines("data/stopwords-de.txt", encoding = "UTF-8")

words <- annotated_corpus$token %>%
    filter(!upos %in% c("X", "SPACE", "PUNCT")) %>%
    select(doc_id, lemma) %>%
    mutate(lemma = tolower(lemma)) %>%
    filter(!lemma %in% tm::stopwords("de")) %>%
    count(doc_id, lemma, sort = TRUE)

doc_word_counts <- words %>%
    group_by(doc_id) %>%
    summarize(total = sum(n))

word_counts <- words %>%
    group_by(lemma) %>%
    summarize(total = sum(n))

word_counts %<>% filter(total > 500) %>%
    filter(stringr::str_detect(lemma, stringr::regex("^[-a-zÀ-ÿ]+$", ignore_case = TRUE))) %>%
    filter(lemma != "-") %>%
    filter(lemma != "dass") %>%
    arrange(desc(total)) %>%
    slice(1:300)

words %<>% filter(lemma %in% word_counts$lemma)

words <- left_join(words, doc_word_counts)

words_tf_idf <- words %>%
    bind_tf_idf(lemma, doc_id, n)

word_features <- words_tf_idf %>%
    select(doc_id, lemma, tf_idf) %>%
    pivot_wider(id_cols = doc_id, names_from = lemma, values_from = tf_idf, values_fill = 0)

features %<>% left_join(word_features, by = c("id" = "doc_id")) %>%
    replace(is.na(.), 0)


rm(annotated_corpus, doc_word_counts, word_counts, word_features, words, words_tf_idf)
