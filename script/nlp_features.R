# POS Features

library(tidyverse)
library(magrittr)

base_dir <- rprojroot::find_root("README.md")

if (!exists("corpus")) corpus <- readRDS(file.path(base_dir, "data", "corpus.RDS"))

if (!exists("annotated_corpus")) annotated_corpus <- readRDS(file.path(base_dir, "data", "annotated_corpus.RDS"))

if (!exists("features")) features <- tibble(id = corpus$id, text_length = nchar(corpus$text), is_conspiracy = ifelse(corpus$site %in% c("conrebbi", "deutschlandpranger", "fm-tv", "hinterderfichte", "Watergate.tv", "Alles Schall und Rauch", "recentr"), 1, 0))

# We need these later

cols_original <- colnames(features)

# These are a lot of useless features and we should drop quite a few of them

xpos_tags <- annotated_corpus$token %>%
    select(doc_id, xpos) %>%
    mutate(n = 1) %>%
    pivot_wider(names_from = xpos, values_from = n, values_fn = sum) %>%
    replace(is.na(.), 0)

xpos_tags <- xpos_tags[, c(TRUE, colSums(xpos_tags[, -1]) > 50000)]

features %<>% left_join(xpos_tags, by = c("id" = "doc_id"))

# Word density

## This is not the most acurate way to calculate this (due to the spacy pos tagger not beeing that great for german)
## but it should be good enough

word_count <- annotated_corpus$token %>%
    filter(upos != "PUNCT") %>%
    count(doc_id) %>%
    rename(word_count = n)

features %<>% left_join(word_count, by = c("id" = "doc_id"))

# Share of uppercase characters

features$number_uppercase <- str_count(corpus$text, "[[:upper:]]")

features$special_char_count <- stringr::str_count(corpus$text, regex("[^-'0-9a-zÀ-ÿ[:space:]]", ignore_case = TRUE))

features %<>% replace(is.na(.), 0) %>%
    mutate(across(!any_of(c(cols_original, "word_count")), ~ .x / text_length * 100)) %>%
    mutate(word_density = text_length / word_count) %>%
    select(-word_count) %>%
    rename(comma = `$,`) %>%
    rename(interpunctuation = `$(`) %>%
    rename(colon = `$.`)

rm(cols_original, xpos_tags, word_count, annotated_corpus)
gc()
