library(dplyr)
library(magrittr)
library(stringr)

base_dir <- rprojroot::find_root("README.md")

corpus <- readRDS(file.path(base_dir, "data", "corpus_full.RDS"))

corpus <- do.call(rbind, corpus)

# Filter out texts we dont want to use

corpus <- corpus %>%
    filter(text != "") %>%
    mutate(text = stringi::stri_trans_nfc(text))

corpus <- corpus[nchar(corpus$text) > 99, ]

langs <- cld3::detect_language(corpus$text)

langs[is.na(langs)] <- "de"

corpus <- corpus[langs == "de", ]

# Do some more filtering

features <- data.frame(id = corpus$id,
                       text_length = nchar(corpus$text),
                       is_conspiracy = ifelse(corpus$site %in% c("conrebbi", "deutschlandpranger", "fm-tv", "hinterderfichte", "Watergate.tv", "Alles Schall und Rauch", "recentr"), 1, 0))

chars <- corpus$text %>%
    paste(collapse = " ") %>%
    tokenizers::tokenize_characters(strip_non_alphanum = FALSE, simplify = TRUE) %>%
    table %>%
    data.frame

regex <- paste0(" (", paste(chars[chars$Freq < 1000, ]$., collapse = "|") ,")+ ")

corpus$text %<>% str_replace_all(emo::ji_rx, "") %>%
    str_replace_all(regex, " ") %>%
    str_replace_all("_+", "") %>%
    str_replace_all("[[:space:]]+", " ") %>%
    str_trim

rm(regex, chars, langs)
gc()
