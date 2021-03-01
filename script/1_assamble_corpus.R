## This script reads all the crawled texts and assembles a corpus from them

library(dplyr)
library(lubridate)
library(magrittr)

# Corpora crawled using older functions

files <- c("articles_cr.RDS", "articles_dp_new.RDS", "articles_fmtv.RDS", "articles_hdf.RDS")
names(files) <- c("conrebbi", "deutschlandpranger", "fm-tv", "hinterderfichte")


import_corpus <- function(i) {
    corpus <- readRDS(file.path("data", files[i]))

    colnames(corpus) <- tolower(colnames(corpus))

    colnames(corpus) <- recode(colnames(corpus),
                               "uri" = "url",
                               "resort" = "category")

    if ("date" %in% colnames(corpus)) corpus$date <- corpus$date %>%
        as.integer %>%
        as.POSIXct(origin = "1970-01-01") %>%
        as_date

    cols <- c(category = NA_character_, date = NA_Date_, author = NA_character_)

    corpus <- tibble::add_column(corpus, !!!cols[setdiff(names(cols), names(corpus))])

    corpus$site <- names(files)[i]

    corpus$id <- paste0(names(files)[i], "_", 1:nrow(corpus))

    corpus
}

corpora <- lapply(seq_along(files), import_corpus)

names(corpora) <- names(files)

# Watergate.tv

watergate_new <- readRDS("data/watergate_corpus.RDS")
watergate_new$date <- watergate_new$date %>%
    lubridate::dmy(locale = "German_Germany")

watergate_new$site <- "Watergate.tv"
watergate_new$author <- NA

watergate_old <- readRDS("data/articles_wg.RDS")

colnames(watergate_old) <- tolower(colnames(watergate_old))

watergate_old <- watergate_old[!watergate_old$uri %in% watergate_new$url, ]

watergate_old$category <- NA
watergate_old$author <- NA

colnames(watergate_old)[1] <- "url"

watergate_old$date <- watergate_old$date %>%
    as.integer %>%
    as.POSIXct(origin = "1970-01-01") %>%
    as_date

watergate <- rbind(watergate_new, watergate_old)

watergate$id <- paste0("watergate_", 1:nrow(watergate))

corpora$watergate <- watergate

rm(list = c("watergate_old", "watergate_new", "watergate"))

# Alles Schall und Rauch

asur <- readRDS("data/asur_corpus.RDS")

attr(asur, "problems") <- NULL

asur$author = "freeman"

asur$file_id <- NULL

asur$site <- "Alles Schall und Rauch"

cols <- c(category = NA_character_, date = NA_Date_, author = NA_character_)

asur <- tibble::add_column(asur, !!!cols[setdiff(names(cols), names(asur))])

asur$date <- asur$date %>%
    gsub("^.+?, ", "", .) %>%
    gsub(",.+?$", "", .) %>%
    lubridate::dmy(locale = "German_Germany")

asur$id <- paste0("asur_", 1:nrow(asur))

corpora$asur <- asur

# Frankfurter Rundschau

fr <- readRDS("data/fr_corpus.RDS")

attr(fr, "spec") <- NULL

fr$site <- "Frankfurter Rundschau"

fr <- tibble::add_column(fr, !!!cols[setdiff(names(cols), names(fr))])

fr$date <- fr$date %>%
    lubridate::dmy_hm() %>%
    lubridate::date()

fr$id <- paste0("fr_", 1:nrow(fr))

corpora$fr <- fr

# recentr

recentr <- readRDS("data/recentr_corpus.RDS")

attr(recentr$text, "names") <- NULL

recentr$text <- gsub("\\/\\/.+]]&gt;", "", recentr$text)

recentr$file_id <- NULL

recentr$site <- "recentr"

recentr <- tibble::add_column(recentr, !!!cols[setdiff(names(cols), names(recentr))])

recentr$date %<>% lubridate::dmy(locale = "German_Germany")

recentr$id <- paste0("recentr_", 1:nrow(recentr))

corpora$recentr <- recentr

# scienceblogs

scienceblogs <- readRDS("data/scienceblogs_corpus.RDS")

attr(scienceblogs, "spec") <- NULL

colnames(scienceblogs)[5] <- "category"

scienceblogs$site <- "scienceblogs"

scienceblogs$date %<>% lubridate::dmy(locale = "German_Germany")

scienceblogs$id <- paste0("scienceblogs_", 1:nrow(scienceblogs))

corpora$scienceblogs <- scienceblogs

# scilogs

scilogs <- readRDS("data/scilogs_corpus.RDS")

attr(scilogs, "spec") <- NULL

colnames(scilogs)[c(4,5)] <- c("category", "author")

scilogs$site <- "scilogs"

scilogs$date %<>% lubridate::dmy(locale = "German_Germany")

scilogs$id <- paste0("scilogs_", 1:nrow(scilogs))

corpora$scilogs <- scilogs

# spiegel online

spon <- as.data.frame(readRDS("data/spon_corpus.RDS"))

spon$date %<>% lubridate::dmy()

spon$time <- NULL
spon$spiegel_plus <- NULL

colnames(spon)[4] <- "category"

spon <- tibble::add_column(spon, !!!cols[setdiff(names(cols), names(spon))])

spon$site <- "Spiegel Online"

spon$id <- paste0("spon_", 1:nrow(spon))

corpora$spon <- spon

saveRDS(corpora, "data/corpus_full.RDS")
