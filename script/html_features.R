library(rvest)
library(tidyverse)
library(digest)
library(stringr)
library(httr)
library(doParallel)
library(magrittr)

base_dir <- rprojroot::find_root("README.md")

if (!exists("corpus")) corpus <- readRDS(file.path(base_dir, "data", "corpus.RDS"))

data_dir <- "~/R-Projects/conspiracy/Data"

config <- list(`Alles Schall und Rauch` = list(folder = "ASuR",
                                               content_selector = ".post"),
               conrebbi = list(folder = "conrebbi",
                               content_selector = ".postcontent"),
               deutschlandpranger = list(folder = "deutschlandpranger",
                                         content_selector = ".evo_post__full_text"),
               `fm-tv` = list(folder = "fmtv",
                              content_selector = ".entry"),
               hinterderfichte = list(folder = "hinterderfichte",
                                      content_selector = ".entry-content"),
               `Watergate.tv` = list(folder = "watergate",
                                     content_selector = ".entry-content, .attachment-content-single"),
               `Frankfurter Rundschau` = list(folder = "FR",
                                              content_selector = ".lp_article_content",
                                              remove_selector = ".id-AuthorList"),
               recentr = list(folder = "recentr",
                              content_selector = ".entry-content"),
               scienceblogs = list(folder = "scienceblogs",
                                   content_selector = ".entry-content",
                                   remove_selector = "#twiago_wm, .asmbeacon"),
               scilogs = list(folder = "scilog",
                              content_selector = ".post__content--is-content",
                              remove_selector = ".shariff"),
               `Spiegel Online` = list(folder = "SPON",
                                       content_selector = 'div[data-article-el="body"]>section.relative',
                                       remove_selector = ".clear-both, .clear-right, .clear-left, .gujAd"))

rows <- split(corpus[, c("url", "site")], seq(nrow(corpus)))

extract_html_features <- function(row, data_dir, config) {
    url <- row$url
    site <- row$site

    # message("Processing: ", url)

    id <- digest::digest(url, algo = "md5")
    file_path <- file.path(data_dir, config[[site]][["folder"]], "raw", paste0(id, ".html"))
    if (!file.exists(file_path)) {
        message("WARNING: About to download '", url, "'")
        if (site == "deutschlandpranger") {
            page <- httr::GET(url,
                              user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/88.0.4324.190 Safari/537.36"))
        } else {
            page <- httr::GET(url,
                              use_proxy("socks5://localhost:9050"),
                              user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/88.0.4324.190 Safari/537.36"))
        }
        page <- httr::content(page)
        # Save the html to disk before we do anything else
        invisible(write_html(page, file_path))
    } else {
        page <- xml2::read_html(file_path, encoding = "UTF-8")
    }

    to_remove <- config[[site]][["remove_selector"]]
    if (!is.null(to_remove)) {
        nodes_re <- page %>%
            html_nodes(to_remove)

        xml2::xml_remove(nodes_re)
    }

    content <- page %>%
        html_nodes(config[[site]][["content_selector"]])

    # Find tweets

    tweets <- content %>%
        html_nodes(".twitter-tweet")

    n_tweets <- length(tweets)

    if (n_tweets > 0) xml_remove(tweets)

    # Process iframes

    iframes <- content %>%
        html_nodes("iframe")

    if (length(iframes) > 0) {
        uris <- iframes %>%
            html_attr("src")

        n_youtube <- sum(grepl("https://www.youtube.com/embed/", uris, fixed  = TRUE))
        n_other_external <- length(iframes) - n_youtube

        xml2::xml_remove(iframes)
    } else {
        n_youtube <- 0
        n_other_external <- 0
    }

    images <- content %>%
        html_nodes("img") %>%
        html_attr("src")

    n_images <- length(images)

    image_filenames <- str_extract(images, "[^/]+(gif|jpe?g|tiff?|png|webp|bmp)$")

    links <- content %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        na.omit

    n_links_internal <- 0
    n_links_external <- 0

    if (length(links) > 0) {
        links_to_images <- links[grepl("(gif|jpe?g|tiff?|png|webp|bmp)$", links)]
        if (length(links_to_images) > 0) {
            filenames <- str_extract(links_to_images, "[^/]+(gif|jpe?g|tiff?|png|webp|bmp)$")
            to_delete <- links_to_images[filenames %in% image_filenames]
            links <- links[!links %in% to_delete]
        }
        links <- links[links != url]
        if (length(links) > 0) {
            n_links_internal <- sum(urltools::domain(links) == urltools::domain(url))
            n_links_external <- length(links) - n_links_internal
        }
    }

    n_strong <- content %>%
        html_nodes("strong") %>%
        length

    n_italic <- content %>%
        html_nodes("i, em") %>%
        length

    tibble(url, n_tweets, n_youtube, n_other_external, n_links_internal, n_links_external, n_italic, n_images, n_strong)
}

cl <- makePSOCKcluster(detectCores(), outfile = file.path(base_dir, "logs", "html_features.log"))

clusterExport(cl = cl, "config")
clusterExport(cl = cl, "data_dir")

clusterEvalQ(cl = cl, library(rvest))
clusterEvalQ(cl = cl, library(httr))
clusterEvalQ(cl = cl, library(stringr))
clusterEvalQ(cl = cl, library(digest))
clusterEvalQ(cl = cl, library(tibble))

html_features <- parLapplyLB(cl, rows, extract_html_features, data_dir = data_dir, config = config)

stopCluster(cl)

html_features <- bind_rows(html_features)

html_features %<>% replace(is.na(.), 0)

cols_original <- colnames(features)

features %<>% cbind(., html_features[, -1]) %>%
    mutate(across(!any_of(cols_original), ~ .x / text_length * 100))

rm(cl, config, html_features, rows, base_dir, cols_original, data_dir, extract_html_features)
gc()
