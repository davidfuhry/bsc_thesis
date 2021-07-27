reticulate::use_condaenv(condaenv = "spcy", required = TRUE)

library(cleanNLP)
library(doParallel)

base_dir <- rprojroot::find_root("README.md")

if (!exists("corpus")) corpus <- readRDS(file.path(base_dir, "data", "corpus.RDS"))

cleanNLP::cnlp_init_spacy(model_name = "de_core_news_md")

annotations <- cleanNLP::cnlp_annotate(corpus, text_name = "text", doc_name = "id")

saveRDS(annotations, file.path(base_dir, "data", "annotated_corpus.RDS"))

### Unused code

# nr <- nrow(corpus)
# n <- nr / 8
# corp_split <- split(corpus, rep(1:ceiling(nr/n), each=n, length.out=nr))
#
# cl <- makePSOCKcluster(8, outfile = "logs/annot.log")
#
# clusterEvalQ(cl = cl, library(cleanNLP))
#
# clusterCall(cl = cl, cnlp_init_udpipe, model_name = "german")
#
# copr_annot <- parLapply(cl = cl, corp_split, cnlp_annotate, doc_name = "id")
#
# token <- rbind(copr_annot[[1]]$token,
#                copr_annot[[2]]$token,
#                copr_annot[[3]]$token,
#                copr_annot[[4]]$token,
#                copr_annot[[5]]$token,
#                copr_annot[[6]]$token,
#                copr_annot[[7]]$token,
#                copr_annot[[8]]$token)
#
# document <- rbind(copr_annot[[1]]$document,
#                   copr_annot[[2]]$document,
#                   copr_annot[[3]]$document,
#                   copr_annot[[4]]$document,
#                   copr_annot[[5]]$document,
#                   copr_annot[[6]]$document,
#                   copr_annot[[7]]$document,
#                   copr_annot[[8]]$document)
#
# annotation <- list(token = token, document = document)
#
# attr(annotation, "class") <- c("cnlp_annotation", "list")
#
# rm(copr_annot)
#
# saveRDS(annotation, "data/annot_udpipe.RDS")
