# Word clouds of the chapters (Word files). Reads the visible text only (no Zotero field codes),
# cuts the reference list, removes stop words, and writes one cloud per chapter plus a 2 x 2 compound figure (PNG + SVG).
# install.packages(c("xml2", "tidytext", "dplyr", "stringr", "wordcloud", "RColorBrewer"))
library(xml2)
library(tidytext)
library(dplyr)
library(stringr)
library(wordcloud)
library(RColorBrewer)

set.seed(1)

# EDIT: folder with the chapter .docx files, and where the figures go
in_dir  <- "path/to/your/documents"
out_dir <- file.path(dirname(in_dir), "figures", "wordcloud")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# --- 1. Find files ---------------------------------------------------------
files <- list.files(in_dir, pattern = "chapter.*\\.docx$", ignore.case = TRUE, full.names = TRUE)
files <- files[!str_starts(basename(files), "~\\$")]            # Word temp files
files <- files[!str_detect(basename(files), "Supplementary")]    # drop supplementary material
print(basename(files))
stopifnot(length(files) > 0)

extra_stop <- c("https", "http", "doi", "www", "fig", "table", "et", "al")

# --- 2. Word counts for every document ---------------------------------------
counts_list <- list()

for (f in files) {

  # visible text only (<w:t>), not field codes (<w:instrText>)
  tmp <- tempfile()
  unzip(f, files = "word/document.xml", exdir = tmp)
  doc <- read_xml(file.path(tmp, "word/document.xml"))
  ns  <- xml_ns(doc)

  paras <- xml_find_all(doc, "//w:p", ns) |>
    vapply(function(p) paste(xml_text(xml_find_all(p, ".//w:t", ns)), collapse = ""),
           character(1))

  # cut everything from the last "References"/"Bibliography" heading onwards
  ref_idx <- which(str_detect(paras, "^\\s*(References|Bibliography|Literature cited)\\s*$"))
  if (length(ref_idx) > 0) paras <- paras[seq_len(max(ref_idx) - 1)]

  # "Chapter3_Intro.docx" -> "Chapter 3"
  label <- str_extract(basename(f), "(?i)chapter\\s*\\d+") |>
    str_replace("(?i)chapter\\s*", "Chapter ")

  counts_list[[label]] <-
    tibble(text = paste(paras, collapse = " ")) |>
    unnest_tokens(word, text) |>
    filter(str_detect(word, "^[[:alpha:]]+$"),   # letters only (keeps å ä ö)
           nchar(word) > 2,
           !word %in% stop_words$word,
           !word %in% extra_stop) |>
    count(word, sort = TRUE)
}

# order panels by chapter number so A, B, C = Chapter 1, 2, 3
counts_list <- counts_list[order(as.numeric(str_extract(names(counts_list), "\\d+")))]

# --- 3. Individual clouds: PNG + SVG -----------------------------------------
for (nm in names(counts_list)) {
  counts <- counts_list[[nm]]
  cat("\n", nm, "\n"); print(head(counts, 15))   # sanity check

  draw <- function() {
    wordcloud(counts$word, counts$n, max.words = 200,
              random.order = FALSE, colors = brewer.pal(8, "Dark2"))
    title(nm)
  }
  draw()   # Plots pane

  fname <- str_remove_all(nm, " ")   # "Chapter 1" -> "Chapter1"

  png(file.path(out_dir, paste0(fname, "_wordcloud.png")), width = 1200, height = 1200, res = 300)
  draw(); dev.off()

  svg(file.path(out_dir, paste0(fname, "_wordcloud.svg")), width = 6, height = 6)
  draw(); dev.off()
}

# --- 4. Compound figure ------------------------------------------
draw_panels <- function() {
  par(mfrow = c(2, 2), mar = c(0, 0, 1.8, 0), oma = c(0, 0, 0, 0))
  for (i in seq_along(counts_list)) {
    counts <- counts_list[[i]]
    wordcloud(counts$word, counts$n, max.words = 100, scale = c(4, 0.5),
              random.order = FALSE, colors = brewer.pal(8, "Dark2"))
    mtext(names(counts_list)[i], side = 3, adj = 0.5, line = 0.2, font = 2, cex = 1.3)
  }
}

draw_panels()   # Plots pane (enlarge it if words get dropped)

png(file.path(out_dir, "wordclouds_compound.png"), width = 3000, height = 3000, res = 300)
draw_panels(); dev.off()

svg(file.path(out_dir, "wordclouds_compound.svg"), width = 10, height = 10)
draw_panels(); dev.off()

list.files(out_dir)   # check what was written
