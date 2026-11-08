library(officer); library(pdftools); library(stringr); library(dplyr)
library(ggplot2); library(jsonlite)                     # jsonlite: Zotero citation fields + Crossref answers

# ============ 0. INPUT ==================================================
# EDIT THIS SECTION. One entry per document: a Word file (.docx, Zotero citations are read exactly)
# or a PDF (two-column journal layout is handled). Names become the labels in tables and figures.
chapters_dir <- "path/to/your/documents"
files <- c(
  Chapter1 = file.path(chapters_dir, "Chapter1.pdf"),
  Chapter2 = file.path(chapters_dir, "Chapter2.docx"),
  Chapter3 = file.path(chapters_dir, "Chapter3.docx"),
  Chapter4 = file.path(chapters_dir, "Chapter4.docx")
)
stopifnot(!anyNA(files), all(file.exists(files)))        # stops here if a path is wrong
# Step C looks up the reference lists of the papers you cite, in Crossref (free, no account, no key).
# Optional: an email address makes Crossref send you to its faster "polite pool" (leave "" to stay anonymous)
crossref_mailto <- ""

# PhD period (shaded in the plots), as decimal years: 2022 + 7/12 ... 2027 + 1/12
phd_start <- 2022 + 7/12
phd_end   <- 2027 + 1/12

# date each chapter was written / published, as decimal years; reference ages are measured from here.
# Default = today for every document in `files`; override below where you know the real date
today_dec <- as.numeric(format(Sys.Date(), "%Y")) + (as.numeric(format(Sys.Date(), "%j")) - 0.5) / 365.25
chapter_date <- setNames(rep(today_dec, length(files)), names(files))
# chapter_date["Chapter1"] <- 2026 + 3/12      # <- override where you know the date

# manual override of the line where the reference list starts, per document (only if auto-detection fails)
# ref_start_manual <- c(Chapter3 = 123)
ref_start_manual <- c()

# things shared by all documents -------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a
norm      <- function(x) tolower(stringi::stri_trans_general(x, "Latin-ASCII"))
particles <- c("van", "von", "de", "der", "den", "le", "la", "di", "da", "du", "del", "ten", "ter", "af")
particle  <- paste0("(?:(?:", paste(particles, collapse = "|"), ")\\s+)*")
name      <- "\\p{Lu}[\\p{L}'’-]+"
lead      <- paste0("^(?:(?:e\\.g\\.|i\\.e\\.|see|cf\\.|also),?\\s*|",
                    "(?i:fig(?:ure)?s?|tables?)\\.?\\s*S?\\d+[a-z]?(?:[–-]\\d+)?,?\\s*)*")   # also skips "Fig. 2," / "Table S1,"

# heading test runs on the line with ALL whitespace removed, so letter-spaced PDF headings ("R e f e r e n c e s") also match
heading_rx <- regex(
  "^[0-9.]*(references?(list)?|bibliography|literaturecited|workscited|referenser|litteratur):?$",
  ignore_case = TRUE)

# start of a reference-list entry: "Surname, I."  (used for PDFs)
auth_rx <- paste0("^", particle, "\\p{Lu}[\\p{L}'’-]+(?: [\\p{L}'’-]+){0,2}, \\p{Lu}\\.")

# sections that are not citable text: acknowledgements, funding, etc. (short heading lines only)
back_rx <- regex(paste0("^\\s*[0-9.]*\\s*(acknowledge?ments?|funding|author contributions?|",
                        "conflicts? of interest|competing interests?|data (and code )?availability( statement)?|",
                        "ethics( statement)?|tack|finansiering)\\s*:?\\s*$"), ignore_case = TRUE)

# PDFs (journal style): back-matter headings are run-in, e.g. "Acknowledgements – We thank…"
run_in_rx <- regex(paste0("^(acknowledge?ments?|funding|author contributions?|conflicts? of interest|",
                          "data (and code )?availability( statement)?|ethics( statement)?)\\b"), ignore_case = TRUE)

# several citations in one bracket are separated by ";" or, after a year, by "," + a capital letter
#   (Smith 2010; Lee 2012)   (Smith 2010, Lee et al. 2012)   but not (Väre et al., 1995, 1996)
split_rx <- paste0(";\\s*|(?<=(?:18|19|20)\\d{2}[a-z]?),\\s+(?=", particle, "\\p{Lu})")

# non-citations that still look like "Word (year)"; add more as they turn up
stoplist <- c(month.name, "Horizon", "Since", "Only", "Before", "After",
              "During", "Until", "By", "In", "Oikos", "Epson")

# ============ A. PER DOCUMENT: read, split, parse, cross-check ===========
res <- list()

for (doc in names(files)) {
  cat("\n=====================", doc, "=====================\n")

  # --- 1. read + split body / reference list ------------------------------
  is_pdf   <- tolower(tools::file_ext(files[[doc]])) == "pdf"
  self_doi <- NA_character_

  if (is_pdf) {
    pages    <- pdf_text(files[[doc]])
    self_doi <- str_extract(pages[1], "10\\.\\d{4,9}/[^\\s]+") |> str_remove("[.,;)]+$")  # the article's own DOI
    # read by word position, column by column (journal PDFs have two columns; plain text extraction
    # joins left and right column into one line). Per page: find the gutter, treat lines that cross it
    # as full-width, otherwise read the left column, then the right column.
    pw <- pdf_pagesize(files[[doc]])
    pd <- pdf_data(files[[doc]])
    paras <- character(0)
    for (p in seq_along(pd)) {
      w <- pd[[p]]
      if (nrow(w) == 0) next
      w <- w |>
        filter(x > 30, x + width < pw$width[p] - 30) |>        # margin text (e.g. vertical download stamps)
        arrange(y, x) |>
        mutate(line = cumsum(c(TRUE, diff(y) > 3)), cx = x + width / 2)
      if (nrow(w) == 0) next
      xs  <- seq(0.35, 0.65, by = 0.005) * pw$width[p]         # candidate gutter positions
      cov <- sapply(xs, function(g) sum(w$x < g & w$x + w$width > g))
      mid <- median(xs[cov == min(cov)])                       # least-covered position = gutter
      w   <- w |> mutate(crosses = x < mid + 3 & x + width > mid - 3)
      ln  <- w |> group_by(line) |> summarise(full = any(crosses), y = min(y), .groups = "drop") |> arrange(y)
      if (mean(ln$full) > 0.7) mid <- -1000                    # most lines cross it: single-column page
      if (mid < 0) ln$full <- FALSE
      ln$block <- cumsum(ln$full | lag(ln$full, default = FALSE))   # a full-width line ends/starts a block
      txt <- w |>
        left_join(ln |> select(line, full, block), by = "line") |>
        mutate(col = ifelse(full | cx < mid, 1, 2)) |>
        group_by(block, col, line) |>
        summarise(y = min(y), text = paste(text[order(x)], collapse = " "), .groups = "drop") |>
        arrange(block, col, y)
      paras <- c(paras, txt$text)
    }
    paras <- str_squish(paras)
    # publisher boilerplate (e.g. Wiley "Downloaded from ..." download stamp)
    paras <- paras[!str_detect(paras, "Downloaded from https?://|Wiley Online Library|onlinelibrary\\.wiley\\.com|Creative Commons License|^\\d{7,8}, \\d{4}, \\d+,")]
    paras <- paras[nzchar(paras) & !str_detect(paras, regex("^(page )?\\d{1,4}( of \\d+)?$", ignore_case = TRUE))]
    if (length(pages) >= 2) {                                      # running headers/footers repeat on most pages
      line_key  <- str_squish(str_remove_all(paras, "\\d+"))       # ignore page numbers / volume numbers
      has_words <- str_count(line_key, "\\p{L}") >= 6               # "100: 1-10." is reference text, not a header
      rep_keys  <- names(which(table(line_key[has_words]) >= max(2, ceiling(0.6 * length(pages)))))
      paras <- paras[!(line_key %in% rep_keys & has_words & nchar(paras) < 100)]
    }
    # join words hyphenated at line ends ("mod-" / "erates"); one line per element
    paras <- paste(paras, collapse = "\n") |>
      str_replace_all("(\\p{L})-\n(\\p{Ll})", "\\1\\2") |> str_split("\n") |> unlist()
  } else {
    paras <- docx_summary(read_docx(files[[doc]])) |>
      filter(content_type %in% c("paragraph", "table cell"), nzchar(trimws(text))) |>   # table cells can hold citations too
      pull(text)
  }

  bib_idx  <- which(str_detect(paras, "ZOTERO_BIBL"))[1]   # 1) Zotero bibliography field
  head_idx <- which(str_detect(str_remove_all(paras, "\\s+"), heading_rx))   # 2) heading text

  if (doc %in% names(ref_start_manual)) {
    ref_start <- ref_start_manual[[doc]]
  } else if (!is.na(bib_idx)) {
    ref_start <- bib_idx - 1                                # first list entry holds the field code
  } else if (length(head_idx) > 0) {
    ref_start <- max(head_idx)
  } else if (is_pdf && any(str_detect(paras, auth_rx))) {
    # no heading found: the list starts where "Surname, I." entries become dense (>= 8 within 40 lines)
    entry_idx <- which(str_detect(paras, auth_rx))
    dense     <- entry_idx[sapply(entry_idx, function(i) sum(entry_idx >= i & entry_idx <= i + 40) >= 8)]
    if (length(dense) == 0) {
      message("No bibliography found in ", doc, ". Set ref_start_manual[\"", doc, "\"] and rerun. Skipping.")
      next
    }
    ref_start <- dense[1] - 1
    message(doc, ": no 'References' heading found; list assumed to start at line ", dense[1], ": ",
            substr(paras[dense[1]], 1, 60))
  } else {
    cand <- grep("refer|bibliog|literature|litteratur", paras, ignore.case = TRUE)
    message("No bibliography found in ", doc, ". Candidates:\n",
            paste(cand, substr(paras[cand], 1, 60), sep = ": ", collapse = "\n"),
            "\nSet ref_start_manual[\"", doc, "\"] and rerun. Skipping.")
    next
  }

  body <- paras[1:ref_start]
  refl <- paras[(ref_start + 1):length(paras)]

  # (a) before the reference list: drop ONLY back-matter sections, wherever they are.
  #     A section runs from its heading to the next short, heading-like line.
  drop <- integer(0)
  if (is_pdf) {
    # PDF: run-in headings in the last 40% of the body -> drop from the first one to the end of the body
    back_heads <- which(str_detect(body, run_in_rx) & seq_along(body) > 0.6 * length(body))
    if (length(back_heads)) drop <- back_heads[1]:length(body)
  } else {
    is_head    <- nchar(body) < 80 & !str_detect(body, "[.!?)]\\s*$")
    back_heads <- which(str_detect(body, back_rx) & nchar(body) < 60)
    for (h in back_heads) {
      nxt  <- which(is_head & seq_along(body) > h)[1]
      last <- if (is.na(nxt)) length(body) else nxt - 1
      drop <- c(drop, h:last)
    }
  }
  dropped_text <- body[unique(drop)]
  if (length(drop)) body <- body[-unique(drop)]

  # (b) after the reference list: cut at supplementary material or such a heading
  end_idx <- which(str_detect(refl, regex("^\\s*(supplementary|supporting|appendix)\\b", ignore_case = TRUE)) |
                     (str_detect(refl, back_rx) & nchar(refl) < 60))[1]
  if (!is.na(end_idx)) refl <- refl[seq_len(end_idx - 1)]

  if (is_pdf) {
    # PDF: entries wrap over several lines. A new entry starts when the previous line ends with "."
    # and this line begins like an entry: "Surname, I." or "Organisation 2022."
    org_rx  <- "^\\p{Lu}[^.,]{2,80}\\s\\(?(?:18|19|20)\\d{2}[a-z]?\\)?[.,]"      # "Global Peatlands Assessment 2022."
    nr      <- length(refl)
    new_entry <- rep(TRUE, nr)
    if (nr > 1) new_entry[-1] <- str_detect(refl[-nr], "[.?]$") & !str_detect(refl[-nr], "(^|\\s)\\p{Lu}\\.$") &
      (str_detect(refl[-1], auth_rx) | str_detect(refl[-1], org_rx))
    refl <- tapply(refl, cumsum(new_entry), paste, collapse = " ") |> as.character()
  }

  cat("heading line:   ", substr(paras[ref_start], 1, 60), "\n")
  cat("list entries:   ", length(refl), "| first:", substr(refl[1], 1, 60), "\n")
  cat("removed", length(drop), "back-matter paragraph(s):",
      if (length(back_heads)) paste(substr(paras[back_heads], 1, 30), collapse = " | ") else "(none)", "\n")

  # --- 2. clean Zotero field codes ----------------------------------------
  body_txt    <- paste(body, collapse = " ")
  intext_dois <- str_extract_all(body_txt, '(?<="DOI":")[^"]+')[[1]] |> unique()

  # exact route for Zotero documents: the citation fields list what is cited (first author, year, DOI),
  # whatever the visible text looks like (tables, odd formatting, ...). Parsed per paragraph.
  zot_json <- str_extract_all(body, 'ADDIN ZOTERO_ITEM CSL_CITATION \\{.*?csl-citation\\.json"\\}') |> unlist()
  zot_df   <- tibble(family = character(), year = integer(), doi = character())
  for (j in zot_json) {
    js <- tryCatch(fromJSON(str_remove(j, "^ADDIN ZOTERO_ITEM CSL_CITATION\\s*"), simplifyVector = FALSE),
                   error = function(e) NULL)
    for (it in js$citationItems) {
      a <- it$itemData$author
      d <- it$itemData$issued$`date-parts`
      zot_df <- bind_rows(zot_df, tibble(
        family = as.character((if (length(a)) a[[1]]$family %||% a[[1]]$literal else NULL) %||% NA),
        year   = suppressWarnings(as.integer((if (length(d)) d[[1]][[1]] else NULL) %||% NA)),
        doi    = as.character(it$itemData$DOI %||% NA)))
    }
  }
  zot_all <- zot_df                       # one row per citation of an item (used to count how often it is cited)
  zot_df  <- distinct(zot_df)
  cat("Zotero fields:", length(zot_json), "| distinct cited items in them:", nrow(zot_df), "\n")

  # strip per paragraph so a field without the usual ending can't eat text in other paragraphs
  body_clean <- body |>
    str_remove_all('ADDIN ZOTERO_ITEM CSL_CITATION \\{.*?csl-citation\\.json"\\}') |>
    paste(collapse = " ")
  refl_clean <- str_remove(refl, "^\\s*ADDIN ZOTERO_BIBL \\{.*?\\} CSL_BIBLIOGRAPHY\\s*")

  cat("ADDIN left in body (want 0):", str_count(body_clean, "ADDIN"), "\n")

  # --- 3. in-text citations -> (first author, year) pairs ------------------
  #   (Smith 2021; Lee et al., 2019)   (Väre et al., 1995, 1996)
  #   (Stark, Horstkotte, et al., 2023)   Kantola et al., (2025)   Smith (2021)
  # (a) parenthetical: split each (...) on ";", take first surname + every year in the chunk
  parens <- str_extract_all(body_clean, "\\([^()]*\\)")[[1]]
  cit_chunks <- parens |> str_split(split_rx) |> unlist() |>
    str_remove_all("^\\s*\\(|\\)\\s*$") |> str_trim()
  auth_a  <- str_match(cit_chunks, paste0(lead, particle, "(", name, ")"))[, 2]
  yrs_a   <- str_extract_all(cit_chunks, "(?:18|19|20)\\d{2}")
  pairs_a <- data.frame(author = rep(auth_a, lengths(yrs_a)),
                        year   = as.integer(unlist(yrs_a)))

  # (b) narrative: "Smith (2021)", "Kantola et al., (2025)", "Smith and Lee (2021)"
  narr_rx <- paste0(particle, "(", name, ")(?: et al\\.?|,? (?:and|&) ", particle, name,
                    "|(?:, ", name, ")+,? et al\\.?)?,? \\(((?:18|19|20)\\d{2}[^()]*)\\)")   # parenthesis must START with the year
  m_b     <- str_match_all(body_clean, narr_rx)[[1]]
  yrs_b   <- str_extract_all(m_b[, 3], "(?:18|19|20)\\d{2}")
  pairs_b <- data.frame(author = rep(m_b[, 2], lengths(yrs_b)),
                        year   = as.integer(unlist(yrs_b)))

  intext_pairs <- bind_rows(pairs_a, pairs_b) |>
    filter(!is.na(author), !author %in% stoplist) |>
    distinct()

  # list years: try Chicago "Name. 2010.", then "(2010)", then any year
  list_years <- as.integer(coalesce(
    str_extract(refl_clean, "(?<=\\. )(18|19|20)\\d{2}(?=[a-z]?\\. )"),
    str_extract(refl_clean, "(?<=\\()(18|19|20)\\d{2}(?=[a-z]?\\))"),
    str_extract(refl_clean, "\\b(18|19|20)\\d{2}\\b")))

  cat("body paragraphs:", length(body), "| in-text author-year pairs:", nrow(intext_pairs),
      "| list entries without year:", sum(is.na(list_years)), "\n")
  if (nrow(intext_pairs) == 0)
    warning(doc, ": no in-text citations parsed. Inspect the body text and the citation style.")

  # --- 4. cross-check text <-> list ----------------------------------------
  refl_n <- norm(refl_clean)

  # text -> list: is the surname + year found in any list entry?
  intext_pairs$found <- sapply(seq_len(nrow(intext_pairs)), function(i)
    any(str_detect(refl_n, fixed(norm(intext_pairs$author[i]))) &
          str_detect(refl_n, paste0("\\b", intext_pairs$year[i], "\\b"))))

  # list -> text: search the text for each entry's first-author key followed by its year
  # inside one citation (no ";" or ")" in between). Key = surname for people,
  # the whole name phrase for organisations / reports.
  lead_txt <- str_extract(refl_clean, "^[^,.(:]+") |> str_trim() |>
    str_remove("\\s*\\(?(?:18|19|20)\\d{2}[a-z]?\\)?$")            # "Global Peatlands Assessment 2022" -> without the year
  s1 <- sapply(lead_txt, function(x) {
    w <- str_split(x, "\\s+")[[1]]
    if (length(w) > 1 && all(w[-length(w)] %in% particles)) w[length(w)] else x   # "van der Wal" -> "Wal"
  }, USE.NAMES = FALSE)
  body_n <- norm(body_clean)

  cited_in_text <- sapply(seq_along(refl_clean), function(i)
    !is.na(s1[i]) && !is.na(list_years[i]) &&
      str_detect(body_n, paste0("\\b", str_replace_all(str_escape(norm(s1[i])), " ", "\\\\s+"),
                                "\\b[^;)]{0,100}?\\b", list_years[i], "[a-z]?\\b")))

  # Zotero route: entry DOI, or first-author surname + year, found among the items in the citation fields
  entry_doi <- str_extract(refl_clean, "10\\.\\d{4,9}/\\S+") |> str_remove("[.,;)]+$")
  fam_last  <- str_extract(norm(zot_df$family), "[^ ]+$")
  s1_last   <- str_extract(norm(s1), "[^ ]+$")
  cited_zotero <- sapply(seq_along(refl_clean), function(i)
    (!is.na(entry_doi[i]) && tolower(entry_doi[i]) %in% tolower(zot_df$doi)) ||
      (!is.na(list_years[i]) && any(fam_last == s1_last[i] & zot_df$year == list_years[i], na.rm = TRUE)))
  cited_in_text <- cited_in_text | cited_zotero

  # how often each list entry is cited in the text of this chapter:
  #   Zotero: every citation item in the fields counts once;  visible text: matches of "Author ... year" inside one citation.
  #   Zotero entries with a DOI are matched by DOI only. Text matches (PDF, or entries without Zotero link) cannot tell apart
  #   entries with the same first author + year: those share the count.
  fam_all <- str_extract(norm(zot_all$family), "[^ ]+$")
  n_zot <- sapply(seq_along(refl_clean), function(i) {
    by_doi <- if (!is.na(entry_doi[i])) !is.na(zot_all$doi) & tolower(zot_all$doi) == tolower(entry_doi[i]) else rep(FALSE, nrow(zot_all))
    by_fy  <- if (!is.na(list_years[i])) fam_all == s1_last[i] & zot_all$year == list_years[i] & (is.na(entry_doi[i]) | is.na(zot_all$doi)) else rep(FALSE, nrow(zot_all))   # author+year only where a DOI is missing
    sum(by_doi | by_fy, na.rm = TRUE)
  })
  n_rx <- sapply(seq_along(refl_clean), function(i)
    if (is.na(s1[i]) || is.na(list_years[i])) 0L else
      str_count(body_n, paste0("\\b", str_replace_all(str_escape(norm(s1[i])), " ", "\\\\s+"),
                               "\\b[^;)]{0,100}?\\b", list_years[i], "[a-z]?\\b")))
  n_text <- if (nrow(zot_all) > 0) ifelse(n_zot > 0, n_zot, n_rx) else n_rx     # Zotero count where available (exact), else text matches

  # entries sharing first author + year can't be told apart without a/b suffixes
  key       <- paste(norm(s1), list_years)
  ambiguous <- duplicated(key) | duplicated(key, fromLast = TRUE)

  cited_not_listed <- intext_pairs[!intext_pairs$found, ]
  listed_not_cited <- data.frame(entry = substr(refl_clean, 1, 70))[!cited_in_text, , drop = FALSE]
  ambiguous_list   <- data.frame(entry = substr(refl_clean, 1, 70))[ambiguous, , drop = FALSE]

  cat("\n-- cited in text, not found in list:\n");  print(cited_not_listed)
  cat("\n-- in list, not found in text:\n");        print(listed_not_cited)
  cat("\n-- same first author + year (check by hand):\n"); print(ambiguous_list)

  # --- store ---------------------------------------------------------------
  list_dois <- str_extract(refl_clean, "10\\.\\d{4,9}/\\S+") |> str_remove("[.,;)]+$")
  dois      <- unique(na.omit(c(intext_dois, list_dois)))
  cat("\nDOIs:", length(dois), "for", length(refl_clean), "list entries\n")

  res[[doc]] <- list(intext_pairs = intext_pairs, list_years = list_years, refl_clean = refl_clean,
                     dois = dois, self_doi = self_doi, zot_df = zot_df, cited_not_listed = cited_not_listed,
                     listed_not_cited = listed_not_cited, ambiguous_list = ambiguous_list,
                     dropped_text = dropped_text, n_text = n_text)
}

# figures are saved as SVG (editable in Inkscape / Illustrator / PowerPoint) in the subfolder "figures" next to "chapters"
fig_dir <- file.path(dirname(chapters_dir), "figures")      # figures are saved next to the documents folder
dir.create(fig_dir, showWarnings = FALSE)
svg_dev <- if (requireNamespace("svglite", quietly = TRUE)) svglite::svglite else grDevices::svg

# ============ B. YEAR DISTRIBUTIONS, ALL DOCUMENTS ======================
years_df <- bind_rows(lapply(names(res), function(d) bind_rows(
  tibble(doc = d, source = "in-text (unique author-year)", year = res[[d]]$intext_pairs$year),
  tibble(doc = d, source = "reference list",               year = res[[d]]$list_years)))) |>
  mutate(doc = factor(doc, levels = names(files)))

# a bar for year y spans y .. y+1 (boundary = 0), so the PhD shading (decimal years) lines up,
# and the median of integer years is drawn in the middle of its bar (+ 0.5)
med_df <- years_df |>
  group_by(doc, source) |>
  summarise(med = median(year, na.rm = TRUE) + 0.5, .groups = "drop")

p_years_text_vs_list <- ggplot(years_df, aes(year)) +
  annotate("rect", xmin = phd_start, xmax = phd_end, ymin = -Inf, ymax = Inf,
           fill = "steelblue", alpha = 0.45) +
  geom_histogram(binwidth = 1, boundary = 0) +
  geom_vline(data = med_df, aes(xintercept = med), colour = "red", linetype = "dashed") +
  geom_text(data = med_df, aes(x = med, y = Inf, label = round(med - 0.5)),
            colour = "red", size = 3, hjust = 1.2, vjust = 1.5) +
  facet_grid(source ~ doc, scales = "free_y") +
  labs(x = NULL, y = "nr. references") +
  theme_minimal(base_size = 14)
print(p_years_text_vs_list)
ggsave(file.path(fig_dir, "years_text_vs_list.svg"), p_years_text_vs_list, device = svg_dev, width = 12, height = 6)

# ---- statistics on the reference lists + median age of the references -----------------------------------
# median age of the references at writing = age by which 50 % of the references were published
# Second estimate: exponential decay fitted to the age distribution (Poisson regression),
# age to reach 50 % = ln(2) / decay rate. Treat it as rough: the literature grows over time, which
# makes references look younger than pure obsolescence would.
set.seed(1)
stats <- list()

for (d in names(res)) {
  y   <- res[[d]]$list_years[!is.na(res[[d]]$list_years)]
  age <- chapter_date[[d]] - (y + 0.5)                        # age of each reference, mid-year
  if (any(age < 0)) {
    message(d, ": ", sum(age < 0), " reference(s) newer than chapter_date, dropped from age statistics")
    age <- age[age >= 0]
  }

  boot <- replicate(2000, median(sample(age, replace = TRUE)))  # bootstrap over references

  a      <- floor(age); a <- a[a <= 50]                       # age classes in whole years
  fit_df <- data.frame(age = 0:max(a), n = tabulate(a + 1, nbins = max(a) + 1))
  lambda <- -coef(glm(n ~ age, family = poisson, data = fit_df))[["age"]]

  stats[[d]] <- tibble(
    doc          = d,
    n_refs       = length(y),
    median_year  = median(y),
    q25_year     = unname(quantile(y, .25)),
    q75_year     = unname(quantile(y, .75)),
    oldest       = min(y),
    newest       = max(y),
    pct_last5y   = round(100 * mean(age <= 5)),
    pct_last10y  = round(100 * mean(age <= 10)),
    pct_in_phd   = round(100 * mean(y + 0.5 >= phd_start & y + 0.5 <= phd_end)),
    age_median = median(age),                            # <- median age (years)
    age_median_lo     = unname(quantile(boot, .025)),           # 95 % bootstrap interval
    age_median_hi     = unname(quantile(boot, .975)),
    age_50pct_expfit = if (is.finite(lambda) && lambda > 0) log(2) / lambda else NA_real_
  )
}
stats <- bind_rows(stats)
print(as.data.frame(stats), digits = 3)

# median age visually: where each curve crosses 0.5
age_df <- bind_rows(lapply(names(res), function(d) {
  y <- res[[d]]$list_years[!is.na(res[[d]]$list_years)]
  tibble(doc = d, age = chapter_date[[d]] - (y + 0.5))
})) |> filter(age >= 0)

p_age_ecdf <- ggplot(age_df, aes(age, colour = doc)) +
  annotate("rect", xmin = min(stats$age_median), xmax = max(stats$age_median), ymin = -Inf, ymax = Inf,
           fill = "steelblue", alpha = 0.45) +
  stat_ecdf() +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  coord_cartesian(xlim = c(0, 50)) +
  labs(x = "Age of reference at writing (years)", y = "Share of references that are younger",
       colour = NULL)+
  theme_minimal(base_size = 14)
print(p_age_ecdf)
ggsave(file.path(fig_dir, "age_ecdf.svg"), p_age_ecdf, device = svg_dev, width = 8, height = 5)

# ============ C. REFERENCES OF THE REFERENCES (Crossref) ================
# Crossref holds the reference list of every paper whose publisher deposited it. Pass 1: one request per cited
# DOI gives that paper's reference list (year, DOI, a dedupe key per entry). Pass 2: entries without a year but
# with a DOI are looked up in batches of 50 (their "issued" year). Everything is cached on disk, so an interrupted
# run (or a new chapter) only fetches what is missing.
cr_cache_file <- "crossref_cache.rds"
doi_year_file <- "crossref_doi_year.rds"
empty_refs <- tibble(doi = character(), year = integer(), year_txt = integer(), key = character())
cr_cache <- if (file.exists(cr_cache_file)) readRDS(cr_cache_file) else list()    # doi -> list(found, has_refs, refs)
cr_cache <- cr_cache[vapply(cr_cache, function(g) "refs" %in% names(g) && "year_txt" %in% names(g$refs), logical(1))]   # drop entries from older versions
doi_year <- if (file.exists(doi_year_file)) readRDS(doi_year_file) else integer(0)     # named: doi -> year (NA = unknown)
this_year <- as.integer(format(Sys.Date(), "%Y"))
# Year of a second-level reference = the publication year of its DOI in Crossref (reliable). Only entries WITHOUT a (usable) DOI fall back
# to the "year" field of the reference entry. A year picked out of free text is off by default: page numbers and years in titles fool it.
use_text_years <- FALSE

# one request; returns the "message" part of the answer, NULL for an unknown DOI (HTTP 404, e.g. Dryad/Zenodo
# data-set DOIs), and stops after repeated other failures (rate limit, network)
cr_json <- function(path) {
  url <- paste0("https://api.crossref.org/", path,
                if (nzchar(crossref_mailto)) paste0(if (grepl("\\?", path)) "&" else "?", "mailto=", crossref_mailto) else "")
  for (attempt in 1:3) {
    msgs <- character(0)
    out <- withCallingHandlers(
      tryCatch(fromJSON(url, simplifyVector = FALSE)$message, error = function(e) e),
      warning = function(w) { msgs <<- c(msgs, conditionMessage(w)); invokeRestart("muffleWarning") })
    if (!inherits(out, "error")) return(out)
    if (any(grepl("404", c(msgs, conditionMessage(out))))) return(NULL)      # R reports the 404 in a warning
    Sys.sleep(2 * attempt)
  }
  stop(conditionMessage(out), " ", paste(unique(msgs), collapse = " "), call. = FALSE)
}
enc <- function(doi) str_replace_all(URLencode(doi, reserved = TRUE), c("%2F" = "/"))
cr_get <- function(doi) cr_json(paste0("works/", enc(doi)))

issued_year <- function(w) {
  y <- suppressWarnings(as.integer(w$issued$`date-parts`[[1]][[1]] %||% NA))
  if (!is.na(y) && y >= 1800 && y <= this_year) y else NA_integer_
}

# year of every entry in a record's reference list, taken ONLY from the structured "year" field.
# (A year picked out of free text is unreliable: page ranges like "1795-1809" or years inside titles look like years.)
ref_years <- function(rec) {
  vapply(rec$reference, function(x) {
    y <- suppressWarnings(as.integer(str_extract(as.character(x$year %||% ""), "\\d{4}")))
    if (!is.na(y) && y >= 1800 && y <= this_year) y else NA_integer_
  }, integer(1))
}
# last resort, used only for entries WITHOUT a DOI: a year in free text, only as "(2004)" or as ". 2004." / ", 2004,"
ref_years_txt <- function(rec) {
  vapply(rec$reference, function(x) {
    t <- x$unstructured %||% ""
    y <- coalesce(str_extract(t, "(?<=\\()(?:18|19|20)\\d{2}(?=[a-z]?\\))"),
                  str_extract(t, "(?<=[.,]\\s)(?:18|19|20)\\d{2}(?=[a-z]?\\s*(?:[.,;]|$))"))
    y <- suppressWarnings(as.integer(y))
    if (!is.na(y) && y >= 1800 && y <= this_year) y else NA_integer_
  }, integer(1))
}

# identity of an entry, to find the same paper cited by several papers / chapters:
# its DOI; else first author + year + volume + first page; else the first 60 letters of the free text; else NA (never merged)
ref_key <- function(x) {
  if (!is.null(x$DOI)) return(tolower(x$DOI))
  if (!is.null(x$author) && !is.null(x$year) && !is.null(x$`first-page`))
    return(paste0("a:", norm(paste(x$author, x$year, x$volume %||% "", x$`first-page`))))
  txt <- norm(str_remove_all(x$unstructured %||% x$`article-title` %||% "", "[^A-Za-z0-9]"))
  if (nchar(txt) >= 20) paste0("u:", substr(txt, 1, 60)) else NA_character_
}

# --- 1. which DOIs to look up, per document ---
dois_by_doc <- list()
for (doc in names(res)) {
  r <- res[[doc]]
  if (length(r$dois) >= 0.5 * length(r$refl_clean)) {
    dois_by_doc[[doc]] <- r$dois
  } else if (!is.na(r$self_doi)) {
    # few DOIs in the list (typical for a PDF): take the DOIs from Crossref's reference list of the article itself
    cat(doc, ": only", length(r$dois), "DOIs for", length(r$refl_clean), "entries; using Crossref's reference list for", r$self_doi, "\n")
    rec <- tryCatch(cr_get(r$self_doi), error = function(e) NULL)
    art <- unique(unlist(lapply(rec$reference, function(x) x$DOI)))
    if (length(art)) dois_by_doc[[doc]] <- art else message(doc, ": Crossref has no reference DOIs for ", r$self_doi, ", skipping.")
  } else {
    message(doc, ": no DOIs and no article DOI found, skipping the Crossref step.")
  }
}
if (length(dois_by_doc) == 0) stop("No document has DOIs to look up (see the messages above).")

# --- 2. pass 1: the reference list of each cited DOI ---
all_dois <- unique(tolower(trimws(unlist(dois_by_doc))))
need     <- setdiff(all_dois, names(cr_cache))
cat("\nDOIs:", length(all_dois), "| already cached:", length(all_dois) - length(need), "| to fetch:", length(need), "\n")

for (i in seq_along(need)) {
  rec <- tryCatch(cr_get(need[i]), error = function(e) e)
  if (inherits(rec, "error")) {
    message("\nStopped at DOI ", i, " of ", length(need), ": ", conditionMessage(rec),
            "\nEverything fetched so far is saved; rerun this section later and it continues from the cache.")
    break
  }
  cr_cache[[need[i]]] <-
    if (is.null(rec)) list(found = FALSE, has_refs = FALSE, refs = empty_refs) else
      list(found = TRUE, has_refs = !is.null(rec$reference),
           refs = if (is.null(rec$reference)) empty_refs else tibble(
             doi  = tolower(vapply(rec$reference, function(x) x$DOI %||% NA_character_, character(1))),
             year = ref_years(rec),
             year_txt = ref_years_txt(rec),
             key  = vapply(rec$reference, ref_key, character(1))))
  if (i %% 25 == 0) { saveRDS(cr_cache, cr_cache_file); cat(i, "/", length(need), "done\n") }
  Sys.sleep(0.2)                                                   # stay well below Crossref's rate limit
}
saveRDS(cr_cache, cr_cache_file)

# --- 3. pass 2: publication year of every DOI in the reference lists (batches of 50) ---
refs_all <- bind_rows(empty_refs, bind_rows(lapply(cr_cache[intersect(all_dois, names(cr_cache))], function(g) g$refs)))
miss_all <- unique(refs_all$doi[!is.na(refs_all$doi)])                    # every DOI: its own publication year is the reliable one
miss_all <- miss_all[!grepl("[,;<>]", miss_all)]                   # characters that break the batch filter
need2    <- setdiff(miss_all, names(doi_year))
cat("\nsecond-level DOIs to get a publication year for:", length(miss_all), "| already looked up:",
    length(miss_all) - length(need2), "| to look up:", length(need2), "\n")

batches <- split(need2, ceiling(seq_along(need2) / 50))
for (b in seq_along(batches)) {
  d   <- batches[[b]]
  got <- tryCatch(cr_json(paste0("works?filter=", paste0("doi:", enc(d), collapse = ","), "&select=DOI,issued&rows=50")),
                  error = function(e) e)
  if (inherits(got, "error")) {
    message("\nStopped at batch ", b, " of ", length(batches), ": ", conditionMessage(got),
            "\nEverything fetched so far is saved; rerun this section later and it continues from the cache.")
    break
  }
  items <- got$items
  yrs   <- setNames(vapply(items, issued_year, integer(1)), tolower(vapply(items, function(w) w$DOI, "")))
  if (length(items) == 0 && length(d) > 1) {                       # batch filter not understood: ask one by one
    yrs <- integer(0)
    for (one in d) {
      w <- tryCatch(cr_get(one), error = function(e) NULL)
      if (!is.null(w)) yrs[one] <- issued_year(w)
      Sys.sleep(0.2)
    }
  }
  doi_year[d] <- NA_integer_                                       # asked: unknown unless answered below
  doi_year[names(yrs)] <- yrs
  saveRDS(doi_year, doi_year_file)
  if (b %% 10 == 0) cat(b, "/", length(batches), "batches done\n")
  Sys.sleep(0.2)
}

# --- 4. references of cited papers per chapter: fill in years, drop unknown years, remove duplicates ---
their_doc <- list()
for (doc in names(dois_by_doc)) {
  ds <- unique(tolower(trimws(dois_by_doc[[doc]])))
  got <- cr_cache[intersect(ds, names(cr_cache))]
  r2 <- bind_rows(empty_refs, bind_rows(lapply(got, function(g) g$refs)))      # every entry of the cited papers' lists
  year_doi <- unname(doi_year[r2$doi])                                         # publication year of the DOI (NA: no DOI / not found)
  year_txt <- if (use_text_years) ifelse(is.na(r2$doi), r2$year_txt, NA_integer_) else rep(NA_integer_, nrow(r2))
  # how often does the "year" field of the entry agree with the DOI year? (check of the field's reliability)
  both <- !is.na(r2$year) & !is.na(year_doi)
  cat(doc, ": entries with field year and DOI year:", sum(both), "| equal:", round(100 * mean(r2$year[both] == year_doi[both])),
      "% | differ by more than 1 year:", round(100 * mean(abs(r2$year[both] - year_doi[both]) > 1)), "%\n")
  r2$src <- ifelse(!is.na(year_doi), "doi", NA_character_)                     # only a DOI year counts
  n_list <- sum(r2$src == "field", na.rm = TRUE); n_doi <- sum(r2$src == "doi", na.rm = TRUE); n_txt <- sum(r2$src == "text", na.rm = TRUE)
  r2$year <- year_doi                                                          # no DOI or no Crossref result = dropped
  r2 <- r2[!is.na(r2$year), ]
  r2u <- r2[is.na(r2$key) | !duplicated(r2$key), ]                             # the same paper cited by several papers counts once
  cat(doc, ": DOIs", length(ds), "| known to Crossref", sum(vapply(got, function(g) g$found, logical(1))),
      "| with a reference list", sum(vapply(got, function(g) g$has_refs, logical(1))), "\n   entries:", length(year_doi),
      "| year via DOI:", n_doi, "| year field (no DOI result):", n_list, "| year from text:", n_txt,
      "| unknown:", length(year_doi) - n_list - n_doi - n_txt,
      sprintf("(coverage %.0f%%)", 100 * (n_list + n_doi + n_txt) / max(length(year_doi), 1)),
      "\n   unique works with a year:", nrow(r2u), "(removed", nrow(r2) - nrow(r2u), "duplicates)\n")
  their_doc[[doc]] <- tibble(doc = doc, key = r2u$key, year = r2u$year, src = r2u$src)
}

# ============ D. ALL CHAPTERS TOGETHER, OVERLAY, MORE STATS ==============
# chapter references per chapter: same work = same DOI, or same first-author surname + same start of the title
# (so Wang 2020a and Wang 2020b, or two different "Wang 2020", are NOT merged; initials are not used)
ent <- list()
for (doc in names(their_doc)) {
  rl <- res[[doc]]$refl_clean; yr <- res[[doc]]$list_years; nt <- res[[doc]]$n_text
  d  <- str_extract(rl, "10\\.\\d{4,9}/\\S+") |> str_remove("[.,;)]+$") |> tolower()
  a  <- str_extract(rl, "^[^,.(:]+") |> str_trim() |> str_remove("\\s*\\(?(?:18|19|20)\\d{2}[a-z]?\\)?$")
  ti <- str_remove(rl, "^.*?\\(?\\b(?:18|19|20)\\d{2}[a-z]?\\b\\)?[.,]?\\s*")         # text after the year = title first
  tt <- substr(str_remove_all(norm(str_extract(ti, "^[^.?!]+") |> coalesce(ti)), "[^a-z0-9]"), 1, 40)   # first 40 letters of the title (up to the first full stop)
  ent[[doc]] <- tibble(doc = doc, doi = d, ay = paste0("t:", norm(a), "|", tt), year = yr, txt = rl, n_text = nt)
}
ent <- bind_rows(ent) |> filter(!is.na(year))
# an entry without DOI (e.g. the Chapter 1 PDF) inherits the DOI of an entry with the same surname + title start in any chapter
ay2doi <- ent |> filter(!is.na(doi)) |> distinct(ay, .keep_all = TRUE) |> select(ay, doi_ay = doi)
ent <- ent |> left_join(ay2doi, by = "ay") |> mutate(key = coalesce(doi, doi_ay, ay))
mine_doc <- list()
for (doc in names(their_doc)) {
  m <- ent[ent$doc == doc, c("doc", "key", "year")]
  mine_doc[[doc]] <- m[!duplicated(m$key), ]                                   # a work listed twice in one chapter counts once
}


# pooled over chapters: a work cited in several chapters (or by several cited papers) counts ONCE
all_name <- "All chapters (unique)"
mine_all  <- bind_rows(mine_doc);  mine_all  <- mine_all[is.na(mine_all$key)  | !duplicated(mine_all$key), ]
their_all <- bind_rows(their_doc); their_all <- their_all[is.na(their_all$key) | !duplicated(their_all$key), ]

cat("\n-- redundancy --\n")
cat("chapter references:  ", nrow(bind_rows(mine_doc)),  "entries in the chapters ->", nrow(mine_all),  "unique works\n")
cat("references of cited papers: ", nrow(bind_rows(their_doc)), "entries (after per-chapter removal) ->", nrow(their_all), "unique works\n")
cat("unique chapter references that are also cited inside the cited papers:", sum(mine_all$key %in% their_all$key), "of", nrow(mine_all), "\n")
cat("\n-- chapter references shared between chapters (diagonal = chapter size) --\n")
ov <- sapply(names(mine_doc), function(a) sapply(names(mine_doc), function(b) length(intersect(mine_doc[[a]]$key, mine_doc[[b]]$key))))
print(ov)
cat("\n-- references of cited papers shared between chapters --\n")
ov2 <- sapply(names(their_doc), function(a) sapply(names(their_doc), function(b) length(intersect(na.omit(their_doc[[a]]$key), na.omit(their_doc[[b]]$key)))))
print(ov2)
# keys of "references of cited papers" without a DOI (author/volume/page or text) match less reliably: a lower bound
cat("\n% of references of cited papers identified by DOI, per chapter:\n")
print(sapply(their_doc, function(x) round(100 * mean(grepl("^10\\.", x$key), na.rm = TRUE))))
cat("\n-- references of cited papers shared between chapters, counting only DOI-identified works --\n")
print(sapply(names(their_doc), function(a) sapply(names(their_doc), function(b)
  length(intersect(grep("^10\\.", their_doc[[a]]$key, value = TRUE), grep("^10\\.", their_doc[[b]]$key, value = TRUE))))))

plot_df <- bind_rows(
  bind_rows(mine_doc)  |> transmute(doc, level = "chapter references",  year),
  bind_rows(their_doc) |> transmute(doc, level = "references of cited papers", year),
  mine_all  |> transmute(doc = all_name, level = "chapter references",  year),
  their_all |> transmute(doc = all_name, level = "references of cited papers", year)) |>
  mutate(doc   = factor(doc, levels = c(names(files), all_name)),
         level = factor(level, levels = c("references of cited papers", "chapter references")))   # chapter references are drawn on top

# --- statistics: every chapter and the pooled set, both levels (age = chapter date - mid-year of the reference) ---
set.seed(1)
combos <- plot_df |> distinct(doc, level) |> arrange(level, doc)
stats2 <- list()
for (k in seq_len(nrow(combos))) {
  d <- as.character(combos$doc[k]); l <- as.character(combos$level[k])
  y <- plot_df$year[plot_df$doc == d & plot_df$level == l]
  now <- if (d %in% names(chapter_date)) chapter_date[[d]] else max(chapter_date)
  age <- now - (y + 0.5); y <- y[age >= 0]; age <- age[age >= 0]
  boot <- replicate(2000, median(sample(age, replace = TRUE)))
  a <- floor(age); a <- a[a <= 50]
  lambda <- if (length(a) > 3) -coef(glm(n ~ age, family = poisson,
                                         data = data.frame(age = 0:max(a), n = tabulate(a + 1, nbins = max(a) + 1))))[["age"]] else NA_real_
  stats2[[k]] <- tibble(
    doc = d, level = l, n = length(y),
    median_year = median(y), q25_year = unname(quantile(y, .25)), q75_year = unname(quantile(y, .75)),
    oldest = min(y), newest = max(y),
    age_mean = mean(age), age_sd = sd(age), age_iqr = IQR(age),
    pct_last5y = round(100 * mean(age <= 5)), pct_last10y = round(100 * mean(age <= 10)),
    pct_older20y = round(100 * mean(age > 20)), pct_older30y = round(100 * mean(age > 30)),
    pct_in_phd = round(100 * mean(y + 0.5 >= phd_start & y + 0.5 <= phd_end)),
    age_median = median(age), age_median_lo = unname(quantile(boot, .025)), age_median_hi = unname(quantile(boot, .975)),
    age_50pct_expfit = if (is.finite(lambda) && lambda > 0) log(2) / lambda else NA_real_)
}
stats2 <- bind_rows(stats2)
stats_chapters <- stats2 |> filter(doc != all_name)
stats_all      <- stats2 |> filter(doc == all_name)
print(as.data.frame(stats_chapters), digits = 3)
print(as.data.frame(stats_all), digits = 3)
# write.csv(stats2, "citation_stats.csv", row.names = FALSE)

# --- figures: references of cited papers (grey) with chapter references (red) on top, same y-axis within a panel ---
med2 <- plot_df |> group_by(doc, level) |> summarise(med = median(year) + 0.5, .groups = "drop")
cols <- c("references of cited papers" = "grey70", "chapter references" = "firebrick")
cols2 <- c("references of cited papers" = "grey45", "chapter references" = "firebrick")

# per chapter
pd1 <- plot_df |> filter(doc != all_name) |> droplevels()
md1 <- med2    |> filter(doc != all_name) |> droplevels()
p_histograms_per_chapter <- ggplot(pd1, aes(year, fill = level)) +
  annotate("rect", xmin = phd_start, xmax = phd_end, ymin = -Inf, ymax = Inf, fill = "steelblue", alpha = 0.45) +
  geom_histogram(binwidth = 1, boundary = 0, position = "identity") +
  geom_vline(data = md1, aes(xintercept = med, colour = level), linetype = "dashed") +
  scale_fill_manual(values = cols) + scale_colour_manual(values = cols2) +
  facet_wrap(~doc, scales = "free_y", nrow = 2) +
  labs(x = NULL, y = "unique works", fill = NULL, colour = NULL) +
  theme_minimal(base_size = 14) + theme(legend.position = "bottom")
print(p_histograms_per_chapter)
ggsave(file.path(fig_dir, "histograms_per_chapter.svg"), p_histograms_per_chapter, device = svg_dev, width = 12, height = 7)

# all chapters pooled
pd2 <- plot_df |> filter(doc == all_name) |> droplevels()
md2 <- med2    |> filter(doc == all_name) |> droplevels()
p_histogram_all_chapters <- ggplot(pd2, aes(year, fill = level)) +
  annotate("rect", xmin = phd_start, xmax = phd_end, ymin = -Inf, ymax = Inf, fill = "steelblue", alpha = 0.45) +
  geom_histogram(binwidth = 1, boundary = 0, position = "identity") +
  geom_vline(data = md2, aes(xintercept = med, colour = level), linetype = "dashed") +
  scale_fill_manual(values = cols) + scale_colour_manual(values = cols2) +
  labs(x = NULL, y = "unique works", fill = NULL, colour = NULL) +
  theme_minimal(base_size = 14) + theme(legend.position = "bottom")
print(p_histogram_all_chapters)
ggsave(file.path(fig_dir, "histogram_all_chapters.svg"), p_histogram_all_chapters, device = svg_dev, width = 8, height = 5)

# ---- when is a study most likely to be cited? peak of the smoothed age distribution ----
# NOTE: for "references of cited papers" the age here is measured at the chapter date; section E gives the lag at their own date
bw_mult <- 2          # smoothing: 1 = automatic bandwidth (Sheather-Jones), 2-3 = clearly smoother
set.seed(1)
peak <- list(); dens <- list()
combos <- plot_df |> distinct(doc, level)
for (k in seq_len(nrow(combos))) {
  d <- as.character(combos$doc[k]); l <- as.character(combos$level[k])
  now <- if (d %in% names(chapter_date)) chapter_date[[d]] else max(chapter_date)
  age <- now - (plot_df$year[plot_df$doc == d & plot_df$level == l] + 0.5)
  age <- age[age >= 0]
  bw <- bw_mult * tryCatch(bw.SJ(age), error = function(e) bw.nrd0(age))   # SJ fails on small / tied samples -> rule of thumb
  dn <- density(age, bw = bw, from = 0, to = 50)
  mode_age <- dn$x[which.max(dn$y)]
  boot <- replicate(500, { s  <- sample(age, replace = TRUE)
  dd <- density(s, bw = bw, from = 0, to = 50)
  dd$x[which.max(dd$y)] })
  dens[[k]] <- tibble(doc = d, level = l, age = dn$x, dens = dn$y)
  peak[[k]] <- tibble(doc = d, level = l, n = length(age), peak_age = mode_age,
                      peak_lo = unname(quantile(boot, .025)), peak_hi = unname(quantile(boot, .975)),
                      cited_peak_year = now + mode_age)
}
peak <- bind_rows(peak); dens <- bind_rows(dens)
print(as.data.frame(peak), digits = 3)
dens$doc <- factor(dens$doc, levels = c(names(files), all_name))
peak$doc <- factor(peak$doc, levels = levels(dens$doc))

p_density_per_chapter <- ggplot(dens |> filter(doc != all_name) |> droplevels(), aes(age, dens, colour = level)) +
  geom_line(linewidth = 1) +
  geom_vline(data = peak |> filter(doc != all_name) |> droplevels(), aes(xintercept = peak_age, colour = level), linetype = "dashed") +
  scale_colour_manual(values = cols) + facet_wrap(~doc, nrow = 2) +
  labs(x = "Age of reference at writing (years)", y = "Density", colour = NULL) +
  theme_minimal(base_size = 14) + theme(legend.position = "bottom")
print(p_density_per_chapter)
ggsave(file.path(fig_dir, "density_per_chapter.svg"), p_density_per_chapter, device = svg_dev, width = 12, height = 7)

p_density_all_chapters <- ggplot(dens |> filter(doc == all_name) |> droplevels(), aes(age, dens, colour = level)) +
  geom_line(linewidth = 1) +
  geom_vline(data = peak |> filter(doc == all_name) |> droplevels(), aes(xintercept = peak_age, colour = level), linetype = "dashed") +
  scale_colour_manual(values = cols) +
  labs(x = "Age of reference at writing (years)", y = "Density", colour = NULL) +
  theme_minimal(base_size = 14) + theme(legend.position = "bottom")
print(p_density_all_chapters)
ggsave(file.path(fig_dir, "density_all_chapters.svg"), p_density_all_chapters, device = svg_dev, width = 8, height = 5)


# ============ E. DOES THE CITATION LAG CHANGE OVER TIME? (one curve per cited paper) =================
# For every paper you cite: age of each of ITS references at the time that paper was published
# (age = year of the cited paper - year of the reference). One median age per paper, then its trend over time.
# Not de-duplicated on purpose: every citation is one observation of a lag.
min_refs <- 15      # papers with fewer references (with a known year) are too noisy for a median

# 1. publication year of every cited paper (batched DOI lookup, same as pass 2; cached in doi_year)
need3   <- setdiff(all_dois, names(doi_year))
need3   <- need3[!grepl("[,;<>]", need3)]
batches <- split(need3, ceiling(seq_along(need3) / 50))
cat("publication years to look up:", length(need3), "\n")
for (b in seq_along(batches)) {
  d   <- batches[[b]]
  got <- tryCatch(cr_json(paste0("works?filter=", paste0("doi:", enc(d), collapse = ","), "&select=DOI,issued&rows=50")),
                  error = function(e) e)
  if (inherits(got, "error")) { message("Stopped at batch ", b, ": ", conditionMessage(got), " (rerun to continue)"); break }
  items <- got$items
  yrs   <- setNames(vapply(items, issued_year, integer(1)), tolower(vapply(items, function(w) w$DOI, "")))
  if (length(items) == 0 && length(d) > 1) {
    yrs <- integer(0)
    for (one in d) { w <- tryCatch(cr_get(one), error = function(e) NULL); if (!is.null(w)) yrs[one] <- issued_year(w); Sys.sleep(0.2) }
  }
  doi_year[d] <- NA_integer_
  doi_year[names(yrs)] <- yrs
  saveRDS(doi_year, doi_year_file)
  Sys.sleep(0.2)
}

# 2. one row per citation: which paper, its year, the year of the reference
ev <- list()
for (doi in all_dois) {
  g <- cr_cache[[doi]]
  if (is.null(g) || !g$has_refs) next
  ev[[doi]] <- tibble(paper = doi, paper_year = unname(doi_year[doi]),
                      ref_year = unname(doi_year[g$refs$doi]))      # DOI year only
}
ev <- bind_rows(ev) |>
  filter(!is.na(paper_year), !is.na(ref_year)) |>
  mutate(age = paper_year - ref_year) |>
  filter(age >= 0)
cat("citations with a known lag:", nrow(ev), "| from", n_distinct(ev$paper), "papers\n")

# 3. one median age per cited paper
paper_df <- ev |>
  group_by(paper, paper_year) |>
  summarise(n = n(), med_age = median(age), q25 = unname(quantile(age, .25)), q75 = unname(quantile(age, .75)),
            pct_last5y = 100 * mean(age <= 5), .groups = "drop") |>
  filter(n >= min_refs)
cat("papers with >=", min_refs, "references:", nrow(paper_df), "\n")

# trend of the median age with publication year (weighted by number of references)
fit <- lm(med_age ~ paper_year, data = paper_df, weights = n)
cat("\nchange in median age per year (years/year), with 95% CI:\n")
print(cbind(estimate = coef(fit), confint(fit))["paper_year", , drop = FALSE])
print(cor.test(paper_df$paper_year, paper_df$med_age, method = "spearman", exact = FALSE)[c("estimate", "p.value")])

p_median_age_trend <- ggplot(paper_df, aes(paper_year, med_age)) +
  geom_point(aes(size = n), alpha = 0.5) +
  geom_smooth(aes(weight = n), method = "loess", formula = y ~ x, colour = "firebrick", fill = "firebrick", alpha = 0.2) +
  geom_smooth(aes(weight = n), method = "lm", formula = y ~ x, colour = "black", linetype = "dashed", se = FALSE) +
  labs(x = "Publication year of the cited paper", y = "Median age of its references (years)", size = "references") +
  theme_minimal(base_size = 14)
print(p_median_age_trend)
ggsave(file.path(fig_dir, "median_age_trend.svg"), p_median_age_trend, device = svg_dev, width = 8, height = 5)

# the curves themselves: age distribution of references, by period of the citing paper
ev_p <- ev |> semi_join(paper_df, by = "paper") |>
  mutate(period = cut(paper_year, c(-Inf, 1999, 2009, 2019, Inf), labels = c("up to 1999", "2000s", "2010s", "2020s")))
p_lag_ecdf_by_period <- ggplot(ev_p, aes(age, colour = period)) +
  stat_ecdf(linewidth = 1) +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  coord_cartesian(xlim = c(0, 50)) +
  scale_x_continuous(breaks = seq(0, 50, 5)) +
  labs(x = "Age of reference when cited (years)", y = "Share of references that are younger", colour = "citing paper from") +
  theme_minimal(base_size = 14)
print(p_lag_ecdf_by_period)
ggsave(file.path(fig_dir, "lag_ecdf_by_period.svg"), p_lag_ecdf_by_period, device = svg_dev, width = 8, height = 5)

# 4. corrected median age of "references of cited papers" per chapter (age at the cited paper's publication, not at the chapter date)
set.seed(1)
their_lag <- list()
for (doc in names(dois_by_doc)) {
  e    <- ev[ev$paper %in% unique(tolower(trimws(dois_by_doc[[doc]]))), ]
  ages <- split(e$age, e$paper)
  boot <- replicate(1000, median(unlist(ages[sample(names(ages), replace = TRUE)])))   # resample whole papers
  their_lag[[doc]] <- tibble(doc = doc, papers = length(ages), citations = nrow(e),
                             age_median = median(e$age), age_median_lo = unname(quantile(boot, .025)),
                             age_median_hi = unname(quantile(boot, .975)),
                             age_q25 = unname(quantile(e$age, .25)), age_q75 = unname(quantile(e$age, .75)))
}
their_lag <- bind_rows(their_lag)
print(as.data.frame(their_lag), digits = 3)

# ============ F. SELF-CITATIONS (among the references listed in the chapters) =================
# Self-citation = an entry that has one of these people among its authors. Matching is on the author part of the entry
# (the text before the year), ignoring accents. CHECK the printed entries: a common surname can match another person.
# EDIT: surnames (lower case, no accents; regular expressions) of the people whose works count as self-citations
self_rx <- c(Cunow = "cunow", Vaisanen = "vaisanen", Olofsson = "olofsson", `Blume-Werry` = "blume[- ]*werry")

ent$author_part <- str_extract(ent$txt, "^.*?(?=\\(?\\b(?:18|19|20)\\d{2}[a-z]?\\b)") |> coalesce(ent$txt) |> norm()
ent$first_part  <- str_extract(ent$txt, "^[^,.(:]+") |> coalesce("") |> norm()
for (nm in names(self_rx)) ent[[paste0("by_", nm)]] <- str_detect(ent$author_part, self_rx[[nm]])
ent$self_any   <- str_detect(ent$author_part, paste(self_rx, collapse = "|"))
ent$self_first <- str_detect(ent$first_part,  paste(self_rx, collapse = "|"))

# unique works: once per chapter, and once over all chapters
ent_u  <- ent[!duplicated(ent[c("doc", "key")]), ]
pooled <- ent[!duplicated(ent$key), ] |> mutate(doc = all_name)

self_tab <- bind_rows(ent_u, pooled) |>
  group_by(doc) |>
  summarise(n_refs = n(),
            n_self_any = sum(self_any),   pct_self_any   = round(100 * mean(self_any), 1),
            n_self_first = sum(self_first), pct_self_first = round(100 * mean(self_first), 1),
            across(starts_with("by_"), sum),
            median_year_self   = median(year[self_any]),
            median_year_others = median(year[!self_any]), .groups = "drop") |>
  mutate(doc = factor(doc, levels = c(names(files), all_name))) |> arrange(doc)
print(as.data.frame(self_tab))

cat("\n-- entries counted as self-citations (check by hand) --\n")
chk <- ent_u[ent_u$self_any, ]
print(data.frame(doc = chk$doc, year = chk$year, entry = substr(chk$txt, 1, 90)), right = FALSE)

self_long <- bind_rows(self_tab |> transmute(doc, name = "any author",   value = pct_self_any),
                       self_tab |> transmute(doc, name = "first author", value = pct_self_first))
p_self_citation_share <- ggplot(self_long, aes(doc, value, fill = name)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("any author" = "firebrick", "first author" = "grey50")) +
  labs(x = NULL, y = "% of the references", fill = "Self-citation as") +
  theme_minimal(base_size = 14) + theme(legend.position = "bottom")
print(p_self_citation_share)
ggsave(file.path(fig_dir, "self_citation_share.svg"), p_self_citation_share, device = svg_dev, width = 8, height = 5)

# ============ G. HOW OFTEN IS THE SAME REFERENCE CITED IN THE TEXT? (rank-abundance) =================
# Unit: one entry of a chapter's reference list. Count = number of times it is cited in the TEXT of that chapter
# (n_text from section A). Entries never found in the text have count 0 and are reported separately.
cnt_doc <- ent[!duplicated(ent[c("doc", "key")]), ] |> select(doc, key, year, txt, n_text)
cnt_all <- ent |> group_by(key) |>
  summarise(n_chapters = n_distinct(doc), year = first(year), txt = first(txt),
            n_text = sum(n_text), .groups = "drop") |>                                 # a work used in several chapters: counts add up
  mutate(doc = all_name)

cnt_both <- bind_rows(cnt_doc, cnt_all |> select(doc, key, year, txt, n_text))
ra_stats <- list()
for (d in unique(cnt_both$doc)) {
  n_all <- cnt_both$n_text[cnt_both$doc == d]
  n     <- sort(n_all[n_all > 0], decreasing = TRUE)                       # references that are cited in the text
  top   <- ceiling(0.10 * length(n))
  ra_stats[[d]] <- tibble(
    doc = d, n_refs_listed = length(n_all), n_not_cited_in_text = sum(n_all == 0), n_cited = length(n),
    citations_total = sum(n), mean_per_ref = mean(n), median_per_ref = median(n), max_per_ref = max(n),
    pct_cited_once = round(100 * mean(n == 1), 1), pct_cited_5plus = round(100 * mean(n >= 5), 1),
    share_of_citations_top10pct = round(100 * sum(n[seq_len(top)]) / sum(n), 1),   # share of all citations going to the 10 % most cited
    gini = sum((2 * seq_along(rev(n)) - length(n) - 1) * rev(n)) / (length(n) * sum(n)))   # 0 = all cited equally often, 1 = one reference gets everything
}
ra_stats <- bind_rows(ra_stats) |> mutate(doc = factor(doc, levels = c(names(files), all_name))) |> arrange(doc)
print(as.data.frame(ra_stats), digits = 3)

cat("\n-- the 15 most often cited references (all chapters pooled) --\n")
top15 <- cnt_all[cnt_all$n_text > 0, ]; top15 <- top15[order(-top15$n_text), ][1:min(15, nrow(top15)), ]
print(data.frame(n_text = top15$n_text, n_chapters = top15$n_chapters, year = top15$year, entry = substr(top15$txt, 1, 80)), right = FALSE)

# rank-abundance: rank 1 = most cited reference
ra <- cnt_both |> filter(n_text > 0) |> group_by(doc) |>
  arrange(desc(n_text), .by_group = TRUE) |> mutate(rank = row_number()) |> ungroup() |>
  mutate(doc = factor(doc, levels = c(names(files), all_name)))

# per chapter
p_rank_abundance_chapters <- ggplot(ra |> filter(doc != all_name) |> droplevels(), aes(rank, n_text, colour = doc)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.5) +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20, 50)) +
  labs(x = "Rank of the reference (1 = most often cited in the text)", y = "Citations in the text (log scale)", colour = NULL) +
  theme_minimal(base_size = 14) + theme(legend.position = "bottom")
print(p_rank_abundance_chapters)
ggsave(file.path(fig_dir, "rank_abundance_per_chapter.svg"), p_rank_abundance_chapters, device = svg_dev, width = 8, height = 5)

# all chapters pooled (a work used in several chapters: citations added up)
p_rank_abundance_all <- ggplot(ra |> filter(doc == all_name) |> droplevels(), aes(rank, n_text)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.5) +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20, 50)) +
  labs(x = "Rank of the reference (1 = most often cited in the text)", y = "Citations in the text (log scale)") +
  theme_minimal(base_size = 14)
print(p_rank_abundance_all)
ggsave(file.path(fig_dir, "rank_abundance_all_chapters.svg"), p_rank_abundance_all, device = svg_dev, width = 8, height = 5)
