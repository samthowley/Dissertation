# 01_extract_workbook_RC.R
# Extract Matt's rating-curve workbook (01_Raw_data/rating curves_clean.xlsx):
#  - per-site continuous series (Water Depth, Flow) from each "Site X" sheet
#  - per-site DG measurement pairs (stage, Q)
#  - Summarized sheet daily Q per site
# Then reverse-engineer the depth->Q rule for each site from the continuous columns.

library(tidyverse)
library(readxl)

setwd("C:/Dissertation")
f <- "01_Raw_data/rating curves_clean.xlsx"
out_dir <- "scratch pad"

sites <- c("3","5","5a","6","6a","7","9","13","14","15")

# ---- continuous series from each site sheet ----
cont_all <- data.frame()
for (s in sites) {
  sh  <- paste0("Site ", s)
  d   <- read_excel(f, sheet = sh, .name_repair = "minimal")
  nms <- names(d)
  # continuous block = last three named columns of every sheet
  named <- which(nms != "")
  cols  <- tail(named, 3)
  cat(sh, ":", paste(nms[cols], collapse = " | "), "\n")
  cc <- d[, cols]
  names(cc) <- c("DateTime", "depth", "Q")
  # DateTime can arrive as POSIXct or as excel serial number
  if (!inherits(cc$DateTime, "POSIXct")) {
    cc$DateTime <- as.POSIXct(as.numeric(cc$DateTime) * 86400,
                              origin = "1899-12-30", tz = "UTC")
  }
  cc <- cc %>%
    mutate(depth = as.numeric(depth), Q = as.numeric(Q), ID = s) %>%
    filter(!is.na(DateTime), !(is.na(depth) & is.na(Q)))
  cont_all <- rbind(cont_all, cc)
}
write_csv(cont_all, file.path(out_dir, "workbook_continuous.csv"))

# ---- DG measurement pairs (stage, Q) from left block ----
dg_all <- data.frame()
for (s in sites) {
  sh  <- paste0("Site ", s)
  d   <- read_excel(f, sheet = sh, .name_repair = "minimal")
  nms <- names(d)
  first_blank <- which(nms == "")[1]  # DG block sits left of the first unnamed spacer
  q_col     <- which(str_detect(nms, regex("Flow \\(Q", ignore_case = TRUE)))[1]
  stage_col <- which(str_detect(nms, regex("^(Adj)?Stage", ignore_case = TRUE)) &
                     seq_along(nms) < first_blank)
  stage_col <- tail(stage_col, 1)  # prefer AdjStage where present (5a)
  if (is.na(q_col) | length(stage_col) == 0) { cat(sh, ": no DG block found\n"); next }
  dg <- d[, c(1, q_col, stage_col)]
  names(dg) <- c("DateTime", "Q_dg", "stage")
  dg <- dg %>%
    mutate(Q_dg = as.numeric(Q_dg), stage = as.numeric(stage), ID = s) %>%
    filter(!is.na(Q_dg) | !is.na(stage))
  dg_all <- rbind(dg_all, dg)
}
write_csv(dg_all, file.path(out_dir, "workbook_DG_pairs.csv"))

# ---- Summarized sheet: wide blocks -> long ----
sm  <- read_excel(f, sheet = "Summarized", .name_repair = "minimal")
nms <- names(sm)
site_starts <- which(str_detect(nms, "^Site "))
summ_all <- data.frame()
for (i in seq_along(site_starts)) {
  st <- site_starts[i]
  id <- str_remove(nms[st], "^Site ")
  block <- sm[, st:(st + 6)]
  bn <- names(block)
  date_i <- which(str_detect(bn, "^Date"))[1]
  q_i    <- which(str_detect(bn, regex("^Q ", ignore_case = TRUE)))[1]
  b <- block[, c(date_i, q_i)]
  names(b) <- c("Date", "Q_summ")
  b <- b %>%
    mutate(Date = as.Date(Date), Q_summ = as.numeric(Q_summ), ID = id) %>%
    filter(!is.na(Date))
  summ_all <- rbind(summ_all, b)
}
write_csv(summ_all, file.path(out_dir, "workbook_summarized.csv"))
cat("\nSummarized sites:", paste(unique(summ_all$ID), collapse = ", "), "\n")

# ---- reverse-engineer depth->Q rule per site ----
cat("\n================ depth->Q rule per site ================\n")
for (s in sites) {
  cc <- cont_all %>% filter(ID == s, !is.na(depth), !is.na(Q))
  if (nrow(cc) == 0) { cat("Site", s, ": no paired depth/Q\n"); next }
  pos <- cc %>% filter(depth > 0, Q > 0)
  n_zeroQ  <- sum(cc$Q == 0)
  max_d_zeroQ <- suppressWarnings(max(cc$depth[cc$Q == 0]))
  min_d_posQ  <- suppressWarnings(min(cc$depth[cc$Q > 0]))
  # is Q a deterministic function of depth?
  dup <- cc %>% group_by(depth) %>%
    summarize(nQ = n_distinct(round(Q, 6)), .groups = "drop") %>%
    filter(nQ > 1)
  fitline <- ""
  if (nrow(pos) > 10) {
    m <- lm(log10(Q) ~ log10(depth), data = pos)
    a <- 10^coef(m)[1]; b <- coef(m)[2]
    pred <- a * pos$depth^b
    maxrel <- max(abs(pred - pos$Q) / pos$Q)
    fitline <- sprintf("power fit: Q = %.6g * depth^%.6g | R2=%.6f | max rel err=%.3g",
                       a, b, summary(m)$r.squared, maxrel)
  }
  cat(sprintf("Site %-3s n=%6d | Q==0: %5d (depth up to %.4f; min depth w/ Q>0 = %.4f) | depths mapping to >1 Q: %d\n   %s\n",
      s, nrow(cc), n_zeroQ,
      ifelse(is.finite(max_d_zeroQ), max_d_zeroQ, NA),
      ifelse(is.finite(min_d_posQ), min_d_posQ, NA),
      nrow(dup), fitline))
}
