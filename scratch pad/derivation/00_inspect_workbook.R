library(readxl)
f <- "C:/Dissertation/01_Raw_data/rating curves_clean.xlsx"
shts <- excel_sheets(f)
print(shts)
for (s in shts) {
  d <- read_excel(f, sheet = s, n_max = 5, .name_repair = "minimal")
  cat("\n=== SHEET:", s, "===\n")
  cat("cols:", paste(names(d), collapse = " | "), "\n")
  print(as.data.frame(d))
  dfull <- read_excel(f, sheet = s, .name_repair = "minimal")
  cat("nrow:", nrow(dfull), "\n")
}
