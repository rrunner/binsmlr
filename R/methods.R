# print method for objects of class bin
print.bin <- function(x, ...) {
  cat(paste0("Bin ", if (x[["is_merged"]]) "(merged)"), "\n")
  cat("---", "\n\n")
  cat("Obligors:", "\t", x[["num_obl"]], "\n")
  cat("Defaults:", "\t", x[["num_def"]], "\n")
  cat("\n")
  cat("Default rate:", "\t", x[["dr"]], "\n")
  cat("Odds ratio:", "\t", x[["odds"]], "\n")
  cat("\n")
  cat("Min score:", "\t", x[["min_score"]], "\n")
  cat("Mid score:", "\t", x[["mid_score"]], "\n")
  cat("Max score:", "\t", x[["max_score"]], "\n")
}
