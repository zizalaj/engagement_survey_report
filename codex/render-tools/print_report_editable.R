setwd("C:/Users/JuiceUP/OneDrive - JuiceUP s.r.o/Plocha/Engagement survey/Survey automatizace/Codex/automatizace AI")

args <- commandArgs(trailingOnly = TRUE)

input_html <- if (length(args) >= 1) args[[1]] else "report_test.html"
output_pdf <- if (length(args) >= 2) args[[2]] else file.path("pdf_output", "report_editable_print.pdf")

if (!requireNamespace("pagedown", quietly = TRUE)) {
  stop("Package 'pagedown' is required. Install it with install.packages('pagedown').", call. = FALSE)
}

if (!file.exists(input_html)) {
  stop(
    sprintf("Input HTML does not exist: %s", normalizePath(input_html, winslash = "/", mustWork = FALSE)),
    call. = FALSE
  )
}

output_dir <- dirname(output_pdf)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

message(sprintf("Printing '%s' to '%s'...", input_html, output_pdf))

pagedown::chrome_print(
  input = normalizePath(input_html, winslash = "/", mustWork = TRUE),
  output = normalizePath(output_pdf, winslash = "/", mustWork = FALSE)
)

message("Done.")
