
ensure_pdftools <- function() {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Package 'pdftools' is required. Install it with install.packages('pdftools').", call. = FALSE)
  }
}

parse_pages <- function(page_spec) {
  if (length(page_spec) > 1) {
    return(sort(unique(as.integer(page_spec))))
  }

  if (is.numeric(page_spec)) {
    return(sort(unique(as.integer(page_spec))))
  }

  parts <- trimws(unlist(strsplit(as.character(page_spec), ",")))
  vals <- integer(0)

  for (part in parts) {
    if (!nzchar(part)) {
      next
    }

    if (grepl("^[0-9]+-[0-9]+$", part)) {
      bounds <- as.integer(unlist(strsplit(part, "-")))
      vals <- c(vals, seq.int(bounds[1], bounds[2]))
    } else if (grepl("^[0-9]+$", part)) {
      vals <- c(vals, as.integer(part))
    } else {
      stop(sprintf("Invalid page selector: '%s'", part), call. = FALSE)
    }
  }

  sort(unique(vals))
}

export_pdf_pages <- function(pdf_path, pages, output_dir, dpi = 160L) {
  ensure_pdftools()

  dpi <- as.integer(dpi)
  if (is.na(dpi) || dpi <= 0) {
    stop("DPI must be a positive integer.", call. = FALSE)
  }

  if (!file.exists(pdf_path)) {
    stop(
      sprintf("PDF does not exist: %s", normalizePath(pdf_path, winslash = "/", mustWork = FALSE)),
      call. = FALSE
    )
  }

  selected_pages <- parse_pages(pages)
  if (length(selected_pages) == 0) {
    stop("No valid pages were selected.", call. = FALSE)
  }

  info <- pdftools::pdf_info(pdf_path)
  if (max(selected_pages) > info$pages) {
    stop(
      sprintf("Requested page %d, but PDF only has %d pages.", max(selected_pages), info$pages),
      call. = FALSE
    )
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  message(sprintf(
    "Exporting pages %s from '%s' into '%s' at %d DPI...",
    paste(selected_pages, collapse = ", "),
    pdf_path,
    output_dir,
    dpi
  ))

  files <- pdftools::pdf_convert(
    pdf = pdf_path,
    pages = selected_pages,
    dpi = dpi,
    filenames = file.path(output_dir, sprintf("page-%02d.png", selected_pages))
  )

  invisible(files)
}

create_target_pngs <- function(pages, dpi = 160L) {
  export_pdf_pages(
    pdf_path = "GRAFICKY_NAVHR_Engagement.pdf",
    pages = pages,
    output_dir = file.path("codex", "render-tools", "pdf-pages", "target"),
    dpi = dpi
  )
}

create_iteration_pngs <- function(pages, dpi = 160L) {
  export_pdf_pages(
    pdf_path = file.path("pdf_output", "report_editable_print.pdf"),
    pages = pages,
    output_dir = file.path("codex", "render-tools", "pdf-pages", "current"),
    dpi = dpi
  )
}
