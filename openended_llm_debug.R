#!/usr/bin/env Rscript

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

required_packages <- c("dplyr", "ggplot2", "httr", "jsonlite")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(
    "Required package(s) not installed: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

`%>%` <- dplyr::`%>%`

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
candidate_roots <- unique(Filter(nzchar, c(
  if (length(script_arg) > 0) {
    dirname(normalizePath(sub("^--file=", "", script_arg[[1]]), winslash = "/", mustWork = TRUE))
  } else {
    ""
  },
  normalizePath(getwd(), winslash = "/", mustWork = TRUE),
  tryCatch(
    dirname(normalizePath("openended_llm_debug.R", winslash = "/", mustWork = TRUE)),
    error = function(e) ""
  )
)))

repo_root <- ""
for (candidate_root in candidate_roots) {
  if (
    file.exists(file.path(candidate_root, "R", "typeform_helpers.R")) &&
    file.exists(file.path(candidate_root, "R", "openended_helpers.R")) &&
    file.exists(file.path(candidate_root, "openended_llm_debug.R"))
  ) {
    repo_root <- candidate_root
    break
  }
}

if (!nzchar(repo_root)) {
  stop(
    "Could not resolve the project root for openended_llm_debug.R. ",
    "Run it from the repo root or source it after setwd(repo_root).",
    call. = FALSE
  )
}

script_path <- file.path(repo_root, "openended_llm_debug.R")

source(file.path(repo_root, "R", "typeform_helpers.R"), local = TRUE)
source(file.path(repo_root, "R", "openended_helpers.R"), local = TRUE)

cat_line <- function(...) {
  cat(paste0(..., collapse = ""), "\n", sep = "")
}

print_usage <- function(message = NULL) {
  usage_lines <- c(
    "Usage:",
    "  Rscript openended_llm_debug.R [options]",
    "  source(\"openended_llm_debug.R\"); run_openended_llm_debug(...)",
    "",
    "Options:",
    "  --form-id VALUE                  Typeform form id. Defaults to report_editable_api.qmd value r5VHgEYK.",
    "  --input-rds PATH                 Read final_long_df from an .rds file instead of Typeform.",
    "  --input-csv PATH                 Read final_long_df from a .csv file instead of Typeform.",
    "  --question-ref VALUE             Restrict run to one open-ended question ref.",
    "  --question-title-pattern VALUE   Restrict run to question titles matching this regex.",
    "  --max-questions N                Limit how many open-ended questions to process.",
    "  --max-responses-per-question N   Limit how many responses are sent to OpenAI per question.",
    "  --page-size N                    Typeform responses page size. Default 1000.",
    "  --max-pages N                    Max Typeform response pages to fetch. Default 200.",
    "  --output-dir PATH                Output directory. Default: output/openended-llm-debug-<timestamp>/",
    "  --help                           Show this help.",
    "",
    "Examples:",
    "  Rscript openended_llm_debug.R",
    "  Rscript openended_llm_debug.R --form-id r5VHgEYK --question-ref abc123",
    "  Rscript openended_llm_debug.R --question-title-pattern \"Co by\" --max-responses-per-question 50",
    "  Rscript openended_llm_debug.R --input-rds output/final_long_df.rds --max-questions 1"
  )

  if (!is.null(message) && nzchar(message)) {
    cat_line("Error: ", message)
    cat_line("")
  }

  cat(paste(usage_lines, collapse = "\n"), "\n", sep = "")
}

read_arg_value <- function(args, i) {
  if (i >= length(args)) {
    stop("Missing value for argument ", args[[i]], call. = FALSE)
  }
  args[[i + 1L]]
}

parse_int_arg <- function(value, name) {
  parsed <- suppressWarnings(as.integer(value))
  if (is.na(parsed) || parsed < 1L) {
    stop(name, " must be a positive integer.", call. = FALSE)
  }
  parsed
}

parse_cli_args <- function(args) {
  opts <- list(
    form_id = "r5VHgEYK",
    input_rds = NULL,
    input_csv = NULL,
    question_ref = NULL,
    question_title_pattern = NULL,
    max_questions = NULL,
    max_responses_per_question = NULL,
    page_size = 1000L,
    max_pages = 200L,
    output_dir = file.path(
      repo_root,
      "output",
      paste0("openended-llm-debug-", format(Sys.time(), "%Y%m%d-%H%M%S"))
    ),
    help = FALSE
  )

  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]

    if (arg %in% c("--help", "-h")) {
      opts$help <- TRUE
    } else if (arg == "--form-id") {
      opts$form_id <- read_arg_value(args, i)
      i <- i + 1L
    } else if (startsWith(arg, "--form-id=")) {
      opts$form_id <- sub("^--form-id=", "", arg)
    } else if (arg == "--input-rds") {
      opts$input_rds <- read_arg_value(args, i)
      i <- i + 1L
    } else if (startsWith(arg, "--input-rds=")) {
      opts$input_rds <- sub("^--input-rds=", "", arg)
    } else if (arg == "--input-csv") {
      opts$input_csv <- read_arg_value(args, i)
      i <- i + 1L
    } else if (startsWith(arg, "--input-csv=")) {
      opts$input_csv <- sub("^--input-csv=", "", arg)
    } else if (arg == "--question-ref") {
      opts$question_ref <- read_arg_value(args, i)
      i <- i + 1L
    } else if (startsWith(arg, "--question-ref=")) {
      opts$question_ref <- sub("^--question-ref=", "", arg)
    } else if (arg == "--question-title-pattern") {
      opts$question_title_pattern <- read_arg_value(args, i)
      i <- i + 1L
    } else if (startsWith(arg, "--question-title-pattern=")) {
      opts$question_title_pattern <- sub("^--question-title-pattern=", "", arg)
    } else if (arg == "--max-questions") {
      opts$max_questions <- parse_int_arg(read_arg_value(args, i), "--max-questions")
      i <- i + 1L
    } else if (startsWith(arg, "--max-questions=")) {
      opts$max_questions <- parse_int_arg(sub("^--max-questions=", "", arg), "--max-questions")
    } else if (arg == "--max-responses-per-question") {
      opts$max_responses_per_question <- parse_int_arg(
        read_arg_value(args, i),
        "--max-responses-per-question"
      )
      i <- i + 1L
    } else if (startsWith(arg, "--max-responses-per-question=")) {
      opts$max_responses_per_question <- parse_int_arg(
        sub("^--max-responses-per-question=", "", arg),
        "--max-responses-per-question"
      )
    } else if (arg == "--page-size") {
      opts$page_size <- parse_int_arg(read_arg_value(args, i), "--page-size")
      i <- i + 1L
    } else if (startsWith(arg, "--page-size=")) {
      opts$page_size <- parse_int_arg(sub("^--page-size=", "", arg), "--page-size")
    } else if (arg == "--max-pages") {
      opts$max_pages <- parse_int_arg(read_arg_value(args, i), "--max-pages")
      i <- i + 1L
    } else if (startsWith(arg, "--max-pages=")) {
      opts$max_pages <- parse_int_arg(sub("^--max-pages=", "", arg), "--max-pages")
    } else if (arg == "--output-dir") {
      opts$output_dir <- read_arg_value(args, i)
      i <- i + 1L
    } else if (startsWith(arg, "--output-dir=")) {
      opts$output_dir <- sub("^--output-dir=", "", arg)
    } else {
      stop("Unknown argument: ", arg, call. = FALSE)
    }

    i <- i + 1L
  }

  if (!is.null(opts$input_rds) && !is.null(opts$input_csv)) {
    stop("Use only one of --input-rds or --input-csv.", call. = FALSE)
  }

  opts$output_dir <- normalizePath(opts$output_dir, winslash = "/", mustWork = FALSE)
  opts
}

safe_slug <- function(x, default = "question") {
  x <- as.character(x %||% default)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x[is.na(x)] <- default
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "-", x)
  x <- gsub("(^-+|-+$)", "", x)
  x <- gsub("-{2,}", "-", x)

  if (!nzchar(x)) {
    default
  } else {
    x
  }
}

write_json_file <- function(x, path, pretty = TRUE, auto_unbox = TRUE) {
  jsonlite::write_json(
    x = x,
    path = path,
    pretty = pretty,
    auto_unbox = auto_unbox,
    null = "null"
  )
}

write_csv_file <- function(df, path) {
  utils::write.csv(df, file = path, row.names = FALSE, fileEncoding = "UTF-8", na = "")
}

initialize_openended_debug_fonts <- function() {
  regular_font_path <- file.path(repo_root, "fonts", "EuropaGroNr2JU Regular.otf")
  bold_font_path <- file.path(repo_root, "fonts", "EuropaGroNr2JU Bold.ttf")
  plot_family <- "sans"

  if (
    requireNamespace("showtext", quietly = TRUE) &&
    requireNamespace("sysfonts", quietly = TRUE) &&
    file.exists(regular_font_path) &&
    file.exists(bold_font_path)
  ) {
    existing_families <- tryCatch(
      sysfonts::font_families(),
      error = function(e) character()
    )

    font_added <- TRUE
    if (!"Europa" %in% existing_families) {
      font_added <- tryCatch(
        {
          sysfonts::font_add("Europa", regular = regular_font_path, bold = bold_font_path)
          TRUE
        },
        error = function(e) FALSE
      )
    }

    if (isTRUE(font_added)) {
      showtext_ok <- tryCatch(
        {
          showtext::showtext_auto()
          TRUE
        },
        error = function(e) FALSE
      )

      if (isTRUE(showtext_ok)) {
        plot_family <- "Europa"
      }
    }
  }

  options(openended_plot_family = plot_family)
  plot_family
}

build_openai_debug_request_body <- function(payload, schema) {
  list(
    model = get_openai_model(),
    temperature = 0,
    input = list(
      list(
        role = "system",
        content = list(
          list(
            type = "input_text",
            text = build_openended_prompt_text()
          )
        )
      ),
      list(
        role = "user",
        content = list(
          list(
            type = "input_text",
            text = jsonlite::toJSON(
              payload,
              auto_unbox = TRUE,
              pretty = TRUE,
              null = "null"
            )
          )
        )
      )
    ),
    text = list(
      format = list(
        type = "json_schema",
        name = "openended_topics",
        description = "Topic-level sentiment coding for Czech open-ended employee survey responses.",
        schema = schema,
        strict = TRUE
      )
    )
  )
}

load_final_long_df <- function(opts) {
  if (!is.null(opts$input_rds)) {
    data_obj <- readRDS(opts$input_rds)
    if (is.list(data_obj) && "final_long_df" %in% names(data_obj)) {
      return(data_obj$final_long_df)
    }
    return(data_obj)
  }

  if (!is.null(opts$input_csv)) {
    return(utils::read.csv(opts$input_csv, stringsAsFactors = FALSE, check.names = FALSE))
  }

  tf_token <- get_typeform_token(file.path(repo_root, "token.txt"))
  api_bundle <- build_final_long_df(
    tf_token = tf_token,
    form_id = opts$form_id,
    page_size = opts$page_size,
    max_pages = opts$max_pages
  )
  api_bundle$final_long_df
}

filter_openended_input <- function(openended_input_df, opts) {
  if (nrow(openended_input_df) == 0) {
    return(openended_input_df)
  }

  filtered_df <- openended_input_df

  if (!is.null(opts$question_ref) && nzchar(opts$question_ref)) {
    filtered_df <- filtered_df[filtered_df$question_ref == opts$question_ref, , drop = FALSE]
  }

  if (!is.null(opts$question_title_pattern) && nzchar(opts$question_title_pattern)) {
    keep <- grepl(opts$question_title_pattern, filtered_df$question_title, ignore.case = TRUE)
    filtered_df <- filtered_df[keep, , drop = FALSE]
  }

  filtered_df
}

build_question_index <- function(openended_input_df) {
  if (nrow(openended_input_df) == 0) {
    return(data.frame(
      question_ref = character(),
      question_title = character(),
      input_responses = integer(),
      stringsAsFactors = FALSE
    ))
  }

  split_df <- split(openended_input_df, openended_input_df$question_ref)
  rows <- lapply(split_df, function(question_df) {
    data.frame(
      question_ref = openended_first_non_empty(question_df$question_ref, default = ""),
      question_title = openended_first_non_empty(question_df$question_title, default = ""),
      input_responses = nrow(question_df),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

limit_question_df <- function(question_df, max_responses_per_question = NULL) {
  if (is.null(max_responses_per_question) || nrow(question_df) <= max_responses_per_question) {
    return(question_df)
  }

  question_df[seq_len(max_responses_per_question), , drop = FALSE]
}

save_plot_files <- function(plot_obj, question_dir) {
  if (is.null(plot_obj)) {
    return(list(png = NA_character_, pdf = NA_character_))
  }

  png_path <- file.path(question_dir, "balloon_plot.png")
  pdf_path <- file.path(question_dir, "balloon_plot.pdf")
  save_one_plot <- function(path, label, dpi = NULL) {
    tryCatch(
      {
        ggsave_args <- list(
          filename = path,
          plot = plot_obj,
          width = 14.6,
          height = 6.1,
          units = "in",
          bg = "white"
        )

        if (!is.null(dpi)) {
          ggsave_args$dpi <- dpi
        }

        do.call(ggplot2::ggsave, ggsave_args)
        path
      },
      error = function(e) {
        error_path <- file.path(question_dir, paste0("plot_save_error_", label, ".txt"))
        writeLines(conditionMessage(e), error_path, useBytes = TRUE)
        cat_line("[plot] Failed to save ", label, " plot: ", conditionMessage(e))
        NA_character_
      }
    )
  }

  list(
    png = save_one_plot(png_path, "png", dpi = 200),
    pdf = save_one_plot(pdf_path, "pdf")
  )
}

run_question_debug <- function(question_df, question_index, total_questions, output_dir, opts) {
  question_ref <- openended_first_non_empty(question_df$question_ref, default = "unknown-ref")
  question_title <- openended_first_non_empty(question_df$question_title, default = "Unknown question")
  limited_df <- question_df
  question_label <- sprintf("[%s/%s] %s", question_index, total_questions, question_ref)

  question_dir <- file.path(
    output_dir,
    sprintf(
      "%02d-%s-%s",
      question_index,
      safe_slug(question_ref, default = "question"),
      safe_slug(substr(question_title, 1L, 50L), default = "title")
    )
  )
  dir.create(question_dir, recursive = TRUE, showWarnings = FALSE)

  write_csv_file(question_df, file.path(question_dir, "question_input_all.csv"))

  if (!is.null(opts$max_responses_per_question)) {
    limited_df <- limit_question_df(question_df, opts$max_responses_per_question)
  }

  write_csv_file(limited_df, file.path(question_dir, "question_input_sent_to_llm.csv"))

  cat_line(question_label, " | responses sent: ", nrow(limited_df))

  payload <- build_openended_payload(limited_df)
  schema <- openended_llm_schema()
  request_body <- build_openai_debug_request_body(payload, schema)

  write_json_file(payload, file.path(question_dir, "payload.json"))
  write_json_file(schema, file.path(question_dir, "schema.json"))
  write_json_file(request_body, file.path(question_dir, "openai_request_body.json"))

  api_result <- tryCatch(
    call_openai_openended(payload, schema),
    error = function(e) {
      list(
        ok = FALSE,
        status = "script_error",
        error_message = conditionMessage(e),
        payload = payload,
        schema = schema
      )
    }
  )

  saveRDS(api_result, file.path(question_dir, "api_result.rds"))

  if (!is.null(api_result$response_body)) {
    writeLines(as.character(api_result$response_body), file.path(question_dir, "api_response_body.txt"), useBytes = TRUE)
  }

  if (!is.null(api_result$response_text)) {
    writeLines(as.character(api_result$response_text), file.path(question_dir, "response_text.json"), useBytes = TRUE)
  }

  if (!isTRUE(api_result$ok)) {
    return(data.frame(
      question_ref = question_ref,
      question_title = question_title,
      input_responses = nrow(question_df),
      responses_sent = nrow(limited_df),
      api_status = as.character(api_result$status %||% "unknown"),
      parsed_status = NA_character_,
      coded_rows = 0L,
      plot_png = NA_character_,
      plot_pdf = NA_character_,
      question_dir = question_dir,
      stringsAsFactors = FALSE
    ))
  }

  parsed_df <- parse_openended_llm_response(api_result$response_text)
  parsed_status <- openended_status(parsed_df)
  normalized_df <- normalize_openended_coded_df(parsed_df, limited_df)

  write_csv_file(normalized_df, file.path(question_dir, "coded_rows.csv"))

  if (nrow(normalized_df) == 0) {
    return(data.frame(
      question_ref = question_ref,
      question_title = question_title,
      input_responses = nrow(question_df),
      responses_sent = nrow(limited_df),
      api_status = as.character(api_result$status %||% "ok"),
      parsed_status = parsed_status,
      coded_rows = 0L,
      plot_png = NA_character_,
      plot_pdf = NA_character_,
      question_dir = question_dir,
      stringsAsFactors = FALSE
    ))
  }

  plot_obj <- plot_openended_balloon(normalized_df)
  plot_paths <- save_plot_files(plot_obj, question_dir)

  data.frame(
    question_ref = question_ref,
    question_title = question_title,
    input_responses = nrow(question_df),
    responses_sent = nrow(limited_df),
    api_status = as.character(api_result$status %||% "ok"),
    parsed_status = parsed_status,
    coded_rows = nrow(normalized_df),
    plot_png = plot_paths$png,
    plot_pdf = plot_paths$pdf,
    question_dir = question_dir,
    stringsAsFactors = FALSE
  )
}

run_openended_llm_debug <- function(form_id = "r5VHgEYK",
                                    input_rds = NULL,
                                    input_csv = NULL,
                                    question_ref = NULL,
                                    question_title_pattern = NULL,
                                    max_questions = NULL,
                                    max_responses_per_question = NULL,
                                    page_size = 1000L,
                                    max_pages = 200L,
                                    output_dir = file.path(
                                      repo_root,
                                      "output",
                                      paste0("openended-llm-debug-", format(Sys.time(), "%Y%m%d-%H%M%S"))
                                    )) {
  opts <- list(
    form_id = form_id,
    input_rds = input_rds,
    input_csv = input_csv,
    question_ref = question_ref,
    question_title_pattern = question_title_pattern,
    max_questions = max_questions,
    max_responses_per_question = max_responses_per_question,
    page_size = page_size,
    max_pages = max_pages,
    output_dir = normalizePath(output_dir, winslash = "/", mustWork = FALSE),
    help = FALSE
  )

  if (!is.null(opts$input_rds) && !is.null(opts$input_csv)) {
    stop("Use only one of input_rds or input_csv.", call. = FALSE)
  }

  plot_family <- initialize_openended_debug_fonts()
  dir.create(opts$output_dir, recursive = TRUE, showWarnings = FALSE)

  metadata <- list(
    date = as.character(Sys.time()),
    repo_root = repo_root,
    form_id = opts$form_id,
    input_rds = opts$input_rds,
    input_csv = opts$input_csv,
    question_ref = opts$question_ref,
    question_title_pattern = opts$question_title_pattern,
    max_questions = opts$max_questions,
    max_responses_per_question = opts$max_responses_per_question,
    page_size = opts$page_size,
    max_pages = opts$max_pages,
    output_dir = opts$output_dir,
    plot_family = plot_family,
    openai_key_present = has_openai_key(),
    openai_model = get_openai_model()
  )
  write_json_file(metadata, file.path(opts$output_dir, "run_metadata.json"))
  writeLines(capture.output(sessionInfo()), file.path(opts$output_dir, "session_info.txt"))

  cat_line("Repo root: ", repo_root)
  cat_line("Output dir: ", opts$output_dir)
  cat_line("Plot font family: ", plot_family)
  cat_line("OpenAI key present: ", if (has_openai_key()) "yes" else "no")
  cat_line("OpenAI model: ", get_openai_model())

  final_long_df <- tryCatch(
    load_final_long_df(opts),
    error = function(e) {
      stop("Failed to load source data: ", conditionMessage(e), call. = FALSE)
    }
  )

  saveRDS(final_long_df, file.path(opts$output_dir, "final_long_df.rds"))
  openended_input_df <- build_openended_input_df(final_long_df)
  write_csv_file(openended_input_df, file.path(opts$output_dir, "openended_input_all.csv"))

  filtered_input_df <- filter_openended_input(openended_input_df, opts)
  write_csv_file(filtered_input_df, file.path(opts$output_dir, "openended_input_filtered.csv"))

  question_index_df <- build_question_index(filtered_input_df)
  write_csv_file(question_index_df, file.path(opts$output_dir, "question_index.csv"))

  if (!is.null(opts$max_questions) && nrow(question_index_df) > opts$max_questions) {
    keep_refs <- head(question_index_df$question_ref, opts$max_questions)
    filtered_input_df <- filtered_input_df[filtered_input_df$question_ref %in% keep_refs, , drop = FALSE]
    question_index_df <- build_question_index(filtered_input_df)
  }

  if (nrow(filtered_input_df) == 0) {
    cat_line("No open-ended responses matched the requested filters.")
    return(invisible(list(
      exit_status = 1L,
      output_dir = opts$output_dir,
      summary = data.frame(),
      coded_all = openended_empty_coded_df(),
      question_index = question_index_df,
      filtered_input = filtered_input_df
    )))
  }

  cat_line("Open-ended questions to process: ", nrow(question_index_df))
  cat_line("Total open-ended responses after filters: ", nrow(filtered_input_df))

  question_refs <- unique(as.character(filtered_input_df$question_ref))
  question_results <- vector("list", length(question_refs))
  coded_results <- vector("list", length(question_refs))

  for (i in seq_along(question_refs)) {
    question_ref_i <- question_refs[[i]]
    question_df <- filtered_input_df[filtered_input_df$question_ref == question_ref_i, , drop = FALSE]
    result_row <- run_question_debug(question_df, i, length(question_refs), opts$output_dir, opts)
    question_results[[i]] <- result_row

    coded_path <- file.path(result_row$question_dir[[1]], "coded_rows.csv")
    if (file.exists(coded_path)) {
      coded_df <- utils::read.csv(coded_path, stringsAsFactors = FALSE, check.names = FALSE)
      if (nrow(coded_df) > 0) {
        coded_results[[i]] <- coded_df
      }
    }
  }

  summary_df <- do.call(rbind, question_results)
  rownames(summary_df) <- NULL
  write_csv_file(summary_df, file.path(opts$output_dir, "summary.csv"))

  non_null_coded_results <- coded_results[!vapply(coded_results, is.null, logical(1))]

  if (length(non_null_coded_results) > 0 && any(vapply(non_null_coded_results, nrow, integer(1)) > 0L)) {
    coded_results <- non_null_coded_results[vapply(non_null_coded_results, nrow, integer(1)) > 0L]
    coded_all_df <- do.call(rbind, coded_results)
  } else {
    coded_all_df <- openended_empty_coded_df()
  }

  write_csv_file(coded_all_df, file.path(opts$output_dir, "coded_all.csv"))

  cat_line("")
  cat_line("Summary:")
  for (i in seq_len(nrow(summary_df))) {
    cat_line(
      "  - ",
      summary_df$question_ref[[i]],
      " | api_status=",
      summary_df$api_status[[i]],
      " | parsed_status=",
      summary_df$parsed_status[[i]] %||% "NA",
      " | coded_rows=",
      summary_df$coded_rows[[i]]
    )
  }

  success_count <- sum(summary_df$coded_rows > 0L, na.rm = TRUE)
  cat_line("")
  cat_line("Questions with coded output: ", success_count, "/", nrow(summary_df))
  cat_line("Artifacts written to: ", opts$output_dir)

  invisible(list(
    exit_status = if (success_count > 0L) 0L else 1L,
    output_dir = opts$output_dir,
    summary = summary_df,
    coded_all = coded_all_df,
    question_index = question_index_df,
    filtered_input = filtered_input_df,
    final_long_df = final_long_df
  ))
}

run_openended_llm_debug_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  opts <- tryCatch(
    parse_cli_args(args),
    error = function(e) {
      print_usage(conditionMessage(e))
      return(NULL)
    }
  )

  if (is.null(opts)) {
    return(invisible(list(exit_status = 1L)))
  }

  if (isTRUE(opts$help)) {
    print_usage()
    return(invisible(list(exit_status = 0L)))
  }

  run_openended_llm_debug(
    form_id = opts$form_id,
    input_rds = opts$input_rds,
    input_csv = opts$input_csv,
    question_ref = opts$question_ref,
    question_title_pattern = opts$question_title_pattern,
    max_questions = opts$max_questions,
    max_responses_per_question = opts$max_responses_per_question,
    page_size = opts$page_size,
    max_pages = opts$max_pages,
    output_dir = opts$output_dir
  )
}

if (sys.nframe() == 0L) {
  cli_result <- tryCatch(
    run_openended_llm_debug_cli(),
    error = function(e) {
      cat_line("Error: ", conditionMessage(e))
      invisible(list(exit_status = 1L))
    }
  )

  quit(save = "no", status = cli_result$exit_status %||% 1L)
}
