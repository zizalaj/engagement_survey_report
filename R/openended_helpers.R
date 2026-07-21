openended_empty_input_df <- function() {
  data.frame(
    respondent_id = character(),
    question_ref = character(),
    question_title = character(),
    response_text = character(),
    stringsAsFactors = FALSE
  )
}

openended_empty_coded_df <- function() {
  data.frame(
    respondent_id = character(),
    question_ref = character(),
    question_title = character(),
    topic = character(),
    sentiment = character(),
    sentiment_score = integer(),
    evidence = character(),
    stringsAsFactors = FALSE
  )
}

openended_first_non_empty <- function(x, default = "") {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]

  if (length(x) == 0) {
    return(default)
  }

  x[[1]]
}

openended_prompt_version <- function() {
  "v1"
}

openended_schema_version <- function() {
  "v1"
}

openended_max_topics_per_response <- function() {
  2L
}

openended_sentiment_levels <- function() {
  c("positive", "neutral", "mixed", "negative", "unclear")
}

openended_sentiment_score_map <- function() {
  c(
    positive = 1L,
    neutral = 0L,
    mixed = 0L,
    negative = -1L,
    unclear = 0L
  )
}

openended_status <- function(df, status = NULL) {
  if (!is.null(status)) {
    attr(df, "openended_status") <- status
    return(df)
  }

  attr(df, "openended_status", exact = TRUE) %||% "unknown"
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

openended_message <- function(...) {
  message("[openended] ", paste0(..., collapse = ""))
}

openended_plot_family <- function() {
  family <- getOption("openended_plot_family", default = "Europa")
  family <- trimws(as.character(family %||% "Europa"))

  if (!nzchar(family)) {
    return("Europa")
  }

  family
}

openended_question_id <- function(question_df) {
  paste(
    openended_first_non_empty(question_df$question_ref, default = "unknown_ref"),
    openended_first_non_empty(question_df$question_title, default = "unknown_title"),
    sep = " :: "
  )
}

has_openai_key <- function() {
  key <- Sys.getenv("OPENAI_API_KEY", unset = "")
  nzchar(trimws(key))
}

get_openai_model <- function() {
  model <- Sys.getenv("OPENAI_MODEL", unset = "gpt-4.1-mini")
  model <- trimws(model)

  if (!nzchar(model)) {
    return("gpt-4.1-mini")
  }

  model
}

clean_openended_text_for_llm <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("[[:space:]]+", " ", x)
  x <- trimws(x)
  x <- gsub("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}", "[EMAIL]", x, perl = TRUE)
  x <- gsub("https?://\\S+|www\\.\\S+", "[URL]", x, perl = TRUE)
  x <- gsub("\\+?\\d[\\d\\s().-]{7,}\\d", "[PHONE]", x, perl = TRUE)
  x
}

build_openended_input_df <- function(final_long_df) {
  required_cols <- c(
    "respondent_id",
    "otazka_ref",
    "otazka_title",
    "odpoved_hodnota",
    "typ_otazky"
  )

  if (is.null(final_long_df) || !all(required_cols %in% names(final_long_df))) {
    return(openended_empty_input_df())
  }

  df <- final_long_df[final_long_df$typ_otazky == "long_text", required_cols, drop = FALSE]

  if (nrow(df) == 0) {
    return(openended_empty_input_df())
  }

  cleaned_text <- clean_openended_text_for_llm(df$odpoved_hodnota)
  keep <- !is.na(df$odpoved_hodnota) & nzchar(cleaned_text)

  if (!any(keep)) {
    return(openended_empty_input_df())
  }

  df <- df[keep, , drop = FALSE]
  cleaned_text <- cleaned_text[keep]

  respondent_values <- as.character(df$respondent_id)
  unique_ids <- unique(respondent_values)
  anon_ids <- sprintf("R%03d", seq_along(unique_ids))
  anon_map <- stats::setNames(anon_ids, unique_ids)

  data.frame(
    respondent_id = unname(anon_map[respondent_values]),
    question_ref = as.character(df$otazka_ref),
    question_title = as.character(df$otazka_title),
    response_text = cleaned_text,
    stringsAsFactors = FALSE
  )
}

openended_llm_schema <- function() {
  list(
    type = "object",
    additionalProperties = FALSE,
    required = list("items"),
    properties = list(
      items = list(
        type = "array",
        items = list(
          type = "object",
          additionalProperties = FALSE,
          required = c(
            "respondent_id",
            "question_ref",
            "topic",
            "sentiment",
            "sentiment_score",
            "evidence"
          ),
          properties = list(
            respondent_id = list(type = "string"),
            question_ref = list(type = "string"),
            topic = list(type = "string"),
            sentiment = list(
              type = "string",
              enum = as.list(openended_sentiment_levels())
            ),
            sentiment_score = list(
              type = "integer",
              enum = as.list(c(-1L, 0L, 1L))
            ),
            evidence = list(type = "string")
          )
        )
      )
    )
  )
}

build_openended_payload <- function(question_df) {
  max_topics <- openended_max_topics_per_response()

  if (is.null(question_df) || nrow(question_df) == 0) {
    return(list(
      question_ref = "",
      question_title = "",
      language = "cs",
      instructions = list(
        max_topics_per_response = max_topics,
        topic_label_language = "cs",
        sentiment_values = openended_sentiment_levels()
      ),
      responses = list()
    ))
  }

  responses <- lapply(seq_len(nrow(question_df)), function(i) {
    list(
      respondent_id = as.character(question_df$respondent_id[[i]]),
      text = as.character(question_df$response_text[[i]])
    )
  })

  list(
    question_ref = openended_first_non_empty(question_df$question_ref),
    question_title = openended_first_non_empty(question_df$question_title),
    language = "cs",
    instructions = list(
      max_topics_per_response = max_topics,
      topic_label_language = "cs",
      sentiment_values = openended_sentiment_levels()
    ),
    responses = responses
  )
}

build_openended_prompt_text <- function() {
  paste(
    "You are coding Czech open-ended employee survey responses.",
    "Return structured JSON only.",
    "For each response, identify up to two topics actually present in the text.",
    "Use short Czech topic labels.",
    "Reuse the same topic label for the same concept within the same question.",
    "Assign sentiment toward that specific topic only.",
    "Allowed sentiment values are positive, neutral, mixed, negative, unclear.",
    "If sentiment cannot be inferred, use unclear.",
    "Keep evidence short and grounded in the response text.",
    "Do not include raw respondent IDs outside the provided anonymized respondent_id values.",
    sep = " "
  )
}

call_openai_openended <- function(payload, schema) {
  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    return(list(
      ok = FALSE,
      status = "missing_package",
      payload = payload,
      schema = schema,
      response_text = NULL,
      parsed_response = NULL
    ))
  }

  api_key <- Sys.getenv("OPENAI_API_KEY", unset = "")
  api_key <- trimws(api_key)

  if (!nzchar(api_key)) {
    return(list(
      ok = FALSE,
      status = "missing_key",
      payload = payload,
      schema = schema,
      response_text = NULL,
      parsed_response = NULL
    ))
  }

  request_body <- list(
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

  request_json <- jsonlite::toJSON(
    request_body,
    auto_unbox = TRUE,
    pretty = FALSE,
    null = "null"
  )

  response <- tryCatch(
    httr::POST(
      url = "https://api.openai.com/v1/responses",
      httr::add_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ),
      body = request_json,
      encode = "raw",
      httr::timeout(120)
    ),
    error = function(e) e
  )

  if (inherits(response, "error")) {
    openended_message("OpenAI API request failed: ", conditionMessage(response))
    return(list(
      ok = FALSE,
      status = "request_error",
      payload = payload,
      schema = schema,
      response_text = NULL,
      parsed_response = NULL
    ))
  }

  response_text_raw <- tryCatch(
    httr::content(response, as = "text", encoding = "UTF-8"),
    error = function(e) ""
  )

  if (httr::http_error(response)) {
    openended_message(
      "OpenAI API returned HTTP ",
      response$status_code,
      " for question ",
      payload$question_ref %||% "unknown"
    )
    return(list(
      ok = FALSE,
      status = "http_error",
      payload = payload,
      schema = schema,
      response_text = NULL,
      parsed_response = NULL,
      response_body = response_text_raw
    ))
  }

  parsed_response <- tryCatch(
    jsonlite::fromJSON(response_text_raw, simplifyVector = FALSE),
    error = function(e) e
  )

  if (inherits(parsed_response, "error")) {
    openended_message("Failed to parse OpenAI API response JSON: ", conditionMessage(parsed_response))
    return(list(
      ok = FALSE,
      status = "response_parse_error",
      payload = payload,
      schema = schema,
      response_text = NULL,
      parsed_response = NULL
    ))
  }

  structured_text <- extract_openai_structured_text(parsed_response)

  if (is.null(structured_text) || !nzchar(trimws(structured_text))) {
    openended_message(
      "OpenAI response did not contain structured text for question ",
      payload$question_ref %||% "unknown"
    )
    return(list(
      ok = FALSE,
      status = "empty_output",
      payload = payload,
      schema = schema,
      response_text = NULL,
      parsed_response = parsed_response
    ))
  }

  list(
    ok = TRUE,
    status = "ok",
    payload = payload,
    schema = schema,
    response_text = structured_text,
    parsed_response = parsed_response
  )
}

extract_openai_structured_text <- function(parsed_response) {
  if (is.null(parsed_response)) {
    return(NULL)
  }

  output_text <- parsed_response$output_text
  if (!is.null(output_text) && nzchar(trimws(as.character(output_text)))) {
    return(as.character(output_text))
  }

  output <- parsed_response$output
  if (!is.list(output) || length(output) == 0) {
    return(NULL)
  }

  for (item in output) {
    content <- item$content
    if (!is.list(content) || length(content) == 0) {
      next
    }

    for (part in content) {
      text_value <- part$text
      if (is.list(text_value) && !is.null(text_value$value)) {
        text_value <- text_value$value
      }

      if (!is.null(text_value) && nzchar(trimws(as.character(text_value)))) {
        return(as.character(text_value))
      }
    }
  }

  NULL
}

parse_openended_llm_response <- function(response_text) {
  if (is.null(response_text) || !nzchar(trimws(as.character(response_text)))) {
    return(openended_status(openended_empty_coded_df(), "failed"))
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    return(openended_status(openended_empty_coded_df(), "failed"))
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(response_text, simplifyVector = FALSE),
    error = function(e) e
  )

  if (inherits(parsed, "error")) {
    openended_message("Failed to parse structured model JSON: ", conditionMessage(parsed))
    return(openended_status(openended_empty_coded_df(), "failed"))
  }

  items <- parsed$items
  if (is.null(items)) {
    return(openended_status(openended_empty_coded_df(), "failed"))
  }

  if (!is.list(items) || length(items) == 0) {
    return(openended_status(openended_empty_coded_df(), "empty_output"))
  }

  sentiment_map <- openended_sentiment_score_map()

  rows <- lapply(items, function(item) {
    if (!is.list(item)) {
      return(NULL)
    }

    sentiment <- tolower(trimws(as.character(item$sentiment %||% "unclear")))
    if (!sentiment %in% names(sentiment_map)) {
      sentiment <- "unclear"
    }

    topic <- trimws(as.character(item$topic %||% ""))
    evidence <- trimws(as.character(item$evidence %||% ""))
    respondent_id <- trimws(as.character(item$respondent_id %||% ""))
    question_ref <- trimws(as.character(item$question_ref %||% ""))

    if (!nzchar(topic) || !nzchar(respondent_id)) {
      return(NULL)
    }

    data.frame(
      respondent_id = respondent_id,
      question_ref = question_ref,
      question_title = "",
      topic = topic,
      sentiment = sentiment,
      sentiment_score = unname(sentiment_map[[sentiment]]),
      evidence = evidence,
      stringsAsFactors = FALSE
    )
  })

  rows <- rows[!vapply(rows, is.null, logical(1))]

  if (length(rows) == 0) {
    return(openended_status(openended_empty_coded_df(), "empty_output"))
  }

  df <- do.call(rbind, rows)
  openended_status(
    unique(df),
    "ok"
  )
}

openended_cache_dir <- function() {
  cache_root <- Sys.getenv("OPENENDED_CACHE_ROOT", unset = "")
  cache_root <- trimws(cache_root)

  if (!nzchar(cache_root)) {
    cache_root <- getwd()
  }

  cache_dir <- file.path(cache_root, "cache", "openended_llm")

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  cache_dir
}

make_openended_cache_key <- function(question_df,
                                     model = get_openai_model(),
                                     prompt_version = openended_prompt_version(),
                                     schema_version = openended_schema_version(),
                                     max_topics_per_response = openended_max_topics_per_response()) {
  if (is.null(question_df) || nrow(question_df) == 0) {
    return(digest::digest(
      list(
        model = model,
        prompt_version = prompt_version,
        schema_version = schema_version,
        max_topics_per_response = max_topics_per_response,
        question = "empty"
      ),
      algo = "sha256",
      serialize = TRUE
    ))
  }

  question_df <- question_df[order(question_df$respondent_id, question_df$response_text), , drop = FALSE]

  digest::digest(
    list(
      model = model,
      prompt_version = prompt_version,
      schema_version = schema_version,
      max_topics_per_response = max_topics_per_response,
      question_ref = as.character(question_df$question_ref),
      question_title = as.character(question_df$question_title),
      anonymized_responses = as.character(question_df$response_text),
      respondent_id = as.character(question_df$respondent_id)
    ),
    algo = "sha256",
    serialize = TRUE
  )
}

openended_cache_path <- function(cache_key) {
  file.path(openended_cache_dir(), paste0(cache_key, ".rds"))
}

read_openended_cache <- function(cache_key) {
  cache_path <- openended_cache_path(cache_key)

  if (!file.exists(cache_path)) {
    return(NULL)
  }

  cached <- tryCatch(
    readRDS(cache_path),
    error = function(e) e
  )

  if (inherits(cached, "error")) {
    openended_message("Failed to read open-ended cache file: ", basename(cache_path))
    return(NULL)
  }

  cached
}

write_openended_cache <- function(cache_key, coded_df) {
  cache_path <- openended_cache_path(cache_key)

  write_ok <- tryCatch(
    {
      saveRDS(coded_df, cache_path)
      TRUE
    },
    error = function(e) {
      openended_message("Failed to write open-ended cache file: ", basename(cache_path))
      FALSE
    }
  )

  invisible(write_ok)
}

normalize_openended_coded_df <- function(coded_df, question_df) {
  if (is.null(coded_df) || nrow(coded_df) == 0) {
    return(openended_empty_coded_df())
  }

  sentiment_map <- openended_sentiment_score_map()
  allowed_respondents <- unique(as.character(question_df$respondent_id))
  default_question_ref <- openended_first_non_empty(question_df$question_ref)
  default_question_title <- openended_first_non_empty(question_df$question_title)
  max_topics <- openended_max_topics_per_response()

  coded_df <- coded_df[coded_df$respondent_id %in% allowed_respondents, , drop = FALSE]

  if (nrow(coded_df) == 0) {
    return(openended_empty_coded_df())
  }

  coded_df$question_ref <- ifelse(
    nzchar(trimws(as.character(coded_df$question_ref))),
    as.character(coded_df$question_ref),
    default_question_ref
  )
  coded_df$question_title <- default_question_title
  coded_df$topic <- trimws(as.character(coded_df$topic))
  coded_df$evidence <- trimws(as.character(coded_df$evidence))
  coded_df$sentiment <- tolower(trimws(as.character(coded_df$sentiment)))
  coded_df$sentiment[!coded_df$sentiment %in% names(sentiment_map)] <- "unclear"
  coded_df$sentiment_score <- unname(sentiment_map[coded_df$sentiment])

  coded_df <- coded_df[nzchar(coded_df$topic), , drop = FALSE]

  if (nrow(coded_df) == 0) {
    return(openended_empty_coded_df())
  }

  rownames(coded_df) <- NULL

  split_rows <- split(coded_df, coded_df$respondent_id)
  limited_rows <- lapply(split_rows, function(df) {
    head(df, max_topics)
  })
  coded_df <- do.call(rbind, limited_rows)

  if (is.null(coded_df) || nrow(coded_df) == 0) {
    return(openended_empty_coded_df())
  }

  rownames(coded_df) <- NULL
  unique(coded_df)
}

code_openended_question <- function(question_df) {
  if (is.null(question_df) || nrow(question_df) == 0) {
    return(openended_status(openended_empty_coded_df(), "empty"))
  }

  cache_key <- make_openended_cache_key(question_df)
  cached_df <- read_openended_cache(cache_key)

  if (is.data.frame(cached_df)) {
    cached_df <- normalize_openended_coded_df(cached_df, question_df)

    if (nrow(cached_df) > 0) {
      return(openended_status(cached_df, "ok"))
    }
  }

  if (!has_openai_key()) {
    return(openended_status(openended_empty_coded_df(), "missing_key"))
  }

  payload <- build_openended_payload(question_df)
  schema <- openended_llm_schema()
  api_result <- tryCatch(
    call_openai_openended(payload, schema),
    error = function(e) e
  )

  if (inherits(api_result, "error")) {
    openended_message(
      "Unexpected open-ended coding failure for question ",
      openended_question_id(question_df),
      ": ",
      conditionMessage(api_result)
    )
    return(openended_status(openended_empty_coded_df(), "failed"))
  }

  if (!isTRUE(api_result$ok)) {
    return(openended_status(openended_empty_coded_df(), "failed"))
  }

  coded_df <- parse_openended_llm_response(api_result$response_text)
  coded_status <- openended_status(coded_df)

  if (!identical(coded_status, "ok")) {
    return(openended_status(openended_empty_coded_df(), "failed"))
  }

  coded_df <- normalize_openended_coded_df(coded_df, question_df)
  if (nrow(coded_df) == 0) {
    return(openended_status(openended_empty_coded_df(), "failed"))
  }

  write_openended_cache(cache_key, coded_df)
  openended_status(coded_df, "ok")
}

code_openended_questions <- function(openended_input_df) {
  if (is.null(openended_input_df) || nrow(openended_input_df) == 0) {
    return(openended_status(openended_empty_coded_df(), "empty"))
  }

  question_ids <- unique(as.character(openended_input_df$question_ref))
  results <- lapply(question_ids, function(question_id) {
    question_df <- openended_input_df[openended_input_df$question_ref == question_id, , drop = FALSE]
    code_openended_question(question_df)
  })

  statuses <- vapply(results, openended_status, character(1))
  non_empty_results <- results[vapply(results, nrow, integer(1)) > 0]

  combined <- if (length(non_empty_results) == 0) {
    openended_empty_coded_df()
  } else {
    rownames_df <- do.call(rbind, non_empty_results)
    rownames_df <- unique(rownames_df)
    rownames(rownames_df) <- NULL
    rownames_df
  }

  attr(combined, "openended_question_statuses") <- statuses
  openended_status(combined, if (any(statuses == "ok")) "ok" else statuses[[1]])
}

prepare_openended_analysis <- function(final_long_df) {
  input_df <- build_openended_input_df(final_long_df)
  if (nrow(input_df) == 0) {
    return(list(
      input = input_df,
      coded = openended_empty_coded_df(),
      plot_present = FALSE,
      status = "empty"
    ))
  }

  if (!has_openai_key()) {
    openended_message("OPENAI_API_KEY is missing; skipping automated open-ended coding.")
    return(list(
      input = input_df,
      coded = openended_empty_coded_df(),
      plot_present = FALSE,
      status = "missing_key"
    ))
  }

  coded_df <- tryCatch(
    code_openended_questions(input_df),
    error = function(e) {
      openended_message("Open-ended orchestration failed: ", conditionMessage(e))
      openended_status(openended_empty_coded_df(), "failed")
    }
  )

  question_statuses <- attr(coded_df, "openended_question_statuses", exact = TRUE)
  status <- if (length(question_statuses) == 0) {
    openended_status(coded_df)
  } else if (any(question_statuses == "ok")) {
    "ok"
  } else if (all(question_statuses == "missing_key")) {
    "missing_key"
  } else {
    "failed"
  }

  list(
    input = input_df,
    coded = coded_df,
    plot_present = identical(status, "ok") && nrow(coded_df) > 0,
    status = status
  )
}

openended_sentiment_colors <- function() {
  c(
    positive = "#63E8C6",
    neutral = "#c5c5c5",
    mixed = "#A5A5A5",
    negative = "#ff67aa",
    unclear = "#E6E6E6"
  )
}

openended_sentiment_labels <- function() {
  enc2utf8(c(
    positive = "Pozitivní",
    neutral = "Neutrální",
    mixed = "Smíšený",
    negative = "Negativní",
    unclear = "Nejasný"
  ))
}

openended_escape_html <- function(text) {
  text <- as.character(text %||% "")

  if (requireNamespace("htmltools", quietly = TRUE)) {
    return(as.character(htmltools::htmlEscape(text)))
  }

  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub('"', "&quot;", text, fixed = TRUE)
  gsub("'", "&#39;", text, fixed = TRUE)
}

aggregate_openended_balloon_df <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame(
      question_ref = character(),
      question_title = character(),
      topic = factor(character()),
      sentiment = factor(character(), levels = openended_sentiment_levels()),
      sentiment_label = factor(character()),
      n_respondents = integer(),
      total_n = integer(),
      stringsAsFactors = FALSE
    ))
  }

  key_df <- unique(df[, c("respondent_id", "question_ref", "question_title", "topic", "sentiment"), drop = FALSE])
  key_df$topic <- trimws(as.character(key_df$topic))
  key_df$sentiment <- tolower(trimws(as.character(key_df$sentiment)))
  key_df <- key_df[nzchar(key_df$topic), , drop = FALSE]
  key_df <- key_df[key_df$sentiment %in% openended_sentiment_levels(), , drop = FALSE]

  if (nrow(key_df) == 0) {
    return(data.frame(
      question_ref = character(),
      question_title = character(),
      topic = factor(character()),
      sentiment = factor(character(), levels = openended_sentiment_levels()),
      sentiment_label = factor(character()),
      n_respondents = integer(),
      total_n = integer(),
      stringsAsFactors = FALSE
    ))
  }

  counts <- stats::aggregate(
    list(n_respondents = rep(1L, nrow(key_df))),
    by = list(
      question_ref = key_df$question_ref,
      question_title = key_df$question_title,
      topic = key_df$topic,
      sentiment = key_df$sentiment
    ),
    FUN = sum
  )

  topic_totals <- stats::aggregate(
    list(total_n = counts$n_respondents),
    by = list(topic = counts$topic),
    FUN = sum
  )

  total_map <- stats::setNames(topic_totals$total_n, topic_totals$topic)
  topic_order <- topic_totals$topic[order(-topic_totals$total_n, topic_totals$topic)]
  topic_levels <- rev(topic_order)

  sentiment_levels <- openended_sentiment_levels()
  sentiment_labels <- openended_sentiment_labels()

  counts$total_n <- unname(total_map[counts$topic])
  counts$topic <- factor(counts$topic, levels = topic_levels)
  counts$sentiment <- factor(counts$sentiment, levels = sentiment_levels)
  counts$sentiment_label <- factor(
    unname(sentiment_labels[as.character(counts$sentiment)]),
    levels = unname(sentiment_labels[sentiment_levels])
  )

  counts[order(counts$topic, counts$sentiment), , drop = FALSE]
}

plot_openended_balloon <- function(df) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }

  plot_df <- aggregate_openended_balloon_df(df)
  sentiment_levels <- openended_sentiment_levels()
  sentiment_labels <- openended_sentiment_labels()
  sentiment_colors <- openended_sentiment_colors()
  plot_family <- openended_plot_family()

  if (nrow(plot_df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::annotate(
          "text",
          x = 1,
          y = 1,
          label = enc2utf8("Bez dostatečných dat pro graf"),
          family = plot_family,
          fontface = "bold",
          size = 10
        ) +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = "white", colour = NA),
          panel.background = ggplot2::element_rect(fill = "white", colour = NA)
        )
    )
  }

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = sentiment_label,
      y = topic,
      size = n_respondents,
      fill = sentiment,
      color = sentiment
    )
  ) +
    ggplot2::geom_point(shape = 21, stroke = 1.6, alpha = 0.95) +
    ggplot2::geom_text(
      ggplot2::aes(label = n_respondents),
      family = plot_family,
      fontface = "bold",
      size = 6,
      color = "#111111"
    ) +
    ggplot2::scale_x_discrete(
      drop = FALSE,
      limits = unname(sentiment_labels[sentiment_levels]),
      labels = unname(sentiment_labels[sentiment_levels])
    ) +
    ggplot2::scale_fill_manual(values = sentiment_colors, drop = FALSE) +
    ggplot2::scale_color_manual(values = sentiment_colors, drop = FALSE) +
    ggplot2::scale_size_continuous(range = c(14, 34)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_family = plot_family) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "#E8E8E8", linewidth = 0.8),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      legend.position = "none",
      axis.text.x = ggplot2::element_text(
        family = plot_family,
        face = "bold",
        size = 22,
        colour = "#202020"
      ),
      axis.text.y = ggplot2::element_text(
        family = plot_family,
        face = "bold",
        size = 22,
        colour = "#202020"
      ),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 12, r = 20, b = 12, l = 6)
    ) +
    ggplot2::coord_cartesian(clip = "off")
}

split_openended_pages <- function(openended_coded) {
  if (is.null(openended_coded) || nrow(openended_coded) == 0) {
    return(list())
  }

  question_order <- unique(as.character(openended_coded$question_ref))
  lapply(question_order, function(question_ref) {
    page_df <- openended_coded[openended_coded$question_ref == question_ref, , drop = FALSE]
    rownames(page_df) <- NULL
    page_df
  })
}

get_openended_title <- function(df_page) {
  if (is.null(df_page) || nrow(df_page) == 0 || !"question_title" %in% names(df_page)) {
    return("Otevrene otazky")
  }

  openended_escape_html(
    openended_first_non_empty(df_page$question_title, default = "Otevrene otazky")
  )
}

openended_page_ref <- function(df_page) {
  if (is.null(df_page) || nrow(df_page) == 0 || !"question_ref" %in% names(df_page)) {
    return("")
  }

  openended_first_non_empty(df_page$question_ref, default = "")
}

openended_note_html <- function() {
  paste(
    "Pozn.: Jedna odpov&#283;&#271; m&#367;&#382;e b&#253;t za&#345;azena do v&#237;ce t&#233;mat.",
    "Velikost bodu odpov&#237;d&#225; po&#269;tu respondent&#367;."
  )
}

openended_fallback_copy_html <- function() {
  paste(
    "Otev&#345;en&#233; odpov&#283;di jsou v datech k dispozici,",
    "ale nepoda&#345;ilo se je automaticky zpracovat do grafu.",
    "Zbytek reportu byl vyrenderov&#225;n bez p&#345;eru&#353;en&#237;."
  )
}

openended_placeholder_page <- function() {
  data.frame(
    question_ref = "",
    question_title = "Otevrene odpovedi",
    stringsAsFactors = FALSE
  )
}

render_openended_result_slides <- function(openended_pages) {
  if (length(openended_pages) == 0) {
    return(invisible(NULL))
  }

  slide_template <- paste(
    '<div class="slide multiple-results-v2 openended-results-v2">',
    '<div class="ei-kicker">Výsledky</div>',
    '<div class="mrv-title">{{question_title}}</div>',
    '<div class="mrv-chart">',
    '```{r}',
    '#| echo: false',
    '#| warning: false',
    '#| message: false',
    '#| fig-width: 14.6',
    '#| fig-height: 6.1',
    '',
    'df_page <- openended_pages[[{{i}}]]',
    'plot_openended_balloon(df_page)',
    '```',
    '</div>',
    '<div style="margin-top: 22px; max-width: 1500px; font-family: &quot;Europa Regular&quot;, &quot;Europa&quot;, sans-serif; font-size: 22px; line-height: 1.32;">',
    'Pozn.: Jedna odpověď může být zařazena do více témat. Velikost bodu odpovídá počtu respondentů.',
    '</div>',
    '</div>',
    sep = "\n"
  )

  rendered_slides <- lapply(seq_along(openended_pages), function(i) {
    expanded_slide <- knitr::knit_expand(
      text = slide_template,
      i = i,
      question_title = get_openended_title(openended_pages[[i]])
    )

    knitr::knit_child(text = expanded_slide, quiet = TRUE, envir = environment())
  })

  cat(unlist(rendered_slides), sep = "\n")
}

resolve_openended_section_name <- function(agenda_cfg) {
  if (!is.list(agenda_cfg) || is.null(agenda_cfg$sections)) {
    return("Otevrene otazky")
  }

  matches <- agenda_cfg$sections[grepl("^Otev", agenda_cfg$sections)]

  if (length(matches) == 0) {
    return("Otevrene otazky")
  }

  matches[[1]]
}

render_openended_fallback_slide <- function() {
  slide_html <- paste(
    '<div class="slide section-slide section-slide--pvc">',
    '<div class="section-left-bleed" aria-hidden="true"></div>',
    '<div class="section-right-bleed" aria-hidden="true"></div>',
    '<div class="section-bottom-bleed" aria-hidden="true"></div>',
    '<div class="agenda-content">',
    '<div class="agenda-title">Otev&#345;en&#233; odpov&#283;di</div>',
    '<div class="section-description">',
    'Otev&#345;en&#233; odpov&#283;di jsou v datech k dispozici, ale nepoda&#345;ilo se je automaticky zpracovat do grafu. Zbytek reportu byl vyrenderov&#225;n bez p&#345;eru&#353;en&#237;.',
    '</div>',
    '</div>',
    '</div>',
    sep = "\n"
  )

  cat(slide_html, sep = "\n")
}

render_openended_slide_page_v2 <- function(question_title,
                                           chart_inner_html,
                                           footer_html = "",
                                           chart_class = "mrv-chart",
                                           df_page = NULL) {
  slide_template <- paste(
    '<div class="slide multiple-results-v2 openended-results-v2">',
    '<div class="ei-kicker">V&#253;sledky</div>',
    '<div class="mrv-title">{{question_title}}</div>',
    '<div class="{{chart_class}}">',
    '{{chart_inner_html}}',
    '</div>',
    '{{footer_html}}',
    '</div>',
    sep = "\n"
  )

  expanded_slide <- knitr::knit_expand(
    text = slide_template,
    question_title = question_title,
    chart_inner_html = chart_inner_html,
    footer_html = footer_html,
    chart_class = chart_class
  )

  cat(knitr::knit_child(text = expanded_slide, quiet = TRUE, envir = environment()), sep = "\n")
}

render_openended_result_slide_v2 <- function(df_page) {
  chart_inner_html <- paste(
    '```{r}',
    '#| echo: false',
    '#| warning: false',
    '#| message: false',
    '#| fig-width: 14.6',
    '#| fig-height: 6.1',
    '',
    'plot_openended_balloon(df_page)',
    '```',
    sep = "\n"
  )
  footer_html <- paste(
    '<div class="openended-note">',
    openended_note_html(),
    '</div>',
    sep = "\n"
  )

  render_openended_slide_page_v2(
    question_title = get_openended_title(df_page),
    chart_inner_html = chart_inner_html,
    footer_html = footer_html,
    chart_class = "mrv-chart",
    df_page = df_page
  )
}

render_openended_fallback_slide_v2 <- function(df_page = openended_placeholder_page()) {
  chart_inner_html <- paste(
    '<div class="openended-fallback-copy">',
    openended_fallback_copy_html(),
    '</div>',
    sep = "\n"
  )

  render_openended_slide_page_v2(
    question_title = get_openended_title(df_page),
    chart_inner_html = chart_inner_html,
    footer_html = "",
    chart_class = "mrv-chart mrv-chart--fallback",
    df_page = df_page
  )
}

render_openended_section <- function(agenda_cfg, openended_analysis) {
  section_name <- resolve_openended_section_name(agenda_cfg)
  has_openended_section <- FALSE

  if (exists("openend_present", inherits = TRUE)) {
    has_openended_section <- isTRUE(get("openend_present", inherits = TRUE))
  } else if (is.list(openended_analysis) && !is.null(openended_analysis$input)) {
    has_openended_section <- nrow(openended_analysis$input) > 0
  }

  if (!has_openended_section) {
    return(invisible(NULL))
  }

  render_section_cover_slide(
    agenda_cfg = agenda_cfg,
    section_name = section_name,
    title = "Otev&#345;en&#233; ot&#225;zky",
    copy = paste(
      "Tato sekce shrnuje odpov&#283;di na otev&#345;en&#233; ot&#225;zky.",
      "Odpov&#283;di byly automaticky rozt&#345;&#237;d&#283;ny do t&#233;mat a ke ka&#382;d&#233;mu t&#233;matu byl p&#345;i&#345;azen orienta&#269;n&#237; sentiment.",
      "V&#253;sledky slou&#382;&#237; jako rychl&#253; kvalitativn&#237; p&#345;ehled hlavn&#237;ch motiv&#367; v odpov&#283;d&#237;ch."
    )
  )

  coded_df <- if (is.list(openended_analysis) && !is.null(openended_analysis$coded)) {
    openended_analysis$coded
  } else {
    openended_empty_coded_df()
  }
  input_pages <- if (is.list(openended_analysis) && !is.null(openended_analysis$input)) {
    split_openended_pages(openended_analysis$input)
  } else {
    list()
  }
  coded_pages <- split_openended_pages(coded_df)

  if (length(input_pages) == 0) {
    if (length(coded_pages) > 0) {
      invisible(lapply(coded_pages, render_openended_result_slide_v2))
    } else {
      render_openended_fallback_slide_v2()
    }
    return(invisible(NULL))
  }

  coded_page_refs <- vapply(coded_pages, openended_page_ref, character(1))
  coded_page_map <- stats::setNames(coded_pages, coded_page_refs)

  invisible(lapply(input_pages, function(input_page) {
    page_ref <- openended_page_ref(input_page)
    coded_page <- coded_page_map[[page_ref]]

    if (is.data.frame(coded_page) && nrow(coded_page) > 0) {
      render_openended_result_slide_v2(coded_page)
    } else {
      render_openended_fallback_slide_v2(input_page)
    }
  }))

  invisible(NULL)
}
