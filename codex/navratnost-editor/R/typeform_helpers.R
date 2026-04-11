`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

read_typeform_token <- function(token_path) {
  if (!file.exists(token_path)) stop("Token file not found.")

  token <- trimws(readLines(token_path, warn = FALSE, encoding = "UTF-8")[1])
  if (!nzchar(token)) stop("Token file is empty.")

  token
}

fetch_typeform_forms <- function(tf_token, page = 1, page_size = 100) {
  res <- httr::GET(
    "https://api.typeform.com/forms",
    httr::add_headers(Authorization = paste("Bearer", tf_token)),
    query = list(page = page, page_size = page_size)
  )
  httr::stop_for_status(res)

  out <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
  out$items %||% data.frame()
}

get_form_definition <- function(tf_token, form_id) {
  url <- paste0("https://api.typeform.com/forms/", form_id)
  res <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", tf_token)))
  httr::stop_for_status(res)

  jsonlite::fromJSON(
    httr::content(res, "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
}

get_responses_page <- function(tf_token, form_id, page_size = 1000, before = NULL) {
  base <- paste0("https://api.typeform.com/forms/", form_id, "/responses")
  query <- list(page_size = page_size)

  if (!is.null(before) && nzchar(before)) {
    query$before <- before
  }

  res <- httr::GET(
    base,
    httr::add_headers(Authorization = paste("Bearer", tf_token)),
    query = query
  )
  httr::stop_for_status(res)

  jsonlite::fromJSON(
    httr::content(res, "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
}

get_all_responses <- function(tf_token, form_id, page_size = 1000, max_pages = 200) {
  all_items <- list()
  before <- NULL

  for (i in seq_len(max_pages)) {
    page <- get_responses_page(tf_token, form_id, page_size = page_size, before = before)
    items <- page$items %||% list()

    if (length(items) == 0) break

    all_items <- c(all_items, items)

    last_token <- items[[length(items)]]$token %||% ""
    if (!nzchar(last_token)) break

    before <- last_token
    if (length(items) < page_size) break
  }

  all_items
}

answer_value <- function(answer) {
  field_type <- answer[["field"]][["type"]] %||% ""

  if (identical(field_type, "multiple_choice")) {
    return(as.character(answer[["choice"]][["label"]] %||% NA_character_))
  }

  if (identical(field_type, "long_text")) {
    return(as.character(answer[["text"]] %||% NA_character_))
  }

  if (identical(field_type, "choice")) {
    return(as.character(answer[["choice"]][["label"]] %||% NA_character_))
  }

  as.character(answer[["number"]] %||% answer[["boolean"]] %||% answer[["text"]] %||% NA_character_)
}

preprocess_responses <- function(response_list) {
  processed_list <- vector("list", length(response_list))

  for (i in seq_along(response_list)) {
    respondent <- response_list[[i]]
    respondent_answers <- respondent[["answers"]] %||% list()

    resp_data <- list(
      respondent_id = respondent[["response_id"]] %||% NA_character_,
      landed_at = respondent[["landed_at"]] %||% NA_character_,
      submitted_at = respondent[["submitted_at"]] %||% NA_character_
    )

    respondent_rows <- vector("list", length(respondent_answers))

    for (j in seq_along(respondent_answers)) {
      answer <- respondent_answers[[j]]

      respondent_rows[[j]] <- list(
        respondent_id = resp_data$respondent_id,
        landed_at = resp_data$landed_at,
        submitted_at = resp_data$submitted_at,
        typ_otazky = answer[["field"]][["type"]] %||% NA_character_,
        otazka_ref = answer[["field"]][["ref"]] %||% NA_character_,
        odpoved_hodnota = answer_value(answer)
      )
    }

    processed_list[[i]] <- respondent_rows
  }

  processed_list
}

process_survey_codebook <- function(definition) {
  survey_list <- list()
  k <- 1

  for (i in seq_along(definition[["fields"]])) {
    field <- definition[["fields"]][[i]]
    field_type <- field[["type"]] %||% ""

    if (identical(field_type, "group")) {
      group_title <- as.character(field[["title"]] %||% NA_character_)
      group_ref <- as.character(field[["ref"]] %||% NA_character_)
      group_fields <- field[["properties"]][["fields"]] %||% list()

      for (j in seq_along(group_fields)) {
        question <- group_fields[[j]]
        survey_list[[k]] <- list(
          group_title = group_title,
          group_ref = group_ref,
          otazka_title = question[["title"]] %||% NA_character_,
          otazka_ref = question[["ref"]] %||% NA_character_
        )
        k <- k + 1
      }

      next
    }

    if (identical(field_type, "statement")) next

    survey_list[[k]] <- list(
      group_title = NA_character_,
      group_ref = NA_character_,
      otazka_title = field[["title"]] %||% NA_character_,
      otazka_ref = field[["ref"]] %||% NA_character_
    )
    k <- k + 1
  }

  dplyr::bind_rows(survey_list)
}

build_final_long_df <- function(tf_token, form_id, page_size = 1000, max_pages = 200) {
  definition <- get_form_definition(tf_token, form_id)
  all_responses <- get_all_responses(tf_token, form_id, page_size = page_size, max_pages = max_pages)

  survey_data_preprocessed <- preprocess_responses(all_responses)
  survey_df_long <- dplyr::bind_rows(unlist(survey_data_preprocessed, recursive = FALSE))
  codebook_df <- process_survey_codebook(definition)

  final_long_df <- survey_df_long %>%
    dplyr::left_join(codebook_df, by = "otazka_ref")

  list(
    definition = definition,
    all_responses = all_responses,
    final_long_df = final_long_df,
    codebook_df = codebook_df
  )
}

find_department_question <- function(zakl_info_otazky, oddeleni_expr = "jakém") {
  oddeleni_index <- stringr::str_detect(zakl_info_otazky, oddeleni_expr)
  matches <- zakl_info_otazky[oddeleni_index]

  matches[[1]] %||% NA_character_
}

normalize_oddeleni_manual <- function(oddeleni_manual) {
  if (is.null(oddeleni_manual) || length(oddeleni_manual) == 0) {
    return(tibble::tibble(
      odpoved_hodnota = character(),
      pocet_zamestnancu = numeric()
    ))
  }

  if (is.character(oddeleni_manual) && length(oddeleni_manual) == 1) {
    oddeleni_manual <- jsonlite::fromJSON(oddeleni_manual, simplifyDataFrame = TRUE)
  }

  oddeleni_manual <- tibble::as_tibble(oddeleni_manual)

  required_cols <- c("odpoved_hodnota", "pocet_zamestnancu")
  missing_cols <- setdiff(required_cols, names(oddeleni_manual))
  if (length(missing_cols) > 0) {
    stop("oddeleni_manual is missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  oddeleni_manual %>%
    dplyr::transmute(
      odpoved_hodnota = as.character(.data$odpoved_hodnota),
      pocet_zamestnancu = suppressWarnings(as.numeric(.data$pocet_zamestnancu))
    )
}

extract_return_rate_inputs <- function(final_long_df,
                                       celkem_zamestnancu = NA_real_,
                                       oddeleni_manual = NULL,
                                       oddeleni_expr = "jakém") {
  zakl_info <- final_long_df %>%
    dplyr::filter(.data$group_title == "Základní info", .data$typ_otazky == "multiple_choice") %>%
    dplyr::select(.data$odpoved_hodnota, .data$otazka_title)

  zakl_info_otazky <- unique(zakl_info$otazka_title)
  oddeleni_ot <- find_department_question(zakl_info_otazky, oddeleni_expr = oddeleni_expr)

  if (is.na(oddeleni_ot)) {
    oddeleni <- tibble::tibble(
      odpoved_hodnota = character(),
      pocet_respondentu = integer(),
      pocet_zamestnancu = numeric()
    )
  } else {
    oddeleni_base <- zakl_info %>%
      dplyr::filter(.data$otazka_title == oddeleni_ot) %>%
      dplyr::group_by(.data$odpoved_hodnota) %>%
      dplyr::summarise(pocet_respondentu = dplyr::n(), .groups = "drop")

    oddeleni_manual_df <- normalize_oddeleni_manual(oddeleni_manual)

    oddeleni <- oddeleni_base %>%
      dplyr::left_join(oddeleni_manual_df, by = "odpoved_hodnota") %>%
      dplyr::mutate(pocet_zamestnancu = dplyr::coalesce(.data$pocet_zamestnancu, 0))
  }

  odd_souhrn_long <- oddeleni %>%
    tidyr::pivot_longer(cols = c("pocet_respondentu", "pocet_zamestnancu"),
                        names_to = "Kategorie", values_to = "Pocet") %>%
    dplyr::mutate(
      oddeleni_trunc = stringr::str_trunc(.data$odpoved_hodnota, width = 25, ellipsis = "...")
    )

  doba_vyplneni <- final_long_df %>%
    dplyr::select(.data$respondent_id, .data$landed_at, .data$submitted_at) %>%
    dplyr::group_by(.data$respondent_id) %>%
    dplyr::mutate(
      landed_at = lubridate::ymd_hms(.data$landed_at),
      submitted_at = lubridate::ymd_hms(.data$submitted_at),
      cas_vyplneni = .data$submitted_at - .data$landed_at
    ) %>%
    dplyr::distinct(.data$respondent_id, .data$cas_vyplneni)

  avg_doba_vyplneni <- mean(doba_vyplneni$cas_vyplneni)
  avg_doba_vyplneni <- round(as.numeric(avg_doba_vyplneni, units = "mins"), 0)

  pocet_resp <- dplyr::n_distinct(final_long_df$respondent_id)
  navratnost <- if (isTRUE(!is.na(celkem_zamestnancu) && celkem_zamestnancu > 0)) {
    round((pocet_resp / celkem_zamestnancu) * 100, 2)
  } else {
    NA_real_
  }

  list(
    zakl_info = zakl_info,
    zakl_info_otazky = zakl_info_otazky,
    oddeleni_ot = oddeleni_ot,
    oddeleni = oddeleni,
    odd_souhrn_long = odd_souhrn_long,
    avg_doba_vyplneni = avg_doba_vyplneni,
    pocet_resp = pocet_resp,
    navratnost = navratnost
  )
}

prepare_form_choices <- function(forms_df) {
  if (is.null(forms_df) || nrow(forms_df) == 0) {
    return(setNames(character(), character()))
  }

  forms_tbl <- tibble::as_tibble(forms_df)
  ids <- as.character(forms_tbl$id %||% seq_len(nrow(forms_tbl)))
  labels <- if ("title" %in% names(forms_tbl)) as.character(forms_tbl$title) else ids

  empty_label <- is.na(labels) | !nzchar(labels)
  labels[empty_label] <- ids[empty_label]

  stats::setNames(ids, paste0(labels, " (", ids, ")"))
}
