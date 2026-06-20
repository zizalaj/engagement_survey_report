instal_funkce <- function(balicek) {
  if (!requireNamespace(balicek, quietly = TRUE)) {
    install.packages(balicek)
  }

  suppressPackageStartupMessages(
    library(balicek, character.only = TRUE)
  )
}

instal_funkce("shiny")
instal_funkce("DT")
instal_funkce("dplyr")
instal_funkce("jsonlite")
instal_funkce("here")
instal_funkce("quarto")
instal_funkce("pagedown")

source(
  here::here("codex", "navratnost-editor", "R", "typeform_helpers.R"),
  local = TRUE
)

source(
  here::here("codex", "render-tools", "print_report_editable.R"),
  local = TRUE
)

sanitize_department_table <- function(df) {
  df %>%
    dplyr::transmute(
      odpoved_hodnota = as.character(.data$odpoved_hodnota),
      pocet_respondentu = as.integer(.data$pocet_respondentu),
      pocet_zamestnancu = suppressWarnings(as.numeric(.data$pocet_zamestnancu))
    )
}

build_preview_metrics <- function(department_df, total_employees) {
  respondents <- sum(department_df$pocet_respondentu, na.rm = TRUE)

  company_return_rate <- if (isTRUE(is.finite(total_employees)) && total_employees > 0) {
    round((respondents / total_employees) * 100, 2)
  } else {
    NA_real_
  }

  per_department <- department_df %>%
    dplyr::mutate(
      navratnost = dplyr::if_else(
        is.na(.data$pocet_zamestnancu) | .data$pocet_zamestnancu <= 0,
        NA_real_,
        round((.data$pocet_respondentu / .data$pocet_zamestnancu) * 100, 2)
      )
    )

  list(
    respondents = respondents,
    company_return_rate = company_return_rate,
    per_department = per_department
  )
}

available_demography_groups <- function(final_long_df) {
  if (is.null(final_long_df) || !"group_title" %in% names(final_long_df)) {
    return(character())
  }

  final_long_df %>%
    dplyr::filter(!is.na(.data$group_title), nzchar(.data$group_title)) %>%
    dplyr::distinct(.data$group_title) %>%
    dplyr::arrange(.data$group_title) %>%
    dplyr::pull(.data$group_title) %>%
    as.character()
}

available_departments <- function(department_df) {
  if (is.null(department_df) || nrow(department_df) == 0) {
    return(character())
  }

  department_df %>%
    dplyr::filter(!is.na(.data$odpoved_hodnota), nzchar(.data$odpoved_hodnota)) %>%
    dplyr::distinct(.data$odpoved_hodnota) %>%
    dplyr::arrange(.data$odpoved_hodnota) %>%
    dplyr::pull(.data$odpoved_hodnota) %>%
    as.character()
}

safe_department_filename <- function(department_name) {
  safe_name <- iconv(as.character(department_name), from = "", to = "ASCII//TRANSLIT")
  safe_name <- tolower(safe_name %||% "oddeleni")
  safe_name <- gsub("[^a-z0-9]+", "_", safe_name)
  safe_name <- gsub("^_+|_+$", "", safe_name)

  if (!nzchar(safe_name)) {
    safe_name <- "oddeleni"
  }

  safe_name
}

ui <- shiny::fluidPage(
  shiny::titlePanel("Navratnost editor"),
  shiny::fluidRow(
    shiny::column(
      width = 4,
      shiny::selectInput("form_id", "Dostupne formulare", choices = c()),
      shiny::selectInput(
        "demography_group",
        "Nazev skupiny demografickych informaci",
        choices = c("Vyberte skupinu" = ""),
        selected = ""
      ),
      shiny::numericInput(
        "celkem_zamestnancu",
        "Celkovy pocet zamestnancu",
        value = NA,
        min = 1,
        step = 1
      ),
      shiny::radioButtons(
        "report_scope",
        "Rozsah reportu",
        choices = c(
          "Pouze firemni report" = "firma",
          "Pouze oddeleni" = "oddeleni",
          "Firma i oddeleni" = "oboji"
        ),
        selected = "firma"
      ),
      shiny::checkboxGroupInput(
        "selected_departments",
        "Oddeleni pro managerske reporty",
        choices = character(),
        selected = character()
      ),
      shiny::actionButton("refresh_form", "Nacist znovu", class = "btn-secondary"),
      shiny::actionButton("render_report", "Vyrenderovat report", class = "btn-primary"),
      shiny::uiOutput("download_report_ui")
    ),
    shiny::column(
      width = 8,
      shiny::uiOutput("status_message"),
      shiny::fluidRow(
        shiny::column(width = 4, shiny::wellPanel(
          shiny::tags$strong("Pocet respondentu"),
          shiny::div(shiny::textOutput("respondents_value"))
        )),
        shiny::column(width = 4, shiny::wellPanel(
          shiny::tags$strong("Navratnost firmy"),
          shiny::div(shiny::textOutput("return_rate_value"))
        )),
        shiny::column(width = 4, shiny::wellPanel(
          shiny::tags$strong("Oddeleni"),
          shiny::div(shiny::textOutput("department_count_value"))
        ))
      )
    )
  ),
  shiny::hr(),
  shiny::h4("Oddeleni"),
  DT::DTOutput("oddeleni_table"),
  shiny::hr(),
  shiny::h4("Nahled navratnosti dle oddeleni"),
  DT::DTOutput("department_preview")
)

server <- function(input, output, session) {
  token_path <- here::here("token.txt")
  report_path <- here::here("report_editable_api.qmd")
  company_output_dir <- here::here("codex", "navratnost-editor", "output")
  department_output_dir <- file.path(company_output_dir, "oddeleni-reports")
  forms_rv <- shiny::reactiveVal(NULL)
  form_data_rv <- shiny::reactiveVal(NULL)
  demography_by_form_rv <- shiny::reactiveVal(list())
  empty_department_table <- dplyr::tibble(
    odpoved_hodnota = character(),
    pocet_respondentu = integer(),
    pocet_zamestnancu = numeric()
  )
  current_table_rv <- shiny::reactiveVal(empty_department_table)
  respondents_rv <- shiny::reactiveVal(0L)
  status_rv <- shiny::reactiveVal("Nacitam formulare.")
  last_rendered_files <- shiny::reactiveVal(character())

  clear_current_preview <- function() {
    current_table_rv(empty_department_table)
    respondents_rv(0L)
    shiny::updateCheckboxGroupInput(session, "selected_departments", choices = character(), selected = character())
  }

  apply_demography_group <- function(demography_group_override = NULL) {
    form_id <- input$form_id %||% ""
    demography_group <- demography_group_override %||% input$demography_group %||% ""
    final_long_df <- form_data_rv()

    if (!nzchar(form_id) || is.null(final_long_df)) {
      clear_current_preview()
      return()
    }

    if (!nzchar(demography_group)) {
      clear_current_preview()
      status_rv("Vyberte skupinu demografickych informaci.")
      return()
    }

    result <- tryCatch({
      inputs <- extract_return_rate_inputs(
        final_long_df = final_long_df,
        demography_group_title = demography_group
      )

      list(
        table = sanitize_department_table(inputs$oddeleni),
        respondents = as.integer(inputs$pocet_resp),
        has_group_data = nrow(inputs$zakl_info) > 0
      )
    }, error = function(e) {
      status_rv(paste("Nepodarilo se nacist data formulare:", conditionMessage(e)))
      NULL
    })

    if (is.null(result)) {
      clear_current_preview()
      return()
    }

    current_table_rv(result$table)
    respondents_rv(result$respondents)

    if (!isTRUE(result$has_group_data)) {
      status_rv("Vybrana skupina neobsahuje multiple_choice odpovedi.")
      return()
    }

    if (nrow(result$table) == 0) {
      status_rv("Vybrana skupina byla nactena, ale oddeleni nebylo rozpoznano.")
      return()
    }

    status_rv("Data formulare nactena. Rucni hodnoty jsou jen pro toto otevrene sezeni.")
  }

  load_forms <- function() {
    status_rv("Nacitam formulare z Typeform API.")

    forms_df <- tryCatch({
      tf_token <- get_typeform_token(token_path)
      fetch_typeform_forms(tf_token, page = 1, page_size = 100)
    }, error = function(e) {
      status_rv(paste("Nepodarilo se nacist formulare:", conditionMessage(e)))
      NULL
    })

    forms_rv(forms_df)

    if (!is.null(forms_df) && nrow(forms_df) > 0) {
      choices <- prepare_form_choices(forms_df)
      shiny::updateSelectInput(
        session,
        "form_id",
        choices = choices,
        selected = unname(choices[[1]])
      )
      status_rv("Formulare nacteny.")
    } else if (is.null(forms_df) || nrow(forms_df) == 0) {
      shiny::updateSelectInput(session, "form_id", choices = c())
      shiny::updateSelectInput(
        session,
        "demography_group",
        choices = c("Vyberte skupinu" = ""),
        selected = ""
      )
      form_data_rv(NULL)
      clear_current_preview()
    }
  }

  load_selected_form <- function(form_id) {
    shiny::req(nzchar(form_id))
    status_rv("Nacitam odpovedi vybraneho formulare.")

    result <- tryCatch({
      tf_token <- get_typeform_token(token_path)
      api_bundle <- build_final_long_df(tf_token, form_id)

      list(
        final_long_df = api_bundle$final_long_df,
        groups = available_demography_groups(api_bundle$final_long_df)
      )
    }, error = function(e) {
      status_rv(paste("Nepodarilo se nacist data formulare:", conditionMessage(e)))
      NULL
    })

    if (is.null(result)) {
      form_data_rv(NULL)
      shiny::updateSelectInput(
        session,
        "demography_group",
        choices = c("Vyberte skupinu" = ""),
        selected = ""
      )
      clear_current_preview()
      return()
    }

    form_data_rv(result$final_long_df)
    groups <- result$groups

    if (length(groups) == 0) {
      shiny::updateSelectInput(
        session,
        "demography_group",
        choices = c("Skupiny nenalezeny" = ""),
        selected = ""
      )
      clear_current_preview()
      status_rv("Pro tento formular nebyly nalezeny dostupne skupiny.")
      return()
    }

    saved_map <- demography_by_form_rv()
    saved_group <- saved_map[[form_id]] %||% ""
    selected_group <- if (nzchar(saved_group) && saved_group %in% groups) saved_group else ""

    shiny::updateSelectInput(
      session,
      "demography_group",
      choices = c("Vyberte skupinu" = "", stats::setNames(groups, groups)),
      selected = selected_group
    )

    if (!nzchar(selected_group)) {
      clear_current_preview()
      status_rv("Formular nacten. Vyberte skupinu demografickych informaci.")
      return()
    }

    apply_demography_group(selected_group)
  }

  shiny::observe({
    load_forms()
  })

  shiny::observeEvent(input$refresh_form, {
    load_forms()
  })

  shiny::observeEvent(input$form_id, {
    load_selected_form(input$form_id)
  }, ignoreNULL = TRUE)

  shiny::observeEvent(input$demography_group, {
    form_id <- input$form_id %||% ""

    if (!nzchar(form_id)) {
      return()
    }

    saved_map <- demography_by_form_rv()
    saved_map[[form_id]] <- input$demography_group %||% ""
    demography_by_form_rv(saved_map)

    apply_demography_group()
  }, ignoreNULL = FALSE)

  shiny::observe({
    department_choices <- available_departments(current_table_rv())
    selected_departments <- intersect(input$selected_departments %||% character(), department_choices)

    shiny::updateCheckboxGroupInput(
      session,
      "selected_departments",
      choices = stats::setNames(department_choices, department_choices),
      selected = selected_departments
    )
  })

  output$status_message <- shiny::renderUI({
    shiny::div(class = "alert alert-info", status_rv())
  })

  output$download_report_ui <- shiny::renderUI({
    files <- last_rendered_files()

    if (length(files) == 0 || !all(file.exists(files))) {
      return(
        shiny::actionButton(
          "download_report_placeholder",
          "Stahnout report",
          class = "btn-secondary"
        )
      )
    }

    shiny::downloadButton("download_report", "Stahnout report", class = "btn-success")
  })

  shiny::observeEvent(input$download_report_placeholder, {
    shiny::showNotification(
      "Nejprve vyrenderujte PDF report. Potom jej budete moci stahnout v aplikaci.",
      type = "message"
    )
  }, ignoreInit = TRUE)

  output$oddeleni_table <- DT::renderDT({
    DT::datatable(
      current_table_rv(),
      rownames = FALSE,
      editable = list(target = "cell", disable = list(columns = c(0, 1))),
      options = list(dom = "t", paging = FALSE, ordering = FALSE),
      colnames = c("Oddeleni", "Pocet respondentu", "Pocet zamestnancu")
    )
  })

  shiny::observeEvent(input$oddeleni_table_cell_edit, {
    edit <- input$oddeleni_table_cell_edit
    tbl <- current_table_rv()

    shiny::req(nrow(tbl) >= edit$row)

    if (edit$col == 2) {
      new_value <- suppressWarnings(as.numeric(edit$value))
      tbl[edit$row, "pocet_zamestnancu"] <- new_value
      current_table_rv(sanitize_department_table(tbl))
    }
  })

  preview_metrics <- shiny::reactive({
    build_preview_metrics(
      current_table_rv(),
      suppressWarnings(as.numeric(input$celkem_zamestnancu))
    )
  })

  output$respondents_value <- shiny::renderText({
    format(preview_metrics()$respondents, big.mark = " ")
  })

  output$return_rate_value <- shiny::renderText({
    rate <- preview_metrics()$company_return_rate
    if (is.na(rate)) {
      return("doplnte celkovy pocet zamestnancu")
    }

    paste0(format(rate, nsmall = 2, decimal.mark = ","), " %")
  })

  output$department_count_value <- shiny::renderText({
    nrow(current_table_rv())
  })

  output$department_preview <- DT::renderDT({
    preview_metrics()$per_department %>%
      dplyr::transmute(
        Oddeleni = .data$odpoved_hodnota,
        Respondenti = .data$pocet_respondentu,
        Zamestnanci = .data$pocet_zamestnancu,
        Navratnost = dplyr::if_else(
          is.na(.data$navratnost),
          "",
          paste0(format(.data$navratnost, nsmall = 2, decimal.mark = ","), " %")
        )
      ) %>%
      DT::datatable(
        rownames = FALSE,
        options = list(dom = "t", paging = FALSE, ordering = FALSE)
      )
  })

  shiny::observeEvent(input$render_report, {
    shiny::req(nzchar(input$form_id))

    total_employees <- suppressWarnings(as.numeric(input$celkem_zamestnancu))
    dept_table <- current_table_rv()
    report_scope <- input$report_scope %||% "firma"
    selected_departments <- input$selected_departments %||% character()
    needs_company_report <- report_scope %in% c("firma", "oboji")
    needs_department_reports <- report_scope %in% c("oddeleni", "oboji")

    if (!nzchar(input$demography_group %||% "")) {
      shiny::showNotification("Vyberte skupinu demografickych informaci.", type = "error")
      return()
    }

    if (!isTRUE(is.finite(total_employees)) || total_employees <= 0) {
      shiny::showNotification("Zadejte platny celkovy pocet zamestnancu.", type = "error")
      return()
    }

    if (nrow(dept_table) == 0) {
      shiny::showNotification("Pro vybranou skupinu nejsou dostupna data oddeleni.", type = "error")
      return()
    }

    if (needs_company_report && any(is.na(dept_table$pocet_zamestnancu))) {
      shiny::showNotification("Doplnte pocet zamestnancu pro vsechna oddeleni.", type = "error")
      return()
    }

    if (needs_department_reports && length(selected_departments) == 0) {
      shiny::showNotification("Vyberte alespon jedno oddeleni pro managersky report.", type = "error")
      return()
    }

    if (needs_department_reports) {
      selected_department_rows <- dept_table %>%
        dplyr::filter(.data$odpoved_hodnota %in% selected_departments)

      if (nrow(selected_department_rows) != length(selected_departments)) {
        shiny::showNotification("Nektera vybrana oddeleni uz nejsou v aktualni tabulce.", type = "error")
        return()
      }

      if (any(is.na(selected_department_rows$pocet_zamestnancu))) {
        shiny::showNotification("Doplnte pocet zamestnancu pro vybrana oddeleni.", type = "error")
        return()
      }
    }

    if (!dir.exists(company_output_dir)) {
      dir.create(company_output_dir, recursive = TRUE)
    }

    if (needs_department_reports && !dir.exists(department_output_dir)) {
      dir.create(department_output_dir, recursive = TRUE)
    }

    manual_json <- jsonlite::toJSON(
      dept_table %>% dplyr::select(.data$odpoved_hodnota, .data$pocet_zamestnancu),
      dataframe = "rows",
      auto_unbox = TRUE,
      na = "null"
    )

    common_params <- list(
      pruzkum = input$form_id,
      form_id = input$form_id,
      demografie = input$demography_group,
      celkem_zamestnancu = total_employees,
      oddeleni_manual = manual_json
    )

    render_one_report <- function(output_dir, output_name, execute_params) {
      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      setwd(output_dir)

      quarto::quarto_render(
        input = report_path,
        output_file = output_name,
        execute_dir = here::here(),
        execute_params = execute_params,
        quiet = FALSE
      )

      file.path(output_dir, output_name)
    }

    render_one_pdf <- function(output_dir, output_name, execute_params) {
      html_path <- render_one_report(
        output_dir = output_dir,
        output_name = output_name,
        execute_params = execute_params
      )

      pdf_path <- sub("\\.html$", ".pdf", html_path)

      print_html_to_pdf(
        input_html = html_path,
        output_pdf = pdf_path
      )
    }

    last_rendered_files(character())
    rendered_files <- character()
    status_rv("Renderuji reporty do PDF.")

    tryCatch({
      if (needs_company_report) {
        rendered_files <- c(
          rendered_files,
          render_one_pdf(
            output_dir = company_output_dir,
            output_name = "report_editable_api_preview.html",
            execute_params = c(common_params, list(report_type = "firma", oddeleni_report = NULL))
          )
        )
      }

      if (needs_department_reports) {
        for (department_name in selected_departments) {
          output_name <- paste0(
            "report_oddeleni_",
            safe_department_filename(department_name),
            ".html"
          )

          rendered_files <- c(
            rendered_files,
            render_one_pdf(
              output_dir = department_output_dir,
              output_name = output_name,
              execute_params = c(
                common_params,
                list(
                  report_type = "oddeleni",
                  oddeleni_report = department_name
                )
              )
            )
          )
        }
      }

      last_rendered_files(rendered_files)

      status_rv(
        paste(
          "PDF reporty byly vyrenderovany:",
          paste(basename(rendered_files), collapse = ", "),
          "Nyni je muzete stahnout tlacitkem Stahnout report."
        )
      )
    }, error = function(e) {
      last_rendered_files(character())
      message_text <- paste("Render selhal:", conditionMessage(e))
      status_rv(message_text)
      shiny::showNotification(message_text, type = "error", duration = NULL)
      return()
    })
  })

  output$download_report <- shiny::downloadHandler(
    filename = function() {
      files <- last_rendered_files()

      if (length(files) == 1) {
        return(basename(files[[1]]))
      }

      paste0("reporty_", Sys.Date(), ".zip")
    },
    content = function(file) {
      files <- last_rendered_files()

      if (length(files) == 0 || !all(file.exists(files))) {
        stop("No rendered report files are available for download.")
      }

      if (length(files) == 1) {
        copied <- file.copy(files[[1]], file, overwrite = TRUE)
        if (!isTRUE(copied)) {
          stop("Nepodarilo se pripravit report ke stazeni.")
        }
        return(invisible(NULL))
      }

      temp_dir <- tempfile("reports_")
      dir.create(temp_dir, recursive = TRUE)
      on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)
      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)

      copied_files <- file.path(temp_dir, basename(files))
      copied <- file.copy(files, copied_files, overwrite = TRUE)

      if (!all(copied)) {
        stop("Nepodarilo se pripravit vsechny reporty ke stazeni.")
      }

      setwd(temp_dir)
      utils::zip(zipfile = file, files = basename(copied_files))
    },
    contentType = "application/octet-stream"
  )
}

shiny::shinyApp(ui, server)
