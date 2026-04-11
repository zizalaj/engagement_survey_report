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

source(
  here::here("codex", "navratnost-editor", "R", "typeform_helpers.R"),
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

ui <- shiny::fluidPage(
  shiny::titlePanel("Navratnost editor"),
  shiny::fluidRow(
    shiny::column(
      width = 4,
      shiny::selectInput("form_id", "Dostupne formulare", choices = c()),
      shiny::numericInput(
        "celkem_zamestnancu",
        "Celkovy pocet zamestnancu",
        value = NA,
        min = 1,
        step = 1
      ),
      shiny::actionButton("refresh_form", "Nacist znovu", class = "btn-secondary"),
      shiny::actionButton("render_report", "Vyrenderovat report", class = "btn-primary")
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
  report_path <- here::here("report_editable_api_copy.qmd")
  forms_rv <- shiny::reactiveVal(NULL)
  current_table_rv <- shiny::reactiveVal(
    dplyr::tibble(
      odpoved_hodnota = character(),
      pocet_respondentu = integer(),
      pocet_zamestnancu = numeric()
    )
  )
  respondents_rv <- shiny::reactiveVal(0L)
  status_rv <- shiny::reactiveVal("Načítám formuláře.")

  load_forms <- function() {
    status_rv("Načítám formuláře z Typeform API.")

    forms_df <- tryCatch({
      tf_token <- read_typeform_token(token_path)
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
      status_rv("Formuláře načteny.")
    } else if (is.null(forms_df) || nrow(forms_df) == 0) {
      shiny::updateSelectInput(session, "form_id", choices = c())
      current_table_rv(current_table_rv()[0, ])
    }
  }

  load_selected_form <- function(form_id) {
    shiny::req(nzchar(form_id))
    status_rv("Načítám odpovědi vybraného formuláře.")

    result <- tryCatch({
      tf_token <- read_typeform_token(token_path)
      api_bundle <- build_final_long_df(tf_token, form_id)
      inputs <- extract_return_rate_inputs(api_bundle$final_long_df)

      list(
        table = sanitize_department_table(inputs$oddeleni),
        respondents = as.integer(inputs$pocet_resp)
      )
    }, error = function(e) {
      status_rv(paste("Nepodarilo se nacist data formulare:", conditionMessage(e)))
      NULL
    })

    if (is.null(result)) {
      current_table_rv(current_table_rv()[0, ])
      respondents_rv(0L)
      return()
    }

    current_table_rv(result$table)
    respondents_rv(result$respondents)
    status_rv("Data formuláře načtena. Ruční hodnoty jsou jen pro toto otevřené sezení.")
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

  output$status_message <- shiny::renderUI({
    shiny::div(class = "alert alert-info", status_rv())
  })

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

    if (!isTRUE(is.finite(total_employees)) || total_employees <= 0) {
      shiny::showNotification("Zadejte platny celkovy pocet zamestnancu.", type = "error")
      return()
    }

    if (any(is.na(dept_table$pocet_zamestnancu))) {
      shiny::showNotification("Doplnte pocet zamestnancu pro vsechna oddeleni.", type = "error")
      return()
    }

    manual_json <- jsonlite::toJSON(
      dept_table %>% dplyr::select(.data$odpoved_hodnota, .data$pocet_zamestnancu),
      dataframe = "rows",
      auto_unbox = TRUE,
      na = "null"
    )

    output_dir <- here::here("codex", "navratnost-editor", "output")
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    output_name <- "report_editable_api_preview.html"
    output_file <- file.path(output_dir, output_name)
    status_rv("Renderuji report do HTML nahledu.")

    tryCatch({
      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      setwd(output_dir)

      quarto::quarto_render(
        input = report_path,
        output_file = output_name,
        execute_dir = here::here(),
        execute_params = list(
          form_id = input$form_id,
          celkem_zamestnancu = total_employees,
          oddeleni_manual = manual_json,
          token_path = normalizePath(token_path, winslash = "/", mustWork = TRUE)
        ),
        quiet = FALSE
      )

      utils::browseURL(output_file)
      status_rv("Report byl vyrenderovan a otevren v prohlizeci.")
    }, error = function(e) {
      message_text <- paste("Render selhal:", conditionMessage(e))
      status_rv(message_text)
      shiny::showNotification(message_text, type = "error", duration = NULL)
    })
  })
}

shiny::shinyApp(ui, server)
