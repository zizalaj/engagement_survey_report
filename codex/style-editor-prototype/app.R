library(shiny)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required. Install it with install.packages('jsonlite').", call. = FALSE)
}

`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) {
    return(y)
  }
  x
}

coerce_num <- function(x, default = NA_real_) {
  out <- suppressWarnings(as.numeric(x))
  if (length(out) == 0 || is.na(out)) {
    return(default)
  }
  out
}

coerce_int <- function(x, default = 0L) {
  out <- suppressWarnings(as.integer(x))
  if (length(out) == 0 || is.na(out)) {
    return(default)
  }
  out
}

trim_or_empty <- function(x) {
  if (is.null(x)) {
    return("")
  }
  trimws(as.character(x))
}

format_css_value <- function(x, digits = 4) {
  if (is.null(x) || is.na(x)) {
    return("")
  }
  if (abs(x - round(x)) < 1e-8) {
    return(sprintf("%d", as.integer(round(x))))
  }
  txt <- format(round(x, digits), nsmall = 0, trim = TRUE, scientific = FALSE)
  txt <- sub("0+$", "", txt)
  txt <- sub("\\.$", "", txt)
  txt
}

read_text_file <- function(path, default = "") {
  if (!file.exists(path)) {
    return(default)
  }
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

write_text_file <- function(path, text) {
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  writeLines(text, path, useBytes = TRUE)
  invisible(TRUE)
}

ensure_generated_dirs <- function(generated_dir, preset_dir) {
  if (!dir.exists(generated_dir)) {
    dir.create(generated_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(preset_dir)) {
    dir.create(preset_dir, recursive = TRUE, showWarnings = FALSE)
  }
}

to_web_path <- function(x) {
  gsub("\\\\", "/", x)
}

path_relative_to <- function(path, root) {
  path_norm <- to_web_path(normalizePath(path, winslash = "/", mustWork = FALSE))
  root_norm <- to_web_path(normalizePath(root, winslash = "/", mustWork = TRUE))
  root_prefix <- paste0(root_norm, "/")

  if (startsWith(tolower(path_norm), tolower(root_prefix))) {
    return(substr(path_norm, nchar(root_prefix) + 1L, nchar(path_norm)))
  }
  basename(path_norm)
}

list_qmd_choices <- function(project_root) {
  qmd_files <- list.files(
    path = project_root,
    pattern = "\\.qmd$",
    full.names = TRUE,
    ignore.case = TRUE,
    recursive = FALSE
  )

  if (!length(qmd_files)) {
    return(character(0))
  }

  rel <- vapply(qmd_files, path_relative_to, character(1), root = project_root)
  rel <- sort(unique(to_web_path(rel)))
  setNames(rel, rel)
}

choose_default_qmd <- function(qmd_choices) {
  if (!length(qmd_choices)) {
    return("")
  }

  preferred <- c("report_editable_api.qmd", "report_editable.qmd", "report.qmd")
  values <- unname(qmd_choices)
  for (candidate in preferred) {
    if (candidate %in% values) {
      return(candidate)
    }
  }

  values[[1]]
}

read_qmd_front_matter <- function(qmd_path) {
  if (!file.exists(qmd_path)) {
    return(character(0))
  }

  lines <- readLines(qmd_path, warn = FALSE, encoding = "UTF-8")
  if (!length(lines) || trimws(lines[1]) != "---") {
    return(character(0))
  }

  if (length(lines) < 2) {
    return(character(0))
  }

  end_candidates <- which(trimws(lines[-1]) %in% c("---", "..."))
  if (!length(end_candidates)) {
    return(character(0))
  }

  end_idx <- end_candidates[1] + 1L
  if (end_idx <= 2) {
    return(character(0))
  }
  lines[2:(end_idx - 1L)]
}

qmd_includes_override_css <- function(qmd_path, override_rel = "generated/style-overrides.css") {
  fm <- read_qmd_front_matter(qmd_path)
  if (!length(fm)) {
    return(FALSE)
  }

  fm_text <- tolower(paste(fm, collapse = "\n"))
  override_pattern <- "generated[/\\\\]style-overrides\\.css"

  grepl(override_pattern, fm_text, perl = TRUE)
}

guess_qmd_output_html <- function(qmd_path) {
  fm <- read_qmd_front_matter(qmd_path)
  out_idx <- grep("^\\s*output-file\\s*:", fm, ignore.case = TRUE)

  output_name <- ""
  if (length(out_idx)) {
    raw <- sub("^\\s*output-file\\s*:\\s*", "", fm[out_idx[1]], ignore.case = TRUE)
    raw <- trimws(raw)
    raw <- sub("\\s+#.*$", "", raw)
    raw <- sub("^['\"]", "", raw)
    raw <- sub("['\"]$", "", raw)
    output_name <- trimws(raw)
  }

  if (!nzchar(output_name)) {
    output_name <- paste0(tools::file_path_sans_ext(basename(qmd_path)), ".html")
  }

  output_name <- to_web_path(output_name)
  file.path(dirname(qmd_path), output_name)
}

find_quarto_binary <- function() {
  candidates <- character(0)

  qpath_env <- trim_or_empty(Sys.getenv("QUARTO_PATH", unset = ""))
  if (nzchar(qpath_env)) {
    candidates <- c(
      candidates,
      qpath_env,
      file.path(qpath_env, "quarto.exe"),
      file.path(qpath_env, "bin", "quarto.exe")
    )
  }

  from_path <- unname(Sys.which("quarto"))
  if (nzchar(from_path)) {
    candidates <- c(candidates, from_path)
  }

  candidates <- c(
    candidates,
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe",
    "C:/Program Files/Quarto/bin/quarto.exe"
  )

  candidates <- unique(to_web_path(candidates))
  existing <- candidates[file.exists(candidates)]
  if (!length(existing)) {
    return("")
  }
  existing[1]
}

render_quarto_file <- function(project_root, qmd_rel) {
  qmd_rel <- to_web_path(trim_or_empty(qmd_rel))
  if (!nzchar(qmd_rel)) {
    return(list(success = FALSE, message = "No Quarto document selected.", log = character(0), html_path = ""))
  }

  qmd_path <- file.path(project_root, qmd_rel)
  if (!file.exists(qmd_path)) {
    return(list(
      success = FALSE,
      message = sprintf("Selected Quarto file does not exist: %s", qmd_rel),
      log = character(0),
      html_path = ""
    ))
  }

  quarto_bin <- find_quarto_binary()
  if (!nzchar(quarto_bin)) {
    return(list(
      success = FALSE,
      message = "Quarto executable was not found. Set QUARTO_PATH or install Quarto.",
      log = character(0),
      html_path = ""
    ))
  }

  expected_html <- guess_qmd_output_html(qmd_path)
  expected_rel <- path_relative_to(expected_html, project_root)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(project_root)

  started <- Sys.time()
  cmd_out <- tryCatch(
    suppressWarnings(
      system2(
        command = quarto_bin,
        args = c("render", shQuote(qmd_rel)),
        stdout = TRUE,
        stderr = TRUE
      )
    ),
    error = function(e) e
  )
  elapsed <- round(as.numeric(difftime(Sys.time(), started, units = "secs")), 2)

  if (inherits(cmd_out, "error")) {
    return(list(
      success = FALSE,
      message = sprintf("Quarto render failed before completion: %s", conditionMessage(cmd_out)),
      log = conditionMessage(cmd_out),
      html_path = "",
      elapsed = elapsed
    ))
  }

  status <- attr(cmd_out, "status")
  if (is.null(status)) {
    status <- 0L
  }

  if (!file.exists(expected_html)) {
    return(list(
      success = FALSE,
      message = sprintf("Render finished but expected HTML was not found: %s", expected_rel),
      log = cmd_out,
      html_path = "",
      elapsed = elapsed
    ))
  }

  if (!identical(as.integer(status), 0L)) {
    return(list(
      success = FALSE,
      message = sprintf("Quarto render returned non-zero exit status (%s).", status),
      log = cmd_out,
      html_path = expected_html,
      elapsed = elapsed
    ))
  }

  list(
    success = TRUE,
    message = sprintf("Rendered %s in %0.2fs", qmd_rel, elapsed),
    log = cmd_out,
    html_path = expected_html,
    elapsed = elapsed
  )
}

build_iframe_src <- function(html_path, project_root, resource_prefix) {
  if (is.null(html_path) || !nzchar(trim_or_empty(html_path)) || !file.exists(html_path)) {
    return("")
  }
  rel <- to_web_path(path_relative_to(html_path, project_root))
  paste0(
    resource_prefix,
    "/",
    utils::URLencode(rel, reserved = FALSE),
    "?ts=",
    as.integer(Sys.time())
  )
}

sanitize_preset_name <- function(x) {
  clean <- trim_or_empty(x)
  clean <- gsub("[^A-Za-z0-9_-]+", "-", clean)
  clean <- gsub("(^-+|-+$)", "", clean)
  clean
}

default_global_tokens <- function() {
  list(
    enabled = FALSE,
    accent = "#63e8c6",
    accent_light = "#e1faf4",
    accent_pink = "#ff8ade",
    accent_pink_strong = "#ff67aa",
    yellow = "#ffd06b",
    yellow_light = "#ffedc3",
    light_grey = "#d1d1d1",
    normal_grey = "#a5a5a5",
    page_bg = "#ffffff",
    black = "#000000"
  )
}

merge_tokens <- function(defaults, incoming) {
  out <- defaults
  if (!is.list(incoming)) {
    return(out)
  }
  for (nm in names(defaults)) {
    if (!is.null(incoming[[nm]])) {
      out[[nm]] <- incoming[[nm]]
    }
  }
  out
}

build_token_css <- function(tokens) {
  if (!isTRUE(tokens$enabled)) {
    return("")
  }

  var_map <- c(
    accent = "--accent",
    accent_light = "--accent-light",
    accent_pink = "--accent-pink",
    accent_pink_strong = "--accent-pink-strong",
    yellow = "--yellow",
    yellow_light = "--yellow-light",
    light_grey = "--light-grey",
    normal_grey = "--normal-grey",
    page_bg = "--page-bg",
    black = "--black"
  )

  rows <- character(0)
  for (nm in names(var_map)) {
    value <- trim_or_empty(tokens[[nm]])
    if (nzchar(value)) {
      rows <- c(rows, sprintf("  %s: %s;", var_map[[nm]], value))
    }
  }

  if (!length(rows)) {
    return("")
  }

  paste(c(":root {", rows, "}"), collapse = "\n")
}

build_intro_markup <- function() {
  paste(
    '<div class="slide intro-slide" data-component-id="intro_slide">',
    '  <div class="top-badge"><span class="pill">Quick Scan</span></div>',
    '  <div class="main accent" data-component-id="intro_main">Engagement survey</div>',
    '  <div class="subtitle-wrap">',
    '    <div class="subtitle" data-component-id="intro_subtitle">Management output</div>',
    '    <div class="reserved-logo-space" aria-hidden="true"></div>',
    "  </div>",
    '  <div class="brand-placeholder" data-component-id="intro_logo">juiceUP</div>',
    "</div>",
    sep = "\n"
  )
}

build_agenda_markup <- function() {
  paste(
    '<div class="slide agenda-slide" data-component-id="agenda_slide">',
    '  <div class="agenda-content">',
    '    <div class="agenda-title" data-component-id="agenda_title">Agenda</div>',
    '    <ul class="agenda-list" data-component-id="agenda_list">',
    '      <li class="agenda-item"><span class="agenda-num">01</span><span class="agenda-text">Survey in numbers</span></li>',
    '      <li class="agenda-item"><span class="agenda-num">02</span><span class="agenda-text">Engagement results</span></li>',
    '      <li class="agenda-item"><span class="agenda-num">03</span><span class="agenda-text">Driver results</span></li>',
    '      <li class="agenda-item"><span class="agenda-num">04</span><span class="agenda-text">Problematic topics</span></li>',
    '      <li class="agenda-item"><span class="agenda-num">05</span><span class="agenda-text">Multiple choice</span></li>',
    "    </ul>",
    "  </div>",
    '  <div class="progress-bar" data-component-id="agenda_progress"></div>',
    "</div>",
    sep = "\n"
  )
}

build_section_markup <- function() {
  paste(
    '<div class="slide section-slide" data-component-id="section_slide">',
    '  <div class="section-number" data-component-id="section_number">02</div>',
    '  <div class="section-title" data-component-id="section_title">Engagement results</div>',
    '  <div class="section-description" data-component-id="section_description">',
    "    Summary section for headline findings with quick interpretation.",
    "  </div>",
    '  <div class="scv-progress" data-component-id="section_progress">',
    '    <div class="scv-progress-fill"></div>',
    "  </div>",
    "</div>",
    sep = "\n"
  )
}

build_eio_markup <- function() {
  paste(
    '<div class="slide eio-v2" data-component-id="eio_slide">',
    '  <div class="eio-title" data-component-id="eio_title">Engagement by department</div>',
    '  <div class="eio-chart" data-component-id="eio_chart">',
    '    <div class="chart-placeholder">Chart area (.eio-chart)</div>',
    "  </div>",
    '  <div class="eio-legend" data-component-id="eio_legend">',
    '    <span class="legend-item"><span class="swatch teal"></span> Department</span>',
    '    <span class="legend-item"><span class="swatch grey"></span> Company</span>',
    "  </div>",
    "</div>",
    sep = "\n"
  )
}

build_driver_company_markup <- function() {
  paste(
    '<div class="slide driver-company-v2" data-component-id="drvco_slide">',
    '  <div class="drvco-title" data-component-id="drvco_title">Driver results for company</div>',
    '  <div class="drvco-grid" data-component-id="drvco_grid">',
    '    <div class="drvco-plot" data-component-id="drvco_plot">',
    '      <div class="chart-placeholder">Plot area (.drvco-plot)</div>',
    "    </div>",
    '    <div class="drvco-text">',
    "      Top questions and grouped score explanation placeholder.",
    "    </div>",
    "  </div>",
    "</div>",
    sep = "\n"
  )
}

build_driver_detail_markup <- function() {
  paste(
    '<div class="slide driver-detail2-v2" data-component-id="drvdet_slide">',
    '  <div class="drvdet-title" data-component-id="drvdet_title">Driver detail by score</div>',
    '  <div class="drvdet-plot-full" data-component-id="drvdet_plot">',
    '    <div class="chart-placeholder">Plot area (.drvdet-plot-full)</div>',
    "  </div>",
    "</div>",
    sep = "\n"
  )
}

build_problematic_markup <- function() {
  paste(
    '<div class="slide problematic-breakdown-v2" data-component-id="pbv_slide">',
    '  <div class="pbv-title" data-component-id="pbv_title">Problematic question by department</div>',
    '  <div class="pbv-chart" data-component-id="pbv_chart">',
    '    <div class="chart-placeholder">Chart area (.pbv-chart)</div>',
    "  </div>",
    '  <div class="pbv-legend" data-component-id="pbv_legend">',
    '    <span class="legend-item"><span class="swatch teal"></span> Positive</span>',
    '    <span class="legend-item"><span class="swatch grey"></span> Neutral</span>',
    '    <span class="legend-item"><span class="swatch pink"></span> Negative</span>',
    "  </div>",
    "</div>",
    sep = "\n"
  )
}

build_multiple_choice_markup <- function() {
  paste(
    '<div class="slide multiple-results-v2" data-component-id="mrv_slide">',
    '  <div class="mrv-title" data-component-id="mrv_title">Multiple choice results</div>',
    '  <div class="mrv-chart" data-component-id="mrv_chart">',
    '    <div class="chart-placeholder">Chart area (.mrv-chart)</div>',
    "  </div>",
    "</div>",
    sep = "\n"
  )
}

create_component_registry <- function() {
  list(
    intro = list(
      label = "Intro slide",
      markup = build_intro_markup,
      components = list(
        intro_slide = list(
          label = "Whole intro slide",
          selector = ".intro-slide",
          groups = c("spacing", "size")
        ),
        intro_main = list(
          label = "Main title",
          selector = ".intro-slide .main",
          groups = c("position", "size", "typography")
        ),
        intro_subtitle = list(
          label = "Subtitle",
          selector = ".intro-slide .subtitle",
          groups = c("position", "size", "typography")
        ),
        intro_logo = list(
          label = "Brand logo",
          selector = ".intro-slide .brand-logo, .intro-slide .brand-placeholder",
          groups = c("position", "size")
        )
      )
    ),
    agenda = list(
      label = "Agenda slide",
      markup = build_agenda_markup,
      components = list(
        agenda_slide = list(
          label = "Whole agenda slide",
          selector = ".agenda-slide",
          groups = c("spacing", "size")
        ),
        agenda_title = list(
          label = "Agenda title",
          selector = ".agenda-title",
          groups = c("position", "size", "typography")
        ),
        agenda_list = list(
          label = "Agenda list",
          selector = ".agenda-list",
          groups = c("position", "spacing", "size")
        ),
        agenda_progress = list(
          label = "Progress indicator",
          selector = ".agenda-slide .scv-progress, .agenda-slide .progress-bar",
          groups = c("position", "size")
        )
      )
    ),
    section = list(
      label = "Section cover slide",
      markup = build_section_markup,
      components = list(
        section_slide = list(
          label = "Whole section slide",
          selector = ".section-slide",
          groups = c("spacing", "size")
        ),
        section_number = list(
          label = "Section number",
          selector = ".section-slide .section-number",
          groups = c("position", "size", "typography", "spacing")
        ),
        section_title = list(
          label = "Section title",
          selector = ".section-slide .section-title",
          groups = c("position", "size", "typography")
        ),
        section_description = list(
          label = "Section description",
          selector = ".section-slide .section-description",
          groups = c("position", "size", "typography")
        ),
        section_progress = list(
          label = "Progress indicator",
          selector = ".section-slide .scv-progress",
          groups = c("position", "size")
        )
      )
    ),
    eio = list(
      label = "Engagement by department (.eio-v2)",
      markup = build_eio_markup,
      components = list(
        eio_slide = list(
          label = "Whole EIO slide",
          selector = ".eio-v2",
          groups = c("spacing", "size")
        ),
        eio_title = list(
          label = "EIO title",
          selector = ".eio-v2 .eio-title",
          groups = c("position", "size", "typography")
        ),
        eio_chart = list(
          label = "EIO chart area",
          selector = ".eio-v2 .eio-chart",
          groups = c("position", "size", "spacing")
        ),
        eio_legend = list(
          label = "EIO legend",
          selector = ".eio-v2 .eio-legend",
          groups = c("position", "size", "typography", "spacing")
        )
      )
    ),
    driver_company = list(
      label = "Driver company (.driver-company-v2)",
      markup = build_driver_company_markup,
      components = list(
        drvco_slide = list(
          label = "Whole driver company slide",
          selector = ".driver-company-v2",
          groups = c("spacing", "size")
        ),
        drvco_title = list(
          label = "Driver company title",
          selector = ".driver-company-v2 .drvco-title",
          groups = c("position", "size", "typography")
        ),
        drvco_grid = list(
          label = "Driver company grid",
          selector = ".driver-company-v2 .drvco-grid",
          groups = c("position", "size", "spacing")
        ),
        drvco_plot = list(
          label = "Driver company plot",
          selector = ".driver-company-v2 .drvco-plot",
          groups = c("position", "size", "spacing")
        )
      )
    ),
    driver_detail = list(
      label = "Driver detail (.driver-detail2-v2)",
      markup = build_driver_detail_markup,
      components = list(
        drvdet_slide = list(
          label = "Whole driver detail slide",
          selector = ".driver-detail2-v2",
          groups = c("spacing", "size")
        ),
        drvdet_title = list(
          label = "Driver detail title",
          selector = ".driver-detail2-v2 .drvdet-title",
          groups = c("position", "size", "typography")
        ),
        drvdet_plot = list(
          label = "Driver detail plot",
          selector = ".driver-detail2-v2 .drvdet-plot-full",
          groups = c("position", "size", "spacing")
        )
      )
    ),
    problematic = list(
      label = "Problematic breakdown (.problematic-breakdown-v2)",
      markup = build_problematic_markup,
      components = list(
        pbv_slide = list(
          label = "Whole problematic slide",
          selector = ".problematic-breakdown-v2",
          groups = c("spacing", "size")
        ),
        pbv_title = list(
          label = "Problematic title",
          selector = ".problematic-breakdown-v2 .pbv-title",
          groups = c("position", "size", "typography")
        ),
        pbv_chart = list(
          label = "Problematic chart",
          selector = ".problematic-breakdown-v2 .pbv-chart",
          groups = c("position", "size", "spacing")
        ),
        pbv_legend = list(
          label = "Problematic legend",
          selector = ".problematic-breakdown-v2 .pbv-legend",
          groups = c("position", "size", "typography", "spacing")
        )
      )
    ),
    multiple_choice = list(
      label = "Multiple choice result (.multiple-results-v2)",
      markup = build_multiple_choice_markup,
      components = list(
        mrv_slide = list(
          label = "Whole multiple choice slide",
          selector = ".multiple-results-v2",
          groups = c("spacing", "size")
        ),
        mrv_title = list(
          label = "Multiple choice title",
          selector = ".multiple-results-v2 .mrv-title",
          groups = c("position", "size", "typography")
        ),
        mrv_chart = list(
          label = "Multiple choice chart",
          selector = ".multiple-results-v2 .mrv-chart",
          groups = c("position", "size", "spacing")
        )
      )
    )
  )
}

get_slide_choices <- function(registry) {
  setNames(names(registry), vapply(registry, function(x) x$label, character(1)))
}

get_component_choices <- function(registry, slide_id) {
  slide <- registry[[slide_id]]
  if (is.null(slide)) {
    return(character(0))
  }
  comps <- slide$components
  setNames(names(comps), vapply(comps, function(x) x$label, character(1)))
}

get_selected_component <- function(registry, slide_id, component_id) {
  slide <- registry[[slide_id]]
  if (is.null(slide)) {
    return(NULL)
  }
  comp <- slide$components[[component_id]]
  if (is.null(comp)) {
    return(NULL)
  }
  list(
    id = component_id,
    slide_id = slide_id,
    slide_label = slide$label,
    label = comp$label,
    selector = comp$selector,
    groups = comp$groups
  )
}

resolve_selection <- function(registry, slide_id = NULL, component_id = NULL) {
  slide_ids <- names(registry)
  safe_slide <- if (!is.null(slide_id) && slide_id %in% slide_ids) slide_id else slide_ids[[1]]

  comp_ids <- names(registry[[safe_slide]]$components)
  safe_component <- if (!is.null(component_id) && component_id %in% comp_ids) component_id else comp_ids[[1]]

  list(slide_id = safe_slide, component_id = safe_component)
}

default_override <- function(component) {
  list(
    selector = component$selector,
    slide_id = component$slide_id,
    slide_label = component$slide_label,
    component_id = component$id,
    component_label = component$label,
    groups = component$groups,
    position_mode = "none",
    x = 0,
    y = 0,
    enable_z_index = FALSE,
    z_index = 1,
    enable_width = FALSE,
    width = NA_real_,
    enable_height = FALSE,
    height = NA_real_,
    enable_max_width = FALSE,
    max_width = NA_real_,
    enable_min_height = FALSE,
    min_height = NA_real_,
    enable_font_size = FALSE,
    font_size = NA_real_,
    enable_line_height = FALSE,
    line_height = NA_real_,
    enable_letter_spacing = FALSE,
    letter_spacing = NA_real_,
    enable_font_weight = FALSE,
    font_weight = "normal",
    enable_text_align = FALSE,
    text_align = "left",
    enable_margin = FALSE,
    margin_top = 0,
    margin_right = 0,
    margin_bottom = 0,
    margin_left = 0,
    enable_padding = FALSE,
    padding_top = 0,
    padding_right = 0,
    padding_bottom = 0,
    padding_left = 0,
    enable_gap = FALSE,
    gap = NA_real_,
    enable_column_gap = FALSE,
    column_gap = NA_real_,
    enable_row_gap = FALSE,
    row_gap = NA_real_
  )
}

merge_override <- function(defaults, incoming) {
  out <- defaults
  if (!is.list(incoming)) {
    return(out)
  }
  for (nm in names(defaults)) {
    if (!is.null(incoming[[nm]])) {
      out[[nm]] <- incoming[[nm]]
    }
  }
  out
}

get_component_override <- function(overrides, component) {
  defaults <- default_override(component)
  incoming <- overrides[[component$id]]
  merge_override(defaults, incoming)
}

reset_override_group <- function(override, group) {
  ov <- override
  if (identical(group, "position")) {
    ov$position_mode <- "none"
    ov$x <- 0
    ov$y <- 0
    ov$enable_z_index <- FALSE
    ov$z_index <- 1
  } else if (identical(group, "size")) {
    ov$enable_width <- FALSE
    ov$width <- NA_real_
    ov$enable_height <- FALSE
    ov$height <- NA_real_
    ov$enable_max_width <- FALSE
    ov$max_width <- NA_real_
    ov$enable_min_height <- FALSE
    ov$min_height <- NA_real_
  } else if (identical(group, "typography")) {
    ov$enable_font_size <- FALSE
    ov$font_size <- NA_real_
    ov$enable_line_height <- FALSE
    ov$line_height <- NA_real_
    ov$enable_letter_spacing <- FALSE
    ov$letter_spacing <- NA_real_
    ov$enable_font_weight <- FALSE
    ov$font_weight <- "normal"
    ov$enable_text_align <- FALSE
    ov$text_align <- "left"
  } else if (identical(group, "spacing")) {
    ov$enable_margin <- FALSE
    ov$margin_top <- 0
    ov$margin_right <- 0
    ov$margin_bottom <- 0
    ov$margin_left <- 0
    ov$enable_padding <- FALSE
    ov$padding_top <- 0
    ov$padding_right <- 0
    ov$padding_bottom <- 0
    ov$padding_left <- 0
    ov$enable_gap <- FALSE
    ov$gap <- NA_real_
    ov$enable_column_gap <- FALSE
    ov$column_gap <- NA_real_
    ov$enable_row_gap <- FALSE
    ov$row_gap <- NA_real_
  }
  ov
}

collect_css_properties <- function(override) {
  props <- character(0)
  groups <- override$groups %||% c("position", "size", "typography", "spacing")

  if ("position" %in% groups) {
    mode <- trim_or_empty(override$position_mode)
    if (mode %in% c("relative", "absolute")) {
      props <- c(props, sprintf("position: %s;", mode))
    }

    x <- coerce_num(override$x, 0)
    y <- coerce_num(override$y, 0)
    if (abs(x) > 1e-8 || abs(y) > 1e-8) {
      props <- c(
        props,
        sprintf(
          "transform: translate(%spx, %spx);",
          format_css_value(x),
          format_css_value(y)
        )
      )
    }

    if (isTRUE(override$enable_z_index)) {
      z <- coerce_int(override$z_index, 1L)
      props <- c(props, sprintf("z-index: %d;", z))
    }
  }

  if ("size" %in% groups) {
    if (isTRUE(override$enable_width) && !is.na(override$width)) {
      props <- c(props, sprintf("width: %spx;", format_css_value(override$width)))
    }
    if (isTRUE(override$enable_height) && !is.na(override$height)) {
      props <- c(props, sprintf("height: %spx;", format_css_value(override$height)))
    }
    if (isTRUE(override$enable_max_width) && !is.na(override$max_width)) {
      props <- c(props, sprintf("max-width: %spx;", format_css_value(override$max_width)))
    }
    if (isTRUE(override$enable_min_height) && !is.na(override$min_height)) {
      props <- c(props, sprintf("min-height: %spx;", format_css_value(override$min_height)))
    }
  }

  if ("typography" %in% groups) {
    if (isTRUE(override$enable_font_size) && !is.na(override$font_size)) {
      props <- c(props, sprintf("font-size: %spx;", format_css_value(override$font_size)))
    }
    if (isTRUE(override$enable_line_height) && !is.na(override$line_height)) {
      props <- c(props, sprintf("line-height: %s;", format_css_value(override$line_height)))
    }
    if (isTRUE(override$enable_letter_spacing) && !is.na(override$letter_spacing)) {
      props <- c(
        props,
        sprintf("letter-spacing: %spx;", format_css_value(override$letter_spacing))
      )
    }
    if (isTRUE(override$enable_font_weight) && nzchar(trim_or_empty(override$font_weight))) {
      props <- c(props, sprintf("font-weight: %s;", trim_or_empty(override$font_weight)))
    }
    if (isTRUE(override$enable_text_align) && nzchar(trim_or_empty(override$text_align))) {
      props <- c(props, sprintf("text-align: %s;", trim_or_empty(override$text_align)))
    }
  }

  if ("spacing" %in% groups) {
    if (isTRUE(override$enable_margin)) {
      props <- c(props, sprintf("margin-top: %spx;", format_css_value(override$margin_top %||% 0)))
      props <- c(props, sprintf("margin-right: %spx;", format_css_value(override$margin_right %||% 0)))
      props <- c(props, sprintf("margin-bottom: %spx;", format_css_value(override$margin_bottom %||% 0)))
      props <- c(props, sprintf("margin-left: %spx;", format_css_value(override$margin_left %||% 0)))
    }

    if (isTRUE(override$enable_padding)) {
      props <- c(props, sprintf("padding-top: %spx;", format_css_value(override$padding_top %||% 0)))
      props <- c(props, sprintf("padding-right: %spx;", format_css_value(override$padding_right %||% 0)))
      props <- c(props, sprintf("padding-bottom: %spx;", format_css_value(override$padding_bottom %||% 0)))
      props <- c(props, sprintf("padding-left: %spx;", format_css_value(override$padding_left %||% 0)))
    }

    if (isTRUE(override$enable_gap) && !is.na(override$gap)) {
      props <- c(props, sprintf("gap: %spx;", format_css_value(override$gap)))
    }
    if (isTRUE(override$enable_column_gap) && !is.na(override$column_gap)) {
      props <- c(props, sprintf("column-gap: %spx;", format_css_value(override$column_gap)))
    }
    if (isTRUE(override$enable_row_gap) && !is.na(override$row_gap)) {
      props <- c(props, sprintf("row-gap: %spx;", format_css_value(override$row_gap)))
    }
  }

  props
}

build_css_block <- function(override) {
  selector <- trim_or_empty(override$selector)
  if (!nzchar(selector)) {
    return("")
  }

  props <- collect_css_properties(override)
  if (!length(props)) {
    return("")
  }

  paste(
    sprintf("%s {", selector),
    paste0("  ", props, collapse = "\n"),
    "}",
    sep = "\n"
  )
}

is_override_empty <- function(override) {
  !length(collect_css_properties(override))
}

build_override_css <- function(overrides, registry, token_overrides, manual_css = "") {
  lines <- c(
    "/* =========================================================",
    "   Generated style overrides",
    "   Source: Shiny style editor",
    sprintf("   Updated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "   Base CSS is not modified.",
    "   ========================================================= */",
    ""
  )

  has_content <- FALSE

  token_css <- build_token_css(token_overrides)
  if (nzchar(token_css)) {
    lines <- c(
      lines,
      "/* ---------- Global tokens ---------- */",
      token_css,
      ""
    )
    has_content <- TRUE
  }

  for (slide_id in names(registry)) {
    slide_meta <- registry[[slide_id]]
    slide_lines <- character(0)

    for (component_id in names(slide_meta$components)) {
      component <- get_selected_component(registry, slide_id, component_id)
      current <- overrides[[component_id]]
      if (is.null(current)) {
        next
      }

      merged <- merge_override(default_override(component), current)
      block <- build_css_block(merged)
      if (!nzchar(block)) {
        next
      }

      slide_lines <- c(
        slide_lines,
        sprintf("/* Component: %s */", component$label),
        block,
        ""
      )
    }

    if (length(slide_lines)) {
      lines <- c(
        lines,
        sprintf("/* ---------- %s ---------- */", slide_meta$label),
        slide_lines
      )
      has_content <- TRUE
    }
  }

  manual_css <- trim_or_empty(manual_css)
  if (nzchar(manual_css)) {
    lines <- c(
      lines,
      "/* ---------- Manual overrides ---------- */",
      manual_css,
      ""
    )
    has_content <- TRUE
  }

  if (!has_content) {
    lines <- c(lines, "/* No generated overrides. */")
  }

  paste(lines, collapse = "\n")
}

save_state <- function(path, state_data) {
  jsonlite::write_json(
    x = state_data,
    path = path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  invisible(TRUE)
}

load_state <- function(path, default_tokens) {
  out <- list(
    version = 1,
    overrides = list(),
    manual_css = "",
    tokens = default_tokens,
    selection = list(slide_id = NULL, component_id = NULL)
  )

  if (!file.exists(path)) {
    return(out)
  }

  parsed <- tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (is.null(parsed)) {
    return(out)
  }

  out$overrides <- if (is.list(parsed$overrides)) parsed$overrides else list()
  out$manual_css <- as.character(parsed$manual_css %||% "")
  out$tokens <- merge_tokens(default_tokens, parsed$tokens)

  if (is.list(parsed$selection)) {
    out$selection$slide_id <- parsed$selection$slide_id %||% NULL
    out$selection$component_id <- parsed$selection$component_id %||% NULL
  }

  out
}

list_preset_choices <- function(preset_dir) {
  files <- sort(list.files(preset_dir, pattern = "\\.json$", full.names = FALSE))
  if (!length(files)) {
    return(character(0))
  }
  setNames(files, tools::file_path_sans_ext(files))
}

save_preset <- function(preset_dir, preset_name, state_data) {
  clean <- sanitize_preset_name(preset_name)
  if (!nzchar(clean)) {
    stop("Preset name is empty.")
  }
  preset_path <- file.path(preset_dir, sprintf("%s.json", clean))
  save_state(preset_path, state_data)
  preset_path
}

load_preset <- function(preset_path, default_tokens) {
  load_state(preset_path, default_tokens)
}

build_preview_css <- function(debug_boxes = FALSE, selected_component_id = NULL) {
  css <- c(
    "body { background: #f4f4f1; }",
    ".panel-block {",
    "  background: #ffffff;",
    "  border: 1px solid #dedbd2;",
    "  border-radius: 10px;",
    "  padding: 14px 16px;",
    "  margin-bottom: 14px;",
    "  box-shadow: 0 8px 24px rgba(0, 0, 0, 0.05);",
    "}",
    ".panel-block h3 { margin-top: 0; }",
    ".compact-help { color: #555; font-size: 12px; margin-bottom: 10px; }",
    ".preview-shell { overflow: auto; background: #ece9df; border-radius: 12px; padding: 12px; }",
    ".preview-stage { position: relative; overflow: hidden; background: #e9e6de; border-radius: 8px; }",
    ".preview-canvas { position: relative; width: 1920px; height: 1080px; transform-origin: top left; }",
    ".preview-canvas .slide {",
    "  width: 1920px;",
    "  height: 1080px;",
    "  position: relative;",
    "  overflow: hidden;",
    "  box-sizing: border-box;",
    "}",
    ".preview-guides { position: absolute; inset: 0; pointer-events: none; z-index: 9999; }",
    ".preview-guides .guide-v-center {",
    "  position: absolute; top: 0; bottom: 0; left: 960px; width: 1px;",
    "  background: rgba(255, 103, 170, 0.65);",
    "}",
    ".preview-guides .guide-h-center {",
    "  position: absolute; left: 0; right: 0; top: 540px; height: 1px;",
    "  background: rgba(99, 232, 198, 0.65);",
    "}",
    ".preview-guides .guide-safe-area {",
    "  position: absolute; top: 64px; right: 80px; bottom: 64px; left: 80px;",
    "  border: 2px dashed rgba(30, 30, 30, 0.28);",
    "}",
    ".chart-placeholder {",
    "  border: 2px dashed rgba(0, 0, 0, 0.35);",
    "  border-radius: 8px;",
    "  min-height: 260px;",
    "  display: flex;",
    "  align-items: center;",
    "  justify-content: center;",
    "  color: #404040;",
    "  font-size: 30px;",
    "  background: rgba(255, 255, 255, 0.7);",
    "}",
    ".legend-item { display: inline-flex; align-items: center; margin-right: 16px; font-size: 24px; }",
    ".legend-item .swatch { width: 20px; height: 20px; margin-right: 8px; display: inline-block; border-radius: 3px; }",
    ".legend-item .swatch.teal { background: #63e8c6; }",
    ".legend-item .swatch.grey { background: #a5a5a5; }",
    ".legend-item .swatch.pink { background: #ff8ade; }",
    ".brand-placeholder {",
    "  position: absolute;",
    "  left: 48px;",
    "  bottom: 40px;",
    "  min-width: 180px;",
    "  min-height: 56px;",
    "  padding: 10px 14px;",
    "  border: 2px solid #000;",
    "  display: flex;",
    "  align-items: center;",
    "  justify-content: center;",
    "  font-size: 26px;",
    "  font-weight: 700;",
    "  background: #fff;",
    "}",
    ".css-output { white-space: pre-wrap; min-height: 280px; margin: 0; font-family: Consolas, monospace; }"
  )

  if (isTRUE(debug_boxes)) {
    css <- c(
      css,
      "[data-component-id] {",
      "  outline: 2px dashed rgba(255, 103, 170, 0.85);",
      "  outline-offset: 2px;",
      "}"
    )
  }

  selected_component_id <- trim_or_empty(selected_component_id)
  if (nzchar(selected_component_id)) {
    css <- c(
      css,
      sprintf(
        "[data-component-id='%s'] { outline: 3px solid rgba(99, 232, 198, 0.95) !important; outline-offset: 3px; box-shadow: 0 0 0 2px rgba(0, 0, 0, 0.18); }",
        selected_component_id
      )
    )
  }

  paste(css, collapse = "\n")
}

build_preview_markup <- function(registry, slide_id, zoom = 0.33, show_guides = TRUE) {
  slide <- registry[[slide_id]]
  if (is.null(slide)) {
    return("<div>Slide not found.</div>")
  }

  slide_markup <- if (is.function(slide$markup)) slide$markup() else as.character(slide$markup)
  zoom <- max(0.15, min(1.0, coerce_num(zoom, 0.33)))
  stage_w <- round(1920 * zoom)
  stage_h <- round(1080 * zoom)

  guides <- ""
  if (isTRUE(show_guides)) {
    guides <- paste(
      '<div class="preview-guides">',
      '  <div class="guide-v-center"></div>',
      '  <div class="guide-h-center"></div>',
      '  <div class="guide-safe-area"></div>',
      "</div>",
      sep = "\n"
    )
  }

  paste(
    sprintf('<div class="preview-shell"><div class="preview-stage" style="width:%dpx; height:%dpx;">', stage_w, stage_h),
    sprintf('<div class="preview-canvas" style="transform: scale(%s);">', format_css_value(zoom, digits = 3)),
    guides,
    slide_markup,
    "</div></div></div>",
    sep = "\n"
  )
}

input_to_override <- function(input, component) {
  ov <- default_override(component)
  ov$position_mode <- trim_or_empty(input$position_mode %||% "none")
  ov$x <- coerce_num(input$x, 0)
  ov$y <- coerce_num(input$y, 0)
  ov$enable_z_index <- isTRUE(input$enable_z_index)
  ov$z_index <- coerce_int(input$z_index, 1L)

  ov$enable_width <- isTRUE(input$enable_width)
  ov$width <- coerce_num(input$width, NA_real_)
  ov$enable_height <- isTRUE(input$enable_height)
  ov$height <- coerce_num(input$height, NA_real_)
  ov$enable_max_width <- isTRUE(input$enable_max_width)
  ov$max_width <- coerce_num(input$max_width, NA_real_)
  ov$enable_min_height <- isTRUE(input$enable_min_height)
  ov$min_height <- coerce_num(input$min_height, NA_real_)

  ov$enable_font_size <- isTRUE(input$enable_font_size)
  ov$font_size <- coerce_num(input$font_size, NA_real_)
  ov$enable_line_height <- isTRUE(input$enable_line_height)
  ov$line_height <- coerce_num(input$line_height, NA_real_)
  ov$enable_letter_spacing <- isTRUE(input$enable_letter_spacing)
  ov$letter_spacing <- coerce_num(input$letter_spacing, NA_real_)
  ov$enable_font_weight <- isTRUE(input$enable_font_weight)
  ov$font_weight <- trim_or_empty(input$font_weight %||% "normal")
  ov$enable_text_align <- isTRUE(input$enable_text_align)
  ov$text_align <- trim_or_empty(input$text_align %||% "left")

  ov$enable_margin <- isTRUE(input$enable_margin)
  ov$margin_top <- coerce_num(input$margin_top, 0)
  ov$margin_right <- coerce_num(input$margin_right, 0)
  ov$margin_bottom <- coerce_num(input$margin_bottom, 0)
  ov$margin_left <- coerce_num(input$margin_left, 0)

  ov$enable_padding <- isTRUE(input$enable_padding)
  ov$padding_top <- coerce_num(input$padding_top, 0)
  ov$padding_right <- coerce_num(input$padding_right, 0)
  ov$padding_bottom <- coerce_num(input$padding_bottom, 0)
  ov$padding_left <- coerce_num(input$padding_left, 0)

  ov$enable_gap <- isTRUE(input$enable_gap)
  ov$gap <- coerce_num(input$gap, NA_real_)
  ov$enable_column_gap <- isTRUE(input$enable_column_gap)
  ov$column_gap <- coerce_num(input$column_gap, NA_real_)
  ov$enable_row_gap <- isTRUE(input$enable_row_gap)
  ov$row_gap <- coerce_num(input$row_gap, NA_real_)

  ov
}

update_component_inputs <- function(session, override) {
  updateSelectInput(session, "position_mode", selected = override$position_mode %||% "none")
  updateSliderInput(session, "x", value = coerce_num(override$x, 0))
  updateSliderInput(session, "y", value = coerce_num(override$y, 0))
  updateCheckboxInput(session, "enable_z_index", value = isTRUE(override$enable_z_index))
  updateSliderInput(session, "z_index", value = coerce_int(override$z_index, 1L))

  updateCheckboxInput(session, "enable_width", value = isTRUE(override$enable_width))
  updateNumericInput(session, "width", value = override$width %||% NA_real_)
  updateCheckboxInput(session, "enable_height", value = isTRUE(override$enable_height))
  updateNumericInput(session, "height", value = override$height %||% NA_real_)
  updateCheckboxInput(session, "enable_max_width", value = isTRUE(override$enable_max_width))
  updateNumericInput(session, "max_width", value = override$max_width %||% NA_real_)
  updateCheckboxInput(session, "enable_min_height", value = isTRUE(override$enable_min_height))
  updateNumericInput(session, "min_height", value = override$min_height %||% NA_real_)

  updateCheckboxInput(session, "enable_font_size", value = isTRUE(override$enable_font_size))
  updateNumericInput(session, "font_size", value = override$font_size %||% NA_real_)
  updateCheckboxInput(session, "enable_line_height", value = isTRUE(override$enable_line_height))
  updateNumericInput(session, "line_height", value = override$line_height %||% NA_real_)
  updateCheckboxInput(session, "enable_letter_spacing", value = isTRUE(override$enable_letter_spacing))
  updateNumericInput(session, "letter_spacing", value = override$letter_spacing %||% NA_real_)
  updateCheckboxInput(session, "enable_font_weight", value = isTRUE(override$enable_font_weight))
  updateSelectInput(session, "font_weight", selected = override$font_weight %||% "normal")
  updateCheckboxInput(session, "enable_text_align", value = isTRUE(override$enable_text_align))
  updateSelectInput(session, "text_align", selected = override$text_align %||% "left")

  updateCheckboxInput(session, "enable_margin", value = isTRUE(override$enable_margin))
  updateNumericInput(session, "margin_top", value = override$margin_top %||% 0)
  updateNumericInput(session, "margin_right", value = override$margin_right %||% 0)
  updateNumericInput(session, "margin_bottom", value = override$margin_bottom %||% 0)
  updateNumericInput(session, "margin_left", value = override$margin_left %||% 0)

  updateCheckboxInput(session, "enable_padding", value = isTRUE(override$enable_padding))
  updateNumericInput(session, "padding_top", value = override$padding_top %||% 0)
  updateNumericInput(session, "padding_right", value = override$padding_right %||% 0)
  updateNumericInput(session, "padding_bottom", value = override$padding_bottom %||% 0)
  updateNumericInput(session, "padding_left", value = override$padding_left %||% 0)

  updateCheckboxInput(session, "enable_gap", value = isTRUE(override$enable_gap))
  updateNumericInput(session, "gap", value = override$gap %||% NA_real_)
  updateCheckboxInput(session, "enable_column_gap", value = isTRUE(override$enable_column_gap))
  updateNumericInput(session, "column_gap", value = override$column_gap %||% NA_real_)
  updateCheckboxInput(session, "enable_row_gap", value = isTRUE(override$enable_row_gap))
  updateNumericInput(session, "row_gap", value = override$row_gap %||% NA_real_)
}

working_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

if (
  file.exists(file.path(working_dir, "codex", "style-editor-prototype", "app.R")) &&
  file.exists(file.path(working_dir, "styles_editable.css"))
) {
  project_root <- working_dir
  app_dir <- normalizePath(
    file.path(project_root, "codex", "style-editor-prototype"),
    winslash = "/",
    mustWork = TRUE
  )
} else {
  app_dir <- working_dir
  project_root <- normalizePath(file.path(app_dir, "..", ".."), winslash = "/", mustWork = TRUE)
}

generated_dir <- file.path(project_root, "generated")
preset_dir <- file.path(generated_dir, "presets")
override_css_path <- file.path(generated_dir, "style-overrides.css")
state_path <- file.path(generated_dir, "style-editor-state.json")

ensure_generated_dirs(generated_dir, preset_dir)

base_css_candidates <- c(
  file.path(project_root, "styles_editable.css"),
  file.path(project_root, "styles-base.css")
)
base_css_path <- base_css_candidates[file.exists(base_css_candidates)][1]
base_css_text <- if (!is.na(base_css_path)) read_text_file(base_css_path) else ""

ai_css_path <- file.path(project_root, "styles-ai.css")
ai_css_text <- if (file.exists(ai_css_path)) read_text_file(ai_css_path) else ""

registry <- create_component_registry()
token_defaults <- default_global_tokens()
loaded_state <- load_state(state_path, token_defaults)
selection <- resolve_selection(
  registry = registry,
  slide_id = loaded_state$selection$slide_id,
  component_id = loaded_state$selection$component_id
)

initial_component <- get_selected_component(registry, selection$slide_id, selection$component_id)
initial_override <- get_component_override(loaded_state$overrides, initial_component)
initial_preset_choices <- list_preset_choices(preset_dir)
qmd_choices <- list_qmd_choices(project_root)
initial_qmd <- choose_default_qmd(qmd_choices)

quarto_preview_prefix <- "quarto_preview_files"
if (quarto_preview_prefix %in% names(resourcePaths())) {
  suppressWarnings(removeResourcePath(quarto_preview_prefix))
}
addResourcePath(quarto_preview_prefix, project_root)

ui <- fluidPage(
  tags$head(
    tags$title("Slide style editor"),
    tags$style(
      HTML(
        "
        body { background: #f4f4f1; }
        .app-title { margin: 0 0 4px 0; font-size: 28px; font-weight: 700; }
        .app-subtitle { margin: 0; color: #505050; max-width: 980px; }
        .nudge-grid { display: grid; grid-template-columns: repeat(3, 44px); gap: 6px; justify-content: start; margin-top: 6px; }
        .nudge-grid .btn { padding: 6px 0; }
        .code-path { font-family: Consolas, monospace; font-size: 12px; color: #444; }
        .status-ok { color: #1f6d3d; font-size: 12px; }
        .status-error { color: #8e1d1d; font-size: 12px; }
        .small-label { font-size: 12px; color: #555; margin-top: 6px; margin-bottom: 2px; }
        "
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      div(
        class = "panel-block",
        h1(class = "app-title", "Component style editor"),
        p(
          class = "app-subtitle",
          "Safe pipeline: source CSS stays untouched. This app writes only generated/style-overrides.css and state JSON."
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 4,
      div(
        class = "panel-block",
        h3("Selection"),
        selectInput("slide_id", "Slide", choices = get_slide_choices(registry), selected = selection$slide_id),
        selectInput(
          "component_id",
          "Component",
          choices = get_component_choices(registry, selection$slide_id),
          selected = selection$component_id
        ),
        uiOutput("component_meta")
      ),
      div(
        class = "panel-block",
        h3("Position"),
        p(class = "compact-help", "Use translate x/y for safe nudging before absolute positioning."),
        selectInput(
          "position_mode",
          "Position mode",
          choices = c("No explicit position" = "none", "relative" = "relative", "absolute" = "absolute"),
          selected = initial_override$position_mode
        ),
        sliderInput("x", "X translate (px)", min = -400, max = 400, value = coerce_num(initial_override$x, 0), step = 1),
        sliderInput("y", "Y translate (px)", min = -400, max = 400, value = coerce_num(initial_override$y, 0), step = 1),
        checkboxInput("enable_z_index", "Override z-index", value = isTRUE(initial_override$enable_z_index)),
        sliderInput("z_index", "z-index", min = 0, max = 100, value = coerce_int(initial_override$z_index, 1L), step = 1),
        numericInput("nudge_step", "Nudge step", value = 2, min = 1, max = 50, step = 1),
        div(
          class = "nudge-grid",
          div(),
          actionButton("nudge_up", "\u2191"),
          div(),
          actionButton("nudge_left", "\u2190"),
          actionButton("nudge_down", "\u2193"),
          actionButton("nudge_right", "\u2192"),
          actionButton("nudge_up_10", "\u2191x10"),
          actionButton("nudge_down_10", "\u2193x10"),
          actionButton("nudge_right_10", "\u2192x10")
        )
      ),
      div(
        class = "panel-block",
        h3("Size"),
        checkboxInput("enable_width", "Override width", value = isTRUE(initial_override$enable_width)),
        numericInput("width", "Width (px)", value = initial_override$width %||% NA_real_, min = 50, max = 1920, step = 1),
        checkboxInput("enable_height", "Override height", value = isTRUE(initial_override$enable_height)),
        numericInput("height", "Height (px)", value = initial_override$height %||% NA_real_, min = 20, max = 1080, step = 1),
        checkboxInput("enable_max_width", "Override max-width", value = isTRUE(initial_override$enable_max_width)),
        numericInput("max_width", "Max width (px)", value = initial_override$max_width %||% NA_real_, min = 50, max = 1920, step = 1),
        checkboxInput("enable_min_height", "Override min-height", value = isTRUE(initial_override$enable_min_height)),
        numericInput("min_height", "Min height (px)", value = initial_override$min_height %||% NA_real_, min = 0, max = 1080, step = 1)
      ),
      div(
        class = "panel-block",
        h3("Typography"),
        checkboxInput("enable_font_size", "Override font-size", value = isTRUE(initial_override$enable_font_size)),
        numericInput("font_size", "Font size (px)", value = initial_override$font_size %||% NA_real_, min = 8, max = 220, step = 1),
        checkboxInput("enable_line_height", "Override line-height", value = isTRUE(initial_override$enable_line_height)),
        numericInput("line_height", "Line-height", value = initial_override$line_height %||% NA_real_, min = 0.6, max = 3.0, step = 0.01),
        checkboxInput("enable_letter_spacing", "Override letter-spacing", value = isTRUE(initial_override$enable_letter_spacing)),
        numericInput("letter_spacing", "Letter-spacing (px)", value = initial_override$letter_spacing %||% NA_real_, min = -5, max = 20, step = 0.1),
        checkboxInput("enable_font_weight", "Override font-weight", value = isTRUE(initial_override$enable_font_weight)),
        selectInput("font_weight", "Font weight", choices = c("normal", "500", "600", "700", "800"), selected = initial_override$font_weight %||% "normal"),
        checkboxInput("enable_text_align", "Override text-align", value = isTRUE(initial_override$enable_text_align)),
        selectInput("text_align", "Text align", choices = c("left", "center", "right", "justify"), selected = initial_override$text_align %||% "left")
      ),
      div(
        class = "panel-block",
        h3("Spacing"),
        checkboxInput("enable_margin", "Override margins", value = isTRUE(initial_override$enable_margin)),
        numericInput("margin_top", "Margin top (px)", value = initial_override$margin_top %||% 0, min = -400, max = 400, step = 1),
        numericInput("margin_right", "Margin right (px)", value = initial_override$margin_right %||% 0, min = -400, max = 400, step = 1),
        numericInput("margin_bottom", "Margin bottom (px)", value = initial_override$margin_bottom %||% 0, min = -400, max = 400, step = 1),
        numericInput("margin_left", "Margin left (px)", value = initial_override$margin_left %||% 0, min = -400, max = 400, step = 1),
        checkboxInput("enable_padding", "Override paddings", value = isTRUE(initial_override$enable_padding)),
        numericInput("padding_top", "Padding top (px)", value = initial_override$padding_top %||% 0, min = -200, max = 400, step = 1),
        numericInput("padding_right", "Padding right (px)", value = initial_override$padding_right %||% 0, min = -200, max = 400, step = 1),
        numericInput("padding_bottom", "Padding bottom (px)", value = initial_override$padding_bottom %||% 0, min = -200, max = 400, step = 1),
        numericInput("padding_left", "Padding left (px)", value = initial_override$padding_left %||% 0, min = -200, max = 400, step = 1),
        checkboxInput("enable_gap", "Override gap", value = isTRUE(initial_override$enable_gap)),
        numericInput("gap", "Gap (px)", value = initial_override$gap %||% NA_real_, min = 0, max = 200, step = 1),
        checkboxInput("enable_column_gap", "Override column-gap", value = isTRUE(initial_override$enable_column_gap)),
        numericInput("column_gap", "Column gap (px)", value = initial_override$column_gap %||% NA_real_, min = 0, max = 200, step = 1),
        checkboxInput("enable_row_gap", "Override row-gap", value = isTRUE(initial_override$enable_row_gap)),
        numericInput("row_gap", "Row gap (px)", value = initial_override$row_gap %||% NA_real_, min = 0, max = 200, step = 1)
      ),
      div(
        class = "panel-block",
        h3("Reset and presets"),
        actionButton("reset_component", "Reset selected component"),
        br(), br(),
        selectInput("reset_group", "Reset property group", choices = c("position", "size", "typography", "spacing"), selected = "position"),
        actionButton("reset_group_btn", "Reset selected group"),
        br(), br(),
        actionButton("reset_all", "Reset all overrides"),
        hr(),
        textInput("preset_name", "Preset name", placeholder = "example-layout-v1"),
        actionButton("save_preset", "Save preset"),
        br(), br(),
        selectInput("preset_to_load", "Load preset", choices = initial_preset_choices),
        actionButton("load_preset", "Load selected preset")
      ),
      div(
        class = "panel-block",
        h3("Global tokens (optional)"),
        checkboxInput("enable_tokens", "Enable :root token overrides", value = isTRUE(loaded_state$tokens$enabled)),
        textInput("token_accent", "Accent", value = loaded_state$tokens$accent),
        textInput("token_accent_light", "Accent light", value = loaded_state$tokens$accent_light),
        textInput("token_accent_pink", "Accent pink", value = loaded_state$tokens$accent_pink),
        textInput("token_accent_pink_strong", "Accent pink strong", value = loaded_state$tokens$accent_pink_strong),
        textInput("token_yellow", "Yellow", value = loaded_state$tokens$yellow),
        textInput("token_yellow_light", "Yellow light", value = loaded_state$tokens$yellow_light),
        textInput("token_light_grey", "Light grey", value = loaded_state$tokens$light_grey),
        textInput("token_normal_grey", "Normal grey", value = loaded_state$tokens$normal_grey),
        textInput("token_page_bg", "Page background", value = loaded_state$tokens$page_bg),
        textInput("token_black", "Black", value = loaded_state$tokens$black)
      )
    ),
    column(
      width = 8,
      div(
        class = "panel-block",
        h3("Preview"),
        selectInput(
          "preview_mode",
          "Preview mode",
          choices = c("Mock preview", "Real Quarto preview"),
          selected = "Mock preview"
        ),
        selectInput(
          "qmd_file",
          "Quarto document",
          choices = qmd_choices,
          selected = initial_qmd
        ),
        checkboxInput("auto_render_quarto", "Auto-render after changes", value = FALSE),
        actionButton("render_quarto", "Render Quarto preview"),
        uiOutput("qmd_css_warning"),
        textOutput("quarto_render_status"),
        textOutput("quarto_last_render"),
        sliderInput("preview_zoom", "Preview zoom", min = 0.15, max = 1.0, value = 0.33, step = 0.01),
        checkboxInput("debug_boxes", "Show element outlines", value = FALSE),
        checkboxInput("show_guides", "Show center / safe-area guides", value = TRUE),
        uiOutput("preview_ui"),
        uiOutput("quarto_preview_link"),
        div(class = "small-label", "Quarto render log"),
        tags$pre(class = "css-output", textOutput("quarto_log")),
        div(class = "small-label", "Override CSS output path"),
        div(class = "code-path", "generated/style-overrides.css"),
        textOutput("write_status", container = span)
      ),
      div(
        class = "panel-block",
        h3("Generated CSS"),
        tags$pre(class = "css-output", textOutput("css_text"))
      ),
      div(
        class = "panel-block",
        h3("Manual CSS overrides"),
        p(class = "compact-help", "Advanced one-off overrides appended at the end of generated/style-overrides.css."),
        textAreaInput("manual_css", "Manual CSS overrides", rows = 12, value = loaded_state$manual_css)
      )
    )
  )
)

server <- function(input, output, session) {
  state <- reactiveValues(
    overrides = loaded_state$overrides,
    manual_css = loaded_state$manual_css,
    tokens = loaded_state$tokens,
    selected_slide = selection$slide_id,
    selected_component = selection$component_id
  )

  write_status_rv <- reactiveVal("")
  quarto_html_path_rv <- reactiveVal("")
  quarto_log_rv <- reactiveVal("No Quarto render yet.")
  quarto_status_rv <- reactiveVal("Quarto preview is ready. Select a .qmd and click Render Quarto preview.")
  quarto_last_render_rv <- reactiveVal("")

  do_quarto_render <- function(source_label = "Manual") {
    qmd_rel <- trim_or_empty(input$qmd_file)
    if (!nzchar(qmd_rel)) {
      quarto_status_rv("No Quarto document selected.")
      return(invisible(NULL))
    }

    quarto_status_rv(sprintf("%s render in progress for %s ...", source_label, qmd_rel))
    result <- render_quarto_file(project_root = project_root, qmd_rel = qmd_rel)

    if (length(result$log)) {
      quarto_log_rv(paste(result$log, collapse = "\n"))
    } else {
      quarto_log_rv("")
    }

    if (isTRUE(result$success)) {
      quarto_html_path_rv(result$html_path)
      quarto_status_rv(result$message)
      quarto_last_render_rv(sprintf("Last render: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    } else {
      if (nzchar(trim_or_empty(result$html_path)) && file.exists(result$html_path)) {
        quarto_html_path_rv(result$html_path)
      }
      quarto_status_rv(result$message)
      quarto_last_render_rv(sprintf("Last render attempt: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    }
  }

  selected_component <- reactive({
    slide_id <- input$slide_id %||% state$selected_slide
    component_id <- input$component_id %||% state$selected_component
    get_selected_component(registry, slide_id, component_id)
  })

  output$component_meta <- renderUI({
    comp <- selected_component()
    if (is.null(comp)) {
      return(tags$div("No component selected."))
    }
    tags$div(
      tags$div(tags$strong(comp$label)),
      tags$div(class = "code-path", comp$selector),
      tags$div(class = "small-label", sprintf("Editable groups: %s", paste(comp$groups, collapse = ", ")))
    )
  })

  output$qmd_css_warning <- renderUI({
    qmd_rel <- trim_or_empty(input$qmd_file)
    if (!nzchar(qmd_rel)) {
      return(tags$div(class = "status-error", "No .qmd file available for real preview."))
    }

    qmd_path <- file.path(project_root, qmd_rel)
    if (!file.exists(qmd_path)) {
      return(tags$div(class = "status-error", sprintf("Selected file does not exist: %s", qmd_rel)))
    }

    includes_override <- qmd_includes_override_css(
      qmd_path = qmd_path,
      override_rel = "generated/style-overrides.css"
    )

    if (!isTRUE(includes_override)) {
      return(
        tags$div(
          class = "status-error",
          "Warning: this .qmd YAML does not include generated/style-overrides.css after the base stylesheet."
        )
      )
    }

    tags$div(class = "status-ok", "YAML check: generated/style-overrides.css detected.")
  })

  observeEvent(input$qmd_file, {
    qmd_rel <- trim_or_empty(input$qmd_file)
    if (!nzchar(qmd_rel)) {
      quarto_html_path_rv("")
      return()
    }

    qmd_path <- file.path(project_root, qmd_rel)
    if (!file.exists(qmd_path)) {
      quarto_html_path_rv("")
      return()
    }

    expected_html <- guess_qmd_output_html(qmd_path)
    if (file.exists(expected_html)) {
      quarto_html_path_rv(expected_html)
      quarto_status_rv(sprintf("Loaded existing render output for %s.", qmd_rel))
    } else {
      quarto_status_rv(sprintf("No existing HTML found for %s. Click Render Quarto preview.", qmd_rel))
    }
  }, ignoreInit = FALSE)

  observeEvent(input$render_quarto, {
    updateSelectInput(session, "preview_mode", selected = "Real Quarto preview")
    do_quarto_render("Manual")
  }, ignoreInit = TRUE)

  observeEvent(input$slide_id, {
    state$selected_slide <- input$slide_id
    component_choices <- get_component_choices(registry, input$slide_id)
    current_component <- state$selected_component %||% names(component_choices)[1]
    if (!(current_component %in% unname(component_choices))) {
      current_component <- unname(component_choices)[1]
    }
    updateSelectInput(session, "component_id", choices = component_choices, selected = current_component)
  }, ignoreInit = TRUE)

  observeEvent(input$component_id, {
    state$selected_component <- input$component_id
  }, ignoreInit = TRUE)

  observeEvent(list(input$slide_id, input$component_id), {
    comp <- selected_component()
    if (is.null(comp)) {
      return()
    }
    current <- get_component_override(state$overrides, comp)
    update_component_inputs(session, current)
  }, ignoreInit = FALSE)

  observeEvent(input$manual_css, {
    state$manual_css <- input$manual_css %||% ""
  }, ignoreInit = TRUE)

  observeEvent(
    list(
      input$enable_tokens,
      input$token_accent,
      input$token_accent_light,
      input$token_accent_pink,
      input$token_accent_pink_strong,
      input$token_yellow,
      input$token_yellow_light,
      input$token_light_grey,
      input$token_normal_grey,
      input$token_page_bg,
      input$token_black
    ),
    {
      state$tokens <- list(
        enabled = isTRUE(input$enable_tokens),
        accent = input$token_accent %||% "",
        accent_light = input$token_accent_light %||% "",
        accent_pink = input$token_accent_pink %||% "",
        accent_pink_strong = input$token_accent_pink_strong %||% "",
        yellow = input$token_yellow %||% "",
        yellow_light = input$token_yellow_light %||% "",
        light_grey = input$token_light_grey %||% "",
        normal_grey = input$token_normal_grey %||% "",
        page_bg = input$token_page_bg %||% "",
        black = input$token_black %||% ""
      )
    },
    ignoreInit = TRUE
  )

  observeEvent(
    list(
      input$position_mode,
      input$x,
      input$y,
      input$enable_z_index,
      input$z_index,
      input$enable_width,
      input$width,
      input$enable_height,
      input$height,
      input$enable_max_width,
      input$max_width,
      input$enable_min_height,
      input$min_height,
      input$enable_font_size,
      input$font_size,
      input$enable_line_height,
      input$line_height,
      input$enable_letter_spacing,
      input$letter_spacing,
      input$enable_font_weight,
      input$font_weight,
      input$enable_text_align,
      input$text_align,
      input$enable_margin,
      input$margin_top,
      input$margin_right,
      input$margin_bottom,
      input$margin_left,
      input$enable_padding,
      input$padding_top,
      input$padding_right,
      input$padding_bottom,
      input$padding_left,
      input$enable_gap,
      input$gap,
      input$enable_column_gap,
      input$column_gap,
      input$enable_row_gap,
      input$row_gap
    ),
    {
      comp <- selected_component()
      if (is.null(comp)) {
        return()
      }
      updated <- input_to_override(input, comp)
      if (is_override_empty(updated)) {
        state$overrides[[comp$id]] <- NULL
      } else {
        state$overrides[[comp$id]] <- updated
      }
    },
    ignoreInit = TRUE
  )

  nudge_xy <- function(dx = 0, dy = 0, multiplier = 1) {
    step <- coerce_num(input$nudge_step, 2) * multiplier
    x_now <- coerce_num(input$x, 0)
    y_now <- coerce_num(input$y, 0)
    x_new <- max(-400, min(400, x_now + dx * step))
    y_new <- max(-400, min(400, y_now + dy * step))
    updateSliderInput(session, "x", value = x_new)
    updateSliderInput(session, "y", value = y_new)
  }

  observeEvent(input$nudge_up, nudge_xy(0, -1, 1), ignoreInit = TRUE)
  observeEvent(input$nudge_down, nudge_xy(0, 1, 1), ignoreInit = TRUE)
  observeEvent(input$nudge_left, nudge_xy(-1, 0, 1), ignoreInit = TRUE)
  observeEvent(input$nudge_right, nudge_xy(1, 0, 1), ignoreInit = TRUE)
  observeEvent(input$nudge_up_10, nudge_xy(0, -1, 10), ignoreInit = TRUE)
  observeEvent(input$nudge_down_10, nudge_xy(0, 1, 10), ignoreInit = TRUE)
  observeEvent(input$nudge_right_10, nudge_xy(1, 0, 10), ignoreInit = TRUE)

  observeEvent(input$reset_component, {
    comp <- selected_component()
    if (is.null(comp)) {
      return()
    }
    state$overrides[[comp$id]] <- NULL
    update_component_inputs(session, default_override(comp))
  }, ignoreInit = TRUE)

  observeEvent(input$reset_group_btn, {
    comp <- selected_component()
    if (is.null(comp)) {
      return()
    }
    current <- get_component_override(state$overrides, comp)
    reset_group <- input$reset_group %||% "position"
    updated <- reset_override_group(current, reset_group)
    if (is_override_empty(updated)) {
      state$overrides[[comp$id]] <- NULL
    } else {
      state$overrides[[comp$id]] <- updated
    }
    update_component_inputs(session, updated)
  }, ignoreInit = TRUE)

  observeEvent(input$reset_all, {
    state$overrides <- list()
    state$manual_css <- ""
    state$tokens <- default_global_tokens()

    comp <- selected_component()
    if (!is.null(comp)) {
      update_component_inputs(session, default_override(comp))
    }

    updateTextAreaInput(session, "manual_css", value = "")
    updateCheckboxInput(session, "enable_tokens", value = FALSE)
    updateTextInput(session, "token_accent", value = state$tokens$accent)
    updateTextInput(session, "token_accent_light", value = state$tokens$accent_light)
    updateTextInput(session, "token_accent_pink", value = state$tokens$accent_pink)
    updateTextInput(session, "token_accent_pink_strong", value = state$tokens$accent_pink_strong)
    updateTextInput(session, "token_yellow", value = state$tokens$yellow)
    updateTextInput(session, "token_yellow_light", value = state$tokens$yellow_light)
    updateTextInput(session, "token_light_grey", value = state$tokens$light_grey)
    updateTextInput(session, "token_normal_grey", value = state$tokens$normal_grey)
    updateTextInput(session, "token_page_bg", value = state$tokens$page_bg)
    updateTextInput(session, "token_black", value = state$tokens$black)
  }, ignoreInit = TRUE)

  refresh_preset_select <- function(selected_value = NULL) {
    choices <- list_preset_choices(preset_dir)
    updateSelectInput(session, "preset_to_load", choices = choices, selected = selected_value)
  }

  observeEvent(input$save_preset, {
    payload <- list(
      version = 1,
      overrides = state$overrides,
      manual_css = state$manual_css,
      tokens = state$tokens,
      selection = list(
        slide_id = input$slide_id %||% state$selected_slide,
        component_id = input$component_id %||% state$selected_component
      )
    )

    path <- tryCatch(
      save_preset(preset_dir, input$preset_name %||% "", payload),
      error = function(e) e
    )

    if (inherits(path, "error")) {
      showNotification(
        paste("Preset save failed:", conditionMessage(path)),
        type = "error",
        duration = 6
      )
      return()
    }

    file_name <- basename(path)
    refresh_preset_select(selected_value = file_name)
    showNotification(sprintf("Preset saved: %s", file_name), type = "message", duration = 4)
  }, ignoreInit = TRUE)

  observeEvent(input$load_preset, {
    preset_file <- trim_or_empty(input$preset_to_load)
    if (!nzchar(preset_file)) {
      showNotification("Select a preset first.", type = "warning", duration = 4)
      return()
    }

    preset_path <- file.path(preset_dir, preset_file)
    loaded <- tryCatch(
      load_preset(preset_path, token_defaults),
      error = function(e) e
    )

    if (inherits(loaded, "error")) {
      showNotification(
        paste("Preset load failed:", conditionMessage(loaded)),
        type = "error",
        duration = 6
      )
      return()
    }

    state$overrides <- loaded$overrides
    state$manual_css <- loaded$manual_css
    state$tokens <- merge_tokens(token_defaults, loaded$tokens)

    updateTextAreaInput(session, "manual_css", value = state$manual_css)
    updateCheckboxInput(session, "enable_tokens", value = isTRUE(state$tokens$enabled))
    updateTextInput(session, "token_accent", value = state$tokens$accent)
    updateTextInput(session, "token_accent_light", value = state$tokens$accent_light)
    updateTextInput(session, "token_accent_pink", value = state$tokens$accent_pink)
    updateTextInput(session, "token_accent_pink_strong", value = state$tokens$accent_pink_strong)
    updateTextInput(session, "token_yellow", value = state$tokens$yellow)
    updateTextInput(session, "token_yellow_light", value = state$tokens$yellow_light)
    updateTextInput(session, "token_light_grey", value = state$tokens$light_grey)
    updateTextInput(session, "token_normal_grey", value = state$tokens$normal_grey)
    updateTextInput(session, "token_page_bg", value = state$tokens$page_bg)
    updateTextInput(session, "token_black", value = state$tokens$black)

    resolved <- resolve_selection(
      registry = registry,
      slide_id = loaded$selection$slide_id %||% input$slide_id,
      component_id = loaded$selection$component_id %||% input$component_id
    )

    state$selected_slide <- resolved$slide_id
    state$selected_component <- resolved$component_id

    updateSelectInput(
      session,
      "slide_id",
      choices = get_slide_choices(registry),
      selected = resolved$slide_id
    )
    updateSelectInput(
      session,
      "component_id",
      choices = get_component_choices(registry, resolved$slide_id),
      selected = resolved$component_id
    )

    showNotification(sprintf("Preset loaded: %s", preset_file), type = "message", duration = 4)
  }, ignoreInit = TRUE)

  generated_css <- reactive({
    build_override_css(
      overrides = state$overrides,
      registry = registry,
      token_overrides = state$tokens,
      manual_css = state$manual_css
    )
  })

  auto_render_signal <- debounce(
    reactive({
      list(
        css = generated_css(),
        qmd_file = trim_or_empty(input$qmd_file),
        preview_mode = input$preview_mode %||% "Mock preview",
        auto_render = isTRUE(input$auto_render_quarto)
      )
    }),
    millis = 800
  )

  observeEvent(auto_render_signal(), {
    signal <- auto_render_signal()
    if (!isTRUE(signal$auto_render)) {
      return()
    }
    if (!identical(signal$preview_mode, "Real Quarto preview")) {
      return()
    }
    if (!nzchar(signal$qmd_file)) {
      return()
    }
    do_quarto_render("Auto")
  }, ignoreInit = TRUE)

  debounced_css <- debounce(generated_css, millis = 300)

  observe({
    css <- debounced_css()
    err <- tryCatch(
      {
        write_text_file(override_css_path, css)
        NULL
      },
      error = function(e) e
    )

    if (is.null(err)) {
      write_status_rv(sprintf("Saved %s", format(Sys.time(), "%H:%M:%S")))
    } else {
      write_status_rv(sprintf("Write failed: %s", conditionMessage(err)))
    }
  })

  state_payload <- reactive({
    list(
      version = 1,
      overrides = state$overrides,
      manual_css = state$manual_css,
      tokens = state$tokens,
      selection = list(
        slide_id = input$slide_id %||% state$selected_slide,
        component_id = input$component_id %||% state$selected_component
      )
    )
  })

  debounced_state <- debounce(state_payload, millis = 300)

  observe({
    payload <- debounced_state()
    tryCatch(
      {
        save_state(state_path, payload)
      },
      error = function(e) {
        write_status_rv(sprintf("State save failed: %s", conditionMessage(e)))
      }
    )
  })

  output$preview_ui <- renderUI({
    preview_mode <- input$preview_mode %||% "Mock preview"

    if (identical(preview_mode, "Real Quarto preview")) {
      html_path <- quarto_html_path_rv()
      if (!nzchar(trim_or_empty(html_path)) || !file.exists(html_path)) {
        return(
          tags$div(
            class = "preview-shell",
            tags$div(
              style = "padding:18px; font-size:14px;",
              "No real Quarto HTML is available yet. Click ",
              tags$strong("Render Quarto preview"),
              " or select a .qmd with an existing rendered HTML output."
            )
          )
        )
      }

      iframe_src <- build_iframe_src(
        html_path = html_path,
        project_root = project_root,
        resource_prefix = quarto_preview_prefix
      )

      return(
        tags$div(
          class = "preview-shell",
          style = "background:#d7d3ca; padding:10px;",
          tags$div(
            style = "height:900px; overflow:auto; background:#fff; border-radius:8px;",
            tags$iframe(
              src = iframe_src,
              style = "width:100%; height:900px; border:0; display:block;"
            )
          )
        )
      )
    }

    slide_id <- input$slide_id %||% selection$slide_id
    component_id <- input$component_id %||% selection$component_id
    zoom <- coerce_num(input$preview_zoom, 0.33)

    preview_css <- build_preview_css(
      debug_boxes = isTRUE(input$debug_boxes),
      selected_component_id = component_id
    )
    preview_html <- build_preview_markup(
      registry = registry,
      slide_id = slide_id,
      zoom = zoom,
      show_guides = isTRUE(input$show_guides)
    )

    tagList(
      if (nzchar(base_css_text)) tags$style(HTML(base_css_text)),
      if (nzchar(ai_css_text)) tags$style(HTML(ai_css_text)),
      tags$style(HTML(generated_css())),
      tags$style(HTML(preview_css)),
      HTML(preview_html)
    )
  })

  output$css_text <- renderText({
    generated_css()
  })

  output$quarto_preview_link <- renderUI({
    preview_mode <- input$preview_mode %||% "Mock preview"
    if (!identical(preview_mode, "Real Quarto preview")) {
      return(NULL)
    }

    html_path <- quarto_html_path_rv()
    if (!nzchar(trim_or_empty(html_path)) || !file.exists(html_path)) {
      return(tags$div(class = "status-error", "No rendered HTML available yet."))
    }

    iframe_src <- build_iframe_src(
      html_path = html_path,
      project_root = project_root,
      resource_prefix = quarto_preview_prefix
    )

    tags$div(
      class = "small-label",
      "Rendered HTML: ",
      tags$a(
        href = iframe_src,
        target = "_blank",
        rel = "noopener noreferrer",
        path_relative_to(html_path, project_root)
      )
    )
  })

  output$write_status <- renderText({
    status <- write_status_rv()
    status %||% ""
  })

  output$quarto_render_status <- renderText({
    quarto_status_rv()
  })

  output$quarto_last_render <- renderText({
    quarto_last_render_rv()
  })

  output$quarto_log <- renderText({
    quarto_log_rv()
  })
}

shinyApp(ui, server)
