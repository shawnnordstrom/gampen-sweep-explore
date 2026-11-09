library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

csv_path <- Sys.getenv("SWEEP_CSV", "data/sweep.csv")
expected <- c("run_id", "path", "lr", "momentum", "augmentation", "weight_decay", "nesterov",
              "epochs", "repeat_id", "split", "bt_scatter", "flux_scatter", "re_scatter",
              "coverage_68_bt", "coverage_68_flux", "coverage_68_re", "frac_thrown_out",
              "is_released")
numeric_cols <- c("lr", "momentum", "weight_decay", "bt_scatter", "flux_scatter", "re_scatter",
                  "coverage_68_bt", "coverage_68_flux", "coverage_68_re", "frac_thrown_out")

as_flag <- function(x) {
  x <- tolower(trimws(as.character(x)))
  ifelse(x %in% c("true", "t", "on", "yes", "1"), TRUE,
         ifelse(x %in% c("false", "f", "off", "no", "0"), FALSE, NA))
}

runs_all <- read_csv(csv_path, col_types = cols(.default = col_character()), na = c("", "NA"))
for (col in setdiff(expected, names(runs_all))) runs_all[[col]] <- NA_character_
runs_all <- runs_all |>
  select(all_of(expected)) |>
  mutate(across(all_of(numeric_cols), as.numeric),
         across(c(epochs, repeat_id), as.integer),
         path = toupper(path),
         augmentation = ifelse(as_flag(augmentation), "on", "off"),
         nesterov = as_flag(nesterov),
         is_released = coalesce(as_flag(is_released), FALSE),
         split = ifelse(tolower(split) %in% c("validation", "val", "dev", "devel"), "devel",
                        tolower(split)),
         coverage_68_mean = (coverage_68_bt + coverage_68_flux + coverage_68_re) / 3)

metrics <- c("B/T scatter" = "bt_scatter", "Flux scatter" = "flux_scatter",
             "R_e scatter" = "re_scatter", "68% coverage (mean of 3)" = "coverage_68_mean",
             "Fraction thrown out" = "frac_thrown_out")
coverage_cols <- c("B/T" = "coverage_68_bt", "Flux" = "coverage_68_flux", "R_e" = "coverage_68_re")
paths <- c("A", "B1", "B2")
target <- 68.27

metric_label <- function(m) names(metrics)[metrics == m]
fmt_e <- function(x) vapply(x, function(v) format(signif(v, 2), scientific = TRUE), "")
recipe_label <- function(d, sep = " · ") {
  paste0(d$path, " · lr ", fmt_e(d$lr), sep,
         sprintf("mom %s · wd %s · Nesterov %s · aug %s · %s ep", d$momentum, fmt_e(d$weight_decay),
                 ifelse(d$nesterov, "on", "off"), d$augmentation, d$epochs))
}

log_lr <- log10(runs_all$lr[!runs_all$is_released])
log_lr <- log_lr[is.finite(log_lr)]
lr_range <- if (length(log_lr)) c(floor(min(log_lr) * 10), ceiling(max(log_lr) * 10)) / 10 else c(-9, -5)

sort_js <- "
$(document).on('click', '.sortable th', function () {
  var th = $(this), i = th.index(), asc = !th.data('asc');
  th.data('asc', asc);
  var body = th.closest('table').children('tbody');
  var key = function (tr) {
    var t = $(tr).children().eq(i).text().trim(), n = t === '' ? NaN : Number(t);
    return isNaN(n) ? t : n;
  };
  var rows = body.children('tr').get().sort(function (a, b) {
    var x = key(a), y = key(b);
    var c = typeof x === typeof y ? (x < y ? -1 : x > y ? 1 : 0) : (typeof x === 'number' ? -1 : 1);
    return asc ? c : -c;
  });
  body.append(rows);
});"


ui <- fluidPage(
  tags$head(tags$script(HTML(sort_js)), tags$style(".sortable th { cursor: pointer; }")),
  titlePanel("GaMPEN transfer-learning sweep explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("metric", "Metric", metrics),
      checkboxGroupInput("paths", "Transfer path", paths, selected = paths, inline = TRUE),
      sliderInput("lr", "Learning rate (log10)", min = lr_range[1], max = lr_range[2],
                  value = lr_range, step = 0.1),
      helpText(textOutput("lr_text", inline = TRUE)),
      radioButtons("aug", "Augmentation", c("Both" = "both", "On" = "on", "Off" = "off"),
                   inline = TRUE),
      radioButtons("split", "Split", c("Validation" = "devel", "Test" = "test"),
                   selected = "test", inline = TRUE),
      checkboxInput("released", "Show released model", TRUE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Sweep",
                 plotOutput("sweep_plot", hover = hoverOpts("sweep_hover", delay = 100)),
                 verbatimTextOutput("hover_info")),
        tabPanel("Repeats", plotOutput("repeats_plot", height = "auto")),
        tabPanel("Calibration", plotOutput("calibration_plot"))
      ),
      hr(),
      downloadButton("download", "Download filtered CSV"),
      div(class = "sortable", style = "overflow-x: auto; margin-top: 10px;", tableOutput("table"))
    )
  )
)


server <- function(input, output, session) {
  output$lr_text <- renderText(sprintf("%s to %s", fmt_e(10^input$lr[1]), fmt_e(10^input$lr[2])))

  runs <- reactive({
    filter(runs_all, !is_released, path %in% input$paths, split == input$split,
           is.na(lr) | between(log10(lr), input$lr[1], input$lr[2]),
           input$aug == "both" | augmentation == input$aug)
  })
  released <- reactive({
    if (!input$released) return(runs_all[0, ])
    filter(runs_all, is_released, is.na(split) | split == input$split)
  })
  released_value <- function(col) {
    v <- released()[[col]]
    v[!is.na(v)][1]
  }

  sweep_points <- reactive({
    runs() |>
      mutate(value = .data[[input$metric]],
             lr_plot = lr * 10^(0.03 * (match(path, paths) - 2))) |>
      filter(!is.na(value), !is.na(lr), lr > 0)
  })

  output$sweep_plot <- renderPlot({
    d <- sweep_points()
    validate(need(nrow(d) > 0, "No runs with this metric match the filters."))
    rel <- released_value(input$metric)
    p <- ggplot(d, aes(lr_plot, value, colour = path)) +
      geom_point(size = 2.5, alpha = 0.8) +
      scale_x_log10(breaks = sort(unique(d$lr)), labels = fmt_e) +
      scale_colour_hue(limits = paths) +
      labs(x = "Learning rate", y = metric_label(input$metric), colour = "Path")
    if (input$metric == "coverage_68_mean") p <- p + geom_hline(yintercept = target, linetype = "dotted")
    if (!is.na(rel)) {
      p <- p + geom_hline(yintercept = rel, linetype = "dashed") +
        annotate("text", x = min(d$lr_plot), y = rel, label = "released model", hjust = 0, vjust = -0.5)
    }
    p
  })

  output$hover_info <- renderText({
    h <- if (is.null(input$sweep_hover)) NULL else
      nearPoints(sweep_points(), input$sweep_hover, xvar = "lr_plot", yvar = "value", maxpoints = 1)
    if (is.null(h) || nrow(h) == 0) return("Hover over a point to see its run and recipe.")
    sprintf("%s (repeat %s)\n%s\n%s = %s", h$run_id, h$repeat_id, recipe_label(h),
            metric_label(input$metric), signif(h$value, 3))
  })

  repeat_runs <- reactive({
    d <- filter(runs(), !is.na(repeat_id), !is.na(.data[[input$metric]]))
    d$value <- d[[input$metric]]
    d$recipe <- recipe_label(d, sep = "\n")
    d |>
      add_count(recipe) |>
      filter(n >= 2) |>
      mutate(recipe = sub("\n", sprintf("  (n = %d)\n", n[1]), recipe, fixed = TRUE), .by = recipe)
  })
  repeat_summary <- reactive({
    repeat_runs() |>
      group_by(recipe, path, n) |>
      summarise(mean = mean(value), se = sd(value) / sqrt(first(n)), .groups = "drop") |>
      mutate(lo = mean - qt(0.975, n - 1) * se, hi = mean + qt(0.975, n - 1) * se,
             recipe = reorder(recipe, -mean))
  })

  output$repeats_plot <- renderPlot({
    s <- repeat_summary()
    validate(need(nrow(s) > 0, "No recipe has 2 or more repeats with this metric under these filters."))
    pts <- mutate(repeat_runs(), recipe = factor(recipe, levels(s$recipe)))
    rel <- released_value(input$metric)
    p <- ggplot(s, aes(y = recipe, colour = path)) +
      geom_point(data = pts, aes(x = value), alpha = 0.3, size = 2) +
      geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.25) +
      geom_point(aes(x = mean), size = 3) +
      scale_colour_hue(limits = paths) +
      labs(x = metric_label(input$metric), y = NULL, colour = "Path",
           subtitle = "Mean with 95% t-interval; faint points are individual repeats")
    if (input$metric == "coverage_68_mean") p <- p + geom_vline(xintercept = target, linetype = "dotted")
    if (!is.na(rel)) {
      p <- p + geom_vline(xintercept = rel, linetype = "dashed") +
        annotate("text", x = rel, y = Inf, label = "released model", hjust = 1.05, vjust = 1.5)
    }
    p
  }, height = function() 140 + 45 * max(1, nrow(repeat_summary())))

  coverage_long <- function(d) {
    bind_rows(lapply(names(coverage_cols), function(p) {
      tibble(path = d$path, parameter = p, coverage = d[[coverage_cols[[p]]]])
    })) |>
      filter(!is.na(coverage)) |>
      mutate(parameter = factor(parameter, names(coverage_cols)))
  }

  output$calibration_plot <- renderPlot({
    d <- coverage_long(runs())
    validate(need(nrow(d) > 0, "No runs with 68% coverage match the filters."))
    rel <- coverage_long(released())
    p <- ggplot(d, aes(parameter, coverage, colour = path)) +
      geom_hline(yintercept = target, linetype = "dashed") +
      geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6, seed = 1),
                 size = 2, alpha = 0.7) +
      scale_colour_hue(limits = paths) +
      labs(x = NULL, y = "Galaxies inside their 68% interval (%)", colour = "Path",
           subtitle = "Dashed line: ideal 68.27%. Above it, intervals are wider than needed; below it, overconfident.")
    if (nrow(rel) > 0) {
      p <- p + geom_point(data = rel, aes(parameter, coverage, shape = "Released model"),
                          inherit.aes = FALSE, size = 4, stroke = 1.5) +
        scale_shape_manual(values = 4, name = NULL)
    }
    p
  })

  table_data <- reactive(bind_rows(runs(), released()))

  output$table <- renderTable({
    mutate(table_data(), lr = fmt_e(lr), weight_decay = fmt_e(weight_decay),
           momentum = as.character(momentum))
  }, digits = 3, na = "NA")

  output$download <- downloadHandler(
    filename = function() sprintf("sweep_%s_filtered.csv", input$split),
    content = function(file) write_csv(table_data(), file, na = "NA")
  )
}

shinyApp(ui, server)
