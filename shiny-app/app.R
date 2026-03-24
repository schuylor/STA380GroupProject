library(shiny)
library(bslib)
library(ggplot2)
library(colourpicker)
library(plotly)

source("R/bootstrap_functions.R", local = TRUE)
load("data/lizard_data.rda")
# =====================================================================
# FRONT-END (Sage)
# =====================================================================
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Theme 3: Bootstrap Analysis of Florida Scrub Lizards",

  sidebar = sidebar(
    title = "Analysis Parameters",

    selectInput(inputId = "hypo_select",
                label = "1. Comparison Hypothesis:",
                choices = c("Male vs. Female (Snout Length)",
                            "Gravid vs. Non-Gravid (Body Temp)")),

    radioButtons(inputId = "stat_select",
                 label = "2. Statistic of Interest:",
                 choices = c("Difference in Means" = "mean",
                             "Difference in Medians" = "median")),

    numericInput(inputId = "num_iter",
                 label = "3. Number of Iterations:",
                 value = 1000, min = 100, max = 10000, step = 100),

    numericInput(inputId = "rand_seed",
                 label = "4. Random Seed:",
                 value = 123),

    colourpicker::colourInput(inputId = "plot_color",
                              label = "5. Visualization Color:",
                              value = "#3498DB")
  ),

  card(
    card_header("Bootstrap Distribution Histogram"),
    plotlyOutput(outputId = "boot_plot")
  ),

  card(
    card_header("Comparative Statistics Table"),
    tableOutput(outputId = "comp_table")
  )
)

# =====================================================================
# BACK-END (Sage)
# =====================================================================
server <- function(input, output, session) {

  # clean data, logistic important here!!
  clean_data <- reactive({
    df <- lizard_data
    colnames(df) <- tolower(gsub(" ", "_", trimws(gsub("\\.+", " ", colnames(df)))))
    return(df)
  })

  boot_results <- reactive({
    req(input$hypo_select)
    req(input$num_iter)

    # CHECK IF USER ENTERED THE CORRECT ITER / SEED
    validate(
      need(input$num_iter >= 100 && input$num_iter <= 10000,
           "Set Iterations in [100, 10000].")
    )

    validate(
      need(is.numeric(input$rand_seed),
           "Invalid seed.")
    )
    set.seed(input$rand_seed)

    df <- clean_data()

    # mouse-listener
    if (input$hypo_select == "Male vs. Female (Snout Length)") {
      g1_raw <- df$snout_vent_length_mm[df$sex == "M"]
      g2_raw <- df$snout_vent_length_mm[df$sex == "F"]
      name1 <- "Male"
      name2 <- "Female"
    } else {
      valid_rows <- !is.na(df$reproductive_status) & df$reproductive_status != ""
      gravid_data <- df[valid_rows, ]

      g1_raw <- gravid_data$body_temperature_c[gravid_data$reproductive_status == "gravid"]
      g2_raw <- gravid_data$body_temperature_c[gravid_data$reproductive_status == "nongravid"]
      name1 <- "Gravid"
      name2 <- "Non-Gravid"
    }

    group1 <- as.numeric(as.character(g1_raw))
    group2 <- as.numeric(as.character(g2_raw))

    group1 <- group1[!is.na(group1)]
    group2 <- group2[!is.na(group2)]

    # error catching
    if(length(group1) == 0 || length(group2) == 0) {
      stop("Selected data columns are not numeric! (Check column names)")
    }
    res <- two_sample_bootstrap(group1 = group1,
                                        group2 = group2,
                                        iterations = input$num_iter,
                                        stat = input$stat_select)

    return(list(
      diffs = res,
      g1_data = group1,
      g2_data = group2,
      n1 = name1,
      n2 = name2
    ))
  })

  output$boot_plot <- renderPlotly({
    res_list <- boot_results()
    data_for_plot <- data.frame(diffs = res_list$diffs)

    p <- ggplot(data_for_plot, aes(x = diffs)) +
      geom_histogram(fill = input$plot_color, color = "black", bins = 30) +
      geom_vline(xintercept = mean(data_for_plot$diffs), color = "red", linewidth = 1.2) +
      theme_minimal() +
      labs(
        title = paste("Bootstrap Distribution (", input$num_iter, " Iterations)", sep = ""),
        x = paste("Difference in", tools::toTitleCase(input$stat_select)),
        y = "Frequency"
      )

    ggplotly(p)
  })

  output$comp_table <- renderTable({
    res_list <- boot_results()
    data.frame(
      Group = c(res_list$n1, res_list$n2),
      Mean = c(mean(res_list$g1_data), mean(res_list$g2_data)),
      Median = c(median(res_list$g1_data), median(res_list$g2_data)),
      SD = c(sd(res_list$g1_data), sd(res_list$g2_data))
    )
  })
}
shinyApp(ui = ui, server = server)
