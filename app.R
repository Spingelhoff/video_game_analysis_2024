#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggiraph)
library(ggrepel)
library(patchwork)
library(gt)
library(forcats)

data <- readRDS("video_game_analysis_2024.RDS")

ui <- page_fluid(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Pixelify+Sans&display=swap');
      * {
        font-family: 'Pixelify Sans', sans-serif;
      }"
    ))
  ),
  titlePanel(h1("Metacritic Featured Games Throughout Time", align = "center")),
  fluidRow(
    column(2),
    column(4,
      sliderInput(
        "YInput",
        "Year of Release",
        min = year(min(data$release_date, na.rm = TRUE)),
        max = year(max(data$release_date, na.rm = TRUE)),
        value = 2010,
        sep = "",
        step = 5
      ),
      align = "center"
    ),
    column(4,
      selectizeInput(
        "PInput",
        "Platform",
        choices = c("All", unique(data$platform_condensed)),
        multiple = TRUE,
        selected = "All"
      ),
      align = "center"
    ),
    column(2)
  ),
    # Show a plot of the generated distribution
  fluidRow(
    column(12,
      girafeOutput("combined_plot", height = "100%")
    )
  ),
  fluidRow(
    column(3,
      tableOutput("critic_title_table")
    ),
    column(3,
      tableOutput("user_title_table")
    ),
    column(3,
      tableOutput("ambivalence_table_critic")
    ),
    column(3,
      tableOutput("ambivalence_table_user")
    )
  ),
  fluidRow(
    column(4,
      girafeOutput("locality_plot", height = "100%")
    ),
    column(4,
      girafeOutput("exclusivity_plot", height = "100%")
    ),
    column(4,
      girafeOutput("independance_plot", height = "100%")       
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data <- readRDS("video_game_analysis_2024.RDS")
  
  abbrev_text <- function(x) {
    paste0(
      "<div style=\"display:table;table-layout:fixed;width:100%;\">",
      "<div title=\"", x , "\", ", # `<p>` has been changed to `<div>` here
      "style=\"overflow-x:hidden;text-overflow:ellipsis;white-space:nowrap\">",
      x,
      "</div>",
      "</div>"
    )
  }
  
  yearly_data <- reactive({
    tempData <- data |>
      filter(
        year == input$YInput
      )
    if (!("All" %in% input$PInput) & !is.null(input$PInput)) {
      tempData <- tempData |>
        filter(
          platform_condensed %in% input$PInput
        )
    }
      tempData
  })
  
  yearly_genre_ratings <- reactive({
    yearly_data() |>
      group_by(
        genre,
        metacritic_score_factorized
      ) |>
      reframe(
        n_scored = n_distinct(title)
      )
  })
  
  yearly_ambivalence <- reactive({
    yearly_data() |>
      group_by(title) |>
      reframe(
        genre = first(genre),
        avg_user_score = mean(user_score, na.rm = TRUE) * 10,
        avg_metacritic_score = mean(metacritic_score, na.rm = TRUE),
        critic_coverage = max(critic_coverage),
        classification = case_when(
          title %in% yearly_ambivalence_user()$Title ~ "Preferred",
          title %in% yearly_ambivalence_critic()$Title ~ "Preferred",
          title %in% yearly_critic_titles()$Title ~ "High Scored",
          title %in% yearly_user_titles()$Title ~ "High Scored",
          TRUE ~ "No Preference"
        )
      ) |>
      na.omit()
  })
  
  output$combined_plot <- renderGirafe({
    genre_plot <- ggplot(
      yearly_genre_ratings(),
      aes(x = fct_rev(genre), y = n_scored, fill = metacritic_score_factorized, data_id = genre)) +
      geom_col_interactive(color = "black") +
      coord_flip() +
      theme_classic() +
      scale_x_discrete(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      scale_fill_brewer(palette = "RdBu", direction = -1) +
      theme(
        legend.position = "bottom",
        text = element_text(size = 10)
      ) +
      xlab("# of Games") +
      ylab("Genre") +
      labs(
        fill = "Scoring"
      )
    
    ambivalence_plot <- ggplot(
      yearly_ambivalence(),
      aes(x = avg_user_score, y = avg_metacritic_score, size = critic_coverage, alpha = critic_coverage, color = classification, data_id = genre)) +
      geom_point_interactive(
        aes(tooltip = paste0(
          "Title: ", title, "\n",
          "Genre: ", genre, "\n",
          "Critic Score: ", avg_metacritic_score, "\n",
          "User Score:", avg_user_score, "\n")
        )
      ) +
      theme_classic() +
      scale_color_manual(
        name = "Preference",
        values = c("#4F7EBB", "grey", "#BC3D41")) +
      scale_size_continuous(name = "Coverage") +
      scale_alpha_continuous(name = "Coverage") +
      theme(
        legend.position = "bottom",
        text = element_text(size = 10)
      ) +
      xlab("User Score") +
      ylab("Metacritic Score") +
      guides(
        color = "none"
      )
    
    girafe(ggobj = genre_plot + ambivalence_plot, width_svg = 12, height_svg = 5) |>
      girafe_options(
        opts_hover(css = "stroke:black;stroke-width:2px;fill-opacity:1;"),
        opts_hover_inv(css = "fill-opacity:0.2"),
        opts_selection(css = "stroke:black;stroke-width:2px;fill-opacity:1;",
                       type = "multiple"),
        opts_selection_inv(css = "fill-opacity:0.2")
      )
  })
  
  yearly_critic_titles <- reactive({
    yearly_data() |>
      group_by(title) |>
      reframe(
        score = mean(metacritic_score, na.rm = TRUE),
        genre = first(genre)
      ) |>
      arrange(desc(score)) |>
      head(5) |>
      mutate(
        rank = seq_along(title)
      ) |>
      select(
        Rank = rank,
        Title = title,
        Genre = genre
      )
  })
  
  output$critic_title_table <- render_gt(
    gt(yearly_critic_titles()) |>
      tab_header(
        title = "Top 5 Critic Scored Games"
      ) |>
      tab_options(
        heading.background.color = "#4F7EBB",
        table.font.size = 13
      ) |>
      text_transform(
        fn = abbrev_text,
        locations = cells_body(column = Title)
      )
  )
  
  yearly_user_titles <- reactive({
    yearly_data() |>
      group_by(title) |>
      reframe(
        score = mean(user_score, na.rm = TRUE),
        genre = first(genre)
      ) |>
      arrange(desc(score)) |>
      head(5) |>
      mutate(
        rank = seq_along(title)
      ) |>
      select(
        Rank = rank,
        Title = title,
        Genre = genre
      )
  })
  
  output$user_title_table <- render_gt(
    gt(yearly_user_titles()) |>
      tab_header(
        title = "Top 5 User Scored Games"
      ) |>
      tab_options(
        heading.background.color = "#4F7EBB",
        table.font.size = 13
      ) |>
      text_transform(
        fn = abbrev_text,
        locations = cells_body(column = Title)
      )
  )
  
  yearly_ambivalence_critic <- reactive({
    yearly_data() |>
      group_by(
        title
      ) |>
      reframe(
        ambivalence = mean(user_metacritic_differential, na.rm = TRUE),
        genre = max(genre)
      ) |>
      arrange(ambivalence) |>
      head(5) |>
      mutate(
        rank = seq_along(title)
      ) |>
      select(
        Rank = rank,
        Title = title,
        Genre = genre
      )
  })
  
  output$ambivalence_table_critic <- render_gt(
    gt(yearly_ambivalence_critic()) |>
      tab_header(
        title = "Top 5 Critic Preferred Games"
      ) |>
      tab_options(
        heading.background.color = "#BC3D41",
        table.font.size = 13
      ) |>
      text_transform(
        fn = abbrev_text,
        locations = cells_body(column = Title)
      )
  )
  
  yearly_ambivalence_user <- reactive({
    yearly_data() |>
      group_by(
        title
      ) |>
      reframe(
        ambivalence = mean(user_metacritic_differential, na.rm = TRUE),
        genre = max(genre)
      ) |>
      arrange(desc(ambivalence)) |>
      head(5) |>
      mutate(
        rank = seq_along(title)
      ) |>
      select(
        Rank = rank,
        Title = title,
        Genre = genre
      )
  })  
  
  output$ambivalence_table_user <- render_gt(
    gt(yearly_ambivalence_user()) |>
      tab_header(
        title = "Top 5 User Preferred Games"
      ) |>
      tab_options(
        heading.background.color = "#BC3D41",
        table.font.size = 13
      ) |>
      text_transform(
        fn = abbrev_text,
        locations = cells_body(column = Title)
      )
  )
  
  yearly_locality <- reactive({
    yearly_data() |>
      group_by(locality) |>
      reframe(n_games = n_distinct(title))
  })
  
  output$locality_plot <- renderGirafe({
    plot <- ggplot(yearly_locality(),
           aes(x = "", y = n_games, fill = locality)) +
      geom_col(color = "black") +
      coord_polar(theta = "y") +
      theme_void() +
      scale_fill_manual(
        name = "Locality",
        values = c("#4F7EBB", "grey")) +
      theme(
        text = element_text(size = 14),
        legend.position = "bottom")
    
    girafe(ggobj = plot, height_svg = 7)
  })
  
  yearly_exclusivity <- reactive({
    yearly_data() |>
      group_by(exclusivity) |>
      reframe(n_games = n_distinct(title))
  })
  
  output$exclusivity_plot <- renderGirafe({
    plot <- ggplot(yearly_exclusivity(),
           aes(x = "", y = n_games, fill = exclusivity)) +
      geom_col(color = "black") +
      coord_polar(theta = "y") +
      theme_void() +
      scale_fill_manual(
        name = "Exclusivity",
        values = c("#4F7EBB", "grey")) +
      theme(
        text = element_text(size = 14),
        legend.position = "bottom")
    
    girafe(ggobj = plot, height_svg = 7)
  })
  
  yearly_indie <- reactive({
    yearly_data() |>
      group_by(independance) |>
      reframe(n_games = n_distinct(title))
  })
  
  output$independance_plot <- renderGirafe({
    plot <- ggplot(yearly_indie(),
           aes(x = "", y = n_games, fill = independance)) +
      geom_col(color = "black") +
      coord_polar(theta = "y") +
      theme_void() +
      scale_fill_manual(
        name = "Independance",
        values = c("#4F7EBB", "grey")) +
      theme(
        text = element_text(size = 14),
        legend.position = "bottom")
    
    girafe(ggobj = plot, height_svg = 7)
  })
}

shinyApp(ui = ui, server = server)
