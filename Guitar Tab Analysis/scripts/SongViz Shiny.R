## shiny app

library(rvest)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(showtext)
library(gganimate)
library(forcats)
library(gifski)
library(png)
library(shiny)



# functions

# function for scraping text from webpage
scrape_data <- function (x) {
  y <- read_html(x) %>%
    html_text()
  return(y)
}

capo_from_url <- function(tab_url) {
  # scrape data from guitartabs.cc
  song_html <- scrape_data(tab_url)
  
  # extract tab section
  song_text <- sub('\\\tComments.*', "", sub('.*Artist:', "", song_html))
  # extract capo
  capo_text <- str_match_all(song_text, "Capo:*(.*?)\\s*\\\r")
  capo <- as.numeric(str_extract(capo_text,"\\(?[0-9,.]+\\)?"))
  
  return(capo)
}

songdf_from_url <- function(tab_url) {
  # scrape data from guitartabs.cc
  song <- scrape_data(tab_url)
  
  # extract tab section
  song_text <- sub('\\\tComments.*', "", sub('.*Artist:', "", song))
  
  # extract individual strings
  song_tab <- str_match_all(song_text, "\\\n\\|\\s*(.*?)\\s*\\|\\\r")
  
  #count of tab rows
  tab_count <- nrow(song_tab[[1]]) / 6
  
  # combine with notes and octave data
  song_df <- data.frame(tabs = song_tab[[1]][1:nrow(song_tab[[1]])],
                              notes = rep(c("E", "A", "D", "G", "B", "E"), tab_count),
                              string = rep(c("1", "2", "3", "4", "5", "6"), tab_count),
                              tab_row = (rep(c(seq(1:tab_count)), each = 6)),
                              octave_id = rep(c(2, 2, 3, 3, 3, 4), tab_count))
  
  # extract individual notes and calculate actual note played
  song_long <- song_df %>% 
    mutate(tab_raw = tabs,
           tabs = str_extract_all(song_df$tabs,"\\(?[0-9,.]+\\)?")) %>% 
    unnest(cols = tabs) %>%
    group_by(tab_row, string) %>%
    mutate(note_num = 1:n()) %>%
    mutate(tab_pos = as.list(gregexpr("\\(?[0-9,.]+\\)?", tab_raw))[[1]][note_num]) %>%
    left_join(guitar_strings, by = c("notes", "octave_id")) %>%
    mutate(pos_id = (tab_row * 1000) + tab_pos,
           tabs = as.numeric(tabs),
           id = id + tabs + capo) %>%
    select(-notes, -octave_id) %>%
    left_join(full_scale, by = "id")
  
  return(song_long)
}

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Song Visualisation"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      textInput(inputId = "url",
                label = "Enter URL:",
                  placeholder = "Enter song URL")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "animated_plot")
      
    )
  )
)

server <- function(input, output) {
  output$animated_plot <- renderImage({
    
    tab_url <- input$url
    
    capo <- capo_from_url(tab_url)
    
    song_df <- songdf_from_url(tab_url)
    
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext='.gif')
    

    p <- whole_song_long %>%
      mutate(string = factor(string, levels = c("6", "5", "4", "3", "2", "1"))) %>%
      select(pos_id, string, tabs, tab_row, notes, section) %>%
      mutate(tabs = as.numeric(gsub(0, NA, tabs))) %>%
      ggplot(aes(x = string, y = tabs)) +
      geom_point(aes(color = notes), size = 20, alpha = 0.65, shape = 1, stroke = 8) +
      geom_col(aes(y = 12, fill = notes), width = 0.06, alpha = 0.65) +
      transition_states(pos_id, transition_length = 2) +
      shadow_wake(wake_length = 0.03) +
      exit_fade() +
      #scale_y_continuous(breaks = seq(1:max(whole_song_long$tabs)), limits = c(0, 12)) +
      scale_y_reverse(breaks = seq(max(whole_song_long$tabs):1), limits = c(12, 0)) +
      scale_color_manual(values = palette) +
      scale_fill_manual(values = palette) +
      theme_minimal() +
      labs(y = "FRET", x = "", color = "NOTE\n") +
      guides(size = "none", fill = "none", color = guide_legend(title.position="top", title.hjust = 0.5)) +
      theme(legend.position = "right",
            text = element_text(size = 25, color = fgcolor),
            plot.background = element_rect(color = bgcolor, fill = bgcolor),
            panel.background = element_rect(color = bgcolor, fill = bgcolor),
            axis.title.y = element_text(face = "bold", size = 20, color = "#6c3b4d", hjust = 0.5),
            plot.title = element_text(hjust = 0.5, size = 40),
            panel.grid.major.x = element_line(size = 4.5, color = "#f5f1ed"),
            panel.grid.major.y = element_line(size = 1.5, color = "#8a6d51"),
            panel.grid.minor.y = element_blank(),
            legend.key.height = unit(100, "pt"),
            plot.subtitle = element_text(hjust = 0.5, size = 30),
            legend.text = element_text(size = 30),
            legend.title = element_text(face = "bold", size = 20, color = "#6c3b4d", hjust = 0.5))

    anim_save("outfile.gif", animate(p, height = 900, width = 400, fps = 4)) # New

    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif'
    )}, deleteFile = TRUE)}
# 
# # Define server logic required to draw a histogram ----
# server <- function(input, output) {
#   
#   # Histogram of the Old Faithful Geyser Data ----
#   # with requested number of bins
#   # This expression that generates a histogram is wrapped in a call
#   # to renderPlot to indicate that:
#   #
#   # 1. It is "reactive" and therefore should be automatically
#   #    re-executed when inputs (input$bins) change
#   # 2. Its output type is a plot
#   output$animated_plot <- renderPlot({
#     
#     # url of tab
#     tab_url <- input$url
#     
#     
#     
#   })
#   
# }

shinyApp(ui, server)
