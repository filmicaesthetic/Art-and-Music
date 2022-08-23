
library(dplyr)
library(tidytext)
library(tidyr)
library(data.table)
library(ggplot2)
library(stringr)
library(stringi)
library(forcats)
library(ggstream)
library(grid)
library(gridExtra)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(fresh)
library(bslib)
library(showtext)
library(sysfonts)
library(showtextdb)

# import google font
font_add_google("Open Sans", "Open Sans")
showtext_auto()

# colours
main_pal <- c("joy" = "#e9a53a",
              "trust" = "#071e2f",
              "surprise" = "#f09e9e",
              "anticipation" = "#c77849",
              "sadness" = "#467495",
              "fear" = "#60376b",
              "anger" = "#520a18",
              "disgust" = "#71de8b")


bg_col <- "#f7f7f5"
block_col <- "#d4d0cb"

poem_split <- function(poem_raw) {
  
  poem_raw_df <- data.frame(poem_text = c(poem_raw))
  
  poem_line <- poem_raw_df %>%
    unnest_lines(line, "poem_text") %>%
    mutate(line_id = row_number())
  
  poem_sent <- poem_raw_df %>%
    unnest_sentences(line, poem_text) %>%
    mutate(line_id = row_number())
  
  if (nrow(poem_sent) > nrow(poem_line)) {
    poem <- poem_sent
  } else {
    poem <- poem_line
  }

  # remove incorrectly formatted characters
  poem_tidy <- poem %>% mutate(line = gsub("â€.", '', line))
  
  # split words to rows
  poem_split <- poem_tidy %>%
    mutate(line_id = row_number()) %>%
    mutate(word = strsplit(gsub('[?]', ' ?', gsub('[!]', ' !', gsub('[^A-Za-z0-9?! ]', "", line))), " "),
    ) %>%
    unnest(word)
  
  # add word id column
  poem_split <- poem_split %>%
    mutate(word_id = seq(1:nrow(poem_split)))
  
  return(poem_split)
  
}

poem_long <- function(poem_split) {
  # import NRC Word-Emotion Association Sentiment Lexicon
  nrc_all <- get_sentiments("nrc")
  
  # identify words that appear in the emotion lexicon
  poem_sent <- poem_split %>% 
    filter() %>%
    mutate(joy = as.numeric(word %in% nrc_all$word[nrc_all$sentiment == "joy"]),
           trust = as.numeric(word %in% nrc_all$word[nrc_all$sentiment == "trust"]),
           surprise = as.numeric(word %in% nrc_all$word[nrc_all$sentiment == "surprise"]),
           anticipation = as.numeric(word %in% nrc_all$word[nrc_all$sentiment == "anticipation"]),
           sadness = as.numeric(word %in% nrc_all$word[nrc_all$sentiment == "sadness"]),
           fear = as.numeric(word %in% nrc_all$word[nrc_all$sentiment == "fear"]),
           anger = as.numeric(word %in% nrc_all$word[nrc_all$sentiment == "anger"]),
           disgust = as.numeric(word %in% nrc_all$word[nrc_all$sentiment == "disgust"]),
           total = joy + trust + surprise + anticipation + sadness + fear + anger + disgust) %>%
    mutate(joy = joy / total,
           trust = trust / total,
           surprise = surprise / total,
           anticipation = anticipation / total,
           sadness = sadness / total,
           fear = fear / total,
           anger = anger / total,
           disgust = disgust / total,
           total = joy + trust + surprise + anticipation + sadness + fear + anger + disgust) %>%
    select(-total)
  
  # replace NAs with 0 
  poem_sent[is.na(poem_sent)] <- 0
  
  # reshape the data frame
  poem_long <- melt(setDT(poem_sent), id.vars = c("line","line_id","word", "word_id"), variable.name = "emotion")
  
  return(poem_long)
}

poem_words <- function(poem_long) {

  # aggregate by word
  poem_byword <- poem_long %>% 
    group_by(line, line_id, emotion, word, word_id) %>% 
    summarise(value = sum(value)) %>% 
    filter() %>% 
    group_by(word) %>% 
    mutate(test = value) %>%
    select(-line, -value) %>%
    pivot_wider(id_cols= c(line_id, word, word_id), names_from = emotion, values_from = test) 
  
  # # additional stats
  poem_words <- poem_byword %>%
    arrange(line_id, word_id) %>%
    mutate(word_no_punc = gsub('[^A-Za-z0-9 ]', "", word),
           chars = nchar(word),
           words = nchar(gsub('[^ ]', "", word_no_punc)) + 1,
           punc = nchar(gsub('[A-Za-z0-9 \t\n\r\v\f]', "", word)),
           comma = nchar(gsub('[^,]', "", word)),
           question = nchar(gsub('[^?]', "", word)),
           exclaim = nchar(gsub('[^!]', "", word)),
           quote = nchar(gsub('[^"]', "", word))) %>%
    group_by(line_id) %>%
    mutate(char_sum = cumsum(chars))
  
  # replace NAs
  poem_words[is.na(poem_words)] <- 0
  
  return(poem_words)
  
}

emotion_df <- function(poem_words) {
  # identify highest scoring emotions
  emotion_sum <- poem_words %>% mutate(x = 1) %>% group_by(x) %>% summarise(joy = sum(joy, na.rm = TRUE),
                                                                            trust = sum(trust, na.rm = TRUE),
                                                                            surprise = sum(surprise, na.rm = TRUE),
                                                                            anticipation = sum(anticipation, na.rm = TRUE),
                                                                            sadness = sum(sadness, na.rm = TRUE),
                                                                            fear = sum(fear, na.rm = TRUE),
                                                                            anger = sum(anger, na.rm = TRUE),
                                                                            disgust = sum(disgust, na.rm = TRUE)) %>%
    pivot_longer(-x, names_to = "emotion", values_to = "value") %>%
    arrange(-value)
  
  return(emotion_sum)
}

poem_byline <- function(poem_long) {
  
  # aggregate by line
  poem_byline <- poem_long %>% 
    group_by(line, line_id, emotion, word_id) %>% 
    summarise(value = sum(value)) %>% 
    filter(value > 0) %>% 
    group_by(line_id) %>% 
    mutate(test = value) %>%
    select(-line)
  
  return(poem_byline)
  
}

plot_lines <- function(poem_words) {
  
  # identify highest scoring emotions
  emotion_sum <- poem_words %>% 
    mutate(x = 1) %>% 
    group_by(x) %>% 
    summarise(joy = sum(joy, na.rm = TRUE),
              trust = sum(trust, na.rm = TRUE),
              surprise = sum(surprise, na.rm = TRUE),
              anticipation = sum(anticipation, na.rm = TRUE),
              sadness = sum(sadness, na.rm = TRUE),
              fear = sum(fear, na.rm = TRUE),
              anger = sum(anger, na.rm = TRUE),
              disgust = sum(disgust, na.rm = TRUE)) %>%
    pivot_longer(-x, names_to = "emotion", values_to = "value") %>%
    arrange(-value, emotion) %>%
    head(3)
  
  # create palette from top 3 emotions
  pal <- main_pal[emotion_sum$emotion]
  
  emo_1 <- paste(emotion_sum$emotion[1])
  emo_2 <- paste(emotion_sum$emotion[2])
  emo_3 <- paste(emotion_sum$emotion[3])
  emo_1 <- as.name(emo_1)
  emo_2 <- as.name(emo_2)
  emo_3 <- as.name(emo_3)
  
  # poem chart
  lines <- poem_words %>%
    mutate(line_id = as.numeric(line_id)) %>%
    ggplot(aes(x = line_id, y = chars)) +
    geom_col(data = poem_words %>% arrange(line_id, word_id), fill = block_col, color = bg_col) +
    geom_col(data = poem_words %>% arrange(line_id, word_id) %>% mutate(emo_1 = ifelse(is.na(!!emo_1) == TRUE, 0, !!emo_1)), aes(alpha = emo_1), fill = as.character(pal[as.character(emo_1)]), color = bg_col) +
    geom_col(data = poem_words %>% arrange(line_id, word_id) %>% mutate(emo_2 = ifelse(is.na(!!emo_2) == TRUE, 0, !!emo_2)), aes(alpha = emo_2), fill = as.character(pal[as.character(emo_2)]), color = bg_col) +
    geom_col(data = poem_words %>% arrange(line_id, word_id) %>% mutate(emo_3 = ifelse(is.na(!!emo_3) == TRUE, 0, !!emo_3)), aes(alpha = emo_3), fill = as.character(pal[as.character(emo_3)]), color = bg_col) +
    scale_alpha(range = c(0, 1)) +
    scale_x_reverse() +
    scale_y_reverse() +
    coord_flip() +
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(color = bg_col, fill = bg_col),
          plot.background = element_rect(colour = bg_col, fill = bg_col),
          legend.position = "none",
          plot.margin=unit(c(0.1,0.1,0.1,-0.5), "cm"),
          plot.caption = element_text(size = 10, color = "#1d1d1d")
    )
  
  return(lines)
  
}

plot_stream <- function(poem_byline, poem_words) {
  
  # identify highest scoring emotions
  emotion_sum <- poem_words %>% 
    mutate(x = 1) %>% 
    group_by(x) %>% 
    summarise(joy = sum(joy, na.rm = TRUE),
              trust = sum(trust, na.rm = TRUE),
              surprise = sum(surprise, na.rm = TRUE),
              anticipation = sum(anticipation, na.rm = TRUE),
              sadness = sum(sadness, na.rm = TRUE),
              fear = sum(fear, na.rm = TRUE),
              anger = sum(anger, na.rm = TRUE),
              disgust = sum(disgust, na.rm = TRUE)) %>%
    pivot_longer(-x, names_to = "emotion", values_to = "value") %>%
    arrange(-value, emotion) %>%
    head(3)
  
  # create palette from top 3 emotions
  pal <- main_pal[emotion_sum$emotion]
  
  emo_1 <- paste(emotion_sum$emotion[1])
  emo_2 <- paste(emotion_sum$emotion[2])
  emo_3 <- paste(emotion_sum$emotion[3])
  emo_1 <- as.name(emo_1)
  emo_2 <- as.name(emo_2)
  emo_3 <- as.name(emo_3)
  
  #prepare line data for streamgraph
  
  poem_line <- data.frame(line_id = rep(seq(1:max(poem_byline$line_id)), 3),
                          emotion = rep(c(as.character(emo_1), as.character(emo_2), as.character(emo_3)), each = max(poem_byline$line_id)))
  
  poem_line <- poem_line %>% 
    left_join(poem_byline, by = c("line_id", "emotion")) %>% 
    group_by(line_id, emotion) %>% 
    summarise(value = sum(test))
  
  poem_line[is.na(poem_line)] <- 0
  
  bw_fig <- ifelse(max(poem_line$line_id) <=12, 0.95,
                   ifelse(max(poem_line$line_id) <=25, 0.9,
                          ifelse(max(poem_line$line_id) <=50, 0.8,
                                 ifelse(max(poem_line$line_id) <=100, 0.7,
                                        ifelse(max(poem_line$line_id) <=200, 0.6,
                                               ifelse(max(poem_line$line_id) <=400, 0.5,
                                                      ifelse(max(poem_line$line_id) <=800, 0.4,
                                                             ifelse(max(poem_line$line_id) <=12, 0.3, 0.2
                          ))))))))
  
  # streamgraph
  stream <- data.frame(poem_line) %>%
    mutate(emotion = fct_reorder(emotion, value, sum)) %>%
    ggplot(aes(x = line_id, y = value)) +
    geom_stream(aes(fill = as.factor(emotion)), type = "ridge", bw = bw_fig) +
    scale_fill_manual(values = pal) +
    theme_minimal() +
    coord_flip() +
    scale_x_reverse() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(color = bg_col, fill = bg_col),
          plot.background = element_rect(colour = bg_col, fill = bg_col),
          legend.position = "none",
          plot.margin=unit(c(0.1,0.1,0.1,-0.5), "cm"))
  
  return(stream)
  
}

plot_legend <- function(poem_words, plot_title) {
  # identify highest scoring emotions
  emotion_sum <- poem_words %>% 
    mutate(x = 1) %>% 
    group_by(x) %>% 
    summarise(joy = sum(joy, na.rm = TRUE),
              trust = sum(trust, na.rm = TRUE),
              surprise = sum(surprise, na.rm = TRUE),
              anticipation = sum(anticipation, na.rm = TRUE),
              sadness = sum(sadness, na.rm = TRUE),
              fear = sum(fear, na.rm = TRUE),
              anger = sum(anger, na.rm = TRUE),
              disgust = sum(disgust, na.rm = TRUE)) %>%
    pivot_longer(-x, names_to = "emotion", values_to = "value") %>%
    arrange(-value, emotion)
  
  filt <- emotion_sum %>% head(3)
  
  emotion_sum <- emotion_sum %>%
    mutate(label = ifelse(emotion %in% filt$emotion, paste0(emotion), NA)) %>%
    mutate(emotion = as.factor(emotion))
  
  # create palette from top 3 emotions
  pal <- main_pal[filt$emotion]
  
  # emotion bar chart
  leg <- data.frame(emotion_sum) %>% 
    mutate(emotion = fct_reorder(emotion, value, sum)) %>% 
    ggplot(aes(x = 1, y = value)) +
    geom_col(aes(fill = emotion), color = bg_col, position = position_stack()) +
    geom_text(aes(label = label, group = emotion), position = position_stack(vjust = 0.9), hjust = 1, fontface="bold", color = "white", family = "Open Sans") +
    theme_minimal() +
    coord_flip() +
    ggtitle(paste(str_wrap(plot_title, 40))) +
    scale_fill_manual(values = pal, na.value = block_col) +
    labs(caption = "Visualisation: @filmicaesthetic | NRC Word-Emotion Lexicon: www.saifmohammad.com") +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(size = 16, hjust = 0.5, family = "Open Sans", face = "bold", margin = margin(t = 20, b = 20, unit = "pt")),
          panel.background = element_rect(color = bg_col, fill = bg_col),
          plot.caption = element_text(family = "Open Sans", size = 6, hjust = 0.5, color = "#333333"),
          plot.background = element_rect(colour = bg_col, fill = bg_col),
          legend.position = "none")
  
  return(leg)
  
}

plot_combine <- function(lines, stream) {
  # arrange plots
  
  g <- grid.arrange(lines, stream, nrow = 1, widths = c(2, 1))
  
  return(g)
  
}

jscode <-
  '$(document).on("shiny:connected", function(e) {
  var jsWidth = window.innerWidth;
  Shiny.onInputChange("GetScreenWidth",jsWidth);
});
'

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  tags$script(jscode),
  
  # hide error message before text input provided
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "emotions.css")
  ),
  
  # Main panel for displaying outputs ----
  fluidPage(width = 12,
            theme = bs_theme(version = 4, bootswatch = "lux"),
            navbarPage(title = "Visual Emotions",
                       
                                sidebarLayout(
                                  sidebarPanel(width = 4,
                                               textAreaInput('poem_text', label = NULL, placeholder = "Paste the text from a poem here.", rows = 10),
                                               textInput('plot_title', label = NULL, placeholder = "Add a plot title (optional)"),
                                               submitButton(text = "Analyse Text")
                                  ),
                                  mainPanel(width = 8,
                                            plotOutput('poem_plot', width = "100%")
                                  )
                                )
                       
                       ,
            )
  )
)


# Define server
server <- function(input, output, session) {
  
  emotion_sum <- reactive ({ if(is.null(input$poem_text) == FALSE) {
    poem_split <- poem_split(poem_raw_df())
    poem_long <- poem_long(poem_split)
    poem_words <- poem_words(poem_long)
    emotion_sum <- emotion_df(poem_words)
    return(emotion_sum)
  } else {
    emotion_sum <- data.frame(x = c(rep(1, 8)),
                              emotion = c("joy", "trust", "surprise", "anticipation", "sadness", "fear", "anger", "disgust"),
                              value = c(rep(1, 8)))
    return(emotion_sum)
  } })
  
  plotTitle <- reactive ({ input$plot_title })
  
  poem_raw_df <- reactive({ if(is.null(input$poem_text) ==  FALSE) {return(input$poem_text)} }) 
  
  plotHeight <- reactive({ if(is.null(input$poem_text) ==  FALSE) {
    poem <- poem_raw_df()
    df <- poem_split(poem)
    lines <- max(df$line_id)
    height <- ifelse(50 + (lines * 12) > 400, 50 + (lines * 12), 400 )
    return(height)
    } })
  
  output$poem_plot <- renderPlot({
    if (is.null(poem_raw_df) == FALSE) {
    poem_split <- poem_split(poem_raw_df())
    poem_long <- poem_long(poem_split)
    poem_words <- poem_words(poem_long)
    poem_byline <- poem_byline(poem_long)
    stream <- plot_stream(poem_byline, poem_words)
    lines <- plot_lines(poem_words)
    
    g <- plot_combine(lines, stream)
    ptitle <- plotTitle()
    
    # plot title details
    null_title <- is.null(ptitle)
    breaks_title <- str_count(str_wrap(ptitle, 40), "\\\n")
    
    leg_height <- ifelse(null_title == TRUE, 50, 90 + ((breaks_title + 1) * 20))
    legend_plot <- plot_legend(poem_words, ptitle)
    total_height <- plotHeight()
    leg_prop <- leg_height / total_height
    main_prop <- (total_height - leg_height) / total_height
    
    g <- grid.arrange(legend_plot, g, nrow = 2, heights = c(leg_prop, main_prop))

    g
    }
  }, height = reactive({if(is.null(plotHeight()) == FALSE) {plotHeight()} else {0} }))
  
  
  observe({
    cat(input$GetScreenWidth)
  })
  
}

shinyApp(ui, server)
