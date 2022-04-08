### Guitar Tab Analysis

library(rvest)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(showtext)
library(gganimate)
library(forcats)

# function for scraping text from webpage
scrape_data <- function (x) {
  y <- read_html(x) %>%
    html_text()
  return(y)
}

# octave of notes
notes <- data.frame(notes = c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"))

# full potential range of guitar
full_scale <- data.frame(id = seq(1:(nrow(notes) * 6)),
                         notes = c(rep(notes$notes, 6)),
                         octave_id = c(rep(seq(1:6), each = nrow(notes)))
                         )

# standard tuning strings
guitar_strings <- data.frame(notes = c("E", "B", "G", "D", "A", "E"),
                             octave_id = c(4, 3, 3, 3, 2, 2)) %>% 
  left_join(full_scale, by = c("notes", "octave_id"))


# url of tab
tab_url <- "https://www.guitartabs.cc/tabs/j/jeff_buckley/hallelujah_tab_ver_7.html"

# scrape data from guitartabs.cc
hallelujah <- scrape_data(tab_url)

# extract tab section
hallelujah_text <- sub('\\\tComments.*', "", sub('.*show chord diagrams', "", hallelujah))
# extract individual strings
hallelujah_tab <- str_match_all(hallelujah_text, "\\\n(.*?)\\-\\s*(.*?)\\s*\\|\\\r")
# extract capo
capo_text <- str_match_all(hallelujah_text, "Capo*(.*?)\\s*\\\r")

if (is.na(str_count(capo_text[[1]][1])) == FALSE) {
  capo <- as.numeric(str_extract(capo_text,"\\(?[0-9,.]+\\)?"))} else { 
    capo <- 0}

#count of tab rows
tab_count <- nrow(hallelujah_tab[[1]]) / 6
# combine with notes and octave data
hallelujah_df <- data.frame(tabs = hallelujah_tab[[1]][1:nrow(hallelujah_tab[[1]])],
           notes = rep(c("E", "A", "D", "G", "B", "E"), tab_count),
           string = rep(c("1", "2", "3", "4", "5", "6"), tab_count),
           tab_row = (rep(c(seq(1:tab_count)), each = 6)),
           octave_id = rep(c(2, 2, 3, 3, 3, 4), tab_count))

# separate out sections
intro <- hallelujah_df[1:36,] %>% mutate(section = "intro")
verse <- hallelujah_df[37:66,] %>% mutate(section = "verse")
chorus <- hallelujah_df[67:78,] %>% mutate(section = "chorus")
bridge <- hallelujah_df[79:90,] %>% mutate(section = "bridge")
interlude <- hallelujah_df[91:96,] %>% mutate(section = "interlude")
solo <- hallelujah_df[97:120,] %>% mutate(section = "solo")
outro <- hallelujah_df[121:138,] %>% mutate(section = "outro")

# combine sections into one df
whole_song <- rbind(intro, verse, chorus, bridge, interlude, solo, outro)
# extract individual notes and calculate actual note played
whole_song_long <- whole_song %>% 
  mutate(tab_raw = tabs,
         tabs = str_extract_all(whole_song$tabs,"\\(?[0-9,.]+\\)?")) %>% 
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


