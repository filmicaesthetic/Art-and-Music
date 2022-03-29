### Guitar Tab Analysis

library(rvest)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(showtext)

# import font from Google
font_add_google("Red Hat Mono", "Red Hat Mono")
showtext_auto()

# function for scraping text from webpage
scrape_data <- function (x) {
  y <- read_html(x) %>%
    html_text()
  return(y)
}

# capo value to be added
capo <- 5

# octave of notes
notes <- data.frame(notes = c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"))

# full potential range of guitar
full_scale <- data.frame(id = seq(1:(nrow(notes) * 6)),
                         notes = c(rep(notes$notes, 6)),
                         octave_id = c(rep(seq(1:6), each = nrow(notes)))
                         )

# standard tuning strings
guitar_strings <- data.frame(notes = c("E", "A", "D", "G", "B", "E"),
                             octave_id = c(2, 2, 3, 3, 3, 4)) %>% 
  left_join(full_scale, by = c("notes", "octave_id"))

# scrape data from guitartabs.cc
hallelujah <- scrape_data("https://www.guitartabs.cc/tabs/j/jeff_buckley/hallelujah_tab_ver_7.html")

# extract tab section
hallelujah_text <- sub('\\\tComments.*', "", sub('.*Artist:', "", hallelujah))
# extract individual strings
hallelujah_tab <- str_match_all(hallelujah_text, "\\\n\\|\\s*(.*?)\\s*\\|\\\r")
# combine with notes and octave data
hallelujah_df <- data.frame(tabs = hallelujah_tab[[1]][1:nrow(hallelujah_tab[[1]])],
           notes = rep(c("E", "A", "D", "G", "B", "E"), nrow(hallelujah_tab[[1]]) / 6),
           octave_id = rep(c(2, 2, 3, 3, 3, 4), nrow(hallelujah_tab[[1]]) / 6))

# separate out sections
intro <- hallelujah_df[1:36,] %>% mutate(section = "intro")
verse <- hallelujah_df[37:66,] %>% mutate(section = "verse")
chorus <- hallelujah_df[67:78,] %>% mutate(section = "chorus")
bridge <- hallelujah_df[79:90,] %>% mutate(section = "bridge")
interlude <- hallelujah_df[91:96,] %>% mutate(section = "interlude")
solo <- hallelujah_df[97:120,] %>% mutate(section = "solo")
outro <- hallelujah_df[121:138,] %>% mutate(section = "outro")

# combine sections into one df
whole_song <- rbind(intro, verse, chorus, verse, chorus, verse, chorus, verse, chorus, bridge, interlude, solo, outro)
# extract individual notes and calculate actual note played
whole_song_long <- whole_song %>% 
  mutate(tabs = str_extract_all(whole_song$tabs,"\\(?[0-9,.]+\\)?")) %>% 
  unnest(cols = tabs) %>%
  left_join(guitar_strings, by = c("notes", "octave_id")) %>%
  mutate(tabs = as.numeric(tabs),
         id = id + tabs + capo) %>%
  select(-notes, -octave_id) %>%
  left_join(full_scale, by = "id")

# plot notes
whole_song_long %>%
  group_by(notes, octave_id, section) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = notes, y = octave_id)) +
  geom_point(aes(size = n, color = section), alpha = 0.35, shape = 1, stroke = 3) +
  scale_size(range = c(1, 50)) +
  ylim(c(1, 5)) +
  coord_polar() +
  theme_minimal() +
  ggtitle("Hallelujah", subtitle = "by Jeff Buckley") +
  labs(y = "OCTAVE", x = "", color = "SECTION", caption = "data: guitartabs.cc") +
  guides(size = "none", color = guide_legend(title.position="top", title.hjust = 0.5)) +
  theme(legend.position = "bottom",
        text = element_text(family = "Red Hat Mono", size = 35),
        axis.title.y = element_text(face = "bold", size = 30, color = "#a87986", hjust = 0.72),
        plot.title = element_text(hjust = 0.5, size = 65),
        plot.subtitle = element_text(hjust = 0.5, size = 40),
        legend.title = element_text(face = "bold", size = 30, color = "#a87986", hjust = 0.72))

# save plot
ggsave("hallelujah.png", width = 6, height = 8, dpi = 300)
