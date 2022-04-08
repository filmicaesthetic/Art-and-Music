## Visualisations

# import font from Google
font_add_google("Red Hat Mono", "Red Hat Mono")
showtext_auto()

# palette

bgcolor <- "#e6dacf"
fgcolor <- "#192530"
palette <- c("A" = "#90486c", 
             "A#" = "#6c3b4d", 
             "B" = "#d9b239", 
             "C" = "#ca7e6c", 
             "C#" = "#a74a4a", 
             "D" = "#d17826", 
             "D#" = "#a35712", 
             "E" = "#3d667e", 
             "F" = "#53a578", 
             "F#" = "#3b6c58", 
             "G" = "#e06846", 
             "G#" = "#c14333")

output_height = 8

# polar plot notes
whole_song_long %>%
  group_by(notes, octave_id) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = notes, y = octave_id)) +
  geom_point(aes(size = n, color = notes), alpha = 0.35, shape = 1, stroke = output_height / 2) +
  scale_size(range = c(1, output_height * 5)) +
  scale_color_manual(values = palette) +
  ylim(c(1, 5)) +
  coord_polar() +
  theme_minimal() +
  ggtitle("Hallelujah", subtitle = "by Jeff Buckley") +
  labs(y = "OCTAVE", x = "", color = "SECTION", caption = "data: guitartabs.cc") +
  guides(size = "none", color = guide_legend(title.position="top", title.hjust = 0.5)) +
  theme(legend.position = "bottom",
        plot.background = element_rect(color = bgcolor, fill = bgcolor),
        panel.background = element_rect(color = bgcolor, fill = bgcolor),
        text = element_text(family = "Red Hat Mono", size = output_height * 4),
        axis.title.y = element_text(face = "bold", size = output_height * 4, color = "#6c3b4d", hjust = 0.72),
        plot.title = element_text(hjust = 0.5, size = output_height * 8),
        plot.subtitle = element_text(hjust = 0.5, size = output_height * 5),
        legend.title = element_text(face = "bold", size = output_height * 4, color = "#6c3b4d", hjust = 0.72))

# save plot
ggsave("hallelujah.png", width = output_height * 0.75, height = output_height, dpi = 300)



# animate strings
a <- whole_song_long %>%
  mutate(string = factor(string, levels = c("6", "5", "4", "3", "2", "1"))) %>%
  select(pos_id, string, tabs, tab_row, notes, section) %>%
  mutate(tabs = as.numeric(gsub(0, NA, tabs))) %>%
  ggplot(aes(x = string, y = tabs)) +
  geom_point(aes(color = notes), size = 20, alpha = 0.65, shape = 1, stroke = 8) + 
  annotate("rect", xmin = 6.5, xmax = 12, ymin = -Inf, ymax = Inf, fill = bgcolor, color = bgcolor) +
  annotate("text", x = 12, y = 4, label = "Hallelujah", size = 38, hjust = 1, color = fgcolor) +
  annotate("text", x = 12, y = 4.6, label = "BY JEFF BUCKLEY", size = 12, hjust = 1, color = fgcolor) +
  geom_col(aes(y = 12, fill = notes), width = 0.06, alpha = 0.65) +
  #scale_y_continuous(breaks = seq(1:max(whole_song_long$tabs)), limits = c(0, 12)) +
  scale_y_reverse(breaks = seq(max(whole_song_long$tabs):1), limits = c(12, 0)) +
  scale_color_manual(values = palette) +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  transition_states(pos_id, transition_length = 2) +
  shadow_wake(wake_length = 0.03) +
  exit_fade() +
  labs(y = "FRET", x = "", color = "NOTE\n") +
  guides(size = "none", fill = "none", color = guide_legend(title.position="top", title.hjust = 0.5)) +
  theme(legend.position = "right",
        text = element_text(family = "Red Hat Mono", size = 25, color = fgcolor),
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

# save animation
animate(a, height = 1800, width = 1300, fps = 6, duration = 60)
anim_save("outputs/hallelujah_x.gif")

library(cowplot)
