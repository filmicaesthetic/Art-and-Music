## 
## Creative Analysis of The Raven by Edgar Allan Poe using
## NRC Word-Emotion Association Lexicon: https://www.saifmohammad.com
##

library(dplyr)
library(tidytext)
library(tidyr)
library(data.table)
library(ggplot2)
library(forcats)
library(ggstream)
library(gridExtra)
library(cowplot)

# import poem data
the_raven <- read.csv("~/R/Art & Music/data/Poetry Analysis/the_raven.csv", quote="'")

# remove incorrectly formatted characters
the_raven_tidy <- the_raven %>% mutate(line = trimws(gsub("â€.", '', line)))

# split words to rows
raven_split <- the_raven_tidy %>%
  mutate(line_id = seq(1:nrow(the_raven_tidy))) %>%
  mutate(word = strsplit(gsub('[?]', ' ?', gsub('[!]', ' !', gsub('[^A-Za-z0-9?! ]', "", trimws(line)))), " "),
  ) %>%
  unnest(word)

# add word id column
raven_split <- raven_split %>%
  mutate(word_id = seq(1:nrow(raven_split)))

# calculate text stats
raven_stats <- the_raven_tidy %>% 
  mutate(line_id = seq(1:nrow(the_raven_tidy)),
         line_no_punc = gsub('[^A-Za-z0-9 ]', "", trimws(line)),
         chars = nchar(line),
         words = nchar(gsub('[^ ]', "", line_no_punc)) + 1,
         punc = nchar(gsub('[A-Za-z0-9 \t\n\r\v\f]', "", trimws(line))),
         comma = nchar(gsub('[^,]', "", line)),
         stop = nchar(gsub('[^.]', "", line)),
         hyphen = nchar(gsub('[^-]', "", line)),
         question = nchar(gsub('[^?]', "", line)),
         exclaim = nchar(gsub('[^!]', "", line)),
         quote = nchar(gsub('[^"]', "", line)))

# import NRC Word-Emotion Association Sentiment Lexicon
nrc_all <- get_sentiments("nrc")

# identify words that appear in the emotion lexicon
raven_sent <- raven_split %>% 
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
raven_sent[is.na(raven_sent)] <- 0

# reshape the data frame
raven_long <- melt(setDT(raven_sent), id.vars = c("line","line_id","word", "word_id"), variable.name = "emotion")

# aggregate by line
raven_byline <- raven_long %>% 
  group_by(line, line_id, emotion, word_id) %>% 
  summarise(value = sum(value)) %>% 
  filter(value > 0) %>% 
  group_by(line_id) %>% 
  mutate(test = value / sum(value)) %>%
  select(-line)

# aggregate by word
raven_byword <- raven_long %>% 
  group_by(line, line_id, emotion, word, word_id) %>% 
  summarise(value = sum(value)) %>% 
  filter() %>% 
  group_by(word) %>% 
  mutate(test = value / sum(value)) %>%
  select(-line, -value) %>%
  pivot_wider(id_cols= c(line_id, word, word_id), names_from = emotion, values_from = test) 

# additional stats
raven_words <- raven_byword %>% 
  arrange(line_id, word_id) %>%
  mutate(word_no_punc = gsub('[^A-Za-z0-9 ]', "", trimws(word)),
         chars = nchar(word),
         words = nchar(gsub('[^ ]', "", word_no_punc)) + 1,
         punc = nchar(gsub('[A-Za-z0-9 \t\n\r\v\f]', "", trimws(word))),
         comma = nchar(gsub('[^,]', "", word)),
         question = nchar(gsub('[^?]', "", word)),
         exclaim = nchar(gsub('[^!]', "", word)),
         quote = nchar(gsub('[^"]', "", word))) %>%
  group_by(line_id) %>%
  mutate(char_sum = cumsum(chars))

# replace NAs
raven_words[is.na(raven_words)] <- 0

# colours
anticipation_col <- "#47bf6d" 
sadness_col <- "#5082a3"
fear_col <- "#d6db40"
bg_col <- "#2e2e2e"
block_col <- "#616161"

# identify highest scoring emotions
emotion_sum <- raven_words %>% mutate(x = 1) %>% group_by(x) %>% summarise(joy = sum(joy, na.rm = TRUE),
                                         trust = sum(trust, na.rm = TRUE),
                                         surprise = sum(surprise, na.rm = TRUE),
                                         anticipation = sum(anticipation, na.rm = TRUE),
                                         sadness = sum(sadness, na.rm = TRUE),
                                         fear = sum(fear, na.rm = TRUE),
                                         anger = sum(anger, na.rm = TRUE),
                                         disgust = sum(disgust, na.rm = TRUE)) %>%
  pivot_longer(-x, names_to = "emotion", values_to = "value") %>%
  arrange(-value)

# create custom colour palette
pal <- c("anticipation" = "#47bf6d", 
         "sadness" = "#5082a3", 
         "fear" = "#d6db40")

# emotion bar chart
emotion_sum %>% mutate(emotion = fct_reorder(emotion, -value)) %>% ggplot(aes(x = emotion, y = value)) +
  geom_col(aes(fill = emotion)) +
  theme_minimal() +
  scale_fill_manual(values = pal, na.value = "#616161") +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(color = bg_col, fill = bg_col),
        plot.background = element_rect(colour = bg_col, fill = bg_col),
        legend.position = "none")

ggsave("emotion_totals.png")

# poem chart

lines <- raven_words %>% ggplot(aes(x = line_id, y = chars)) +
  geom_col(aes(), fill = block_col, color = bg_col) +
  geom_col(data = raven_words %>% mutate(anticipation = ifelse(is.na(anticipation) == TRUE, 0, anticipation)), aes(alpha = anticipation), fill = anticipation_col, color = bg_col) +
  geom_col(data = raven_words %>% mutate(sadness = ifelse(is.na(sadness) == TRUE, 0, sadness)), aes(alpha = sadness), fill = sadness_col, color = bg_col) +
  geom_col(data = raven_words %>% mutate(fear = ifelse(is.na(fear) == TRUE, 0, fear)), aes(alpha = fear), fill = fear_col, color = bg_col) +
  geom_point(data = raven_words %>% filter(fear > 0.2), aes(y = char_sum - (chars / 2), size = fear, alpha = fear), shape = 0, stroke = 1.5, color = fear_col) +
  geom_point(data = raven_words %>% filter(sadness > 0.2), aes(y = char_sum - (chars / 2), size = sadness, alpha = sadness), shape = 0, stroke = 1.5, color = sadness_col) +  
  geom_point(data = raven_words %>% filter(anticipation > 0.2), aes(y = char_sum - (chars / 2), size = anticipation, alpha = anticipation), shape = 0, stroke = 1.5, color = anticipation_col) +
  geom_smooth(data = raven_words %>% arrange(line_id, word_id) %>% filter(anticipation > 0.2), aes(y = char_sum - (chars / 2), size = anticipation / 2), color = anticipation_col, se = FALSE, method = "lm", formula = y ~ poly(x, 4)) +
  geom_smooth(data = raven_words %>% arrange(line_id, word_id) %>% filter(sadness > 0.2), aes(y = char_sum - (chars / 2), size = sadness / 2), color = sadness_col, se = FALSE, method = "lm", formula = y ~ poly(x, 15)) +
  geom_smooth(data = raven_words %>% arrange(line_id, word_id) %>% filter(fear > 0.2), aes(y = char_sum - (chars / 2), size = fear / 2), color = fear_col, se = FALSE, method = "lm", formula = y ~ poly(x, 8)) +
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
        plot.margin=unit(c(0.1,0.1,0.1,-0.5), "cm"))

#prepare line data for streamgraph

raven_line <- data.frame(line_id = rep(seq(1:max(raven_byline$line_id)), 3),
                         emotion = rep(c("fear", "anticipation", "sadness"), each = max(raven_byline$line_id)))

raven_line <- raven_line %>% left_join(raven_byline, by = c("line_id", "emotion")) %>% group_by(line_id, emotion) %>% summarise(value = sum(test))

raven_line[is.na(raven_line)] <- 0

# streamgraph
stream <- raven_line %>%
  ggplot(aes(x = line_id, y = value)) +
  geom_stream(aes(fill = as.factor(emotion)), type = "ridge", bw = 0.65) +
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

# arrange plots

g <- grid.arrange(lines, stream, nrow = 1, widths = c(2, 1))

ggsave("Poetry Analysis/outputs/the_raven.png", g,  width = 5, height = 7, dpi = 300)


# punctuation stats

emo_punc <- raven_line %>% left_join(raven_stats %>% select(line_id, punc, comma, stop, question, exclaim, quote), by = "line_id")

emo_punc %>%
  filter(quote > 0) %>%
  ggplot(aes(x = emotion, y = value)) +
  geom_col(aes(fill = emotion))

# word stats

word_score <- raven_byword %>% 
  group_by(word) %>% 
  summarise(
    sadness = sum(sadness, na.rm = TRUE),
    fear = sum(fear, na.rm = TRUE),
    anticipation = sum(anticipation, na.rm = TRUE),
    total = sadness + fear + anticipation
  ) %>%
  arrange(-total)
  
