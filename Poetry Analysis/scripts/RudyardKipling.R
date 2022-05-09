## 
## Creative Analysis of If by Rudyard Kipling using
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
poem <- read.csv("~/R/Art & Music/Poetry Analysis/data/if.csv", quote='"')

# remove incorrectly formatted characters
poem_tidy <- poem %>% mutate(line = trimws(gsub("â€.", '', line)))

# split words to rows
poem_split <- poem_tidy %>%
  mutate(line_id = seq(1:nrow(poem_tidy))) %>%
  mutate(word = strsplit(gsub('[?]', ' ?', gsub('[!]', ' !', gsub('[^A-Za-z0-9?! ]', "", trimws(line)))), " "),
  ) %>%
  unnest(word)

# add word id column
poem_split <- poem_split %>%
  mutate(word_id = seq(1:nrow(poem_split)))

# calculate text stats
poem_stats <- poem_tidy %>% 
  mutate(line_id = seq(1:nrow(poem_tidy)),
         line_no_punc = gsub('[^A-Za-z0-9 ]', "", trimws(line)),
         chars = nchar(line),
         words = nchar(gsub('[^ ]', "", line_no_punc)) + 1,
         punc = nchar(gsub('[A-Za-z0-9 \t\n\r\v\f]', "", trimws(line))),
         comma = nchar(gsub('[^,]', "", line)),
         stop = nchar(gsub('[^.]', "", line)),
         hyphen = nchar(gsub('[^-]', "", line)),
         colon = nchar(gsub('[^:]', "", line)),
         semi = nchar(gsub('[^;]', "", line)),
         question = nchar(gsub('[^?]', "", line)),
         exclaim = nchar(gsub('[^!]', "", line)),
         quote = nchar(gsub('[^"]', "", line)))

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

# aggregate by line
poem_byline <- poem_long %>% 
  group_by(line, line_id, emotion, word_id) %>% 
  summarise(value = sum(value)) %>% 
  filter(value > 0) %>% 
  group_by(line_id) %>% 
  mutate(test = value / sum(value)) %>%
  select(-line)

# aggregate by word
poem_byword <- poem_long %>% 
  group_by(line, line_id, emotion, word, word_id) %>% 
  summarise(value = sum(value)) %>% 
  filter() %>% 
  group_by(word) %>% 
  mutate(test = value / sum(value)) %>%
  select(-line, -value) %>%
  pivot_wider(id_cols= c(line_id, word, word_id), names_from = emotion, values_from = test) 

# additional stats
poem_words <- poem_byword %>% 
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
poem_words[is.na(poem_words)] <- 0

# colours
anticipation_col <- "#47bf6d" 
sadness_col <- "#5082a3"
fear_col <- "#d6db40"
anger_col <- "#e34252"
trust_col <- "#6661ed"
bg_col <- "#f7f7f5"
block_col <- "#d4d0cb"

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

# create custom colour palette
pal <- c("anticipation" = "#47bf6d", 
         "trust" = "#6661ed", 
         "anger" = "#e34252")

# emotion bar chart
emotion_sum %>% mutate(emotion = fct_reorder(emotion, -value)) %>% 
  ggplot(aes(x = emotion, y = value)) +
  geom_col(aes(fill = emotion), color = bg_col) +
  theme_minimal() +
  scale_fill_manual(values = pal, na.value = "#616161") +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(color = bg_col, fill = bg_col),
        plot.background = element_rect(colour = bg_col, fill = bg_col),
        legend.position = "none")

ggsave("poem_emotion_totals.png")

poem_words %>% arrange(word_id)

# poem chart
lines <- poem_words %>%
  ggplot(aes(x = line_id, y = chars)) +
  geom_col(data = poem_words %>% arrange(line_id, word_id), fill = block_col, color = bg_col) +
  geom_col(data = poem_words %>% arrange(line_id, word_id) %>% mutate(anticipation = ifelse(is.na(anticipation) == TRUE, 0, anticipation)), aes(alpha = anticipation), fill = anticipation_col, color = bg_col) +
  geom_col(data = poem_words %>% arrange(line_id, word_id) %>% mutate(trust = ifelse(is.na(trust) == TRUE, 0, trust)), aes(alpha = trust), fill = trust_col, color = bg_col) +
  geom_col(data = poem_words %>% arrange(line_id, word_id) %>% mutate(anger = ifelse(is.na(anger) == TRUE, 0, anger)), aes(alpha = anger), fill = anger_col, color = bg_col) +
  geom_point(data = poem_words %>% arrange(line_id, word_id) %>% filter(anger > 0.2), aes(y = char_sum - (chars / 2), size = anger, alpha = anger), shape = 0, stroke = 1.5, color = anger_col) +
  geom_point(data = poem_words %>% arrange(line_id, word_id) %>% filter(trust > 0.2), aes(y = char_sum - (chars / 2), size = trust, alpha = trust), shape = 0, stroke = 1.5, color = trust_col) +  
  geom_point(data = poem_words %>% arrange(line_id, word_id) %>% filter(anticipation > 0.2), aes(y = char_sum - (chars / 2), size = anticipation, alpha = anticipation), shape = 0, stroke = 1.5, color = anticipation_col) +
  geom_smooth(data = poem_words %>% arrange(line_id, word_id) %>% filter(anticipation > 0.15), aes(y = char_sum - (chars / 2), size = anticipation / 2), color = anticipation_col, se = FALSE, method = "lm", formula = y ~ poly(x, 4)) +
  geom_smooth(data = poem_words %>% arrange(line_id, word_id) %>% filter(trust > 0.15), aes(y = char_sum - (chars / 2), size = trust / 2), color = trust_col, se = FALSE, method = "lm", formula = y ~ poly(x, 5)) +
  geom_smooth(data = poem_words %>% arrange(line_id, word_id) %>% filter(anger > 0.15), aes(y = char_sum - (chars / 2), size = anger / 2), color = anger_col, se = FALSE, method = "lm", formula = y ~ poly(x, 5)) +
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
        plot.margin=unit(c(0.1,0.1,0.1,-0.5), "cm")
        )

#prepare line data for streamgraph

poem_line <- data.frame(line_id = rep(seq(1:max(poem_byline$line_id)), 3),
                        emotion = rep(c("anger", "anticipation", "trust"), each = max(poem_byline$line_id)))

poem_line <- poem_line %>% left_join(poem_byline, by = c("line_id", "emotion")) %>% group_by(line_id, emotion) %>% summarise(value = sum(test))

poem_line[is.na(poem_line)] <- 0

# streamgraph
stream <- poem_line %>%
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

ggsave("Poetry Analysis/outputs/if.png", g,  width = 5, height = 7, dpi = 300)

# punctuation stats

emo_punc <- poem_line %>% left_join(poem_stats %>% select(line_id, punc, comma, stop, question, exclaim, quote, semi, colon), by = "line_id")

sum(poem_stats$colon)

emo_punc %>%
  filter(colon > 0) %>%
  ggplot(aes(x = emotion, y = value)) +
  geom_col(aes(fill = emotion))

# word stats

word_score <- poem_byword %>% 
  group_by(word) %>% 
  summarise(
    trust = sum(trust, na.rm = TRUE),
    anger = sum(anger, na.rm = TRUE),
    anticipation = sum(anticipation, na.rm = TRUE),
    total = trust + anger + anticipation
  ) %>%
  arrange(-total)

