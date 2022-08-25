# whole poem process function

poem_raw <- "I wanna talk to you
The last time we talked, Mr. Smith, you reduced me to tears
I promise you it won't happen again
 
Do I attract you? Do I repulse you with my queasy smile?
Am I too dirty, am I too flirty? Do I like what you like?
I could be wholesome, I could be loathsome, I guess I'm a little bit shy
Why don't you like me, why don't you like me, without making me try?
 
I tried to be like Grace Kelly, mmh
But all her looks were too sad, aah
So I tried a little Freddie, mmh
I've gone identity mad!
 
I could be brown, I could be blue, I could be violet sky
I could be hurtful, I could be purple, I could be anything you like
Gotta be green, gotta be mean, gotta be everything more
Why don't you like me, why don't you like me?
Why don't you walk out the door?
 
Getting angry doesn't solve anything
 
How can I help it, how can I help it? How can I help what you think?
Hello my baby, hello my baby, putting my life on my brink
Why don't you like me, why don't you like me? Why don't you like yourself?
Should I bend over, should I look older, just to be put on your shelf?
 
I tried to be like Grace Kelly, mmh
But all her looks were too sad, aah
So I tried a little Freddie, mmh
I've gone identity mad!
 
I could be brown, I could be blue, I could be violet sky
I could be hurtful, I could be purple, I could be anything you like
Gotta be green, gotta be mean, gotta be everything more
Why don't you like me, why don't you like me?
Walk out the door!
 
Say what you want to satisfy yourself, hey
But you only want what everybody else says you should want
You want
 
I could be brown, I could be blue, I could be violet sky
I could be hurtful, I could be purple, I could be anything you like
Gotta be green, gotta be mean, gotta be everything more
Why don't you like me, why don't you like me?
Walk out the door!
 
I could be brown, I could be blue, I could be violet sky
I could be hurtful, I could be purple, I could be anything you like
Gotta be green, gotta be mean, gotta be everything more
Why don't you like me, why don't you like me?
Walk out the door!
 
Uuh, ah
Humphry, we're leaving
Ca-ching!"

poem_raw_df <- data.frame(poem_text = c(poem_raw))

poem <- poem_raw_df %>%
  unnest_lines(line, poem_text) %>%
  mutate(line_id = 1:n())

# remove incorrectly formatted characters
poem_tidy <- poem %>% mutate(line = gsub("â€.", '', line))

# split words to rows
poem_split <- poem_tidy %>%
  mutate(line_id = seq(1:nrow(poem_tidy))) %>%
  mutate(word = strsplit(gsub('[?]', ' ?', gsub('[!]', ' !', gsub('[^A-Za-z0-9?! ]', "", line))), " "),
  ) %>%
  unnest(word)

# add word id column
poem_split <- poem_split %>%
  mutate(word_id = seq(1:nrow(poem_split)))

# # calculate text stats
# poem_stats <- poem_tidy %>% 
#   mutate(line_id = seq(1:nrow(poem_tidy)),
#          line_no_punc = gsub('[^A-Za-z0-9 ]', "", trimws(line)),
#          chars = nchar(line),
#          words = nchar(gsub('[^ ]', "", line_no_punc)) + 1,
#          punc = nchar(gsub('[A-Za-z0-9 \t\n\r\v\f]', "", trimws(line))),
#          comma = nchar(gsub('[^,]', "", line)),
#          stop = nchar(gsub('[^.]', "", line)),
#          hyphen = nchar(gsub('[^-]', "", line)),
#          colon = nchar(gsub('[^:]', "", line)),
#          semi = nchar(gsub('[^;]', "", line)),
#          question = nchar(gsub('[^?]', "", line)),
#          exclaim = nchar(gsub('[^!]', "", line)),
#          quote = nchar(gsub('[^"]', "", line)))

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
  mutate(test = value) %>%
  select(-line)

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
  arrange(-value) %>%
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
        plot.margin=unit(c(0.1,0.1,0.1,-0.5), "cm")
  )

#prepare line data for streamgraph

poem_line <- data.frame(line_id = rep(seq(1:max(poem_long$line_id)), 3),
                        emotion = rep(c(as.character(emo_1), as.character(emo_2), as.character(emo_3)), each = max(poem_long$line_id)))

poem_line <- poem_line %>% 
  left_join(poem_byline, by = c("line_id", "emotion")) %>% 
  group_by(line_id, emotion) %>% 
  summarise(value = sum(test))

poem_line[is.na(poem_line)] <- 0

library(ggimage)


# streamgraph
stream <- poem_line %>%
  ggplot(aes(x = line_id, y = value)) +
  geom_stream(aes(fill = as.factor(emotion)), type = "ridge", bw = 0.65) +
  #geom_image(data = twit_df, aes(image=image)) +
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

return(g)

poem_text_to_plot <- function(poem_raw) {
  
  
  
}

## LEGEND

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
  #mutate(value = value + (row_number() / 9999)) %>%
  mutate(emotion = fct_reorder(emotion, value, sum)) %>% 
  ggplot(aes(x = 1, y = value)) +
  geom_col(aes(fill = emotion), color = bg_col, position = position_stack()) +
  geom_text(aes(label = label, group = emotion), position = position_stack(vjust = 0.9), hjust = 1, fontface="bold", color = "white", family = "Open Sans") +
  theme_minimal() +
  coord_flip() +
  ggtitle(paste(str_wrap("plot_title", 40))) +
  scale_fill_manual(values = pal, na.value = block_col) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = 16, hjust = 0.5, family = "Open Sans", face = "bold", margin = margin(t = 20, b = 20, unit = "pt")),
        panel.background = element_rect(color = bg_col, fill = bg_col),
        plot.background = element_rect(colour = bg_col, fill = bg_col),
        legend.position = "none")
leg
