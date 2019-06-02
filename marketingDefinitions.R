## The Devil is in the Data
## Defining Marketing with the Rvest and Tidytext Packages
## https://lucidmanager.org/rvest-and-tidytext/

library(tidyverse)
library(rvest)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(topicmodels)
lm_palette <- c("#6A6A9D", "#FFAB4C", "#E82A44")

## Scrape definitions from website
definitions <- read_html("https://heidicohen.com/marketing-definition/") %>%
    html_nodes("ol li") %>%
    html_text() %>%
    enframe() %>%
    mutate(No = 1:nrow(.)) %>%
    select(No, Definition = value)

## bag of words
def_words <- definitions[1:72, ] %>%
    unnest_tokens(word, Definition) %>%
    mutate(word = gsub("s$", "", word))

## Word frequencies
words_freq <- def_words %>%
    anti_join(stop_words) %>%
    count(word) %>%
    filter(!(word %in%
             c("marketing", "vice", "president", "chief", "executive", "’")))

pdf("marketingcloud.pdf")
wordcloud(words_freq$word, words_freq$n, max.words = 40, colors = lm_palette)
dev.off()

word_freq %>%
    top_n(10) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) + geom_col(fill = lm_palette[2]) +
    coord_flip() +
    theme(text = element_text(size=20))

ggsave("marketingtop10words.png", dpi = 300)

## Topic models
marketing_dtm <- word_freq %>%
    mutate(doc = 1) %>%
    cast_dtm(doc, word, n)

marketing_lda <- LDA(marketing_dtm, k = 4) %>%
    tidy(matrix = "beta") %>%
    group_by(topic) %>%
    top_n(5, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

marketing_lda %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    scale_fill_manual(values = lm_palette[-1:-2]) + 
       facet_wrap(~topic, scales = "free") +
       coord_flip() +
       theme(text = element_text(size = 20))

ggsave("MarketingTopics.png", dpi = 300)
