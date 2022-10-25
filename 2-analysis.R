### Purpose ### 

# This file contains the code to clean and analyze the 2000 NY Times articles collected 
# mentioning the term 'Palestine' in the first Intifada 

# Set up 

library(dplyr)
library(quanteda)
library(quanteda.textplots)
library(ggplot2)
library(lubridate)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data Cleaning #

articles <- read.csv('../articles.csv')

keywords <- read.csv('../keywords.csv')

articles <- articles %>% 
  select(response.docs.pub_date, response.docs.abstract, 
         response.docs.lead_paragraph, response.docs.keywords, 
         response.docs.word_count, 
         response.docs.headline.main, response.docs.byline.original) %>% # Select the needed columns
  mutate(response.docs.pub_date = 
           lubridate::ymd(stringr::str_extract(response.docs.pub_date, '\\d{4}-\\d{2}-\\d{2}')))  #To convert chr to date

articles$response.meta.hits[1] # 3371

# Through the response$meta.hits feature, we can see that there were a total of 
# 3371 articles published by NYT during the given period mentioning the term 
# "Palestine". Out of which, we will study 2000.

# Analysis #

# We first analyze the tagged keywords by NYT. 


keywords <- keywords %>% 
  select(name, value) %>%
  group_by(value, name) %>%
  summarize(n = n()) %>% 
  arrange(desc(n)) #Group keywords by terms and then count their occurence overall 

keywords %>% 
  head(10)

keywords %>% 
  filter(value == 'Israel') %>% 
  select(n) # 12

keywords %>% 
  filter(value == 'Palestine') %>% 
  select(n) # 0

# We notice that, interestingly, whereas Israel appears in the keywords 
# provided by NYT 12 times, Palestine appears 0 times. However, 
# West Bank(a disputed territory back then) appears a total of 118 times.


articles <- articles %>% 
  select(!response.docs.keywords) 

corp_headlines <- corpus(articles, text_field = "response.docs.headline.main") #Create a corpus of headlines for textual analysis.
corp_headlines <- tokens(corp_headlines)

# Wordcloud of top features 

dfm <- dfm(corp_headlines, remove_punct = TRUE, remove = stopwords('en'))
top_words <- as.data.frame(topfeatures(dfm, 10))
top_words <- top_words %>% 
  tibble::rownames_to_column("name") 
colnames(top_words) = c('Word', 'value')
top_words <- top_words %>% arrange(desc(value))
top_words$Word <- factor(top_words$Word, levels = top_words$Word)

textplot_wordcloud(dfm, min_count = 20, random_order = FALSE,
                   rotation = .25, 
                   colors = RColorBrewer::brewer.pal(10, "Spectral"))

# Headlines 

p_headlines <- ggplot(top_words) + 
  geom_col(aes(x= Word, y = value)) + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(), 
    axis.title.y = element_blank(), 
    axis.title.x = element_blank(), 
    axis.ticks.x = element_blank()
  )

p_headlines


# 'PLO' appears to be used most in headlines. PLO refers to the Palestinian Liberation 
# Organization. We use the kwic function to assess in what contexts some of these 
# words have been used. The organization was one of the main actors in Palestine 
# freedom during the period. Its leader was Yasser Arafat. We also see that US and 
# Israel have been among top3 mentioned words, however there is still no mention of "palestine". 

# A key component of how social media can shape discourse is through 
# legitimizing certain narratives, specially without a democratic process. 
# The reluctance can be indicative of the paper being averse to 'recognize' Palestine's autonomy. 

# We will make use of the kwic(keywords in context) function of quanteda to 
# analyze the context in which terms "plo" and "israel" appear together with 
# the term "peace" after or before the post. 


plo <- data.frame(kwic(corp_headlines, "p.l.o")) #Extract context for term plo
israel <- data.frame(kwic(corp_headlines, 'israel')) #Extract context for term israel

plo %>% 
  mutate(peace = ifelse(grepl('peace', tolower(pre), fixed = TRUE), 1, 0)) %>% 
  filter(peace == 1)

plo %>% 
  mutate(peace = ifelse(grepl('peace', tolower(post), fixed = TRUE), 1, 0)) %>% 
  filter(peace == 1)

israel %>% 
  mutate(peace = ifelse(grepl('peace', tolower(post), fixed = TRUE), 1, 0)) %>% 
  filter(peace == 1)


israel %>% 
  mutate(peace = ifelse(grepl('peace', tolower(pre), fixed = TRUE), 1, 0)) %>% 
  filter(peace == 1)


# In all of the dataframes, we note that the word "peace" has mostly occured within 
# Israel's context. We also see that there is no mention of Israel as the active actor 
# in establishing peace with PLO, its always the reverse.

# We further use kwic to understand US' involvement in the given period.

us <- kwic(corp_headlines, "u.s") 

us %>% 
  mutate(peace = ifelse(grepl('peace', tolower(pre), fixed = TRUE), 1, 0)) %>% 
  filter(peace == 1)

us %>% 
  mutate(peace = ifelse(grepl('peace', tolower(post), fixed = TRUE), 1, 0)) %>% 
  filter(peace == 1)

# We see increasing evidence that the US and its involvement in the 
# conflict has been associated by its role as a 'peace negotiator'. 
# The use of words like Plans in US peace context suggest the same.

# Let's move our analysis to the lead paragraphs of articles.

corp_leadpara <- corpus(articles, text_field = "response.docs.lead_paragraph") # Lead para

dates <- articles$response.docs.pub_date
authors <- articles$response.docs.byline.original

docvars(corp_leadpara, "Date") <- dates
docvars(corp_leadpara, "Authors") <- authors

corpus_leadpara_token <- tokens(corp_leadpara) %>%  
  tokens_remove(pattern = stopwords("en")) %>%
  tokens_group(groups = Authors)


Authors_df <- data.frame(summary(corpus_leadpara_token)) %>%
  filter(!Freq == 'character') %>%
  arrange(desc(Freq)) %>% 
  head(10) #Most engaged authors


Authors_list <- Authors_df %>% 
  select(Var1)

by_author_dfm <- dfm(corpus_leadpara_token)


peace_by_author <- function(x, dfm){
  
  # Number of times each author talks about peace 
  
  by_author_dfm = dfm
  im <- as.data.frame(dfm_sort(by_author_dfm)) %>% 
    filter(doc_id == x) %>% 
    select(doc_id, peace)
  return(im)
}


top_10_authors_peace <- bind_rows(lapply(Authors_list$Var1, peace_by_author, by_author_dfm))
top_10_authors_peace$doc_id <- factor(top_10_authors_peace$doc_id, levels = top_10_authors_peace$doc_id)
top_10_authors_peace$doc_id <- forcats::fct_rev(top_10_authors_peace$doc_id)


ggplot(top_10_authors_peace) +
  geom_col(aes(x = doc_id, y = peace)) + 
  coord_flip() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(), 
    axis.title.y = element_blank(), 
    axis.title.x = element_blank(), 
    axis.ticks.x = element_blank(), 
    axis.ticks.y = element_blank(), 
    plot.title = element_text(size = 10)
  ) + 
  labs(title = 'Most engaged authors and their mentions of peace') 


# Using the date and lead paragraphs features, we first make a list of authors 
# that have the most written pieces during the given period. 
# This plot shows how many times each of these authors have mentioned the word "peace" in 
# their lead paragraphs. 

# From the graph, we can see that Clyde Haberman and Alan Cowell have mentioned 
# the word the most in their lead paragraphs. An obvious limitation of this 
# analysis(and the report) by large is that we can not see the actual content of 
# articles, however, the assumption is that peace ought to be an important keyword 
# in times of violence, and its mention in the lead paragraph can indicate further 
# information about the tone/subject of the article.

# We will now analyze the texts by date : 

corpus_leadpara_token_date <- tokens(corp_leadpara) %>%  
  tokens_remove(pattern = stopwords("en")) %>%
  tokens_group(groups = Date)

date_dfm <- dfm(corpus_leadpara_token_date)

by_date_dfm <- as.data.frame(date_dfm)

by_date_dfm <- by_date_dfm %>% 
  select(doc_id, peace, arafat) %>% 
  mutate(doc_id = ymd(doc_id)) %>% 
  group_by(year = year(doc_id), month = month(doc_id)) %>% 
  summarise(peace = sum(peace), arafat = sum(arafat)) %>% 
  mutate(date = paste0(year,'-',month)) %>% 
  mutate(date = ym(date))


p_date_keywords <- ggplot(by_date_dfm) +
  geom_line(aes(x = date, y = peace), color = "green") + 
  geom_line(aes(x = date, y = arafat), color = "blue") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(), 
    axis.title.y = element_blank(), 
    axis.title.x = element_blank(), 
    axis.line.x = element_line(), 
    axis.ticks.y = element_blank()) + 
  labs(title = "Mention of key terms in NYT with time.",
       subtitle = "Green line represents 'peace' and Blue line represents 'Arafat'") 

p_date_keywords

# While we see that the mention of peace has been relatively consistent 
# during the period, we see a period of significant boom in the mention of 'Arafat' 
# after which the mentions decrease. This could suggest a period of 'transition' in 
# the way peace was reported. Initially, when the protests had just begun, 
# the spotlight placed on leaders like Arafat may have been significant, however, as 
# time went by, peace for Palestine may have become a 'compromise', 
# without much spotlight in media.

corpus_leadpara_token_year <- tokens(corp_leadpara) %>% 
  tokens_remove(pattern = stopwords("en")) %>%
  tokens_group(groups = year(Date))

date_dfm_year <- dfm(corpus_leadpara_token_year)

dfm_weight_date <- date_dfm_year %>% 
  dfm_weight(scheme = "prop") 

prop <- as.data.frame(dfm_weight_date) 

p_date_freq <- ggplot(prop) + 
  geom_col(aes(x = doc_id, y = peace), fill = "green") + 
  geom_col(aes(x = doc_id, y = arafat), fill = "blue") + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(), 
    axis.title.y = element_blank(), 
    axis.title.x = element_blank(), 
    axis.ticks.x = element_blank()
  ) + 
  labs(title = 'Feature frequency with year', 
       caption = "Green line represents 'peace' and Blue line represents 'Arafat'")

p_date_freq


# The stacked bar graph complements the above observation, and we can see a 
# visual shift in the discourse. 

### Conclusion 

# The main thrust of this paper was to provide an 
# overview of interesting insights from textual analysis of 
# 2000 most popular NYTimes articles collected containing the term 
# 'Palestine'. Whereas the scope of this analysis was not to infer 
# insights regarding tone and sentiments, we can see interesting results 
# highlighted through text analysis and visualizations, that need further 
# structuring with more rhobust methods. 
