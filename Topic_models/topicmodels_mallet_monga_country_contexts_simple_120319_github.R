library(dplyr)
library(tidyr)

# Load LDAvis inputs

#' Note: data witheld because they contain third party content

setwd("C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Deforestation review/Deforestation_messaging_analysis_GitHub/Deforestation_messaging_analysis/")

load("Data/mongabay_LDAVIS_inputs.Rdata")

#' @param theta matrix, with each row containing the probability distribution
#' over topics for a document, with as many rows as there are documents in the
#' corpus, and as many columns as there are topics in the model.
#' @param doc.length integer vector containing the number of tokens in each
#' document of the corpus.

# compute counts of tokens across K topics (length-K vector):
# (this determines the areas of the default topic circles when no term is 
# highlighted)
topic.frequency <- colSums(theta * doc.length)
topic.proportion <- topic.frequency/sum(topic.frequency)

#' @param phi matrix, with each row containing the distribution over terms 
#' for a topic, with as many rows as there are topics in the model, and as 
#' many columns as there are terms in the vocabulary.

# token counts for each term-topic combination (widths of red bars)
term.topic.frequency <- phi * topic.frequency

term.frequency <- colSums(term.topic.frequency)

# term-topic frequency table
tmp <- term.topic.frequency

# reorder topics by LDAvis order
load("Data/mongabay_LDAVIS_order_simple.Rdata")
tmp <- term.topic.frequency[LDAVis.order,]

# round down infrequent term occurrences so that we can send sparse 
# data to the browser:
r <- row(tmp)[tmp >= 0.5]
c <- col(tmp)[tmp >= 0.5]
dd <- data.frame(Term = vocab[c], Topic = r, Freq = round(tmp[cbind(r, c)]),
                 stringsAsFactors = FALSE)

# Normalize token frequencies:
dd[, "Freq"] <- dd[, "Freq"]/term.frequency[match(dd[, "Term"], vocab)]
token.table <- dd[order(dd[, 1], dd[, 2]), ]

# verify term topic frequencies match LDAvis
# View(token.table[token.table$Term=="indonesia",])

# Load countries in order of deforestation

join <- read.table("join_table")

join <- join %>% arrange(desc(total_loss))

countries <- join$country

# Create country contexts table

countries_length <- length(countries)

countries_list <- list()

for (i in 1:countries_length) {
  
  country_table <- token.table[token.table$Term==countries[i],]
  
  countries_list[[i]] <- country_table
  
}

countries_topics <- do.call(rbind.data.frame,countries_list)

rownames(countries_topics) <- NULL

# Organize country context tables with dplyr.
# Sort topics for each country in descending
# order of their proportion.

# Format tables for manuscript

colnames(countries_topics) <- c("Country", "Topic", "Probability")

countries_topics$Country <- factor(countries_topics$Country, levels = unique(countries_topics$Country))

# Order countries by number of mentions in each source

countries_topics <- countries_topics %>%  arrange(Country,desc(Probability))

#' Create summary tables showing the labels for each topic conext 
#' for outlier countries

# Add topic names to countries topics

mongabay_topic_names <- read.csv("Data/mongabay_topic_names.csv")

countries_topics_names <- left_join(countries_topics,mongabay_topic_names,by="Topic") %>%
  select(-Label)

countries_topics_names_high_prob <- countries_topics_names[countries_topics_names$Probability>0.1,]

# Round down the topic probabilities to two significant digits

countries_topics_names_high_prob$Probability <- round(countries_topics_names_high_prob$Probability,2)

# Write topic contexts for top countries with deforestation

countries_topics_top <- countries_topics_names_high_prob[countries_topics_names_high_prob$Country %in% unique(countries_topics_names_high_prob$Country)[1:10],]

# Load outliers from the country mentions versus deforestation regressions

load("Data/monga_outliers.Rdata")

countries_topics_top$UnderRepresented <- ifelse(countries_topics_top$Country %in% monga_outliers, "Yes", "No")

countries_topics_top <- countries_topics_top %>% arrange(Topic,UnderRepresented,Country)

countries_topics_top$Name <- factor(countries_topics_top$Name, levels = c(levels(countries_topics_top$Name),""))

countries_topics_top[duplicated(countries_topics_top$Name),c("Topic","Name")] <- ""

countries_topics_top <- countries_topics_top %>% select(Topic,Name,Country,Probability,UnderRepresented)

colnames(countries_topics_top)[5] <- "Under-represented"

# Write tables

write.csv(countries_topics_top,
          "Manuscript_figures/mongabay_deforestation_top_contexts_simple.csv",
          row.names = FALSE)
