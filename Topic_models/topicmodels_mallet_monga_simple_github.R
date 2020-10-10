# Set higher upper limit on R's Java Virtual Machine for topic model
# before loading libraries

options(java.parameters = "-Xmx16g")

library(mallet)
library(tm)
library(gtools)
library(stringr)
library(rgdal)
library(SnowballC)
library(LDAvis)
library(servr)
library(parallel)
library(dplyr)

#' Note: data witheld because they contain third party content

setwd("C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Deforestation review/Deforestation_messaging_analysis_GitHub/Deforestation_messaging_analysis/")

# IMPORT DATA

# Load list of web scraped Mongabay articles

load("Data/links_text_all.Rdata")

# Remove non ascii characters from data files

links_text <- lapply(links_text, iconv, from="latin1", to="ASCII", sub="")

docs <- VCorpus(VectorSource(links_text))

# PREPROCESS

docs_processed <- tm_map(docs, tolower)

# Remove all text after related articles

for(i in seq(docs_processed))   
{
  index <- grep("related articles",docs_processed[[i]])
  if (length(index) > 0) {
    replace_doc <- docs_processed[[i]][1:(index-1)]
    docs_processed[[i]] <- replace_doc
  }
}

# From http://data.okfn.org/data/datasets/geo-boundaries-world-110m

countries <- readOGR("Data/countries.geojson")

# Subset countries in the tropics

countries <- countries[(coordinates(countries)[,2]>(-24) & coordinates(countries)[,2]<(24)),]

# Find multi-word country names in the tropics

countriesSpace <- tolower(grep(" ", countries$admin, value=TRUE))

# Remove spaces from countries with multi-word names to count
# mentions of that country and not its constituent words
# e.g. count mentions of "ivorycoast" instead of 
# "ivory" and "coast" separately

countriesNoSpace <- tolower(gsub(" ", "", countriesSpace))

# Replace multi-word country names in the tropics with abbreviated names

countriesLong <- c("united republic of tanzania",
                   "democratic republic of the congo",
                   "republic of congo",
                   "trinidad and tobago",
                   "east timor")

countriesLongNoSpace <- tolower(gsub(" ", "", countriesLong))

countriesShort <- c("tanzania",
                    "drc",
                    "congo",
                    "trinidad",
                    "timor")

for(i in seq(countriesLongNoSpace))
{
  countriesNoSpace <- gsub(countriesLongNoSpace[i],countriesShort[i], countriesNoSpace)   
}

for(j in seq(docs_processed))   
{   
  for(i in seq(countriesSpace))
  {
    docs_processed[[j]] <- gsub(countriesSpace[i],countriesNoSpace[i], docs_processed[[j]])   
  }
}

commonPhrases <- c("Conservation news",
                   "Environmental science and conservation news",
                   "article published by",
                   "Did I miss something",
                   "Feel free to comment publicly below or use the following form to contact me",
                   "Mongabay depends on support from readers like you",
                   "Please join us",
                   "Too busy to read the whole story",
                   "Check out our 10-second news feeds",
                   "NewslettersEmail alerts")

commonPhrases <- tolower(commonPhrases)

docs_processed <- tm_map(docs_processed, removeWords, commonPhrases)
docs_processed <- tm_map(docs_processed, removeWords, stopwords("english"))

docs_processed <- tm_map(docs_processed, removePunctuation)
docs_processed <- tm_map(docs_processed, removeNumbers)

docs_processed <- tm_map(docs_processed, stemDocument)
docs_processed <- tm_map(docs_processed, stripWhitespace)

# Remove words with fewer than two characters or more than 40

for (j in seq(docs_processed)) {
  docs_processed[[j]] <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", docs_processed[[j]])
  docs_processed[[j]] <- gsub(" *\\b[[:alpha:]]{40,}\\b *", " ", docs_processed[[j]])
}

docs_processed <- tm_map(docs_processed, PlainTextDocument)

# Save the preprocessed documents to quickly reload later

# save(docs_processed,file="Data/docs_mongabay_processed_simple.Rdata")

load("Data/docs_mongabay_processed_simple.Rdata")



# GENERATE TERM FREQUENCY TABLE

# Create document-term matrix

dtm <- DocumentTermMatrix(docs_processed)

# Save the matrix to quickly reload later

# save(dtm,file="Data/dtm_simple.Rdata")

load("Data/dtm_simple.Rdata")

# Set the sparsity of the matrix to remove infrequent terms
# The percent sparsity determines the amount of empty space
# in the matrix, e.g. a matrix with 99% sparsity has 99% empty
# space, maximum

dtm <- removeSparseTerms(dtm, 0.9999)

wf_function <- function(x) {
  
  # Collapse the matrix by summing over columns
  
  freq <- colSums(as.matrix(x))
  
  # Create sort order (descending)
  
  ord <- order(freq,decreasing=TRUE)
  
  # List all terms in decreasing order of freq
  
  wf <- data.frame(word=names(freq), freq=freq)
  
  return(wf)
  
}

wf <- wf_function(dtm)

# Combine original and alternative country name counts

alternative_names <- c("leste","burma","divoir","southsudan","drc")

original_names <- c("timor","myanmar","ivorycoast","sudan","congo")

for (i in seq(alternative_names)) {
  
  index <- which(wf$word==alternative_names[i])
  
  alternative_count <- wf[index,]$freq
  
  if (length(index)==1) {
    
    wf[wf$word==original_names[i],]$freq <- wf[wf$word==original_names[i],]$freq + alternative_count
    
    wf <- wf[-index,]
    
  }
  
}



# Add count of CAR to Central African Republic count

upper_term <- c("Central African Republic","Papua New Guinea")

grep_acronym_term <- c("CAR","PNG")

grep_lower_stem_term <- c("centralafricanrepubl","papuanewguinea")

for (i in 1:2) {
  
  docs_sub <- tm_filter(docs, FUN = function(x) any(grep(upper_term[i], content(x))))
  
  # Convert docs directly to dtm that does not convert text to lowercase
  # and check frequency of CAR
  
  dtm <- DocumentTermMatrix(docs_sub,control = list(tolower=FALSE))
  
  wf_unprocessed <- wf_function(dtm)
  
  # Count mentions of CAR and add them to Central African Republic count
  
  indices <- grep(paste0("(?<![[:alpha:]])",grep_acronym_term[i],"(?![[:alpha:]])"),wf_unprocessed$word,perl=TRUE)
  
  alternative_count <- sum(wf_unprocessed[indices,]$freq)
  
  original_count <- wf[grep(paste0("^",grep_lower_stem_term[i]),wf$word),]$freq
  
  wf[grep(paste0("^",grep_lower_stem_term[i]),wf$word),]$freq <- original_count + alternative_count
  
}



# Write data frame

write.csv(wf,"Data/mongabay_word_freq_simple.csv",row.names = FALSE)



# TOPIC MODELING

# Write a stopwords text file to be read by mallet

write(stopwords("english"),"stopwords.txt")

# Count number of documents for creating vectors

num_docs <- length(docs_processed)

# Create a vector storing the text of each article file as elements

files_text <- vector(mode="character", length=num_docs)

for (i in 1:num_docs) {
  files_text[i] <- paste(as.character(docs_processed[[i]]), collapse = " ") 
}

# Create a vector with unique article names for each file as elements

filenames <- vector(mode="character", length=num_docs)

for (i in 1:num_docs) {
  filenames[i] <- paste0("article",i) 
}

# Create a mallet instance list object. Specify the stoplist
# as a file, since it can't be passed in a list from R.

mallet_instances <- mallet.import(filenames, files_text, "stopwords.txt")

# Create a topic trainer object.
# Specify 50 topics as determined by the bend in the 
# likelihood curve with increasing numbers of topics.

topic_model <- MalletLDA(num.topics = 50)

# Set the random seed so that the topic model reproduces
# the same results every time it's run.

topic_model$model$setRandomSeed(as.integer(42))

# Load documents.

topic_model$loadDocuments(mallet_instances)

# Optimize hyperparameters every 20 iterations,
# after 50 burn-in iterations.

topic_model$setAlphaOptimization(20, 50)

# Train the model and specify the number of iterations.

topic_model$train(200)

# Run through a few iterations where we pick the best topic for each token, 
# rather than sampling from the posterior distribution.

topic_model$maximize(10)



# Organize the labels for the 50 topics in a table.

# Get the probability of words in topics.

topic.words <- mallet.topic.words(topic_model, smoothed=T, normalized=T)

topic_word_list <- list()

for (i in 1:50) {
  topic_word_list[[i]] <- mallet.top.words(topic_model, topic.words[i,],5)$word
}

topic_word_list_collapsed <- lapply(topic_word_list,paste,collapse = ", ")

topic_word_vector <- unlist(topic_word_list_collapsed)

topic_word_table <- data.frame(topic=seq(1,50),label=topic_word_vector)



# The table of 50 topics shows that some topics are made up
# of non-English language words.

# Remove documents with a sizeable proportion of one of these
# topics and retrain the model.

# It's enough to remove documents with one non-English topic
# since these documents contain the other non-English topics
# if the documents are in the same language.

# Get the probability of topics in documents.

doc.topics <- mallet.doc.topics(topic_model, smoothed=T, normalized=T)

docs_processed <- docs_processed [ ( doc.topics[,22] < 0.1 ) ]



# Use LDAVis to visualize distances between topics

# Generate LDAvis inputs

phi <- mallet.topic.words(topic_model, normalized = TRUE, smoothed = TRUE)

theta <- mallet.doc.topics(topic_model, normalized = TRUE, smoothed = TRUE)

doc.length <- rowSums(mallet.doc.topics(topic_model))

vocab <- as.character(mallet.word.freqs(topic_model)$words)

term.frequency <- mallet.word.freqs(topic_model)$term.freq

# Save LDAvis inputs to generate country context table

# save(phi,theta,doc.length,vocab,term.frequency,file="Data/mongabay_LDAVIS_inputs.Rdata")

# load("Data/mongabay_LDAVIS_inputs.Rdata")

# Run createJSON on a cluster of machines to speed it up

cl <- makeCluster(detectCores() - 1)
cl # socket cluster with 3 nodes on host 'localhost'
system.time(
  json <- createJSON(phi, theta, doc.length, vocab, term.frequency)
)

serVis(json, out.dir = "Manuscript_figures/vis_monga", open.browser = FALSE)

# createJSON() reorders topics in decreasing order of term frequency.
# Extract LDAVis topic order to rearrange topic order of visualizations in R

LDAVis.order <- RJSONIO::fromJSON(json)$topic.order

# save(LDAVis.order,file="Data/mongabay_LDAVIS_order_simple.Rdata")

load("Data/mongabay_LDAVIS_order_simple.Rdata")



# Reorder table of topic labels to match order of LDAvis.

topic_word_table_LDAvis_order <- topic_word_table[LDAVis.order,]

topic_word_table_LDAvis_order$topic <- seq(1,50)

colnames(topic_word_table_LDAvis_order) <- c("Topic","Label")

# Write reordered table.

write.csv(topic_word_table_LDAvis_order,file = "Data/mongabay_topic_word_table.csv",row.names = FALSE)



library(dplyr)

# Add topics frequencies

mongabay_topic_word_table <- read.csv("Data/mongabay_topic_word_table.csv")

monga_filename <- "Data/lda.json"

json <- RJSONIO::fromJSON(monga_filename)

freq <- json$mdsDat$Freq

mongabay_topic_word_table_freq <- mongabay_topic_word_table %>% 
  mutate(Frequency=round(freq,1)) %>%
  select(Topic,Frequency,Label)

write.csv(mongabay_topic_word_table_freq,"Data/mongabay_topic_word_table_freq.csv",
          row.names = FALSE)



# Add topic names

mongabay_topic_names <- read.csv("Data/mongabay_topic_names.csv")

mongabay_topic_word_table_freq_names <- mongabay_topic_names %>% 
  select(Topic,Name) %>%
  left_join(mongabay_topic_word_table_freq,by="Topic") %>%
  select(Topic,Frequency,Label,Name)

mongabay_topic_word_table_freq_names$Name <- trimws(mongabay_topic_word_table_freq_names$Name)

colnames(mongabay_topic_word_table_freq_names)[3] <- c("Stemmed Keywords")

write.csv(mongabay_topic_word_table_freq_names,file = "Manuscript_figures/mongabay_topic_word_table_freq_names.csv",
          row.names = FALSE)
