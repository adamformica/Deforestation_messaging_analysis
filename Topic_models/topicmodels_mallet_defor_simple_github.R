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

#' Note: data witheld because they contain third party content
#' and large file size

setwd("C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Deforestation review/Deforestation_messaging_analysis_GitHub/Deforestation_messaging_analysis/")

local_path <- "C:/Users/Sensonomic Admin/Desktop/Scopus/Deforestation review"

# IMPORT DATA

# PARALELLIZATION FOR SPRINGER AND WILEY

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

# send packages to multicores 
clusterEvalQ(cl, library(tm))

# SPRINGER

# use mixedsort to match windows sorting
filenames_springer <- mixedsort(list.files(paste0(local_path,"Springer"),pattern = "pdf$",full.names = TRUE))

no_parallel_docs <- length(filenames_springer) - (length(filenames_springer) %% no_cores)

filenames_parallel <- filenames_springer[1:no_parallel_docs]

Rpdf <- readPDF(control = list(text = "-layout"))

no_parallel_docs_div_cores <- no_parallel_docs/no_cores

# send data to multicores 
clusterExport(cl, c("filenames_parallel","Rpdf","no_parallel_docs_div_cores"))

seq <- seq(1,no_parallel_docs,by=no_parallel_docs_div_cores)

wrapper <- function (start) Corpus(URISource(filenames_parallel[start:(start+no_parallel_docs_div_cores-1)]), readerControl = list(reader = Rpdf))

docs_springer_parallel <- parLapply(cl, seq, wrapper)

if (length(filenames_springer) - no_parallel_docs > 0 ) {
  
  filenames_non_parallel <- filenames_springer[(no_parallel_docs+1):length(filenames_springer)]
  
  docs_springer_non_parallel <- Corpus(URISource(filenames_non_parallel), readerControl = list(reader = Rpdf))
  
}

# WILEY

# use mixedsort to match windows sorting
filenames_wiley <- mixedsort(list.files(paste0(local_path,"Wiley"),pattern = "pdf$",full.names = TRUE))

no_parallel_docs <- length(filenames_wiley) - (length(filenames_wiley) %% no_cores)

filenames_parallel <- filenames_wiley[1:no_parallel_docs]

Rpdf <- readPDF(control = list(text = "-layout"))

no_parallel_docs_div_cores <- no_parallel_docs/no_cores

# send data to multicores 
clusterExport(cl, c("filenames_parallel","Rpdf","no_parallel_docs_div_cores"))

seq <- seq(1,no_parallel_docs,by=no_parallel_docs_div_cores)

wrapper <- function (start) Corpus(URISource(filenames_parallel[start:(start+no_parallel_docs_div_cores-1)]), readerControl = list(reader = Rpdf))

docs_wiley_parallel <- parLapply(cl, seq, wrapper)

if (length(filenames_wiley) - no_parallel_docs > 0 ) {
  
  filenames_non_parallel <- filenames_wiley[(no_parallel_docs+1):length(filenames_wiley)]
  
  docs_wiley_non_parallel <- Corpus(URISource(filenames_non_parallel), readerControl = list(reader = Rpdf))
  
}

# return resources to OS
stopCluster(cl)

# ELSEVIER

# use mixedsort to match windows sorting
filenames_elsevier <- mixedsort(list.files(paste0(local_path,"Elsevier"),pattern = "txt$",full.names = TRUE))

files_elsevier <- lapply(filenames_elsevier,readLines)

# remove non-alphabetic characters

files_elsevier <- lapply(files_elsevier, function(x) str_replace_all(x,"[^[A-z][:space:]]", ""))

docs_elsevier <- Corpus(VectorSource(files_elsevier))

# MERGE AND SAVE CORPORA

# https://stackoverflow.com/questions/20971094/tm-combine-list-of-corpora
# If recursive = TRUE existing corpus meta data is also merged, otherwise discarded.

docs_springer_parallel <- do.call(function(...) c(..., recursive = TRUE), docs_springer_parallel)

docs_wiley_parallel <- do.call(function(...) c(..., recursive = TRUE), docs_wiley_parallel)

if (exists("docs_springer_non_parallel") & exists("docs_wiley_non_parallel")) {

  docs <- c(docs_springer_parallel,docs_springer_non_parallel,docs_wiley_parallel,docs_wiley_non_parallel,docs_elsevier)

} else {

  print("remove one or more non parallel corpora from the list")

}

# save(docs,file=paste0(local_path,"docs_unprocessed.Rdata"))

load(paste0(local_path,"docs_unprocessed.Rdata"))

# PREPROCESS

docs_processed <- tm_map(docs, tolower)

# From http://data.okfn.org/data/datasets/geo-boundaries-world-110m

countries <- readOGR(paste0(local_path,"countries.geojson"))

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

# save(docs_processed,file=paste0(local_path,"docs_literature_processed_simple.Rdata"))

load(paste0(local_path,"docs_literature_processed_simple.Rdata"))



# GENERATE TERM FREQUENCY TABLE

# Create document-term matrix

dtm <- DocumentTermMatrix(docs_processed)

# Save the matrix to quickly reload later

# save(dtm,file=paste0(local_path,"dtm_literature_simple.Rdata"))

load(paste0(local_path,"dtm_literature_simple.Rdata"))

# Set the sparsity of the matrix to remove infrequent terms
# The percent sparsity determines the amount of empty space
# in the matrix, e.g. a matrix with 99% sparsity has 99% empty
# space, maximum

dtm <- removeSparseTerms(dtm, 0.999)

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
    
    original_count <- wf[wf$word==original_names[i],]$freq
    
    wf[wf$word==original_names[i],]$freq <- original_count + alternative_count
    
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

write.csv(wf,"Data/literature_word_freq_simple.csv",row.names = FALSE)



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

write.csv(topic_word_table,file = "Data/literature_topic_word_table_unordered.csv",row.names = FALSE)



# Use LDAVis to visualize distances between topics

# Generate LDAvis inputs

phi <- mallet.topic.words(topic_model, normalized = TRUE, smoothed = TRUE)

theta <- mallet.doc.topics(topic_model, normalized = TRUE, smoothed = TRUE)

doc.length <- rowSums(mallet.doc.topics(topic_model))

vocab <- as.character(mallet.word.freqs(topic_model)$words)

term.frequency <- mallet.word.freqs(topic_model)$term.freq

# Save LDAvis inputs to generate country context table

# save(phi,theta,doc.length,vocab,term.frequency,file="Data/literature_LDAVIS_inputs.Rdata")

load("Data/literature_LDAVIS_inputs.Rdata")

# Run createJSON on a cluster of machines to speed it up

cl <- makeCluster(detectCores() - 1)
cl # socket cluster with 3 nodes on host 'localhost'
system.time(
  json <- createJSON(phi, theta, doc.length, vocab, term.frequency)
)

# serVis(json, out.dir = "Manuscript_figures/vis_lit", open.browser = FALSE)

# createJSON() reorders topics in decreasing order of term frequency.
# Extract LDAVis topic order to rearrange topic order of visualizations in R

LDAVis.order <- RJSONIO::fromJSON(json)$topic.order

# save(LDAVis.order,file="Data/literature_LDAVIS_order_simple.Rdata")

load("Data/literature_LDAVIS_order_simple.Rdata")



# Reorder table of topic labels to match order of LDAvis.

topic_word_table_LDAvis_order <- topic_word_table[LDAVis.order,]

topic_word_table_LDAvis_order$topic <- seq(1,50)

colnames(topic_word_table_LDAvis_order) <- c("Topic","Label")

# Write reordered table.

write.csv(topic_word_table_LDAvis_order,file = "Data/literature_topic_word_table.csv",row.names = FALSE)



library(dplyr)

# Add topics frequencies

literature_topic_word_table <- read.csv("Data/literature_topic_word_table.csv")

defor_filename <- "Data/lda.json"

json <- RJSONIO::fromJSON(defor_filename)

freq <- json$mdsDat$Freq

literature_topic_word_table_freq <- literature_topic_word_table %>% 
  mutate(Frequency=round(freq,1)) %>%
  select(Topic,Frequency,Label)

write.csv(literature_topic_word_table_freq,"Data/literature_topic_word_table_freq.csv",
          row.names = FALSE)



# Add topic names

literature_topic_names <- read.csv("Data/literature_topic_names.csv")

literature_topic_word_table_freq_names <- literature_topic_names %>% 
  select(Topic,Name) %>%
  left_join(literature_topic_word_table_freq,by="Topic") %>%
  select(Topic,Frequency,Label,Name)

literature_topic_word_table_freq_names$Name <- trimws(literature_topic_word_table_freq_names$Name)

colnames(literature_topic_word_table_freq_names)[3] <- c("Stemmed Keywords")
  
write.csv(literature_topic_word_table_freq_names,file = "Manuscript_figures/literature_topic_word_table_freq_names.csv",
          row.names = FALSE)
