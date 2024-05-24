#!/usr/bin/env Rscript
#
# W.M. Otte (w.m.otte@umcutrecht.nl)
#
# Aim: first analyis of the generated text corpus (toy example (NLP))
################################################################################

# FIRST TIME:
# install.packages( 'quanteda' )
# install.packages( 'quanteda.textstats' )
# install.packages( 'quanteda.textmodels' )

# load library
library( 'quanteda' )
library( 'quanteda.textstats' )
library( 'quanteda.textmodels' )

# plotting settings
options( width = 110 )

# output dir
outdir <- 'out.01.first.analysis'
dir.create( outdir, showWarnings = FALSE )

# load input dataset
df <- read.table( 'out.00.prepare.corpus/dataset.tsv', row.names = 1 )

# inspect
str( df )

# make corpus [https://tutorials.quanteda.io/basic-operations/corpus/corpus/]
corp <- corpus( df$text, docvars = paste0( "Verwijsbrief-", df$id ) )

# add epilepsy diagnosis group
corp$group <- c( 'no', 'no', 'no', 'yes', 'yes', 'yes' )

# get summary
summary( corp )

#### TOKENS ####

# get all tokens per document
toks <- tokens( corp, remove_punct = TRUE, remove_symbols = TRUE, remove_url = TRUE, remove_separators = TRUE )

# get all 1-gram + the 2-gram tokens (e.g., "mri scan" -> "mri_scan")
toks_ngram <- tokens_ngrams( toks, n = 1:2 )

# remove stopwords [remove or add if required]
stopwords <- c( 'de', 'van', 'en', 'is', 'het', 'op', 'voor', 'met', 'zijn', 'wordt', 'er' )

# tokens with stoptokens removed
toks_clean <- tokens_remove( toks_ngram, stopwords )

# make a document-feature matrix [i.e., with docs as rows and tokens as columns]
dfmat <- dfm( toks_clean )

# extract frequency of tokens
tstat_freq <- textstat_frequency( dfmat )

# plot most frequent tokens
head( tstat_freq, 50 )

# cluster documents into dendrogram
tstat_dist <- as.dist( textstat_dist( dfmat ) )
clust <- hclust( tstat_dist )
plot( clust, xlab = "Distance", ylab = NULL )

##############################
#### Classification model ####
##############################

# Naive Bayes model
tmod_nb <- textmodel_nb( dfmat, dfmat$group, distribution = "Bernoulli" )

# summary [first 15 features]
summary( tmod_nb, 15 )

# check classification
actual_class <- dfmat$group

# predict class
predicted_class <- predict( tmod_nb, newdata = dfmat )

# 2x2 table
tab_class <- table( actual_class, predicted_class )
print( tab_class )

###
# confusion matrix stats (e.g., sensitivity, specificity, etc.)
# requires installation of package 'caret': install.packages( 'caret' )
###
#confusion_mat <- caret::confusionMatrix( tab_class, mode = "everything", positive = "yes" )
#print( confusion_mat )

# 20 most positive tokens (i.e., related to the 'yes' group)
sort( tmod_nb$param[ "yes", ] / colSums( tmod_nb$param ), dec = T )[ 1:20 ]

# 20 most negative tokens (i.e., related to the 'no' group)
sort( tmod_nb$param[ "no", ] / colSums( tmod_nb$param ), dec = T )[ 1:20 ]
