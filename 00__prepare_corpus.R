#!/usr/bin/env Rscript
#
# W.M. Otte (w.m.otte@umcutrecht.nl)
#
# Aim: prepare text corpus for usage as toy example (NLP)
#
# Input data is generated with Gemini 1.5 (Google)
#
################################################################################

# FIRST TIME:
# install.packages( 'stringr' )

library( 'stringr' )

# output dir
outdir <- 'out.00.prepare.corpus'
dir.create( outdir, showWarnings = FALSE )

# input files
infiles <- dir( 'data', full.names = TRUE )

# initiate container with single documents per row
df <- NULL

for( i in 1:length( infiles ) )
{
    # read text input file as one sentence per row
    txt <- readLines( infiles[ i ] )   
    
    # concat all sentences into a single long sentence [' ' as separator]
    single <- stringr::str_c( txt, collapse = ' ' )
    
    # remove tabs, if present (to prevent issues with saving the output as TSV)
    single <- stringr::str_remove_all( single, '\t' )

    # convert to lowercase
    single <- stringr::str_to_lower( single )
        
    # add to container
    df <- rbind( df, single )
}

# convert into data.frame with two columns: 'id', 'text'
out <- data.frame( id = 1:nrow( df ), 
                   text = df )

# reset rownames
rownames( out ) <- NULL

# inspect output
str( out )

# save output to tsv [tsv]
write.table( out, file = paste0( outdir, '/dataset.tsv' ), quote = TRUE, sep = '\t' )
