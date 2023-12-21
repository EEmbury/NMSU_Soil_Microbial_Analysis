### tutorial: https://benjjneb.github.io/dada2/tutorial.html

#doi:10.1002/ece3.6594 -- recommends dada2 pipeline

library(dada2); packageVersion("dada2")


path <- "~/Jornada_bacteria/Romero-Olivares_Project_002-fastqs" # CHANGE ME to the directory containing the fastq files after unzipping.
list.files(path)

# Forward and reverse fastq filenames have format: SAMPLENAME_R1_001.fastq and SAMPLENAME_R2_001.fastq
fnFs <- sort(list.files(path, pattern="_R1_001.fastq", full.names = TRUE))
fnRs <- sort(list.files(path, pattern="_R2_001.fastq", full.names = TRUE))
# Extract sample names, assuming filenames have format: SAMPLENAME_XXX.fastq
sample.names <- sapply(strsplit(basename(fnFs), "_"), `[`, 1)


##### inspect read quality #####

# forward reads
plotQualityProfile(fnFs[1:2])

#reads look good, will trim at 290

# reverse reads
plotQualityProfile(fnRs[1:2])

#reads are very messy, will trim at 200


#### filter and trim #####

# Place filtered files in filtered/ subdirectory
filtFs <- file.path(path, "filtered", paste0(sample.names, "_F_filt.fastq.gz"))
filtRs <- file.path(path, "filtered", paste0(sample.names, "_R_filt.fastq.gz"))

names(filtFs) <- sample.names
names(filtRs) <- sample.names


#sample 129 was showing very low counts, will drop sample

out <- filterAndTrim(fnFs, filtFs, fnRs, filtRs, truncLen=c(290,200),
                     maxN=0, maxEE=c(2,2), truncQ=2, rm.phix=TRUE,
                     compress=TRUE, multithread=FALSE) # On Windows set multithread=FALSE
head(out)


#### learn error rates ####
errF <- learnErrors(filtFs, multithread=TRUE)
errR <- learnErrors(filtRs, multithread=TRUE)
plotErrors(errF, nominalQ=TRUE)


#### sample inference #####

dadaFs <- dada(filtFs, err=errF, multithread=TRUE)
dadaRs <- dada(filtRs, err=errR, multithread=TRUE)

dadaFs[[1]]
dadaRs[[1]]

#### merge paired reads ####
mergers <- mergePairs(dadaFs, filtFs, dadaRs, filtRs, verbose=TRUE)
# Inspect the merger data.frame from the first sample
head(mergers[[1]])


#### construct sequence table ####
seqtab <- makeSequenceTable(mergers)
dim(seqtab)

# Inspect distribution of sequence lengths 
table(nchar(getSequences(seqtab)))


#### remove chimeras ####
seqtab.nochim <- removeBimeraDenovo(seqtab, method="consensus", multithread=TRUE, verbose=TRUE)
dim(seqtab.nochim)

sum(seqtab.nochim)/sum(seqtab) 


#### track reads through the pipeline ####
getN <- function(x) sum(getUniques(x))
track <- cbind(out, sapply(dadaFs, getN), sapply(dadaRs, getN), sapply(mergers, getN), rowSums(seqtab.nochim))
# If processing a single sample, remove the sapply calls: e.g. replace sapply(dadaFs, getN) with getN(dadaFs)
colnames(track) <- c("input", "filtered", "denoisedF", "denoisedR", "merged", "nonchim")
rownames(track) <- sample.names
head(track)

#### assign taxonomy ####

# using SILVA database formated for dada2
# https://zenodo.org/records/4587955

taxa <- assignTaxonomy(seqtab.nochim, "~/Jornada_bacteria/SILVA_dada2/silva_nr99_v138.1_wSpecies_train_set.fa", multithread=TRUE)

taxa.print <- taxa # Removing sequence rownames for display only
rownames(taxa.print) <- NULL
head(taxa.print)

#export "taxa" and "seqtab.nochim" for downstream analyses
