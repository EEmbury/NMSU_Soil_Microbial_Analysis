### tutorial: https://benjjneb.github.io/dada2/tutorial.html

#doi:10.1002/ece3.6594 -- recommends dada2 pipeline

library(dada2); packageVersion("dada2")


path <- "~/Jornada_fungi/dada2" # CHANGE ME to the directory containing the fastq files after unzipping.
list.files(path)

R1_path <- "~/Jornada_fungi/dada2/Read_1_ITSx"
R2_path <- "~/Jornada_fungi/dada2/Read_2_ITSx"

# Forward and reverse fastq filenames have format: SAMPLENAME_R1_001.fastq and SAMPLENAME_R2_001.fastq
fnFs_fungi <- sort(list.files(R1_path, pattern="_R1_001.fastq.gz", full.names = TRUE))
fnRs_fungi <- sort(list.files(R2_path, pattern="_R2_001.fastq.gz", full.names = TRUE))
# Extract sample names, assuming filenames have format: SAMPLENAME_XXX.fastq
sample.names <- sapply(strsplit(basename(fnFs_fungi), "_"), `[`, 1)


##### inspect read quality #####

# forward reads
plotQualityProfile(fnFs_fungi[1:2])

#reads look okay, will trim at 200

# reverse reads
plotQualityProfile(fnRs_fungi[1:2])

#reads are messy, will trim at 150


#### filter and trim #####

# Place filtered files in filtered/ subdirectory
filtFs_fungi <- file.path(path, "filtered", paste0(sample.names, "_F_filt.fastq.gz"))
filtRs_fungi <- file.path(path, "filtered", paste0(sample.names, "_R_filt.fastq.gz"))

names(filtFs_fungi) <- sample.names
names(filtRs_fungi) <- sample.names


out_fungi <- filterAndTrim(fnFs_fungi, filtFs_fungi, fnRs_fungi, filtRs_fungi, truncLen=c(200, 150),
                     maxN=0, maxEE=c(2,2), truncQ=2, rm.phix=TRUE,
                     compress=TRUE, multithread=FALSE) # On Windows set multithread=FALSE
head(out_fungi)


#### learn error rates ####
errF <- learnErrors(filtFs_fungi, multithread=TRUE)
errR <- learnErrors(filtRs_fungi, multithread=TRUE)
plotErrors(errF, nominalQ=TRUE)


#### sample inference #####

dadaFs <- dada(filtFs_fungi, err=errF, multithread=TRUE)
dadaRs <- dada(filtRs_fungi, err=errR, multithread=TRUE)

dadaFs[[1]]
dadaRs[[1]]

#### merge paired reads ####
mergers <- mergePairs(dadaFs, filtFs_fungi, dadaRs, filtRs_fungi, verbose=TRUE)
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
track <- cbind(out_fungi, sapply(dadaFs, getN), sapply(dadaRs, getN), sapply(mergers, getN), rowSums(seqtab.nochim))
# If processing a single sample, remove the sapply calls: e.g. replace sapply(dadaFs, getN) with getN(dadaFs)
colnames(track) <- c("input", "filtered", "denoisedF", "denoisedR", "merged", "nonchim")
rownames(track) <- sample.names
head(track)

#### assign taxonomy ####

# using SILVA database formated for dada2
# https://zenodo.org/records/4587955

# # When using this resource, please cite it as follows:
# Abarenkov, Kessy; Zirk, Allan; Piirmann, Timo; Pöhönen, Raivo; Ivanov, Filipp; Nilsson, R. Henrik; Kõljalg, Urmas (2023): UNITE general FASTA release for Fungi. Version 18.07.2023. UNITE Community. https://doi.org/10.15156/BIO/2938067
# 
# Includes singletons set as RefS (in dynamic files).

taxa <- assignTaxonomy(seqtab.nochim, "~/Jornada_fungi/Fungi_R/sh_general_release_dynamic_18.07.2023.fasta", multithread=TRUE)

taxa.print <- taxa # Removing sequence rownames for display only
rownames(taxa.print) <- NULL
head(taxa.print)

#export "taxa" and "seqtab.nochim" for downstream analyses
