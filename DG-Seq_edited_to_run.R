#####################################
###### DG-SEQ -- EXAMPLE
#####################################

##### PRE-PROCESSING (to be run on the terminal)
# bowtie2-build [REF].fasta [REF]
# bowtie2 -x [REF] -U /storage/home/aat5490/scratch/samples_refs/CymRSV/[SAMPLE TRIMMED + CLEANED].fastq -S [OUTPUT NAME].sam
# bwa index [PATH TO REF.FASTA]
# samtools view -S -b [OUTPUT].sam > [OUTPUT].bam
# samtools sort [OUTPUT].bam -o [OUTPUT 2].bam
# samtools index [OUTPUT 2].bam
# bwa mem [PATH TO REF.FASTA] [PATH TO SAMPLE.FASTQ] > [ALIGNMENT].sam
# samtools view -S -b [ALIGNMENT].sam > [ALIGNMENT].bam
# samtools sort [ALIGNMENT].bam -o [ALIGNMENT 2].bam
# samtools index [ALIGNMENT 2].bam

# Be sure to delete excess files, DG-seq gets upset if intermediate .bam files are in the same folder

### 1. Load functions and packages from "Supplemental_File_S1_Functions.R"
library(Rsamtools)
library(data.table)
library(tester)
library(dplyr)
library(tidyr)
library(reshape2)
library(seqinr)
library(stringr)

#### 2. Generate uncertainty matrices for each reference
# should be computed once for all references used for the sample at hand and subsequently stored as an .RDatafile, to avoid recomputing
refFolder <- "/storage/home/aat5490/scratch/samples_refs/BMV/ref/" # NO indexes can be in this folder
unc_ref = compute_uncertainty_ref(refFolder)
save(unc_ref,file = "NameOfRefResults2.RData")

### 3. Load uncertainty matrices
load("NameOfRefResults.RData")

### 4. Compute raw counts
data = "/storage/home/aat5490/scratch/samples_refs/BMV/alignment2_sorted.bam" # .bam file that has ALREADY been bwa-mem'd, sorted, and indexed
import <- importData(data)
lengthRef <- import$lengthRef
f = function(x) ((as.integer(intToBits(x)[5])+1)%%2)
df <- as.data.table(import$df)

df[,c("cur2") := tstrsplit(tag.SA, ";", fixed=TRUE, keep=1L)]
df[,c("letters.cigar1") := gsub("[[:digit:]]","",cigar,perl=T)]
df[,c("number.cigar11","number.cigar12","number.cigar13") := tstrsplit(cigar, "[[:alpha:]]",keep=c(1,2,3))]
df[,c("sign1"):=sapply(flag,f)]


## SA TAGS

d1 = df[!is.na(tag.SA)] # Issue may be here. There are no SA tags.
if (nrow(d1)!=0) d1[,c("pos2","sign2_sub","cigar2") := tstrsplit(cur2, ",", fixed=TRUE, keep=c(2,3,4))] else d1[,c("pos2","sign2_sub","cigar2") := 0]
d1[,c("letters.cigar2") := gsub("[[:digit:]]","",cigar2)]
if (nrow(d1)!=0) d1[,c("number.cigar21","number.cigar22") := tstrsplit(cigar2, "[[:alpha:]]",keep=c(1,2))] else d1[,c("number.cigar21","number.cigar22") := 0]
d1[,c("sign2"):=as.integer(sapply(sign2_sub, function (x) {if (x=="+") 1 else 0}))]
d1[,c("coherence"):=(sign1+sign2)%%2]


# DELETIONS

d2 = d1[coherence==0 & letters.cigar1 %in% c("MH","MS") & letters.cigar2 %in% c("HM","SM")]
d2[,c("length1"):=as.integer(number.cigar11)]
d2[,c("length2"):=as.integer(number.cigar21)]
d2[,c("pos1"):=as.integer(pos)]
d2[,c("pos2"):=as.integer(pos2)]
d2[,c("u"):=length1-length2]
d2[,c("va_u"):=as.integer(sapply(u,function(x) max(x,0)))]
d2[,c("x"):=pos1 + length1 - 1 - va_u]
d2[,c("y"):=pos2]
d2 = d2[ , .N, by = .(x, y, coherence, u)]

## SMALL DELETIONS

d5 = df[is.na(tag.SA) & !is.na(cigar) & letters.cigar1=="MDM"]
d5[,c("length1"):=as.integer(number.cigar11)]
d5[,c("length2"):=as.integer(number.cigar12)]
d5[,c("pos1"):=as.integer(pos)]
d5[,c("x"):=pos1 + length1 - 1]
d5[,c("y"):=pos1 + length1 + length2]
d5[,c("u"):=0]
d5[,c("sign2"):=sign1]
d5[,c("coherence"):=0]
d5 = d5[ , .N, by = .(x, y, coherence, u)]


final = rbindlist(list(d2,d5))
names(final) = c("jumpStart","jumpEnd","changeStrand","uncertainty","count")
write.csv2(final, "/storage/home/aat5490/scratch/samples_refs/BMV/final.csv", row.names = FALSE)
