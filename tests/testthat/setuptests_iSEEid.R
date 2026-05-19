library(iSEE)
library(scRNAseq)

# Example data ----
sce <- ReprocessedAllenData(assays = "tophat_counts")
class(sce)

library(scater)
library(scrapper)
sce <- normalizeRnaCounts.se(sce, assay.type = "tophat_counts", size.factors = NULL)

sce <- runPCA(sce, ncomponents=4)
sce <- runTSNE(sce)
rowData(sce)$ave_count <- rowMeans(assay(sce, "tophat_counts"))
rowData(sce)$n_cells <- rowSums(assay(sce, "tophat_counts") > 0)

sce
