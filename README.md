
<!-- README.md is generated from README.Rmd. Please edit that file -->

# iSEEid

<!-- badges: start -->

[![R-CMD-check-bioc](https://github.com/iSEE/iSEEid/actions/workflows/R-CMD-check-bioc.yaml/badge.svg)](https://github.com/iSEE/iSEEid/actions/workflows/R-CMD-check-bioc.yaml)
<!-- badges: end -->

The goal of iSEEid is to use `iSEE` to `id`entify cells

## Installation

Get the latest stable `R` release from
[CRAN](http://cran.r-project.org/).  
Then, install *[iSEEid](https://bioconductor.org/packages/3.24/iSEEid)*
from [Bioconductor](http://bioconductor.org/) using the following code:

``` r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}

BiocManager::install("iSEEid")
```

And the development version from
[GitHub](https://github.com/iSEE/iSEEid) with:

``` r
BiocManager::install("iSEE/iSEEid")
```

## Example

We use the Allen Brain Atlas dataset from
*[scRNAseq](https://bioconductor.org/packages/3.24/scRNAseq)* as a
running example, pre-processed in a summary manner just for showing most
functionality.

``` r
library("iSEEid")
library("iSEE")
library("scRNAseq")
library("scater")
library("scrapper")

sce <- ReprocessedAllenData(assays = "tophat_counts")
sce <- normalizeRnaCounts.se(sce, assay.type = "tophat_counts", size.factors = NULL)
sce <- runPCA(sce, ncomponents = 4)
sce <- runTSNE(sce)

colData(sce)["cell_type"] <- "unassigned"

sce # this is the SummarizedExperiment object you use to store your data

iSEE(sce, initial = list(
  ReducedDimensionPlot(
    PanelWidth = 6L
  ),
  SampleIdentificationCenter(
    ColumnSelectionSource = "ReducedDimensionPlot1",
    PanelWidth = 6L
  )
))
```

![](vignettes/appshot_basic_iSEEid.png)

## Disclaimer

The large language model [claude.ai](https://claude.ai/) (Sonnet 4.6)
has been used in conversational mode to enable the output format switch
in the panel, to comply with the remainder of iSEE’s architecture. Code
suggestions have been revised and validated before committing them.

## Code of Conduct

Please note that the
*[iSEEid](https://bioconductor.org/packages/3.24/iSEEid)* project is
released with a [Contributor Code of
Conduct](http://bioconductor.org/about/code-of-conduct/). By
contributing to this project, you agree to abide by its terms.
