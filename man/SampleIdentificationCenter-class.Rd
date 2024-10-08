% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/panel_SampleIdentificationCenter.R
\name{SampleIdentificationCenter-class}
\alias{SampleIdentificationCenter-class}
\title{The SampleIdentificationCenter class}
\description{
The SampleIdentificationCenter is a \linkS4class{ColumnDataTable} subclass that is dedicated
to TODO..
}
\section{Slot overview}{

The following slots control the behavior of the panel: TODO

In addition, this class inherits all slots from its parent \linkS4class{ColumnDataTable}
classes.
}

\section{Constructor}{

\code{SampleIdentificationCenter(...)} creates an instance of a
\code{SampleIdentificationCenter} class,
where any slot and its value can be passed to \code{...} as a named argument.
}

\section{Supported methods}{

In the following code snippets, \code{x} is an instance of a
\linkS4class{SampleIdentificationCenter} class.
}

\examples{
library(scRNAseq)

# Example data ----
sce <- ReprocessedAllenData(assays="tophat_counts")
class(sce)

library(scater)
sce <- logNormCounts(sce, exprs_values="tophat_counts")

sce <- runPCA(sce, ncomponents=4)
sce <- runTSNE(sce)
rowData(sce)$ave_count <- rowMeans(assay(sce, "tophat_counts"))
rowData(sce)$n_cells <- rowSums(assay(sce, "tophat_counts") > 0)

# launch the app itself ----

if (interactive()) {
    iSEE(sce, initial=list(
        ReducedDimensionPlot(),
        SampleIdentificationCenter())
    )
}

}
\seealso{
\link{ColumnDataTable}, for the base class.
}
\author{
Federico Marini
}
