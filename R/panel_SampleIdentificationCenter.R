#' The SampleIdentificationCenter class
#'
#' The SampleIdentificationCenter is a [ColumnDataTable-class] subclass that is dedicated
#' to TODO..
#'
#' @section Slot overview:
#' The following slots control the behavior of the panel: TODO
#'
#' In addition, this class inherits all slots from its parent [ColumnDataTable-class]
#' classes.
#'
#' @section Constructor:
#' `SampleIdentificationCenter(...)` creates an instance of a
#' `SampleIdentificationCenter` class,
#' where any slot and its value can be passed to `...` as a named argument.
#'
#' @section Supported methods:
#' In the following code snippets, `x` is an instance of a
#' [SampleIdentificationCenter-class] class.
#'
#'
#' @examples
#' library(scRNAseq)
#'
#' # Example data ----
#' sce <- ReprocessedAllenData(assays="tophat_counts")
#' class(sce)
#'
#' library(scater)
#' sce <- logNormCounts(sce, exprs_values="tophat_counts")
#'
#' sce <- runPCA(sce, ncomponents=4)
#' sce <- runTSNE(sce)
#' rowData(sce)$ave_count <- rowMeans(assay(sce, "tophat_counts"))
#' rowData(sce)$n_cells <- rowSums(assay(sce, "tophat_counts") > 0)
#'
#' # launch the app itself ----
#'
#' if (interactive()) {
#'     iSEE(sce, initial=list(
#'         ReducedDimensionPlot(),
#'         SampleIdentificationCenter())
#'     )
#' }
#'
#' @author Federico Marini
#'
#' @seealso
#' \link{ColumnDataTable}, for the base class.
#' @name SampleIdentificationCenter-class
NULL

# Definition -------------------------------------------------------------------

collated <- character(0)

.plotBinResolution <- "Content"
collated[.plotBinResolution] <- "numeric"

#' @export
#' @importClassesFrom iSEE ColumnTable ColumnDataTable
#' @import SummarizedExperiment
#' @importFrom shinyAce aceEditor
setClass("SampleIdentificationCenter",
         contains = "ColumnDataTable"  ,
         slots = collated
         )

#' @export
#' @importFrom methods new
SampleIdentificationCenter <- function(...) {
  new("SampleIdentificationCenter", ...)
}

#' @importMethodsFrom iSEE .fullName
#' @export
setMethod(".fullName", "SampleIdentificationCenter",
          function(x) "Sample identification center panel")

#' @export
#' @importMethodsFrom iSEE .panelColor
setMethod(".panelColor", "SampleIdentificationCenter", function(x) "#00C4DA")

# Initialization ---------------------------------------------------------------

#' @export
#' @importFrom methods callNextMethod
#' @importFrom iSEE .emptyDefault
setMethod("initialize", "SampleIdentificationCenter", function(.Object, ...) {
  args <- list(...)

  # args <- .emptyDefault(args, .plotBinResolution, 100)
  # args[["Downsample"]] <- FALSE

  do.call(callNextMethod, c(list(.Object), args))
})


# Interface --------------------------------------------------------------------

#' @export
#' @importFrom shiny tagList
setMethod(".defineDataInterface", "SampleIdentificationCenter", function(x) {
  panel_name <- .getEncodedName(x)

  .addSpecificTour(class(x), .plotBinResolution, function(panel_name) {
    data.frame(
      element=paste0("#", panel_name, "_", .plotBinResolution),
      intro="Here, we can change the bin size of the plot.
Larger values will result in larger bins, sacrificing resolution for more stable counts.
One should avoid setting this too low as then the plot just collapses to showing each point as a separate bin."
    )
  })

  tagList(
  .numericInput.iSEE(x, .plotBinResolution, label="Bin resolution:",
                     min=1, value=x[[.plotBinResolution]], step = 1)
  )


})

#' #' @export
#' #' @importFrom shiny tagList
#' setMethod(".defineInterface", "SampleIdentificationCenter", function(x) {
#'   plot_name <- .getEncodedName(x)
#'
#'   .addSpecificTour(class(x), .plotBinResolution, function(plot_name) {
#'     data.frame(
#'       element=paste0("#", plot_name, "_", .plotBinResolution),
#'       intro="Here, we can change the bin size of the plot.
#' Larger values will result in larger bins, sacrificing resolution for more stable counts.
#' One should avoid setting this too low as then the plot just collapses to showing each point as a separate bin."
#'     )
#'   })
#'
#'   tagList(
#'     .numericInput.iSEE(x, .plotBinResolution, label="Bin resolution:",
#'                        min=1, value=x[[.plotBinResolution]], step = 1)
#'   )
#' })

#' #' @export
#' setMethod(".allowableColorByDataChoices", "SampleIdentificationCenter", function(x, se) {
#'   .getCachedCommonInfo(se, "ColumnDotPlot")$continuous.colData.names
#' })



#' @export
setMethod(".defineDataInterface", "SampleIdentificationCenter", function(x) {
  NULL
})

# Observers --------------------------------------------------------------------

#' @export
#' @importFrom methods callNextMethod
setMethod(".createObservers", "SampleIdentificationCenter", function(x, se, input, session, pObjects, rObjects) {
  callNextMethod()

  panel_name <- .getEncodedName(x)

  .createProtectedParameterObservers(panel_name,
                                     fields=c(.plotBinResolution),
                                     input=input, pObjects=pObjects, rObjects=rObjects)

  invisible(NULL)
})


# Panel output -----------------------------------------------------------------

#' @export
#' @importFrom shiny tagList
setMethod(".defineOutput", "SampleIdentificationCenter", function(x) {
  panel_name <- .getEncodedName(x)

  id_editor <- paste0(panel_name, "_editor")
  id_rationale <- paste0(panel_name, "_rationale")
  id_copypaste <- paste0(panel_name, "_copypaste")
  id_commander <- paste0(panel_name, "_commander")

  .addSpecificTour(class(x), "editor", function(panel_name) {
    data.frame(
      element=paste0("#", panel_name, "_editor"),
      intro="editor context help."
    )
  })

  .addSpecificTour(class(x), "rationale", function(panel_name) {
    data.frame(
      element=paste0("#", panel_name, "_rationale"),
      intro="rationale context help."
    )
  })

  .addSpecificTour(class(x), "copypaste", function(panel_name) {
    data.frame(
      element=paste0("#", panel_name, "_copypaste"),
      intro="_copypaste context help."
    )
  })

  .addSpecificTour(class(x), "commander", function(panel_name) {
    data.frame(
      element=paste0("#", panel_name, "_commander"),
      intro="_commander context help."
    )
  })


  tagList(
    span(id = paste0(id_editor, "_specific_help"),
         style="display:inline-block; padding-bottom:5px;",
         HTML("<strong>Selected cells:</strong> <sup>?</sup>")),
    aceEditor(id_editor,
              mode="markdown",
              theme="xcode",
              autoComplete="disabled",
              value=slot(x, "Content"),
              debounce=1000,
              height="500px"
    ),

    span(id = paste0(id_rationale, "_specific_help"),
         style="display:inline-block; padding-bottom:5px;",
         HTML("<strong>Specify the rationale for the selection:</strong> <sup>?</sup>")),
    textInput(id_rationale, label = "",
              placeholder = "Tell me why"),

    span(id = paste0(id_copypaste, "_specific_help"),
         style="display:inline-block; padding-bottom:5px;",
         HTML("<strong>Copy to clipboard</strong> <sup>?</sup>")),
    br(),
    actionButton(id_copypaste, label = "Copy to clipboard", icon = icon("copy")),
    br(),

    span(id = paste0(id_commander, "_specific_help"),
         style="display:inline-block; padding-bottom:5px;",
         HTML("<strong>Generate the full command:</strong> <sup>?</sup>")),
    br(),
    actionButton(id_commander, label = "Create command!", icon = icon("magic")),
    hr()

  )

})





# Tour definition ---------------------------------------------------------

#' @export
setMethod(".definePanelTour", "SampleIdentificationCenter", function(x) {
  prev <- callNextMethod()

  prev[1,"intro"] <- sprintf("The <font color=\"%s\">Sample identification center</font> panel shows TODO</em>.", .getPanelColor(x))

  prev
})
