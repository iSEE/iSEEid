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
#' @docType methods
#'
#' @aliases
#' SampleIdentificationCenter SampleIdentificationCenter-class
#' .createObservers,SampleIdentificationCenter-method
#' .defineOutput,SampleIdentificationCenter-method
#' .definePanelTour,SampleIdentificationCenter-method
#' .fullName,SampleIdentificationCenter-method
#' .generateOutput,SampleIdentificationCenter-method
#' .panelColor,SampleIdentificationCenter-method
#' .renderOutput,SampleIdentificationCenter-method
#' initialize,SampleIdentificationCenter-method
#'
#' @examples
#' library(iSEE)
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
#'         SampleIdentificationCenter(
#'           ColumnSelectionSource = "ReducedDimensionPlot1"
#'         )
#'       )
#'     )
#' }
#'
#' @author Federico Marini
#'
#' @seealso
#' \link{ColumnDataTable}, for the base class.
#' @name SampleIdentificationCenter-class
NULL

# Constants --------------------------------------------------------------------

# Definition -------------------------------------------------------------------

collated <- character(0)

#' @export
#' @importClassesFrom iSEE ColumnTable ColumnDataTable Panel
#' @import SummarizedExperiment
#' @importFrom shinyAce aceEditor
setClass("SampleIdentificationCenter",
         contains = "Panel"  ,
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

  do.call(callNextMethod, c(list(.Object), args))
})


# Interface --------------------------------------------------------------------

# TODO - placeholder

## Idea: have a radio button/checkbox to control what the editor can give
## Idea: have a textInput where to record the reason why one would select those cells
## These elements could just "refine" the behavior of the content returned by the text editor

# Observers --------------------------------------------------------------------

#' @export
#' @importFrom methods callNextMethod
setMethod(".createObservers", "SampleIdentificationCenter", function(x, se, input, session, pObjects, rObjects) {
  callNextMethod()

  panel_name <- .getEncodedName(x)

  invisible(NULL)
})


# Panel output -----------------------------------------------------------------

#' @export
#' @importFrom shiny tagList uiOutput
setMethod(".generateOutput", "SampleIdentificationCenter", function(x, se, all_memory, all_contents) {
  print(".generateOutput-SampleIdentificationCenter")
  panel_name <- .getEncodedName(x)

  all_cmds <- list()

  panel_env <- new.env()
  panel_env$se <- se

  all_cmds$select <- .processMultiSelections(x, all_memory, all_contents, panel_env)
  print(all_cmds)
  .textEval(all_cmds, panel_env)
  print(ls(panel_env))

  selected_names <- panel_env$col_selected[["active"]]
  print(selected_names)

  list(
    commands=all_cmds,
    contents=aceEditor(
      panel_name,
      mode="r",
      theme="solarized_light",
      value=paste0(selected_names, collapse = "\n"),
      height=paste0(slot(x, .organizationHeight), "px")),
    varname=panel_name)
})


#' @export
#' @importFrom shiny tagList
setMethod(".defineOutput", "SampleIdentificationCenter", function(x) {
  print(".defineOutput-SampleIdentificationCenter")
  panel_name <- .getEncodedName(x)

  print(x)

  tagList(
    uiOutput(panel_name)
  )

})

#' @export
#' @importFrom shiny renderPlot tagList wellPanel nearPoints renderUI
setMethod(".renderOutput", "SampleIdentificationCenter", function(x, se, output, pObjects, rObjects) {
  print(".renderOutput-SampleIdentificationCenter")
  panel_name <- .getEncodedName(x)
  force(se) # defensive programming to avoid difficult bugs due to delayed evaluation.

  # nocov start
  output[[panel_name]] <- renderUI({
    .retrieveOutput(panel_name, se, pObjects, rObjects)$contents
  })
  # nocov end

  callNextMethod()
})

# Transmission -----------------------------------------------------------------

#' @export
setMethods(".multiSelectionResponsive", "SampleIdentificationCenter", function(x, dims = character(0)){
  if ("column" %in% dims) {
    return(TRUE)
  }
  return(FALSE)
})

# Tour definition --------------------------------------------------------------

#' @export
setMethod(".definePanelTour", "SampleIdentificationCenter", function(x) {
  prev <- callNextMethod()

  prev[1,"intro"] <- sprintf("The <font color=\"%s\">Sample identification center</font> panel shows TODO</em>.", .getPanelColor(x))

  prev
})


cellids_to_command <- function(cellids,
                               object_name = "se",
                               coldata_annotation = "cell_type",
                               comment_rationale = "") {
  cmd <- c()
  cmd <- paste0(cmd,
                "## This is your SummarizedExperiment object\n# ", object_name,
                "\n\n## In this slot you store your annotation e.g. your cell label\n# ",
                "colData(", object_name, ")[['", coldata_annotation, "']]",
                "\n\n## To rename the selected cells to their new label, you can use\n",
                "colData(", object_name, ")[['", coldata_annotation, "']][\n",
                "  c(",
                paste0(.quoteElement(cellids), collapse = ",\n    "),
                "\n  )] <- 'new_cell_type'\n"
  )

}

.quoteElement <- function(x) {
  paste0("'", x, "'")
}

