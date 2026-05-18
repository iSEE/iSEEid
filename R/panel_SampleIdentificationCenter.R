#' The SampleIdentificationCenter class
#'
#' The SampleIdentificationCenter is a [iSEE::Panel-class] subclass that is dedicated
#' to generating ready-to-use R code for assigning a sample label to samples
#' received by a selection from another panel.
#'
#' @section Slot overview:
#' The following slots control the behavior of the panel: 
#' \itemize{
#' \item \code{EditorUsageMode}, a logical scalar determining whether to show 
#' the full R command for making the sample label assignments (if \code{FALSE}, 
#' displays the sample id list as plain text).
#' \item \code{AnnotationRationale}, a string specifying the rationale for the 
#' sample label assignment.
#' \item \code{CellTypeLabel}, a string providing the label to assign to the 
#' selected samples. 
#' \item \code{ColDataColumn}, a string indicating the name of the colData 
#' column to store the assigned labels.  
#' }
#'
#' In addition, this class inherits all slots from its parent [iSEE::Panel-class]
#' class.
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
#' @return A panel designed to work within the iSEE framework
#'
#' @aliases
#' SampleIdentificationCenter SampleIdentificationCenter-class .createObservers,SampleIdentificationCenter-method .defineDataInterface,SampleIdentificationCenter-method .defineOutput,SampleIdentificationCenter-method .definePanelTour,SampleIdentificationCenter-method .fullName,SampleIdentificationCenter-method .generateOutput,SampleIdentificationCenter-method .multiSelectionResponsive,SampleIdentificationCenter-method .panelColor,SampleIdentificationCenter-method .renderOutput,SampleIdentificationCenter-method initialize,SampleIdentificationCenter-method
#'
#' @examples
#' library(iSEE)
#' library(scRNAseq)
#'
#' # Example data ----
#' sce <- ReprocessedAllenData(assays = "tophat_counts")
#' class(sce)
#'
#' library(scater)
#' library(scrapper)
#' sce <- normalizeRnaCounts.se(sce, assay.type = "tophat_counts", size.factors = NULL)
#'
#' sce <- runPCA(sce, ncomponents=4)
#' sce <- runTSNE(sce)
#' rowData(sce)$ave_count <- rowMeans(assay(sce, "tophat_counts"))
#' rowData(sce)$n_cells <- rowSums(assay(sce, "tophat_counts") > 0)
#'
#' # launch the app itself ----
#'
#' if (interactive()) {
#'   iSEE(sce, initial = list(
#'        ReducedDimensionPlot(),
#'        SampleIdentificationCenter(
#'          ColumnSelectionSource = "ReducedDimensionPlot1"
#'       )
#'     )
#'   )
#' }
#'
#' @author Federico Marini
#'
#' @seealso
#' [iSEE::Panel-class], for the base class.
#'
#' @name SampleIdentificationCenter-class
NULL

# Constants --------------------------------------------------------------------

# Definition -------------------------------------------------------------------

collated <- character(0)

.EditorUsage <- "EditorUsageMode"
.AnnotationRationale <- "AnnotationRationale"
.CellTypeLabel <- "CellTypeLabel"
.ColDataColumn <- "ColDataColumn"

collated[.EditorUsage] <- "logical"
collated[.AnnotationRationale] <- "character"
collated[.CellTypeLabel] <- "character"
collated[.ColDataColumn] <- "character"

#' @export
#' @importClassesFrom iSEE ColumnTable ColumnDataTable Panel
#' @import SummarizedExperiment
#' @importFrom shinyAce aceEditor
setClass("SampleIdentificationCenter",
         contains = "Panel",
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

  args <- .emptyDefault(args, .AnnotationRationale, "")
  args <- .emptyDefault(args, .CellTypeLabel, "new_cell_type")
  args <- .emptyDefault(args, .ColDataColumn, "cell_type")

  do.call(callNextMethod, c(list(.Object), args))
})


# Interface --------------------------------------------------------------------

# TODO - placeholder

#' @export
#' @importFrom shiny tagList textInput span HTML
setMethod(".defineDataInterface", "SampleIdentificationCenter", function(x, se, select_info) {
  panel_name <- .getEncodedName(x)

  tagList(
    .checkboxInput.iSEE(
      x, .EditorUsage,
      label = "Show full R command (uncheck for displaying the cell id list as plain text)",
      value = TRUE),
    textInput(
      inputId  = paste0(panel_name, "_", .AnnotationRationale),
      label    = "Specify the rationale for the selection",
      value    = slot(x, .AnnotationRationale),
      placeholder = "e.g. Overexpression of marker X"
    ),
    textInput(
      inputId  = paste0(panel_name, "_", .ColDataColumn),
      label    = "Enter the colData column for annotation",
      value    = slot(x, .ColDataColumn),
      placeholder = "e.g. cell_type (has to be present in the object)"
    ),
    textInput(
      inputId  = paste0(panel_name, "_", .CellTypeLabel),
      label    = "Enter the new cell type label (to be assigned)",
      value    = slot(x, .CellTypeLabel),
      placeholder = "e.g. CD4+ T cells"
    )
  )

})

# Observers --------------------------------------------------------------------

#' @export
#' @importFrom methods callNextMethod
setMethod(".createObservers", "SampleIdentificationCenter", function(x, se, input, session, pObjects, rObjects) {
  callNextMethod()

  panel_name <- .getEncodedName(x)

  .createProtectedParameterObservers(panel_name,
                                     fields = c(.EditorUsage,
                                                .AnnotationRationale,
                                                .CellTypeLabel,
                                                .ColDataColumn),
                                     input = input, pObjects = pObjects, rObjects = rObjects)

  invisible(NULL)
})


# Panel output -----------------------------------------------------------------

#' @export
#' @importFrom shiny tagList uiOutput
setMethod(".generateOutput", "SampleIdentificationCenter", function(x, se, all_memory, all_contents) {
  # print(".generateOutput-SampleIdentificationCenter")
  panel_name <- .getEncodedName(x)

  all_cmds <- list()

  panel_env <- new.env()
  panel_env$se <- se

  all_cmds$select <- .processMultiSelections(x, all_memory, all_contents, panel_env)
  # print(all_cmds)
  .textEval(all_cmds, panel_env)
  # print(ls(panel_env))

  selected_names <- panel_env$col_selected[["active"]]
  # print(selected_names)

  editor_contents <- if (isTRUE(slot(x, .EditorUsage))) {
    cellids_to_command(
      selected_names,
      coldata_annotation  = slot(x, .ColDataColumn),
      new_cell_type_label = slot(x, .CellTypeLabel),
      comment_rationale   = slot(x, .AnnotationRationale)
    )
  } else {
    paste0(selected_names, collapse = "\n")
  }

  list(
    commands = all_cmds,
    contents = aceEditor(
      panel_name,
      mode  = if (isTRUE(slot(x, .EditorUsage))) "r" else "plain_text",
      theme = "solarized_light",
      value = editor_contents,
      height = paste0(slot(x, .organizationHeight), "px")),
    varname = panel_name)
})


#' @export
#' @importFrom shiny tagList
setMethod(".defineOutput", "SampleIdentificationCenter", function(x) {
  # print(".defineOutput-SampleIdentificationCenter")
  panel_name <- .getEncodedName(x)

  # print(x)

  tagList(
    uiOutput(panel_name)
  )

})

#' @export
#' @importFrom shiny renderPlot tagList wellPanel nearPoints renderUI
setMethod(".renderOutput", "SampleIdentificationCenter", function(x, se, output, pObjects, rObjects) {
  # print(".renderOutput-SampleIdentificationCenter")
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
setMethod(".multiSelectionResponsive", "SampleIdentificationCenter", function(x, dim = character(0)) {
  dim == "column"
})

# Tour definition --------------------------------------------------------------

#' @export
setMethod(".definePanelTour", "SampleIdentificationCenter", function(x) {
  panel_name <- .getEncodedName(x)

  collated <- rbind(
    c(paste0("#", panel_name),
      sprintf("The <font color=\"%s\">Sample identification center</font> panel is dedicated to cell type annotation. It receives a column selection from another panel and generates ready-to-use R code to assign a cell type label to the selected cells. Alternatively, it does output the selected cells as plain text.", .getPanelColor(x))),

    .addTourStep(x, .dataParamBoxOpen,
                 "The <i>Data parameters</i> box contains all the annotation controls described in this tour.<br/><br/><strong>Action:</strong> click on this box to open up available options."),

    c(paste0("#", panel_name, "_", .EditorUsage),
      "This checkbox controls what is shown in the editor above.<br/><br/>When <b>checked</b>, the editor displays a full R command you can copy and run directly to annotate the selected cells.<br/><br/>When <b>unchecked</b>, the editor shows a plain list of cell IDs - useful if you just need the names for downstream use outside of R."),

    c(paste0("#", panel_name, "_", .AnnotationRationale),
      "Use this field to record <i>why</i> you are assigning this label - e.g. <i>Overexpression of marker X</i>.<br/><br/>When filled in, the text is automatically injected as a <code>## Rationale:</code> comment directly above the assignment command in the editor."),

    c(paste0("#", panel_name, "_", .ColDataColumn),
      "Enter the name of the <code>colData</code> column where the cell type label should be stored - e.g. <code>cell_type</code>.<br/><br/>This value is used in the generated command as the column index: <code>colData(se)[..., \'cell_type\']</code>. The column must already exist in your object to have a valid command (remember to initialize this if needed)."),

    c(paste0("#", panel_name, "_", .CellTypeLabel),
      "Enter the cell type label to assign to the selected cells - e.g. <code>CD4+ T cells</code>.<br/><br/>This becomes the right-hand side of the assignment in the generated command."),

    .addTourStep(x, .dataParamBoxOpen,
                 "You can basically iterate at will the selection in other panels, making sure this one receives it, and specify upon need the new names for the cells to assign."),

    .addTourStep(x, "SelectionBoxOpen",
                 "The <i>Selection parameters</i> box controls how this panel receives cell selections from other panels.<br/><br/><strong>Action:</strong> click on this box to open up the available options."),

    .addTourStep(x, "ColumnSelectionSource", is_selectize=TRUE,
                 "Use this dropdown to choose which panel transmits its column (cell) selection here - e.g. a <i>Reduced dimension plot</i> where you brushed or lassoed a group of cells.<br/><br/>Once set, any selection made in the chosen panel will immediately update the editor content in this panel."),

    c(paste0("#", panel_name),
      sprintf("The editor element is a bit of the heart for the <font color=\"%s\">Sample identification center</font> panel. By using this, cleverly combined with patience and efficient column selections from other panels, this can hopefully make the cell type annotation process a bit more breezy!", .getPanelColor(x)))

  )

  rbind(
    data.frame(element=collated[,1], intro=collated[,2], stringsAsFactors=FALSE)
  )
})


cellids_to_command <- function(cellids,
                               object_name = "se",
                               coldata_annotation = "cell_type",
                               new_cell_type_label = "new_cell_type",
                               comment_rationale = "") {

  rationale_line <- if (nzchar(trimws(comment_rationale))) {
    paste0("## Rationale: ", comment_rationale, "\n")
  } else {
    ""
  }

  cmd <- paste0(
    "## This is your SummarizedExperiment object\n# ", object_name,
    "\n\n## In this slot you store your annotation e.g. your cell label\n# ",
    "colData(", object_name, ")[['", coldata_annotation, "']]\n\n",
    "## To rename the selected cells to their new label, you can use the command(s) below\n\n",
    rationale_line,
    "colData(", object_name, ")[\n",
    "  c(",
    paste0(.quoteElement(cellids), collapse = ",\n    "),
    "\n  ), '", coldata_annotation, "'] <- '", new_cell_type_label, "'\n"
  )

  return(cmd)

}

.quoteElement <- function(x) {
  paste0("'", x, "'")
}
