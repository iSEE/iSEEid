SampleIdentificationCenter

test_that("SampleIdentificationCenter() constructor works", {
  out <- SampleIdentificationCenter()

  expect_s4_class(out, "SampleIdentificationCenter")
})

test_that(".fullName(SampleIdentificationCenter) works", {
  x <- SampleIdentificationCenter()
  out <- .fullName(x)

  expect_identical(out, "Sample identification center panel")
})

test_that(".panelColor(SampleIdentificationCenter) works", {
  x <- SampleIdentificationCenter()
  out <- .panelColor(x)

  expect_identical(out, "#00C4DA")
})

test_that(".createObservers(SampleIdentificationCenter) works", {
  x <- SampleIdentificationCenter()
  out <- .createObservers(x, se, NULL, NULL, NULL, NULL)

  expect_null(out)
})

test_that("SampleIdentificationCenter interface is generated correctly", {
  x <- SampleIdentificationCenter(PanelId=1L)
  expect_error(ui <- .defineInterface(x, SummarizedExperiment(), list()), NA)
  expect_true(any(grepl("SampleIdentificationCenter1_", as.character(ui))))
  expect_false(.hideInterface(x, "SelectionParamOpen"))

  out.ui <- .defineOutput(x)
  expect_true(any(grepl("SampleIdentificationCenter1", as.character(out.ui))))
})

test_that("SampleIdentificationCenter tour generation works correctly", {
  x <- SampleIdentificationCenter(PanelId=1L)
  tour <- .definePanelTour(x)

  expect_identical(colnames(tour), c("element", "intro"))
})

test_that(".generateOutput(SampleIdentificationCenter) works", {
  x <- SampleIdentificationCenter()

  out <- .generateOutput(x, se = sce)

  expect_true(is.list(out))
})

test_that("SampleIdentificationCenter tour generation works correctly", {
  x <- SampleIdentificationCenter(PanelId=1L)
  tour <- .definePanelTour(x)

  expect_identical(colnames(tour), c("element", "intro"))
})

test_that("cellids_to_command works", {
  cell_ids <- c("SRR2140028", "SRR2140022")

  out <- cellids_to_command(cellids = cell_ids, object_name = "sce",
                            comment_rationale = "just for testing")

  expect_true(is.character(out))
  expect_true(grepl(pattern = "## Rationale: just for testing", x = out))

  out_text <- cellids_to_command(cellids = cell_ids, object_name = "sce",
                                 comment_rationale = "just for testing")
})
