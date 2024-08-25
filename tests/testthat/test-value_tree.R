test_that("value_tree returns a DiagrammeR object", {
  diagram <- "graph LR; A-->B"
  result <- value_tree(diagram)
  expect_s3_class(result, "htmlwidget")
  expect_s3_class(result, "DiagrammeR")
})

test_that("value_tree handles empty diagram", {
  expect_error(value_tree(""), "A diagram specification must be provided")
})

test_that("value_tree passes additional arguments to mermaid", {
  diagram <- "graph LR; A-->B"
  result <- value_tree(diagram, width = 800, height = 600)
  expect_equal(result$width, 800)
  expect_equal(result$height, 600)
})

test_that("value_tree correctly processes complex diagrams", {
  complex_diagram <- "
    graph LR;
    A(<B>Benefit-Risk Balance</B>)-->B(<B>Benefits</B>)
    B-->C(<B>Primary Efficacy</B>)
    A-->D(<B>Risks</B>)
    style A fill:#7ABD7E
    style B fill:#FFE733
  "
  result <- value_tree(complex_diagram)
  expect_s3_class(result, "DiagrammeR")
})

test_that("value_tree handles invalid mermaid syntax", {
  invalid_diagram <- "graph DT; A-->"
  expect_error(value_tree(invalid_diagram), "Invalid graph specification: Must start with 'graph TB;', 'graph BT;', 'graph RL;', or 'graph LR;'")
})

test_that("value_tree preserves diagram content", {
  diagram <- "graph LR; A-->B"
  result <- value_tree(diagram)
  expect_true(grepl("graph LR; A-->B", result$x$diagram))
})
