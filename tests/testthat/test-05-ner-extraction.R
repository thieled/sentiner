# Tests for NER extraction using GLiNER
# These tests verify that named entity recognition works

test_that("gliner_extract works end-to-end", {
  skip_if_not(reticulate::py_module_available("gliner"), "GLiNER not available")
  
  # Create test data once
  docs <- data.frame(
    doc_id = 1:3,
    text = c(
      "Donald Trump and the Republicans won.",
      "Hillary Clinton and the Democrats fought back.",
      "Biden is the current president."
    )
  )
  
  cleaned <- sentiner::clean_text(docs, text_col = "text", id_col = "doc_id")
  
  # Single extraction for all checks
  ner_result <- sentiner::gliner_extract(
    cleaned,
    labels = c("politician", "political_party")
  )
  
  # Validate structure
  expect_true(data.table::is.data.table(ner_result))
  expect_true(nrow(ner_result) > 0)
  
  # Validate required columns
  required_cols <- c("id", "entity_name", "label", "score", "start", "end", 
                     "ner_model", "ner_detected")
  expect_true(all(required_cols %in% names(ner_result)))
  
  # Validate scores are in valid range
  expect_true(all(ner_result$score >= 0 & ner_result$score <= 1))
  
  # Validate metadata columns
  expect_true(all(!is.na(ner_result$ner_model)))
  expect_true(all(!is.na(ner_result$ner_detected)))
  
  # Validate labels are correct
  expect_true(all(ner_result$label %in% c("politician", "political_party")))
  
  # Validate sentence tracking
  expect_true("sen_id" %in% names(ner_result))
  expect_true("sen_idx" %in% names(ner_result))
})
