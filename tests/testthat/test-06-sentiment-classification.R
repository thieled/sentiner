test_that("get_targeted_sentiment returns data.table with expected columns", {
  skip_on_os("mac")
  skip_if_not(reticulate::py_module_available("transformers"), "transformers not available")
  
  docs <- data.frame(
    doc_id = 1,
    text = "I love Trump!"
  )
  
  cleaned <- sentiner::clean_text(docs, text_col = "text", id_col = "doc_id")
  ner_result <- sentiner::gliner_extract(cleaned, labels = "politician")
  
  skip_if(nrow(ner_result) == 0, "No entities extracted")
  
  result <- sentiner::get_targeted_sentiment(
    data = ner_result,
    text_col = "text_clean",
    entity_col = "entity_name",
    model = "dthiele/deberta-v3-base-targsenti-v5"
  )
  
  expect_is(result, "data.table")
  expect_true(nrow(result) > 0)
  expect_true(all(c("sentiment", "sentiment_confidence", "sentiment_model") %in% names(result)))
  expect_true(all(result$sentiment %in% c("positive", "neutral", "negative")))
  expect_true(all(result$sentiment_confidence >= 0 & result$sentiment_confidence <= 1))
})
