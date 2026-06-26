# Tests for targeted sentiment classification - optimized for speed
# Single extraction reused across all sentiment checks

test_that("get_targeted_sentiment completes successfully", {
  skip_if_not(reticulate::py_module_available("transformers"), "transformers not available")
  
  docs <- data.frame(
    doc_id = 1:2,
    text = c(
      "I love Donald Trump and Republicans!",
      "I dislike Joe Biden and Democrats."
    )
  )
  
  cleaned <- sentiner::clean_text(docs, text_col = "text", id_col = "doc_id")
  ner_result <- sentiner::gliner_extract(cleaned, labels = c("politician", "political_party"))
  
  skip_if(nrow(ner_result) == 0, "No entities extracted")
  
  senti_result <- sentiner::get_targeted_sentiment(
    data = ner_result,
    text_col = "text_clean",
    entity_col = "entity_name",
    model = "dthiele/deberta-v3-base-targsenti-v5"
  )
  
  expect_true(data.table::is.data.table(senti_result))
  expect_true(nrow(senti_result) > 0)
  expect_true("sentiment" %in% names(senti_result))
  expect_true("sentiment_confidence" %in% names(senti_result))
  expect_true("sentiment_model" %in% names(senti_result))
})

test_that("sentiment labels are valid classes", {
  skip_if_not(reticulate::py_module_available("transformers"), "transformers not available")
  
  docs <- data.frame(doc_id = 1, text = "I love Trump.")
  cleaned <- sentiner::clean_text(docs, text_col = "text", id_col = "doc_id")
  ner_result <- sentiner::gliner_extract(cleaned, labels = "politician")
  
  skip_if(nrow(ner_result) == 0, "No entities extracted")
  
  senti_result <- sentiner::get_targeted_sentiment(
    data = ner_result,
    text_col = "text_clean",
    entity_col = "entity_name",
    model = "dthiele/deberta-v3-base-targsenti-v5"
  )
  
  valid_sentiments <- c("positive", "neutral", "negative")
  expect_true(all(senti_result$sentiment %in% valid_sentiments))
})

test_that("confidence scores are valid probabilities", {
  skip_if_not(reticulate::py_module_available("transformers"), "transformers not available")
  
  docs <- data.frame(doc_id = 1, text = "Biden is bad.")
  cleaned <- sentiner::clean_text(docs, text_col = "text", id_col = "doc_id")
  ner_result <- sentiner::gliner_extract(cleaned, labels = "politician")
  
  skip_if(nrow(ner_result) == 0, "No entities extracted")
  
  senti_result <- sentiner::get_targeted_sentiment(
    data = ner_result,
    text_col = "text_clean",
    entity_col = "entity_name",
    model = "dthiele/deberta-v3-base-targsenti-v5"
  )
  
  expect_true(all(senti_result$sentiment_confidence >= 0 & senti_result$sentiment_confidence <= 1))
})

test_that("model metadata is recorded", {
  skip_if_not(reticulate::py_module_available("transformers"), "transformers not available")
  
  docs <- data.frame(doc_id = 1, text = "Harris is great.")
  cleaned <- sentiner::clean_text(docs, text_col = "text", id_col = "doc_id")
  ner_result <- sentiner::gliner_extract(cleaned, labels = "politician")
  
  skip_if(nrow(ner_result) == 0, "No entities extracted")
  
  senti_result <- sentiner::get_targeted_sentiment(
    data = ner_result,
    text_col = "text_clean",
    entity_col = "entity_name",
    model = "dthiele/deberta-v3-base-targsenti-v5"
  )
  
  expect_true(all(!is.na(senti_result$sentiment_model)))
  expect_true(grepl("deberta", senti_result$sentiment_model[1]))
})
