test_that("clean_text produces proper output structure", {
  docs <- data.frame(
    doc_id = 1,
    text = "Donald Trump and Joe Biden are politicians."
  )
  
  cleaned <- sentiner::clean_text(
    docs,
    text_col = "text",
    id_col = "doc_id"
  )
  
  expect_true(data.table::is.data.table(cleaned))
  expect_true("text_clean" %in% names(cleaned))
})

test_that("clean_text with multiple docs produces rows", {
  docs <- data.frame(
    doc_id = c(1, 2),
    text = c(
      "The Republicans are a major party.",
      "The Democrats are also major."
    )
  )
  
  result <- sentiner::clean_text(
    docs,
    text_col = "text",
    id_col = "doc_id"
  )
  
  expect_true(nrow(result) > 0)
  expect_true(all(!is.na(result$text_clean)))
})

test_that("clean_text requires valid input", {
  docs <- data.frame(
    doc_id = 1,
    text = NA_character_
  )
  
  expect_error(
    sentiner::clean_text(docs, text_col = "text", id_col = "doc_id")
  )
})
