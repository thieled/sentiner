test_that("clean_text returns data.table", {
  docs <- data.frame(doc_id = 1, text = "Hello world.")
  result <- sentiner::clean_text(docs, text_col = "text", id_col = "doc_id")
  expect_true(data.table::is.data.table(result))
  expect_true("text_clean" %in% names(result))
})

test_that("clean_text produces output", {
  docs <- data.frame(doc_id = 1, text = "First sentence. Second sentence.")
  result <- sentiner::clean_text(docs, text_col = "text", id_col = "doc_id")
  expect_true(nrow(result) > 0)
})

test_that("replace_emoji_with_name removes emojis", {
  result <- sentiner::replace_emoji_with_name("Hello 😊")
  expect_false(grepl("😊", result))
})

test_that("replace_emoji_with_name preserves text without emojis", {
  text <- "No emojis here"
  result <- sentiner::replace_emoji_with_name(text)
  expect_equal(result, text)
})
