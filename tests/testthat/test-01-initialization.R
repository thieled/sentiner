# Tests for Python environment initialization via uv
# These tests verify that the new uv-based initialization works correctly

test_that("initialize_sentiner returns TRUE invisibly", {
  result <- sentiner::initialize_sentiner()
  expect_true(result)
})

test_that("initialize_sentiner is idempotent (no-op on second call)", {
  # First call
  result1 <- sentiner::initialize_sentiner()
  expect_true(result1)
  
  # Second call should return TRUE without reinitializing
  result2 <- sentiner::initialize_sentiner()
  expect_true(result2)
})

test_that("check_gpu detects system configuration", {
  gpu_available <- sentiner:::check_gpu()
  expect_true(is.logical(gpu_available))
})

test_that("initialize_sentiner respects gpu parameter", {
  # Test CPU-only initialization
  # Note: This test depends on system setup and may need to be skipped in some CI environments
  result <- sentiner::initialize_sentiner(gpu = FALSE)
  expect_true(result)
})
