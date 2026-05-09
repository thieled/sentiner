#' Initialize the sentiner Python environment
#'
#' Declares all required Python packages via [reticulate::py_require()],
#' which provisions them using `uv` into an ephemeral virtual environment.
#' The function is called automatically when the first sentiner function
#' is run.
#'
#' @details
#' The function for the most part grabs the GPU version of the Python
#' packages when `uv` discovers a NVIDIA, AMD, or Intel GPU.
#' `onnxruntime-gpu` only supports CUDA (NVIDIA), so the `gpu` parameter
#' reflects NVIDIA presence only; AMD and Intel users get CPU `onnxruntime`
#' while still getting GPU-accelerated PyTorch.
#'
#' If you want full control over which Python is used, pass an explicit
#' `python` argument or follow reticulate's
#' [order of discovery](https://rstudio.github.io/reticulate/articles/versions.html#order-of-discovery)
#' to bring your own interpreter or virtual environment.
#'
#' @section Controlling install locations:
#' By default, `uv` and the ephemeral environments it creates are stored
#' under [tools::R_user_dir()]`("reticulate", "cache")`. For an ML stack
#' like sentiner's (PyTorch, transformers, onnxruntime), this directory
#' can grow to several GB. To relocate it, set one or more of the
#' following environment variables in your `.Renviron` *before* starting
#' R (so they are picked up before reticulate or `uv` initialize):
#'
#' - `R_USER_CACHE_DIR` — reticulate's managed area, including downloaded
#'   `uv`, ephemeral virtual environments, and Python interpreters
#'   installed via `uv`. This is the broadest setting and usually the
#'   only one you need.
#' - `UV_CACHE_DIR` — `uv`'s own wheel cache. This is the directory that
#'   typically grows the fastest, since every resolved dependency version
#'   is cached here.
#' - `UV_PYTHON_INSTALL_DIR` — where `uv` stores Python interpreters it
#'   downloads. Useful if you want to share interpreters across projects
#'   but keep environments project-local.
#'
#' Avoid placing any of these on cloud-synced folders such as OneDrive,
#' Dropbox, or iCloud Drive: sync conflicts on the many small files in a
#' Python environment can corrupt installations and dramatically slow
#' down package resolution.
#'
#' Reticulate prunes its managed cache automatically every 120 days by
#' default. To shorten this, set in your `.Rprofile`:
#'
#' ```r
#' options(reticulate.max_cache_age = as.difftime(30, units = "days"))
#' ```
#'
#' To clear the cache manually:
#'
#' ```r
#' unlink(tools::R_user_dir("reticulate", "cache"), recursive = TRUE)
#' ```
#'
#' @param gpu Logical. Whether to install `onnxruntime-gpu` (CUDA only).
#'   Defaults to auto-detection via `check_gpu()`.
#' @param python Optional path to a Python interpreter or an existing
#'   virtual environment directory. If supplied, sentiner skips
#'   `py_require()` and binds to the provided Python via
#'   [reticulate::use_python()] or [reticulate::use_virtualenv()]. The
#'   user is then responsible for ensuring all required packages are
#'   available.
#'
#' @return Invisibly returns `TRUE`.
#' @export
initialize_sentiner <- function(gpu = check_gpu(), python = NULL) {
  if (isTRUE(.env$initialized_sentiner)) {
    return(invisible(TRUE))
  }

  if (!is.null(python)) {
    # User brought their own — skip py_require, just bind
    if (dir.exists(python)) {
      reticulate::use_virtualenv(python, required = TRUE)
    } else {
      reticulate::use_python(python, required = TRUE)
    }
    .env$initialized_sentiner <- TRUE
    return(invisible(TRUE))
  }

  # py_require() with the ephemeral-uv backend requires reticulate >= 1.41
  rlang::check_installed(
    "reticulate (>= 1.41.0)",
    reason = "to provision the sentiner Python environment via uv."
  )

  cli::cli_alert_info(
    "Initializing sentiner Python backend (first call may install dependencies)."
  )

  # Non-torch NLP stack — resolved together by uv via py_require
  reticulate::py_require(
    c(
      "transformers",
      "gliner",
      "accelerate",
      "sentencepiece==0.2.0",
      "protobuf",
      "pysimdjson",
      "cython"
    )
  )

  # UV_TORCH_BACKEND=auto lets uv detect NVIDIA/AMD/Intel and pick the right wheels
  withr::with_envvar(c(UV_TORCH_BACKEND = "auto"), {
    reticulate::py_require(c("torch", "torchvision", "torchaudio"))
  })

  # onnxruntime-gpu is CUDA-only; everything else gets the CPU build
  if (isTRUE(gpu)) {
    reticulate::py_require("onnxruntime-gpu")
  } else {
    reticulate::py_require("onnxruntime")
  }

  # this triggers the installation of packages and even python through uv
  try(reticulate::py_config())

  .env$initialized_sentiner <- TRUE
  invisible(TRUE)
}


#' CUDA-detection only, just for onnxruntime-gpu
#' @keywords internal
#' @noRd
check_gpu <- function() {
  if (!is.null(.env$gpu_info)) {
    return(.env$gpu_info$n > 0)
  }
  has_nvidia <- nzchar(Sys.which("nvidia-smi")) || file.exists("/dev/nvidiactl")
  .env$gpu_info <- list(n = as.integer(has_nvidia))
  has_nvidia
}
