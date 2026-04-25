#' @title Initialize the sentiner Python environment
#'
#' @description
#' Declares all required Python packages via \code{\link[reticulate]{py_require}},
#' which provisions them using uv into an ephemeral virtual environment.
#' PyTorch is installed with \code{UV_TORCH_BACKEND=auto}, which lets uv
#' auto-detect NVIDIA (CUDA), AMD (ROCm), and Intel GPUs and pick the correct
#' wheels — no manual driver querying needed. Pass \code{gpu = FALSE} to force
#' CPU-only onnxruntime.
#'
#' The function is called automatically at the start of each sentiner function
#' and is a no-op after the first successful call within a session.
#'
#' @details
#' Non-torch packages are declared with \code{py_require()} so that reticulate /
#' uv can resolve them together. PyTorch is handled by setting
#' \code{UV_TORCH_BACKEND=auto}, which instructs uv to query for CUDA, ROCm,
#' and Intel GPU drivers and select the matching PyTorch index automatically.
#'
#' \code{onnxruntime-gpu} only supports CUDA, so the \code{gpu} parameter
#' reflects NVIDIA presence only; AMD and Intel users get CPU onnxruntime while
#' still getting GPU-accelerated PyTorch.
#'
#' @param gpu Logical. Whether to install \code{onnxruntime-gpu} (CUDA only).
#'   Defaults to auto-detection via \code{check_gpu()}.
#'
#' @return Invisibly returns \code{TRUE}.
#' @export
initialize_sentiner <- function(gpu = check_gpu()) {
  if (isTRUE(.env$initialized_sentiner)) {
    return(invisible(TRUE))
  }

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
