#' @title Install and Set Up a Conda Environment
#'
#' @description
#' Ensures Miniconda is installed, creates (or recreates) a Conda environment
#' with the specified name and Python version, and activates it.
#' This function does not handle fastText installation, as the R package
#' \pkg{fastText} should be used instead.
#'
#' @param conda_env_name Character. The name of the Conda environment to create or use.
#'   Defaults to `"r-sentiner"`.
#' @param python_version Character. Python version to use when creating the Conda environment.
#'   Defaults to `"3.11"`. If the environment already exists, the version is not changed
#'   unless \code{force = TRUE}.
#' @param conda_path Character. Optional path to a Miniconda installation. If `NULL`,
#'   autodetects or installs Miniconda to the default location.
#' @param check_path Logical. Checks whether the installed conda is on PATH. Default is TRUE.
#' @param ask Logical. If `TRUE`, prompts the user before installing Miniconda.
#'   Default is `TRUE`.
#' @param force Logical. If `TRUE`, removes an existing Conda environment with the same
#'   name before recreating it with the requested Python version. Default is `FALSE`.
#' @param verbose Logical. If `TRUE`, prints progress messages. Default is `TRUE`.
#'
#' @return Invisibly returns `NULL`. Called for its side effects of installing and
#' configuring the Conda environment.
#'
#' @export
install_conda_env <- function(conda_env_name = "r-sentiner",
                              python_version = "3.11",
                              conda_path = NULL,
                              check_path = TRUE,
                              ask = TRUE,
                              force = FALSE,
                              verbose = TRUE) {
  vmessage <- function(...) if (verbose) message(...)

  # Step 1: Ensure Miniconda is installed
  t <- tryCatch({
    if (!is.null(conda_path)) {
      reticulate::conda_list(conda = file.path(conda_path, "bin", "conda"))
    } else {
      reticulate::conda_list()
    }
  }, error = function(e) NULL)

  if (is.null(t)) {
    permission <- TRUE
    if (ask) {
      cli::cli_alert_warning("No suitable conda installation was found.")
      response <- tolower(trimws(readline("Install Miniconda? (y/n): ")))
      permission <- response %in% c("y", "yes")
    }
    if (!permission) stop("Aborted by user")

    if (!is.null(conda_path)) {
      vmessage("Installing Miniconda at: ", conda_path)
      reticulate::install_miniconda(path = conda_path, force = force)
    } else {
      vmessage("Installing Miniconda at default location.")
      reticulate::install_miniconda(force = force)
    }
  }

  # Step 1b Ensure conda is on PATH
  if (check_path) {
    conda_cmd <- tryCatch(
      suppressWarnings(system2("conda", "--version", stdout = TRUE, stderr = TRUE)),
      error = function(e) NULL
    )
    if (is.null(conda_cmd) || length(conda_cmd) == 0) {
      if (!is.null(conda_path)) {
        bin_dir <- if (.Platform$OS.type == "windows") {
          file.path(conda_path, "Scripts")
        } else {
          file.path(conda_path, "bin")
        }
        if (dir.exists(bin_dir)) {
          vmessage("Adding conda to PATH: ", bin_dir)
          Sys.setenv(PATH = paste(bin_dir, Sys.getenv("PATH"), sep = .Platform$path.sep))
        } else {
          cli::cli_alert_warning("Conda binary directory not found: {bin_dir}")
        }
      }
    }
  }

  # Step 2: Check if env exists
  env_exists <- if (!is.null(conda_path)) {
    reticulate::condaenv_exists(envname = conda_env_name,
                                conda = file.path(conda_path, "bin", "conda"))
  } else {
    reticulate::condaenv_exists(envname = conda_env_name)
  }

  # Step 3: Remove env if force = TRUE and it exists
  if (env_exists && force) {
    vmessage("Removing existing conda environment: ", conda_env_name)
    if (!is.null(conda_path)) {
      reticulate::conda_remove(envname = conda_env_name,
                               conda = file.path(conda_path, "bin", "conda"))
    } else {
      reticulate::conda_remove(envname = conda_env_name)
    }
    env_exists <- FALSE
  }

  # Step 4: Create env if missing
  if (!env_exists) {
    vmessage("Creating conda environment: ", conda_env_name)
    if (!is.null(conda_path)) {
      reticulate::conda_create(envname = conda_env_name,
                               conda = file.path(conda_path, "bin", "conda"),
                               python_version = python_version)
    } else {
      reticulate::conda_create(envname = conda_env_name,
                               python_version = python_version)
    }
  } else {
    vmessage("Conda environment already exists: ", conda_env_name)
  }


  # Step 4: Activate env
  vmessage("Activating conda environment: ", conda_env_name)
  if (!is.null(conda_path)) {
    reticulate::use_condaenv(conda_env_name, required = TRUE,
                             conda = file.path(conda_path, "bin", "conda"))
  } else {
    reticulate::use_condaenv(conda_env_name, required = TRUE)
  }

  invisible(NULL)
}


# install_torch <- function(conda_env_name = "r-sentiner",
#                           python_version = "3.11",
#                           conda_path = NULL,
#                           verbose = TRUE) {
#
#   vmessage <- function(...) if (verbose) message(...)
#
#   # Make sure the environment exists
#   install_conda_env(
#     conda_env_name = conda_env_name,
#     python_version = python_version,
#     conda_path = conda_path,
#     ask = TRUE,
#     force = FALSE,
#     verbose = verbose
#   )
#
#   # Detect GPU
#   gpu_available <- FALSE
#   if (.Platform$OS.type == "windows") {
#     gpu_info <- try(system("wmic path win32_VideoController get name", intern = TRUE), silent = TRUE)
#     gpu_available <- any(grepl("NVIDIA", gpu_info, ignore.case = TRUE))
#   } else {
#     gpu_info <- try(system("nvidia-smi", intern = TRUE), silent = TRUE)
#     gpu_available <- !inherits(gpu_info, "try-error") && length(gpu_info) > 0
#   }
#
#   # Detect CUDA version if GPU present
#   cuda_version <- NULL
#   if (gpu_available) {
#     nvcc_output <- tryCatch(system("nvcc --version", intern = TRUE), error = function(e) NULL)
#     if (!is.null(nvcc_output)) {
#       version_line <- nvcc_output[grepl("release", nvcc_output)]
#       cuda_version <- sub(".*release ([0-9]+\\.[0-9]+).*", "\\1", version_line)
#     }
#   }
#
#   # Construct install URL
#   base_url <- "https://download.pytorch.org/whl/"
#   index_url <- if (is.null(cuda_version)) {
#     paste0(base_url, "cpu")
#   } else {
#     paste0(base_url, "cu", gsub("\\.", "", cuda_version))
#   }
#
#   # Validate URL
#   validate_url <- function(url) {
#     if (!requireNamespace("httr", quietly = TRUE)) stop("Package 'httr' is required.")
#     response <- tryCatch(httr::HEAD(url), error = function(e) NULL)
#     !is.null(response) && httr::status_code(response) == 200
#   }
#
#   if (!validate_url(index_url)) {
#     cli::cli_warn("Invalid or unavailable PyTorch URL: {index_url}. Falling back to CPU-only build.")
#     index_url <- paste0(base_url, "cpu")
#     cuda_version <- NULL
#   }
#
#   vmessage("Installing PyTorch into conda environment '", conda_env_name, "' ...")
#   if (is.null(cuda_version)) {
#     vmessage("No CUDA detected. Installing CPU-only build.")
#   } else {
#     vmessage("CUDA ", cuda_version, " detected. Installing GPU build.")
#   }
#
#   # Install via reticulate into conda env
#   tryCatch({
#     reticulate::py_install(
#       packages = c("torch", "torchvision", "torchaudio"),
#       envname = conda_env_name,
#       method = "conda",
#       pip = TRUE,
#       pip_options = paste("--index-url", index_url)
#     )
#     vmessage("Torch installation completed successfully.")
#   }, error = function(e) {
#     stop("Torch installation failed: ", e$message)
#   })
#
#   # Verify installation
#   tryCatch({
#     torch <- reticulate::import("torch", delay_load = TRUE)
#     vmessage("Torch successfully installed and imported.")
#     vmessage("Torch version: ", torch$`__version__`)
#     vmessage("CUDA available: ", torch$cuda$is_available())
#     if (!is.null(cuda_version)) {
#       vmessage("CUDA version (from torch): ", torch$version$cuda)
#     }
#   }, error = function(e) {
#     stop("Verification failed: Torch is not properly installed.")
#   })
#
#   # Install onnxruntime-gpu / onnxruntime via reticulate into conda env
#   tryCatch({
#
#     if(gpu_available){
#       reticulate::py_install(
#         packages = c("onnxruntime-gpu"),
#         envname = conda_env_name,
#         method = "conda",
#         pip = TRUE
#       )
#       vmessage("Installed onnxruntime-gpu.")
#       }else{
#         reticulate::py_install(
#           packages = c("onnxruntime"),
#           envname = conda_env_name,
#           method = "conda",
#           pip = TRUE)
#         vmessage("Installed onnxruntime.")
#       }
#
#   }, error = function(e) {
#     stop("onnxruntime installation failed: ", e$message)
#   })
#
#   invisible(NULL)
# }




#' @title Install PyTorch in a Conda Environment
#'
#' @description Installs PyTorch and related libraries (`torch`, `torchvision`, `torchaudio`)
#' in a Conda environment created with [install_conda_env()]. CUDA availability is detected
#' via the NVIDIA driver (`nvidia-smi`, not `nvcc`). A compatible PyTorch wheel index is selected
#' using a conservative mapping that **never chooses below cu126** and (by default) **prefers cu126**
#' for broad compatibility. Falls back to CPU wheels if needed.
#'
#' @param conda_env_name Character. Name of the conda environment. Default "r-easynmt".
#' @param python_version Character. Python version to use when creating the Conda environment.
#'   Defaults to `"3.11"`. If the environment already exists, the version is not changed.
#' @param conda_path Character. Path to Conda installation. If NULL, autodetect.
#' @param verbose Logical. If TRUE, prints progress messages. Default TRUE.
#'
#' @return Invisibly returns `NULL`.
#' @export
install_torch <- function(conda_env_name = "r-easynmt",
                          python_version = "3.11",
                          conda_path = NULL,
                          verbose = TRUE) {

  vmessage <- function(...) if (isTRUE(verbose)) base::message(...)

  install_conda_env(
    conda_env_name = conda_env_name,
    python_version = python_version,
    conda_path = conda_path,
    ask = TRUE,
    force = FALSE,
    verbose = verbose
  )

  gpu_available <- check_gpu()
  cuda_version <- if (isTRUE(gpu_available)) get_cuda_version() else NULL

  index_url <- construct_pytorch_install_url(cuda_version)

  if (!validate_url(index_url)) {
    cli::cli_warn("Invalid or unavailable PyTorch URL: {index_url}. Falling back to CPU-only build.")
    index_url <- "https://download.pytorch.org/whl/cpu"
    cuda_version <- NULL
  }

  vmessage("Installing PyTorch into conda environment '", conda_env_name, "' ...")
  if (is.null(cuda_version)) {
    vmessage("No GPU detected or no driver-reported CUDA. Installing CPU-only build.")
  } else {
    vmessage("Driver-reported CUDA ", cuda_version, " detected. Installing GPU build.")
  }

  tryCatch(
    {
      reticulate::py_install(
        packages = c("torch", "torchvision", "torchaudio"),
        envname = conda_env_name,
        method = "conda",
        pip = TRUE,
        pip_options = paste("--index-url", index_url)
      )
      vmessage("Torch installation completed successfully.")
    },
    error = function(e) {
      base::stop("Torch installation failed: ", conditionMessage(e))
    }
  )

  tryCatch(
    {
      torch <- reticulate::import("torch", delay_load = TRUE)
      vmessage("Torch successfully installed and imported.")
      vmessage("Torch version: ", torch$`__version__`)
      vmessage("CUDA available (torch): ", torch$cuda$is_available())
      if (!is.null(cuda_version)) {
        vmessage("CUDA version (from torch): ", torch$version$cuda)
      }
    },
    error = function(e) {
      base::stop("Verification failed: Torch is not properly installed.")
    }
  )

  # Install onnxruntime-gpu / onnxruntime via reticulate into conda env
  tryCatch({

    if(gpu_available){
      reticulate::py_install(
        packages = c("onnxruntime-gpu"),
        envname = conda_env_name,
        method = "conda",
        pip = TRUE
      )
      vmessage("Installed onnxruntime-gpu.")
    }else{
      reticulate::py_install(
        packages = c("onnxruntime"),
        envname = conda_env_name,
        method = "conda",
        pip = TRUE)
      vmessage("Installed onnxruntime.")
    }

  }, error = function(e) {
    stop("onnxruntime installation failed: ", e$message)
  })

  invisible(NULL)
}







#' @title Detect GPU availability (NVIDIA)
#'
#' @description Internal helper that checks for an NVIDIA GPU. Prefers `nvidia-smi -L` when
#' available; on Windows falls back to `wmic` if needed.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
check_gpu <- function() {
  if (nzchar(Sys.which("nvidia-smi"))) {
    out <- try(base::system("nvidia-smi -L", intern = TRUE), silent = TRUE)
    return(!inherits(out, "try-error") && base::length(out) > 0)
  }

  if (identical(.Platform$OS.type, "windows")) {
    gpu_info <- try(
      base::system("wmic path win32_VideoController get name", intern = TRUE),
      silent = TRUE
    )
    return(!inherits(gpu_info, "try-error") && any(grepl("NVIDIA", gpu_info, ignore.case = TRUE)))
  }

  FALSE
}

#' @title Detect CUDA availability (driver-level)
#'
#' @description Internal helper that checks for CUDA availability via the NVIDIA driver.
#' Uses `nvidia-smi -L` when available; on Windows falls back to GPU presence.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
check_cuda <- function() {
  if (identical(.Platform$OS.type, "windows")) {
    out <- try(base::system("nvidia-smi -L", intern = TRUE), silent = TRUE)
    if (!inherits(out, "try-error") && base::length(out) > 0) return(TRUE)
    return(check_gpu())
  }

  out <- try(base::system("nvidia-smi -L", intern = TRUE), silent = TRUE)
  !inherits(out, "try-error") && base::length(out) > 0
}

#' @title Get CUDA version (driver-reported)
#'
#' @description Internal helper that extracts the CUDA version reported by the NVIDIA driver
#' using `nvidia-smi` (not `nvcc`).
#'
#' @return Character scalar like `"12.6"`, or `NULL`.
#' @keywords internal
#' @noRd
get_cuda_version <- function() {
  out <- try(base::system("nvidia-smi", intern = TRUE), silent = TRUE)
  if (inherits(out, "try-error") || base::length(out) == 0) return(NULL)

  line <- out[grepl("CUDA Version", out, ignore.case = TRUE)]
  if (base::length(line) == 0) return(NULL)

  m <- base::regexec("CUDA Version:\\s*([0-9]+\\.[0-9]+)", line[1])
  regm <- base::regmatches(line[1], m)
  if (base::length(regm) == 0 || base::length(regm[[1]]) < 2) return(NULL)

  regm[[1]][2]
}

#' @title Detect NVIDIA GPU name
#'
#' @description Internal helper to detect the NVIDIA GPU name without torch or nvcc.
#' Prefers `nvidia-smi --query-gpu=name` and falls back to `wmic` on Windows.
#'
#' @return Character scalar GPU name, or `NULL`.
#' @keywords internal
#' @noRd
get_nvidia_gpu_name <- function() {
  if (identical(.Platform$OS.type, "windows")) {
    out <- try(
      base::system("nvidia-smi --query-gpu=name --format=csv,noheader", intern = TRUE),
      silent = TRUE
    )
    if (!inherits(out, "try-error") && base::length(out) > 0) return(base::trimws(out[1]))

    out2 <- try(
      base::system("wmic path win32_VideoController get name", intern = TRUE),
      silent = TRUE
    )
    if (inherits(out2, "try-error") || base::length(out2) == 0) return(NULL)

    out2 <- base::trimws(out2)
    out2 <- out2[nzchar(out2)]
    out2 <- out2[!grepl("^name$", out2, ignore.case = TRUE)]
    if (base::length(out2) == 0) return(NULL)
    return(out2[1])
  }

  out <- try(
    base::system("nvidia-smi --query-gpu=name --format=csv,noheader", intern = TRUE),
    silent = TRUE
  )
  if (inherits(out, "try-error") || base::length(out) == 0) return(NULL)
  base::trimws(out[1])
}

#' @title Choose PyTorch CUDA wheel tag (conservative)
#'
#' @description Internal helper that selects the PyTorch wheel index tag.
#' Rules:
#' - If no CUDA driver -> `"cpu"`
#' - Never select tags below `"cu126"` (avoid cu118/cu121 entirely)
#' - Prefer `"cu126"` by default for widest compatibility
#' - Optionally allow `"cu128"` only if driver reports >= 12.8
#' - Never select `"cu130"` (even if driver reports >= 13.0), to avoid surprising upgrades
#' - Pascal guard: Tesla P100 always uses `"cu126"`
#'
#' @param cuda_version Character scalar like `"12.6"`, or `NULL`.
#' @param gpu_name Character scalar GPU name, or `NULL`.
#'
#' @return Character scalar: `"cpu"`, `"cu126"`, or `"cu128"`.
#' @keywords internal
#' @noRd
choose_pytorch_cuda_tag <- function(cuda_version, gpu_name = NULL) {
  if (is.null(cuda_version)) return("cpu")

  if (!is.null(gpu_name) && grepl("\\bP100\\b", gpu_name, ignore.case = TRUE)) {
    return("cu126")
  }

  v <- suppressWarnings(base::as.numeric(cuda_version))
  if (is.na(v)) return("cpu")

  # Hard floor at 12.6 and never choose cu130.
  if (v < 12.6) return("cpu")
  if (v >= 12.8) return("cu128")
  "cu126"
}

#' @title Construct PyTorch installation URL
#'
#' @description Internal helper to build the PyTorch wheel index URL based on the selected CUDA tag.
#'
#' @param cuda_version Character scalar like `"12.6"`, or `NULL`.
#'
#' @return Character scalar URL.
#' @keywords internal
#' @noRd
construct_pytorch_install_url <- function(cuda_version) {
  base_url <- "https://download.pytorch.org/whl/"

  gpu_name <- get_nvidia_gpu_name()
  tag <- choose_pytorch_cuda_tag(cuda_version = cuda_version, gpu_name = gpu_name)

  paste0(base_url, tag)
}

#' @title Validate URL by checking HTTP response
#'
#' @description Internal helper that validates a URL with an HTTP HEAD request.
#'
#' @param url Character scalar.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
validate_url <- function(url) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    base::stop("Package 'httr' is required for URL validation.")
  }

  response <- tryCatch(httr::HEAD(url), error = function(e) NULL)
  !is.null(response) && httr::status_code(response) == 200
}









#' @title Install Python Dependencies for easieRnmt
#' @description Installs required Python dependencies into the active environment.
#' This function can be extended with additional packages as needed.
#' @param packages Character vector of Python packages to install.
#'   Default includes "pytz" and "python-dateutil".
#' @param envname Name of the conda or virtual environment where packages
#'   should be installed. If NULL, installs into the active environment.
#' @param method Installation method passed to reticulate::py_install
#'   ("auto", "virtualenv", or "conda").
#' @param pip Logical; if TRUE, force installation via pip.
#' @param ... Further arguments passed to reticulate::py_install().
#' @export
install_deps <- function(
    packages = c("pytz",
                 "python-dateutil"),
    envname = NULL,
    method = "auto",
    pip = TRUE,
    ...
) {
  tryCatch({
    reticulate::py_install(
      packages = packages,
      envname = envname,
      method = method,
      pip = pip,
      ...
    )
    message("Successfully installed Python dependencies: ", paste(packages, collapse = ", "))
  }, error = function(e) {
    warning("Failed to install dependencies: ", conditionMessage(e))
  })
}




#' @title Install GLiNER in a conda Environment
#'
#' @description
#' Installs the GLiNER library and its dependencies in a conda environment
#' managed through \code{reticulate}. It creates or activates the specified
#' environment, installs PyTorch (with CUDA if available, otherwise CPU),
#' installs FastText (using a precompiled wheel on Windows), and finally
#' installs gliner with all required dependencies.
#'
#' Note: Calling GliNER from reticulate is hacky, as there is a persistent
#' DLL Error. At the moment, I implemeted a workaround that calls gliner
#' externally, without embedding it via reticulate.
#'
#' @param python_version Character string. Python version to use when creating
#'   the conda environment. Default is \code{"3.11"}.
#' @param conda_env_name Character string. Name of the conda environment to
#'   create or activate. Default is \code{"r-sentiner"}.
#' @param ask Logical. If \code{TRUE}, ask before installing Miniconda. Default
#'   is \code{TRUE}.
#' @param force Logical. If \code{TRUE}, force reinstallation of the conda
#'   environment and Python packages. Default is \code{FALSE}.
#' @param verbose Logical. If \code{TRUE}, print progress messages during
#'   installation. Default is \code{TRUE}.
#' @param conda_path Optional character string. Path to a conda installation. If
#'   \code{NULL}, the default conda installation is used.
#' @param check_path Logical. Checks whether the installed conda is on PATH. Default is TRUE.
#'
#' The function installs the following dependencies if FastText is not
#' available: \code{onnxruntime}, \code{numpy}, \code{protobuf}, \code{sentencepiece},
#' \code{torch}, \code{tqdm}, \code{transformers}, \code{cython},
#' \code{pysimdjson}, \code{accelerate}, and \code{gliner}.
#'
#' @export
install_sentiner <- function(
    python_version = "3.11",
    conda_env_name = "r-sentiner",
    check_path = TRUE,
    ask = TRUE,
    force = FALSE,
    verbose = TRUE,
    conda_path = NULL
) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Please install reticulate first.")
  }

  # 1. Install or activate conda env (installs miniconda if not present)
  install_conda_env(
    python_version = python_version,
    conda_env_name = conda_env_name,
    check_path = check_path,
    ask = ask,
    force = force,
    verbose = verbose,
    conda_path = conda_path
  )

  # 2. Install PyTorch, with automatic CUDA or CPU support
  install_torch(conda_env_name = conda_env_name,
                conda_path = conda_path,
                verbose = verbose)


  # 3. Install gliner and dependencis
  pckgs <- c("transformers",
              "gliner",
              "accelerate",
              "sentencepiece==0.2.0",
              "protobuf",
              "pysimdjson",
              "cython")

  reticulate::py_install(
    packages = pckgs,
    envname = conda_env_name,
    method = "conda",
    pip = TRUE)

  # 7. Mark as initialized
  options("sentiner_initialized" = TRUE)

  invisible(TRUE)
}



#' @title Initialize the r-sentiner Conda Environment
#'
#' @description
#' Initializes and verifies the Python environment used by \pkg{gliner}.
#' It builds on \code{\link{install_sentiner}} by activating the specified
#' conda environment, checking the Python configuration, and verifying that
#' required Python packages are correctly installed.
#'
#' Specifically, the function:
#' \itemize{
#'   \item Ensures that the Python environment created by
#'   \code{install_sentiner()} is active.
#'   \item Prints the active environment, Python path, and Python version.
#'   \item Verifies that PyTorch is installed and reports its version as
#'   well as whether CUDA is available.
#'   \item Checks whether \code{gliner} are installed
#'   and reports their versions.
#'   \item Sets a global option (\code{sentiner_initialized = TRUE}) so that
#'   subsequent calls can skip redundant initialization.
#' }
#'
#' @param python_version Character scalar. Python version to use for the
#' environment. Default is \code{"3.11"}.
#' @param conda_env_name Character scalar. Name of the conda environment to
#' create or activate. Default is \code{"r-sentiner"}.
#' @param ask Logical. If \code{TRUE}, prompt the user before creating or
#' overwriting an environment. Default is \code{TRUE}.
#' @param force Logical. If \code{TRUE}, force reinstallation of the
#' environment even if it already exists. Default is \code{FALSE}.
#' @param verbose Logical. If \code{TRUE}, print informative messages about
#' the environment configuration and installed packages. Default is
#' \code{TRUE}.
#' @param conda_path Optional character scalar. Path to the conda
#' installation to use. If \code{NULL}, the default conda installation is
#' used.
#'
#' @return This function is called for its side effects. It prints messages
#' about the environment and package versions if \code{verbose = TRUE}, and
#' sets the global option \code{sentiner_initialized = TRUE}.
#'
#' @export
initialize_sentiner <- function(python_version = "3.11",
                               conda_env_name = "r-sentiner",
                               ask = TRUE,
                               force = FALSE,
                               verbose = TRUE,
                               conda_path = NULL) {

  vmessage <- function(...) if (verbose) message(...)


  ## Helper:
  get_python_module_version <- function(module, conda_env_name) {
      out <- processx::run(
        "conda",
        c("run", "-n", conda_env_name,
          "python", "-c", sprintf("import %s; print(%s.__version__)", module, module)),
        echo = FALSE,
        error_on_status = FALSE
      )$stdout

      # Clean up output: remove CR/LF and trim
      version <- gsub("[\r\n]", "", out)
      version <- trimws(version)

      if (nzchar(version)) version else NA_character_
    }


  if (!is.null(options("sentiner_initialized")$sentiner_initialized)) {
    vmessage("r-sentiner environment is already initialized.")
    gliner_version <- get_python_module_version("gliner", conda_env_name)

  } else {

    install_conda_env(python_version = python_version,
                      conda_env_name = conda_env_name,
                      ask = ask,
                      force = force,
                      verbose = verbose,
                      conda_path = conda_path)

    # Check if torch is available
    if (!reticulate::py_module_available("torch")) {
      install_torch(conda_env_name = conda_env_name,
                    conda_path = conda_path,
                    verbose = verbose)
    }

    # Check if gliner is available -- Note: due to a dll issue on Windows, this is hacky
    gliner_version <- get_python_module_version("gliner", conda_env_name)

    if (is.null(gliner_version)) {
      install_sentiner(python_version = python_version,
                      conda_env_name = conda_env_name,
                      ask = ask,
                      force = force,
                      verbose = verbose,
                      conda_path = conda_path)
    }
  }

  # Verify Python installation
  cfg <- tryCatch(reticulate::py_config(), error = function(e) NULL)
  python <- if (!is.null(cfg)) cfg$python else "not initialized"
  libpython <- if (!is.null(cfg)) cfg$libpython else "not initialized"
  version <- if (is.null(cfg) || all(is.na(cfg$version))) "unknown" else
    if (is.character(cfg$version)) cfg$version else paste(cfg$version, collapse = ".")

  vmessage("Python: ", python)
  vmessage("Libpython: ", libpython)
  vmessage("Python Version: ", version)

  # Verify PyTorch installation
  tryCatch({
    torch <- reticulate::import("torch")
    vmessage("Torch version: ", torch$`__version__`)
    vmessage("CUDA available: ", torch$cuda$is_available())
  }, error = function(e) {
    stop("Verification failed: Torch is not properly installed.")
  })

  if (!is.null(gliner_version)) {
    vmessage("gliner version: ", gliner_version)
  } else {
    vmessage("gliner not found in active Python environment.")
  }

  options("sentiner_initialized" = TRUE)
}



