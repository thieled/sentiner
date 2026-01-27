# R script to find the gliner_runner.py location on your VM
# Run this on your VM to find where the script is installed

cat("Finding sentiner installation paths...\n\n")

# Package installation directory
pkg_path <- system.file(package = "sentiner")
cat("Package root:", pkg_path, "\n")

# Python script location
script_path <- system.file("python", "gliner_runner.py", package = "sentiner")
cat("Python script:", script_path, "\n")

# Check if file exists
if (file.exists(script_path)) {
  cat("\n✓ Script file exists\n")
  cat("File size:", file.size(script_path), "bytes\n")
} else {
  cat("\n✗ Script file NOT FOUND\n")
}

# Show all files in inst/python directory
python_dir <- system.file("python", package = "sentiner")
if (dir.exists(python_dir)) {
  cat("\nFiles in python directory:\n")
  print(list.files(python_dir, full.names = TRUE))
}
