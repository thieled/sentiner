## Other Changes

- Added comprehensive test suite for Python backend integration
- Updated documentation for `initialize_sentiner()` with cache control examples

## Breaking Changes

- `conda_env_name` parameter removed from `get_targeted_sentiment()` (no longer applicable with uv)
- Requires Python 3.12+ (managed by uv)**`check_backend()`**: New diagnostic function to verify Python backend setup
  - Reports GPU detection status
  - Shows PyTorch version and CUDA availability
  - Displays cache directory locations
  
- **Cache directory control**: Users can now customize Python package and model cache locations
  - New parameters in `initialize_sentiner()`: `uv_cache_dir` and `models_cache_dir`
  - Essential for deployment on VMs with limited root storage

## Bug Fixes

- Fixed GPU recognition on Windows systems (now properly detects CUDA via environment variables)
- Fixed missing `pandas` dependency in Python backend

## Documentation

- Added comprehensive test suite for Python backend integration
- Improved documentation for `initialize_sentiner()` with cache control examples

## Technical Details

- **Breaking changes**: `conda_env_name` parameter removed from `get_targeted_sentiment()` (no longer needed with uv)
- Requires Python 3.12+ (managed by uv)
- Minimum PyTorch version: 2.0
