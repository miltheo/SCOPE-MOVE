# Changelog

This file records repository-level SCOPE-MOVE changes. User-facing application changes are tracked separately in `app/CHANGELOG.md`.

## 2.0.0 (2026-05-14)

### Changed
- Replaced the earlier Shiny app codebase with a static HTML, CSS, and JavaScript application.
- Updated the scoping review evidence base to April 2026, extending the previous August 2024 search date.
- Made `app/` the canonical static deployment folder.
- Standardised relative paths so the app can run locally, from `/app/`, or from a web-root deployment.
- Updated analysis scripts and references for the current v2.0.0 repository structure.
- Standardised the study-characteristics input filename as `analysis/inputs/Study_Characteristics.csv`.
- Stopped tracking generated files under `analysis/outputs/`; these outputs are regenerated locally from `analysis/scripts/`.

### Added
- Static SCOPE-MOVE Evidence Explorer with Overview, Primary Tool, Model Ranking, Studies, Quality, Instructions, and Data sections.
- Generated `app/data.js` browser bundle built from reproducible extraction inputs and app-specific support files.
- Repository-level changelog for v2.0.0 release preparation.

### Fixed
- Normalised generated confidence intervals in the app data build so interval bounds remain ordered around the estimate and within valid metric limits.
- Updated the study-characteristics UpSet plot call for the installed `UpSetR` argument name.

### Removed
- Removed old Shiny runtime files from the deployable app folder.
- Removed generated analysis outputs from Git tracking while keeping the local regeneration workflow.
