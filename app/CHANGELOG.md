# Changelog (SCOPE-MOVE)

This file records **user-visible** changes only (UI, tabs, controls, displayed outputs).  
Internal refactors and maintenance are not listed.

## Release summary

| Version | Date       | Release type | User-visible changes (summary) |
|---|----:|---|--------|
| 1.0.5   | 2026-02-20 | Patch        | Cleaned datasets, added Zenodo DOI. |
| 1.0.4   | 2026-02-20 | Patch        | Added Data Dictionary and harmonised file names. |
| 1.0.3   | 2026-02-15 | Patch        | Replaced the sidebar theme selector with a persistent floating theme toggle and redesigned the Home and Instructions tabs (tiles, breakdown bars, collapsible sections). |
| 1.0.2   | 2026-02-05 | Patch        | Added theme selector (System, Light, Dark). Implemented app-wide dark-mode styling (cards, inputs, dataset table). Improved plot rendering for dark mode (text, hover styling, pooled line contrast) and reduced plot grid clutter. Updated Home page guidance and headline stats. |
| 1.0.1   | 2026-01-21 | Patch        | Added Home tab with summary statistics and resource links. Rebuilt Forest Plot layout with fixed sidebar and scrollable plot region. Added pooled estimate card and pooling method toggle (Simple vs Manuscript). Improved forest plot labels and legend styling. |
| 1.0.0   | Prototype  | Baseline     | Initial prototype with interactive forest plot + filters, pooled estimate line, DOI click-through, dataset table, and instructions page. |

## 1.0.5 (2026-02-20)

### Added
- Zenodo DOI in app

### Changed
- Cleaned datasets to remove inconsistencies

## 1.0.4 (2026-02-20)

### Added
- Data_Dictionary.pdf to repository in /analysis/inputs/

### Changed
- Nothing in the app, app is same as 1.0.3.
- Harmonised model grouping conventions in file names.

## 1.0.3 (2026-02-15)

### Added
- Floating theme toggle button (bottom-right) cycling **System → Light → Dark**, with icon and tooltip.
- Persistent theme preference using browser storage (theme retained across sessions).
- Home tab jump buttons to **Forest Plot**, **Instructions**, and **Dataset**.
- Home tab “At a glance” breakdown showing validation rows by **domain** and **prediction model group** using horizontal stacked bars.

### Changed
- Home tab redesigned to a “hero + tiles + feature cards” layout with improved readability and headline statistics.
- Instructions tab restructured into collapsible sections using progressive disclosure (`details/summary`), with muted-body styling for longer explanatory text.
- Forest Plot pooling method labels updated to use **covariates** wording (simple model described as “no covariates”).

### Fixed
- Theme styling consolidated into a single CSS variable system applied consistently across tabs, including DataTable and modal styling.
- Improved mobile responsiveness for Home and Instructions layout components.

### Known issue
- None observed. If you encounter a theme persistence issue in some browsers, clearing site storage resets the theme to **System**.

## 1.0.2 (2026-02-05)

### Added
- Appearance selector in Forest Plot: **System**, **Light**, **Dark**.
- App-wide theme styling applied to background, cards, text, inputs, and Dataset table.

### Changed
- Forest plot rendering adapted for dark mode (text colour, hover label styling, pooled line contrast).
- Plot styling simplified by removing major x-grid lines.
- Home tab updated copy and headline statistics, including **unique prediction models**.

### Known issue
- In some dark-mode render paths, the forest plot legend title may not fully adopt the dark colour styling (cosmetic only).

## 1.0.1 (2026-01-21)

### Added
- Home tab with:
  - Quick start steps.
  - At-a-glance summary counts.
  - Links to repository and manuscript placeholder, plus version and license display.
- Pooled estimate card displaying:
  - Estimate (95% CI).
  - Participants (summed).
  - Validation rows and study count.
- Pooling method selector:
  - Simple meta-regression (no moderators).
  - Manuscript meta-regression (participant-weighted multilevel model with moderators shown when used).

### Changed
- Forest Plot tab layout rebuilt into a stable two-column view (scrollable sidebar + plot area).
- Forest plot labelling updated (x-axis displayed as percentage; y-axis titled "Model Validations").
- Domain legend presented as “Domains” with fixed palette and ordering.

## 1.0.0 (Prototype baseline)

### Added
- Forest Plot tab:
  - Interactive forest plot with hover metadata per validation.
  - Click-to-open DOI (where available).
  - Filters for metric, domain, model group, age group, and wearable brand.
  - Pooled estimate line shown when N ≥ 3.
  - Horizontal separator indicating validations above vs below pooled estimate.
- Dataset tab:
  - Searchable, filterable study metadata table.
- Instructions tab:
  - Definitions for domains, protocols, environments, validation types, and interpretation guidance.

## Planned updates:
- Inclusion of manuscript quality assessment.
- Inclusion of model availability from manuscript extraction meta-data.
- Inclusion of performance metric class-averaged accuracy from manuscript extraction meta-data.
- Inclusion of additional studies and prediction models subjected to the same or higher standard review methodology compared to the original manuscript (estimated every 2 years)