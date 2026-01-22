# Changelog (SCOPE-MOVE)

This file records **user-visible** changes only (UI, tabs, controls, displayed outputs).  
Internal refactors and maintenance are not listed.

## Release summary

| Version | Date | Release type | User-visible changes (summary) |
|---|---:|---|---|
| 1.0.2 | 2026-01-22 | Patch | Added theme selector (System, Light, Dark). Implemented app-wide dark-mode styling (cards, inputs, dataset table). Improved plot rendering for dark mode (text, hover styling, pooled line contrast) and reduced plot grid clutter. Updated Home page guidance and headline stats. |
| 1.0.1 | 2026-01-21 | Patch | Added Home tab with summary statistics and resource links. Rebuilt Forest Plot layout with fixed sidebar and scrollable plot region. Added pooled estimate card and pooling method toggle (Simple vs Manuscript). Improved forest plot labels and legend styling. |
| 1.0.0 | Prototype | Baseline | Initial prototype with interactive forest plot + filters, pooled estimate line, DOI click-through, dataset table, and instructions page. |

## 1.0.2 (2026-01-22)

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
