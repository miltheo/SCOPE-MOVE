# Changelog (SCOPE-MOVE)

This file records user-visible application changes only. Repository maintenance and analysis workflow changes are tracked in the root `CHANGELOG.md`.

## Release summary

| Version | Date | Release type | User-visible changes (summary) |
|---|---:|---|---|
| 2.0.0 | 2026-05-14 | Major | Rebuilt as a web-based evidence explorer and updated the review evidence base through April 2026. |
| 1.0.5 | 2026-02-20 | Patch | Cleaned datasets and added Zenodo DOI. |
| 1.0.4 | 2026-02-20 | Patch | Added Data Dictionary and harmonised file names. |
| 1.0.3 | 2026-02-15 | Patch | Replaced the sidebar theme selector with a persistent floating theme toggle and redesigned the Home and Instructions tabs. |
| 1.0.2 | 2026-02-05 | Patch | Added theme selector, app-wide dark-mode styling, plot rendering improvements, and updated Home guidance. |
| 1.0.1 | 2026-01-21 | Patch | Added Home tab, pooled estimate card, pooling method toggle, and rebuilt Forest Plot layout. |
| 1.0.0 | Prototype | Baseline | Initial prototype with interactive forest plot, pooled estimate line, DOI click-through, dataset table, and instructions page. |

## 2.0.0 (2026-05-14)

### Changed
- Rebuilt the application as a web-based HTML, CSS, and JavaScript dashboard, replacing the earlier Shiny interface.
- Updated the displayed evidence base through April 2026, extending the previous August 2024 search date.
- Consolidated runtime files in `app/` for static hosting and manual deployment.
- Updated app paths and asset references to support local use, repository hosting, and web-root deployment.
- Refined laptop-scale layout behaviour for the Primary Tool forest view and compact landscape panels.

### Added
- Overview-first navigation with purpose text, use-case routes, and evidence-density matrix.
- Primary Tool with metric filtering, pooled reference lines, above-pooled highlighting, external-validation markers, DOI-linked rows, selected-row table, and responsive forest plot refresh.
- Model Ranking tab with an exploratory shortlist of externally validated model identities and expandable evidence details.
- Studies tab with country, device, participant, and compact prediction model type landscape summaries.
- Quality tab with study-level QUADAS-2 summaries and concise signalling-question text.
- Instructions tab with app-use guidance, definitions, source-link guidance, and interpretation notes.
- Data tab for inspecting bundled source and derived tables.

## 1.0.5 (2026-02-20)

### Added
- Zenodo DOI in app.

### Changed
- Cleaned datasets to remove inconsistencies.

## 1.0.4 (2026-02-20)

### Added
- Data Dictionary to the repository analysis inputs.

### Changed
- Harmonised model grouping conventions in file names.

## 1.0.3 (2026-02-15)

### Added
- Floating theme toggle cycling System, Light, and Dark modes.
- Persistent theme preference using browser storage.
- Home tab jump buttons to Forest Plot, Instructions, and Dataset.
- Home tab breakdown of validation rows by domain and prediction model group.

### Changed
- Redesigned the Home tab with summary tiles and feature cards.
- Restructured the Instructions tab into collapsible sections.
- Updated Forest Plot pooling method labels to use covariates wording.

### Fixed
- Consolidated theme styling across tabs, including DataTable and modal styling.
- Improved mobile responsiveness for Home and Instructions layout components.

## 1.0.2 (2026-02-05)

### Added
- Appearance selector for System, Light, and Dark modes.
- App-wide theme styling for background, cards, text, inputs, and Dataset table.

### Changed
- Adapted forest plot rendering for dark mode.
- Simplified plot styling by reducing grid clutter.
- Updated Home page guidance and headline statistics.

## 1.0.1 (2026-01-21)

### Added
- Home tab with quick start steps, summary counts, resource links, version, and license display.
- Pooled estimate card showing estimate, 95% CI, participants, validation rows, and study count.
- Pooling method selector for simple and manuscript meta-regression views.

### Changed
- Rebuilt Forest Plot tab into a stable two-column layout.
- Updated forest plot axis labelling and domain legend presentation.

## 1.0.0 (Prototype baseline)

### Added
- Interactive forest plot with hover metadata, DOI click-through, filters, pooled estimate line, and above/below pooled separator.
- Searchable and filterable study metadata table.
- Instructions page with definitions and interpretation guidance.
