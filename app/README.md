# SCOPE-MOVE Application

This folder contains the canonical SCOPE-MOVE v2.0.0 application.

SCOPE-MOVE v2.0.0 is a web-based HTML, CSS, and JavaScript application. It replaces the earlier Shiny prototype with a self-contained browser deployment that can be hosted from this folder without a server-side runtime.

The v2.0.0 evidence base updates the review coverage to April 2026, extending the previous search date of August 2024.

## Runtime Files

- `index.html`: application entry point.
- `app.js`: filtering, navigation, pooled-line, ranking, table, and chart logic.
- `styles.css`: visual system and responsive layout rules.
- `data.js`: generated data bundle used by the browser.
- `data/`: app-specific source files read by `build-data.js`.

## Launch

Open `index.html` directly, or serve this folder from any static web server.

The app uses relative paths, so it works when hosted from:

- `/app/` inside the GitHub repository.
- A web-root deployment such as `https://scope-move.miltheo.com`.

## Rebuild Data

From the repository root:

```bash
node app/build-data.js
```

The build reads reproducible source inputs from `analysis/inputs/` and app-specific support files from `app/data/`, then writes `app/data.js`.

The app data bundle is independent of the generated manuscript-facing files under `analysis/outputs/`.

## Deployment Note

For static hosting, deploy the public runtime files in this folder together: `index.html`, `app.js`, `styles.css`, `data.js`, and the `data/` folder. Branding assets and favicons are intentionally ignored by Git and should only be added to a deployment outside the public repository if needed.
