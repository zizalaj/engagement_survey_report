# Style Editor Prototype

This folder contains a minimal Shiny prototype for visual tuning of the survey report.

## What it does

- previews the real `intro-slide` and `agenda-slide` structure
- reads the current base and AI CSS files
- generates a separate overrides stylesheet instead of editing the source CSS

## Files

- `app.R` = Shiny app
- `generated/style-overrides.css` = generated overrides file
- `notes/project-note-2026-03-14.md` = render chain and style ownership note

## Run

```r
shiny::runApp("codex/style-editor-prototype")
```

## Current scope

The prototype exposes:

- core report color tokens
- a few intro slide typography/layout controls
- a few agenda slide typography/layout controls

It is intentionally small so the workflow stays stable while you validate the tuning approach.
