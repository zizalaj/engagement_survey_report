# Render Chain Note

Date: 2026-03-14

## Current render chain

The report currently renders from [report.qmd](C:\Users\JuiceUP\OneDrive - JuiceUP s.r.o\Plocha\Engagement survey\Survey automatizace\Codex\automatizace AI\report.qmd), which combines:

- Quarto YAML
- slide markup written in HTML
- inline R chunks that load Excel data, compute metrics, and generate plots

The YAML currently targets paged HTML, not PDF directly:

- `format: html`
- `pagedjs: true`
- `self-contained: true`
- `embed-resources: true`
- `page-layout: full`
- `output-file: report_test.html`

Active stylesheets in the YAML:

- [styles-base.css](C:\Users\JuiceUP\OneDrive - JuiceUP s.r.o\Plocha\Engagement survey\Survey automatizace\Codex\automatizace AI\styles-base.css)
- [styles-ai.css](C:\Users\JuiceUP\OneDrive - JuiceUP s.r.o\Plocha\Engagement survey\Survey automatizace\Codex\automatizace AI\styles-ai.css)

The render sequence is therefore:

1. Quarto renders `report.qmd` to paged HTML.
2. Paged.js applies the fixed 1920x1080 page model declared in CSS.
3. A browser print/export step produces the PDF artifact.

## Current vs stale outputs

Timestamp check:

- `report.qmd`: 2026-03-03 13:49:43
- `report.html`: 2026-03-03 13:54:38
- `pdf_output/report_print.pdf`: 2026-03-03 13:54:41
- `styles-ai.css`: 2026-03-03 13:56:28
- `styles-base.css`: 2026-02-24 16:06:12
- `styles.css`: 2025-12-09 10:02:45

Assessment:

- [report.html](C:\Users\JuiceUP\OneDrive - JuiceUP s.r.o\Plocha\Engagement survey\Survey automatizace\Codex\automatizace AI\report.html) is probably a current render artifact from the same working session as `report.qmd`.
- [pdf_output/report_print.pdf](C:\Users\JuiceUP\OneDrive - JuiceUP s.r.o\Plocha\Engagement survey\Survey automatizace\Codex\automatizace AI\pdf_output\report_print.pdf) is probably the latest printable output from that same render/export cycle.
- The `output-file` setting points to `report_test.html`, but that file is not present in the workspace. That means either:
  - Quarto was rendered previously with different YAML, or
  - the final HTML was renamed or exported by another step.
- [styles.css](C:\Users\JuiceUP\OneDrive - JuiceUP s.r.o\Plocha\Engagement survey\Survey automatizace\Codex\automatizace AI\styles.css) appears stale and inactive, because it is not referenced by the current YAML and is much older than the active files.
- The old `include` artifact in `report.qmd` should be treated as historical only.

## Slide class map

Slide classes found in `report.qmd`:

- `intro-slide`
- `agenda-slide`
- `section-slide`
- `survey-numbers-slide`
- `metrics-slide`
- `jakcist-slide jakcist--engindex`
- `jakcist-slide jakcist--individual`
- `engindex-slide`
- `engindex-firma-ref`
- `engindex-slide indeng-slide`
- `engindex-slide engindex-oddeleni-slide`
- `sledovane-drivery slide`
- `prezentace-driveru slide`
- `jakcist-dr-bench slide`
- `jakcist-dr-celkem slide`
- `jakcist-dr-detail slide`
- `vysledky-dr-bench slide`
- `vysledky-dr-celkem slide`

Ownership by stylesheet:

Base layouts in [styles-base.css](C:\Users\JuiceUP\OneDrive - JuiceUP s.r.o\Plocha\Engagement survey\Survey automatizace\Codex\automatizace AI\styles-base.css):

- global `.slide`
- `intro-slide`
- `agenda-slide`
- `section-slide`
- `survey-numbers-slide`
- `metrics-slide`
- `jakcist-slide` variants
- `engindex-slide`
- `engindex-firma-ref`
- `engindex-oddeleni-slide`

AI-generated / later layouts in [styles-ai.css](C:\Users\JuiceUP\OneDrive - JuiceUP s.r.o\Plocha\Engagement survey\Survey automatizace\Codex\automatizace AI\styles-ai.css):

- `sledovane-drivery`
- `prezentace-driveru`
- `jakcist-dr-bench`
- `jakcist-dr-celkem`
- `jakcist-dr-detail`
- `vysledky-dr-bench`
- `vysledky-dr-celkem`
- `vysledky-dr-detail`

Practical interpretation:

- `styles-base.css` owns the shared tokens and the first, more stable part of the deck.
- `styles-ai.css` owns the newer driver/result slides and depends heavily on the variables defined in `styles-base.css`.

## First parameters worth exposing in Shiny

These are the first 18 parameters most likely to speed up visual fine-tuning without turning the app into a full CSS editor.

Global tokens:

1. `accent`
2. `accent_light`
3. `accent_pink`
4. `accent_pink_strong`
5. `yellow`
6. `yellow_light`
7. `light_grey`
8. `normal_grey`
9. `page_bg`
10. `black`

Intro slide:

11. `intro_main_size`
12. `intro_subtitle_size`
13. `intro_badge_size`
14. `intro_logo_left`
15. `intro_logo_bottom`

Agenda slide:

16. `agenda_padding_y`
17. `agenda_padding_x`
18. `agenda_title_size`
19. `agenda_list_gap`
20. `agenda_item_gap`

## Smallest viable editor architecture

The prototype should not rewrite the source CSS files. Instead it should:

1. read the existing `styles-base.css` and `styles-ai.css`
2. let the user adjust a small parameter set
3. generate a separate overrides file
4. preview only one or two slides first
5. keep the generated CSS usable by the full Quarto render later

That is the architecture implemented in:

- [codex/style-editor-prototype/app.R](C:\Users\JuiceUP\OneDrive - JuiceUP s.r.o\Plocha\Engagement survey\Survey automatizace\Codex\automatizace AI\codex\style-editor-prototype\app.R)
- [codex/style-editor-prototype/generated/style-overrides.css](C:\Users\JuiceUP\OneDrive - JuiceUP s.r.o\Plocha\Engagement survey\Survey automatizace\Codex\automatizace AI\codex\style-editor-prototype\generated\style-overrides.css)
