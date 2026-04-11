# Navratnost Editor

Shiny aplikace pro API report s rucnim doplnenim:

- celkoveho poctu zamestnancu
- poctu zamestnancu v jednotlivych oddelenich

Appka nacita dostupne formulare z Typeform API, pro vybranou formu pripravi tabulku oddeleni a po kliknuti na render vyrenderuje:

- `report_editable_api_copy.qmd`

do HTML nahledu v prohlizeci.

## Spusteni

V R konzoli z korene projektu:

```r
shiny::runApp("codex/navratnost-editor")
```

## Poznamky

- Rucne zadane hodnoty jsou pouze pro aktualni otevrene sezeni.
- Po zmene formulare se tabulka inicializuje znovu z aktualnich API dat.
- Report se renderuje s Quarto parametry:
  - `form_id`
  - `celkem_zamestnancu`
  - `oddeleni_manual`
  - `token_path`
