# Navratnost Editor

Shiny aplikace pro API report s rucnim doplnenim:

- celkoveho poctu zamestnancu
- poctu zamestnancu v jednotlivych oddelenich

Appka nacita dostupne formulare z Typeform API, pro vybranou formu pripravi tabulku oddeleni a po kliknuti na render vyrenderuje:

- `report_editable_api.qmd`

do PDF reportu, ktere je mozne stahnout primo z aplikace.

## Spusteni

V R konzoli z korene projektu:

```r
shiny::runApp("codex/navratnost-editor")
```

## Poznamky

- Rucne zadane hodnoty jsou pouze pro aktualni otevrene sezeni.
- Po zmene formulare se tabulka inicializuje znovu z aktualnich API dat.
- Aplikace renderuje HTML mezikrok a nasledne jej tiskne do PDF pres `pagedown::chrome_print()`.
- Report se renderuje s Quarto parametry:
  - `form_id`
  - `demografie`
  - `celkem_zamestnancu`
  - `oddeleni_manual`
  - `report_type`
  - `oddeleni_report`

## Deployment secrets

Pro lokalni vyvoj je podporovano bud projektove `.Renviron`, nebo lokalni `token.txt`.

Priklad `.Renviron`:

```sh
TYPEFORM_TOKEN=your_token_here
```

- `.Renviron` a `token.txt` nesmi byt commitnuty do Gitu.
- V Posit Connect Cloud nastavte `TYPEFORM_TOKEN` jako secret nebo environment variable.
- Pro PDF tisk musi byt v cilovem prostredi k dispozici Chrome nebo Chromium kompatibilni s `pagedown`.
- Vyrenderovane PDF reporty jsou v aplikaci ke stazeni pres tlacitko `Stahnout report`.
