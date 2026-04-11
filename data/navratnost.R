### návratnost dle oddělení

options(repos = c(CRAN = "https://cloud.r-project.org"))

# Check and install the 'here' package if it is not already installed
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}

if (!requireNamespace("dpylr", quietly = TRUE)) {
  install.packages("dplyr")
}

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
}

if (!requireNamespace("stringr", quietly = TRUE)) {
  install.packages("stringr")
}


library(here)
library(dplyr)
library(openxlsx)
library(stringr)

data_path <- here("data","aktualni_pruzkum.xlsx")

aktualni_pruzkum <- read.xlsx(data_path)

otazky_aktualni <- names(aktualni_pruzkum)

oddeleni_expr <- "jakém"

oddeleni_index <- str_detect(otazky_aktualni,oddeleni_expr)

oddeleni_ot <- otazky_aktualni[oddeleni_index]

oddeleni <- aktualni_pruzkum[,oddeleni_ot, drop = F]

names(oddeleni) <- "oddeleni"

odd_souhrn <- oddeleni %>%
  group_by(oddeleni) %>%
  summarise(pocet_respondentu = n())%>%
  mutate(pocet_zamestnancu = 0)

wb = createWorkbook()
zalozka = addWorksheet(wb,"Návratnost průzkumu")
writeData(wb, sheet = zalozka, x = odd_souhrn)

adresa_vystup <- here("Návratnost průzkumu.xlsx")
saveWorkbook(wb,adresa_vystup, overwrite = T)
