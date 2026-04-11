############# Kód pro výpočty do engagement survey ###########################

instal_funkce <- function(balicek){
  if (!requireNamespace(balicek, quietly = T)){
    install.packages(balicek)
    library(balicek)
  }
  else{
    library(balicek, character.only = T)
  }
}

instal_funkce("here")
instal_funkce("dplyr")
instal_funkce("tidyr")
instal_funkce("readxl")
instal_funkce("extrafont")
instal_funkce("showtext")
instal_funkce("stringr")
instal_funkce("openxlsx")
instal_funkce("ggplot2")

font_add("Europa", regular = here("fonts", "EuropaGroNr2JU Regular.otf"),
         bold = here("fonts", "EuropaGroNr2JU Bold.ttf"))
showtext_auto()

aktualni_pruzkum <- read_excel(here("data","aktualni_pruzkum.xlsx"))
#rok2 <- read_excel("Rok2.xlsx")


otazky_aktualni <- names(aktualni_pruzkum)
#otazky_minule <- names(rok1)


######################## sekce průzkum v číslech #########################################################################################


################### základní informace


### průměrná délka vyplnění

cas_znacky <- c("Start Date (UTC)", "Submit Date (UTC)")

cas_sloupce <- cas_znacky[cas_znacky %in% otazky_aktualni]

casy <- aktualni_pruzkum[,cas_sloupce]

casy <- casy %>%
  mutate(
    `Start Date (UTC)` = as.POSIXlt(`Start Date (UTC)`),
    `Submit Date (UTC)` = as.POSIXlt(`Submit Date (UTC)`)
  )

casy$diff <- as.numeric(difftime(casy$`Submit Date (UTC)`,casy$`Start Date (UTC)`))

prum_cas_vyplneni_sec <- mean(casy$diff)

prum_cas_vyplneni_min <- round(prum_cas_vyplneni_sec/60,0)




### pocet respondentu

pocet_resp <- nrow(aktualni_pruzkum)



################### návratnost

### návratnost dle oddělení

oddeleni_expr <- "jakém"

oddeleni_index <- str_detect(otazky_aktualni,oddeleni_expr)

oddeleni_ot <- otazky_aktualni[oddeleni_index]

oddeleni <- aktualni_pruzkum[,oddeleni_ot]

names(oddeleni) <- "oddeleni"

odd_souhrn <- oddeleni %>%
  group_by(oddeleni) %>%
  summarise(pocet_respondentu = n())%>%
  mutate(pocet_zamestnancu = 0)

wb = createWorkbook()
zalozka = addWorksheet(wb,"Návratnost průzkumu")
writeData(wb, sheet = zalozka, x = odd_souhrn)
saveWorkbook(wb,"Návratnost průzkumu.xlsx", overwrite = T)


odd_souhrn_vyplnene <- read_excel("Návratnost průzkumu vyplněné.xlsx", sheet = "Návratnost průzkumu")

odd_souhrn_long <- odd_souhrn_vyplnene %>%
  pivot_longer(cols= 2:3, names_to = "Kategorie", values_to = "Pocet")


odd_souhrn_long$oddeleni_trunc <- str_trunc(odd_souhrn_long$oddeleni, width = 25, ellipsis = "...")



ggplot(odd_souhrn_long,aes(x = oddeleni_trunc, y = Pocet, fill = Kategorie))+
  geom_col(width = .5, position = "dodge")+
  geom_text(aes(label = Pocet), 
            position = position_dodge(width = .5),
            family = "Europa",
            fontface = "plain", size = 5,
            vjust = -.5)+
  theme(
    axis.text.x = element_text(angle = 15),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.background = element_blank()
  )+
  scale_fill_manual(values = c("pocet_respondentu" = "#63E8C6","pocet_zamestnancu" = "#DBDBDB"),
                    labels = c("Počet respondentů", "Počet zaměstnanců"))


### celková návratnost
resp_celkem <- sum(odd_souhrn_vyplnene[,2])
zam_celkem <- sum(odd_souhrn_vyplnene[,3])

navratnost <- round(resp_celkem/zam_celkem*100,0)


############### demografie

### pozice

pozice_expr <- "jaké\\s"

pozice_index <- str_detect(otazky_aktualni, pozice_expr)

pozice_ot <- otazky_aktualni[pozice_index]

pozice <- aktualni_pruzkum[,pozice_ot]

names(pozice) <- "pozice"

pozice_souhrn <- pozice %>%
  group_by(pozice) %>%
  summarise(pocet = n())


ggplot(pozice_souhrn, aes(pozice, pocet))+
  geom_col(width = .5, fill = "#63E8C6", position = "dodge")+
  geom_text(aes(label = pocet), 
            family = "Europa Bold", size = 6, hjust = -.3)+
  coord_flip()+
  theme(
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
    
  )


### doba ve firmě

doba_expr <- "dlouho"

doba_index <- str_detect(otazky_aktualni, doba_expr)

doba_ot <- otazky_aktualni[doba_index]

doba <- aktualni_pruzkum[,doba_ot]

names(doba) <- "doba"

az_expr <- "\\d+(?= až{1})"

do_expr <- "do\\s\\d{1}"

vice_expr <- "více"

doba_souhrn <- doba %>%
  group_by(doba) %>%
  summarise(pocet = n()) %>%
  mutate(az = case_when(
    str_detect(doba,do_expr) == T ~ 0,
    str_detect(doba,vice_expr) == T ~ 100,
    .default = as.integer(str_extract(doba,az_expr))
  )) %>%
  arrange(az) %>%
  mutate(doba = factor(doba, levels = doba))


#musí se vymyslet, jak ty odpovědi řadit!!
# udělat si variables jako do xx, xx a více a leading digit.
# na základě toho udělat ranking

ggplot(doba_souhrn, aes(doba, pocet))+
  geom_col(width = .5, fill = "#FFD06B", position = "dodge")+
  geom_text(aes(label = pocet), 
            family = "Europa", fontface = "bold", size = 6, hjust = -.3)+
  coord_flip()+
  theme(
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
    
  )


#### věk

vek_expr <- "let\\?"

vek_index <- str_detect(otazky_aktualni, vek_expr)

vek_ot <- otazky_aktualni[vek_index]

vek <- aktualni_pruzkum[,vek_ot]

names(vek) <- "vek"

vek_souhrn <- vek %>%
  count(vek) %>%
  rename(pocet = "n")

ggplot(vek_souhrn, aes(x = vek, y = pocet))+
  geom_col(fill = "#63E8C6", width = .7, position = "dodge")+
  geom_text(aes(label = pocet), 
            family = "Europa", fontface = "bold", 
            size = 5, vjust = -.3)+
  theme(
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
###############################################################################




################### engagement #########################################################################################################



engot_r1 <- rok1[,5:8]
engot_r2 <- rok2[,5:8]

## definování otázek

eng_expr <- "náplní své|baví|smysl|hrdý"

eng_index <- str_detect(otazky_aktualni, eng_expr)

engagement <- aktualni_pruzkum[eng_index]


engot_cat1 <- engot_r1 %>%
  mutate(across(everything(),
                ~ case_when(
                  .x == 1 ~ "Úplný engagement",
                  .x %in% 2:3 ~ "Částečný engagement",
                  .x %in% 4:5 ~ "Slabý engagement"
                )))


engot_cat2 <- engot_r2 %>%
  mutate(across(everything(),
                ~ case_when(
                  .x == 1 ~ "Úplný engagement",
                  .x %in% 2:3 ~ "Částečný engagement",
                  .x %in% 4:5 ~ "Slabý engagement"
                )))


eng1 <- engot_cat1 %>%
  pivot_longer(
    cols = everything(),         # take all columns (each question)
    names_to = "otazka",       # new column with question names
    values_to = "engagement"       # new column with engagement category
  ) %>%
  count(otazka, engagement, name = "n") %>% 
  group_by(otazka)%>%
  mutate(pct = round((n/sum(n))*100,1),
         label_y = cumsum(pct) - .5*pct,
         engagement = factor(engagement, levels = c("Slabý engagement", "Částečný engagement", "Úplný engagement")))%>%
  ungroup()%>%
  mutate(rok = "rok1",
         rok = factor(rok)) %>%
  arrange(otazka, engagement) 



eng2 <- engot_cat2 %>%
  pivot_longer(
    cols = everything(),         # take all columns (each question)
    names_to = "otazka",       # new column with question names
    values_to = "engagement"       # new column with engagement category
  ) %>%
  count(otazka, engagement, name = "n") %>% 
  group_by(otazka)%>%
  mutate(pct = round((n/sum(n))*100,1),
         label_y = cumsum(pct) - .5*pct,
         engagement = factor(engagement, levels = c("Slabý engagement", "Částečný engagement", "Úplný engagement")))%>%
  mutate(rok = "rok2",
         rok = factor(rok)) %>%
  arrange(otazka, engagement) 



eng_srovnani <- bind_rows(eng1,eng2)


ggplot(eng_srovnani, aes(rok, pct, fill = engagement))+
  geom_col(position = position_stack())+
  geom_text(aes(label = paste(pct,"%")),
            position = position_stack(vjust = 0.5),
            family = "Europa Bold", size = 4)+
  facet_wrap(~otazka, nrow = 4)+
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")+
  coord_flip()





################ drivery engagementu ###########################################

## Já a tým

# definice okruhu
tym_expr <- "důvěřujeme|problémy tak|o chybách|mi jasné|svými nápady|nedaří"

tym_index <- str_detect(otazky_aktualni,tym_expr)

tym <- aktualni_pruzkum[tym_index]



## Já a vdeoucí

# definice okruhu
vedouci_expr <- "zadává úkoly|pochválí za|zajímá o mé|pravidelnou zpětnou|vedoucí podporuje|chová spravedlivě"

vedouci_index <- str_detect(otazky_aktualni,vedouci_expr)

vedouci <- aktualni_pruzkum[vedouci_index]


## Já a firma

# definice okruhu
firma_expr <- "cílům a úspěchu|(T|t)op management|změnách ve firmě|mými osobními|(R|r)ozumím firemním"

firma_index <- str_detect(otazky_aktualni,firma_expr)

firma <- aktualni_pruzkum[firma_index]



## Já a výkon

# definice okruhu
vykon_expr <- "práci dobře|být efektivní|ohodnocení skrze|podporu od oddělení|pracovní standard"

vykon_index <- str_detect(otazky_aktualni,vykon_expr)

vykon <- aktualni_pruzkum[vykon_index]


## Já a rozvoj

# definice okruhu
rozvoj_expr <- "profesnímu růstu|možnost kariérního|profesního postupu|dostávám uznání|sdílejí zkušenosti"

rozvoj_index <- str_detect(otazky_aktualni,rozvoj_expr)

rozvoj <- aktualni_pruzkum[rozvoj_index]



###################### Multiple choice otázky ##################################

multiple_choice_expr <- "\."

rozvoj_index <- str_detect(otazky_aktualni,multiple_choice_expr)


