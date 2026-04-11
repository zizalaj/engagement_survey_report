########################## Skript na zpracování api dat ########################

library(dplyr)
library(tidyr)
library(purrr)
library(httr)
library(jsonlite)
library(lubridate)
library(stringr)

setwd("C:/Users/JuiceUP/OneDrive - JuiceUP s.r.o/Plocha/Engagement survey/Survey automatizace/API přístup/typeform_API")


token_path <- "token.txt"

##############################################################################
############## Načtení dat

if (!file.exists(token_path)) stop("Token file not found.")
tf_token <- trimws(readLines(token_path, warn = FALSE)[1])
if (nchar(tf_token) == 0) stop("Token file is empty.")


tf_get <- function(page,page_size){
  res <- httr::GET(
    "https://api.typeform.com/forms",
    httr::add_headers(Authorization = paste("Bearer", tf_token)),
    query = list(page = page, page_size = page_size)
  )
  httr::stop_for_status(res)
  
  out <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
  items_df <- out$items
}


forms <- tf_get(1,100)


form <- "P1j6rq1O"


get_form_definition <- function(tf_token, form_id) {
  url <- paste0("https://api.typeform.com/forms/", form_id)
  res <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", tf_token)))
  httr::stop_for_status(res)
  jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

def <- get_form_definition(tf_token,form)

get_responses_page <- function(tf_token, form_id, page_size = 1000, before = NULL) {
  base <- paste0("https://api.typeform.com/forms/", form_id, "/responses")
  query <- list(page_size = page_size)
  if (!is.null(before) && nzchar(before)) query$before <- before
  
  res <- httr::GET(
    base,
    httr::add_headers(Authorization = paste("Bearer", tf_token)),
    query = query
  )
  httr::stop_for_status(res)
  jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}


responses <- get_responses_page(tf_token,form)



# Fetch ALL responses by walking "before" tokens (descending submitted_at order)
get_all_responses <- function(tf_token, form_id, page_size = 1000, max_pages = 200) {
  all_items <- list()
  before <- NULL
  
  for (i in seq_len(max_pages)) {
    page <- get_responses_page(tf_token, form_id, page_size = page_size, before = before)
    items <- page$items %||% list()
    
    if (length(items) == 0) break
    all_items <- c(all_items, items)
    
    # Typeform includes a per-response "token" used for before/after traversal
    last_token <- items[[length(items)]]$token %||% ""
    if (!nzchar(last_token)) break
    
    # Next page: older than the last token
    before <- last_token
    
    # If we got less than page_size, we're done
    if (length(items) < page_size) break
  }
  
  all_items
}

all_responses <- get_all_responses(tf_token,form)





##################################################################
##### celkový loop - zpracování odpovědí


odpoved_klasif <- function(o){
  if(o[["field"]][["type"]] == 'multiple_choice'){
    return(as.character(o[["choice"]][["label"]]))
  }
  else if(o[["field"]][["type"]] == 'long_text'){
    return(as.character(o[["text"]]))
  }
  else{
    return(as.character(o[["number"]]))
  }
}


###### 
###### tvorba listu + long dataframe

preprocess_data <- function(ls){
  
  processed_list <- list()
  
  ### loop přes respondenty
  for(i in seq_along(ls)){
    
    
    respondent <- ls[[i]]
    
    resp_data <- list(
      respondent_id  = respondent[["response_id"]],
      landed_at    = respondent[["landed_at"]],
      submitted_at = respondent[["submitted_at"]]
    )
    
    respondent_x <- list()
    ### loop přes odpovědi uvnitř respondenta
    for(j in seq_along(respondent[["answers"]])){
      
      odp <- respondent[["answers"]][[j]]
      
      odpoved <- list(
        respondent_id = resp_data$respondent_id,
        landed_at = resp_data$landed_at,
        submitted_at = resp_data$submitted_at,
        typ_otazky = odp[["field"]][["type"]],
        otazka_ref = odp[["field"]][["ref"]],
        odpoved_hodnota = odpoved_klasif(odp)
      )
      
      respondent_x[[j]] <- odpoved
      
    }
    
    processed_list[[i]] <- respondent_x
  }
  
  return(processed_list)
}



survey_data_preprocessed <- preprocess_data(all_responses)


survey_df_long <- dplyr::bind_rows(unlist(survey_data_preprocessed, recursive = FALSE))



#######
###### číselník skupin


add_group_title <- function(f){
  if(f[["type"]] == 'group'){
    return(as.character(f[["title"]]))
  }
  else{
    return(NA_character_)
  }
}

add_group_ref <- function(f){
  if(f[["type"]] == 'group'){
    return(as.character(f[["ref"]]))
  }
}


add_q_title <- function(f){
  
  if(f[["type"]] == 'group'){
    return(as.character(f[["ref"]]))
  }
  
}


process_survey_ciselnik <- function(ls){
  
  k <- 1
  
  survey_list <- list()
  
  for(i in seq_along(ls[["fields"]])){
    
    field <- ls[["fields"]][[i]]
    
    if(field[["type"]] == 'group'){
      
      group_data <- list(
        group_title = add_group_title(field),
        group_ref = add_group_ref(field)
      )
      
      
      for(j in seq_along(field[["properties"]][["fields"]])){
        
        ot <- field[["properties"]][["fields"]][[j]]
        
        survey_list[[k]] <- list(
          
          group_title = group_data$group_title,
          group_ref = group_data$group_ref,
          otazka_title = ot[["title"]],
          otazka_ref = ot[["ref"]]
        )
        k <- k+1
      }
    }
    
    else if(field[["type"]] == 'statement') next
    
    else{
      
      survey_list[[k]] <- list(
        group_title = NA_character_,
        group_ref = NA_character_,
        otazka_title = field[["title"]],
        otazka_ref = field[["ref"]]
      )
      
      k <- k+1
    }
    
  }
  return(survey_list)
  
}

ciselnik_list <- process_survey_ciselnik(def)

ciselnik_df <- dplyr::bind_rows(ciselnik_list)


final_long_df <- survey_df_long %>%
  left_join(ciselnik_df, by = "otazka_ref")



######### další zpracování - specifické pro engagement survey

multiple_choice_extra <- final_long_df %>% 
  filter(typ_otazky == "multiple_choice") %>%
  summarise(n = n_distinct(otazka_ref))


open <- final_long_df %>%
  filter(typ_otazky == "rating") %>%
  summarise(n = n_distinct(otazka_ref))

### identifikace sekcí helpers


include_multiple_choice <- function(df){
  
  multiple_choice_extra <- df %>% 
    filter(typ_otazky == "multiple_choice") %>%
    summarise(n = n_distinct(otazka_ref))
  
  if(multiple_choice_extra$n > 3){
    return(TRUE)
  }
  else{return(FALSE)}
  
}

mc_present <- include_multiple_choice(final_long_df)


include_openended <- function(df){
  openend <- df %>%
    filter(typ_otazky == "long_text") %>%
    summarise(n = n_distinct(otazka_ref))
  
  if(openend$n > 0){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}


openend_present <- include_openended(final_long_df)

###################################
######### zpracování odpovědí


##### demografie

zakl_info <- final_long_df %>%
  filter(group_title == "Základní info" & typ_otazky == "multiple_choice")%>%
  dplyr::select(odpoved_hodnota,otazka_title)

zakl_info_otazky <- unique(zakl_info$otazka_title)

## průměrná doba vyplnění

doba_vyplneni <- final_long_df %>%
  dplyr::select(respondent_id,landed_at,submitted_at)%>%
  group_by(respondent_id) %>%
  mutate(
    landed_at = ymd_hms(landed_at),
    submitted_at = ymd_hms(submitted_at),
    cas_vyplneni = submitted_at - landed_at
  )%>%
  distinct(respondent_id,cas_vyplneni)

avg_doba_vyplneni <- mean(doba_vyplneni$cas_vyplneni)
avg_doba_vyplneni <- round(as.numeric(avg_doba_vyplneni, units = "mins"),0)


#### počet respondentů

pocet_resp <- length(unique(final_long_df$respondent_id))


#### návratnost 

## respondenti per oddělení



oddeleni_expr <- "jakém"

oddeleni_index <- str_detect(zakl_info_otazky,oddeleni_expr)

oddeleni_ot <- zakl_info_otazky[oddeleni_index]

oddeleni <- zakl_info %>%
  filter(otazka_title == oddeleni_ot)%>%
  group_by(odpoved_hodnota) %>%
  summarise(pocet_respondentu = n())%>%
  mutate(pocet_zamestnancu = 0)


odd_souhrn_long <- oddeleni %>%
  pivot_longer(cols= 2:3, names_to = "Kategorie", values_to = "Pocet")


odd_souhrn_long$oddeleni_trunc <- str_trunc(odd_souhrn_long$odpoved_hodnota, 
                                            width = 25, ellipsis = "...")




