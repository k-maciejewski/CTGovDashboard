# selectors

#### Define date aggregation ####
which_date_agg <- c("month") #, "quarter", "halfyear")

selectorsUI <- function(id){
  selectInput(NS(id,'date_agg_choice'),
              label = 'Date aggregation (month)', #, quarter, half-year):',
              choices = which_date_agg,
              selected = "month")
}

selectorsServer <- function(id) {
  
  moduleServer(id, 
               function(input, output, session){
                 reactive(input$date_agg_choice)
               })
}


# load selector csv
selectors <- vroom::vroom(here::here("dashboard_selectors2.csv"))

#### selectors ####
select_study_type <- selectors %>%
  ungroup %>%
  dplyr::distinct(select_study_type) %>%
  filter(!is.na(.)) %>%
  arrange(select_study_type) %>%
  unlist(., use.names = F)
select_study_type <- append(select_study_type, "missing")

select_phase <- selectors %>%
  ungroup %>%
  dplyr::distinct(select_phase) %>%
  filter(!is.na(.)) %>%
  arrange(select_phase) %>%
  unlist(., use.names = F)

select_intervention_types <- selectors %>%
  ungroup %>%
  dplyr::distinct(select_intervention_types) %>%
  filter(!is.na(.)) %>%
  arrange(select_intervention_types) %>%
  unlist(., use.names = F)


select_u_s_fda_regulated_device <- selectors %>%
  ungroup %>%
  dplyr::distinct(select_u_s_fda_regulated_device) %>%
  filter(!is.na(.)) %>%
  unlist(., use.names = F)


select_record_status <- selectors %>%
  ungroup %>%
  dplyr::distinct(select_record_status) %>%
  filter(!is.na(.)) %>% 
  arrange(select_record_status) %>%
  unlist(., use.names = F)
select_record_status <- append(select_record_status, "missing")


select_results_status <- selectors %>%
  ungroup %>%
  dplyr::distinct(select_results_status) %>%  
  filter(!is.na(.)) %>% 
  arrange(select_results_status) %>%
  unlist(., use.names = F)


select_fdaaa_status <- selectors %>%
  ungroup %>%
  dplyr::distinct(select_fdaaa_status_bin) %>%
  filter(!is.na(.)) %>% 
  unlist(., use.names = F) # Yes or No


select_sponsor <- selectors %>%
  ungroup %>%
  dplyr::distinct(select_sponsor_bin) %>%
  filter(!is.na(.)) %>% 
  unlist(., use.names = F) # Yes or No


select_responsible_party <- selectors %>%
  ungroup %>%
  dplyr::distinct(select_responsible_party_bin) %>%
  filter(!is.na(.)) %>%
  unlist(., use.names = F) # Yes or No
select_responsible_party <- append(select_responsible_party, "missing")


select_nih_grants <- selectors %>%
  ungroup %>%
  dplyr::distinct(select_nih_grants_binary) %>%
  filter(!is.na(.)) %>%
  unlist(., use.names = F) # Yes or No


which_words <- c("brief_title",
                 "collaborators",
                 "intervention_types",
                 "nih_grants")