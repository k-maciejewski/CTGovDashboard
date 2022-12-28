# selectors

#### Define date aggregation ####
which_date_agg <- c("month", "quarter", "halfyear")
#which_date_agg <- c("month")

selectorsUI <- function(id){
  selectInput(NS(id,'date_agg_choice'),
              label = 'Choose date aggregation (month, quarter, half-year):',
              choices = which_date_agg,
              selected = "month")
}

selectorsServer <- function(id) {
  
  # df <- reactive({
  #   if(input$date_agg_choice == "quarter"){
  #     date<- c("2017-01-01","2017-01-02","2017-01-03","2017-01-04")
  #     ws<-c(rep(0:3,1))
  #     SiteA <- data.frame(date,ws,stringsAsFactors = FALSE)
  #     x <- SiteA
  #   }
  #   if(input$df == "SiteB"){
  #     date<- c("2017-01-01","2017-01-02","2017-01-03",
  #              "2017-01-04","2017-01-05", "2017-01-06")
  #     ws<-c(rep(0:5,1))
  #     SiteB <- data.frame(date,ws,stringsAsFactors = FALSE)
  #     x <- SiteB
  #   }
  #   return(x)
  # })
  
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
#select_phase <- append(select_phase, "missing")

select_intervention_types <- selectors %>%
  ungroup %>%
  dplyr::distinct(select_intervention_types) %>%
  filter(!is.na(.)) %>%
  arrange(select_intervention_types) %>%
  unlist(., use.names = F)
#select_intervention_types <- append(select_intervention_types, "missing")


select_u_s_fda_regulated_device <- selectors %>%
  ungroup %>%
  dplyr::distinct(select_u_s_fda_regulated_device) %>%
  filter(!is.na(.)) %>%
  unlist(., use.names = F)
#select_u_s_fda_regulated_device <- append(select_u_s_fda_regulated_device, "missing")


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
#select_results_status <- append(select_results_status, "missing")


select_fdaaa_status <- selectors %>%
  ungroup %>%
  #mutate(fdaaa_status = ifelse(is.na(fdaaa_status), "No", "Yes")) %>%
  dplyr::distinct(select_fdaaa_status_bin) %>%
  filter(!is.na(.)) %>% 
  unlist(., use.names = F) # Yes or No
#select_fdaaa_status <- append(select_fdaaa_status, "missing")


select_sponsor <- selectors %>%
  ungroup %>%
  #mutate(sponsor = ifelse(sponsor == "Yale University", "Yes", "No")) %>%
  dplyr::distinct(select_sponsor_bin) %>%
  filter(!is.na(.)) %>% 
  unlist(., use.names = F) # Yes or No
#select_sponsor <- append(select_sponsor, "missing")


select_responsible_party <- selectors %>%
  ungroup %>%
  # mutate(responsible_party =
  #          ifelse(
  #            is.na(responsible_party),
  #            "None",
  #            ifelse(
  #              str_detect(responsible_party, "Principal Investigator"),
  #              "PI",
  #              "Sponsor"
  #            )
  #          )) %>%
  dplyr::distinct(select_responsible_party_bin) %>%
  filter(!is.na(.)) %>%
  unlist(., use.names = F) # Yes or No
select_responsible_party <- append(select_responsible_party, "missing")


select_nih_grants <- selectors %>%
  ungroup %>%
  #mutate(nih_grants = ifelse(is.na(nih_grants), "No", "Yes")) %>%
  dplyr::distinct(select_nih_grants_binary) %>%
  filter(!is.na(.)) %>%
  unlist(., use.names = F) # Yes or No
#select_nih_grants <- append(select_nih_grants, "missing")


which_words <- c("brief_title",
                 "collaborators",
                 "intervention_types",
                 "nih_grants")