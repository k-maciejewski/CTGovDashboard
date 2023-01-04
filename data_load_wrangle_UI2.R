#### Data load UI and server ####

dataUI <- function(id){
  tagList(
    useShinyFeedback(),
    (fileInput(NS(id,"record_information"),
              label = "Record Information csv file",
              accept = c(".csv"), 
    )),
    fileInput(NS(id,"review_history"),
              label = "Review History csv file",
              accept = c(".csv")
              
    ),
    #textOutput("data")
  )
}

#### reactive create final ds ####

dataServer <- function(id){        
  moduleServer(id, 
               function(input, output, session){
                 
                 final_2008_all <- reactive({
                   
                   req(input$record_information,
                       input$review_history)

                   #### load data ####
                   Review_History <- vroom::vroom(here::here(input$review_history$datapath)) %>%
                     janitor::clean_names()
                   
                   Record_Information_Download <- vroom::vroom(here::here(input$record_information$datapath)) %>%
                     janitor::clean_names() 
                   
                   #### Validate data ####
                   ext_Info <- tools::file_ext(here::here(input$record_information$datapath))
                   #required
                   record_info_columns <- read.csv(here::here('record_info_columns.csv')) %>% 
                     unlist()
                   # record_info_columns <- Yale_record_info %>%  clean_names %>% colnames()
                   # write.csv(record_info_columns, file = 'record_info_columns.csv', row.names=FALSE)
                   #loaded
                   column_names_Info <- colnames(Record_Information_Download)
                   
                   feedbackWarning("record_information", !ext_Info == "csv", "Incorrect file type for Record Information Download")
                   feedbackWarning("record_information", !sum(record_info_columns %in% column_names_Info) == 50, "Missing necessary columns for Record Information Download; please check upload")
                   
                   # shiny::validate(
                   #   need(ext_Info == "csv", "Incorrect file type for Record Information Download"),
                   #   need(sum(record_info_columns %in% column_names_Info) == 50, 
                   #        "Missing necessary columns for Record Information Download")
                   # )
                   
                   ext_RH <- tools::file_ext(here::here(input$review_history$datapath))
                   #required
                   record_hist_columns <- read.csv(here::here('record_hist_columns.csv')) %>% 
                     unlist()
                   # record_hist_columns <- Review_History %>%  clean_names %>% colnames()
                   # write.csv(record_hist_columns, file = 'record_hist_columns.csv', row.names=FALSE)
                   #loaded
                   column_names_RH <- colnames(Review_History)
                   
                   feedbackWarning("review_history", !ext_Info == "csv", "Incorrect file type for Record Information Download")
                   feedbackWarning("review_history", !(length(column_names_RH) == 6), 
                   "Missing necessary columns for Record History; please check upload")
                   
                   
                   # shiny::validate(
                   #   need(ext_RH == "csv", "Incorrect file type for Record History"),
                   #   need(sum(record_hist_columns %in% column_names_RH) == 6, 
                   #        "Missing necessary columns for Record History")
                   # )
                   #### initial manipulation data ####
                   Review_Record <-
                     left_join(
                       Review_History,
                       Record_Information_Download,
                       by = c("protocol_id", "clinical_trials_gov_id")
                     )
                   
                   ### parse the datetimes ###
                   parse <- function(x) parse_date_time(x, orders = c("m/y", "m/d/y", "%m/%d/%y %H:%M"))
                   Review_Record <- Review_Record %>% 
                     mutate_at(c("date_time",
                                 "start_date",
                                 "primary_completion_date",
                                 "study_completion_date",
                                 "initial_release",
                                 "initial_results_release",
                                 "verification_date",
                                 "last_update",
                                 "last_release",
                                 "initial_public_release",
                                 "last_public_release",
                                 "update_expected",
                                 "corrections_expected",
                                 "results_expected",
                                 "all_results_expected"),
                               parse)
                   
                   ### parse the NA's ###
                   replaceNA <- function(x) replace_na(x, "Missing")
                   Review_Record <- Review_Record %>% 
                     mutate_at(c("study_type",
                                 "phase",
                                 "intervention_types",
                                 "u_s_fda_regulated_device",
                                 "record_status",
                                 "results_status",
                                 "fdaaa_status",
                                 "sponsor",
                                 "responsible_party",
                                 "nih_grants"),
                               replaceNA)
                   
                   #### registration only #### 
                   Review_Record_Reg <- Review_Record %>%
                     filter(record_type == "Registration")
                   
                   # filter > 2008 - where complete records begin
                   reg_2008 <- Review_Record_Reg %>%
                     filter(lubridate::year(initial_release) > 2008)
                   
                   # A = first release
                   # B = first publish
                   reg_2008_a <- reg_2008 %>%
                     arrange(., date_time) %>%
                     group_by(protocol_id, clinical_trials_gov_id, event) %>%
                     mutate(indicator =
                              ifelse(
                                event == "Release" &
                                  (
                                    lubridate::as_date(date_time) == lubridate::as_date(initial_release) &
                                      row_number() == 1
                                  ),
                                "A",
                                ifelse(event == "Publish" &
                                         row_number() == 1, "B",
                                       "")
                              )) %>%
                     ungroup() %>%
                     group_by(protocol_id, clinical_trials_gov_id) %>%
                     arrange(., (date_time), .by_group = T) %>%
                     select(
                       protocol_id,
                       clinical_trials_gov_id,
                       record_type,
                       event,
                       user_reviewer,
                       date_time,
                       indicator,
                       everything()
                     )
                   
                   # registration accept date
                   reg_2008_pub <- reg_2008_a %>%
                     filter(indicator == "B") %>%
                     mutate(initial_registration = date_time)
                   
                   reg_2008_b <- left_join(reg_2008_a,
                                           reg_2008_pub[, c("protocol_id",
                                                            "clinical_trials_gov_id",
                                                            "initial_registration")],
                                           by = c("protocol_id", "clinical_trials_gov_id")) %>%
                     mutate(diff_release_pub = difftime(
                       as.Date(initial_registration),
                       as.Date(initial_release),
                       units = "days"
                     ))
                   
                   # N tries
                   reg_2008_try <- reg_2008_b %>%
                     arrange(., date_time) %>%
                     group_by(protocol_id, clinical_trials_gov_id) %>%
                     filter(date_time <= initial_registration) %>%
                     mutate(tries = ((event == 'Release') &
                                       str_starts((user_reviewer), "QA", negate = T) &
                                       ((lead(event) %in% c("Reset", "Publish")) &
                                          str_starts(lead(user_reviewer), "QA"))
                     )) %>%
                     mutate(N_tries = max(cumsum(((event == 'Release') &
                                                    str_starts((user_reviewer), "QA", negate = T)
                     ) &
                       ((lead(event) %in% c("Reset", "Publish")) &
                          str_starts(lead(user_reviewer), "QA")
                       )))) %>%
                     mutate(diff = ceiling(difftime(lead(date_time), date_time, units = "days"))) %>%
                     mutate(CT_gov_response = ifelse(tries == FALSE, NA, (diff))) %>%
                     filter(CT_gov_response > 0) %>% # suspicious 0-days - need to check; resubmitted at same second of acceptance
                     mutate(CT_gov_avg_record_response = mean(CT_gov_response, na.rm = T)) %>%
                     ungroup()
                   
                   # first try per group = ctgov response time for first submission
                   reg_2008_try1 <- reg_2008_try %>% 
                     arrange(., date_time) %>%
                     group_by(protocol_id, clinical_trials_gov_id) %>%
                     slice(1) %>% 
                     rename("first submission response time" = "diff") %>% 
                     select(protocol_id, clinical_trials_gov_id, `first submission response time`)
                   
                   reg_2008_try <- left_join(reg_2008_try, reg_2008_try1)
                   reg_2008_try %>% select(protocol_id, clinical_trials_gov_id, diff)
                   
                   # join tries to dates
                   # success if accept on first try
                   reg_2008_all <- left_join(reg_2008_b, reg_2008_try) %>%
                     mutate(success = ifelse(N_tries <= 2, 1, 0)) %>%
                     # fill these NA's so they can apply to all in group
                     ungroup()
                   
                   #### results only ####
                   Review_Record_Res <- Review_Record %>%
                     filter(record_type == "Results")
                   
                   # filter > 2008 - where complete records begin
                   results_2008 <- Review_Record_Res %>%
                     filter(lubridate::year(initial_release) > 2008)
                   
                   # A = first release
                   # B = first publish
                   results_2008_a <- results_2008 %>%
                     arrange(., date_time) %>%
                     group_by(protocol_id, clinical_trials_gov_id, event) %>%
                     mutate(indicator =
                              ifelse(
                                event == "Release" &
                                  (lubridate::as_date(date_time) == lubridate::as_date(initial_results_release) &
                                     row_number() == 1),
                                "A",
                                ifelse(event == "Publish" &
                                         row_number() == 1, "B",
                                       "")
                              )) %>%
                     ungroup() %>%
                     group_by(protocol_id, clinical_trials_gov_id) %>%
                     arrange(., (date_time), .by_group = T) %>%
                     select(
                       protocol_id,
                       clinical_trials_gov_id,
                       record_type,
                       event,
                       user_reviewer,
                       date_time,
                       indicator,
                       everything()
                     )
                   
                   # results accept date
                   results_2008_pub <- results_2008_a %>%
                     filter(indicator == "B") %>%
                     mutate(initial_results = date_time) %>% 
                     filter(!is.na(initial_results))
                   
                   results_2008_b <- left_join(results_2008_a,
                                               results_2008_pub[, c("protocol_id", "clinical_trials_gov_id", "initial_results")],
                                               by = c("protocol_id", "clinical_trials_gov_id")) %>%
                     mutate(diff_release_pub = difftime(
                       lubridate::as_date(initial_results),
                       lubridate::as_date(initial_results_release),
                       units = "days"
                     ))
                   
                   # N tries
                   results_2008_try <- results_2008_b %>%
                     arrange(., date_time) %>%
                     group_by(protocol_id, clinical_trials_gov_id) %>%
                     filter(date_time <= initial_results) %>%
                     mutate(tries = ((event == 'Release') &
                                       str_starts((user_reviewer), "QA", negate = T) &
                                       ((lead(event) %in% c("Reset", "Publish")) &
                                          str_starts(lead(user_reviewer), "QA"))
                     )) %>%
                     mutate(N_tries = max(cumsum(((event == 'Release') &
                                                    str_starts((user_reviewer), "QA", negate = T)
                     ) &
                       ((lead(event) %in% c("Reset", "Publish")) &
                          str_starts(lead(user_reviewer), "QA")
                       )))) %>%
                     mutate(diff = ceiling(difftime(lead(date_time), date_time, units = "days"))) %>%
                     mutate(CT_gov_response = ifelse(tries == FALSE, NA, (diff))) %>%
                     filter(CT_gov_response > 0) %>% # suspicious 0-days - need to check; resubmitted at same second of acceptance
                     mutate(CT_gov_avg_record_response = mean(CT_gov_response, na.rm = T)) %>%
                     ungroup()
                   
                   # first try per group = ctgov response time for first submission
                   results_2008_try1 <- results_2008_try %>% 
                     arrange(., date_time) %>%
                     group_by(protocol_id, clinical_trials_gov_id) %>%
                     slice(1) %>% 
                     rename("first submission response time" = "diff") %>% 
                     select(protocol_id, clinical_trials_gov_id, `first submission response time`)
                   
                   results_2008_try <- left_join(results_2008_try, results_2008_try1)
                   
                   results_2008_try %>% select(protocol_id, clinical_trials_gov_id, diff)
                   # join tries to dates
                   # are results on time?
                   # time to publish past primary completion date
                   results_2008_all <- left_join(results_2008_b, results_2008_try) %>% #, by = c("protocol_id", "clinical_trials_gov_id")) %>%
                     mutate(results_publish = ifelse(!is.na(initial_results), 1, 0)) %>% 
                     mutate(success = ifelse(N_tries <= 2, 1, 0)) %>%
                     mutate(ontime = ifelse(initial_results <= primary_completion_date, 1, 0)) %>%
                     mutate(results_12_mo = ifelse(
                       initial_results %within%
                         interval(
                           primary_completion_date %m+% years(-1),
                           primary_completion_date %m+% years(1)
                         )
                       ,
                       1 ,
                       0
                     )) %>%
                     mutate(past_primary_completion = difftime(initial_results, primary_completion_date, units = "days")) %>% 
                     # fill these NA's so they can apply to all in group
                     fill(N_tries, CT_gov_avg_record_response, `first submission response time`)  %>%
                     ungroup() 
                   
                   #### UNION FINAL DS ####
                   final_2008_all <- union_all(results_2008_all, reg_2008_all)
                   
                   # # NIH funded, results dates empty, then expected = primary complete + 12, all exp = study complete + 12
                   # # if withdrawn then ...
                   final_2008_all <- final_2008_all %>%
                     mutate(results_expected = ifelse(nih_grants != "Missing" & 
                                                        study_type != "Observational" & # and not observational
                                                        is.na(results_expected),
                                                      as.Date(primary_completion_date) %m+% years(1),
                                                      as.Date(results_expected)),
                            all_results_expected = ifelse(nih_grants != "Missing" &
                                                            is.na(all_results_expected),
                                                          as.Date(study_completion_date) %m+% years(1),
                                                          as.Date(all_results_expected))) %>%
                     mutate(results_expected = as.Date(results_expected, origin='1970-01-01'),
                            all_results_expected = as.Date(all_results_expected, origin='1970-01-01'))
                   
                   # join/create the binary filters #
                   final_2008_all <- left_join(final_2008_all, selectors[c(1:3),c(1:2)], 
                                               by = c("fdaaa_status" = "select_fdaaa_status"))
                   final_2008_all <- left_join(final_2008_all, selectors[,c(4:5)], 
                                               by = c("nih_grants" = "select_nih_grants"))
                   final_2008_all <- final_2008_all %>% 
                     mutate(select_responsible_party = responsible_party,
                                            select_responsible_party_bin = 
                                              ifelse(is.na(responsible_party), 
                                                     "Missing",
                                              ifelse(responsible_party == "[Sponsor]",
                                                     "Sponsor", 
                                                     "PI")))
                   })
               })
}
