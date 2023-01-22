# CT.gov dashboard
# Kaitlin Maciejewski
# Yale Center for Analytical Sciences

#----------------------------------------------------------------------------#

# #### load the libraries ####
library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinythemes)
library(plotly)
library(rlang)
library(lubridate)
library(scales)
library(DT)
library(tidyverse)
library(shinyWidgets)
library(zoo)
library(knitr)
library(conductor)
library(shinybusy)
library(shinyFeedback)

#----------------------------------------------------------------------------#

#### SOURCES ####
source("conduct_tour.R")
source("data_load_wrangle_UI2.R")
source("selectors.R")

#### Helper functions ####
# fn for selectors
pickerInput_fn <- function(id0, id, inputId) {
  pickerInput(
    inputId = paste0(gsub("select_", "", id0), c("_choice")),
    label = paste0(gsub("_", " ", id0), ":"),
    choices = id,
    selected = id,
    options = list(`actions-box` = TRUE),
    multiple = TRUE
  )
}
# function to allow not drilling down past months
# input arg's of interest from dateRangeInput then add new arg's
dateRangeInput2 <-
  function(inputId,
           label,
           minview = "days",
           maxview = "decades") {
    d <- shiny::dateRangeInput(
      inputId,
      label,
      format = "mm/yyyy",
      start = Sys.Date() - years(1),
      end = Sys.Date(),
      separator = " - ",
      min = "2008-01-01",
      max = Sys.Date(),
      autoclose = T
    )
    d$children[[2L]]$children[[1]]$attribs[["data-date-min-view-mode"]] <-
      minview
    d$children[[2L]]$children[[3]]$attribs[["data-date-min-view-mode"]] <-
      minview
    d$children[[2L]]$children[[1]]$attribs[["data-date-max-view-mode"]] <-
      maxview
    d$children[[2L]]$children[[3]]$attribs[["data-date-max-view-mode"]] <-
      maxview
    d
  }

#----------------------------------------------------------------------------#

#### UI ####
ui <- fluidPage(
  useShinyFeedback(),
  theme = shinytheme("cosmo"),
  #shinyFeedback::useShinyFeedback(),
  add_busy_bar(color = "orange", centered = T),
  # for the tour
  useConductor(),
  # for the widget/buttons
  useShinyjs(),
  # this is to edit widths of columns in DT
  tags$head(tags$style(
    HTML("
    td[data-type='factor'] input {
    width: 100px !important;}
    "),
    ".sw-dropdown {
      position: relative;
      display: inline-block;
    }"
  )),
  headerPanel(title = "", windowTitle = "CTGov dashboard"),
  #### STAGE upload ####
  conditionalPanel(
    condition = "input.selectstage == 'stageuploaddata'",
    textInput(
      "selectstage",
      label = "",
      value = "stageuploaddata",
      width = '300px'
    ),
    fluidRow(column(8,
                    includeHTML(
                      "introdashboard.Rhtml" #info
                    ) 
                    ),
                    column(
                      4,
                      fluidRow(h3("Load data here!"), #load data
                               dataUI("load_data")
                               ),
                      fluidRow(textOutput("text")),
                      fluidRow(actionButton("loadDataButton", strong("Go!"), style = 'background-color: #F1A8A8'))
                      )
                    )
    ),
    #### STAGE analysis ####
    conditionalPanel(
      condition = "input.selectstage == 'stageanalysis'",
      ####navbar####
      fluidRow(column(6,
                      h3(strong("Yale ClinicalTrials.gov Reporting Dashboard"), style = "color:#ffffff;")
                      ),
               column(6,
                      div(
                        fluidRow(
                          dropdown(
                            inputId = "downloads",
                            label = "downloads",
                            width = "300px",
                            p(downloadLink("report", "Pdf report"), align = 'left'),
                            p(downloadLink("prospective", "Prospective dataset"), align = 'left'),
                          ),
                          actionButton('help',
                                       'Help',
                                       style = 'background-color: #fffacd; color: black;'),
                          actionButton('info',
                                       'Application Tour',
                                       style = 'background-color: green;'),
                          actionButton("loaddata",
                                       ("Return to data load"),
                                       style = 'background-color: #F1A8A8; color: black;'),
                          align = 'right',
                          )
                        )
                      ), style = "background-color:#222222;"
               )
      ,
      #### sidebar ####
      sidebarLayout(div(
        id = "Sidebar",
        sidebarPanel(
          wellPanel(id = "daterange", style = "padding: 0px; border-color: transparent;", 
                    dateRangeInput2(
                      inputId = 'Date_range_selector0',
                      label = "Date range of data to display: ",
                      minview = "months"
                    ), 
          ),
          wellPanel(id = "dateagg", style = "padding: 0px; border-color: transparent;",
                    selectorsUI("date_agg"),
          ),
          wellPanel(id = "selectors", 
                    actionButton("reset", "Reset inputs"),
                    style = "padding: 0px; border-color: transparent;",
                   uiOutput('resetable_input')
          )
        ),
      ),
      #### main panel ####
      mainPanel(fluidPage(
        id = "mainpanel",
        tabsetPanel(
          id = "tabset",
          type = "tabs",
          #### registration plots ####
          tabPanel(
            title = "Registration",
            fluidRow(column(6, wellPanel(
              h4("Average number of days to publish registration"),
              plotlyOutput("plot1")
            )),
            column(6, wellPanel(
              h4("Average number of tries to publish registration"),
              plotlyOutput("plot2")
            ))),
            fluidRow(
              column(6, wellPanel(
              h4("Percent success on first try"),
              plotlyOutput("plot3")
            )),
            column(6, wellPanel(
              h4("Average CT.gov response time"),
              plotlyOutput("plot4")
            ))),
            fluidRow(column(
              DTOutput("table1"), width = 12
              ))
          ),
          #### results plots ####
          tabPanel(
            "Results",
            fluidRow(
              column(6, wellPanel(
              h4("Average number of days to publish results"),
              plotlyOutput("plot5")
            )),
            column(6, wellPanel(
              h4("Average number of tries to publish results"),
              plotlyOutput("plot6")
            ))),
            fluidRow(
              column(6, wellPanel(
              h4("Percent success on first try"),
              plotlyOutput("plot7")
            )),
            column(6, wellPanel(
              h4("Average CT.gov response time"),
              plotlyOutput("plot8")
            ))),
            fluidRow(
              column(6, wellPanel(
              h4("Results published in 12 months from end of study"),
              plotlyOutput("plot9")
            )),
            column(6, wellPanel(
              h4("Days published past primary completion date"),
              plotlyOutput("plot10")
            ))),
            fluidRow(
              column(DTOutput("table2"), width = 12))
          ),
        )
      )))
    ))

#----------------------------------------------------------------------------#  
  
#### server ####
  server <- function(input, output, session) {
    #### helper functions and setup ####
    
    # nav align
    shinyjs::addClass(id = "menus", class = "navbar-right")

    # don't show the stage text
    shinyjs::hide("selectstage")
    
    # load data
    final_2008_all <- dataServer("load_data")
    
    output$text <-
      renderText({
        if(
          !is.null(final_2008_all())
        ){
          print("Files loaded successfully")
        }
        else if(is.null(final_2008_all())){
          print("Please check uploaded files")
        }
      })
  
    observeEvent(input$loadDataButton, {
      req(final_2008_all())
             updateTextInput(session, "selectstage", value = 'stageanalysis')
             conductor$init()$start()
    })
    
    # date agg
    date_agg_choice <- selectorsServer("date_agg")
    
    # table with all the dates so none in report are missing
    date_table <- reactive({
      month <- as.tibble(as.POSIXct(format(
        seq(
          floor_date(as_date(input$Date_range_selector0[1]), "month"),
          floor_date(as_date(input$Date_range_selector0[2]), "month"),
          by = "month"
        )
      ))) %>%
        rename(date_time_agg = value)
      
      quarter <- as.tibble(as.POSIXct(format(
        seq(
          floor_date(
            as_date(input$Date_range_selector0[1]),
            "quarter"
          ),
          floor_date(as_date(input$Date_range_selector0[2]), "quarter"),
          by = "quarter"
        )
      ))) %>% rename(date_time_agg = value)
      
      halfyear <- as.tibble(as.POSIXct(format(
        seq(
          floor_date(
            as_date(input$Date_range_selector0[1]),
            "halfyear"
          ),
          floor_date(as_date(input$Date_range_selector0[2]), "halfyear"),
          by = "6 month"
        )
      )))  %>%  rename(date_time_agg = value)
      
      ifelse(
        date_agg_choice() == "month",
        return(month),
        ifelse(date_agg_choice() == "quarter",
               return(quarter),
               return(halfyear))
      )
    })
    
    # label months or ranges
    date_label <- reactive({
      ifelse(
        date_agg_choice() == "month",
        "date_label_m",
        ifelse(
          date_agg_choice() == "quarter",
          "date_label_q",
          "date_label_h"
        )
      )
    })
    
    #### plotlyfn ###
    plotlyfn <- function(df, yvar, ylabel) {
      plot_ly(
        x = df[["date_time_agg"]],
        y = df[[deparse(substitute(yvar))]],
        type = "scatter",
        mode = "markers",
        hovertemplate = "%{x} <br>Average: %{y} </br><extra></extra>"
      ) %>%
        add_lines() %>%
        layout(
          xaxis = list(
            title = "Initial Release Month",
            type = "date",
            tickformat = "%b %Y"
          ),
          yaxis = list(title = ylabel),
          showlegend = FALSE
        )
    }
    
    #### buttons ####
    # return to load screen
    observeEvent(input$loaddata, {
      updateTextInput(session, "selectstage", value = "stageuploaddata")
    })
    
    # help
    observeEvent(input$help, {
      showModal(modalDialog(includeHTML("introdashboard.Rhtml"),
                            easyClose = T))
    })
    
    # tour
    observeEvent(input$info, {
      shinyjs::show(id = "Sidebar") # ensures sidebar is open
      shinyjs::addCssClass(id = "main", "col-sm-12")
      conductor$init()$start() # start the tour
    })
    
    #### sidebar selectors ####
    output$resetable_input <- renderUI({
      times <- input$reset
      div(id=letters[(times %% length(letters)) + 1],
          pickerInput_fn("select_study_type", select_study_type, inputId = 'X'),
          #pickerInput_fn("select_phase", select_phase),
          pickerInput_fn("select_intervention_types", select_intervention_types),
          pickerInput_fn(
            "select_u_s_fda_regulated_device",
            select_u_s_fda_regulated_device
          ),
          pickerInput_fn("select_record_status", select_record_status),
          pickerInput(
            "results_status_choice",
            label = ("Select results status: (applies to Results only)"),
            choices = select_results_status,
            selected = select_results_status,
            options = list(`actions-box` = TRUE),
            multiple = T
          ),
          pickerInput_fn("select_fdaaa_status", select_fdaaa_status),
          pickerInput(
            "responsible_party_choice",
            label = ("Select responsible party status: (applies to Registration only)"),
            choices = select_responsible_party,
            selected = select_responsible_party,
            options = list(`actions-box` = TRUE),
            multiple = T
          ),
          #pickerInput_fn("select_sponsor", select_sponsor),
          pickerInput_fn("select_nih_grants", select_nih_grants))
    })
    
    
    #### reactive REGistration dataset ####
    data_reactive_registration <- reactive({
      data_reactive_registration <-
        final_2008_all() %>%
        filter(
          record_type == "Registration" &
            indicator == "A" &
            between(
              as.Date(date_time),
              # date_time range selected
              floor_date(as_date(input$Date_range_selector0[1]), date_agg_choice()),
              floor_date(as_date(input$Date_range_selector0[2]), date_agg_choice())
            )
        ) %>%
        # aggregate by agg_choice
        group_by(date_time_agg = as.Date(floor_date(date_time, date_agg_choice()))) %>%
        mutate(
          plyr::count(date_time_agg),
          diff_release_pub_agg = mean.difftime(diff_release_pub, na.rm = T),
          `first submission response time agg` = mean.difftime(`first submission response time`, na.rm = T),
          # calc aggregations
          tries_agg = mean(N_tries, na.rm = T),
          success_pct_agg = mean(success, na.rm = T),
          success_agg = sum(success, na.rm = T),
          CT_gov_avg_record_response_agg = mean(CT_gov_avg_record_response, na.rm = T)
        )  %>%
        mutate(
          diff_release_pub_agg = round(diff_release_pub_agg, 1),
          `first submission response time agg` = round(`first submission response time agg`, 1),
          # round digits
          tries_agg = round(tries_agg, 1),
          success_pct_agg = 100 * round(success_pct_agg, 3),
          success_agg = round(success_agg, 1),
          CT_gov_avg_record_response_agg = round(CT_gov_avg_record_response_agg, 1)
        ) %>%
        rename(., N_records_agg = freq) %>%
        ungroup()
    })
    
    #### reactive REGistration filtered dataset ####
    data_reactive_registration_filter <- reactive({
      data_reactive_registration <- data_reactive_registration() %>%
        # apply reactive input filters
        filter(study_type %in% c(input$study_type_choice)) %>%
        # filter(., grepl(paste(input$phase_choice, collapse = "|"),
        #                 phase)) %>%
        filter(., grepl(
          paste(input$intervention_types_choice, collapse = "|"),
          intervention_types
        )) %>%
        filter(u_s_fda_regulated_device %in% c(input$u_s_fda_regulated_device_choice)) %>%
        filter(record_status %in% c(input$record_status_choice)) %>%
        #filter(results_status %in% c(input$results_status_choice)) #%>%
        filter(select_fdaaa_status_bin %in% c(input$fdaaa_status_choice)) %>% # y/n has to link back
        #filter(select_sponsor_bin %in% c(input$sponsor_choice)) %>%
        filter(select_responsible_party_bin %in% c(input$responsible_party_choice)) %>%
        filter(select_nih_grants_binary %in% c(input$nih_grants_choice)) %>%
        arrange(desc(date_time_agg))
      
      data_reactive_registration2 <-
        left_join(date_table(), data_reactive_registration)
      
      data_reactive_registration3 <- data_reactive_registration2 %>%
        mutate(
          date_label_h = paste(
            month(date_time_agg, label = TRUE),
            year(date_time_agg),
            "-",
            month(date_time_agg %m+% months(5), label = TRUE),
            year(date_time_agg %m+% months(5))
          ),
          date_label_q = paste(
            month(date_time_agg, label = TRUE),
            year(date_time_agg),
            "-",
            month(date_time_agg %m+% months(2), label = TRUE),
            year(date_time_agg %m+% months(2))
          ),
          date_label_m = paste(month(date_time_agg, label = TRUE),
                               year(date_time_agg))
        )
      
      data_reactive_registration_agg <-
        data_reactive_registration3 %>%
        arrange(., date_time_agg)
      
      data_reactive_registration_agg %>%
        dplyr::select(., date_label(), ends_with("agg")) %>%
        distinct_all() %>%
        mutate(N_records_agg = replace_na(N_records_agg, 0))
    })
    
    #### RESults dataset ####
    # add row for avg time for 1st try
    data_reactive_results <- reactive({
      data_reactive_results <- final_2008_all() %>%
        filter(
          record_type == "Results" &
            indicator == "A" &
            between(
              as.Date(date_time),
              # date_time range selected
              floor_date(as_date(input$Date_range_selector0[1]), date_agg_choice()),
              floor_date(as_date(input$Date_range_selector0[2]), date_agg_choice())
            )
        ) %>% 
        # aggregate by agg_choice
        group_by(date_time_agg = as.Date(floor_date(date_time, date_agg_choice()))) %>%
        mutate(plyr::count(date_time_agg)) %>% # submitted
        rename(., N_records_agg = freq) %>%
        add_count(date_time_agg, wt = results_publish) %>% # published
        rename(., N_pub_agg = n) %>%
        # calc aggregations
        mutate(
          diff_release_pub_agg = mean.difftime(diff_release_pub[!is.na(initial_results)], na.rm = T),
          `first submission response time agg` = mean.difftime(`first submission response time`[!is.na(initial_results)], na.rm = T),
          tries_agg = mean(N_tries[!is.na(initial_results)], na.rm = T),
          success_pct_agg = mean(success[!is.na(initial_results)], na.rm = T),
          success_agg = sum(success[!is.na(initial_results)], na.rm = T),
          CT_gov_avg_record_response_agg = mean(CT_gov_avg_record_response[!is.na(initial_results)], na.rm = T),
          results_12_mo_agg = sum(results_12_mo[!is.na(initial_results)], na.rm = T),
          past_primary_completion_agg = mean(past_primary_completion[!is.na(initial_results)], na.rm = T)
        ) %>%
        mutate(
          diff_release_pub_agg = round(diff_release_pub_agg, 1),
          `first submission response time agg` = round(`first submission response time agg`, 1),
          # round digits
          tries_agg = round(tries_agg, 1),
          success_pct_agg = 100 * round(success_pct_agg, 3),
          success_agg = round(success_agg, 1),
          CT_gov_avg_record_response_agg = round(CT_gov_avg_record_response_agg, 1),
          results_12_mo_agg = round(results_12_mo_agg, 2),
          past_primary_completion_agg = round(past_primary_completion_agg, 1)
        ) %>%
        ungroup() %>%
        # apply reactive input filters
        filter(study_type %in% c(input$study_type_choice)) %>%
        # filter(., grepl(paste(input$phase_choice, collapse = "|"),
        #                 phase)) %>%
        filter(., grepl(
          paste(input$intervention_types_choice, collapse = "|"),
          intervention_types
        )) %>%
        filter(u_s_fda_regulated_device %in% c(input$u_s_fda_regulated_device_choice)) %>%
        filter(record_status %in% c(input$record_status_choice)) %>%
        filter(results_status %in% c(input$results_status_choice)) %>%
        filter(select_fdaaa_status_bin %in% c(input$fdaaa_status_choice)) %>% # y/n has to link back
        #filter(sponsor %in% c(input$sponsor_choice)) %>%
        #filter(responsible_party %in% c(input$responsible_party_choice)) %>%
        filter(select_nih_grants_binary %in% c(input$nih_grants_choice)) %>%
        arrange(desc(date_time_agg))
      
      data_reactive_results2 <-
        left_join(date_table(), data_reactive_results)
      
      data_reactive_results3 <- data_reactive_results2 %>%
        mutate(
          date_label_h = paste(
            month(date_time_agg, label = TRUE),
            year(date_time_agg),
            "-",
            month(date_time_agg %m+% months(5), label = TRUE),
            year(date_time_agg %m+% months(5))
          ),
          date_label_q = paste(
            month(date_time_agg, label = TRUE),
            year(date_time_agg),
            "-",
            month(date_time_agg %m+% months(2), label = TRUE),
            year(date_time_agg %m+% months(2))
          ),
          date_label_m = paste(month(date_time_agg, label = TRUE),
                               year(date_time_agg))
        )
      
      data_reactive_results_agg <- data_reactive_results3 %>%
        arrange(., date_time_agg) %>%
        dplyr::select(., date_label(), ends_with("agg")) %>%
        distinct_all()
      
      data_reactive_results_agg1 <- data_reactive_results_agg %>%
        mutate(N_records_agg = replace_na(N_records_agg, 0)) %>%
        mutate(N_pub_agg = replace_na(N_pub_agg, 0))
      
      data_reactive_results_agg2 <- data_reactive_results_agg1 %>%
        mutate(across(
          tries_agg:results_12_mo_agg,
          ~ if_else(near(N_pub_agg, 0), NA_real_, .x)
        ))
    })
    
    #### registration plots and table ####
    
    #### REGistration plots ####
    output$plot1 <- renderPlotly({
      plotlyfn(data_reactive_registration_filter(),
               diff_release_pub_agg,
               ylabel = "Average number of days to publish")
    })
    
    output$plot2 <- renderPlotly({
      plotlyfn(data_reactive_registration_filter(),
               tries_agg,
               ylabel = "Average number of tries")
    })
    
    output$plot3 <- renderPlotly({
      plotlyfn(data_reactive_registration_filter(),
               success_pct_agg,
               ylabel = "Percent success on first try")
    })
    
    output$plot4 <-  renderPlotly({
      plotlyfn(data_reactive_registration_filter(),
               CT_gov_avg_record_response_agg,
               ylabel = "Average CT.gov response (days)")
    })
    
    table1 <- reactive({
      data_reactive_registration_filter() %>%
        arrange(., date_time_agg) %>%
        dplyr::select(.,-c("date_time_agg")) %>%
        distinct_all() %>%
        data.table::transpose(., make.names = date_label())  # transpose
    })
    
    #### REGistration output table1 ####
    output$table1 <- renderDT({
      DT::datatable(
        table1(),
        rownames = c(
          "Number of records",
          "Average time (days)",
          "Avg CT.gov response time (days) for first try",
          "Average tries",
          "% Success first try",
          "N Success first try",
          "Avg CT.gov response time (days)"
        ),
        extensions = "FixedColumns",
        options = list(
          lengthChange = FALSE,
          dom = 't',
          scrollX = TRUE,
          fixedColumns = list(leftColumns = 1)
        )
      )
    })
    
    
    #### RESults plots ####
    output$plot5 <- renderPlotly({
      plotlyfn(data_reactive_results(),
               diff_release_pub_agg,
               ylabel = "Average number of days to publish")
    })
    
    output$plot6 <- renderPlotly({
      plotlyfn(data_reactive_results(),
               tries_agg,
               ylabel = "Average number of tries")
    })
    
    output$plot7 <- renderPlotly({
      plotlyfn(data_reactive_results(),
               success_pct_agg,
               ylabel = "Percent success on first try")
    })
    
    output$plot8 <- renderPlotly({
      plotlyfn(data_reactive_results(),
               CT_gov_avg_record_response_agg,
               ylabel = "Percent Average CT.gov response (days)")
    })
    
    output$plot9 <-  renderPlotly({
      plotlyfn(data_reactive_results(),
               results_12_mo_agg,
               ylabel = "Results published by 12 months")
    })
    
    output$plot10 <- renderPlotly({
      plotlyfn(data_reactive_results(),
               past_primary_completion_agg,
               ylabel = "Days published past primary completion date")
    })
    
    #### RESults table ####
    table2 <- reactive({
      table_res <- data_reactive_results() %>%
        dplyr::select(-date_time_agg) %>%
        mutate_all( ~ replace(., is.nan(.), NA))
      
      data.table::transpose(table_res,
                            make.names = date_label())
    })
    
    output$table2 <- renderDT({
      DT::datatable(
        table2(),
        rownames = c(
          "Number of records submitted",
          "Number of records published",
          # if initial_results = NA
          "Average days to publish",
          "Avg CT.gov response time (days) for first try",
          "Average tries",
          "% Success first try",
          "N Success first try",
          "Avg CT.gov response time (days)",
          "Reported within 12mo",
          "Avg Days since primary completion date"
        ),
        extensions = "FixedColumns",
        options = list(
          lengthChange = FALSE,
          dom = 't',
          scrollX = TRUE,
          fixedColumns = list(leftColumns = 1)
        )
      )
    })
    
    #### REPORT ####
    output$report <- downloadHandler(
      filename = function() {
        paste("ctgov_prs_report_", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report_pdf.Rmd")
        file.copy("report_pdf.Rmd", tempReport, overwrite = TRUE)
        # Set up parameters to pass to Rmd document
        params <- list(
          reg = table1(),
          res = table2(),
          data_prospective = data_prospective(),
          date_min = as_date(input$Date_range_selector0[1]),
          date_max = as_date(input$Date_range_selector0[2]),
          study_type = input$study_type_choice,
          #phase = input$phase_choice,
          intervention_types = input$intervention_types_choice,
          u_s_fda_regulated_device = input$u_s_fda_regulated_device_choice,
          record_status = input$record_status_choice,
          results_status = input$results_status_choice,
          fdaaa_status = input$fdaaa_status_choice,
          #sponsor = input$sponsor_choice,
          responsible_party = input$responsible_party_choice,
          nih_grants = input$nih_grants_choice
        )
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(
          tempReport,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      }
    )
    
    #### Prospective report ####
    
    contact_info <- reactive({
      # parse contact info
      # grab the contact column
      contact_list <- final_2008_all() %>%
        select(record_owner:collaborators, problems) %>%
        select(-(last_updater))
      
      # separator is \r\n
      # then try to shuffle into name, email, phone
      record_owner <- transform(contact_list,
                                lapply({
                                  l <- list(record_owner)
                                  
                                  names(l) = c('record_owner')
                                  l
                                },
                                function(x)
                                  do.call(rbind, strsplit(x, "\n", fixed = TRUE))),
                                stringsAsFactors = F) %>%
        mutate(Record_Owner_Name = record_owner.1) %>% # first one will be name
        mutate(Record_Owner_email = ifelse(
          stringi::stri_detect_fixed(record_owner.2, "@"),
          record_owner.2,
          ''
        )) %>%  # all emails contain '@'
        # mutate(contact_phone_1 = ifelse(grepl('[0-9]', substr(record_owner.3, 1, 3)), record_owner.3, '')) %>%
        # first three should contain numbers ex: 203 (20
        select(record_owner, Record_Owner_Name, Record_Owner_email) %>%
        unique()
      
      central_contacts <- transform(contact_list,
                                    lapply({
                                      l <- list(central_contacts)
                                      
                                      names(l) = c('central_contacts')
                                      l
                                    },
                                    function(x)
                                      do.call(rbind, strsplit(x, "\n", fixed = TRUE))),
                                    stringsAsFactors = F) %>%
        mutate(Central_Contacts_Name = central_contacts.1) %>% # first one will be name
        mutate(Central_Contacts_email = ifelse(
          stringi::stri_detect_fixed(central_contacts.2, "@"),
          central_contacts.2,
          ''
        )) %>%  # all emails contain '@'
        mutate(Central_Contacts_phone = ifelse(
          grepl('[0-9]', substr(central_contacts.3, 1, 3)),
          central_contacts.3,
          ifelse(grepl(
            '[0-9]', substr(central_contacts.2, 1, 3)
          ), central_contacts.2, '')
        )) %>% #first three should contain numbers ex: 203 (20
        select(
          central_contacts,
          Central_Contacts_Name,
          Central_Contacts_email,
          Central_Contacts_phone
        ) %>%
        unique()
      
      final_2_contact1 <- left_join(final_2008_all(), record_owner)
      final_2_contact2 <-
        left_join(final_2_contact1, central_contacts)
    })
    
    data_prospective <- reactive({
      data_prospective <- contact_info() %>%
        filter(indicator == "A") %>%
        filter(
          !is.na(update_expected) |
            !is.na(corrections_expected) |
            !is.na(results_expected) |
            !is.na(problems)
        ) %>%
        select(
          protocol_id:record_type,
          brief_title,
          overall_status,
          nih_grants,
          fdaaa_status,
          update_expected:all_results_expected,
          Record_Owner_Name,
          Record_Owner_email,
          Central_Contacts_Name,
          Central_Contacts_email,
          Central_Contacts_phone,
          problems
        ) %>%
        mutate(check = ifelse(nih_grants != "Missing" &
                                fdaaa_status == "Missing", "NIH", "")) %>%
        select(-c(nih_grants, fdaaa_status)) %>%
        mutate_at(vars(update_expected:all_results_expected),
                  ~ as.Date(as.POSIXct(.))) %>%
        rename_all(function(x)
          gsub("_", " ", x)) %>%
        rename_all(str_to_title) %>%
        pivot_longer(
          cols = c(`Update Expected`:`All Results Expected`),
          names_to = "Update Type",
          values_to = "Date"
        ) %>%
        filter(!is.na(Date)) %>%
        mutate(update_flag = ifelse(
          Date %within%
            interval(Sys.Date(), Sys.Date() %m+% months(3)),
          1,
          0
        )) %>%
        filter(update_flag == 1) %>%
        select(-c(update_flag)) %>%
        select(Check, everything()) %>% 
        group_by(., `Protocol Id`) %>% 
        arrange(., desc(`Record Type`), `Update Type`) %>% 
        slice(1) %>% 
        ungroup()
      # if they have results, show results
      # prioritize "All results" over other two, "Results" over "Update"
      
      data_prospective <- rename(data_prospective, NIH = Check)
      
    })
    
    output$prospective <- downloadHandler(
      filename = function() {
        paste("ctgov_prospective_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(data_prospective(), file, row.names = F)
      }
    )
  }
  
  #### Run the application ####
  shinyApp(ui = ui, server = server)