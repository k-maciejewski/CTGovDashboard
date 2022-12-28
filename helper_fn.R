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
        as_date(input$Date_range_selector0[1]) %m-% months(1),
        "month"
      ),
      floor_date(as_date(input$Date_range_selector0[2]), "month"),
      by = "quarter"
    )
  ))) %>% rename(date_time_agg = value)
  
  halfyear <- as.tibble(as.POSIXct(format(
    seq(
      floor_date(
        as_date(input$Date_range_selector0[1]) %m-% months(1),
        "month"
      ),
      floor_date(as_date(input$Date_range_selector0[2]), "month"),
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