conductor <- Conductor$
  new(exitOnEsc = T,
      keyboardNavigation = T,
      useModalOverlay = T,
      defaultStepOptions = list(
        cancelIcon = list(enabled = TRUE, NULL)
      ))$
  step(
    title = "Welcome!",
    text = "Welcome to the Clinical Trials Reporting Dashboard. <br> Click 'next' to continue the tour <br> Click <strong>X</strong> above to close window and continue to the dashboard",
    buttons = list(
      list(
        action = "next",
        text = "Next"
      )
    )
    #cancelIcon = list(enabled = TRUE, label = "Close")
  )$
  step(
    el = "#daterange",
    title = "Select dates",
    text = "Choose the date range here",
    #cancelIcon = list(enabled = TRUE, label = "Close")
  )$step(
    el = "#dateagg",
    title = "Aggregate",
    text = "Choose the date aggregation",
    #cancelIcon = list(enabled = TRUE, label = "Close")
  )$step(
    el = "#selectors",
    title = "Filter the data",
    text = "Filter the data using the drop downs and checkboxes. Only records of the types selected will be reflected in tables and charts",
    #cancelIcon = list(enabled = TRUE, label = "Close")
  )$
  step(
    el = "#sw-drop-downloads",
    title = "Download reports",
    text = "<strong>Pdf report:</strong>  Shows the registration and results plots, as generated using the side bar. The default shows monthly summary of 1 year. Information about prospective results or updates in next 3 months are also included <br> 
    <strong>Prospective dataset:</strong>  This will return a csv file with information about studies that have updates or results expected within 3 months. The contact information for central contact and record owner, and variables in the problem list",
    #cancelIcon = list(enabled = TRUE, label = "Close")
  )$
  step(
    el = "#help",
    title = "Help",
    text = "Brings up the info guide",
    #cancelIcon = list(enabled = TRUE, label = "Close")
  )$step(
    el = "#info",
    title = "Tour",
    text = "Start the tour again",
    #cancelIcon = list(enabled = TRUE, label = "Close")
  )$
  step(
    el = "#loaddata",
    title = "Return to data load",
    text = "If there is an error message in the plots below, or if you want to load data, try uploading again",
    #cancelIcon = list(enabled = TRUE, label = "Close")
  )$
  step(
    el = "#tabset",
    title = "Data plots and tables",
    text = "<strong>Registration:</strong> shows plots for average number of days to first publish a record registration, average number of tries (), percent of records successful after 2 rounds of try, and the average response time from CT.gov. 
    <br> <br>
    Table format below. The dates, aggregation, and selection of data types (ex Interventional study only) can be controlled on the sidebar. 
    <br> <br>
    <strong>Results:</strong> same as registration, with additional plots for number of results published within 12 months from end of study, and number of days past primary completion date until results were published",
    #cancelIcon = list(enabled = TRUE, label = "Close"),
    buttons = list(
      list(
        action = "back",
        secondary = TRUE,
        text = "Previous"
      ),
      list(
        action = "next",
        text = "Finish"
      )
    )
  )
