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
  )$
  step(
    el = "#daterange",
    title = "Select dates",
    text = "Choose the date range here",
  )$step(
    el = "#dateagg",
    title = "Aggregate",
    text = "Choose the date aggregation",
  )$step(
    el = "#selectors",
    title = "Filter the data",
    text = "Filter the data using the drop downs and checkboxes. Only records of the types selected will be reflected in tables and charts",
  )$
  step(
    el = "#sw-drop-downloads",
    title = "Download reports",
    text = "<strong>Pdf report:</strong>  Shows the registration and results plots, as generated using the side bar. The default shows monthly summary of 1 year. Information about prospective results or updates in next 3 months are also included <br> 
    <strong>Prospective dataset:</strong>  This will return a csv file with information about studies that have updates or results expected within 3 or 6 months. Includes the contact information for central contact and record owner, and variables in the problem list<br>
    <strong>Reviewer dataset:</strong> a comprehensive dataset of reviewer actions taken on all your registered records in your PRS<br>
    <strong>Problem list dataset:</strong> This will return a csv file with information about studies that have any problems listed. The contact information for central contact and record owner, are included",
  )$
  step(
    el = "#help",
    title = "Help",
    text = "Brings up the info guide",
  )$step(
    el = "#info",
    title = "Tour",
    text = "Start the tour again",
  )$
  step(
    el = "#loaddata",
    title = "Return to data load",
    text = "If there is an error message in the plots below, or if you want to load data, try uploading again",
  )$
  step(
    el = "#tabset",
    title = "Data plots and tables",
    text = "<strong>Registration:</strong> shows plots for average number of days to first publish a record registration, average number of tries (), percent of records successful after 2 rounds of try, and the average response time from CT.gov. 
    <br> <br>
    Table format below. The dates, aggregation, and selection of data types (ex Interventional study only) can be controlled on the sidebar. 
    <br> <br>
    <strong>Results:</strong> same as registration, with additional plots for number of results published within 12 months from end of study, and number of days past primary completion date until results were published
    <br> <br>
      <strong>Reviewer data:</strong> a comprehensive dataset of reviewer actions taken on all your registered records in your PRS
    <br> <br>
      <strong>Problem list dataset</strong>This will return a csv file with information about studies that have any problems listed. The contact information for central contact and record owner, are included",
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
