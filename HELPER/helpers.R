

## UI ------------ GENERAL ELEMENTS --------------------

## Badges for Status (red, green, yellow, grey)
mke_status_badge <- function(Status){
  switch(Status,
         
         "Done" = {dashboardBadge(Status, color = "success")},
         
         "Not started" = {dashboardBadge(Status, color = "secondary")},
         
         "Going Well" = {dashboardBadge(Status, color = "warning")},
         
         "Going Bad" = {dashboardBadge(Status, color = "danger")},
         
         {dashboardBadge(Status, color = "primary")}
  )
}

# returns progress status depending on % value (red, green, yellow, grey)
progress_status <- function(Progress){
  case_when(Progress == 0 ~"secondary", 
            Progress > 0 & Progress < 0.5 ~"danger",
            Progress > 0.5 & Progress < 1 ~"warning",
            Progress == 1 ~"success"
  )
}

# makes a progress bar
mke_progress_bar <- function(ns, goal_id, Progress){
  shinyWidgets::progressBar(
    id = ns(paste0(goal_id, "_bar")),
    value = Progress * 100,
    # total = 100,
    display_pct = TRUE,
    size = "lg",
    status = progress_status(Progress),
    striped = FALSE,
    title = NULL,
    range_value = NULL,
    commas = FALSE,
    unit_mark = "%"
  ) 
}

# makes a progress slider (0-100%)
mke_slider_progress <- function(ns, goal_id, Progress){

  shinyWidgets::sliderTextInput(
    inputId = ns(paste0(goal_id, "_progress")),
    label = NULL,#"Progress:",
    choices = (seq(0:100)-1), 
    post = "%", 
    selected = as.character(Progress* 100)
  )
}

# makes a picker for status badges (with labels)
mke_picker_badges <- function(ns, goal_id, Status){
  shinyWidgets::pickerInput(
    inputId = ns(paste0(goal_id, "_status")),
    label = NULL, 
    choices = c("Done", "Going Well","Going Bad","Not started"),
    multiple = FALSE,
    selected = Status, 
    options = list(container = "body", dropupAuto = F),
    choicesOpt = list(
      content = map_chr(c("Done", "Going Well","Going Bad","Not started"), ~as.character(mke_status_badge(.x)))
    )
  )
}


# make a percentage complete plot
mke_percent_pie <- function(percent){
  lwr <- 50
  labels = c('Complete', "Incomplete")
  colors <- ceiling(percent * (255-lwr) + lwr) %>% int_to_hex() %>% complete_zeros(2) %>% paste0('#28a745', .) %>% c('#FFFFFF00')
  values = c(percent, 1-percent)
  
  m <- list(
    l = 0,
    r = 0,
    b = 0,
    t = 0,
    pad = 0
  )
  
  plot_ly(type='pie',
          labels=labels,
          values=values,
          hole = 0.6,
          textinfo='none',
          marker = list(colors = colors, line = list(color = '#FFFFFF', width = 0)),
          showlegend = F) %>% 
    layout(autosize = F, 
           margin = m, width = 85, height = 85,
           plot_bgcolor='transparent',
           paper_bgcolor='transparent'
           ) %>%
    add_annotations(
      text = (paste0(percent * 100, "%")),
      showarrow = FALSE,
      # Styling annotations' text:
      font = list(color = '#28a745',
                  family = 'Impact',
                  size = 40)
    ) %>%
    config(
      displaylogo = FALSE,displayModeBar = FALSE
    )
 # autosize = F, width = 500, height = 500, margin = m
  
}


# make a percentage complete plot
mke_charge_pie <- function(workload){
  m <- list(
    l = 0,
    r = 0,
    b = 0,
    t = 0,
    pad = 0
  )
  workload <- 67
  plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = workload,
    number = list(suffix = "%"), 
    title = list(text = "Workload"),
    type = "indicator",
    mode = "gauge+number",
    gauge = list(
      bar = list(color = "#001f3f"),
      axis =list(range = list(NULL, 150)),
      bgcolor = "transparent",
      borderwidth = 1,
      bordercolor = "gray",
      steps = list(
        list(range = c(0, 25), color = "#adb5bd80"),
        list(range = c(25, 75), color = "#28a74580"),
        list(range = c(75, 100), color = "#ffc10780"),
        list(range = c(100, 150), color = "#dc354580")),
      threshold = list(
        line = list(color = "#001f3f", width = 3),
        thickness = 3,
        value = workload)))%>%
    layout(autosize = F,
           font = list(family = "Arial"),
           margin = m, width = 100, height = 150,
           plot_bgcolor='transparent',
           paper_bgcolor='transparent'
    ) %>%
    config(
      displaylogo = FALSE,displayModeBar = FALSE
  )
}




## UI ------------ GOAL BOXES --------------------

get_states <- function(X, domain){
  Y <- X %>% filter_goals(domain)
  
 calc_state(sum(Y$open_h, na.rm = T),
                  n_goal = nrow(Y),
                  av_h_gls = mean(Y$work_h, na.rm = T),
                  domain = domain)
  
}

# calculates the workload from completed % and n goals
calc_state <- function(open_h_tot = open_h_tot, n_goal, av_h_gls = NULL, domain = "week", h_day = 8){

  # param
  # complete <- 0.95
  #n_goal<- 20
  av_h_gls = ifelse(is.null(av_h_gls), 5, av_h_gls)
 # gls_open <- n_goal * (1 - complete)
  
 # open_h_tot <- sum(open_h)

  tot_h <- switch(domain,
                   "week" = {(6 - lubridate::wday(Sys.time())) * h_day},
                   
                   "quarter" = {((90 - lubridate::yday(Sys.time()) %% 90) / 7 * 5) * h_day},
                   
                   {0}
         )
  # dys_av * gls_day
  # ceiling((gls_open - dys_av * gls_day))
  # floor((dys_av * gls_day) - gls_open)
  
  
  list(
    open_h = open_h_tot,
    available_gls = (tot_h - open_h_tot) / av_h_gls,
    workload = (open_h_tot / tot_h)
  )
}

goal_eval_badge <- function(workload, excess_gls){

  if (workload > 1){
    paste("Excess of Workload // Eliminate", ceiling(state$excess_gls), "Goals") %>%
      dashboardBadge(color = "danger")
  }
  else if (workload > 0.75){
    paste("Challenging Workload // Focus on Important first") %>%
      dashboardBadge(color = "warning")
  }
  else if (workload < 0.33){
    paste("Low Workload // You can add", floor(-excess_gls), "Goals") %>%
      dashboardBadge(color = "primary")
  }
  else {
    paste("Great Workload Balance") %>%
      dashboardBadge(color = "success")
  }
}
# https://plotly.com/r/gauge-charts/
mke_bucket_ttl <- function(ttl, suffix, ns){
  
  tagList(
    fluidRow(style = "margin-left:0px; margin-right:0px; height:110px;",
      column(width = 9,
             fluidRow(h4(ttl)),
             fluidRow(shiny::uiOutput(ns(paste0(suffix, "_ttl"))))
      ),
      column(width = 3, style = "text-align:right;",
      plotlyOutput(ns(paste0(suffix, "_plot")), height = '90px', width = '90px'),
      )
  )
  )
}

# makes the header title, subtitle + detail toggle for box
mke_header <- function(ns, goal_id, Activity, Project){
  tagList(
    fluidRow(
      column(width = 10, 
             fluidRow(h4(Activity)),
             fluidRow(h6(Project))
      ),
      column(width = 2, 
             switchInput(
               inputId = ns(paste0(goal_id, "_toggle")),
               onLabel = icon("eye"),
               offLabel = icon("eye"),
               size = "mini"
             )
      )
    ),
    tags$hr(style = "margin-top: 0px;margin-bottom: 10px;")
  )
}

# makes the Body (details) for box
mke_body <- function(ns, goal_id, Description, Criteria){
  tags$div(
    id = ns(paste0(goal_id, "_body")),
    fluidRow(
      column(width = 3, h6("Details")),
      column(width = 9, p(OdsDataHelper::empty_or_value(Description, rep = "-")))
    ),
    fluidRow(
      column(width = 3, h6("Metric")),
      column(width = 9, p(OdsDataHelper::empty_or_value(Criteria, rep = "-")))
    ),
    tags$hr(style = "margin-top: 0px;margin-bottom: 10px;")
  )
}

# makes the footer with UI elements (edit or fix mode) for box
mke_footer <- function(ns, goal_id, Progress, Status, edit_mode = FALSE){
  if (isTRUE(edit_mode)){
    fluidRow(column(width = 8, mke_slider_progress(ns, goal_id, Progress)), column(width = 4, mke_picker_badges(ns, goal_id, Status)))
  } else {
    fluidRow(column(width = 8, mke_progress_bar(ns, goal_id, Progress)), column(width = 4, mke_status_badge(Status)))
  }
}

# makes box content of a Box
mke_content <- function(ns, goal_id, Activity, Project, Status, Progress, Description, Criteria, edit_mode){

  column(width = 12,
         mke_header(ns, goal_id, Activity, Project),
         shinyjs::hidden(mke_body(ns, goal_id, Description, Criteria)),
         mke_footer(ns, goal_id, Progress, Status, edit_mode)
  )
}

# makes the Value Box for 1 goal
make_box <- function(goal_id, Activity, Project, Category, Status, Progress, Description, Criteria, edit_mode, params, ns, ... ){
  bs4InfoBox(
    title = mke_content(ns, goal_id, Activity, Project, Status, Progress, Description, Criteria,  edit_mode),
    icon = params$ui_data$category_style %>% dplyr::filter(Category == !!Category) %>% pull(icon) %>% shiny::icon(),
    color = params$ui_data$category_style %>% dplyr::filter(Category == !!Category) %>% pull(bs_color),
    width = 12,
    fill = FALSE,
    gradient = FALSE,
    elevation = 3,
    iconElevation = 0,
    tabName = goal_id
  )
}

# Iterating over the filtered list + make the box for each + return named list
mke_goal_box <- function(X, ...){
  print("mke_goal_box")
  a <- X %>% pmap(.,  .f = make_box, ...) %>%
    setNames(X$goal_id)
}

## DATA ------------ HANDLING HELPER --------------------


# filters the goal dataset accoridng to the criteria
filter_goals <- function(X, filter = NULL){

  print(paste("Filter", filter))
  switch(filter,
         
         "day" = {X %>% dplyr::filter(quarter & week & day)},
         
         "week" = {X %>% dplyr::filter(quarter & week)},
         
         "week_only" = {X %>% dplyr::filter(quarter & week & !day)},
         
         "quarter" = {X %>% dplyr::filter(quarter)},
         
         "quarter_only" = {X %>% dplyr::filter((quarter) & !(week) & !(day))},
         
         "all_complete" = {X %>% dplyr::filter(!quarter & Status != "Done")},
         
         "all" = {X},
         
         {NULL}
         
         
  )
  
  #dplyr::select(any_of(c("Activity","Project","Category","Status","Continous","Planned","Concluded"))) %>%
  
  
  #  
  # #a <- list("one" = make_box, params = params)
  #  
  #  return(Y)
}

# Updates the Goal dataset for a given column and a provided input vector
update_data_set <- function(X, input_vec, col){
  print("updating Data")
  row <- names(input_vec)
  value <- unlist(input_vec, use.names = F)
  Y <- X %>% dplyr::rows_update(tibble(goal_id = row, {{col}} := value), by = "goal_id")
  return(Y)
}

# updates list according to sorting
update_sorting <- function(X, input_vec){
  # hierarchy escalation
  print("Sorting update")
  browser()
  input_vec <- input_vec %>% OdsDataHelper::discard_by_name("is_all") 
  input_vec$week <- c(input_vec$week, input_vec$day)
  input_vec$quarter <- c(input_vec$quarter, input_vec$week)
  sorter <- input_vec %>% map_dbl(length) %>% which.max %>% magrittr::extract2(input_vec, .)
  Y <- X %>% mutate(across(.cols = names(input_vec), ~goal_id %in% input_vec[[cur_column()]]))
  Z <- Y %>% arrange(match(goal_id, sorter))
}

# stores dataset in DB or csv
store_data_set <- function(new, old = NULL){
  if (!identical(new, old)){
    print("saving")
    write_csv(new, here("data", "goal.csv"))
    return(OdsUIHelper::reactive_trigger())
  } else {
    return(NULL)
  }
}

# this listens to the input list given a certain id filter and id suffix
listen_to <- function(X, input, filter, suffix){
  X %>% filter_goals(filter) %>% dplyr::mutate(ui_id = paste0(goal_id, suffix))%>%
    dplyr::mutate(value = map(ui_id, ~input[[.x]])) %>% OdsDataHelper::df_to_lst(vals = "value", nms = "goal_id")
}


## ------------ DATA TABLE MODULE --------------------

# makes a nice empty DT in case of no data
make_empty_DT <- function(){
  data.frame("no data available") %>%
    DT::datatable(selection = 'none',rownames = FALSE,colnames = "",filter = "none",
                  options =  list(dom = '', ordering=F, scrollY = '', paging = F,
                                  columnDefs =  list(list(className = 'dt-center', targets = 0))
                  )
    )
}

# formats all dates into local date formats
add_date_format <- function(DT, lc = "en"){
  clnms <-  DT$x$data %>% purrr::map(lubridate::is.POSIXct) %>% unlist(use.names = F)%>%which()
  if (length(clnms) > 0){
    DT %>%  DT::formatDate(
      columns = clnms,
      method =  "toLocaleString",
      params = list(lc)
    )
  } else {
    DT
  }
}

# adds filter if more than 10 rows
DT_filter <- function(n, n_crit = 10){
  if (n > n_crit){
    "top"
  } else {
    "none"
  }
}

DT_header <- function(tbl, has_header = TRUE){
  
  if (!has_header) {return( rep("", ncol(tbl)))}
  #if (table_id == "user"){return(c("", "", ""))}
  colnames(tbl) %>%
    base::replace(c(grep("btn",.), grep("bar",.), grep("img",.), grep("tag",.), grep("nest",.)), c(""))
}

DT_options <- function(tbl){
  # coldefs
  cldfs <- function(tbl){
    btncls <- tbl %>% colnames() %>% stringr::str_detect("btn") %>% which() - 1
    nstctl <- tbl %>% colnames() %>% stringr::str_detect("nest_control") %>% which() - 1
    nstcld <- tbl %>% colnames() %>% stringr::str_detect("child_data") %>% which() - 1
    columnDefs = list(
      list(width = '25px',className = 'dt-center', targets = c(btncls)), # button column defs
      list(visible = FALSE, targets =  nstcld) ,# Hide row numbers and nested columns
      list(orderable = FALSE, width = '20px', className = 'details-control', targets = nstctl) # turn first column into control column
    )
  }
  # make the list
  list(
    dom = 't',
    ordering=F,
    columnDefs = cldfs(tbl),
    scrollY = '550px',
    paging = F
  )
}

make_tags_DT <- function(X, table_id, params){
  # htlm label with text and color
  text_tags <- params$ui_data$text_tags %>% dplyr::filter(table_id == !!table_id)
  tgs <- text_tags %>% dplyr::pull(column) %>% unique()
  if (OdsDataHelper::check_value(tgs)) {
    
    a <-X %>% dplyr::mutate(dplyr::across(tidyselect::any_of(tgs), ~ OdsUIHelper::make_htmltag_vec(.x, text_tags)))
  } else {
    X
  }
}



# Main DT function
make_DT <- function(X, params, table_id = NULL){

   if (nrow(X) == 0){return(make_empty_DT())}
  
  # creating the main Data set
  tbl <- X %>%
    #make_nesting_DT(params = params, table_id = table_id, control_symbol = "&oplus;") %>% # nesting structure
    #make_buttons_DT(table_id, params) %>% # gets the Buttons in to the cols
    make_tags_DT(table_id, params) %>% # gets the HTML tags cols
    #make_bar_DT() %>% # gets the bar cols
    #make_img_DT() %>% # gets the img tags cols
    dplyr::select(!tidyselect::starts_with("btn"), tidyselect::starts_with("btn")) # reorder cols to put buttons last
  
  has_header <- ifelse(table_id %in% c("xyz"), FALSE, TRUE)
  
  # Rendering the data table
  tbl %>%
    DT::datatable(selection = 'none',
                  rownames = F,
                  colnames = DT_header(tbl, has_header), # add header
                  filter = DT_filter(nrow(tbl)), ## add filter or not
                  options = DT_options(tbl), ## add options
                  #callback = htmlwidgets::JS(childrow_callback(tbl)), ## add callback
                  escape = F) %>%
    add_date_format() # forating certain cols
}

## ------------- 
