

## UI ------------ GENERAL ELEMENTS --------------------

## Badges for Status (red, green, yellow, grey)
mke_status_badge <- function(Status, ...){
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
            Progress >= 0.5 & Progress < 1 ~"warning",
            Progress == 1 ~"success"
  )
}

# makes a progress bar
mke_progress_bar <- function(goal_id, Progress, ...){
  shinyWidgets::progressBar(
    id = paste0(goal_id, "_bar"),
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
    selected = round(Progress* 100, digits = 0) %>% min(100) %>% max(0) %>% as.character()
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

# workload recommendation badge
workload_recommend_badge <- function(workload, available_gls, n_goal){

  if (!check_value(workload)){
    return(NULL)
  }
  else if (workload > 1){
    paste(n_goal,"Goals. ","Eliminate", ceiling(-available_gls), ".") %>%
      dashboardBadge(color = "danger")
  }
  else if (workload > 0.75){
    paste(n_goal,"Goals.", "Focus on Important first") %>%
      dashboardBadge(color = "warning")
  }
  else if (workload < 0.33){
    paste(n_goal,"Goals.","You can add", floor(available_gls), "Goals") %>%
      dashboardBadge(color = "primary")
  }
  else {
    paste(n_goal,"Goals.","Great Workload Balance") %>%
      dashboardBadge(color = "success")
  }
}

# make a percentage complete plot
mke_percent_pie <- function(progress_tot){
  lwr <- 50

  progress_tot <- OdsDataHelper::zero_or_numeric(progress_tot) %>% max(0) %>% min(1)
  labels = c('Complete', "Incomplete")
  colors <- ceiling(progress_tot * (255-lwr) + lwr) %>% int_to_hex() %>% complete_zeros(2) %>% paste0('#28a745', .) %>% c('#FFFFFF00')
  values = c(progress_tot, 1 - progress_tot)
  
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
          sort = FALSE,
          hole = 0.6,
          width = 85, height = 85,
          textinfo='none',
          hoverinfo = 'none',
          marker = list(colors = colors, line = list(color = '#FFFFFF', width = 0)),
          showlegend = F) %>% 
    layout(autosize = F, 
           margin = m,
           plot_bgcolor='transparent',
           paper_bgcolor='transparent'
           ) %>%
    add_annotations(
      text = (paste0(round(progress_tot * 100, digits = 0), "%")),
      showarrow = FALSE,
      # Styling annotations' text:
      font = list(color = '#28a745',
                  family = 'Impact',
                  size = 30)
    ) %>%
    config(
      displaylogo = FALSE,displayModeBar = FALSE
    )
 # autosize = F, width = 500, height = 500, margin = m
  
}

# make a percentage complete plot
mke_workload_pie <- function(workload){
  
  value <- round(workload * 100, digits = 0)
  
  m <- list(
    l = 0,
    r = 0,
    b = 0,
    t = 0,
    pad = 0
  )
  plot_ly(
  #  domain = list(x = c(0, 0.5), y = c(0, 1)),
    value = value,
    width = 110, height = 100,
    number = list(suffix = "%"), 
    #title = list(text = "Workload"),
    title = NULL,
    
    type = "indicator",
    mode = "gauge+number",
    gauge = list(
      bar = list(color = "#001f3f"),
      axis =list(range = list(0, 150)),
      bgcolor = "transparent",
      borderwidth = 0,
      bordercolor = "gray",
      steps = list(
        list(range = c(0, 25), color = "#adb5bd80"),
        list(range = c(25, 75), color = "#28a74580"),
        list(range = c(75, 100), color = "#ffc10780"),
        list(range = c(100, 150), color = "#dc354580")),
      threshold = list(
        line = list(color = "#001f3f", width = 3),
        thickness = 3,
        value = value)))%>%
    layout(autosize = F,
           font = list(family = "Arial"),
           margin = m,
           plot_bgcolor='transparent',
           paper_bgcolor='transparent'
    ) %>%
    config(
      displaylogo = FALSE,displayModeBar = FALSE
  )
}



## UI ------------ GOAL BOXES --------------------

# https://plotly.com/r/gauge-charts/
mke_bucket_ttl <- function(ttl, suffix, ns){
  
  tagList(
    fluidRow(style = "margin-left:0px; margin-right:0px;",
      column(width = 6,
             fluidRow(h4(ttl)),
             fluidRow(shiny::uiOutput(ns(paste0(suffix, "_ttl"))))
      ),
      column(width = 3, style = "text-align:right;",
             plotlyOutput(ns(paste0(suffix, "_plot_1")), height = '85px', width = '90px'),
      ),
      column(width = 3, style = "text-align:right;",
             plotlyOutput(ns(paste0(suffix, "_plot_2")), height = '85px', width = '90px'),
      )
  )
  )
}

# makes the header title, subtitle + detail toggle for box
mke_header <- function(ns, goal_id, Activity, Project, edit){
  if (edit){
    tagList(
      fluidRow(
        column(width = 10, 
               fluidRow(h4(Activity)),
               fluidRow(h6(Project))
        ),
        column(width = 2, style = "padding-left: 0px; padding-right: 0px;",
               switchInput(
                 inputId = ns(paste0(goal_id, "_toggle")),
                 onLabel = icon("edit"),
                 offLabel = icon("edit"),
                 size = "mini"
               )
        )
      ),
      tags$hr(style = "margin-top: 0px;margin-bottom: 10px;")
    )
  } else {
    tagList(
      fluidRow(
        column(width = 12, 
               fluidRow(h4(Activity)),
               fluidRow(h6(Project))
        )
      ),
      tags$hr(style = "margin-top: 0px;margin-bottom: 10px;")
    )
  }
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
mke_footer <- function(ns, goal_id, Progress, Status, edit){
  if (edit){
    tagList(
      shinyjs::hidden(fluidRow(id = ns(paste0(goal_id, "_edit_mode")), column(width = 8, mke_slider_progress(ns, goal_id, Progress)), column(width = 4, mke_picker_badges(ns, goal_id, Status)))),
      fluidRow(id = ns(paste0(goal_id, "_view_mode")),
               column(width = 8, shiny::uiOutput(ns(paste0(goal_id, "_bar")))),
               column(width = 4, shiny::uiOutput(ns(paste0(goal_id, "_fixstatus"))))
      )
    )
  } else {
    tagList(
      fluidRow(id = ns(paste0(goal_id, "_view_mode")),
               column(width = 8, shiny::uiOutput(ns(paste0(goal_id, "_bar")))),
               column(width = 4, shiny::uiOutput(ns(paste0(goal_id, "_fixstatus"))))
      )
    )
  }
}

# makes box content of a Box
mke_content <- function(ns, goal_id, Activity, Project, Status, Progress, Description, Criteria, edit){

  column(width = 12,
         mke_header(ns, goal_id, Activity, Project, edit),
         shinyjs::hidden(mke_body(ns, goal_id, Description, Criteria)),
         mke_footer(ns, goal_id, Progress, Status, edit)
  )
}

# makes the Value Box for 1 goal
make_box <- function(goal_id, Activity, Project, Category, Status, Progress, Description, Criteria, params, ns, edit, ... ){
  bs4InfoBox(
    title = mke_content(ns, goal_id, Activity, Project, Status, Progress, Description, Criteria, edit),
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




# Create the entire Sortable Bucket
mke_goal_bucket <- function(goal, id, ttl_left, ttl_right, filter_left, filter_right, params, ns){
  bucket_list(
    header = NULL,
    group_name = ns(id),
    orientation = "horizontal",
    add_rank_list(
      text = mke_bucket_ttl(ttl_left, "left", ns),
      input_id = which_column(filter_left),
      options = sortable_options(height = "400px"),
      labels = filter_goals(goal, filter_left) %>% mke_goal_box(params, ns, edit = TRUE)
    ),
    add_rank_list(
      text = mke_bucket_ttl(ttl_right, "right", ns),
      input_id = which_column(filter_right),
      options = sortable_options(height = "400px"),
      labels = filter_goals(goal, filter_right) %>% mke_goal_box(params, ns, edit = TRUE)
    )
  )
}



## DATA ------------ HANDLING HELPER --------------------

# gives column from filter 
which_column <- function(id){
  sub("_.*", "", id)
}

# filters the goal dataset accoridng to the criteria
filter_goals <- function(X, filter = NULL){
  switch(filter,
         
         "day" = {X %>% dplyr::filter(quarter & week & day)},
         
         "week" = {X %>% dplyr::filter(quarter & week)},
         
         "week_only" = {X %>% dplyr::filter(quarter & week & !day)}, # this is used on rhs in day pane
         
         "quarter" = {X %>% dplyr::filter(quarter)},
         
         "quarter_only" = {X %>% dplyr::filter((quarter) & !(week) & !(day))}, # this is used on rhs in week pane
         
         "all_incomplete" = {X %>% dplyr::filter(!quarter & Status != "Done")},  # this is used on rhs in quarter pane
         
         "all" = {X}, 
         
         {NULL}
         
         
  )
  
  #dplyr::select(any_of(c("Activity","Project","Category","Status","Continous","Planned","Concluded"))) %>%
  
  
  #  
  # #a <- list("one" = make_box, params = params)
  #  
  #  return(Y)
}

# This joins two DFs savely by checking col names and types or it deletes n rows
# X is the master DF. Add is a DF to join withb possibly different structure
# delete are n ids which are deleted
# update a row by replacing the entire row
save_df_join <- function(X, add = NULL, update = NULL, delete = NULL){
  if (!all(is.null(add))){
    # filter the colnames 
    add <- add %>% dplyr::select(any_of(colnames(X)))
    # get column names
    colnms <- names(add)[names(add) %in% names(X)]
    # transforms all column names into same format
    add[colnms] <- lapply(colnms, function(x) {
      match.fun(paste0("as.", class(X[[x]])[1]))(add[[x]])
    })
    # joining sets
    Z <- X %>% dplyr::bind_rows(add)
  }
  if (!all(is.null(update))){
    # filter the colnames 
    update <- update %>% dplyr::select(any_of(colnames(X)))
    # get column names
    colnms <- names(update)[names(update) %in% names(X)]
    # transforms all column names into same format
    update[colnms] <- lapply(colnms, function(x) {
      match.fun(paste0("as.", class(X[[x]])[1]))(update[[x]])
    })
    # joining sets
    id <- OdsDataHelper::find_column(X, what = "id")
    # deleting old row
    Y <- X %>% dplyr::filter(!(.data[[id]] %in% update[[id]]))
    # adding new row
    Z <- Y %>% dplyr::bind_rows(update)
  }
  
  if (!all(is.null(delete))){
    # finding out which colname is id
    id <- OdsDataHelper::find_column(X, what = "id")
    # deleting
    Z <- X %>% dplyr::filter(!(.data[[id]] %in% delete))
  }
  return(Z)
}

# updates list according to sorting
update_sorting <- function(X, input_vec){
  # hierarchy escalation
  print("Sorting update")

  input_vec <- input_vec %>% OdsDataHelper::discard_by_name("is_all") 
  input_vec$week <- c(input_vec$week, input_vec$day)
  input_vec$quarter <- c(input_vec$quarter, input_vec$week)
  sorter <- input_vec %>% map_dbl(length) %>% which.max %>% magrittr::extract2(input_vec, .)
  
  clean_input <- (input_vec) %>% keep_by_name(c(colnames(X)))
  Y <- X %>% mutate(across(.cols = names(clean_input), ~goal_id %in% clean_input[[cur_column()]]))
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
listen_to <- function(X, input, filter, suffix, format = "list"){
  if (format == "list") {
    X %>% filter_goals(filter) %>% dplyr::mutate(ui_id = paste0(goal_id, suffix)) %>%
      dplyr::mutate(value = map_chr(ui_id, ~as.character(ifelse(length(input[[.x]]) == 0, NA,input[[.x]])))) %>% drop_na(value) %>%
      OdsDataHelper::df_to_lst(vals = "value", nms = "goal_id")
  } else if (format == "df"){
    X %>% filter_goals(filter) %>% dplyr::mutate(ui_id = paste0(goal_id, suffix)) %>%
      dplyr::mutate(value = map_chr(ui_id, ~as.character(ifelse(length(input[[.x]]) == 0, NA,input[[.x]])))) %>% drop_na(value)
  } else {
    NULL
  }
}

## LOGIC --------- STATUS CALCULATIONS --------------------


# calculates the available work hours for a domain
domain_work_h <- function(domain, h_day = 8){
  
  tot_h <- switch(sub("_.*", "", domain),
                  
                  "day" = {19 - lubridate::hour(Sys.time()) %>% max(8)},
                  
                  "week" = {(6 - lubridate::wday(Sys.time())) * h_day},
                  
                  "quarter" = {((90 - lubridate::yday(Sys.time()) %% 90) / 7 * 5) * h_day},
                  
                  "quarter1" = {5},
                  
                  
                  "all" = {difftime(as.POSIXct("2027-11-09"), as.POSIXct(Sys.time()), units="weeks") %>% as.numeric() %>% 
                      magrittr::divide_by(52) %>% magrittr::multiply_by(46) %>% magrittr::multiply_by(40)},
                  
                  {0}
  )
}


#runs all state calcs
get_all_states <- function(X, domains){

  ret <- map(domains, ~iterate_states(.x, X)) %>% setNames(domains)
}

# is the iterator for all states
iterate_states <- function(domain, X, ...){

  Y <- X %>% filter_goals(domain)
  a<- calc_state(
    open_h = Y$open_h,
    work_h = Y$work_h,
    n_goal = nrow(Y),
    av_h_gls = mean(X$work_h, na.rm = T),
    domain = domain
    )
}

# calculates the workload and complete State
calc_state <- function(open_h, work_h, n_goal, av_h_gls = NULL, domain = "week", ...){
  work_h_tot <- sum(work_h, na.rm = TRUE)
  open_h_tot <- sum(open_h, na.rm = TRUE)
  av_h_gls = ifelse(is.null(av_h_gls), 5, mean(av_h_gls, na.rm = TRUE))  # param
  tot_h <- domain_work_h(domain, ...) %>% max(0)
  list(
    workload = (open_h_tot / tot_h),
    progress_tot = (1 - sum(open_h_tot) / sum(work_h_tot)),
    open_h = open_h_tot,
    n_goal = n_goal,
    available_gls = (tot_h - open_h_tot) / av_h_gls
  )
}


# RENDERING AND UPDATING FUNCTIONS ---------
# updates any element
update_ui_element <- function(type, id, value, output = NULL, session = NULL, ...){

  switch(type,
         
         # "ProgressBar" = {shinyWidgets::updateProgressBar(session = session, id = id, value = (value))},
         
         "render" = {output[[id]] <- renderUI({value})},
         
         
         "PickerInput" = {updatePickerInput(session = session, inputId = id, selected = value)},
         
         {NULL}
         
  )
}

render_bucket_list <- function(goal, id, ttl_left, ttl_right, filter_left, filter_right, params, ns, output){

  mke_goal_bucket(goal, id, ttl_left, ttl_right, filter_left, filter_right, params, ns) %>% 
    update_ui_element(type = "render", id = id, value = ., output = output)
}

# renders all goals progress bar functions
render_progress_bar <- function(goal, output){
  # updating the slider
  goal %>% mutate(
    id = paste0(goal_id, "_bar"),
    value = pmap(., .f = mke_progress_bar)) %>% 
    pwalk(., .f = update_ui_element, output = output, type = "render")
}

# renders all goals progress bar functions
render_status_badge <- function(goal, output){
  # updating the slider
  goal %>% mutate(
    id = paste0(goal_id, "_fixstatus"),
    value = pmap(., .f = mke_status_badge)) %>% 
    pwalk(., .f = update_ui_element, output = output, type = "render")
}

update_picker_badge <- function(goal, session){
  goal %>% mutate(
    id = paste0(goal_id, "_status"),
    value = Status) %>% 
    pwalk(., .f = update_ui_element, session = session, type = "PickerInput")
}

# Toggles show/ Hide edit mode
toggle_edit_mode <- function(id, flag, ...){
  # mode on
  if (isTRUE(flag)){
    shinyjs::show(paste0(id, "_body"))
    shinyjs::show(paste0(id, "_edit_mode"))
    shinyjs::hide(paste0(id, "_view_mode"))
  } 
  # mode off
  else {
    shinyjs::hide(paste0(id, "_body"))
    shinyjs::hide(paste0(id, "_edit_mode"))
    shinyjs::show(paste0(id, "_view_mode"))
  }
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
