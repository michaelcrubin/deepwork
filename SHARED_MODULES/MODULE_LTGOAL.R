## THIS IS THE MICROSERVICE HANDELS THE LONG TERM GOALS
# AUTHOR: MICHAEL C. RUBIN
# YEAR: 2023
# VERSION: 1.1

## TABLE: ALL GOALS  -----------------------------------------

#  Table preparation
prepare_all_goal <- function(X, params){
  
  Y <- X %>%
    mutate(row_id = goal_id,
           has_btns = TRUE) %>%
    dplyr::select(-any_of(c("quarter","week","day", "Continous", "Progress", "work_h", "Planned", "Concluded", "wip_h", "Comment", "open_h")))
}


# ALL GOALS   Pre Menu part -------

# default for adding
add_all_goal_default <- function(row, params, ...){

  browser()
  categories <- params$ui_data$category_style %>% 
    OdsUIHelper::make_picker_df(nms = "nms", val = "vls",hasna = FALSE)

  list(
    #api_source_id = api_source,
    category = categories,
    title_goal = list(value = "goal_title"),
    name_of_goal = list(value = "name_of_goal"),
    project = list(value = "name_of_project"),
    description = list(value = "describe_project"),
   
    title_metric = list(value = "project_metrics"),
    work_h = list(value = 20, step = 0.25),
    wip_h = list(value = 3, max = 8, step = 0.25),
    goal_due_date = list(value = Sys.Date() + lubridate::days(180), lower = Sys.Date()),
    has_metric = list(value = FALSE),
    criteria = list(value = "define_smart_metric")

  )
}


# detault for update
update_all_goal_default <- function(goal, row, params, ...){
  
  
  browser()
  X <- goal %>% dplyr::filter(goal_id == row)

  
  list(
    #api_source_id = api_source,
    category = categories,
    project = list(value = "name_of_project"),
    goal_due_date = list(value = Sys.Date() + lubridate::days(180), lower = Sys.Date()),
    criteria = list(value = ""),
    
    title_change = list(value = "change_goal"),
    name_of_goal = list(value = "name_of_goal"),
    description = list(value = "describe_project"),
    work_h = list(value = X$work_h, step = 0.25),
    wip_h = list(value = X$wip_h, max = 8, step = 0.25),
    
    
  )

}

# defaults for dummy
default_all_goal <- function(goal, action, row, params){
  switch(action,
         
         "add" = {add_all_goal_default(row, params)},
         
         "update" = {update_all_goal_default(goal, row, params)}
          
       
  )
}


# ALL GOALS  After Menu part -------

# prep update issue record
update_dataevent_prep <- function(data){
  a <- data %>% dplyr::select(-any_of(c("menu_id", "live_message", "temp_id"))) %>% 
    dplyr::mutate(
      update_time = Sys.time(),
      update_user = Sys.getenv("odapes_user")
    )
}




# Manage api_account
manage_all_goal <- function(data, MS_temp_tbl = NULL,  action, table_id, row, params){

  browser()
  ret<-switch(action,
              
         "add" = {},
         
         "update" = {},
         
         "delete" = {}
  )

  return(ret)
  

}




## MODULE: UI FUNCTION ODAPES SETTINGS --------------------
# generates the page with all Tabs for the settings
LTGOAL_UI <- function(id, params) {
  ns <- NS(id)
  # preparing the Data and creating a list of all tabs
  tbs_lst <- OdsTable::make_tab_list("longtermgoal_id", params)

  browser()
  tagList(
    # this is the large header with the foto and name
    #OdsUIHelper::tab_header("ttl_tab_maintain_metos", params),
    # tabs with table
    fluidRow(
      do.call(
        what = tabBox,
        args = c(width = 12, lapply(seq_along(tbs_lst), function(i) {tbs_lst[[i]]}))
      )
    )
  )
}


## MODULE: SERVER FUNCTION ODAPES SETTINGS -------------------
LTGOAL_SERVER <- function(id,  r_data, r_control, params) {
  moduleServer(id,function(input, output, session) {
    
    ##---- APERO ---------------------
    ns <- session$ns
    
    ##---- RENDER THE DT TABLES ---------------------
    ## User API Manager (METOS)
    output$all_goal <- DT::renderDataTable({

      browser()
      r_data$goal %>% 
        prepare_all_goal(params) %>% # preparing the specific data frame
        OdsTable::make_DT("all_goal", params)
    }, server = FALSE)
    
    ## PRE MENU ------------
    # gets defaults values if needed
    observeEvent(r_control$MS_load_default, {
      # filters the relevant Microservices
      req(which_ms(r_control$MS_btn_id) %in% c("MSLTGOAL"))
      ret <- tryCatch({
        switch(which_ms(r_control$MS_btn_id),
               
               "MSLTGOAL" = {default_all_goal(
                 goal = r_data$goal,
                 #MS_temp_tbl = r_control$MS_temp_tbl,
                 action = which_action(r_control$MS_btn_id),
                 row = which_row(r_control$MS_btn_id),
                 params = params
               )},
        )
      }, error = function(e) {
        list(abort = list(success = FALSE, message = "MSPrepare_MSLTGOAL_error_message",source = "abort"))
      })
      
      # decides what to do next
      post_premenu_decision(ret, r_control,r_com, ns)
    })
    
    ## AFTER MENU ------------
    # Runs the update / Storage / delete in DB
    observeEvent(r_control$MS_user_input, {
      # filters the relevant Microservices
      req(which_ms(r_control$MS_btn_id) %in% c("MSLTGOAL"))
      ret  <- tryCatch({
        switch(which_ms(r_control$MS_btn_id),
               
               "MSLTGOAL" = {
                 manage_all_goal(
                   # two standard inputs
                   data = r_control$MS_user_input,
                   #MS_temp_tbl = r_control$MS_temp_tbl,
                   # specific input if needed
                   # control inputs
                   action = which_action(r_control$MS_btn_id),
                   row = which_row(r_control$MS_btn_id),
                   table_id = which_table(r_control$MS_btn_id),
                   params = params
                 )},
        )
      }, error = function(e) {
        list(success = FALSE, message = "MSRunner_MSLTGOAL_error_message", source = "tryCatch")
      })

      # decides what to do next
      post_aftermenu_decision(ret, r_control,r_com)
    })
 
  }) # end module
}

