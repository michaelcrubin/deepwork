## Module Create New Platform user


#####-------------------- UI PART ----------------------------------#####

### UI MODULE -----------------
LongTerm_UI <- function(id, params) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(10, offset = 0,
             shiny::fluidRow(shiny::h2("Long-Term Goals")),
             shiny::fluidRow(shiny::h5("Your long-term Goals define where you want to be in 5 years"))
      ),
      column(2, actionButton(ns("new_goal"), "New Goal"))
    ),
    tags$hr(),
    fluidRow(
      shiny::column(12, offset = 0, DT::dataTableOutput(ns("long_term_tbl")))
      )
  )
}



#####-------------------- SERVER PART ----------------------------------#####

## SERVER FUNCTIONS: Data Handling-----------------

subset_lgtm_data <- function(X){
  
  Y <- X %>%
    mutate(row_id = goal_id,
           has_btns = TRUE) %>%
    dplyr::select(-any_of(c("quarter","week","day", "Continous", "Progress", "work_h", "Planned", "Concluded", "wip_h", "Comment", "open_h")))
}

### SERVER MODULE -----------------
LongTerm_SERVER <- function(id, r_data, r_control, params) {
  moduleServer(id,function(input, output, session) {
    
    ##-------------- APERO----------------
    ns <- session$ns
    # creating new Temp Var for transfer inside module // SOME OR ALL WILL COME FROM CENTRALIZED R_ PART
    
    #-----NEW
    
    output$long_term_tbl <- renderDT({
      r_data$goal %>% subset_lgtm_data() %>%
        OdsTable::make_DT(params, table_id = "long_term_tbl")
    })
  })
}
