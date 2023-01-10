library(shiny)
library(tidyverse)
library(shinyWidgets)
library(shinyjs)
library(DBI)
library(RMariaDB) 
library(RMySQL)
library(readr)
library(here)
library(pool)
library(configr)
library(shiny.i18n)
library(DT)
library(knitr)
library(kableExtra)
library(rmarkdown)
library(tinytex)
library(shinydashboardPlus)
library(shinydashboard)
library(bs4Dash)
library(sortable)
library(shinyjs)
library(lubridate)
library(plotly)

#remotes::install_github("ODAPES/OdsDataHelper",ref="master", dependencies = TRUE , force = TRUE)
#remotes::install_github("ODAPES/OdsDBHelper",ref="master", dependencies = TRUE , force = TRUE)
#remotes::install_github("ODAPES/OdsUIHelper",ref="master",  dependencies = TRUE , force = TRUE)
#remotes::install_github("ODAPES/OdsColor",ref="master",  dependencies = TRUE , force = TRUE)

#renv::snah()

library(OdsDataHelper)
library(OdsDBHelper)
library(OdsUIHelper)
library(OdsColor)


# source the modules
source(here("MODULES","module_weekly.R"), local = F)
source(here("MODULES","module_quarterly.R"), local = F)
source(here("MODULES","module_long_term.R"), local = F)
source(here("HELPER","helpers.R"), local = F)
# 

user <- list(config = "config/system_params.yml")

# sys_params<-get_system_parameters(user)
# text <- import_language()
# 

# params<-list(sys_params = sys_params,
#              text = text,
#              ui_elems = ui_elems
#              )
# 
get_params <- function(){
  list(
    ui_data = list(
      text_tags = read_csv(here("data", "tags.csv")),
      category_style = read_csv(here("data", "category_style.csv"))
    )
  )
}

get_r_control<-function(){

  r_control <- reactiveValues(
    goto_tab = NULL,
    update_reactive = NULL

  )
  return(r_control)
}
get_r_data <- function(){
  goal = read_csv(here("data", "goal.csv")) %>% mutate(open_h = (1 - Progress) * work_h)
  state_week = get_states(goal, "week")
  state_quarter = get_states(goal, "quarter")
  r_data <- reactiveValues(
    goal = goal,
    state_week = state_week,
    state_quarter = state_quarter
  )

  return(r_data)
}

params <- get_params()
#
# App Start -----------------------------------------------------------------

shinyApp(
  ui = dashboardPage(
    title = "Basic Dashboard",
    fullscreen = TRUE,
    header = dashboardHeader(
      title = dashboardBrand(
        title = "Michael's App",
        color = "primary",
       # href = "https://www.google.fr",
        image = "img/mike.jpeg",
      ),
      skin = "light",
      status = "white",
      border = FALSE,
      sidebarIcon = icon("bars"),
      controlbarIcon = icon("th"),
      fixed = FALSE,
      leftUi = tagList(
        dropdownMenu(
          badgeStatus = "info",
          type = "notifications",
          notificationItem(
            inputId = "triggerAction2",
            text = "Error!",
            status = "danger"
          )
        ),
        dropdownMenu(
          badgeStatus = "info",
          type = "tasks",
          taskItem(
            inputId = "triggerAction3",
            text = "My progress",
            color = "orange",
            value = 10
          )
        )
      ),
      rightUi = dropdownMenu(
        badgeStatus = "danger",
        type = "messages",
        messageItem(
          inputId = "triggerAction1",
          message = "message 1",
          from = "Divad Nojnarg",
          image = "https://adminlte.io/themes/v3/dist/img/user3-128x128.jpg",
          time = "today",
          color = "lime"
        )
      )
    ),
    sidebar = dashboardSidebar(
      skin = "light",
      status = "primary",
      elevation = 3,
      sidebarUserPanel(
       # image = "https://image.flaticon.com/icons/svg/1149/1149168.svg",
        name = "Be Focused"
      ),
      sidebarMenu(
        sidebarHeader(h4("Goals")),
        menuItem(
          "Day",
          tabName = "day_id",
          icon = icon("calendar-day")
        ),
        menuItem(
          "Weekly",
          tabName = "weekly_id",
          icon = icon("calendar-week")
        ),
        menuItem(
          "Quarterly",
          tabName = "quarterly_id",
          icon = icon("calendar-days")
        ),
        menuItem(
          "All Goals",
          tabName = "long_term_id",
          icon = icon("bars")
        )
      )
    ),
    # controlbar = dashboardControlbar(
    #   skin = "light",
    #   pinned = TRUE,
    #   collapsed = FALSE,
    #   overlay = FALSE,
    #   controlbarMenu(
    #     id = "controlbarmenu",
    #     controlbarItem(
    #       title = "Item 1",
    #       sliderInput(
    #         inputId = "obs",
    #         label = "Number of observations:",
    #         min = 0,
    #         max = 1000,
    #         value = 500
    #       ),
    #       column(
    #         width = 12,
    #         align = "center",
    #         radioButtons(
    #           inputId = "dist",
    #           label = "Distribution type:",
    #           c(
    #             "Normal" = "norm",
    #             "Uniform" = "unif",
    #             "Log-normal" = "lnorm",
    #             "Exponential" = "exp"
    #           )
    #         )
    #       )
    #     ),
    #     controlbarItem(
    #       "Item 2",
    #       "Simple text"
    #     )
    #   )
    # ),
    # footer = dashboardFooter(
    #   left = a(
    #     href = "https://twitter.com/divadnojnarg",
    #     target = "_blank", "@DivadNojnarg"
    #   ),
    #   right = "2018"
    # ),

# App Body -----------------------------------------------------------------
    
    body = dashboardBody(
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "CSS/prov_css.css")),
      useShinyjs(),
      
      tabItems(
        tabItem(tabName = "weekly_id", Weekly_UI("weekly_id", params)),
        
       #tabItem(tabName = "quarterly_id", Quarterly_UI("quarterly_id", params)),

        tabItem(tabName = "long_term_id", LongTerm_UI("long_term_id", params))
      )
    )
  ),

### SERVER PART-----------------------
  server = function(input, output) {
    
    r_control <- get_r_control()
    r_data <- get_r_data()
    
    observeEvent(r_control$update_reactive, {
      req(r_control$update_reactive)
      print("reimporting")
      # Later more fine graned
      r_data$goal <- read_csv(here("data", "goal.csv"))
    })
    
    # Weekly server logic
    Weekly_SERVER("weekly_id", r_data, r_control, params)
    
    # Weekly server logic
    #Quarterly_SERVER("quarterly_id", r_data, r_control, params)
    
    # long term server logic
    LongTerm_SERVER("long_term_id", r_data, r_control, params)

  }
)