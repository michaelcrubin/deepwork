# LAYER 3 MODULE: This module creates all UI ELEMENTS (like buttons, selectors etc.)
# Detailed Documentation:



## SOME INFORMATION------------
# CAN I REDUCE NS, ID AND PARAMS?
# FOR THE STATIC I NEED ONLY THE NS-ID AND THE UI_DEFS PARAMS CAN ALSO SKIP
# FILEINPUT OK // NO PARAMS
# NUMBER OK // NO PARAMS
# TEXTS OK // USE PARAMS FOR TRANSLATE UI_DATA
# DATEPICKER OK // USE PARAMS FOR TRANSLATE UI_DATA
# SWITCH OK // USE PARAMS FOR TRANSLATE UI DEFS -> THIS IS BEFAUSE THE LABELING CAN BE CHANGED
# CHECKBOX OK // USE PARAMS FOR TRANSLATE UI_DATA
# PICKER OK // USE PARAMS FOR TRANSLATE OPTIONS HARDCODED

# notes regarding main work horse
### DO THIS SMARTER
# I HAVE 1. own label inline / 2. fremdlabel inline, 3. short/top label not inline, 4. static which use a own line, 5. labels itself
# or can the label itself be a variable in ui_data?????
## SOME INFORMATION------------


## DATA -----------

#' DATA: ODAPES PARAMS SIMULATION DATA
#'
#' This is a snapshot of the ODAPES params data to simulate and test. it is not complete.
#'
#' @name params
#' @docType  the ODAPES params list
#' @keywords data
NULL



## FUNCTION: PICKER INPUT --------------------------------------------------------------------------------

#' PICKER: Prepares Defauls Data for Picker Input
#'
#' Function takes a dataframe from a common Odapes table and turns it into a DF which serves as a Default Value for Pickerinput.
#'
#' @param X The dataframe in question
#' @param nms The colname of \code{X} which serves as names in the picker input
#' @param val The colname of \code{X} which serves as vales in the picker input
#' @param optlst a list of optional features. Are also column names. Options are \code{c(order,subtext,fa_icon)}
#' @param hasna Does the pickerlist have a NA option to be selected? Default \code{FALSE}
#' @param selected which entries are selected by default. Can be names or values or T/F list, but must have the same length than nrow(X)
#' @return a standardized dataframe with at least the columns \code{"name", "value"}
#' @importFrom purrr keep
#' @importFrom dplyr mutate select case_when all_of across any_of
#' @importFrom magrittr %>%
#' @importFrom tidyr drop_na
#' @importFrom tibble tibble tibble_row
#' @export
make_picker_df <- function(X, nms, val, optlst = NULL, hasna = FALSE, selected = NULL){

  if (!all(OdsDataHelper::check_value(nms), OdsDataHelper::check_value(val))){return(NULL)}

  optlst <- optlst %>% purrr::keep(. %in% colnames(X))
  Y <- X %>% tidyr::drop_na(any_of(val)) %>%
    dplyr::mutate(value = .data[[val]], name = .data[[nms]]) %>%
    dplyr::mutate(across(all_of(unlist(optlst, use.names = F)), ~ ., .names = "{names(optlst)}"))%>%
    dplyr::arrange(across(any_of(c("order")))) %>%
    dplyr::select(any_of(c("value", "name",  names(optlst))))

  if(isTRUE(hasna)){
    Y <- Y %>% tibble::add_case(tibble::tibble_row(value = NA, name = 'none'), .before = 1)
  }
  Y <- Y %>% dplyr::mutate(selected = dplyr::case_when(any(!!selected %in% value) ~ value %in% !!selected,
                                         any(!!selected %in% name) ~ name %in% !!selected,
                                         TRUE ~ FALSE
  ))

  return(Y)
}


#' PICKER: Actionbox Parameters
#'
#' Function collects parameters to feed as args to UI function.
#'
#' @param params The ODAPES parameter list
#' @return list with options \code{actions-box, size, selectAllText, deselectAllText} for Pickerinput
opt_ac_bx <- function(params){
  list(`actions-box` = TRUE,
       size = 5,
       selectAllText = params$text$t("Select All"),
       deselectAllText = params$text$t("Deselect All")
  )
}

#' PICKER: Searchbox parameters
#'
#' Function collects parameters to feed as args to UI function.
#'
#' @param params The ODAPES parameter list
#' @return list with options \code{live-search, liveSearchNormalize, noneSelectedText, noneResultsText} for Pickerinput
opt_lv_srch <- function(params){
  list(`live-search` = TRUE, liveSearchNormalize = TRUE,
       noneSelectedText = params$text$t("Nothing Selected"),
       noneResultsText = params$text$t("No Options")
  )
}

#' PICKER: Icon parameters
#'
#' Function collects parameters to feed as args to UI function.
#'
#' @param params The ODAPES parameter list
#' @return list with options \code{icon-base} for Pickerinput
opt_icn <- function(params){
  list(`icon-base` = "fa")
}

#' PICKER: Option parameter collector
#'
#' Function collects parameters from different parts to feed as args.
#'
#' @param params The ODAPES parameter list
#' @param opt_vec the vector of options
#' @importFrom purrr reduce map
#' @importFrom magrittr %>%
#' @return list of lists with options \code{actions-box, size, selectAllText, deselectAllText, icon-base, live-search, liveSearchNormalize, noneSelectedText, noneResultsText} for Pickerinput
get_options <-function(opt_vec, params){
  # assings all options functions to a list
  optionFncs <- list(data_icon = opt_icn,
                     data_action = opt_ac_bx,
                     data_live = opt_lv_srch)
  # iterate all options
  opt_vec %>% base::intersect(names(optionFncs)) %>%
    purrr::map(., ~optionFncs[[.x]](params)) %>%
    purrr::reduce(c,  .init = NULL)
}


#' PICKER: Choices Icon parameters
#'
#' Function collects parameters to feed as args to UI function.
#'
#' @param ui_data are the default data comming from the App.
#' @return list with options \code{icon-base} for Pickerinput
chOpt_icon <- function(ui_data){
  if (is.null(ui_data$fa_icon)){return(NULL)}
  list(icon = ui_data$fa_icon)
}

#' PICKER: Choices Subtext parameters
#'
#' Function collects parameters to feed as args to UI function.
#'
#' @param ui_data are the default data comming from the App.
#' @return list with options \code{subtext} for Pickerinput
chOpt_subtext <- function(ui_data){
  if (is.null(ui_data$subtext)){return(NULL)}
  list(subtext = ui_data$subtext)
}

#' PICKER: Choices Labels parameters
#'
#' Function collects parameters to feed as args to UI function.
#'
#' @param ui_data are the default data comming from the App.
#' @return list with options \code{content} for Pickerinput
chOpt_label <- function(ui_data){
  if (any(is.null(ui_data$lbl_vls), is.null(ui_data$lbl_nms))){return(NULL)}
  list(content = sprintf("<span class='label label-%s'>%s</span>",ui_data$lbl_vls, ui_data$lbl_nms))
}


#' PICKER: Choices parameter collector
#'
#' Function collects parameters from different parts to feed as args.
#'
#' @param ui_data are the default data comming from the App.
#' @param opt_vec the vector of options
#' @importFrom purrr reduce map
#' @importFrom magrittr %>%
#' @return list of lists with options \code{icon, subtext, content} for Pickerinput
get_choiceOpt <- function(opt_vec, ui_data){
  # assings all choices opts functions to a list
  choiceOptFncs <- list(data_icon = chOpt_icon,
                        data_subtxt = chOpt_subtext,
                        data_lbl = chOpt_label
  )

  # iterate all choices options
  opt_vec %>% intersect(names(choiceOptFncs))%>%
    map(., ~choiceOptFncs[[.x]](ui_data))%>%
    purrr::reduce(c,.init= NULL)
}


#' PICKER: Multi or Not
#'
#' Function collects parameters for multi selection or not.
#'
#' @param ui_defs Are the ui style definitions. is a row from the table \code{ui_element_D}
#' @param ui_subtype The ui subtype defined for this use.
#' @importFrom purrr reduce map
#' @importFrom magrittr %>%
#' @return list of lists with options \code{multiple, max-options, max-options-text} for Pickerinput
is_multi <- function(ui_subtype, ui_defs){
  switch(ui_subtype,
         "multi" = {list(multiple = TRUE, "max-options" = OdsDataHelper::null_or_value(ui_defs$number), "max-options-text" = OdsDataHelper::null_or_value(ui_defs$txt))}, #
         {list(multiple = FALSE, "max-options" = NULL, "max-options-text" = NULL)}
         )
}

#' PICKER: Selected choices
#'
#' Function selects the defined default selections
#'
#' @param ui_data are the default data comming from the App.
#' @importFrom dplyr filter pull
#' @importFrom magrittr %>%
#' @return a vector of values which are \code{selected = TRUE}
get_selected <- function(ui_data){
  if (is.data.frame(ui_data) & "selected" %in% colnames(ui_data)){
    ui_data %>% dplyr::filter(selected) %>% pull(value)
  } 
  else if (is.list(ui_data) &  "selected" %in% names(ui_data)){
    ui_data %>% magrittr::extract2("selected")
  }
  else {
    return(NULL)
  }
}


#' PICKER: Main parameter collector
#'
#' Function collects all parameters for the Pickerinput and creates a list of them.
#'
#' @param ui_data are the default data comming from the App.
#' @param ui_defs Are the ui style definitions. is a row from the table \code{ui_element_D}
#' @param ui_subtype The ui subtype defined for this use.
#' @param params The ODAPES parameter list
#' @importFrom dplyr select any_of
#' @importFrom tidyselect where
#' @importFrom magrittr %>%
#' @return list of lists with options \code{choices, selected, options, multiple, choicesOpt} for Pickerinput
collect_parameters <-function(ui_data, params, ui_defs, ui_subtype){
  opt_vec <- ui_defs %>% dplyr::select(any_of(c("data_icon","data_subtxt","data_lbl","data_action","data_live"))) %>%
    dplyr::select(where(isTRUE)) %>% names()

  # check if multi // in case, need to append 2 more options to option list
  multi_ret <- is_multi(ui_subtype, ui_defs)
  opts <- multi_ret %>% OdsDataHelper::keep_by_name(c("max-options", "max-options-text")) %>%
    append(get_options(opt_vec, params))

  #opts <-  %>% append(lstmlt)
  list(choices = ui_data %>% OdsDataHelper::df_to_lst("value", "name"),
       selected =  get_selected(ui_data),
       multiple = multi_ret$multiple,
       options = opts,
       choicesOpt = get_choiceOpt(opt_vec, ui_data)
  )
}

#' PICKER: Create ODAPES PickerInput
#'
#' Main function which creates a complete Pickerinput in the ODAPES Style. Note that this function uses the FP Style UI creation \code{OdsUIHelper::UIwalk}
#'
#' @param id the ui_id to identify the ui element. ATTENTION: MUST HAVE THE NAMESPACE APREADY
#' @param label the label for the picker. Can be \code{NULL}, in which case the Label is added programatically (most cases)
#' @param ui_data are the default data comming from the App.
#' @param ui_defs Are the ui style definitions. is a row from the table \code{ui_element_D}
#' @param ui_subtype The ui subtype defined for this use.
#' @param params The ODAPES parameter list
#' @importFrom dplyr select
#' @importFrom shinyWidgets pickerInput
#' @importFrom utils modifyList
#' @importFrom magrittr %>%
#' @return a ready-to-render HTML PickerInput
#' @export
ods_picker <- function(ui_data, id, params, ui_defs, ui_subtype, label){
  list(inputId = id, label = label) %>%
    utils::modifyList(collect_parameters(ui_data, params, ui_defs, ui_subtype))%>%
    OdsUIHelper::UIwalk(.f = shinyWidgets::pickerInput) %>% OdsUIHelper::UIstyleAppend(ui_defs)
}


## UPDATE: Live UI Update process --------------------

#' UPDATE: Collect Update Parameters
#'
#' Function collects all parameters updating any UI element. It's similar to the Pickerinput update collector. It potentially returns many different parameters, if they not apply, it just returns NULL in that particular place.
#'
#' @param ui_data are the default data comming from the App.
#' @param ui_defs Are the ui style definitions. is a row from the table \code{ui_element_D}
#' @param ui_subtype The ui subtype defined for this use.
#' @param params The ODAPES parameter list
#' @importFrom dplyr select any_of
#' @importFrom tidyselect where
#' @importFrom magrittr %>%
#' @return list of lists with options \code{choices, selected, options, value, min, max, multiple, choicesOpt} for any UI Element.
update_collect_parameters <-function(ui_data, params, ui_defs, ui_subtype){

  opt_vec <- ui_defs %>% dplyr::select(any_of(c("data_icon","data_subtxt","data_lbl","data_action","data_live"))) %>%
    dplyr::select(where(isTRUE)) %>% names()

  # check if multi // in case, need to append 2 more options to option list
  multi_ret <- is_multi(ui_subtype, ui_defs)
  opts <- multi_ret%>% OdsDataHelper::keep_by_name(c("max-options", "max-options-text")) %>%
    append(get_options(opt_vec, params))
  lst <- list(
    choices = ui_data %>% OdsDataHelper::df_to_lst("value", "name") %>% null_or_value(),
    selected = get_selected(ui_data),
    value = ui_data$value,
    min = OdsDataHelper::na_or_value(ui_data$min),
    max = OdsDataHelper::na_or_value(ui_data$max),
    step = OdsDataHelper::na_or_value(ui_data$step),
    multiple = multi_ret$multiple,
    options = opts,
    choicesOpt = get_choiceOpt(opt_vec, ui_data)
  )
}


#' UPDATE: Updates any UI Element
#'
#' Function obtains a list of parameters and data, along with a UI Functon and updates a UI element. Note that this function uses the FP Style UI creation \code{OdsUIHelper::UIwalk}
#'
#' @param id the ui_id to identify the ui element. ATTENTION: MUST HAVE THE NAMESPACE APREADY
#' @param ui_function the ui function is any Shiny UI Update funciton. Can be any of \code{updateNumericInput, updatePickerInput, updatePrettyCheckbox}
#' @param ui_data are the default data comming from the App.
#' @param ui_defs Are the ui style definitions. is a row from the table \code{ui_element_D}
#' @param ui_subtype The ui subtype defined for this use.
#' @param params The ODAPES parameter list
#' @param session the session argument of the current Shiny session.
#' @param ... additional args, here used only for pmap use
#' @importFrom utils modifyList
#' @importFrom magrittr %>%
#' @return nothing. Is a side-effect function
#' @export
update_ui <- function(ui_data, session, id, ui_subtype, ui_defs, ui_function, params, ...){
  if (!is.function(ui_function)){return(NULL)}

  argnms <- ui_function %>% base::args() %>% as.list() %>% names()
  list(session = session, inputId = id) %>%
    utils::modifyList(update_collect_parameters(ui_data, params, ui_defs, ui_subtype)) %>%
    OdsDataHelper::keep_by_name(argnms) %>%
    OdsUIHelper::UIwalk(.f = ui_function)
  return(TRUE)
}



## FUNCTION: CHECKBOXES --------------------------------------------------------------------------------

#' CHECKBOX: Main parameter collector
#'
#' Function collects all parameters for the Checkbox and creates a list of them.
#'
#' @param ui_data are the default data comming from the App.
#' @param ui_defs Are the ui style definitions. is a row from the table \code{ui_element_D}
#' @param params The ODAPES parameter list
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @return list with options \code{label, value, status} for Checkbox.
checkbx_data <-function(ui_data, ui_defs, params){
  if (is.na(ui_data)){
    list(label = ui_defs$ttl,
         value = FALSE,
         status = OdsDataHelper::null_or_value(ui_defs$cls)
         )
  } else {
    list(label = params$text$t(ui_data$name),
         value = OdsDataHelper::false_or_value(ui_data$value),
         status = OdsDataHelper::null_or_value(ui_defs$cls)
    )
  }
}

#' CHECKBOX: Create ODAPES CheckBox
#'
#' Main function which creates a complete Checkbox in the ODAPES Style. Note that this function uses the FP Style UI creation \code{OdsUIHelper::UIwalk}
#'
#' @param id the ui_id to identify the ui element. ATTENTION: MUST HAVE THE NAMESPACE APREADY
#' @param label the label for the picker. Can be \code{NULL}, in which case the Label is added programatically (most cases)
#' @param ui_data are the default data comming from the App.
#' @param ui_defs Are the ui style definitions. is a row from the table \code{ui_element_D}
#' @param ui_subtype The ui subtype defined for this use.
#' @param params The ODAPES parameter list
#' @importFrom dplyr select
#' @importFrom shinyWidgets prettyCheckbox
#' @importFrom utils modifyList
#' @importFrom magrittr %>%
#' @return a ready-to-render HTML Checkbox
#' @export
ods_checkbox <- function(ui_data, id, params, ui_defs, ui_subtype, label){
  list(inputId = id) %>%
    utils::modifyList(checkbx_data(ui_data, ui_defs, params)) %>%
    OdsUIHelper::UIwalk(.f = shinyWidgets::prettyCheckbox)%>% OdsUIHelper::UIstyleAppend(ui_defs)
}

## FUNCTION: SWITCH INPUTS --------------------------------------------------------------------------------

#' SWITCH: Label and Status Parameters
#'
#' Function collects the Label and Status parameter for a switch
#'
#' @param ui_defs Are the ui style definitions. is a row from the table \code{ui_element_D}
#' @param params The ODAPES parameter list
#' @param ui_subtype The ui subtype defined for this use.
#' @importFrom magrittr %>%
#' @return list with options \code{onLabel, offLabel, offStatus, onStatus} for Switch
standard_switch_data <- function(ui_subtype,ui_defs, params){
  switch(ui_subtype,

         "on_off" = {list(onLabel = params$text$t(OdsDataHelper::empty_or_value(ui_defs$ttl)), offLabel = params$text$t(OdsDataHelper::empty_or_value(ui_defs$txt)), onStatus = OdsDataHelper::null_or_value(ui_defs$cls), offStatus = OdsDataHelper::null_or_value(ui_defs$css))},

         "yes_no" = {list(onLabel = params$text$t("Yes"), offLabel = params$text$t("No"), onStatus = NULL, offStatus = NULL)},

         "confirm" = {list(onLabel = params$text$t("Agree"), offLabel = params$text$t("Disagree"), onStatus = "success", offStatus = "danger")},

         {list(onLabel = params$text$t(OdsDataHelper::empty_or_value(ui_defs$ttl)), offLabel = params$text$t(OdsDataHelper::empty_or_value(ui_defs$txt)), onStatus = OdsDataHelper::null_or_value(ui_defs$cls), offStatus = NULL)}
  )
}

#' SWITCH: Value Helper
#'
#' Function collects the value parameter
#'
#' @param ui_data are the default data comming from the App. Default \code{FALSE}
#' @importFrom utils modifyList
#' @importFrom magrittr %>%
#' @return list with options \code{value} for Switch
switch_value <- function(ui_data = NULL){
  list(value =  OdsDataHelper::false_or_value(ui_data$value) %>% as.logical())
}

#' SWITCH: Main parameter collector
#'
#' Function collects all parameters for the Checkbox and creates a list of them.
#'
#' @param ui_data are the default data comming from the App.
#' @param ui_defs Are the ui style definitions. is a row from the table \code{ui_element_D}
#' @param params The ODAPES parameter list
#' @param ui_subtype The ui subtype defined for this use.
#' @importFrom utils modifyList
#' @importFrom magrittr %>%
#' @return list with options \code{label, value, status} for Checkbox.
switch_data <- function(ui_data, ui_subtype, ui_defs, params){
  standard_switch_data(ui_subtype, ui_defs, params) %>%
    utils::modifyList(switch_value(ui_data))
}

#' SWITCH: Create ODAPES Switch
#'
#' Main function which creates a complete Switch in the ODAPES Style. Note that this function uses the FP Style UI creation \code{OdsUIHelper::UIwalk}
#'
#' @param id the ui_id to identify the ui element. ATTENTION: MUST HAVE THE NAMESPACE APREADY
#' @param label the label for the picker. Can be \code{NULL}, in which case the Label is added programatically (most cases)
#' @param ui_data are the default data comming from the App.
#' @param ui_defs Are the ui style definitions. is a row from the table \code{ui_element_D}
#' @param ui_subtype The ui subtype defined for this use.
#' @param params The ODAPES parameter list
#' @importFrom utils modifyList
#' @importFrom shinyWidgets switchInput
#' @importFrom magrittr %>%
#' @return a ready-to-render HTML Switch
#' @export
ods_switchInput <- function(ui_data, id, params, ui_defs, ui_subtype, label){
  list(inputId = id) %>%
    utils::modifyList(switch_data(ui_data, ui_subtype, ui_defs, params)) %>%
    OdsUIHelper::UIwalk(.f = shinyWidgets::switchInput) %>% OdsUIHelper::UIstyleAppend(ui_defs)
}


## FUNCTION: DATEPICKER INPUTS --------------------------------------------------------------------------------
#' DATE: Standard parameters Collector
#'
#' Function collects all Standard parameters for the DatePicker. Note that these are a lot. They are inconditional here, i.e. cannot be changed.
#'
#' @importFrom magrittr %>%
#' @importFrom shinyWidgets timepickerOptions
#' @return list with options seen https://dreamrs.github.io/shinyWidgets/reference/airDatepicker.html except of the ones specified elsewhere.
stand_date_opts <- function(){
  list(
    # ui_data opts
    value = NULL,
    placeholder = NULL,
    minDate = NULL,
    maxDate = NULL,
    startView = Sys.time(),

    # ui_type opts
    range = FALSE,
    timepicker = TRUE,
    clearButton = TRUE,

    language = OdsUIHelper::lng_opts(domain = "datepicker_code"),

    separator = " - ",
    multiple = FALSE,
    dateFormat = "yyyy-mm-dd",
    firstDay = NULL,

    disabledDates = NULL,
    highlightedDates = NULL,
    view = 'days',

    minView = 'days',
    monthsField = 'monthsShort',

    todayButton = FALSE,
    width = '100%',
    update_on = "close",

    position = 'top left',
    addon = "none",
    inline = FALSE,
    onlyTimepicker = FALSE,
    toggleSelected = TRUE,
    timepickerOpts = shinyWidgets::timepickerOptions(
      hoursStep = TRUE,
      minutesStep = FALSE,
      dateTimeSeparator = " ",
      timeFormat = "hh"
    )
  )
}

#' DATE: Default Datepicker Parameters
#'
#' Function collects all Default parameters for the Datepicker depending on the subtype.
#'
#' @param ui_subtype The ui subtype defined for this use.
#' @importFrom magrittr %>%
#' @return list with options \code{range, timepicker, clearButton} for Datepicker
date_type_option <- function(ui_subtype){
  switch(ui_subtype,

         "daterange" = {list(range = TRUE, timepicker = FALSE, clearButton = TRUE)},

         "time" = {list(range = FALSE, timepicker = TRUE, clearButton = TRUE)},

         "noclear" = {list(range = FALSE, timepicker = TRUE, clearButton = FALSE)},

         "short" = {list(range = FALSE, timepicker = FALSE, clearButton = TRUE)},

         "date" = {list(range = FALSE, timepicker = FALSE, clearButton = TRUE)},

         {list(NULL)}
  )
}

#' DATE: Specific Datepicker Parameters
#'
#' Function collects all specific parameters for the Datepicker.
#'
#' @param params The ODAPES parameter list
#' @param ui_data are the default data comming from the App.
#' @importFrom magrittr %>%
#' @return list  with options \code{startView, maxDate, minDate, placeholder, value} for Datepicker
date_data_option <- function(ui_data, params){
  if (!OdsDataHelper::check_value(ui_data)){return(list(NULL))}
  list(
    value = OdsDataHelper::null_or_date(ui_data$value),
    placeholder = OdsDataHelper::null_or_value(ui_data$placeholder),
    minDate = OdsDataHelper::null_or_date(ui_data$lower),
    maxDate = OdsDataHelper::null_or_date(ui_data$upper),
    startView = OdsDataHelper::now_or_date(ui_data$value)
  )
}

#' DATE: Create ODAPES DateTime Picker
#'
#' Main function which creates a complete DateTime Picker in the ODAPES Style. Note that this function uses the FP Style UI creation \code{OdsUIHelper::UIwalk}
#'
#' @param id the ui_id to identify the ui element. ATTENTION: MUST HAVE THE NAMESPACE APREADY
#' @param label the label for the picker. Can be \code{NULL}, in which case the Label is added programatically (most cases)
#' @param ui_data are the default data comming from the App.
#' @param ui_defs Are the ui style definitions. is a row from the table \code{ui_element_D}
#' @param ui_subtype The ui subtype defined for this use.
#' @param params The ODAPES parameter list
#' @importFrom utils modifyList
#' @importFrom shinyWidgets airDatepickerInput
#' @importFrom magrittr %>%
#' @return a ready-to-render HTML TextInput
#' @export
ods_datepicker <- function(ui_data, id, params, ui_defs, ui_subtype, label){
  list(inputId = id, label = label) %>%
    utils::modifyList(stand_date_opts()) %>%
    utils::modifyList(date_type_option(ui_subtype))%>%
    utils::modifyList(date_data_option(ui_data, params)) %>%
    OdsUIHelper::UIwalk(.f = shinyWidgets::airDatepickerInput) %>%
    OdsUIHelper::UIstyleAppend(ui_defs)
}


## FUNCTION: TEXT INPUTS --------------------------------------------------------------------------------

#' TEXT: Default Text Input Parameters
#'
#' Function collects all Default parameters for the Textinputs.
#'
#' @param ui_data are the default data comming from the App.
#' @param params The ODAPES parameter list
#' @param ui_subtype The ui subtype defined for this use.
#' @importFrom magrittr %>%
#' @return list with options \code{placeholder, value} for Textinput
default_text_data <- function(ui_data, ui_subtype, params){

  switch(ui_subtype,

         "placeholder" = {list(value = NULL, placeholder = OdsDataHelper::null_or_value(params$text$t(ui_data$value)))},

         "pre_text" = {list(value = OdsDataHelper::null_or_value(params$text$t(ui_data$value)), placeholder = NULL)},

         {list(value = NULL, placeholder = OdsDataHelper::null_or_value(params$text$t(ui_data$value)))}
  )
}

#' TEXT: Text Area height
#'
#' Function collects height of the text area.
#'
#' @param ui_data are the default data comming from the App.
#' @importFrom magrittr %>%
#' @return list with options \code{rows} for Textinput
txt_height <- function(ui_data){
  tryCatch({
    list(rows = as.numeric(ui_data$rows))
  }, error = function(e) {
    list(rows = 3)
  })
}

#' TEXT: Value and Placeholder Collector
#'
#' Function collects all Placeholder and values for the Textinputs.
#'
#' @param ui_data are the default data comming from the App.
#' @param params The ODAPES parameter list
#' @param ui_subtype The ui subtype defined for this use.
#' @importFrom utils modifyList
#' @importFrom magrittr %>%
#' @return list with options \code{placeholder, value} for Textinput
text_data <- function(ui_data, ui_subtype, params){
  if (OdsDataHelper::check_value(ui_data)){
    default_text_data(ui_data, ui_subtype, params)
  } else {
    list(value = NULL, placeholder = NULL)
  }
}

#' TEXT: Create ODAPES Text Input
#'
#' Main function which creates a complete Text Input in the ODAPES Style. Note that this function uses the FP Style UI creation \code{OdsUIHelper::UIwalk}
#'
#' @param id the ui_id to identify the ui element. ATTENTION: MUST HAVE THE NAMESPACE APREADY
#' @param label the label for the picker. Can be \code{NULL}, in which case the Label is added programatically (most cases)
#' @param ui_data are the default data comming from the App.
#' @param ui_defs Are the ui style definitions. is a row from the table \code{ui_element_D}
#' @param ui_subtype The ui subtype defined for this use.
#' @param params The ODAPES parameter list
#' @importFrom utils modifyList
#' @importFrom shiny textInput
#' @importFrom magrittr %>%
#' @return a ready-to-render HTML TextInput
#' @export
ods_textInput <- function(ui_data, id, params, ui_defs, ui_subtype, label){
  list(inputId = id, label = label) %>%
    utils::modifyList(text_data(ui_data, ui_subtype, params))%>%
    OdsUIHelper::UIwalk(.f = shiny::textInput) %>%
    OdsUIHelper::UIstyleAppend(ui_defs)
}

#' TEXT: Create ODAPES Text Area Input
#'
#' Main function which creates a complete Text Area Input in the ODAPES Style. Note that this function uses the FP Style UI creation \code{OdsUIHelper::UIwalk}
#'
#' @param id the ui_id to identify the ui element. ATTENTION: MUST HAVE THE NAMESPACE APREADY
#' @param label the label for the picker. Can be \code{NULL}, in which case the Label is added programatically (most cases)
#' @param ui_data are the default data comming from the App.
#' @param ui_defs Are the ui style definitions. is a row from the table \code{ui_element_D}
#' @param ui_subtype The ui subtype defined for this use.
#' @param params The ODAPES parameter list
#' @importFrom utils modifyList
#' @importFrom shiny textAreaInput
#' @importFrom magrittr %>%
#' @return a ready-to-render HTML Text Area Input
#' @export
ods_textArea <- function(ui_data, id, params, ui_defs, ui_subtype, label){
  list(inputId = id, label = label) %>%
    utils::modifyList(text_data(ui_data, ui_subtype, params)) %>%
    utils::modifyList(txt_height(ui_data)) %>%
    OdsUIHelper::UIwalk(.f = shiny::textAreaInput) %>%
    OdsUIHelper::UIstyleAppend(ui_defs)
}

## FUNCTION: NUMBER INPUT  --------------------------------------------------------------------------------

#' NUMBER: Default Number Input Parameters
#'
#' Function collects all Default parameters for the NumericInput.
#'
#' @param ui_data are the default data comming from the App.
#' @param params The ODAPES parameter list
#' @param ui_subtype The ui subtype defined for this use.
#' @importFrom magrittr %>%
#' @return list with options \code{value, min, max, step} for Numeric input
default_num_data <- function(ui_data, ui_subtype, params){
  switch(ui_subtype,

         "restricted" = {list(value = OdsDataHelper::na_or_value(ui_data$value), min = OdsDataHelper::na_or_value(ui_data$min), max = OdsDataHelper::na_or_value(ui_data$max), step = OdsDataHelper::na_or_value(ui_data$step))},

         {list(value = OdsDataHelper::na_or_value(ui_data$value), min = NA, max = NA, step = NA)}
  )
}

#' NUMBER: Specific Number Input Parameters
#'
#' Function collects all Specific parameters for the NumericInput if given by user. Otherwise calls default inputs.
#'
#' @param ui_data are the default data comming from the App.
#' @param params The ODAPES parameter list
#' @param ui_subtype The ui subtype defined for this use.
#' @importFrom magrittr %>%
#' @return list with options \code{value, min, max, step} for Numeric input
num_data <- function(ui_data,  ui_subtype, params){
  #check_value(ui_data) # not sure if this check is good
  if (all(is.na(ui_data))){
    list(value = NA, min = NA, max = NA, step = NA)
  } else {
    default_num_data(ui_data,  ui_subtype, params)
  }
}

#' NUMBER: Create ODAPES Number Input
#'
#' Main function which creates a complete Number Input in the ODAPES Style. Note that this function uses the FP Style UI creation \code{OdsUIHelper::UIwalk}
#'
#' @param id the ui_id to identify the ui element. ATTENTION: MUST HAVE THE NAMESPACE APREADY
#' @param label the label for the picker. Can be \code{NULL}, in which case the Label is added programatically (most cases)
#' @param ui_data are the default data comming from the App.
#' @param ui_defs Are the ui style definitions. is a row from the table \code{ui_element_D}
#' @param ui_subtype The ui subtype defined for this use.
#' @param params The ODAPES parameter list
#' @importFrom utils modifyList
#' @importFrom shiny textAreaInput
#' @importFrom magrittr %>%
#' @return a ready-to-render HTML Numeric Input
#' @export
ods_numberInput <- function(ui_data, id, params, ui_defs, ui_subtype, label){
  list(inputId = id, label = label) %>%
    utils::modifyList(num_data(ui_data,  ui_subtype, params))%>%
    OdsUIHelper::UIwalk(.f = shiny::numericInput) %>%
    OdsUIHelper::UIstyleAppend(ui_defs)
}


## FUNCTION: FILE INPUTS  --------------------------------------------------------------------------------

#' FILEINPUT: Accepted formats parameters
#'
#' Function gets parameter of accepted file formats.
#'
#' @param ui_data are the default data comming from the App.
#' @param ui_subtype The ui subtype defined for this use.
#' @importFrom magrittr %>%
#' @return vector with a series of extensions of acceted formats.
upload_format_selector <- function(ui_data, ui_subtype){
  if ("format" %in% names(ui_data)){
    return(OdsDataHelper::null_or_value(ui_data$format))
  } else {
    switch(ui_subtype,
           "image" = {c(".jpg", ".png")},
           "map" = {c(".tif", ".shp", ".shx", ".dbf", ".prj", ".kml", ".kmz")},
           "data" = {c("text/csv","text/comma-separated-values,text/plain",".csv")}
    )
  }
}

#' FILEINPUT: Parameters for Fileinput
#'
#' Function collects all parameters for Fileinput
#'
#' @param ui_data are the default data comming from the App.
#' @param ui_defs Are the ui style definitions. is a row from the table \code{ui_element_D}
#' @param ui_subtype The ui subtype defined for this use.
#' @importFrom magrittr %>%
#' @return list with options \code{multiple, accept, buttonLabel, placeholder} for Fileinput
file_params <- function(ui_data, ui_defs, ui_subtype){
  list(
    multiple = OdsDataHelper::false_or_value(ui_data$multi),
    accept = upload_format_selector(ui_data, ui_subtype),
    buttonLabel = OdsDataHelper::null_or_value(ui_defs$txt),
    placeholder = "..."
  )
}

#' FILE: Create ODAPES File Input
#'
#' Main function which creates a complete File Input in the ODAPES Style (to upload files). Note that this function uses the FP Style UI creation \code{OdsUIHelper::UIwalk}
#'
#' @param id the ui_id to identify the ui element. ATTENTION: MUST HAVE THE NAMESPACE APREADY
#' @param label the label for the picker. Can be \code{NULL}, in which case the Label is added programatically (most cases)
#' @param ui_data are the default data comming from the App.
#' @param ui_defs Are the ui style definitions. is a row from the table \code{ui_element_D}
#' @param ui_subtype The ui subtype defined for this use.
#' @param params The ODAPES parameter list
#' @importFrom utils modifyList
#' @importFrom shiny fileInput
#' @importFrom magrittr %>%
#' @return a ready-to-render HTML Numeric Input
#' @export
ods_fileInput <- function(ui_data, id, params, ui_defs, ui_subtype, label){
  list(inputId = id, label = label) %>%
    utils::modifyList(file_params(ui_data, ui_defs, ui_subtype))%>%
    OdsUIHelper::UIwalk(.f = shiny::fileInput) %>%
    OdsUIHelper::UIstyleAppend(ui_defs)
}


## FUNCTION: BUTTONS --------------------------------------------------------------------------------

#' BUTTONS: Buttons for Menus
#'
#' This is the function, which adds buttons to menus in the context of other UI elements. Note that for most cases, buttons are created directly via  \code{c(ods_btn, ods_MS_btn)}, hence this function here is not necessary.
#' Function does only embed buttons into the MS Menu structure, depending on the Button type.
#'
#' @param id the ui_id to identify the ui element. ATTENTION: MUST HAVE THE NAMESPACE APREADY
#' @param label the label for the picker. Can be \code{NULL}, in which case the Label is added programatically (most cases)
#' @param ui_data are the default data comming from the App.
#' @param ui_defs Are the ui style definitions. is a row from the table \code{ui_element_D}
#' @param ui_subtype The ui subtype defined for this use. Possible here are \code{c("MS_btn","normal_btn","download")}
#' @param params The ODAPES parameter list
#' @importFrom utils modifyList
#' @importFrom magrittr %>%
#' @return a ready-to-render HTML action button
#' @export
ods_msButton <- function(ui_data, id, params, ui_defs, ui_subtype, label){

  switch(ui_subtype,

         "MS_btn" = {
           OdsUIHelper::nsid_to_msid(
             OdsDataHelper::empty_or_value(ui_data$table_id, "inherit"),
             OdsDataHelper::empty_or_value(ui_data$row_id,"inherit"),
             id) %>%
             OdsUIHelper::ods_MS_btn(params)
           },

         "normal_btn" = { ## ATTENTION THE ID IS THE CONTAC OF UI ID AND DB ID
           id %>% OdsUIHelper::ods_btn(params)
           },

         "download" = {
           OdsUIHelper::which_nsid(id) %>% OdsUIHelper::ods_btn(params, ui_subtype = "download")
           }
  )
}

## FUNCTION: RENDER COMPONENTS --------------------------------------------------------------------------------

#' RENDER: Create Render Output
#'
#' Main function which creates a REnder Output in the Context of MS Menus. It can be of different types. Can be any of \code{c("ui","html","DT")}
#' Subtypes:
#' - ui is a UI Renderoutput. It is in a Taglist with offset 3 cols from lhs and is 9 cols in total (fitting UI elements)
#' - html is a html Renderoutput. It has no with, col or div at all.
#' - DT is a Datatable Renderoutput. It has the full output with (12 cols)
#'
#' @param id the ui_id to identify the ui element. ATTENTION: MUST HAVE THE NAMESPACE APREADY
#' @param ui_subtype The render output subtype. Can be any of \code{c("ui","html","DT")}
#' @importFrom magrittr %>%
#' @importFrom shiny tagList column uiOutput
#' @importFrom DT dataTableOutput
#' @return a ready-to-render Shiny Renderoutput
#' @export
ods_renderUI <- function(id, ui_subtype){
  switch(ui_subtype,

         "ui"={shiny::tagList(shiny::column(9, offset = 3, shiny::uiOutput(id)))},

         "html"={shiny::uiOutput(id)},

         "DT"={shiny::tagList(shiny::column(12, offset = 0, DT::dataTableOutput(id)))}
  )
}


## FUNCTION: STATIC COMPONENTS --------------------------------------------------------------------------------

#' STATIC: UI Component Label
#'
#' Creates a lhs label for a UI component. This will sit on the 3 cols to the left.
#'
#' @param ui_data are the default data comming from the App.
#' @param ui_defs Are the ui style definitions. is a row from the table \code{ui_element_D}
#' @param params The ODAPES parameter list
#' @importFrom magrittr %>%
#' @importFrom shiny column icon tagList
#' @return a ready-to-render Shiny UI component
#' @export
mke_ms_label <- function(ui_defs, params, ui_data = NULL){
  icn <- OdsDataHelper::null_or_value(ui_defs$icn)
  #css <- ui_defs$css
  if (ui_defs$ttl == "is_var" & OdsDataHelper::check_value(ui_data$name)) {
    ttl <- ui_data$name
    icn <- OdsDataHelper::empty_or_value(ui_data$fa_icon, rep = icn)
  } else {
    ttl <- ui_defs$ttl
  }
  shiny::column(3, offset = 0, shiny::tagList(shiny::icon(icn), params$text$t(ttl)))
}

#' STATIC: Text Divider
#'
#' Creates a UI divider with a Text title embedded
#'
#' @param ui_data are the default data comming from the App.
#' @param ui_defs Are the ui style definitions. is a row from the table \code{ui_element_D}
#' @param params The ODAPES parameter list
#' @importFrom magrittr %>%
#' @importFrom shiny div icon
#' @return a ready-to-render Shiny UI component
#' @export
make_txt_divider <- function(ui_defs, ui_data, params){
  css <- ui_defs$css
  icn <- OdsDataHelper::null_or_value(ui_defs$icn)

  if (OdsDataHelper::check_value(ui_data)) {
    ttl <- ui_data$value
  } else {
    ttl <- ui_defs$ttl
  }
  shiny::div(class = "ui horizontal divider", style = css, shiny::icon(icn), ttl)
}

#' STATIC: Create Static Component
#'
#' Main function which creates a Static Component in the Context of MS Menus. It can be of different types. Can be any of \code{c("txt_divider","divider","empty", "label","fixtext", "smalltxt")}
#' Subtypes:
#' - txt_divider is a hr divider with a title embedded.
#' - divider is a simple hr divider
#' - empty is an empty line of 70%
#' - label is a label fitting a UI element (3 cols on the left)
#' - fixtext is a div with a h5() text element and UI style
#' - smalltext is a div with a p() text element and UI style
#'
#' @param id the ui_id to identify the ui element. ATTENTION: MUST HAVE THE NAMESPACE APREADY
#' @param params The ODAPES parameter list
#' @param ui_data are the default data comming from the App.
#' @param ui_defs Are the ui style definitions. is a row from the table \code{ui_element_D}
#' @param ui_subtype Can be any of \code{c("txt_divider","divider","empty", "label","fixtext", "smalltxt")}
#' @importFrom magrittr %>%
#' @importFrom shiny tags p h5
#' @return a ready-to-render Shiny UI component
#' @export
ods_staticUI <- function(id, ui_subtype, ui_data, ui_defs, params){
  switch(ui_subtype,

         "txt_divider" = {make_txt_divider(ui_defs, ui_data, params)},

         "divider" = {tags$hr()},

         "empty" = {tags$br(style = "line-height: 70%;")},

         "label"={mke_ms_label(ui_defs, params, ui_data)},

         "fixtext"={tags$div(inputId = id, class = ui_defs$cls, shiny::h5(ui_data)) %>% OdsUIHelper::UIstyleAppend(ui_defs)},

         "smalltxt"={tags$div(inputId = id, class = ui_defs$cls, shiny::p(ui_data)) %>% OdsUIHelper::UIstyleAppend(ui_defs)}
         )
}


## MAIN FUNCTION: GENERAL PART --------------------------------------------------------------------------------

#' MAIN: Biforker for UI Element
#'
#' High level function, which is called from the App or an iterator. It's task is to decide which UI main type is generated and call the corresponding lower level function.
#'
#' @param ui_type is the main UI type. Can be any of \code{c("picker","datepicker","checkbox","text","textarea","number","fileInput","switchInput","button","static","render")}
#' @param id the ui_id to identify the ui element. ATTENTION: MUST HAVE THE NAMESPACE APREADY
#' @param label the label for the picker. Can be \code{NULL}, in which case the Label is added programatically (most cases)
#' @param ui_data are the default data comming from the App.
#' @param ui_defs Are the ui style definitions. is a row from the table \code{ui_element_D}
#' @param ui_subtype The ui subtype defined for this use.
#' @param params The ODAPES parameter list
#' @importFrom magrittr %>%
#' @return a ready-to-render HTML component
#' @export
make_ui_element <- function(ui_type, id, ui_data, params, ui_defs, ui_subtype, label = NULL, ...){
  switch(ui_type,

         "picker" = {ods_picker(ui_data, id, params, ui_defs, ui_subtype, label)},

         "datepicker" = {ods_datepicker(ui_data, id, params, ui_defs, ui_subtype, label)},

         "checkbox" = {ods_checkbox(ui_data, id, params, ui_defs, ui_subtype, label)},

         "text" = {ods_textInput(ui_data, id, params, ui_defs, ui_subtype, label)},

         "textarea" = {ods_textArea(ui_data, id, params, ui_defs, ui_subtype, label)},

         "number" = {ods_numberInput(ui_data, id, params, ui_defs, ui_subtype, label)},

         "fileInput" = {ods_fileInput(ui_data, id, params, ui_defs, ui_subtype, label)},

         "button" = {ods_msButton(ui_data, id, params, ui_defs, ui_subtype, label)},

         "switchInput" = {ods_switchInput(ui_data, id, params, ui_defs, ui_subtype, label)},

         "sliderInput" = {ods_sliderInput(ui_data, id, params, ui_defs, ui_subtype, label)},

         "static" = {ods_staticUI(id, ui_subtype, ui_data, ui_defs, params)},

         "render" = {ods_renderUI(id, ui_subtype)}
         )
}


#' MAIN: Iterator helper
#'
#' Helper for a specific situaton if we have iterators.
#'
#' @param ui_data are the default data comming from the App.
#' @param i iterator for vectorized ui elements in tables
#' @importFrom stats setNames
#' @importFrom magrittr %>% extract
#' @return reduced list
reduce_iterator <- function(ui_data, i){
  if (!is.na(i)){
    ui_data %>% unlist(use.names = F) %>%  magrittr::extract(i) %>%
      list() %>% setNames("value")
  } else {
    return(ui_data)
  }
}


#' MAIN: Main UI Element Creator
#'
#' This is the main function, which is called from the App or other Layer 2 Code. It obtains a series of parameteres and default data and manages and decides what UI element to create.
#' It manages the following main tasks and decisions:
#' 1. Is the component inline, full line or short (design)
#' 2. Creates the appropriate label
#' 3. Embeds label and component into the correct columns and taglist
#' 4. Calls the \code{make_ui_element} function to create the actual component
#' 5. Deals with iterations (rows) in case the UI is embedded in a table
#'
#' @param ui_type is the main UI type. Can be any of \code{c("picker","datepicker","checkbox","text","textarea","number","fileInput","switchInput","button","static","render")}
#' @param design defines how the UI element is aligned in the menu. Can be any of \code{c("inline","short","fullwidth", "table")}
#' @param id the ui_id to identify the ui element. ATTENTION: MUST HAVE THE NAMESPACE APREADY
#' @param ui_data are the default data comming from the App.
#' @param ui_defs Are the ui style definitions. is a row from the table \code{ui_element_D}
#' @param ui_subtype The ui subtype defined for this use.
#' @param i iterator for vectorized ui elements in tables
#' @param ... additional args, here only for use in pmap
#' @param params The ODAPES parameter list
#' @importFrom shiny span tagList column tags
#' @importFrom magrittr %>%
#' @return a ready-to-render HTML component
#' @export
MS_ui_element <- function(id, design = "inline", ui_type, ui_subtype = NULL, ui_data = NULL, ui_defs, i = NA, params,  ...){
  if (design == "inline") {
    lbl <- mke_ms_label(ui_defs = ui_defs, params = params, ui_data = ui_data)
    ret <-  shiny::tagList(lbl, shiny::column(6, make_ui_element(ui_type, id, ui_data, params, ui_defs, ui_subtype, label = NULL)))
  }
  else if (design == "short"){
    if (OdsDataHelper::check_value(ui_defs$ttl) | OdsDataHelper::check_value(ui_defs$icn)){
      lbl <- shiny::span(
        shiny::icon(OdsDataHelper::null_or_value(ui_defs$icn)), OdsDataHelper::null_or_value(ui_defs$ttl),
        style="font-size:14px; font-family: Arial Narrow;")
    } else {
      lbl <- NULL
    }
    ret <- shiny::tagList(shiny::column(3, make_ui_element(ui_type, id, ui_data, params, ui_defs, ui_subtype, label = lbl)))
  }
  else if (design == "fullwidth") { ## this has no column and label, it produces everything by itself // mainly static
   ret <- shiny::tagList(make_ui_element(ui_type, id, ui_data, params, ui_defs, ui_subtype, label = NULL))
  }
  else if (design == "table"){
    ui_data <- reduce_iterator(ui_data, i)
    ret <- shiny::tags$td(
      style="width: fit-content; padding-left:5px;padding-right: 5px;",
      make_ui_element(ui_type, id, ui_data, params, ui_defs, ui_subtype, label = NULL)
      )
  }
  else {
    print(paste("Error // missing ms menu def ", id))
  }
  return(ret)
}

