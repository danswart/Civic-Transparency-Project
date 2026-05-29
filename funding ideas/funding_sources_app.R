# funding_sources_app.R
# Funding sources database — Shiny + rhandsontable
# Vertical form for entry + grid for review/edit
# Run with: shiny::runApp("funding_sources_app.R")

# install.packages(c("shiny", "rhandsontable", "DT"))

library(shiny)
library(rhandsontable)
library(DT)

# ---- Configuration ----------------------------------------------------------

data_file <- "funders.rds"

focus_areas <- c(
  "Civic Engagement", "Education", "Government Transparency",
  "Public Health", "Economic Development", "Arts & Culture",
  "Environment", "Research", "General Operating", "Other"
)

statuses <- c("Prospect", "Researching", "Applied", "Awarded",
              "Declined", "Reporting", "Closed")

# Empty template — defines the schema
empty_funders <- function() {
  data.frame(
    org_name         = character(),
    contact_name     = character(),
    email            = character(),
    phone            = character(),
    street_address   = character(),
    website          = character(),
    focus_area       = factor(character(), levels = focus_areas),
    marketing_scheme = character(),
    typical_grant    = numeric(),
    max_grant        = numeric(),
    deadline         = as.Date(character()),
    last_contacted   = as.Date(character()),
    status           = factor(character(), levels = statuses),
    notes            = character(),
    stringsAsFactors = FALSE
  )
}

load_funders <- function() {
  if (base::file.exists(data_file)) {
    base::readRDS(data_file)
  } else {
    empty_funders()
  }
}

# ---- UI ---------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Funding Sources Database"),

  tabsetPanel(
    # ---- Tab 1: Vertical entry form + grid ---------------------------------
    tabPanel(
      "Add / Edit",
      br(),
      fluidRow(
        # Left: vertical form ----
        column(
          width = 4,
          wellPanel(
            h4("New funding source"),
            textInput("f_org_name",       "Organization name"),
            textInput("f_contact_name",   "Contact name"),
            textInput("f_email",          "Email"),
            textInput("f_phone",          "Phone"),
            textAreaInput("f_street_address", "Street address",
                          rows = 2, resize = "vertical"),
            textInput("f_website",        "Website"),
            selectInput("f_focus_area",   "Focus area",
                        choices = c("", focus_areas), selected = ""),
            textAreaInput("f_marketing_scheme", "Marketing / funding scheme",
                          rows = 3, resize = "vertical"),
            numericInput("f_typical_grant", "Typical grant ($)",
                         value = NA, min = 0, step = 1000),
            numericInput("f_max_grant",   "Max grant ($)",
                         value = NA, min = 0, step = 1000),
            dateInput("f_deadline",       "Application deadline",
                      value = NA, format = "yyyy-mm-dd"),
            dateInput("f_last_contacted", "Last contacted",
                      value = NA, format = "yyyy-mm-dd"),
            selectInput("f_status",       "Status",
                        choices = c("", statuses), selected = ""),
            textAreaInput("f_notes",      "Notes",
                          rows = 4, resize = "vertical"),
            br(),
            actionButton("add_entry", "Add to database",
                         class = "btn-primary", width = "100%"),
            br(), br(),
            actionButton("clear_form", "Clear form", width = "100%")
          )
        ),

        # Right: grid + save controls ----
        column(
          width = 8,
          fluidRow(
            column(6,
              actionButton("save", "Save changes to disk",
                           class = "btn-success", width = "100%")
            ),
            column(6,
              downloadButton("download_csv", "Export CSV",
                             style = "width:100%")
            )
          ),
          br(),
          verbatimTextOutput("status_msg"),
          h4("Existing records (editable)"),
          helpText("You can edit cells directly here. Right-click for row delete. ",
                   "Click 'Save changes to disk' to persist."),
          rHandsontableOutput("grid", height = "600px")
        )
      )
    ),

    # ---- Tab 2: View / Filter ----------------------------------------------
    tabPanel(
      "View / Filter",
      br(),
      DTOutput("view_table")
    ),

    # ---- Tab 3: Upcoming deadlines -----------------------------------------
    tabPanel(
      "Upcoming deadlines",
      br(),
      helpText("Deadlines within the next 90 days, sorted soonest first."),
      DTOutput("upcoming_table")
    )
  )
)

# ---- Server -----------------------------------------------------------------

server <- function(input, output, session) {

  funders <- reactiveVal(load_funders())
  status  <- reactiveVal("Loaded.")

  # --- Helpers --------------------------------------------------------------
  date_or_na <- function(x) {
    if (base::is.null(x) || base::length(x) == 0 ||
        base::is.na(x) || base::nchar(base::as.character(x)) == 0) {
      base::as.Date(NA)
    } else {
      base::as.Date(x)
    }
  }

  str_or_na <- function(x) {
    if (base::is.null(x) || base::length(x) == 0 || x == "") {
      NA_character_
    } else {
      x
    }
  }

  reset_form <- function() {
    updateTextInput(session,     "f_org_name",         value = "")
    updateTextInput(session,     "f_contact_name",     value = "")
    updateTextInput(session,     "f_email",            value = "")
    updateTextInput(session,     "f_phone",            value = "")
    updateTextAreaInput(session, "f_street_address",   value = "")
    updateTextInput(session,     "f_website",          value = "")
    updateSelectInput(session,   "f_focus_area",       selected = "")
    updateTextAreaInput(session, "f_marketing_scheme", value = "")
    updateNumericInput(session,  "f_typical_grant",    value = NA)
    updateNumericInput(session,  "f_max_grant",        value = NA)
    updateDateInput(session,     "f_deadline",         value = NA)
    updateDateInput(session,     "f_last_contacted",   value = NA)
    updateSelectInput(session,   "f_status",           selected = "")
    updateTextAreaInput(session, "f_notes",            value = "")
  }

  # --- Add entry from form --------------------------------------------------
  observeEvent(input$add_entry, {
    if (base::is.null(input$f_org_name) || input$f_org_name == "") {
      status("Cannot add: Organization name is required.")
      return()
    }

    new_row <- data.frame(
      org_name         = input$f_org_name,
      contact_name     = str_or_na(input$f_contact_name),
      email            = str_or_na(input$f_email),
      phone            = str_or_na(input$f_phone),
      street_address   = str_or_na(input$f_street_address),
      website          = str_or_na(input$f_website),
      focus_area       = base::factor(str_or_na(input$f_focus_area),
                                       levels = focus_areas),
      marketing_scheme = str_or_na(input$f_marketing_scheme),
      typical_grant    = base::ifelse(base::is.na(input$f_typical_grant),
                                       NA_real_, input$f_typical_grant),
      max_grant        = base::ifelse(base::is.na(input$f_max_grant),
                                       NA_real_, input$f_max_grant),
      deadline         = date_or_na(input$f_deadline),
      last_contacted   = date_or_na(input$f_last_contacted),
      status           = base::factor(str_or_na(input$f_status),
                                       levels = statuses),
      notes            = str_or_na(input$f_notes),
      stringsAsFactors = FALSE
    )

    funders(base::rbind(funders(), new_row))
    status(base::paste0("Added '", input$f_org_name,
                        "' to database (not yet saved to disk)."))
    reset_form()
  })

  observeEvent(input$clear_form, {
    reset_form()
    status("Form cleared.")
  })

  # --- Render editable grid -------------------------------------------------
  output$grid <- renderRHandsontable({
    df <- funders()
    if (base::nrow(df) == 0) {
      df <- empty_funders()
      df[1, ] <- NA
      df$focus_area <- base::factor(NA, levels = focus_areas)
      df$status     <- base::factor(NA, levels = statuses)
    }

    base::suppressWarnings(
      rhandsontable(df, useTypes = TRUE, stretchH = "all", rowHeaders = NULL) |>
        hot_col("org_name",         width = 180) |>
        hot_col("contact_name",     width = 140) |>
        hot_col("email",            width = 180) |>
        hot_col("phone",            width = 120) |>
        hot_col("street_address",   width = 220) |>
        hot_col("website",          width = 180) |>
        hot_col("focus_area",       type = "dropdown", source = focus_areas,
                strict = FALSE, width = 140) |>
        hot_col("marketing_scheme", width = 220) |>
        hot_col("typical_grant",    type = "numeric", format = "$0,0",
                width = 110) |>
        hot_col("max_grant",        type = "numeric", format = "$0,0",
                width = 110) |>
        hot_col("deadline",         type = "date", dateFormat = "YYYY-MM-DD",
                correctFormat = TRUE, width = 110) |>
        hot_col("last_contacted",   type = "date", dateFormat = "YYYY-MM-DD",
                correctFormat = TRUE, width = 120) |>
        hot_col("status",           type = "dropdown", source = statuses,
                strict = FALSE, width = 110) |>
        hot_col("notes",            width = 260) |>
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    )
  })

  # --- Save to disk ---------------------------------------------------------
  observeEvent(input$save, {
    req(input$grid)
    df <- base::suppressWarnings(hot_to_r(input$grid))
    is_empty <- base::apply(df, 1, function(r) base::all(base::is.na(r) | r == ""))
    df <- df[!is_empty, , drop = FALSE]
    base::saveRDS(df, data_file)
    funders(df)
    status(base::paste0("Saved ", base::nrow(df), " rows to ", data_file,
                        " at ", base::format(base::Sys.time(), "%H:%M:%S"), "."))
  })

  output$status_msg <- renderText({ status() })

  # --- View / filter tab ----------------------------------------------------
  output$view_table <- renderDT({
    df <- funders()
    if (base::nrow(df) == 0) return(NULL)
    datatable(
      df,
      filter  = "top",
      options = list(pageLength = 25, scrollX = TRUE),
      rownames = FALSE
    )
  })

  # --- Upcoming deadlines tab -----------------------------------------------
  output$upcoming_table <- renderDT({
    df <- funders()
    if (base::nrow(df) == 0) return(NULL)
    today  <- base::Sys.Date()
    window <- df[!base::is.na(df$deadline) &
                 df$deadline >= today &
                 df$deadline <= today + 90, , drop = FALSE]
    if (base::nrow(window) == 0) return(NULL)
    window <- window[base::order(window$deadline), ]
    window$days_until <- base::as.integer(window$deadline - today)
    datatable(
      window[, c("org_name", "deadline", "days_until", "status",
                 "contact_name", "email")],
      options  = list(pageLength = 25),
      rownames = FALSE
    )
  })

  # --- CSV export -----------------------------------------------------------
  output$download_csv <- downloadHandler(
    filename = function() {
      base::paste0("funders_", base::format(base::Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      utils::write.csv(funders(), file, row.names = FALSE, na = "")
    }
  )
}

# ---- Run --------------------------------------------------------------------

shinyApp(ui, server)
