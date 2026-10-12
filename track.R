#STEP 1: Install required packages

required_packages <- c(
  "shiny",
  "bslib",
  "DT",
  "leaflet",
  "qrcode"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(shiny)
library(bslib)
library(DT)
library(leaflet)

#STEP 2: Create project data folder

data_dir <- "BusConnect_Data"

if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}

#STEP 3: CSV file paths

users_file <- file.path(data_dir, "users.csv")
buses_file <- file.path(data_dir, "buses.csv")
posts_file <- file.path(data_dir, "posts.csv")
reports_file <- file.path(data_dir, "reports.csv")

#STEP 4: Create initial data

if (!file.exists(users_file)) {
  users <- data.frame(
    username = c("officer", "passenger"),
    password = c("Officer123", "Passenger123"),
    role = c("Officer", "Passenger"),
    stringsAsFactors = FALSE
  )
  
  write.csv(users, users_file, row.names = FALSE)
}

if (!file.exists(buses_file)) {
  buses <- data.frame(
    bus_id = c("BUS001", "BUS002", "BUS003"),
    bus_number = c("TN01AB1234", "TN01AB5678", "TN01AB9012"),
    route = c("21", "15", "27"),
    start_point = c("Central", "Airport", "Market"),
    end_point = c("College", "Central", "Bus Stand"),
    next_stop = c("Main Road", "Railway Station", "Hospital"),
    scheduled_time = c("08:30 AM", "09:00 AM", "09:30 AM"),
    status = c("Running", "Delayed", "Running"),
    latitude = c(13.0827, 13.0604, 13.0878),
    longitude = c(80.2707, 80.2496, 80.2785),
    stringsAsFactors = FALSE
  )
  
  write.csv(buses, buses_file, row.names = FALSE)
}

if (!file.exists(posts_file)) {
  posts <- data.frame(
    post_id = integer(),
    username = character(),
    route = character(),
    message = character(),
    timestamp = character(),
    stringsAsFactors = FALSE
  )
  
  write.csv(posts, posts_file, row.names = FALSE)
}

if (!file.exists(reports_file)) {
  reports <- data.frame(
    report_id = integer(),
    username = character(),
    route = character(),
    bus_stop = character(),
    issue = character(),
    description = character(),
    status = character(),
    officer_response = character(),
    timestamp = character(),
    stringsAsFactors = FALSE
  )
  
  write.csv(reports, reports_file, row.names = FALSE)
}

#STEP 5: Helper functions

read_data <- function(file) {
  tryCatch(
    read.csv(file, stringsAsFactors = FALSE),
    error = function(e) data.frame()
  )
}

save_data <- function(data, file) {
  write.csv(data, file, row.names = FALSE)
}

#STEP 6: User Interface

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#087F8C"
  ),
  
  titlePanel("BusConnect"),
  
  p("Smart Public Transport Tracking & Passenger Communication"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Account Login"),
      
      textInput("username", "Username"),
      
      passwordInput("password", "Password"),
      
      actionButton(
        "login",
        "Login",
        class = "btn-primary"
      ),
      
      actionButton(
        "logout",
        "Logout"
      ),
      
      tags$hr(),
      
      h5("Demo Login Details"),
      
      p("Passenger: passenger / Passenger123"),
      
      p("Officer: officer / Officer123"),
      
      tags$hr(),
      
      h5("Create Passenger Account"),
      
      textInput("new_username", "New Username"),
      
      passwordInput("new_password", "New Password"),
      
      actionButton(
        "register",
        "Register"
      ),
      
      tags$hr(),
      
      uiOutput("account_status")
    ),
    
    mainPanel(
      uiOutput("main_content")
    )
    
  )
)

#STEP 7: Server logic

server <- function(input, output, session) {
  
  current_user <- reactiveVal(NULL)
  
#Current login status
  
  output$account_status <- renderUI({
    
    user <- current_user()
    
    if (is.null(user)) {
      return(tags$p(
        "You are not logged in.",
        style = "color:gray;"
      ))
    }
    
    tagList(
      tags$p(
        paste("Logged in as:", user$username),
        style = "font-weight:bold;color:green;"
      ),
      tags$p(
        paste("Role:", user$role)
      )
    )
    
  })
  
#Registration
  
  observeEvent(input$register, {
    
    username <- trimws(input$new_username)
    password <- input$new_password
    
    if (
      !nzchar(username) ||
      !nzchar(password)
    ) {
      showNotification(
        "Enter both username and password.",
        type = "error"
      )
      return()
    }
    
    if (nchar(password) < 8) {
      showNotification(
        "Password must contain at least 8 characters.",
        type = "error"
      )
      return()
    }
    
    users <- read_data(users_file)
    
    if (
      tolower(username) %in%
      tolower(users$username)
    ) {
      showNotification(
        "Username already exists.",
        type = "error"
      )
      return()
    }
    
    new_user <- data.frame(
      username = username,
      password = password,
      role = "Passenger",
      stringsAsFactors = FALSE
    )
    
    users <- rbind(users, new_user)
    
    save_data(users, users_file)
    
    showNotification(
      "Registration successful! Please log in.",
      type = "message"
    )
    
    updateTextInput(
      session,
      "username",
      value = username
    )
    
    updateTextInput(
      session,
      "new_username",
      value = ""
    )
    
    updateTextInput(
      session,
      "new_password",
      value = ""
    )
    
  })
  
  #Login
  
  observeEvent(input$login, {
    
    users <- read_data(users_file)
    
    username <- trimws(input$username)
    password <- input$password
    
    matching_user <- users[
      users$username == username &
        users$password == password,
      ,
      drop = FALSE
    ]
    
    if (nrow(matching_user) == 1) {
      
      current_user(
        list(
          username = matching_user$username[1],
          role = matching_user$role[1]
        )
      )
      
      showNotification(
        "Login successful!",
        type = "message"
      )
      
    } else {
      
      showNotification(
        "Invalid username or password.",
        type = "error"
      )
    }
    
  })
  
  #Logout
  
  observeEvent(input$logout, {
    
    current_user(NULL)
    
    showNotification(
      "Logged out successfully.",
      type = "message"
    )
    
  })
  
#Main application interface
  
  output$main_content <- renderUI({
    
    user <- current_user()
    
    if (is.null(user)) {
      return(
        wellPanel(
          h2("Welcome to BusConnect!"),
          
          p(
            "Track buses, report delays, and communicate ",
            "with transport officers."
          ),
          
          h4("What you can do"),
          
          tags$ul(
            tags$li("View bus routes and status."),
            tags$li("Share bus service updates."),
            tags$li("Report missing or delayed buses."),
            tags$li("View officer responses."),
            tags$li("Generate a QR code for bus information.")
          ),
          
          p(
            "Log in using the account panel to continue."
          )
        )
      )
    }
    
    if (user$role == "Officer") {
      
      tabsetPanel(
        tabPanel(
          "Bus Management",
          br(),
          h3("Bus Information"),
          DTOutput("officer_buses"),
          br(),
          h4("Update Bus Status"),
          selectInput(
            "update_bus",
            "Select Bus",
            choices = NULL
          ),
          selectInput(
            "new_status",
            "New Status",
            choices = c(
              "Running",
              "Delayed",
              "Not Operating",
              "Arrived"
            )
          ),
          actionButton(
            "update_status",
            "Update Status",
            class = "btn-primary"
          )
        ),
        
        tabPanel(
          "Passenger Reports",
          br(),
          DTOutput("officer_reports"),
          br(),
          h4("Respond to Report"),
          numericInput(
            "response_id",
            "Report ID",
            value = 1,
            min = 1,
            step = 1
          ),
          textAreaInput(
            "officer_message",
            "Officer Response",
            rows = 3
          ),
          selectInput(
            "response_status",
            "Report Status",
            choices = c(
              "Under Review",
              "Responded",
              "Resolved"
            )
          ),
          actionButton(
            "send_response",
            "Submit Response",
            class = "btn-success"
          )
        ),
        
        tabPanel(
          "Community",
          br(),
          DTOutput("officer_posts")
        )
      )
      
    } else {
      
      tabsetPanel(
        tabPanel(
          "Home",
          br(),
          h3("Welcome, Passenger!"),
          p("Check bus status and share service updates."),
          DTOutput("passenger_buses")
        ),
        
        tabPanel(
          "Bus Tracking",
          br(),
          selectInput(
            "track_bus",
            "Select Bus",
            choices = NULL
          ),
          leafletOutput(
            "bus_map",
            height = "450px"
          ),
          br(),
          uiOutput("tracking_details")
        ),
        
        tabPanel(
          "Community",
          br(),
          h3("Passenger Community"),
          p("Share updates with passengers using the same route."),
          selectInput(
            "post_route",
            "Select Route",
            choices = NULL
          ),
          textAreaInput(
            "post_message",
            "Your Message",
            placeholder = "Example: Route 21 bus has not arrived.",
            rows = 3
          ),
          actionButton(
            "submit_post",
            "Publish Post",
            class = "btn-primary"
          ),
          br(),
          br(),
          DTOutput("passenger_posts")
        ),
        
        tabPanel(
          "Report Issue",
          br(),
          h3("Report a Bus Problem"),
          selectInput(
            "report_route",
            "Route Number",
            choices = NULL
          ),
          textInput(
            "bus_stop",
            "Bus Stop"
          ),
          selectInput(
            "issue_type",
            "Issue Type",
            choices = c(
              "Bus Delayed",
              "Bus Not Arrived",
              "Bus Skipped Stop",
              "Overcrowding",
              "Route Information",
              "Other"
            )
          ),
          textAreaInput(
            "description",
            "Describe the Problem",
            rows = 4
          ),
          actionButton(
            "submit_report",
            "Submit Report",
            class = "btn-danger"
          ),
          br(),
          br(),
          h4("My Reports"),
          DTOutput("my_reports")
        ),
        
        tabPanel(
          "QR Code",
          br(),
          h3("Bus Information QR Code"),
          selectInput(
            "qr_bus",
            "Select Bus",
            choices = NULL
          ),
          actionButton(
            "generate_qr",
            "Generate QR Code"
          ),
          br(),
          br(),
          uiOutput("qr_output")
        )
      )
    }
    
  })
  
#Refresh bus information
  
  bus_data <- reactive({
    read_data(buses_file)
  })
  
  observe({
    
    buses <- bus_data()
    
    if (nrow(buses) > 0) {
      
      bus_choices <- setNames(
        buses$bus_id,
        paste(
          buses$bus_number,
          "- Route",
          buses$route
        )
      )
      
      updateSelectInput(
        session,
        "track_bus",
        choices = bus_choices
      )
      
      updateSelectInput(
        session,
        "qr_bus",
        choices = bus_choices
      )
      
      updateSelectInput(
        session,
        "update_bus",
        choices = bus_choices
      )
      
      updateSelectInput(
        session,
        "post_route",
        choices = unique(buses$route)
      )
      
      updateSelectInput(
        session,
        "report_route",
        choices = unique(buses$route)
      )
    }
    
  })
  
#Passenger bus table
  
  output$passenger_buses <- renderDT({
    
    req(
      !is.null(current_user()),
      current_user()$role == "Passenger"
    )
    
    buses <- bus_data()
    
    datatable(
      buses[
        ,
        c(
          "bus_number",
          "route",
          "start_point",
          "end_point",
          "next_stop",
          "scheduled_time",
          "status"
        )
      ],
      rownames = FALSE,
      options = list(pageLength = 5)
    )
    
  })
  
#Officer bus table
  
  output$officer_buses <- renderDT({
    
    req(
      !is.null(current_user()),
      current_user()$role == "Officer"
    )
    
    datatable(
      bus_data(),
      rownames = FALSE,
      options = list(pageLength = 5)
    )
    
  })
  
#Update bus status
  
  observeEvent(input$update_status, {
    
    req(
      !is.null(current_user()),
      current_user()$role == "Officer"
    )
    
    buses <- read_data(buses_file)
    
    index <- which(
      buses$bus_id == input$update_bus
    )
    
    if (length(index) != 1) {
      showNotification(
        "Select a valid bus.",
        type = "error"
      )
      return()
    }
    
    buses$status[index] <- input$new_status
    
    save_data(buses, buses_file)
    
    showNotification(
      "Bus status updated successfully."
    )
    
  })
  
#Tracking map
  
  output$bus_map <- renderLeaflet({
    
    req(
      !is.null(current_user()),
      current_user()$role == "Passenger"
    )
    
    buses <- bus_data()
    
    leaflet(buses) |>
      addTiles() |>
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 8,
        popup = ~paste0(
          "<b>Bus:</b> ", bus_number,
          "<br><b>Route:</b> ", route,
          "<br><b>Status:</b> ", status
        )
      )
    
  })
  
#Tracking details
  
  output$tracking_details <- renderUI({
    
    req(input$track_bus)
    
    buses <- bus_data()
    
    selected <- buses[
      buses$bus_id == input$track_bus,
      ,
      drop = FALSE
    ]
    
    req(nrow(selected) == 1)
    
    tagList(
      h4(selected$bus_number[1]),
      p(paste("Route:", selected$route[1])),
      p(paste("Next stop:", selected$next_stop[1])),
      p(paste("Scheduled time:", selected$scheduled_time[1])),
      p(paste("Current status:", selected$status[1])),
      p(
        "Map locations are demonstration data, not verified live GPS."
      )
    )
    
  })
  
#Publish a community post
  
  observeEvent(input$submit_post, {
    
    req(
      !is.null(current_user()),
      current_user()$role == "Passenger"
    )
    
    message <- trimws(input$post_message)
    
    if (!nzchar(message)) {
      showNotification(
        "Enter a message before publishing.",
        type = "error"
      )
      return()
    }
    
    posts <- read_data(posts_file)
    
    next_id <- if (nrow(posts) == 0) {
      1
    } else {
      max(posts$post_id) + 1
    }
    
    new_post <- data.frame(
      post_id = next_id,
      username = current_user()$username,
      route = input$post_route,
      message = message,
      timestamp = format(
        Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      ),
      stringsAsFactors = FALSE
    )
    
    posts <- rbind(posts, new_post)
    
    save_data(posts, posts_file)
    
    updateTextAreaInput(
      session,
      "post_message",
      value = ""
    )
    
    showNotification(
      "Your post has been published."
    )
    
  })
  
#Passenger community table
  
  output$passenger_posts <- renderDT({
    
    req(
      !is.null(current_user()),
      current_user()$role == "Passenger"
    )
    
    posts <- read_data(posts_file)
    
    if (nrow(posts) > 0 && !is.null(input$post_route)) {
      posts <- posts[
        posts$route == input$post_route,
        ,
        drop = FALSE
      ]
    }
    
    datatable(
      posts,
      rownames = FALSE,
      options = list(
        pageLength = 5,
        order = list(list(0, "desc"))
      )
    )
    
  })
  
  output$officer_posts <- renderDT({
    
    req(
      !is.null(current_user()),
      current_user()$role == "Officer"
    )
    
    datatable(
      read_data(posts_file),
      rownames = FALSE,
      options = list(pageLength = 5)
    )
    
  })
  
#Submit a passenger report
  
  observeEvent(input$submit_report, {
    
    req(
      !is.null(current_user()),
      current_user()$role == "Passenger"
    )
    
    description <- trimws(input$description)
    stop_name <- trimws(input$bus_stop)
    
    if (
      !nzchar(description) ||
      !nzchar(stop_name)
    ) {
      showNotification(
        "Enter the bus stop and problem description.",
        type = "error"
      )
      return()
    }
    
    reports <- read_data(reports_file)
    
    next_id <- if (nrow(reports) == 0) {
      1
    } else {
      max(reports$report_id) + 1
    }
    
    new_report <- data.frame(
      report_id = next_id,
      username = current_user()$username,
      route = input$report_route,
      bus_stop = stop_name,
      issue = input$issue_type,
      description = description,
      status = "Submitted",
      officer_response = "",
      timestamp = format(
        Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      ),
      stringsAsFactors = FALSE
    )
    
    reports <- rbind(reports, new_report)
    
    save_data(reports, reports_file)
    
    updateTextAreaInput(
      session,
      "description",
      value = ""
    )
    
    showNotification(
      paste("Report submitted. Report ID:", next_id)
    )
    
  })
  
#Show current passenger's reports

output$my_reports <- renderDT({

req(
  !is.null(current_user()),
  current_user()$role == "Passenger"
)

reports <- read_data(reports_file)

reports <- reports[
  reports$username == current_user()$username,
  ,
  drop = FALSE
]

datatable(
  reports,
  rownames = FALSE,
  options = list(pageLength = 5)
)

})

#Officer reviews reports

output$officer_reports <- renderDT({

req(
  !is.null(current_user()),
  current_user()$role == "Officer"
)

datatable(
  read_data(reports_file),
  rownames = FALSE,
  options = list(pageLength = 5)
)

})

#Officer responds to a report

observeEvent(input$send_response, {

req(
  !is.null(current_user()),
  current_user()$role == "Officer"
)

reports <- read_data(reports_file)

index <- which(
  reports$report_id == input$response_id
)

if (length(index) != 1) {
  showNotification(
    "Report ID not found.",
    type = "error"
  )
  return()
}

response <- trimws(input$officer_message)

if (!nzchar(response)) {
  showNotification(
    "Enter an officer response.",
    type = "error"
  )
  return()
}

reports$status[index] <- input$response_status

reports$officer_response[index] <- response

save_data(reports, reports_file)

updateTextAreaInput(
  session,
  "officer_message",
  value = ""
)

showNotification(
  "Officer response submitted."
)

})

#QR code generation

output$qr_output <- renderUI({

req(
  !is.null(current_user()),
  current_user()$role == "Passenger"
)

req(input$generate_qr > 0)

buses <- bus_data()

selected <- buses[
  buses$bus_id == input$qr_bus,
  ,
  drop = FALSE
]

req(nrow(selected) == 1)

# Demo QR content.
# Replace this with the actual deployed tracking URL
# when the Shiny application is hosted online.

qr_content <- paste0(
  "BusConnect | Bus: ",
  selected$bus_number[1],
  " | Route: ",
  selected$route[1],
  " | Demo Bus ID: ",
  selected$bus_id[1]
)

if (!requireNamespace("qrcode", quietly = TRUE)) {
  return(
    p("Install the qrcode package to generate QR codes.")
  )
}

qr_matrix <- qrcode::qr_code(qr_content)

output$qr_image <- renderPlot({

  plot(
    qr_matrix,
    col = c("white", "black"),
    axes = FALSE,
    xlab = "",
    ylab = "",
    asp = 1
  )

}, res = 120)

tagList(
  h4(selected$bus_number[1]),
  p(paste("Route:", selected$route[1])),
  plotOutput("qr_image", height = "300px"),
  p(
    "This demo QR contains bus details, not a live tracking link."
  )
)

})

}

#STEP 8: Run the application

shinyApp(ui = ui, server = server)