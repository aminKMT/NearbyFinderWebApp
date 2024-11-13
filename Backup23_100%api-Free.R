library(shiny)
library(httr)
library(jsonlite)
library(googleway)
library(ggmap)
library(leaflet)
library(dplyr)
library(openxlsx)
library(shinyjs)
library(DT)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)
library(ggplot2)


# 100 % API Free 
#search_query <- "chinies restaurant" # define any  keyword or name
#Addresses:
m            <- 10 # maximum number of selected/optimal place
radius       <- 6000    # Search Radius
price_range  <-  c(1,4) #numeric vector Specifying the minimum and maximum price ranges. Values range between 0 (most affordable) and 4 (most expensive).
#open_now     <-  FALSE  #logical Returns only those places that are open for business at the time the query is sent. Places that do not specify opening hours in the Google Places database will not be returned if you include this parameter in your query
# Routing parameters+:
#mode           <- "driving"
traffic_model  <- "optimistic"


#*******************************************************************************
# Define UI for application 

ui <- navbarPage(id = "navBar",theme = shinytheme("united"),"THERE",
                 # Tabset Text Frames:    
                 tags$style(HTML("
      .text-frame {
                border: 1px solid #ddd; /* Light grey border */
        padding: 20px; /* Padding inside the frame */
        border-radius: 5px; /* Rounded corners */
        max-width:  800px; /* Maximum width of the frame */
        max-height: 500px; /* Maximum height of the frame */
        overflow-y: auto; /* Enable vertical scrolling */
        margin-left: 0; /* Align to the left side */
        margin-top: 20px; /* Add some space at the top */
  
      }
    }
    ")),
                 
                 # make a font for a copyright:
                 tags$head(
                   tags$style(HTML("
      .copyright {
        position: fixed;
        left: 0;
        bottom: 0;
        margin: 10px;
        font-size: 10px; /* Small font size */
        color: #666; /* Light grey color */
      }
    "))
                 ),
                 
                 # style for "I don't have API KEY":
                 tags$head(
                   tags$style(HTML("
      .api-key-link {
        font-size: 12px; /* Small font size for the link */
        margin-top: 5px; /* Spacing above the link */
      }
    "))
                 ), 
                 #API Page
                 tabPanel("API Input",
                          icon = icon("key"), # Use a key icon for the API input tab
                          # Place the text input in the middle of the page
                          div(style = "display: flex; justify-content: center; flex-direction: column; align-items: center;",
                              textInput("api_key", "Enter your Google Places API Key:", ""),
                              actionButton("next_btn", "Next"),
                              #Not Saving Notice
                              p("The API Key will not be stored"),
                              # Link for users who don't have an API key
                              tags$a(href = "https://developers.google.com/maps/documentation/embed/get-api-key#create-api-keys", 
                                     target = "_blank", 
                                     "What is Google API Key?", 
                                     class = "api-key-link")
                              
                          ),
                          # Creating TabSetPanel in in the API_input Tab: 
                          tabsetPanel(
                            tabPanel("API Guide",
                                     # Apply the style: 
                                     div(class = "text-frame", 
                                         p(
                                           h2("How to Obtain a Google Places API Key"),
                                           p("To use Google Places API, you need to obtain a specific API key from Google Cloud Platform (GCP). Here’s how you can do it:"),
                                           h3("Create a Google Cloud Platform Account:"),
                                           p("If you don't already have a GCP account, you'll need to sign up at ", 
                                             a("Google Cloud Platform", href = "https://cloud.google.com/")),
                                           h3("Create a Project:"),
                                           p("Once logged into GCP, create a new project or select an existing one where you wish to use the Places API."),
                                           h3("Enable the Places API and other APIs:"),
                                           p("Go to the \"APIs & Services\" dashboard, then to the \"Library\" section."),
                                           p("Search for \"Places API\" and select it."),
                                           p("Click on the \"Enable\" button to enable the Places API for your project."),
                                           p("Repeat the above steps to enable the \"Directions API\" and \"Geocoding API\"."),
                                           h3("Create Credentials:"),
                                           p("After enabling the API, navigate to the \"Credentials\" page from the sidebar."),
                                           p("Click on “Create Credentials” and select “API key” from the dropdown menu."),
                                           p("Once the API key is created, you can restrict it for added security.
             For instance, you might restrict it to your web application's domain or to specific APIs (like the Places API)."),
                                           h3("Use the API Key in Your Application:"),
                                           p("With the API key generated, you can now use it in your application. Make sure to keep your API key secure and do not expose it publicly in places where it can be compromised."),
                                           p("Remember that Google Cloud Platform offers a free tier, but depending on the volume of requests and the specific Google Cloud Services you use, there could be costs associated
           with your API usage. It's a good idea to review Google's
             pricing and quota limits for the Places API to understand potential costs and manage your usage accordingly.")
                                         )
                                     )
                            ), 
                            
                            tabPanel ("About THERE", 
                                      fluidRow(
                                        
                                        column(5, div(class = "text-frame",
                                                      p(
                                                        h2("What is THERE?"),
                                                        p("Imagine you and a group of friends want to dine out and are looking for the nearest restaurant that serves
            a specific cuisine, like Indian. In this scenario, THERE can assist you in identifying potential dining spots
            and calculating the travel time for each of your friends, taking into account various factors such as mode of
            transportation (driving, public transportation), departure time, etc. In essence, THERE is a web application
            designed for optimizing location choices using Google API. It can be particularly useful in the logistics sector,
            enabling companies to pinpoint the nearest retailers or supermarkets relative to the diverse locations of their
            warehouses or manufacturing plants"),
                                                        h2("How does it work?"),
                                                        p("1. Insert your Google API Key in the \"API Input\" tab, and press next. Please note that your API Key wont be stored."),
                                                        p("2. In the \"API Input\" tab, input your desired search term, such as Indian Cuisine or supermarket, into the \"Search\" section. 
            Note that the keywords used here should align with the Google label dataset;
            not all keywords are supported, but it is fully compatible with terms like stores, restaurants, supermarkets, shipping companies, etc." ),
                                                        p("3. Enter your addresses in the address bar and use the \"Add\" button to include them in your inputs."),
                                                        p("4. Click on \"FindSpots\" and wait for the results. You can also view the located spots on the map in the \"Map\" tab.") 
                                                      )
                                        ) # end of trh text left side 
                                        ), # end of col (8),
                                        
                                        column(7,
                                               div(style = "margin-top: 0;",
                                                   # Use tags$video to embed the video
                                                   tags$video(src = "Short_Video_THERE.mp4", 
                                                              type = "video/mp4",
                                                              controls = FALSE, # Add video controls (play, pause, etc.)
                                                              width = "792", # Specify the width of the video frame
                                                              height = "485", # Specify the height of the video frame
                                                              attributes = list(controlsList = "nodownload")
                                                   )
                                               )
                                        )# end of the left coloumn
                                      )#end of Fluid row
                            ),
                            
                            tabPanel("Terms & Conditions",
                              div(style = "padding: 20px;",
                                  h2("Terms and Conditions for THERE"),
                                  p("Welcome to THERE! By accessing and using THERE, you agree to be bound by these Terms and Conditions and our Privacy Policy. Please read them carefully."),
                                  h3("1. Google API Key"),
                                  p("Users of THERE are required to use their own Google API Key to access certain features provided by the Google Places Library. It is the user's responsibility to obtain, manage, and secure their Google API Key. THERE is not responsible for any misuse of the Google API Key or any charges that may result from its use."),
                                  h3("2. Use of Services"),
                                  p("THERE allows users to use their own Google API key for conducting location-based operations. Users must not use THERE for any unauthorized or illegal purpose. Users must comply with all applicable laws and regulations and the terms of any third-party services, including the Google Maps Platform Terms of Service."),
                                  h3("3. User Responsibilities"),
                                  p("Users are responsible for their own Google API Key and for ensuring that their use of THERE complies with all applicable laws and terms of service."),
                                  h3("4. Intellectual Property"),
                                  p("All content included in THERE, such as text, graphics, logos, and software, is the property of THERE or its content suppliers and is protected by copyright and other intellectual property laws."),
                                  h3("5. Disclaimer of Warranties"),
                                  p("THERE is provided 'as is' and without warranties of any kind, either express or implied. THERE does not guarantee that THERE will be uninterrupted or error-free."),
                                  h3("6. Limitation of Liability"),
                                  p("THERE will not be liable for any damages or losses arising from your use of THERE or your inability to use it."),
                                  h3("7. Changes to Terms"),
                                  p("THERE reserves the right to modify these Terms and Conditions at any time. Your continued use of THERE after any changes indicates your acceptance of the new Terms."),
                                  p("If you have any questions about these Terms, please contact us at THEREKMT@gmail.com.")

                            )
                          )
                       )
                     ),
                 
                 tabPanel("Home",
                          textInput("Search", label = HTML(paste("Search",
                                                                 icon("search"))), "e.g. Chinese restaurant"),
                          
                          
                          # Embedded CSS for adcvanced search 
                          tags$style(HTML("
                            .advanced-search {
                             position: absolute;
                             top: 8px;
                             right: 30px;
                            } ")),
                          
                          # Embedded CSS for add button  search 
                          tags$style(HTML("
                            .addbutton {
                             position: absolute;
                             top: 27px;
                             right: 130px;
                            } ")),
                          
                          # Embedded CSS for remove button  search 
                          tags$style(HTML("
                            .rembutton {
                             position: absolute;
                             top: 27px;
                             right: 80px;
                            } ")),
                          # Embedded CSS for address bar  search 
                          tags$style(HTML("
                             .addbar {
                             position: absolute;
                             top: 14px;
                             right:100px;
                             left: 6px;
                            } ")),
                          
                          
                          # Sidebar with controls to add new addresses
                          sidebarLayout(
                            sidebarPanel(
                              tags$div(
                                class = "advanced-search",
                                # title = "Advanced Search",                      
                                br(),
                                dropdown(
                                  
                                  tags$h3("Settings"),
                                  
                                  # Date input with default as current date
                                  dateInput("departure_date", label = HTML(paste("Departure Date", 
                                                                                 icon("calendar"))),"Select Departure Date:", 
                                            value = Sys.Date()),
                                  
                                  # Time input with default as current time
                                  textInput("departure_time", label = HTML(paste("Departure Time (HH:MM:SS)",
                                                                                 icon("clock")))
                                            , value = format(Sys.time(), "%H:%M:%S")), 
                                  
                                  switchInput(inputId = "Id015",label = "Open Now", labelWidth = "80px"),
                                  
                                  # Transportation Buttons: 
                                  
                                  # Radio buttons for transportation modes
                                  radioGroupButtons(
                                    inputId = "T_mode",
                                    #label = "Choose a transportation mode:",
                                    choices = c(`<i class='fa fa-car'></i>` = "driving", 
                                                `<i class='fa fa-walking'></i>` = "walking", 
                                                `<i class='fa fa-bicycle'></i>` = "bicycling",
                                                `<i class='fa fa-bus'></i>` = "transit"),
                                    justified = TRUE
                                  ),
                                  
                                  
                                  style = "unite", icon = icon("gear"),
                                  status = "danger", width = "300px", height= "400px",
                                  animate = animateOptions(
                                    enter = animations$fading_entrances$fadeInLeftBig,
                                    exit = animations$fading_exits$fadeOutRightBig
                                  )
                                ),
                              ),
                              
                              tags$div(
                                class= "addbutton", 
                                title = "Add the address",
                                actionButton("addAddress", label = HTML("&plus;"), style = "width:38px; height:38px;
                                                                                    font-size:16px; 
                                                                                    margin-left:0px;
                                                                                    margin-right:20px;"
                                )),
                              tags$div(
                                class= "rembutton",
                                title = "Remove the address",
                                actionButton("removeAddress", label = HTML("&minus;"), style = 
                                               "width:38px; height:38px; font-size:16px; margin-left:20px;margin-right:30px;
                                               margin-bottom:30px;")
                              ),
                              
                              
                              tags$div(
                                class = "addbar", 
                                uiOutput("address_input_ui")
                              ),
                              # Define the style of the address input : 
                              
                              tags$head(
                                tags$style(HTML("
    .addbar {
      margin-top: 0 px; /* Adjust location */
      text-align: center; /* Center align the div content */
    }

    #my_address {
      width: 90%; /* Flexible width */  
      max-width: 500px; /* Maximum width to avoid being too wide */
      height: 35px; /* Fixed height */
      margin-right:  10px; /* Right margin to create space between input and right-side elements */
      margin-left:   20px; /* Right margin to create space between input and right-side elements */
      box-sizing: border-box; /* Include padding and border in the element's total width and height */
    }
  "))
                              ),
                              
                              # max-width: 500px; /* Maximum width to avoid being too wide */                    
                              #margin-right:  0px; /* Right margin to create space between input and right-side elements */
                              
                              div(id= "address1", style ="margin-bottom:10px;margin-top:50px; margin-left:1px;"),
                              
                              # Submit Button
                              tags$div(
                                class= "submit",
                                title= "Start searching",
                                actionButton("submit", "Find Spots", icon= icon("globe"),
                                             style =paste0("margin-bottom:30px;margin-top:10px; margin-left:1px;")
                                )),
                              
                              useShinyjs(),
                              numericInput("numAddresses", "Number of Addresses", 1, min = 0),
                              
                              textOutput(outputId = "full_address"),
                              leafletOutput("my_map") 
                              
                            ), # end  of side panel 
                            
                            # Show the results in the main panel
                            mainPanel(
                              uiOutput("result"),
                            ),
                          ), #this is for sidbarlayout() 
                          
                 ),
                 tabPanel("Map",
                          sidebarLayout(
                            
                            sidebarPanel(
                              
                              actionButton("submit_map", "Spots List", icon= icon("list-alt"))
                            ),
                            mainPanel(
                              leafletOutput("map", width = "100%", height = "800px")
                            )
                          )
                 ),
                 tabPanel("Contact",
                          h2("You can submit your feedback and comments via the following email:"),
                          h1("therekmt@gmail.com"),
                          
                          #Copyright Text
                          absolutePanel(
                            class = "copyright",
                            "Copyright © 2024 THERE----therekmt@gmail.com"
                          )
                          
                 )
     )


#*****************************************************************************************************************

# Define server logic
server <- function(input, output, session) {
  
  
  
  # Next Button to HOME Page 
  observeEvent(input$next_btn, {
    if (input$api_key == "") {
      # Show modal dialog if API key is empty
      showModal(modalDialog(
        title = "Missing API Key",
        "Please insert the API key to proceed.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    } else {
      
      # insert the api_key#########
      api_key <- input$api_key
      set_key(key = api_key )
      register_google(api_key)
      ##############################
      
      output$address_input_ui <- renderUI({
        HTML(paste0("
        <div class='addbar'>
          <input id='my_address' type='text' placeholder='Enter the Address; e.g. 3236 Brown Ave.'>
          <script>
            function initAutocomplete() {
              var autocomplete = new google.maps.places.Autocomplete(document.getElementById('my_address'), {types: ['geocode']});
              autocomplete.addListener('place_changed', function() {
                var place = autocomplete.getPlace();
                if (!place.geometry) return;
                Shiny.setInputValue('my_address', place.formatted_address); // Update Shiny input value
              });
            }
          </script>
          <script src='https://maps.googleapis.com/maps/api/js?key=", api_key, "&libraries=places&callback=initAutocomplete' async defer></script>
        </div>
      "))
      })
      
      # Switch to the HOME tab if API key is not empty
      updateNavbarPage(session, "navBar", selected = "Home")
    }
  })
  
  #Add finder++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  my_address <- reactive({
    if(!is.null(input$jsValueAddressNumber)){
      if(length(grep(pattern = input$jsValueAddressNumber, x = input$jsValuePretty ))==0){
        final_address<- c(input$jsValueAddressNumber, input$jsValuePretty)
      } else{
        final_address<- input$jsValuePretty
      }
      final_address
    }
  })
  mapstyle<- providers$OpenStreetMap.HOT
  
  output$my_map <- renderLeaflet({
    
    
    my_map <- leaflet() %>%
      addProviderTiles(mapstyle) 
    #addTiles()  # Add default OpenStreetMap map tiles
    if(!is.null(input$my_address) && input$my_address!= "") {
      navIcon <- icons(
        iconUrl = "Nav2.png",  # Path to your icon
        iconWidth = 25,                  # Adjust as necessary based on the image's dimensions
        iconHeight = 40,                 # Adjust based on your icon's dimensions
        iconAnchorX = 12,                # Usually half of the width
        iconAnchorY = 40                 # Usually the height value
      )
      
      # Add marker to the map with the navigator icon
      my_map <- my_map %>% addMarkers(
        data = geocode(input$my_address), 
        ~lon, 
        ~lat,
        icon = navIcon,
        popup = paste0(input$my_address)
        # ... other options ...
      )
    }
    return(my_map)
  })
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  observeEvent(input$Ad_Search, {
    # Update navbarPage to select the "Tab 3"
    updateNavbarPage(session, "navBar", selected = "Map")
  })
  
  observeEvent(input$submit_map, {
    # Update navbarPage to select the "Tab 3"
    updateNavbarPage(session, "navBar", selected = "Home")
  })
  
  
  
  # Hide numAddresses input
  shinyjs::hide("numAddresses")
  
  # Adding Action
  observeEvent(input$addAddress, {
    my_address <- input$my_address
    shiny::validate(
      need(my_address, "Address not available")
    )
    
    insertUI(
      
      selector = paste0("#address", input$numAddresses),
      where = "afterEnd",
      ui = textInput(paste0("address", input$numAddresses + 1),"" , value = input$my_address)
    )
    updateNumericInput(session, "numAddresses", value = input$numAddresses + 1)
    updateTextInput(session, "my_address", value = "")
    
  }) 
  
  # Removing Action
  observeEvent(input$removeAddress, {
    
    if (input$numAddresses > 1) {
      removeUI(
        selector = paste0("#address", input$numAddresses),
      )
      updateNumericInput(session, "numAddresses", value = input$numAddresses - 1)
    }
  })
  
  output$full_address <- renderText({
    if(!is.null(input$my_address)){
      input$my_address
    }
  })
  
  
  observeEvent(input$submit, {
    
    if(input$submit > 0 ){
      
      
      showModal(modalDialog(
        title = "Looking for spots",
        "Processing, this might take a few minutes...",
        tags$div(class="progress",
                 tags$div(class="progress-bar progress-bar-striped active", role="progressbar",
                          `aria-valuenow`="100", `aria-valuemin`="0", `aria-valuemax`="100", style="width: 100%")
        ),
        easyClose = FALSE,
        footer = NULL
      ))
      
      # Get all addresses and remove any that are empty or NA
      
      addresses <- sapply(2:(input$numAddresses), function(i) input[[paste0("address", i)]])
      print(input$numAddresses)
      
      
      if(all(sapply(addresses, length)) == 0) {
        output$result <- renderUI({
          verbatimTextOutput("NoInput") 
        }) 
        output$NoInput <- renderText({
          "Please insert an address"
        })
        
      } else {
        
        addresses <- addresses[addresses != "" & !is.na(addresses)]
        print(addresses)
        
        
        
        # Update numAddresses to reflect the new number of addresses
        #updateNumericInput(session, "numAddresses", value = length(addresses))
        customers <- geocode(addresses)
        
        print(customers)
        
        
        #Define the potential store locations:
        # Find the nearest location (store)
        
        print(input$Id015)
        
        
        
        nearest_place <- function(coord) {
          result <- google_places(#search_string  = input$Search,
            location = coord,
            keyword = input$Search,
            #rankby = "distance",
            radius = radius,
            key = input$api_key, 
            open_now = input$Id015,
            price_range = price_range) 
          
          return(result$results)
          
        }
        
        sl <- lapply(1:nrow(customers), function(i) {
          nearest_place(c(customers$lat[i], customers$lon[i]))
        })
        print(length(sl[[1]]))
        
        
        if(all(sapply(sl, length) == 0)) {
          output$result <- renderUI({
            verbatimTextOutput("Error") 
          }) 
          output$Error <- renderText({
            "Sorry, we can not find any location."
          })
          
        } else {# eliminate the empty sub-lists 
          not_zero <- sapply(sl, function(x) length(x) != 0) 
          sl <- sl[not_zero]
          
          fields <- c("business_status", "geometry", "icon", "icon_background_color",
                      "icon_mask_base_uri", "name", "opening_hours", "photos",               
                      "place_id", "plus_code", "price_level", "rating",                
                      "reference", "scope", "types", "user_ratings_total" ,   
                      "vicinity")
          
          valid_sl <- list()
          
          for(i in seq_along(sl)) {
            sublist <- sl[[i]]
            sublist_names <- names(sublist)
            
            if(any(sublist$business_status == "CLOSED_TEMPORARILY")) {
              next
            }
            
            for(j in seq_along(fields)) {
              field <- fields[j]
              
              if(!(field %in% sublist_names)) {
                sublist <- append(sublist, list(NA), after = j-1)
                names(sublist)[j] <- field
              }
            }
            
            # Reorder the fields in sublist to match the order in fields
            sublist <- sublist[match(fields, names(sublist))]
            
            valid_sl[[length(valid_sl)+1]] <- sublist
          }
          
          sl <- valid_sl
          
          print(paste("length(sl)",length(sl)))
          print(paste("length(sl[[1]]) =", length(sl[[1]])) ) 
          
          # ###########again if the sl lenghth is 0 we should get an error:
          if (length(sl) == 0){
            output$result <- renderUI({
              verbatimTextOutput("Error")
            }) 
            output$Error <- renderText({
              "Sorry, we can not find any operational location."
            })
          } else {
            #######################
            output$result <-  renderUI({
              
              
            })
            
            # Extract the first row from each data frame and join them vertically
            sl_cor <- bind_rows(sapply(1:length(sl), function(i) sl[[i]][[2]][1], simplify = FALSE))
            sl_name <- bind_rows(sapply(1:length(sl), function(i) data.frame(sl[[i]][[6]]), simplify = FALSE))
            
            #using the above function for conversion: 
            sl_PriceLevel <- bind_rows(sapply(1:length(sl), function(i) data.frame(sl[[i]][[11]]), simplify = FALSE))
            sl_rating <- bind_rows(sapply(1:length(sl), function(i) data.frame(sl[[i]][[12]]), simplify = FALSE))
            sl_type <- bind_rows(sapply(1:length(sl), function(i) data.frame(sl[[i]][15]), simplify = FALSE))
            
            # Function to create a Google Maps URL using a Place ID
            place_id_to_url <- function(place_id) {
              base_url <- "https://www.google.com/maps/place/?q=place_id:"
              return(paste0(base_url, place_id))
            }
            Gmap_links <- list()
            for (i in 1:length(sl)) {
              Gmap_links[[i]] <- place_id_to_url(sl[[i]][[13]])
            }
            Gmap_links <- data.frame(Links = unlist(Gmap_links))
            sl_Number_Raters <- bind_rows(sapply(1:length(sl), function(i) data.frame(sl[[i]][16]), simplify = FALSE))
            sl_address <- bind_rows(sapply(1:length(sl), function(i) data.frame(sl[[i]][17]), simplify = FALSE))
            
            
            potential_store_locations <-  data.frame(cbind(id            =  1:nrow(sl_cor),
                                                           lat           =  sl_cor$location$lat, 
                                                           lon           =  sl_cor$location$lng,
                                                           Location_name =  sl_name$sl..i....6..,
                                                           Address       =  sl_address$vicinity,
                                                           Price_Level   =  sl_PriceLevel$sl..i....11..,
                                                           Rating        =  sl_rating$sl..i....12..,
                                                           Raters_Number =  sl_Number_Raters$user_ratings_total,
                                                           Gmap_link     =  Gmap_links$Links,
                                                           Info          =  sl_type))
            #Creating departure time: 
            if (input$departure_date == Sys.Date()){
              
              timestamp <- "now"
            } else {
              datetime_str <- paste(input$departure_date, input$departure_time)
              
              # Convert the combined datetime string to a POSIXct object
              datetime_posix <- as.POSIXct(datetime_str, format="%Y-%m-%d %H:%M:%S", tz="UTC")
              
              # Convert POSIXct object to a numeric timestamp
              timestamp <- as.numeric(datetime_posix)
            }
            
            # Create a function to compute the distance matrix using the googleway package:
            T_mode <- input$T_mode
            print(T_mode)
            get_distance_matrix <- function(customers, potential_store_locations) {
              distance_matrix <- matrix(nrow = nrow(customers), ncol = nrow(potential_store_locations))
              
              for (i in 1:nrow(customers)) {
                for (j in 1:nrow(potential_store_locations)) {
                  route <- google_directions(
                    origin = c(customers$lat[i], customers$lon[i]),
                    destination = c(potential_store_locations$lat[j], potential_store_locations$lon[j]),
                    key = input$api_key,
                    mode = T_mode,
                    departure_time = timestamp,
                    traffic_model = traffic_model
                  )
                  distance_matrix[i,j] <- route$routes$legs[[1]]$duration$value #it is for travel time in second; for distance: route$routes$legs[[1]][1, 1][2][[1]]
                }
              }
              
              return(distance_matrix)
            }
            
            #Compute the distance matrix:
            distance_matrix <- get_distance_matrix(customers, potential_store_locations)
            
            
            #Now, perform location allocation analysis:
            # Custom function to find the indices of the 5 smallest elements
            
            find_min_indices <- function(x, m) {
              sorted_indices <- order(x)
              min_indices <- sorted_indices[1:m]
              return(min_indices)
            }
            
            # Apply the custom function to each row of the distance_matrix
            min_distance_indices <- apply(distance_matrix, 1, find_min_indices, m )
            min_distance_indices[is.na(min_distance_indices)] <- 0
            store_location_counts <- table(min_distance_indices) # Indicates the number of customers for whom each restaurant is among their five closest restaurants.
            
            Optimal_store_location_counts<- sort(store_location_counts,decreasing = TRUE)[1:m]
            
            
            
            optimal_store_location_ids <-data.frame(Optimal_store_location_counts)$min_distance_indices
            
            
            optimal_store_locations <- potential_store_locations[potential_store_locations$id %in% optimal_store_location_ids, ]
            stors<- optimal_store_locations
            # just show specific colouns:
            optimal_store_locations  <- optimal_store_locations[c(4, 5, 7, 8, 9)]
            
            optimal_store_locations$url <- paste0('<a href="', optimal_store_locations$Gmap_link, '">Google Map</a>')
            
            #duration matrix:++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            duration_matrix <- get_distance_matrix(customers, stors)
            duration_matrix <- t(duration_matrix)
            duration_matrix<- data.frame(duration_matrix)
            
            # Function to convert seconds to "hh:mm" format
            convert_seconds_to_hhmm <- function(seconds) {
              sprintf("%02d:%02d", seconds %/% 3600, (seconds %% 3600) %/% 60)
            }
            # Apply the function to each column in the dataframe
            duration_matrix <- as.data.frame(lapply(duration_matrix, convert_seconds_to_hhmm))
            
            duration_matrix<- cbind(stors$Location_name, duration_matrix)
            addresses<-data.frame(addresses)
            colnames(duration_matrix)[1] <- "location name"
            colnames(duration_matrix)[-1] <- addresses$addresses
            
            
            
            
            
            output$result <- renderUI({
              tagList(
                DT::dataTableOutput("optimal_locations"),
                h3("Trip Duration (HH:MM)"),
                DT::dataTableOutput("duration_matrix")
              )
            })
            
            
            output$duration_matrix <- DT::renderDataTable({
              DT::datatable(duration_matrix, escape = FALSE,rownames = FALSE, 
                            extensions = c('FixedHeader', 'Buttons'), 
                            options = list(autoWidth = TRUE,
                                           dom = 'Bfrtip',
                                           buttons = c('csv', 'pdf', 'print'),
                                           pageLength = 5,
                                           fixedHeader = TRUE
                            ) )
              
            })
            
            
            
            output$optimal_locations <- DT::renderDataTable({
              DT::datatable(optimal_store_locations[c(1, 2, 3, 4, 6)], 
                            escape = FALSE,rownames = FALSE, 
                            extensions = c('FixedHeader', 'Buttons'), 
                            options = list(autoWidth = TRUE,
                                           dom = 'Bfrtip',
                                           buttons = c('csv', 'pdf', 'print'),
                                           pageLength = 5,
                                           fixedHeader = TRUE
                            ) )
              
            })
            
            output$map <- renderLeaflet({
              map <- leaflet() %>%
                addProviderTiles(mapstyle)  # Add default OpenStreetMap map tiles
              
              # Add points from Customers dataframe
              map <- map %>% addCircleMarkers(
                data = customers, 
                ~lon, 
                ~lat, 
                color = "blue",
                label = paste0(addresses$addresses)
                # ... other options ...
              )
              
              # Add points from optimal_store_locations dataframe
              map <- map %>% addCircleMarkers(
                data = stors, 
                ~lon, 
                ~lat, 
                color = "red",
                label = paste0(stors$Location_name),
                popup = paste0('<a href="', optimal_store_locations$Gmap_link, '">Google Map</a>')
                # ... other options ...
              )
              # Add legend
              map <- map %>% addLegend(
                position = "bottomright",
                colors = c("blue", "red"),
                labels = c("Input Addresses", "Discovered Spots"),
                title = "Legend"
              )
              
              return(map)
            })
            
          }
          
        }
      }
      
      removeModal()
      
    } 
    
  },ignoreNULL = FALSE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

