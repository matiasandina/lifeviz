library(shiny)
library(tidyverse)
library(lubridate)
library(emo)
library(ggtext)
library(rvest)
library(shinyWidgets)

# For recording purposes
# data was cached into the data directory using 

#jis %>%
#  select(emoji, name) %>% 
#  mutate(url = map_chr(emoji,
#                       slowly(~emoji_to_link(.x), 
#                              rate_delay(0.5))),
#         filename = file.path("data", paste0(name, '.png'))) -> pru
#  
# takes A LONG TIME!
# stored in ./data/emoji_urls.csv
#map2(.x = pru$url, .y=pru$filename,
#     function(.x,.y) safely(download.file(.x, .y)))


# initialize things  ####

# things with date 
my_pb <- function(frac, char = "█", space_char="—", round_digits = 2){
  
  frac <- as.numeric(frac)
  if (is.na(frac)) {stop("frac must be coercible to numeric")}
  if (!dplyr::between(frac, 0, 1)) {stop("frac must be between 0 and 1")}
  
  percent_value <- round(frac * 100, round_digits)
  
  completed <- rep(char, frac * 10) 
  total_left <- ifelse(length(completed)>0, 10 - length(completed), 10)
  remaining <- rep(space_char, total_left)
  
  glue::glue_collapse(c("|- ", completed, remaining, " -| ", percent_value, " %"))
} 

# helper functions to visualize emojis
# from here:
# https://www.hvitfeldt.me/blog/real-emojis-in-ggplot2/

emoji_to_link <- function(x) {
  paste0("https://emojipedia.org/emoji/",x) %>%
    read_html() %>%
    html_nodes("tr td a") %>%
    .[1] %>%
    html_attr("href") %>%
    paste0("https://emojipedia.org/", .) %>%
    read_html() %>%
    html_node('div[class="vendor-image"] img') %>%
    html_attr("src")
}

link_to_img <- function(x, size = 25) {
  paste0("<img src='", x, "' width='", size, "'/>")
}

# ui side ####

ui_panels <- sidebarLayout(
  sidebarPanel(
    # ui sidebar style ----
    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar,.js-irs-0 .irs-grid-pol {background: #2F2F30; border-color: #2F2F30}")),
    
    #tags$head(tags$style('h5 {color:red;}')),
    style = "background-color: white;", ##2F2F30
    dateInput('dob',
              label = 'Date of Birth: yyyy-mm-dd', value = NA
    ),
    sliderInput('life_slider', label = "Life in years",
                min = 0, max = 120, value = 90),
    h5(strong("Life Progress")),
    verbatimTextOutput("life_progress_text"),
    hr(),
    radioButtons("weekyear", label = h5(strong("Display Life in:")),
                 choices = list("Years" = "years", "Weeks" = "weeks"), 
                 selected = "years", inline = TRUE),
    hr(),
    h5(strong("Show")),
    checkboxInput("showpast", "Past", FALSE),
    checkboxInput("showpresent", "Present", FALSE),
    hr(),
    h5(strong("Support")),
    h6("Chip in to help keep this up and running in the cloud"),
    HTML("<a href='https://www.patreon.com/bePatron?u=25827926' 
         data-patreon-widget-type='become-patron-button'>Become a Patron!'</a>
         <script async src='https://c6.patreon.com/becomePatronButton.bundle.js'>
         </script>"),
    HTML("<br>"),
    tags$a(
      href="https://www.paypal.com/donate?token=O8HEUAOF8gn9iMLSb_iMP1_2JpxzTPcYm-B6Oj_FdctsgAtd5RYDX0FrVHNXkR6HcGuVVIbVVg1-m9Xx'", 
      tags$img(src="https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif", 
               title="")
    ),
    ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Life",
               plotOutput("lifeplot")),
      tabPanel("Activities",
               plotOutput("defaultplot"),
               fluidRow(
                column(pickerInput("emoji_render", label="Select from available emoji",
                                    multiple = FALSE, choices = jis$emoji),
                       width=5),
                column(textInput("search_term",
                                 label="Search available emojis",
                                 placeholder = "Enter keyword of the thing to plot (optional)"),
                       width=5),
               ),
               textInput("thing_name",
                         label="Custom Name",
                         placeholder = "Enter custom name for the thing to plot (optional)"),
               
               fluidRow(
                 column(
                   numericInput("frequency",
                                label="Times",
                                min = 1,
                                value = 1), width = 3),
                 column(
                   pickerInput("time_frequency",
                               label="Enter frequency unit",
                               multiple = FALSE, choices = c("per week", "per year")),
                   width=3),
                 column(
                   actionButton("newplot",
                                icon = icon("bar-chart-o"), #emo::ji("bar_chart"),
                                label="Make New Plot!"),
                   width=3, style = "margin-top: 25px;"
                 )
               ),
               plotOutput("actplot",height = "auto"))
      
    )
    
  )
)

style='
    background-color: gray; /* Changing background color */
    font-weight: bold; /* Making font bold */
    border-radius: 20px; /* Making border radius */
    width: auto; /* Making auto-sizable width */
    height: auto; /* Making auto-sizable height */
    padding: 5px 30px 5px 30px; /* Making space around letters */
    font-size: 18px; /* Changing font size */
                '

# ui ensamble -----
ui <- fluidPage(
  titlePanel("LifeViz — visualize the events in your life",
             windowTitle = "LifeViz by Matias Andina"),
  
  # p("This app was made to visualize the events in your life. 
  #   ", 
  #   style = '
  #   background-color: #F0E969; /* Changing background color */
  #   border-radius: 20px; /* Making border radius */
  #   width: auto; /* Making auto-sizable width */
  #   height: auto; /* Making auto-sizable height */
  #   padding: 5px 30px 5px 30px; /* Making space around letters */
  #   font-size: 18px; /* Changing font size */
  #               '
  # ),
  
  p(strong("Life Tab:"),
    "Shows the years/weeks in your life. Use slider to increment/decrease lifespan."
    ),

  p(strong("Activities Tab:"),
    "Shows the number of activities/events left in your life. Use menu to select a representative emoji and slider to increase/decrease lifespan."
    ),
  
  #tags$html(class="newclass",
  #style='
   # background-color: gray; /* Changing background color */
   # font-weight: bold; /* Making font bold */
   # border-radius: 20px; /* Making border radius */
   # width: auto; /* Making auto-sizable width */
   # height: auto; /* Making auto-sizable height */
  #  padding: 5px 30px 5px 30px; /* Making space around letters */
  #  font-size: 18px; /* Changing font size */
  #              '),
  ui_panels,
  hr(),
  tags$footer(
    p('Made with 🖤 by Matias Andina (',
      a(href = 'https://twitter.com/NeuroMLA',
        '@NeuroMLA',
        .noWS = "outside"), ')',
      .noWS = c("after-begin", "before-end")),   
    p('Inspiration: waitbutwhy (',
      a(href = 'https://waitbutwhy.com/2015/12/the-tail-end.html',
        'The tail end',
        .noWS = "outside"), ')',
      .noWS = c("after-begin", "before-end")),
    title="", align = "right", style = "
        opacity: 1; // Leave this as 1
        background-color: rgba(0,0,0,0.6);
        position:sticky;
        bottom:0;
        width:100%;
        height:auto; /* Height of the footer */
        color: gray;
        padding: 10px;
        z-index: 1000;"
  )
)


# server side ####
server <- function(input, output, session) {
  
  # if FALSE, show default emoji plot
  makingplots <- reactiveVal(value=FALSE, label = "makingplots")
  
  days_in_life <- reactive({
    birth <- input$dob
    current_life <- today() - birth
    return(as.numeric(current_life))
  })
  
  weeks_in_life <- reactive({
    return(floor(days_in_life()/7))
  })
  
  years_in_life <- reactive({
    return(floor(as.numeric(today()- input$dob)/365))
  })
  
  total_days <- reactive({
    death <- input$dob + years(input$life_slider)
    return(as.numeric(death - input$dob))
  })
  
  years_to_death <- reactive({
    death <- input$dob + years(input$life_slider)
    return(year(death) - year(today()))
  })
  
  weeks_to_death <- reactive({
    return(years_to_death() * 52)
  })
  
  observeEvent(input$dob, handlerExpr = days_in_life())
  observeEvent(input$weekyear, handlerExpr = make_height())
  
  # this is obsolete
  life_progress <- reactive({
    
    # do the math in fraction
    life_fraction <- input$life_slider/100
    # multiply by 10
    life_fraction <- ceiling(life_fraction * 10)
    # make a df
    df <- data.frame(x=0:9, y=1,
                     gone = c(rep("gone", life_fraction),
                              rep("to-go", 10 - life_fraction)))
    
    p1 <- ggplot(df,
                 aes(x,y, color=gone)) +
      geom_point(pch=15)+
      #geom_text(label="█") +
      scale_color_manual(values=c("red", "black")) +
      # xlim(c(-20, 20)) +
      theme(legend.position = "none")+
      cowplot::theme_nothing()+
      ggtitle("Life Progress (%)")
    return(p1)
    
  })
  
  
  search_emojis <- reactive({
    # to lower so we prevent NAs
    search_token <- tolower(input$search_term)
    # remove spaces
    search_token <- str_replace_all(search_token, " ", "_")
    
    tryCatch(
      expr = {
        possible_match <- emo::ji_completion(token = search_token)
        # subset from complete list of names
        emojis <- emo::ji_name[names(possible_match)]
        # if named, selectInput will show "names" instead of "values'
        #names(result) <- emojis$name
        result <- unname(emojis)
        # try to append using keywords besides completion
        try(result <- c(result, emo::ji_find(search_token) %>% pull(emoji)))
      },
      error = function(e){
        message('Could not find emoji, change term or upload photo')
        print(e)
      })
    
    if (length(result > 0)){
      return(result)
    } else {
      return(unname(emo::ji_name[]))
    }
    
  })
  
  show_modal <- reactive({
    showModal('Could not find emoji, change term or upload photo')
  })
  
  observe({
    updatePickerInput(
      session,
      "emoji_render",
      choices = search_emojis()
    )
  })
  
  observeEvent(
    input$newplot,
    {
      makingplots(TRUE)
      make_emoji_plot()
  })
  
  make_height <- reactive({
    if(input$weekyear == "weeks"){
      height = 1000  
    } else {
      height = 500
    }
    return(height)
  })
  
  # calculate pretty squares
  pretty_sq <- function(n) {
    li <- list()
    # initialize on the sqrt and grow
    x = floor(sqrt(n))
    y = floor(sqrt(n))
    while(x*y < n){
      x = x + 1
      if (x * y < n){
        y = y + 1
      }
    }
    li$x <- x
    li$y <- y
    return(li)
  }
  
  
  # life plot -----
  make_lifeplot <- reactive({
    
    # force you to put your dob
    validate(
      need(input$dob, 'Please enter date of birth')
    )
    
    years_lived <- years_in_life()
    
    if (input$weekyear == "years"){
      x = pretty_sq(input$life_slider)$x
      y = pretty_sq(input$life_slider)$y
      #print(x)
      x_var = 1:x
      y_var = 1:y
      dot_size = 5
      x_breaks = scale_x_continuous(breaks = seq(0, x, 5)[-1], #removes 0 seq by 5
                                    expand = c(0, 1))
      x_lab = ""
      y_lab = ""
      proper_theme = theme(panel.background = element_rect(fill = NA),
                           axis.text = element_blank(),
                           axis.ticks = element_blank(),
                           # center title
                           plot.title = element_text(hjust = 0.5, size = 20, face='bold',
                                                     margin = margin(b = -1, unit = "cm")),
                           axis.title = element_text(hjust = 1))
      df <- expand.grid(x = x_var, y = y_var) %>% 
        slice(1:input$life_slider)
      
      complete_year <- expand_grid(x=1:x, y=1:floor(years_lived/x))
      # calculate remainder of years on top row
      remaining_years <- years_lived - floor(years_lived/x) * x
      
      #print(years_lived)
      #print(remaining_years)
      # if remainder is 0, adjust to x. 
      remaining_years <- ifelse(remaining_years==0, 
                                min(1, x),
                                remaining_years)
      complete_year <- bind_rows(complete_year,
                                 expand_grid(x=1:remaining_years, y=ceiling(years_lived/x)))
      past_point <- geom_point(data=complete_year,
                               aes(x,y),
                               color="gray20",
                               pch=15, #filled sq
                               size=dot_size)
      
      current_decade <- ceiling(years_lived/x)
      # adjust in the case years lived is multiple of x (complete row)
      current_decade <- ifelse(years_lived %% x == 0, current_decade + 1, current_decade)
      print(current_decade)
      
      present_point <- geom_point(aes(x=ifelse(remaining_years == 1, 1, remaining_years+1), 
                                      y=current_decade),
                                  color="red",
                                  pch=15, #filled sq
                                  size=dot_size)
    } else {
      x_var = 1:52
      y_var = 1:input$life_slider
      dot_size = 2
      x_breaks = scale_x_continuous(breaks = seq(0, 52, 5)[-1], #removes 0 seq by 5
                                    expand = c(0, 1))
      x_lab = "Weeks of year"
      y_lab = "Years"
      df <- expand.grid(x = x_var, y = y_var) 
      proper_theme <- theme(panel.background = element_rect(fill = NA),
                            # center title
                            plot.title = element_text(hjust = 0.5, size = 20, face='bold',
                                                      margin = margin(b = -0.1, unit = "cm")),
                            axis.title = element_text(hjust = 1))
      
      # the -1 is to prevent week being 53 in some years
      # because we do -1, in week 1 it will show week 0, we adjust that on the ifelse
      week_numbers <- ifelse(isoweek(today()) - 1 < 1, 1, isoweek(today()) - 1)
      
      complete_year <- expand_grid(x=1:52, y=1:years_lived)
      complete_year <- bind_rows(complete_year,
                                 expand_grid(x=1:week_numbers,
                                             y=years_lived + 1 )
      )
      
      past_point <- geom_point(data=complete_year,
                               aes(x,y),
                               color="gray20",
                               pch=15, #filled sq
                               size=dot_size)
      
      # force week to be 52, some years might have 53
      # TODO: also, January 1st to 6th might belong to week 52/53 instead of 1
      # also, consider using isoweek() for accounting for leap years?
      present_week <- min(52, isoweek(today()))
      
      present_point <- geom_point(aes(x=present_week,
                                      y=years_lived + 1),
                                  color="red",
                                  pch=15, #filled sq
                                  size=dot_size)
      
    }
    
    p <-  df %>%
      ggplot(aes(x,y)) +
      geom_point(pch=0, size=dot_size) +
      proper_theme +
      scale_y_continuous(
        breaks = seq(0, input$life_slider, by=5)[-1],
        expand = c(0,1))+
      x_breaks +
      labs(title = paste(input$life_slider, "years of life in", input$weekyear),
           x = x_lab,
           y = y_lab)
    
    if(input$showpast){
      p <- p + past_point
    }
    
    if(input$showpresent){
      p <- p + present_point
    }
    
    return(p)
  })
  
  # render lifeplot ----
  output$lifeplot <- renderPlot({
    make_lifeplot()
  },
  height = function() make_height()
  )
  
  get_emoji <- reactive({
    return(
    jis %>%
      filter(emoji == input$emoji_render)
    )
    })
  
  local_image <- function(x, width = 50) {
    glue::glue("<img src='{x}' width='{width}'/>")
  }
  
  # default plot ----
  make_default_plot <- reactive({
    c(
      "person_in_lotus_position",
      "party_popper",
      "person_playing_water_polo",
      "pizza") %>% 
      map(ji_find) %>% 
      bind_rows(.id="id") %>% 
      mutate(keyword = c("yoga", "party", "water_polo", "slice"),
             keyword = paste("**keyword:**", keyword),
             fi_name = str_replace_all(name, "_", " "),
             filename = local_image(file.path("./data", paste0(fi_name, '.png')),
                                    width = 100)) %>% 
      ggplot(aes(id, y=1, label=filename))+
      geom_richtext()+
      geom_richtext(aes(id, y=0.4, label=paste("**Name:**", name)),
                    fill = NA,
                    label.color = NA, 
                    label.padding = grid::unit(rep(0, 4), "pt"))+
      geom_richtext(aes(id, y=0.2, label=keyword),
                    fill = NA,
                    label.color = NA, 
                    label.padding = grid::unit(rep(0, 4), "pt")) +
      scale_y_continuous(breaks=c(0,1.1),
                         expand = c(0, 0.5))+
      #scale_x_discrete(expand = c(1, 1))+
      theme_void()+
      theme(plot.title = element_text(hjust = 0.5, size = 20, face='bold'),
            plot.subtitle = element_text(hjust = 0.5, size = 12))+
      labs(title = "Examples",
           subtitle = "Use the menu to select activity or search to make custom plot!")
    
    
  })
  
  
  # emoji plot -----
  make_emoji_plot <- reactive({
    
    will_plot <- makingplots()
    
    if(will_plot==FALSE){
      # do the default plot
      p <- make_default_plot()
    } else {
      # do math
      # force you to put your dob
      validate(
        need(input$dob, 'Please enter date of birth')
      )
      # get emoji
      target_emojis <- get_emoji()
      # calculate frequency and total times left
      f <- input$frequency
      ytd <- years_to_death()
      wtd <- weeks_to_death()
      
      times_left <- case_when(input$time_frequency == "per week" ~  f * wtd,
                              input$time_frequency == "per year" ~ f * ytd)
      
      # if times left are too many, divide by 10
      flag <- times_left > 100
      if(flag){
        times_left <- floor(sqrt(times_left))
      }
      
      #browser()
      # get the pretty square
      x <- pretty_sq(times_left)$x
      y <- pretty_sq(times_left)$y
      
      # this will be really slow because it has to get the image each time
      # using local copies
      # transform emojis to links and to html
      #target_emojis <- 
      #  target_emojis %>%
      #  mutate(url = map_chr(emoji, slowly(~emoji_to_link(.x), rate_delay(0.1))),
      #         label = link_to_img(url))
      
      target_emojis <- 
        target_emojis %>%
        mutate(filename = file.path("./data", paste0(target_emojis$name, '.png')),
               label = local_image(filename))
      
      # should be length 1 to be recycled
      target_labels <- target_emojis$label
      
      print(target_labels)
      
      df <- expand.grid(x = 1:x, y = 1:y) %>% 
        slice(1:times_left) %>% 
        mutate(label=target_labels)
      
      custom_name <- ifelse(length(input$thing_name) > 0,
                            input$thing_name,
                            "")
      
      plot_subtitle <- ifelse(flag,
                              glue::glue("Around {times_left^2} left. Each emoji is {times_left} times."),
                              glue::glue("Around {times_left} left. Each emoji is 1 time."))
      
      p <- ggplot(df, aes(x, y, label=label))+
        geom_richtext(fill = NA,
                      label.color = NA, 
                      label.padding = grid::unit(rep(0, 4), "pt"))+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 20, face='bold'),
              plot.subtitle = element_text(hjust = 0.5, size = 12))+
        scale_y_continuous(expand = c(0.1, 0.1))+
        scale_x_continuous(expand = c(0.1, 0.1))+
        labs(title=custom_name,
             subtitle=plot_subtitle)
    }
    
        return(p)
  })
  
  # render default plot ---
  output$defaultplot <- renderPlot({
    make_default_plot()
  })
  
  # render emoji plot ----
  output$actplot <- renderPlot({
    make_emoji_plot()},
    height = function() make_height()
  )
  
  
  # life progress bar -----
  output$life_progress_text <- renderText(
    # force you to put your dob
    if(length(input$dob)>0 & is.Date(input$dob)) {
      my_pb(days_in_life()/total_days())
    } else {
      # do nothing
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
