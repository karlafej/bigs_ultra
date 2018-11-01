library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(lubridate)
library(magrittr)

leader <- read_csv("./data/2018_leaderboard.csv", 
                  col_types = cols(`Avg loop` = col_character(),
                                   `Slow loop` = col_character(),
                                   `Race Time` = col_character()))
laps = read_csv("./data/2018_laps.csv", col_types = "ciiiccc")

laps <-
  laps %>% 
  left_join(leader, by = c("name" = "Name")) %>% 
  mutate(lap_split = as_datetime(ms(lap_split)),
         name = forcats::fct_reorder(name, Rank)) 

times <- as_tibble(seq(0, 60, 12))
colnames(times) <- "hour"
times <- mutate(times, trail = if_else(((hour/12)) %% 2 == 1, "road", "trail"))


ymin <- min(laps$lap_split, na.rm = TRUE)
ymax <- max(laps$lap_split, na.rm = TRUE)

### https://stackoverflow.com/questions/29738975/how-to-align-a-group-of-checkboxgroupinput-in-r-shiny

multicol <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                 height: 320px;
                                 -webkit-column-count: 6; /* Chrome, Safari, Opera */ 
                                 -moz-column-count: 6;    /* Firefox */ 
                                 column-count: 6; 
                                 -moz-column-fill: auto;
                                 -column-fill: auto;
                                 } 
                                 ")) 
  ))

headings <- list(h1("Big's Backyard Ultra 2018"), h3("Time splits"))
  
boxes <- 
  list(tags$div(align = 'left', 
                class = 'multicol',
                checkboxGroupInput("runner", "Select runners:",
                                   leader$Name, selected = leader$Name[1:6], 
                                   inline = FALSE)
  ))

  
ui <- fluidPage(multicol,
  title = "Big Dog's Backyard Ultra",
  fluidRow(multicol, headings, boxes),
     
  fluidRow(
      plotOutput("lapsPlot", height = "500px")
      )
  )


server <- function(input, output) {
  
  output$lapsPlot <- renderPlot({
    laps %>% 
      tidyr::drop_na() %>% 
      dplyr::filter(name %in% input$runner) %>% 
      ggplot() +
      geom_rect(data = times,
                aes(xmin = hour,
                    xmax = hour + 12,
                    ymin = ymin,
                    ymax = ymax,
                    fill = trail),
                alpha = .3) +
      scale_fill_manual(values = c("#888888", "#EEEEEE")) +
      geom_point(aes(x = lap, 
                    y = lap_split, 
                    colour = name)) +
      geom_line(aes(x = lap, 
                    y = as_datetime(lap_split), 
                    colour = forcats::fct_reorder(name, Rank))) +
      theme_bw() +
      scale_x_continuous(breaks = seq(0, 70, 2)) +
      scale_y_datetime(date_breaks = "2 min", date_labels = "%H:%M") +
      theme(legend.position = "bottom", legend.title = element_blank(),
            legend.text = element_text(size = 11),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14)) +
      ylab("Lap split") +
      xlab("Lap") 
  })
}

shinyApp(ui, server)