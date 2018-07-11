library(shiny)
library(tidyverse)
library(lubridate)


dat <- read.csv('user_tree_observations.csv',stringsAsFactors = F)
dat <- dat %>% select(date,is_leaf_mature,is_leaf_fresh,is_flower_bud,is_fruit_ripe,is_fruit_unripe,is_flower_open,user_tree_id,user_id)
treeid <- read.csv('user_tree_table.csv', stringsAsFactors = F) %>% select(user_tree_id,tree_id)
spcid <- read.csv('species_master.csv', stringsAsFactors =F ) %>% select(species_id,species_scientific_name)
trees <- read.csv('trees.csv', stringsAsFactors = F) %>% select(tree_Id,species_id, tree_location_id)
loc <- read.csv('location_master.csv', stringsAsFactors = F) %>% select(tree_location_id,state_id)
dat$date <- as.Date(dat$date)
dat <- dat %>% filter(date > '2012-01-01', date< '2018-01-01')
dat <- dat[!duplicated(dat[,c(1,8)]),]
dat <- left_join(dat,treeid)
dat <- left_join(dat,trees, by = c("tree_id"="tree_Id"))
dat <- left_join(dat,spcid)
dat <- left_join(dat,loc)
spcs_table <- dat %>% group_by(species_scientific_name) %>% summarise(n_distinct(user_tree_id))
dat <- dat %>% filter(state_id == 18, species_scientific_name %in% c('Artocarpus heterophyllus','Cassia fistula','Mangifera indica','Tamarindus indica','Phyllanthus emblica'))
dat$year <- year(dat$date)
dat$week <- week(dat$date)
dat$month <- month(dat$date)
dat$day <- ifelse(day(dat$date)<15,1,15)
dat$fortnight <- paste(dat$month,dat$day, sep = '-')

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("SeasonWatch Kerala"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput('spcs','Select species',choices = unique(dat$species_scientific_name)),
        selectInput('grp','Collate to:',choices = c('Week' = 'week','Fortnight' = 'fortnight')),
        radioButtons('mapval','Map 0,1,2 to:',choices = c('0,1,2' = 1, '0,1,1' = 2, '0,0,1' = 3)),
         sliderInput("obs_year_filter",
                     "Minimum Observations per year per tree",
                     min = 1,
                     max = 50,
                     value = 1),
         sliderInput("year_mintree_filter",
                     "Minimum trees monitored per year",
                     min = 1,
                     max = 50,
                     value = 1),
         sliderInput("week_mintree_filter",
                     "Minimum number of trees monitored per week",
                     min = 1,
                     max = 50,
                     value = 1)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput('phenplot', height = 600),
         plotOutput("obsPlot", height = 200)
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  val_dat <- reactive({
    valind <- dat %>% 
      group_by_('species_scientific_name','year',input$grp) %>% 
      mutate(week_tree = n_distinct(user_tree_id)) %>% 
      filter(week_tree > input$week_mintree_filter) %>% ungroup() %>% 
      group_by(species_scientific_name,year,user_tree_id) %>% 
      summarise(nobs = n_distinct(date)) %>% 
      filter(nobs >= input$obs_year_filter) %>% 
      group_by(species_scientific_name,year) %>% 
      mutate(n_tree = n_distinct(user_tree_id)) %>%
      filter(n_tree>=input$year_mintree_filter)
    valdat <- inner_join(dat,valind[,2:3])
    valdat <- gather(valdat[,c(-9:-12,-14)],'phenophase','value',2:7)
    valdat$value[valdat$value == -1] <- NA
    if(input$mapval == 2) valdat$value[valdat$value == 2] <-  1
    if(input$mapval == 3) {
      valdat$value[valdat$value == 1] <-  0
    }
    return(valdat)
  })
   
  output$obsPlot <- renderPlot({
    valdat1 <- val_dat() %>% filter(phenophase == 'is_leaf_fresh', species_scientific_name == input$spcs) %>% group_by_('species_scientific_name','phenophase','year',input$grp) %>% summarise(ntree = n_distinct(user_tree_id)) %>% filter(ntree >= input$week_mintree_filter)
    allweek <- expand.grid(year = 2012:2017,week = 1:53, stringsAsFactors = F)
    allfort <- expand.grid(year = 2012:2017,month = 1:12, day = c(1,15), stringsAsFactors = F)
    allfort$fortnight <- paste(allfort$month,allfort$day, sep = '-')
    ifelse(input$grp == 'week',valdat1 <- left_join(allweek,valdat1),valdat1 <-  left_join(allfort,valdat1))
    ifelse(input$grp == 'week',valdat1$week <- ymd(paste(valdat1$year,'01','01',sep = '-')) + weeks(valdat1$week),valdat1$fortnight <- as.Date(paste(valdat1$year,valdat1$fortnight, sep = '-')))
    ggplot(data = valdat1,aes_string(x = input$grp,y = 'ntree')) + geom_line(col = 'darkred') + theme_bw() + expand_limits(y = 0) + ylab('Number of Observations')
  })
  
  output$phenplot <- renderPlot({
    phendat1 <- val_dat() %>% filter(species_scientific_name == input$spcs) %>% group_by_('species_scientific_name','phenophase','year',input$grp, 'user_tree_id') %>% arrange(value) %>% slice(1) %>% group_by_('species_scientific_name','phenophase','year',input$grp) %>% summarise(ntree = n_distinct(user_tree_id), prop = sum(value, na.rm =  T)/ntree) %>% filter(ntree >= input$week_mintree_filter)
    allweek <- expand.grid(phenophase = unique(phendat1$phenophase), year = 2012:2017,week = 1:53, stringsAsFactors = F)
    allfort <- expand.grid(phenophase = unique(phendat1$phenophase), year = 2012:2017,month = 1:12, day = c(1,15), stringsAsFactors = F)
    allfort$fortnight <- paste(allfort$month,allfort$day, sep = '-')
    ifelse(input$grp == 'week',phendat1 <- left_join(allweek,phendat1),phendat1 <-  left_join(allfort,phendat1))
    ifelse(input$grp == 'week',phendat1$week <- ymd(paste(phendat1$year,'01','01',sep = '-')) + weeks(phendat1$week),phendat1$fortnight <- as.Date(paste(phendat1$year,phendat1$fortnight, sep = '-')))
    if(input$mapval %in% c(1,3)) phendat1$prop = phendat1$prop/2
    ggplot(data = phendat1,aes_string(x = input$grp,y = 'prop')) + 
      geom_line() + 
      theme_bw() + 
      expand_limits(y = 0) + 
      ylab('Proportion of phenophase') + 
      facet_grid(phenophase~.)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

