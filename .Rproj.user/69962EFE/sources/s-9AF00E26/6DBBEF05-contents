library(shiny)
library(tidyverse)
library(leaflet)
library(geojsonio)
library(raster)
library("readxl")
library(debkeepr)
library(sp)
library(rnaturalearth)
library(WDI)
library(tigris)
library(plotly)

options(scipen = 100000)


#setwd("/Users/lukelorenz/Desktop/Middlebury College/Junior/DataScienceDisciplines/DataScience/Assignments/geojson")

#GETTING/MERGING SHIP DATA + ADDING CONVERSION

textile.data<- read_xlsx("WICVOC012021.xlsx")

#Takes schock and changes to pieces, takes half ps and changes to ps
assign3<- textile.data %>% mutate(real_quantity = 0) #create real value collumn

## Change Guinea to Elmina
assign3$dest_loc_region_arch = str_replace(assign3$dest_loc_region_arch, "Guinea", "Elmina")

#Ardra and Guinea // Ardra and St. Eaustatius == Ardra
assign3$dest_loc_region_arch = str_replace(assign3$dest_loc_region_arch, "Ardra and Guinea", "Ardra")
assign3$dest_loc_region_arch = str_replace(assign3$dest_loc_region_arch, "Ardra and St Eustatius / Guinea", "Ardra")




for(i in 1:length(textile.data$textile_unit)){ 
    if (assign3$textile_unit[i] %in% c("schock", "Schock")){ #fix the schock unit (x4)
        assign3$real_quantity[i] <- as.numeric(textile.data$textile_quantity[i])*4
    }
    else if(assign3$textile_unit[i] == "half ps."){ #fix the half ps. unit (/2)
        assign3$real_quantity[i] <- as.numeric(textile.data$textile_quantity[i])/2
    }
    else  if(assign3$textile_unit[i] == "el"){
        assign3$real_quantity[i] <- as.numeric(textile.data$textile_quantity[i])*as.numeric(textile.data$els_per_ps[i], na.rm = TRUE)
    }
    else{
        assign3$real_quantity[i] <- as.numeric(textile.data$textile_quantity[i]) #for now ignore rolls, els, and lbs- filter these units out in analysis
    }
}

#adding the locations!!

otherdata <- read_csv("withLocations.csv")
assign3$orig_loc_lat <- otherdata$orig_loc_lat
assign3$orig_loc_long <- otherdata$orig_loc_long
assign3$dest_loc_lat <- otherdata$dest_loc_lat
assign3$dest_loc_long <- otherdata$dest_loc_long



#Dutch Republic only listed as modern origin location
for(i in 1:length(assign3$textile_unit)){ 
    if (assign3$orig_loc_region_modern[i] %in% c("Netherlands")){ 
        assign3$orig_loc_region_arch[i] <- "Dutch Republic"
    }
}

#Fill missing Dutch Republic orig region arch when origin port is Amsterdam
for(i in 1:length(assign3$orig_loc_port_arch)){ 
  if (assign3$orig_loc_port_arch[i] %in% c("Amsterdam")){ 
    assign3$orig_loc_region_arch[i] <- "Dutch Republic"
  }
}


#Angola only listed in modern origin location
for(i in 1:length(assign3$dest_loc_region)){ 
  if (assign3$dest_loc_region[i] %in% c("Angola")){ 
    assign3$dest_loc_region_arch[i] <- "Angola"
  }
}

#Ardra and Elmina only listed as Destination Ports
for(i in 1:length(assign3$dest_loc_port)){ 
  if (assign3$dest_loc_port[i] %in% c("Elmina")){ 
    assign3$dest_loc_region_arch[i] <- "Elmina"
  }
  if (assign3$dest_loc_port[i] %in% c("Ardra")){ 
    assign3$dest_loc_region_arch[i] <- "Ardra"
  }
}


#Fill Destination Region Elmina, Ardra, Angola, Arguin from historical port data
for(i in 1:length(assign3$dest_loc_port_arch)){ 
  if (assign3$dest_loc_port_arch[i] %in% c("Elmina")){ 
    assign3$dest_loc_region_arch[i] <- "Elmina"
  }
  if (assign3$dest_loc_port_arch[i] %in% c("Ardra")){ 
    assign3$dest_loc_region_arch[i] <- "Ardra"
  }
  if (assign3$dest_loc_port_arch[i] %in% c("Angola")){ 
    assign3$dest_loc_region_arch[i] <- "Angola"
  }
  if (assign3$dest_loc_port_arch[i] %in% c("Arguin")){ 
    assign3$dest_loc_region_arch[i] <- "Arguin"
  }
  if (assign3$dest_loc_port_arch[i] %in% c("Elmina and Ardra")){ 
    assign3$dest_loc_region_arch[i] <- "Elmina"
  }
  if (assign3$dest_loc_port_arch[i] %in% c("Ardra & Suriname")){ 
    assign3$dest_loc_region_arch[i] <- "Ardra"
  }
  if (assign3$dest_loc_port_arch[i] %in% c("Elmina and Angola")){ 
    assign3$dest_loc_region_arch[i] <- "Elmina"
  }
}


q<- assign3 %>%
  filter(is.na(orig_loc_region_arch))

# Change Guinea to Elmina



#Ardra and Guinea // Ardra and St. Eaustatius == Ardra



assign3 <- assign3 %>%
    mutate(piece_rate = as.numeric(total_value_guldens)/as.numeric(real_quantity))


slavetrade.data <- read.csv("SlaveTrade.csv")


mutated.slavetrade.data <- slavetrade.data %>%
    mutate(new_yr = as.numeric(substr(slavetrade.data$`Date.that.voyage.began`, 1,4))) #Takes substring gets the year

joined.ship.data <- textile.data %>%
    inner_join(mutated.slavetrade.data, by = c("means_of_exchange" = "Vessel.name",
                                               "orig_yr" = "new_yr"))

#GETS THE EXCHANGE RATE FOR COLOR OF TEXTILE TO NUMBER OF SLAVES
joined.ship.data$total_value_stuivers[is.na(joined.ship.data$total_value_stuivers)] <- 0
joined.ship.data$total_value_penningen[is.na(joined.ship.data$total_value_penningen)] <- 0
joined.ship.data$total_value_stuivers[is.na(joined.ship.data$total_value_guldens)] <- 0

#converting vals
converted.joined.ship.data <- joined.ship.data %>% deb_gather_lsd(gsp_col,
                                                                  l = total_value_guldens,
                                                                  s = total_value_stuivers,
                                                                  d = total_value_penningen,
                                                                  bases = c(16,20))
temp.data <- converted.joined.ship.data %>%
    mutate(val = as.numeric(str_extract(shipment_total_value, "^[:digit:]+")))

#mutating to get exchang rate for slaves                         
final.data <- temp.data %>%
    mutate(textile_total = deb_as_decimal(gsp_col)) %>%
    mutate(pct = textile_total/val) %>%
    mutate(text_is_worth_slaves = floor(pct*Total.embarked)) %>%
    mutate(exchange_rate = as.numeric(textile_quantity)/text_is_worth_slaves) #gets ____ pieces of ___ per slave



#Read in geojson files
angola <- geojson_read("geojson/angola.json",
                       what = "sp")
ardra <- geojson_read("geojson/ardra.json",
                      what = "sp")
arguin <- geojson_read("geojson/arguin.json",
                       what = "sp")
bantam <- geojson_read("geojson/bantam.json",
                       what = "sp")
batavia <- geojson_read("geojson/batavia.json",
                        what = "sp")
bengalE <- geojson_read("geojson/bengalE.json",
                        what = "sp")
capeTown <- geojson_read("geojson/capeTown.json",
                         what = "sp")
ceylon <- geojson_read("geojson/ceylon.json",
                       what = "sp")
cGoodHope <- geojson_read("geojson/cGoodHope.json",
                          what = "sp")
cheribon <- geojson_read("geojson/cheribon.json",
                         what = "sp")
coromandel <- geojson_read("geojson/coromandel.json",
                           what = "sp")
dutchrep <- geojson_read("geojson/dutchrepublic.json",
                         what = "sp")
elmina <- geojson_read("geojson/elmina.json",
                       what = "sp")
jambi <- geojson_read("geojson/jambi.json",
                      what = "sp")
japan <- geojson_read("geojson/japan.json",
                      what = "sp")
northeastJava <- geojson_read("geojson/centralJava.json",
                              what = "sp")
makassar <- geojson_read("geojson/makassar.json",
                         what = "sp")
malabar <- geojson_read("geojson/malabar.json",
                        what = "sp")
malacca <- geojson_read("geojson/malacca.json",
                        what = "sp")
mokka <- geojson_read("geojson/mokka.json",
                      what = "sp")
palembang <- geojson_read("geojson/southSumatra.json",
                          what = "sp")
persia <- geojson_read("geojson/persia.json",
                       what = "sp")
siam <- geojson_read("geojson/siam.json",
                     what = "sp")
spiceIslands <- geojson_read("geojson/spiceIslands.json",
                             what = "sp")
surat <- geojson_read("geojson/surat.json",
                      what = "sp")
timor <- geojson_read("geojson/timor.json",
                      what = "sp")
wSumatra <- geojson_read("geojson/wsumatra.json",
                         what = "sp")
eastJava <- geojson_read("geojson/westJava.json", #acutally east java
                         what = "sp")


angola@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "Angola")

arguin@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "Arguin")

batavia@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "Batavia")

bengalE@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "Bengalen")

capeTown@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "capeTown")

ceylon@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "Ceylon")

cGoodHope@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "Cape of Good Hope")

cheribon@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "Cheribon")

coromandel@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "Coromandel")

jambi@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "Jambi")

japan@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "Japan")

makassar@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "Makassar")

malacca@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "Mallaka")

mokka@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "Mokka")

persia@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "Persia")

siam@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "Siam")

spiceIslands@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "Spice Islands")

surat@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "Surat")

timor@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "Timor")

wSumatra@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "West Sumatra, Indonesia")

palembang@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "Palembang")

malabar@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "Malabar")

northeastJava@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "Java's Northeast Coast")

eastJava@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "Java's Northeast Coast")

elmina@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "Elmina")

dutchrep@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "Dutch Republic")

bantam@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "Bantam")

arguin@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "Arguin")

ardra@data <- data.frame() %>%
    add_column(Country = "Country") %>%
    add_row(Country = "Ardra")

#initial join
ab <- raster::union(angola,arguin)
ab <- raster::union(ab,ardra) #Fix later
ab <- raster::union(ab,arguin)
ab <- raster::union(ab,bantam)
ab <- raster::union(ab,batavia)
ab <- raster::union(ab,bengalE)
ab <- raster::union(ab,capeTown)
ab <- raster::union(ab,ceylon)
#ab <- raster::union(ab,cGoodHope) #Also problems with this one
ab <- raster::union(ab,cheribon)
ab <- raster::union(ab,coromandel)
ab <- raster::union(ab,dutchrep)
ab <- raster::union(ab,elmina)
ab <- raster::union(ab,jambi)
ab <- raster::union(ab,japan)
# ab <- raster::union(ab,northeastJava)/// #problems with geojson
ab <- raster::union(ab,eastJava)
ab <- raster::union(ab,makassar)
ab <- raster::union(ab,malabar)
ab <- raster::union(ab,malacca)
ab <- raster::union(ab,mokka)
ab <- raster::union(ab,palembang)
ab <- raster::union(ab,persia)
ab <- raster::union(ab,siam)
ab <- raster::union(ab,spiceIslands)
ab <- raster::union(ab,surat)
ab <- raster::union(ab,timor)
ab <- raster::union(ab,wSumatra)


#Get data into one row
ab@data <- ab@data %>%
    mutate(new.country = case_when(!is.na(Country.1) ~ Country.1,
                                   !is.na(Country.2) ~ Country.2,
                                   !is.na(Country.1.1) ~ Country.1.1,
                                   !is.na(Country.1.2) ~ Country.1.2,
                                   !is.na(Country.1.3) ~ Country.1.3,
                                   !is.na(Country.1.4) ~ Country.1.4))


#For coloring the map if the user selects origin
export_val <- assign3 %>%
    group_by(orig_loc_region_arch)%>%
    summarise(region.value = sum(total_value_guldens, na.rm  = TRUE))
ab.origin <- ab
ab.origin@data<- ab@data %>% 
    left_join(export_val, by = c("new.country" = "orig_loc_region_arch"), na.omit = TRUE)
color <- colorNumeric(palette = "RdYlBu",
                      reverse = TRUE,
                      domain = ab.origin@data$region.value)

#If user selects destination
import_val <- assign3 %>%
    group_by(dest_loc_region_arch)%>%
    summarise(region.value2 = sum(total_value_guldens, na.rm  = TRUE))
ab.dest <- ab.origin
ab.dest@data<- ab.origin@data %>% 
    left_join(import_val, by = c("new.country" = "dest_loc_region_arch"), na.omit = TRUE)
color2 <- colorNumeric(palette = "RdYlBu",
                       reverse = TRUE,
                      domain = ab.dest@data$region.value2)


#myLoc = ""

#Avoid tedious rewrites
switch_func <- function(input,session){
    switch(input$inputChoice,
           "Textile Name" = {
             #this allows me to filter the data and only select textiles that are exported
             if(is_null(myLoc)) {
               return()
             } else {
               text_choices <- assign3 %>%
                 filter(orig_loc_region_arch == myLoc)
               updateSelectInput(session = session, inputId = "inputChoice_two", choices = c("All", unique(text_choices$textile_name)))
               
             }
           },
           
           "Company (WIC/VOC)" = {
               updateSelectInput(session = session, inputId = "inputChoice_two", choices = c("VOC","WIC", "Both"))
           },
           "Origin" = {
               updateSelectInput(session = session, inputId = "inputChoice_two", choices = c("Origin"))
           },
           "Destination" = {
               updateSelectInput(session = session, inputId = "inputChoice_two", choices = c("Destination"))
           },
           "Year" = {
               updateSelectInput(session = session, inputId = "inputChoice_two", choices = c("Year"))
           }
    )
}




#Resets the ma
reset_map <- function(output,input,location){
  
    output$map <- renderLeaflet({
      
      
      #updateSelectInput(inputId = "location", choices = unique(assign3$orig_loc_region_arch), selected = myLoc)
      
  
      print(paste(myLoc, "resetting map..."))
        
        return_graph <- ab %>% 
            leaflet() %>%
          addProviderTiles("CartoDB.PositronNoLabels") %>%
           # addTiles %>%
            addPolygons(color = "black",
                        label = ~new.country,
                        layerId = ab@data$new.country,
                        opacity = 1,
                        weight = 1,
                        stroke = 1) %>%
            setView(65.25,10, 2)
        if(!is_null(location)) {
          print(paste(myLoc,input$inputChoice_two))
          mayb <- assign3 %>%
            filter(orig_loc_region_arch == myLoc & textile_name == input$inputChoice_two)
          
          maybe <- mayb[ , c("orig_loc_region_arch", "orig_loc_lat", "orig_loc_long", "dest_loc_port","dest_loc_lat", "dest_loc_long", "dest_loc_region_arch")]
          
          #adding exporting loc circle
          return_graph <- addCircleMarkers(return_graph, 
                           lat = ~maybe$orig_loc_lat,
                           lng = ~maybe$orig_loc_long,
                           label = ~maybe$orig_loc_region_arch,
                           radius = 2,
                           color = "red"
          )
          
          #adding importing loc circle
          return_graph <- addCircleMarkers(return_graph, 
                                           lat = ~maybe$dest_loc_lat,
                                           lng = ~maybe$dest_loc_long,
                                           label = ~maybe$dest_loc_region_arch,
                                           radius = 2,
                                           color = "blue"
          )
          
          for(i in 1:nrow(maybe)){
            
            return_graph <-  addPolylines(return_graph,
                                   lat = as.numeric(maybe[i, c(2, 5)]),
                                   lng = as.numeric(maybe[i, c(3, 6)]),
                                   label = paste(myLoc, maybe$orig_loc_region_arch),
                                   weight = 1,
                                   color = "purple")
            }
        } 
        
        return_graph
    })
}


#Map, selection bar, graphs (high-> low)
ui <- fluidPage(
  #linking my stylesheets
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "index.css")
  ),
  titlePanel("Dutch Textile Trade"),
      tags$div(class = "container", 
               tags$div(class = "options",
                        
                        selectInput(inputId = "location",
                                    label = "Choose Location",
                                    choices = c("All" = " ", unique(assign3$orig_loc_region_arch))
                                 ),
                         selectInput(inputId = "inputChoice",
                                     label = "Choose identifier!",
                                     choices = c("Textile Name", "Company (WIC/VOC)", "Origin", "Destination", "Year", "Modifiers")),
                         selectInput(inputId = "inputChoice_two",
                                     label = "Choose what you would like to graph!",
                                     choices = NULL),
               ),
               tags$div(class = "map",
                         leafletOutput(outputId = "map", width = "100%")
               ),
        ),
      
        plotlyOutput(outputId = "plot"),
        leafletOutput(outputId = "dropdown"),
        verbatimTextOutput("selection")
)

server <- function(input, output, session) {
    
    myLoc <<- " "
    output$dropdown <- renderLeaflet({
        switch_func(input,session)
    })
    reset_map(output,input, NULL)
    
    change <<- TRUE

    output$plot <- renderPlotly({
      
      print(paste("myLoc: ", myLoc, " input$location: ", input$location, "input$map_shape_click$id: ", input$map_shape_click$id))
      #if the user has not selected a map location or a dropdown location)
      if(is_null(input$map_shape_click$id) && myLoc == " " ) {
        print(1)
        myLoc <<- "Batavia"
        updateSelectInput(inputId = "location", choices = unique(assign3$orig_loc_region_arch), selected = myLoc)
        
        #if the user has not selected a map location and the user has selected a dropdown location
      }else if(is_null(input$map_shape_click$id) && !is_null(input$location)) {
        print(2)
          myLoc <<- input$location
          updateSelectInput(inputId = "location", choices = unique(assign3$orig_loc_region_arch), selected = myLoc)
          
         #if the user has seleccted a map location or dropdown
      } else {
          if(input$map_shape_click$id != myLoc) {
            print(3)
            myLoc <<- input$map_shape_click$id
            updateSelectInput(inputId = "location", choices = unique(assign3$orig_loc_region_arch), selected = myLoc)
            
          } else if (input$location != myLoc ) {
            print(4)
            myLoc <<- input$location
            updateSelectInput(inputId = "location", choices = unique(assign3$orig_loc_region_arch), selected = myLoc)
            
          }
          # if(change == TRUE) {
          #   myLoc <<- input$map_shape_click$id
          #   updateSelectInput(inputId = "location", choices = unique(assign3$orig_loc_region_arch), selected = myLoc)
          # }
          # if(change == FALSE) {
          #   myLoc <<- input$location
          #   updateSelectInput(inputId = "location", choices = unique(assign3$orig_loc_region_arch), selected = myLoc)
          #   
          # }
        }
      
      print(paste("selected" , myLoc))
      
      #Conditionals based on user input from the drop down menu

      
      #if the input is a textile... graph it
      if(input$inputChoice_two %in% unique(assign3$textile_name)) {
        reset_map(output,input, input$inputChoice_two)
        assign3 %>%
          filter(orig_loc_region_arch == myLoc)%>%
           filter(textile_name == input$inputChoice_two ) %>%
              ggplot() +
              geom_col(aes(x = dest_yr, y = as.numeric(textile_quantity)),
                       fill = "#FB8B24") +
              theme_bw() +
              labs(title = paste("The quantity of", input$inputChoice_two, "from", myLoc, "exported by year"), x ="Year", y = "Total Quantity")

        } else {
          switch(input$inputChoice_two,
                   "All" = {
                       #Change graph back to normal
                       reset_map(output,input, NULL)
                       
                   g<- assign3 %>% 
                           group_by(orig_loc_port_arch, textile_name) %>% 
                           filter(piece_rate < 20) %>% #For now, filtering by cheap pieces
                           filter(orig_loc_region_arch == myLoc) %>%
                           summarise(mean_value = mean(piece_rate))%>%
                           ggplot()+
                           geom_tile(aes(x = orig_loc_port_arch, y= textile_name, fill = mean_value))+
                           labs(title = paste("A chart of all exports from", myLoc, "ports") ,x = "Origin Port", y = "Textile") + 
                           guides(fill=guide_legend(title="Mean Value per Piece")) +
                           scale_fill_gradient(low = "#460B2F", high = "#E36414", na.value = NA)

                   ggplotly(g)
                   
                   
                   },

                   "Both" ={
                      s #Change the graph back to normal
                       reset_map(output,input, NULL)
                       
                       assign3 %>%
                           filter(orig_loc_region_arch == myLoc)%>%
                           ggplot() + 
                           geom_col(aes(x = dest_yr, y = as.numeric(textile_quantity)),
                                    fill = "#FB8B24") +
                           theme_bw() +
                          labs(title = paste("Quanity of Textiles Shipped out of",myLoc ,"by Both VOC/WIC"), x ="Year", y = "Total Quantity")
                   },
                   
                   "WIC" = {
                      reset_map(output,input, NULL)
                      
                     dataforWIC <- assign3 %>%
                       filter(orig_loc_region_arch == myLoc)%>%
                       filter(company == "WIC")
                     
                     if(nrow(dataforWIC) == 0) {
                       df <- data.frame(
                         label=c("No available data"),
                         x = c(1.5), y =c(1.5))
                       ggplot(df, aes(x=x, y=y, label=label)) + geom_text(mapping = aes(x = x, y = y), size = 10)
                     } else {
                       dataforWIC %>%
                         ggplot() + 
                         geom_col(aes(x = dest_yr, y = as.numeric(textile_quantity)),
                                  fill = "#FB8B24") +
                         theme_bw() +
                         labs(title = paste("Quanity of Textiles Shipped out of",myLoc ,"by the", input$inputChoice_two, "Company"), x ="Year", y = "Total Quantity")
                       
                     }
                       
                      
                       
                   },
                   
                   "VOC" = {
                     #Change the graph back to normal
                     reset_map(output,input, NULL)
                     
                     dataforVOC <- assign3 %>%
                       filter(orig_loc_region_arch == myLoc)%>%
                       filter(company == "VOC") 
                     
  
                     if(nrow(dataforVOC) == 0) {
                       df <- data.frame(
                         label=c("No available data"),
                         x = c(1.5), y =c(1.5))
                       ggplot(df, aes(x=x, y=y, label=label)) + geom_text(mapping = aes(x = x, y = y), size = 10)
                     } else {
                       dataforVOC %>%
                            ggplot() + 
                             geom_col(aes(x = dest_yr, y = as.numeric(textile_quantity)),
                                      fill = "#FB8B24") +
                             theme_bw() +
                            labs(title = paste("Quanity of Textiles Shipped out of",myLoc ,"by the", input$inputChoice_two, "Company"), x ="Year", y = "Total Quantity")
                     }  
                   },
                   
                   
                   "Origin" = {
                       output$map <- renderLeaflet({
                           
                           print(paste(myLoc, "origin map"))
                           
                           switch_func(input,session)
                           
                           ab.origin %>%
                               leaflet() %>%
                               addTiles %>%
                               addPolygons(color = "black",
                                           label = ~new.country,
                                           layerId = ab.origin@data$new.country,
                                           fillColor = ~color(region.value),
                                           popup = ~region.value,
                                           fillOpacity = 1,
                                           opacity = 1,
                                           weight = 1,
                                           stroke = 1) %>%
                               setView(55.25,0, 3)  %>% #Sets view to center
                               addLegend("topright", pal = color, values = ~region.value,
                                         title = "Total Value (Guldens) Exported",
                                         na.label = "No Exports",
                                         labFormat = labelFormat(suffix = "g"),
                                         opacity = 1)
                           
                       })
                   }
                   ,
                   
                   
                   "Destination" = {
                       output$map <- renderLeaflet({
                           
                           print(paste(myLoc, "destination"))
                           
                           switch_func(input,session)
                           
                           ab.dest %>%
                               leaflet() %>%
                               addTiles %>%
                               addPolygons(color = "black",
                                           label = ~new.country,
                                           layerId = ab.dest@data$new.country,
                                           fillColor = ~color(region.value2),
                                           popup = ~region.value2,
                                           fillOpacity = 1,
                                           opacity = 1,
                                           weight = 1,
                                           stroke = 1) %>%
                               setView(55.25,0, 3) %>% #Sets view to center
                               addLegend("topright", pal = color, values = ~region.value,
                                         title = "Total Value (Guldens) Imported",
                                         na.label = "No Imports",
                                         labFormat = labelFormat(suffix = "g"),
                                         opacity = 1)
                       })
                       
                       },
                   
                   
                   
                   "Year" = {
                       reset_map(output,input, NULL)
                     
                     output$plot <- renderPlot({
                       require(scales)
                       test <- textile.data %>%
                           filter(orig_loc_region_arch == myLoc)
                       test <- assign3 %>%
                         drop_na(orig_loc_port_arch)
                       #Need to group into seperate dataset for the aggregate function
                       test2 <-aggregate(as.numeric(test$textile_quantity), by = list(year = test$orig_yr), FUN = sum)
                       ggplot(data = test2) + 
                           geom_area(mapping = aes(x = year, 
                                                   y = x, 
                                                   fill = color
                           )) + xlab("Year") + ylab("Textile Value") +
                           #This + require(scales) allows the y axis to look much cleaner
                           scale_y_continuous(labels = comma)
                       # scale_fill_manual(values=c("#460B2F", "#FB8B24", "#9A031E", "#E36414", "#894E19", "#3E2362", "#000000"))
                       
                       temp <- textile.data %>%
                           filter(orig_loc_region_arch == myLoc)
                       temp2 <- aggregate(as.numeric(temp$textile_quantity), by = list(year = temp$orig_yr), FUN = sum)
                       ggplot(data = temp2) +
                           geom_area(mapping = aes(x = year, 
                                                   y = x
                           ))+ xlab("Year") + ylab("Textile Value")
                         geom_area(mapping = aes(x = year, 
                                                 y = x,
                                                 fill = "#FB8B24",
                         )) +
                         theme(legend.position = "none") +
                         labs(title = "Total Quantity of All Textiles Shipped", x = "Year", y = "Textile Quantity") +
                         scale_fill_manual(values=c("#FB8B24")) +
                         scale_x_binned("Year") #Displays all of the year
                     })
                   },
                  "Modifiers" = {
                   reset_map(output,input, NULL)
                   
                   #HISTOGRAM FOR COLOR DISTRIBUTION
                   assign3 %>%
                     filter(textile_color_arch == "black") %>%
                     ggplot()+
                     geom_histogram(mapping = aes(x = as.numeric(textile_quantity)),
                                    bins = 30,
                                    color = "black",
                                    fill = "black") +
                     labs(title = paste("Distribution of shipment sizes based on", "Textile Color"), x = "Textile quantity (singular shipment)")
                 })
        }
    })
    
}
#live loading of shiny app
options(shiny.autoreload = TRUE)
shinyApp(ui, server)







