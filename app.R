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
library(reactlog)

options(scipen = 100000)


#setwd("/Users/alecgironda/Desktop/Data Science/final_final_proj")

#GETTING/MERGING SHIP DATA + ADDING CONVERSION

textile.data<- read_xlsx("WICVOC012021.xlsx")

#Takes schock and changes to pieces, takes half ps and changes to ps
assign3<- textile.data %>% mutate(real_quantity = 0, real_guldens = 0, clean_unit = "NO") #create real value collumn

for(i in 1:length(textile.data$textile_unit)){ 
  if (textile.data$textile_unit[i] %in% c("schock", "Schock", "schok")){ #fix the schock unit (x4)
    assign3$real_quantity[i] <- as.numeric(textile.data$textile_quantity[i])*4
      assign3$clean_unit[i] = str_replace(assign3$clean_unit[i], "NO", "ps")
  }
  else if(textile.data$textile_unit[i] == "half ps."){ #fix the half ps. unit (/2)
    assign3$real_quantity[i] <- as.numeric(textile.data$textile_quantity[i])/2
    assign3$clean_unit[i] = str_replace(assign3$clean_unit[i], "NO", "ps")
  }
  else  if(textile.data$textile_unit[i] %in% c("el")){
    assign3$real_quantity[i] <- as.numeric(textile.data$textile_quantity[i])*as.numeric(assign3$els_per_ps[i], na.rm = TRUE)
    assign3$clean_unit[i] = str_replace(assign3$clean_unit[i], "NO", "ps")
  }
  else  if(textile.data$textile_unit[i] %in% c("ps","stux", "ps.", "full ps//.")){
    assign3$real_quantity[i] <- as.numeric(textile.data$textile_quantity[i])
    assign3$clean_unit[i] = str_replace(assign3$clean_unit[i], "NO", "ps")
  }
  else{
    assign3$real_quantity[i] <- as.numeric(textile.data$textile_quantity[i]) #for now ignore rolls, els, and lbs- filter these units out in analysis
  }
}



##Modify Indian Guidlers to Guilders
for(i in 1:length(assign3$shipment_currency)){ 
  if (assign3$shipment_currency[i] %in% c("Indian Guilders")){
    assign3$real_guldens[i] <- as.numeric(assign3$total_value_guldens[i])*(10.5/15)
  }
  else{
    assign3$real_guldens[i] <- as.numeric(assign3$total_value_guldens[i])
  }
}

#10.5 Dutch guilders = 15 Indian guilders

###Working on Units
zzz<- assign3 %>%
  group_by(clean_unit) %>%
  summarise (sum = sum(real_guldens, na.rm = TRUE))

zw<- assign3 %>%
  filter(clean_unit == "NO") %>%
  group_by(textile_name, textile_unit) %>%
  summarise(sum = sum(real_guldens, na.rm  = TRUE))


## Change Guinea to Elmina
assign3$dest_loc_region_arch = str_replace(assign3$dest_loc_region_arch, "Guinea", "Elmina")

#Ardra and Guinea // Ardra and St. Eaustatius == Ardra
assign3$dest_loc_region_arch = str_replace(assign3$dest_loc_region_arch, "Ardra and Guinea", "Ardra")
assign3$dest_loc_region_arch = str_replace(assign3$dest_loc_region_arch, "Ardra and St Eustatius / Guinea", "Ardra")






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
    mutate(piece_rate = as.numeric(real_guldens)/as.numeric(real_quantity))


slavetrade.data <- read.csv("SlaveTrade.csv")


mutated.slavetrade.data <- slavetrade.data %>%
    mutate(new_yr = as.numeric(substr(slavetrade.data$`Date.that.voyage.began`, 1,4))) #Takes substring gets the year

joined.ship.data <- assign3 %>%
    inner_join(mutated.slavetrade.data, by = c("means_of_exchange" = "Vessel.name",
                                               "orig_yr" = "new_yr"))

#GETS THE EXCHANGE RATE FOR COLOR OF TEXTILE TO NUMBER OF SLAVES
joined.ship.data$total_value_stuivers[is.na(joined.ship.data$total_value_stuivers)] <- 0
joined.ship.data$total_value_penningen[is.na(joined.ship.data$total_value_penningen)] <- 0
joined.ship.data$total_value_stuivers[is.na(joined.ship.data$real_guldens)] <- 0

#converting vals
converted.joined.ship.data <- joined.ship.data %>% deb_gather_lsd(gsp_col,
                                                                  l = real_guldens,
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
    mutate(exchange_rate = as.numeric(real_quantity)/text_is_worth_slaves) #gets ____ pieces of ___ per slave



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
    add_row(Country = "Cape Town")

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
    add_row(Country = "Malakka")

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
    add_row(Country = "Java")

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
    summarise(export.value = sum(real_guldens, na.rm  = TRUE))
ab.origin <- ab
ab.origin@data<- ab@data %>% 
    left_join(export_val, by = c("new.country" = "orig_loc_region_arch"), na.omit = TRUE)
color <- colorNumeric(palette = "RdYlBu",
                      reverse = TRUE,
                      domain = ab.origin@data$export.value)

#If user selects destination
import_val <- assign3 %>%
    group_by(dest_loc_region_arch)%>%
    summarise(import.value = sum(real_guldens, na.rm  = TRUE))
ab.dest <- ab.origin
ab.dest@data<- ab.origin@data %>% 
    left_join(import_val, by = c("new.country" = "dest_loc_region_arch"), na.omit = TRUE)
color2 <- colorNumeric(palette = "RdYlBu",
                       reverse = TRUE,
                      domain = ab.dest@data$import.value)



#Avoid tedious rewrites
switch_func <- function(input,session){
  switch(input$inputChoice,
         "Export Data" = {
           #this allows me to filter the data and only select textiles that are exported
           if(is_null(input$map_shape_click$id)) {
             return()
           } else {
             text_choices <- assign3 %>%
               filter(orig_loc_region_arch == input$map_shape_click$id)
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
         },
         "Color" = {
           #this allows me to filter the data and only select textiles that are exported
           if(is_null(input$map_shape_click$id)) {
             return()
           } else {
             #Styling colors
             # assign3$textile_color_arch = str_replace(assign3$textile_color_arch, "BROWN-BLUE", "BROWN BLUE")
             # assign3$textile_color_arch = str_replace(assign3$textile_color_arch, "BLUE / AZURE", "BLUE")
             text_choices <- assign3 %>%
               filter(orig_loc_region_arch == input$map_shape_click$id)
             updateSelectInput(session = session, inputId = "inputChoice_two", choices = c("All Colors", unique(text_choices$textile_color_arch)))
             
           }
         },
         "Pattern" = {
           #this allows me to filter the data and only select textiles that are exported
           if(is_null(input$map_shape_click$id)) {
             return()
           } else {
             assign3$textile_color_arch = toupper(assign3$textile_pattern_arch)
             assign3$textile_color_arch = str_replace(assign3$textile_color_arch, "STRIPES", "STRIPED")
             text_choices <- assign3 %>%
               filter(orig_loc_region_arch == input$map_shape_click$id)
             updateSelectInput(session = session, inputId = "inputChoice_two", choices = c("All Patterns", unique(text_choices$textile_pattern_arch)))
             
           }
         }
         
  )
}



#Resets the map
reset_map <- function(output,input,location, type){
  
  
    output$map <- renderLeaflet({
      
      #defaults
      dateFtoUse <- ab
      myfillCol <- "black"
      layerIDVar <- ab@data$new.country
      legendYesNo <- "no"
      print(paste(input$map_shape_click$id, "resetting map..."))
      if(is_null(type)) {
        
        
        #for if we want origin or destination
      } else{
        if (type == "originMap") {
          dateFtoUse <- ab.origin
          myfillCol <- ~color(export.value)
          layerIDVar <- ab.origin@data$new.country
          legendYesNo <- "origin"
        } else if (type == "destMap") {
          dateFtoUse <- ab.dest
          myfillCol <- ~color2(import.value)
          layerIDVar <- ab.dest@data$new.country
          legendYesNo <- "dest"
          } 
      }
      
      return_graph <- dateFtoUse %>% 
        leaflet() %>%
        addProviderTiles("CartoDB.PositronNoLabels")
      
      if(is_null(input$map_shape_click$id) || (type == "originMap" || type == "destMap") || is_null((type))) {
            return_graph <- return_graph %>% 
              addPolygons(color = "black",
                          label = ~new.country,
                          layerId = layerIDVar,
                          fillColor = myfillCol,
                          fillOpacity = 1,
                          opacity = 1,
                          weight = 1,
                          stroke = 1) %>%  setView(65.25,10, 2)
            
            #to show the legend for the origin map
            if(legendYesNo == "origin") {
              return_graph <- return_graph %>% setView(60.25,-10, 2)  %>% #Sets view to center
                addLegend("bottomleft", pal = color, values = ~export.value,
                          title = "Total Value (Guldens) Exported",
                          na.label = "No Exports",
                          labFormat = labelFormat(suffix = "g"),
                          opacity = 1)
            } else if (legendYesNo == "dest"){
              return_graph <- return_graph %>% 
                addLegend("bottomleft", pal = color2, values = ~import.value,
                           title = "Total Value (Guldens) Imported",
                           na.label = "No Imports",
                           labFormat = labelFormat(suffix = "g"),
                           opacity = 1)
            } 
      } 
      if(!is_null(input$map_shape_click) && ((type != "originMap" && type != "destMap") || is_null(type) )) {
          return_graph <- return_graph %>% 
            addPolygons(color = "black",
                        label = ~new.country,
                        layerId = ab@data$new.country,
                        fillColor = ifelse(input$map_shape_click$id == ab@data$new.country, "red", "black" ),
                        fillOpacity = 1,
                        opacity = 1,
                        weight = 1,
                        stroke = 1) %>%
            setView(65.25,10, 2)
        } 
       
      
      #This adds the lines for the textile
      if(!is_null(location)) {
        print(paste(input$map_shape_click$id,input$inputChoice_two))
        mayb <- assign3 %>%
          filter(orig_loc_region_arch == input$map_shape_click$id & textile_name == input$inputChoice_two)
        
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
                                        label = paste(input$map_shape_click$id, maybe$orig_loc_region_arch),
                                        weight = 1,
                                        color = "purple") %>%
            addLegend("bottomleft",colors = c("red", "blue"), labels = c("Sent", "Recieved"),
                      title = " ",
                      na.label = "No Exports",
                      labFormat = labelFormat(suffix = "g"),
                      opacity = 1
            ) 
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
               tags$div(class = "map",
                         leafletOutput(outputId = "map", width = "100%", height = "100%")
               ),
               tags$div(class = "plots",
                        plotlyOutput(outputId = "plot", width = "100%", height = "100%")
                        ),
               tags$div(class = "options", 
                        tags$div(class = "inlineOptions",
                                 tags$label("Selected Country: "),
                                 
                                 textOutput(outputId = "selectedCountry")
                        ),
                        tags$div(class = "inlineOptions",
                                 selectInput(inputId = "inputChoice",
                                             label = "Choose identifier!",
                                             choices = c("Export Data", "Company (WIC/VOC)", "Origin", "Destination", "Year", "Color", "Pattern"))
                                 ),
                        tags$div(class = "inlineOptions",
                                 selectInput(inputId = "inputChoice_two",
                                             label = "Choose what you would like to graph!",
                                             choices = NULL)
                                 ),
                        
               ),
        ),
      
       
        leafletOutput(outputId = "dropdown"),
        verbatimTextOutput("selection")
)

server <- function(input, output, session) {
  output$selectedCountry <- renderText({
    if(is_null(input$map_shape_click$id)) {
      "Please Select a Location"
    } else {
      input$map_shape_click$id
    }
  })
    
    #the third dropdown with the textile names!!
    output$dropdown <- renderLeaflet({
        switch_func(input,session)
    })
    
    reset_map(output,input, NULL, NULL)
    #the bottom plots
    output$plot <- renderPlotly({
      print(input$map_shape_click$id)
      
      
      
      if(is.null(input$map_shape_click$id)){
        
        #something to show before a location is clicked
        #not workign yet...
        df <- data.frame(
          label=c("No available data"),
          x = c(1.5), y =c(1.5))
        g <- ggplot(df, aes(x=x, y=y, label=label)) + geom_text(mapping = aes(x = x, y = y), size = 10)
        ggplotly(g)
        # Graph of textile name and the value that was sent, filled with something(quantity?)
        
        
        
      }
      #if the input is a textile... graph it
      if(input$inputChoice_two %in% unique(assign3$textile_name)) {
        print("in here")
        reset_map(output,input, input$map_shape_click$id, NULL)
        data <-  assign3 %>%
          filter(orig_loc_region_arch == input$map_shape_click$id)%>%
          filter(textile_name == input$inputChoice_two )
        print(paste("count: ", count(data)))
        if(count(data) == 0) {
          df <- data.frame(
            label=c("No available data"),
            x = c(1.5), y =c(1.5))
          ggplot(df, aes(x=x, y=y, label=label)) + geom_text(mapping = aes(x = x, y = y), size = 10)
        } else {
          data %>%
            ggplot() +
            geom_col(aes(x = dest_yr, y = as.numeric(real_quantity)),
                     fill = "#FB8B24") +
            theme_bw() +
            labs(title = paste("The quantity of", input$inputChoice_two, "from", input$map_shape_click$id, "exported by year"), x ="Year", y = "Total Quantity")
          
        }
        } 
      else if(input$inputChoice_two %in% unique(assign3$textile_color_arch)) {
        reset_map(output,input, input$map_shape_click$id, NULL)
        print("in this loop")
        data <- assign3 %>%
          filter(orig_loc_region_arch == input$map_shape_click$id)%>%
          filter(textile_color_arch == input$inputChoice_two)
        print(paste("count: ", count(data)))
        if(count(data) == 0) {
          df <- data.frame(
            label=c("No available data"),
            x = c(1.5), y =c(1.5))
          ggplot(df, aes(x=x, y=y, label=label)) + geom_text(mapping = aes(x = x, y = y), size = 10)
        } else {
          data %>%
            ggplot()+
            geom_histogram(mapping = aes(x = as.numeric(real_quantity)),
                           bins = 25,
                           color = "black",
                           fill = "black"
            ) +
            labs(title = paste("Distribution of shipment sizes based on", input$inputChoice_two, "from", input$map_shape_click$id), x = "Textile quantity (singular shipment)") +
            xlim(0,5000)
          
        }
      }else if(input$inputChoice_two %in% unique(assign3$textile_pattern_arch)) {
        reset_map(output,input, input$map_shape_click$id, NULL)
        data <- assign3 %>%
          filter(orig_loc_region_arch == input$map_shape_click$id)%>%
          filter(textile_pattern_arch == input$inputChoice_two)
        print(paste("count: ", count(data)))
        if(count(data) == 0) {
          df <- data.frame(
            label=c("No available data"),
            x = c(1.5), y =c(1.5))
          ggplot(df, aes(x=x, y=y, label=label)) + geom_text(mapping = aes(x = x, y = y), size = 10)
        } else {
          data %>%
            ggplot()+
            geom_histogram(mapping = aes(x = as.numeric(real_quantity)),
                           bins = 25,
                           color = "black",
                           fill = "black"
            ) +
            labs(title = paste("Distribution of shipment sizes based on", input$inputChoice_two, "from", input$map_shape_click$id), x = "Textile quantity (singular shipment)") +
            xlim(0,5000)
          
        }
      }
      else {
          switch(input$inputChoice_two,
                   "All" = {
                       #Change graph back to normal
                        reset_map(output,input, NULL, NULL)
                     

                       
                   g<- assign3 %>% 
                           group_by(orig_loc_port_arch, textile_name) %>% 
                           filter(piece_rate < 20) %>% #For now, filtering by cheap pieces
                           filter(orig_loc_region_arch == input$map_shape_click$id) %>%
                           summarise(total_value = sum(real_guldens))%>%
                           ggplot()+
                           geom_tile(aes(x = orig_loc_port_arch, y= textile_name, fill = total_value))+

                           labs(title = paste("A chart of all exports from", input$map_shape_click$id, "ports") ,x = "Origin Port", y = "Textile") + 
                           guides(fill=guide_legend(title="Mean Value per Piece")) +
                           scale_fill_gradient(low = "#460B2F", high = "#E36414", na.value = NA) +
                            theme(plot.title = element_text(size = 10),
                                  axis.title.x = element_text(size = 10),  
                                  axis.title.y = element_text(size = 10)  
                                  ) 


                   ggplotly(g)
                   

                   
                   },
                 "All Colors" = {
                   #Change graph back to normal
                   reset_map(output,input, NULL, NULL)
                   
                   
                   
                   g<- assign3 %>%
                     ggplot()+
                     geom_histogram(mapping = aes(x = as.numeric(real_quantity)),
                                    bins = 25,
                                    color = "black",
                                    fill = "black"
                     ) +
                     labs(title = paste("Distribution of shipment sizes based on", input$inputChoice_two, "from", input$map_shape_click$id), x = "Textile quantity (singular shipment)") +
                     xlim(0,5000)
                   
                   ggplotly(g)
                   
                   
                   
                 },
                 "All Patterns" = {
                   #Change graph back to normal
                   reset_map(output,input, NULL, NULL)
                   
                   
                   
                   g<-assign3 %>%
                     ggplot()+
                     geom_histogram(mapping = aes(x = as.numeric(real_quantity)),
                                    bins = 25,
                                    color = "black",
                                    fill = "black"
                     ) +
                     labs(title = paste("Distribution of shipment sizes based on", input$inputChoice_two, "from", input$map_shape_click$id), x = "Textile quantity (singular shipment)") +
                     xlim(0,5000)
                   
                   
                   ggplotly(g)
                   
                   
                   
                 },


                   "Both" ={
                       #Change the graph back to normal
                       reset_map(output,input, NULL, NULL)
                       
                       assign3 %>%
                           filter(orig_loc_region_arch == input$map_shape_click$id)%>%
                           ggplot() + 
                           geom_col(aes(x = dest_yr, y = as.numeric(real_quantity)),
                                    fill = "#FB8B24") +
                           theme_bw() +
                          labs(title = paste("Quanity of Textiles Shipped out of",input$map_shape_click$id ,"by Both VOC/WIC"), x ="Year", y = "Total Quantity")
                   },
                   
                   "WIC" = {
                      reset_map(output,input, NULL, NULL)
                      
                     dataforWIC <- assign3 %>%
                       filter(orig_loc_region_arch == input$map_shape_click$id)%>%
                       filter(company == "WIC")
                     
                     if(nrow(dataforWIC) == 0) {
                       df <- data.frame(
                         label=c("No available data"),
                         x = c(1.5), y =c(1.5))
                       ggplot(df, aes(x=x, y=y, label=label)) + geom_text(mapping = aes(x = x, y = y), size = 10)
                     } else {
                       dataforWIC %>%
                         ggplot() + 
                         geom_col(aes(x = dest_yr, y = as.numeric(real_quantity)),
                                  fill = "#FB8B24") +
                         theme_bw() +
                         labs(title = paste("Quanity of Textiles Shipped out of",input$map_shape_click$id ,"by the", input$inputChoice_two, "Company"), x ="Year", y = "Total Quantity") 
                       
                     }
                       
                      
                       
                   },
                   
                   "VOC" = {
                     #Change the graph back to normal
                     reset_map(output,input, NULL, NULL)
                     
                     dataforVOC <- assign3 %>%
                       filter(orig_loc_region_arch == input$map_shape_click$id)%>%
                       filter(company == "VOC") 
                     
  
                     if(nrow(dataforVOC) == 0) {
                       df <- data.frame(
                         label=c("No available data"),
                         x = c(1.5), y =c(1.5))
                       ggplot(df, aes(x=x, y=y, label=label)) + geom_text(mapping = aes(x = x, y = y), size = 10)
                     } else {
                       dataforVOC %>%
                            ggplot() + 
                             geom_col(aes(x = dest_yr, y = as.numeric(real_quantity)),
                                      fill = "#FB8B24") +
                             theme_bw() +
                            labs(title = paste("Quanity of Textiles Shipped out of",input$map_shape_click$id ,"by the", input$inputChoice_two, "Company"), x ="Year", y = "Total Quantity")
                     }  
                   },
                   
                   
                   "Origin" = {
                      
                         reset_map(output,input,NULL, "originMap")
                          if(is_null(input$map_shape_click$id)) {
                            return()
                          } else {
                            g<- assign3 %>% 
                              group_by(orig_loc_port_arch, textile_name) %>% 
                              filter(piece_rate < 20) %>% #For now, filtering by cheap pieces
                              filter(orig_loc_region_arch == input$map_shape_click$id) %>%
                              summarise(total_value = sum(real_guldens))%>%
                              ggplot()+
                              geom_tile(aes(x = orig_loc_port_arch, y= textile_name, fill = total_value))+
                              
                              labs(title = paste("A chart of all exports from", input$map_shape_click$id, "ports") ,x = "Origin Port", y = "Textile") + 
                              guides(fill=guide_legend(title="Mean Value per Piece")) +
                              scale_fill_gradient(low = "#460B2F", high = "#E36414", na.value = NA) +
                              theme(plot.title = element_text(size = 10),
                                    axis.title.x = element_text(size = 10),  
                                    axis.title.y = element_text(size = 10)) 
                            
                            ggplotly(g)
                          }
                      }
                   ,
                   
                   
                   "Destination" = {
                       
                        reset_map(output,input,NULL, "destMap")
                        if(is_null(input$map_shape_click$id)) {
                          return()
                        } else {
                          g<- assign3 %>%
                            group_by(dest_loc_port_arch, textile_name) %>%
                            filter(piece_rate < 20) %>% #For now, filtering by cheap pieces
                            filter(dest_loc_region_arch == input$map_shape_click$id) %>%
                            summarise(total_value = sum(real_guldens))%>%
                            ggplot()+
                            geom_tile(aes(x = dest_loc_port_arch, y= textile_name, fill = total_value))+
                            labs(title = paste("A chart of all imports to", input$map_shape_click$id, "ports") ,x = "Origin Port", y = "Textile") +
                            guides(fill=guide_legend(title="Mean Value per Piece")) +
                            scale_fill_gradient(low = "#460B2F", high = "#E36414", na.value = NA) +
                            theme(plot.title = element_text(size = 10),
                                  axis.title.x = element_text(size = 10),
                                  axis.title.y = element_text(size = 10))

                          ggplotly(g)
                        }
                       
                       },
                 
                   
                   
                   "Year" = {
                       
                     reset_map(output,input, NULL, NULL)

                     require(scales)
                     temp <- assign3 %>%
                       filter(orig_loc_region_arch == input$map_shape_click$id)
                       
                     if(nrow(temp) == 0) {
                       df <- data.frame(
                         label=c("No available data"),
                         x = c(1.5), y =c(1.5))
                       ggplot(df, aes(x=x, y=y, label=label)) + 
                         geom_text(mapping = aes(x = x, y = y), size = 10)+
                         labs(x= "",y="")
                         
                     } else {
                       temp2 <- aggregate(as.numeric(temp$real_quantity), by = list(year = temp$orig_yr), FUN = sum,na.rm=TRUE)

                     
                     ggplot(data = temp2) +
                       geom_col(mapping = aes(x = as.factor(year), 
                                               y = x,
                                               fill = "#FB8B24",
                       )) +
                       theme(legend.position = "none") +
                       labs(title = "Total Quantity of All Textiles Shipped", x = "Year", y = "Textile Quantity") +
                       scale_fill_manual(values=c("#FB8B24"))
                     }
                   },
                  "Color" = {
                   reset_map(output,input, NULL, NULL)
                    
                    assign3$textile_color_arch = toupper(assign3$textile_color_arch)
                    assign3$textile_color_arch = str_replace(assign3$textile_color_arch, "BROWN-BLUE", "BROWN BLUE")
                    assign3$textile_color_arch = str_replace(assign3$textile_color_arch, "BLUE / AZURE", "BLUE")
                   
                   #HISTOGRAM FOR COLOR DISTRIBUTION
                 assign3 %>%
                     ggplot()+
                     geom_histogram(mapping = aes(x = as.numeric(real_quantity)),
                                    bins = 100,
                                    color = "black",
                                    fill = "black"
                                    ) +
                     labs(title = paste("Distribution of shipment sizes based on", "Textile Color"), x = "Textile quantity (singular shipment)") +
                     xlim(0,5000)
                     # facet_wrap(~(textile_color_arch)
                  },
                 "Pattern" = {
                   
                 }, 
                 {
                  df <- data.frame(
                  label=c("No available data"),
                  x = c(1.5), y =c(1.5))
                  g <- ggplot(df, aes(x=x, y=y, label=label)) + geom_text(mapping = aes(x = x, y = y), size = 10)
                  ggplotly(g)
              }

                   
                 
          )}
    
    })
}
#live loading of shiny app
options(shiny.reactlog= TRUE)
options(shiny.autoreload = TRUE)
shinyApp(ui, server)




#options(shiny.reactlog= TRUE)



