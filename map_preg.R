# Data manip
library(dplyr)
library(stringr) # string to lower case

# Plotting
library(ggplot2)
library(ggfittext) # scaleable bounding boxes for text
library(showtext) # custom fonts
library(av) # video encoder

# Geospatial
library(albersusa) # US composite map by state
library(geosphere) # easy centroid calculations

# Load pregnancy data and select relevant columns; here I pick the rate for 20-24 year olds
# Then remove the aggregated US data and round the values

# This will be the variable that shows up in each state
var_visualized = 'pregnancyrate2024' # you can change this to any other var

preg <- read.csv('pregenerant.csv')
preg <- preg[,c('state','year',var_visualized)]
preg <- preg[preg$state != 'US',]
preg[,var_visualized] <- round(preg[,var_visualized] ,0)

# Load the composite map of US states
us <- usa_composite(proj="longlat")
us <- fortify(usa_composite(proj="longlat"), region = "name")

# The state indicators in our data and in the map data will need to match;
# we use some functions to generate abbreviations e.g. "Wisconsin" to "WI"
# and change them in the map data to match our pregnancy data
names(us)[6] <- 'region'
us$region <- str_to_lower(us$region)
abbreviations <- state.abb[match(us$region, tolower(state.name))]
abbreviations[is.na(abbreviations)] = "DC" # DC needs to be manually fixed
us$state <- abbreviations

# Calculate state centroids. This is to tell each label where it's state is
# and to correctly place the bounding boxes for the labels (see below).
centroids <- us %>% 
  group_by(region) %>% 
  group_modify(~ data.frame(centroid(cbind(.x$long, .x$lat))))
centroids$state <- state.abb[match(centroids$region, tolower(state.name))]

# Load custom fonts
font_add(family = "Coolvetica", regular = "./fonts/coolvetica.otf")
font_add(family = "CoolveticaCD", regular = "./fonts/coolvetica condensed rg.otf")
showtext_auto()

# Generate boundaries
# This is for the bounding boxes that contain the text on the plot for each state
# allowing it to fill the most space without bleeding into other states
boundaries <- read.csv('./data/boundaries.csv')
boundaries$state <- state.abb[match(boundaries$region, tolower(state.name))]

# If you want to see exactly what the bounding boxes are doing, go to the ggplot code below
# and uncomment the geom_rect() layer. You should see the boxes in the output now

# Generate tiny state data: Tiny states are too small to have their own labels, e.g. new hampshire,
# on top of the state, so for these I want to draw lines that go outside of the actual map
# and put the label there
tiny_states <- read.csv('./data/tinystates.csv')
tiny_states$state <- state.abb[match(tiny_states$region, tolower(state.name))]
tiny_states$state[3] <- "DC"

# Calculate the amount of frames our animation will have (one for each year of data)
# Notice some years can be skipped this way if data is missing
frames = unique(preg$year)
nframes = length(frames)

# This function will 'render' the animation by creating a plot for each year and
# printing it. We feed this function to a video encoder below which will capture
# the printed plots into a movie
render <- \() {
  for(i in 1:nframes) {
    
    print(paste("Rendering frame",i,"of",nframes))
    
    # Set current year label
    current = as.character(frames[i])
    date_label <- paste(preg$year[i])
    
    # We take a subset from the entire data for this particular year
    df_labels <- preg[preg$year == frames[i],]
    df_labels <- df_labels[,c('state',var_visualized)]
    preg_y <- preg[preg$year == frames[i],]
    
    # Merge US composite map with our pregnancy data via state
    # This is why we homogenized the state labels before
    us_comp <- merge(preg_y[,c('state', var_visualized)],us, by = 'state')
    
    # Now again merge the map and data with the generated centroids by state
    # These will tell the plot where the center of each state is
    df_us_comp <- merge(df_labels ,centroids, by = 'state')
    df_us_comp[,var_visualized] <- round(df_us_comp[,var_visualized] ,0)
    
    # Merge the map,data,centroid dataframe with the boundary boxes
    # for the labels
    df_us_comp <- merge(df_us_comp, boundaries, by = 'state')
    
    # Finally, we create a separate dataframe for the tiny states, since their
    # labels won't be drawn on top of the state. We need to handle these differently
    tiny_states_comp <- merge(tiny_states, df_labels, by = 'state')

    # Therefore we remove the tiny states from the main merged dataframe
    df_us_comp[df_us_comp$state %in% tiny_states_comp$state,][,var_visualized] = NA
    
    # Create the map plot
    p <- ggplot(data=us_comp, aes(x=long, y=lat, fill=pregnancyrate2024)) + 
      geom_polygon(aes(group=group),color = "white") +
      geom_fit_text(data = df_us_comp, grow = TRUE, reflow = TRUE, min.size = 3,
                    aes(x=lon, y=lat, xmin = minlong, xmax = maxlong, ymin = minlat, ymax = maxlat,
                        label = pregnancyrate2024, family = "CoolveticaCD", fontface = "plain")) +
      scale_fill_viridis_c(option = "turbo", limits = range(preg[,var_visualized], na.rm = T)) +
      theme(axis.title.x=element_blank(),axis.text.x=element_blank(), 
            axis.ticks.x=element_blank(),axis.title.y=element_blank(), 
            axis.text.y=element_blank(), axis.ticks.y=element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(),panel.background = element_blank()) +
      ggtitle('Pregnancy Rates are Declining in Young Adult Women (20-24) in the US') +
      geom_segment(data = tiny_states_comp, aes(x = lon, y = lat, xend = lon1, yend = lat1), size = .5) +
      # geom_rect(data = boundaries, fill = NA, color = "red", linetype = 'dashed', aes(NULL,NULL, xmin = minlong, xmax = maxlong, ymin = minlat, ymax = maxlat)) +
      annotate("text", size = 8, x = tiny_states_comp$lon1, y = tiny_states_comp$lat1, 
               label = tiny_states_comp[,var_visualized], hjust = 0, 
               family = "CoolveticaCD", fontface = "plain") +
      annotate("text", size = 24, x = -75, y = 25, label = frames[i], hjust = 0, 
               family = "CoolveticaCD", fontface = "plain") +
      labs(fill = "Pregnancy Rates (per 1,000 women)") +
      theme(legend.title = element_text(size = 12)) + 
      scale_x_continuous(breaks = c(NULL), limits = c(-126,-60)) +
      coord_fixed(ratio = 1.4)
    # Some additional styling
    p <- p +
      theme(legend.position = "top",
            legend.direction = "horizontal",
            legend.box = "horizontal",
            legend.box.just = "top",
            legend.margin = margin(t = 0, b = 0, r = 0, l = 0, unit = "cm"),
            legend.title.align = 0.5,
            legend.text.align = 0.5,
            axis.title.y = element_blank(),
            plot.title = element_text(size = 24, hjust = 0.5, family = "CoolveticaCD"))
    
    print(p)
  }
}

# Wrap the render function for easy capture
makeplot <- function(){
  render()
}

# Feed the wrapped render function into video encoder and output
video_file <- file.path( './', 'output.mp4')
av_capture_graphics(makeplot(), video_file, 3840, 2160, res = 320, vfilter = "null", framerate = .5)
av::av_media_info(video_file)
cat('Output saved in folder.')


# Hope this is useful:)