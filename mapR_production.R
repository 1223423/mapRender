library(ggplot2)
library(maps)
library(mapdata) 
library(dplyr)
library(stringr)
library(showtext)
library(sp)
library(geosphere)
library(ggfittext)
library(usmap)
library(lubridate)
library(albersusa)

# 1. Initialization

## Load label data
df_all <- read.csv('alldata.csv')
df_all$period_begin <- dmy(df_all$period_begin)

# Load color data
df_colors <- read.csv('colors.csv')
names(df_colors)[1] <- 'search_word'

# Load composite US map, change columns to suitable names
us <- usa_composite(proj="longlat")
us <- fortify(usa_composite(proj="longlat"), region = "name")
names(us)[6] <- 'region'
us$region <- str_to_lower(us$region)

# Calculate state centroids
centroids <- us %>% 
  group_by(region) %>% 
  group_modify(~ data.frame(centroid(cbind(.x$long, .x$lat))))


# Load  custom fonts
font_add(family = "Coolvetica", regular = "./fonts/coolvetica.otf")
font_add(family = "CoolveticaCD", regular = "./fonts/coolvetica condensed rg.otf")
showtext_auto()

# Generate boundaries and adjustments
boundaries <- read.csv('./data/boundaries.csv')
df_adjustments <- read.csv('./data/adjustments.csv')


# Calculate boundary adjustments
for(i in 1:nrow(df_adjustments)){
  reg <- df_adjustments[i,'region']
  idx <- which(boundaries$region == reg, arr.ind = T)
  boundaries[idx,]$minlat <- boundaries[idx,]$minlat + df_adjustments[i,]$minlat
  boundaries[idx, 'minlong'] = boundaries[idx, 'minlong'] + df_adjustments[i,'minlong']
  boundaries[idx, 'maxlat'] = boundaries[idx, 'maxlat'] + df_adjustments[i,'maxlat']
  boundaries[idx, 'maxlong'] = boundaries[idx, 'maxlong'] + df_adjustments[i,'maxlong']
}


tiny_states <- read.csv('./data/tinystates.csv')

# 2. Frameloop

frames = unique(df_all$period_begin)
nframes = length(frames)

for(i in 1:nframes) {
  
  print(paste("Rendering frame",i,"of",nframes))
  # Date label
  current = ymd(as.character(frames[i]))
  #current = ymd(frames[i])
  date_label <- paste(day(current), month(current, label = T), year(current))
  
  # Subset current frame data
  df_labels <- df_all[df_all$period_begin == frames[i],]
  names(df_labels)[3] <- "region"
  df_labels$region <- str_to_lower(df_labels$region)
  
  # Link label datasets to color by search_world
  df_labels <-  left_join(df_labels, df_colors, by = 'search_word')
  
  # Impute missing colors as red
  df_labels[is.na(df_labels$Color),'Color'] = "red"
  
  # Link us composite map with color via region  
  us_comp <- left_join(us, df_labels[,c('region', 'Color')], by = 'region')
  
  # Link labels and centroids by region
  df_us_comp <- merge(df_labels, centroids, by = 'region')
  
  # Link poly map with adjusted boundaries by region
  df_us_comp <- merge(df_us_comp, boundaries, by = 'region')
  
  # Link tiny states to their labels by region
  tiny_states_comp <- merge(tiny_states, df_labels, by = 'region')
  
  # Add right spacing to tiny state labels
  
  tiny_states_comp$search_word  <- paste(" ", tiny_states_comp$search_word)
  
  # If the state is tiny, don't plot it over the state polygon
  df_us_comp[df_us_comp$region %in% tiny_states_comp$region,]$search_word = " "
  
  # Render frame
  p <- ggplot(data=us_comp, aes(x=long, y=lat, fill=Color)) + 
    geom_polygon(aes(group=group),color = "white") + 
    #geom_point(data = centroids, aes(x=lon, y=lat), color = "red") +
    #geom_rect(data = boundaries, fill = NA, color = "black", aes(NULL,NULL, xmin = minlong, xmax = maxlong, ymin = minlat, ymax = maxlat)) +
    #geom_text(data = df_us, aes(x=lon, y=lat, label = search_word, family = "CoolveticaCD", fontface = "plain")) +
    geom_fit_text(data = df_us_comp, grow = TRUE, reflow = TRUE, min.size = 20, aes(x=lon, y=lat, xmin = minlong, xmax = maxlong, ymin = minlat, ymax = maxlat, label = search_word, family = "CoolveticaCD", fontface = "plain")) +
    guides(fill=FALSE) + 
    theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
          panel.background = element_blank()) + 
    #ggtitle('U.S. Map with States') + 
    geom_segment(data = tiny_states_comp, aes(x = lon, y = lat, xend = lon1, yend = lat1), size = 3) +
    annotate("text", size = 20, x = tiny_states_comp$lon1, y = tiny_states_comp$lat1, label = tiny_states_comp$search_word, hjust = 0, family = "CoolveticaCD", fontface = "plain") +
    annotate("text", size = 64, x = -75, y = 25, label = date_label, hjust = 0, family = "CoolveticaCD", fontface = "plain") +
    scale_x_continuous(breaks = c(NULL), limits = c(-126,-60)) + 
    coord_fixed(ratio = 1.4)
  
  # Save frame to disk
  ggsave(paste('./output/frame',i,'.png',sep =""), plot = p, device = "png", dpi = 72, scale = 1, width = 1920*(14/5), height = 1080*(14/5), units = "px", limitsize = F)
  print("Done")
}

