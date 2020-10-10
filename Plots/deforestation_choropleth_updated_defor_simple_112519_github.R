# Load libraries

libs <- c("dplyr",
          "tidyr",
          "ggplot2",
          "MASS",
          "ggrepel",
          "grid",
          "rgdal", 
          "maptools", 
          "gridExtra",
          "countrycode",
          "raster",
          "plyr",
          "tm",
          "ggpubr")

lapply(libs, require, character.only = TRUE)

#' Note: data witheld because they contain third party content

setwd("C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Deforestation review/Deforestation_messaging_analysis_GitHub/Deforestation_messaging_analysis/")

# Skip first row to stop R from reading headers as data

extent <- read.csv("Data/tree_cover_stats_2016_extent.csv",skip=1)



# Preprocess loss and extent tables

# Find index for percent canopy cover class
# in tree cover loss and extent data

index_extent <- grep("X.10.",colnames(extent))

# Subset data by percent canopy cover class

extent <- extent[,c(1,index_extent)]

# Rename loss and extent columns

colnames(extent)[2] <- "forest_cover_2000"

# Add column with ISO 3-character country names

extent$iso3c <- countrycode(extent$Country,"country.name","iso3c")


# Remove duplicate country codes from the GFW data to prevent join errors

# Find duplicate country code

extent_code_duplicate <- labels(which(table(extent$iso3c)>=2))

# Manually find duplicate index

extent[extent$iso3c==extent_code_duplicate,]

# Remove duplicate row

extent <- extent[-166,]



# Import tree cover loss data

loss <- read.csv("Data/hansen_mask_zonal.csv")



scale <- function(x) round((x * 934.7356 / 1000))

loss_sub <- loss %>% dplyr::select(ISO_A3,HISTO_1:HISTO_17) %>%
  mutate(total_loss=rowSums(dplyr::select(.,HISTO_1:HISTO_17))) %>%
  mutate_at(vars(-ISO_A3), scale) %>%
  dplyr::select(ISO_A3,total_loss) %>%
  filter(ISO_A3 != -99)



# Load countries polygon

# Download from
# http://data.okfn.org/data/datasets/geo-boundaries-world-110m
# listed in the leaflet colors tutorial
# https://rstudio.github.io/leaflet/colors.html

countries <- readOGR("Data/countries.geojson")

# There is no total forest loss data for Somaliland because it's not
# a country in the GFW data. Merge Somalia and Somaliland to prevent 
# "NA" from showing up in the legend of the map of total forest loss by country.

# Save the countries data frame separately

countries_df <- countries@data

# Change Somaliland admin name to Somalia

countries$admin[countries$admin=="Somaliland"] <- "Somalia"

# Merge polygons by admin

countries <- unionSpatialPolygons(countries, countries$admin)

# Remove Somaliland from the countries data frame

countries_df <- countries_df[countries_df$admin!="Somaliland",]

# Match the data frame and polygon row names

rownames(countries_df) <- countries_df$admin

# Add the data frame back to the polygon

countries <- SpatialPolygonsDataFrame(countries,countries_df)



# Make country polygon names consistent
# with country name frequency counts in Mongabay and
# academic literature

# Replace multi-word country names in the tropics with abbreviated names

countries$admin <- tolower(countries$admin)

countriesLong <- c("united republic of tanzania",
                   "democratic republic of the congo",
                   "republic of congo",
                   "trinidad and tobago",
                   "east timor")

countriesShort <- c("tanzania",
                    "drc",
                    "congo",
                    "trinidad",
                    "timor")

for(i in seq(countriesLong))
{
  countries$admin <- gsub(countriesLong[i],countriesShort[i], countries$admin)   
}

# Reorder countries alaphabetically

countries <- countries[order(countries$admin),]

# Remove spaces from countries with multi-word names

countries$admin <- gsub(" ", "", countries$admin)



# Join total tree cover loss to countries polygon data frame by ISO code

countries@data <- left_join(countries@data,loss_sub,by=c("iso_a3" = "ISO_A3"))

countries@data <- left_join(countries@data,extent,by=c("iso_a3" = "iso3c"))

# Subset countries in the tropics

countriesSub <- countries[(coordinates(countries)[,2]>(-24) & coordinates(countries)[,2]<(24)),]

# Subset countries with tree cover extent >1000 ha

# countriesSub <- countriesSub[countriesSub$forest_cover_2000>100000 | is.na(countriesSub$forest_cover_2000),]

# Calculate country areas - delete code once you
# check country size versus coverage plots

countriesSub$area_sqkm <- raster::area(countriesSub) / 10^6



countriesSub@data[countriesSub$admin=="southsudan",c("total_loss","forest_cover_2000","area_sqkm")]



# Combine statistics of countries which can get confused with each other

confusion_names <- c("drc","southsudan")

original_names <- c("congo","sudan")

for (i in seq(original_names)) {
  
  original_data <- countriesSub@data[countriesSub$admin==confusion_names[i],c("total_loss","forest_cover_2000","area_sqkm")]
  
  confusion_data <- countriesSub@data[countriesSub$admin==original_names[i],c("total_loss","forest_cover_2000","area_sqkm")]
  
  combined_data <- rbind(original_data,confusion_data)
  
  summed_data <- apply(combined_data,2,sum)
  
  countriesSub@data[countriesSub$admin==original_names[i],c("total_loss","forest_cover_2000","area_sqkm")] <- summed_data
  
}



# Combine polygons of countries which can get confused with each other

# Save the countriesSub data frame separately

countriesSub_df <- countriesSub@data

# Change country admin name to name of country it can be confused with

countriesSub$admin[countriesSub$admin=="drc"] <- "congo"

countriesSub$admin[countriesSub$admin=="southsudan"] <- "sudan"

# Merge polygons by admin

countriesSub <- unionSpatialPolygons(countriesSub, countriesSub$admin)

# Remove countries that can be confused with others from the data frame

countriesSub_df <- countriesSub_df[countriesSub_df$admin!="drc",]

countriesSub_df <- countriesSub_df[countriesSub_df$admin!="southsudan",]

# Match the data frame and polygon row names

rownames(countriesSub_df) <- countriesSub_df$admin

# Add the data frame back to the polygon

countriesSub <- SpatialPolygonsDataFrame(countriesSub,countriesSub_df)



# Stem the country admin names

docs <- VCorpus(VectorSource(countriesSub$admin))

docs <- tm_map(docs, stemDocument)

countries_stemmed_list <- lapply(docs, as.character)

countries_stemmed_vector <- unlist(countries_stemmed_list, use.names=FALSE)

countriesSub$admin <- countries_stemmed_vector



writeOGR(countriesSub,"Data/countriesSub.geojson","countriesSub",driver = "GeoJSON",overwrite_layer = TRUE)



# Load Mongabay and deforestation literature country counts

load("Data/mongabay_simple.Rdata")

load("Data/literature_simple.Rdata")



# Join the tree cover loss and extent data with the country counts

# Create data frames from the countries data with relevant columns

countriesSub_df <- data.frame(country=countriesSub$admin,
                              total_loss=countriesSub$total_loss,
                              region=countriesSub$region_un,
                              area=countriesSub$area_sqkm,
                              forest_cover=countriesSub$forest_cover_2000,
                              iso=countriesSub$iso_a3)

monga_df <- data.frame(country=mongabay$admin,
                       monga_count=mongabay$count)

lit_df <- data.frame(country=literature$admin,
                            lit_count=literature$count)

# Join data frames by country

join <- left_join(countriesSub_df,monga_df,by="country")

join <- left_join(join,lit_df,by="country")

join <- join %>% drop_na()


join <- filter(join,total_loss>0)

# Convert ha to sq km

join$forest_cover <- join$forest_cover/100

join$proportional_loss <- join$total_loss/join$forest_cover


# Write joined table

write.table(join,"Data/join_table")

join <- read.table("Data/join_table")


# Run binomial regressions of country count versus total tree cover loss 

monga_total_loss_glm <- glm(monga_count~log(total_loss),family = poisson,data=join)

lit_total_loss_glm <- glm(lit_count~log(total_loss),family = poisson,data=join)

# Check for overdispersion

monga_total_loss_glm$deviance/monga_total_loss_glm$df.residual

lit_total_loss_glm$deviance/lit_total_loss_glm$df.residual

# Apply a negative binomial error distribution since all models are overdispersed

monga_total_loss_glm_nb <- glm.nb(monga_count~log(total_loss+0.01),link = "log",data=join)

lit_total_loss_glm_nb <- glm.nb(lit_count~log(total_loss+0.01),link = "log",data=join)

# Run chi-square tests comparing full models and null models
# to check for regression significance

monga_total_loss_glm_nb_null <- glm.nb(monga_count~1,link = "log",data=join)

lit_total_loss_glm_nb_null <- glm.nb(lit_count~1,link = "log",data=join)

monga_total_loss_glm_nb_anova <- anova(monga_total_loss_glm_nb_null,monga_total_loss_glm_nb,test='Chisq')

lit_total_loss_glm_nb_anova <- anova(lit_total_loss_glm_nb_null,lit_total_loss_glm_nb,test='Chisq')

# Organize chi-square results in a data frame

anovas <- list(monga_total_loss_glm_nb_anova,lit_total_loss_glm_nb_anova)

anovas_df <- data.frame("Chi_sq"=rep(0,2), p=rep(0,2))

for (i in 1:2) {
  
  chi_sq <- round(anovas[[i]]$"LR stat."[2],2)
  
  anovas_df[i,1] <- chi_sq
  
  chi_signif <- paste(anovas[[i]]$"Pr(Chi)"[2])
  
  anovas_df[i,2] <- chi_signif
  
}

# Calculate pseudo R-squard values for regressions

monga_total_loss_r_sq <- paste(round((monga_total_loss_glm_nb$null.deviance - monga_total_loss_glm_nb$deviance) / (monga_total_loss_glm_nb$null.deviance), digits=2))

lit_total_loss_r_sq <- paste(round((lit_total_loss_glm_nb$null.deviance - lit_total_loss_glm_nb$deviance) / (lit_total_loss_glm_nb$null.deviance), digits=2))



# Find which countries are in the 95% confidence interval
# of the country count versus total tree cover loss regressions

# https://stackoverflow.com/questions/31959961/labeling-values-in-ggplot-outside-geom-smooth-threshold

# Generate predictions from each regression and calculate 
# the upper and lower bounds of the confidence intervals

monga_loss_preds <- predict(monga_total_loss_glm_nb, type="link", se=TRUE)

monga_loss_fit <- exp(monga_loss_preds$fit)

monga_loss_UL <- exp(monga_loss_preds$fit + 1.96 * monga_loss_preds$se.fit)

monga_loss_LL <- exp(monga_loss_preds$fit - 1.96 * monga_loss_preds$se.fit)

lit_loss_preds <- predict(lit_total_loss_glm_nb, type="link", se=TRUE)

lit_loss_fit <- exp(lit_loss_preds$fit)

lit_loss_UL <- exp(lit_loss_preds$fit + 1.96 * lit_loss_preds$se.fit)

lit_loss_LL <- exp(lit_loss_preds$fit - 1.96 * lit_loss_preds$se.fit)

# Create variables with labels for points outside 
# the confidence region of each regression

join$monga_outside <- ifelse((join$monga_count > monga_loss_UL | join$monga_count < monga_loss_LL), as.character(join$country),"")

join$lit_outside <- ifelse((join$lit_count > lit_loss_UL | join$lit_count < lit_loss_LL), as.character(join$country),"")

# Filter labels for countries with high deforestation

join$monga_outside_high_defor <- ifelse((nchar(join$monga_outside) > 0 & join$total_loss > exp(7.5) & join$total_loss < exp(11.125)), join$monga_outside,"")

join$lit_outside_high_defor <- ifelse((nchar(join$lit_outside) > 0 & join$total_loss > exp(7.5) & join$total_loss < exp(11.125)), join$lit_outside,"")

join$monga_outside_high_priority <- ifelse(join$monga_count < monga_loss_LL & join$total_loss > exp(7.5),"high","low")

join$lit_outside_high_priority <- ifelse(join$lit_count < lit_loss_LL & join$total_loss > exp(7.5), "high","low")

# Label these countries with
# their transformed names for the topic modeling.
# These names can be exported to 
# topicmodels_mallet_defor_country_contexts.R to
# examine country contexts.

join_ordered_defor <- join[order(join$total_loss,decreasing = TRUE),]

monga_outliers <- join_ordered_defor[join_ordered_defor$monga_outside_high_priority=="high",]$monga_outside_high_defor

lit_outliers <- join_ordered_defor[join_ordered_defor$lit_outside_high_priority=="high",]$lit_outside_high_defor

save(monga_outliers,file="Data/monga_outliers.Rdata")

save(lit_outliers,file="Data/lit_outliers.Rdata")



# Plot semi-log regressions

monga_total_loss_semi_log <- ggplot(join, aes(log(total_loss), monga_count)) +
  geom_ribbon(aes(ymin = monga_loss_LL, ymax = monga_loss_UL),alpha = 0,linetype=2,color="black") +
  geom_line(aes(y=monga_loss_fit),size=1) +
  geom_point(aes(shape = region)) +
  theme_bw()

monga_total_loss_semi_log_annotated <- monga_total_loss_semi_log +
  ggplot2::annotate("text", x = -Inf, y = Inf, vjust="top", hjust="left", label = paste("~~pseudo~R^{2} ==", monga_total_loss_r_sq), size=4, parse=TRUE) +
  labs(x = "log(Deforestation (ha))", y = "", shape = "Region")

lit_total_loss_semi_log <- ggplot(join, aes(log(total_loss), lit_count)) +
  geom_ribbon(aes(ymin = lit_loss_LL, ymax = lit_loss_UL),alpha = 0,linetype=2,color="black") +
  geom_line(aes(y=lit_loss_fit),size=1) +
  geom_point(aes(shape = factor(region))) +
  theme_bw()

lit_total_loss_semi_log_annotated <- lit_total_loss_semi_log +
  ggplot2::annotate("text", x = -Inf, y = Inf, vjust="top", hjust="left", label = paste("~~pseudo~R^{2} ==", lit_total_loss_r_sq), size=4, parse=TRUE) +
  labs(x = "",y = "", shape = "Region")

# Plot untransformed regressions zoom

monga_total_loss_semi_log_zoom <- monga_total_loss_semi_log +
  coord_cartesian(xlim=c(7.5,11.125),ylim=c(0,1500)) +
  geom_text_repel(aes(label=monga_outside_high_defor)) +
  labs(x = "log(Deforestation (ha))", y = "Mongabay mentions", shape = "Region")

lit_total_loss_semi_log_zoom <- lit_total_loss_semi_log +
  coord_cartesian(xlim=c(7.5,11.125),ylim=c(0,9000)) +
  geom_text_repel(aes(label=lit_outside_high_defor)) +
  labs(x = "", y = "Literature mentions", shape = "Region")

# Check plot

dev.new(width=6.5,height=6.5,noRStudioGD = TRUE)

ggarrange(lit_total_loss_semi_log_zoom,
          lit_total_loss_semi_log_annotated,
          monga_total_loss_semi_log_zoom,
          monga_total_loss_semi_log_annotated,
          ncol=2, nrow=2, common.legend = TRUE, legend="bottom")

# Print plot

date <- gsub("-","",Sys.Date())

folder_path <- "Manuscript_figures/"

file_path <- paste0(folder_path,"Mentions_defor_",date,".png")

png(file_path,width=6.5,height=6.5,units="in",res=300)

print(ggarrange(lit_total_loss_semi_log_zoom,
                lit_total_loss_semi_log_annotated,
                monga_total_loss_semi_log_zoom,
                monga_total_loss_semi_log_annotated,
                ncol=2, nrow=2, common.legend = TRUE, legend="bottom"))

dev.off()



# Plot log-log regressions

monga_total_loss_log_log <- ggplot(join, aes(log(total_loss), log(monga_count))) +
  geom_ribbon(aes(ymin = log(monga_loss_LL), ymax = log(monga_loss_UL)),alpha = 0,linetype=2,color="black") +
  geom_line(aes(y=log(monga_loss_fit)),size=1) +
  geom_point(aes(shape = region)) +
  labs(x = "log(Forest loss)", y = "log(Mongabay mentions)", shape = "Region", color = "Priority") +
  geom_text_repel(aes(label=monga_outside, col = monga_outside_high_priority),box.padding = 0.22,seed=1) +
  ggplot2::annotate("text", x = -Inf, y = Inf, vjust="top", hjust="left", label = paste("~~pseudo~R^{2} ==", monga_total_loss_r_sq), size=4, parse=TRUE) +
  theme_bw() +
  scale_color_manual(values=c("red","gray40")) +
  theme(legend.position="bottom")

lit_total_loss_log_log <- ggplot(join, aes(log(total_loss), log(lit_count))) +
  geom_ribbon(aes(ymin = log(lit_loss_LL), ymax = log(lit_loss_UL)),alpha = 0,linetype=2,color="black") +
  geom_line(aes(y=log(lit_loss_fit)),size=1) +
  geom_point(aes(shape = factor(region))) +
  labs(x = "log(Forest loss)", y = "log(Defor. lit. mentions)", shape = "Region", color = "Priority") +
  geom_text_repel(aes(label=lit_outside, col = lit_outside_high_priority),force=3) +
  ggplot2::annotate("text", x = -Inf, y = Inf, vjust="top", hjust="left", label = paste("~~pseudo~R^{2} ==", lit_total_loss_r_sq), size=4, parse=TRUE) +
  theme_bw() +
  scale_color_manual(values=c("red","gray40")) +
  theme(legend.position="bottom")

# Print plots

date <- gsub("-","",Sys.Date())

folder_path <- "Manuscript_figures/"

plot_list <- list(monga_total_loss_log_log,lit_total_loss_log_log)

plot_names <- c("Monga_defor_log_log","Lit_defor_log_log")

for (i in 1:length(plot_names)) {
  
  file_path <- paste0(folder_path,plot_names[i],"_",date,".png")
  
  png(file_path,width=6.5,height=6.5,units="in",res=300)
  
  print(plot_list[[i]])
  
  dev.off()
  
}



# https://github.com/tidyverse/ggplot2/wiki/plotting-polygon-shapefiles

# http://www.cookbook-r.com/Graphs/

countries@data$id = rownames(countries@data)
countries.points = fortify(countries, region="id")
countries.df = plyr::join(countries.points, countries@data, by="id")

countriesSub@data$id = rownames(countriesSub@data)
countriesSub.points = fortify(countriesSub, region="id")
countriesSub.df = plyr::join(countriesSub.points, countriesSub@data, by="id")

breaks <- quantile(countriesSub.df$total_loss, probs=0:5/5, na.rm = TRUE)

# round to one significant digit

breaks <- signif(breaks,1)

countriesSub.df$quant <- factor(findInterval(countriesSub.df$total_loss, vec=breaks, all.inside = TRUE))

# divide by 100 to convert ha to km2

breaks <- breaks/100

breaks <- prettyNum(breaks,big.mark=",",scientific=FALSE)

loss_labs <- c(paste(breaks[1],"-",breaks[2]),
               paste(breaks[2],"-",breaks[3]),
               paste(breaks[3],"-",breaks[4]),
               paste(breaks[4],"-",breaks[5]),
               paste(breaks[5],"-",breaks[6]))

mongabay@data$id = rownames(mongabay@data)
mongabay.points = fortify(mongabay, region="id")
mongabay.df = plyr::join(mongabay.points, mongabay@data, by="id")

breaks <- quantile(mongabay.df$count, probs=0:5/5)

# round to one significant digit

breaks <- signif(breaks,1)

mongabay.df$quant <- factor(findInterval(mongabay.df$count, vec=breaks, all.inside = TRUE))

breaks <- prettyNum(breaks,big.mark=",",scientific=FALSE)

monga_labs <- c(paste(breaks[1],"-",breaks[2]),
                paste(breaks[2],"-",breaks[3]),
                paste(breaks[3],"-",breaks[4]),
                paste(breaks[4],"-",breaks[5]),
                paste(breaks[5],"-",breaks[6]))

literature@data$id = rownames(literature@data)
literature.points = fortify(literature, region="id")
literature.df = plyr::join(literature.points, literature@data, by="id")

breaks <- quantile(literature.df$count, probs=0:5/5)

# round to one significant digit

breaks <- signif(breaks,1)

literature.df$quant <- factor(findInterval(literature.df$count, vec=breaks, all.inside = TRUE))

breaks <- prettyNum(breaks,big.mark=",",scientific=FALSE)

literature_labs <- c(paste(breaks[1],"-",breaks[2]),
               paste(breaks[2],"-",breaks[3]),
               paste(breaks[3],"-",breaks[4]),
               paste(breaks[4],"-",breaks[5]),
               paste(breaks[5],"-",breaks[6]))

slope_map <- ggplot() + 
  geom_polygon(data=countries.df, aes(long,lat,group=group), fill="white", col="black") +
  geom_polygon(data=countriesSub.df, aes(long,lat,group=group,fill=quant), col="black") +
  coord_fixed(xlim=c(min(countriesSub.df[countriesSub.df$admin=="mexico",]$long),max(countriesSub.df$long)),
              ylim=c(min(countriesSub.df$lat),max(countriesSub.df$lat))) +
  scale_fill_brewer(palette = "Reds", name="Deforestation (sq km)", labels=loss_labs) +
  labs(x="",y="") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA))

monga_map <- ggplot() + 
  geom_polygon(data=countries.df, aes(long,lat,group=group), fill="white", col="black") +
  geom_polygon(data=mongabay.df, aes(long,lat,group=group,fill=quant), col="black") +
  coord_fixed(xlim=c(min(countriesSub.df[countriesSub.df$admin=="mexico",]$long),max(countriesSub.df$long)),
              ylim=c(min(countriesSub.df$lat),max(countriesSub.df$lat))) +
  scale_fill_brewer(palette = "Greens", name="Mongabay\nmentions\n(n=3135\narticles)", labels=monga_labs) +
  labs(x="",y="lat") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA))

literature_map <- ggplot() + 
  geom_polygon(data=countries.df, aes(long,lat,group=group), fill="white", col="black") +
  geom_polygon(data=literature.df, aes(long,lat,group=group,fill=quant), col="black") +
  coord_fixed(xlim=c(min(countriesSub.df[countriesSub.df$admin=="mexico",]$long),max(countriesSub.df$long)),
              ylim=c(min(countriesSub.df$lat),max(countriesSub.df$lat))) +
  scale_fill_brewer(palette = "Blues", name="Literature\nmentions\n(n=5278\narticles)", labels=literature_labs) +
  labs(x="lon",y="") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA))

# Check plot

dev.new(width=10,height=8,noRStudioGD = TRUE)

grid.arrange(slope_map,literature_map,monga_map)

grid.text("(a)",x=unit(0.025,"npc"),y=unit(0.32+0.33*2,"npc"))

grid.text("(b)",x=unit(0.025,"npc"),y=unit(0.32+0.33,"npc"))

grid.text("(c)",x=unit(0.025,"npc"),y=unit(0.32,"npc")) 

# Print plot

date <- gsub("-","",Sys.Date())

folder_path <- "Manuscript_figures/"

file_path <- paste0(folder_path,"Mentions_defor_maps_",date,".png")

png(file_path,width=10,height=8,units="in",res=300)

print(grid.arrange(slope_map,literature_map,monga_map))
      
print(grid.text("(a)",x=unit(0.025,"npc"),y=unit(0.32+0.33*2,"npc")))

print(grid.text("(b)",x=unit(0.025,"npc"),y=unit(0.32+0.33,"npc")))

print(grid.text("(c)",x=unit(0.025,"npc"),y=unit(0.32,"npc")))

dev.off()