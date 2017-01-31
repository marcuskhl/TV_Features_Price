
# library -----------------------------------------------------------------


library(openxlsx)
library(reshape2)

source("scrape_melt.R")
files <- list.files("./Jan2017", full.names = T)
files <- files[!grepl("~\\$", files)]
files <- files[grepl("xlsx", files)]
#aspect_ratio_list   <- read.csv("aspect_ratio_list.csv", stringsAsFactors = F)                                   
#backlight_list      <- read.csv("backlight_list.csv", stringsAsFactors = F   )                                     
#brand_list          <- read.csv("brand_list.csv", stringsAsFactors = F )                                           
##brand_model         <- read.csv("brand_model.csv", stringsAsFactors = F)
## code will fall over if regular expression style text is in model number eg ? () *
#display_format_list <- read.csv("display_format_list.csv", stringsAsFactors = F)                                    
#refresh_rate_list   <- read.csv("refresh_rate_list.csv", stringsAsFactors = F )                                     

looped_files <- c()
loop_xl <- function(input_files) {
  for (i in 1:length(input_files)) {
    loop_file <- readWorkbook(input_files[i], colNames = F)
    if (i == 1) {looped_files <- loop_file } else {looped_files <- rbind(loop_file, looped_files)}
  }
  looped_files
}


# dictionary --------------------------------------------------------------

refresh_rate_synonyms <- readWorkbook("TV_set_cleaning_controls.xlsx","refresh_rate_synonyms")
refresh_rate_list  <- readWorkbook("TV_set_cleaning_controls.xlsx", "refresh_rate_list" )
display_format_synonyms <- readWorkbook("TV_set_cleaning_controls.xlsx","display_format_synonyms")
display_format_list <- readWorkbook("TV_set_cleaning_controls.xlsx","display_format_list" )
brands_synonyms <- readWorkbook("TV_set_cleaning_controls.xlsx","brands_synonyms" )
brands <- readWorkbook("TV_set_cleaning_controls.xlsx","brands")
backlight_list <- readWorkbook("TV_set_cleaning_controls.xlsx","backlight_list")
backlight_list_synonyms <- readWorkbook("TV_set_cleaning_controls.xlsx","backlight_list_synonyms")
aspect_ratios <- readWorkbook("TV_set_cleaning_controls.xlsx","aspect_ratios")
aspect_ratios_synonyms  <- readWorkbook("TV_set_cleaning_controls.xlsx","aspect_ratios_synonyms")
displays <- readWorkbook("TV_set_cleaning_controls.xlsx","display_list")
displays_synonyms  <- readWorkbook("TV_set_cleaning_controls.xlsx","display_list_synonyms")
platforms <- readWorkbook("TV_set_cleaning_controls.xlsx","platform_list")
platforms_synonyms  <- readWorkbook("TV_set_cleaning_controls.xlsx","platform_list_synonyms")
dvds <- readWorkbook("TV_set_cleaning_controls.xlsx","dvd_list")
dvd_synonyms  <- readWorkbook("TV_set_cleaning_controls.xlsx","dvd_list_synonyms")

hdmi_specs <- c("1.0", "1.1", "1.2", "1.3", "1.4", "2.0", "2.1")


# all_uk_and_us -----------------------------------------------------------

uk_files <- grep("uk", files, ignore.case = T, value = T)
us_files <- grep("USTVPST", files, ignore.case = T, value = T)

all_uk <- loop_xl(uk_files)
all_us <- loop_xl(us_files)

all_uk$Country <- "UK"
all_us$Country <- "USA"

english_files <- rbind(all_uk, all_us)
first_pass <- english_files
colnames(first_pass) <- c("Descriptor","Description", "Country")

# translations ------------------------------------------------------------

source("translation_sub_script.R")
first_pass <- rbind(first_pass, translated)

# first pass ----------------------------------------------------------------

first_pass$Item <- NA
first_pass_length <- length(first_pass[,1])

k <- 0
for (i in 1:first_pass_length) {
  
  if (first_pass[i,1] == "Model Name") {k <- k + 1}
first_pass[i,4] <- k # $Item currently in position 4
print(paste0(round(i/first_pass_length*100,2), "%")) 
next
}

### work on one item at a time

second_pass <- data.frame(Item=integer(), 
                               Country=character(),
                               Brand=character(),
                               Model=character(), 
                               Display=character(), 
                               Size=character(),
                               Ratio=character(),
                               Brightness=character(),
                               Format=character(),
                               HDMI=character(),
                               CI=character(),
                               Power=character(),
                               Backlight=character(),
                               Refresh=character(),
                               USB=character(),
                               #Internet=character(),
                               Ethernet=character(),
                               WIFI=character(),
                               THREED=character(),
                               Glasses=character(),
                               DVD=character(),
                               Curved=character(),
                               Platform=character()
                               #OS=character()
  ) 
  
first_pass$Description <- gsub("[Ss][Iiì]", "Yes", first_pass$Description)
first_pass$Description <- gsub("sim", "Yes", first_pass$Description)
first_pass$Description <- gsub("[Jj][Aa]", "Yes", first_pass$Description)
first_pass$Description <- gsub("[Oo][Uu][Ii]", "Yes", first_pass$Description)

first_pass$Description <- gsub("[Nn][oO][nN]", "No", first_pass$Description)
first_pass$Description <- gsub("[Nn][Ee][Ii][nN]", "No", first_pass$Description)
first_pass$Description <- gsub("[Nn][AaãÃ][Oo]", "No", first_pass$Description)


# loop start --------------------------------------------------------------

for (j in 1:10)#k)
      {

current_item <- first_pass[first_pass$Item == j,]


# Brands ------------------------------------------------------------------

clean_brand <- NA

for (i in 1:length(brands$Brands)) {
  brand <- brands$Brands[i]
 if (length(grep(brand, current_item$Description)) == 0) 
  next 
  else clean_brand <- brand
  break
}

if(is.na(clean_brand)) {
  for (i in 1:length((brands_synonyms$Brands_synonyms))) {
    brand <- brands_synonyms$Brands_synonyms[i]
    if (length(grep(brand, current_item$Description)) == 0)
      next
      else clean_brand <- brands_synonyms$Brands[i]
      break
  }
}

# Display Tech ------------------------------------------------------------

clean_display <- NA

for (i in 1:length(displays$Displays)) {
  display <- displays$Displays[i]
  if (length(grep(display, current_item$Description)) == 0)
    next
  else clean_display <- display
  break
}

if(is.na(clean_display)) {
  for (i in 1:length((displays_synonyms$Displays.Synonym))) {
    display <- displays_synonyms$Displays.Synonym[i]
    if (length(grep(display, current_item$Description)) == 0)
      next
    else clean_display <- displays_synonyms$Displays[i]
    break
  }
}

# Aspect Ratio ------------------------------------------------------------

clean_ratio <- NA

for (i in 1:length(aspect_ratios$Aspect.Ratios)) {
  ratio <- aspect_ratios$Aspect.Ratios[i]
  if (length(grep(ratio, current_item$Description)) == 0)
    next
  else clean_ratio <- ratio
  break
}

if(is.na(clean_ratio)) {
  for (i in 1:length((aspect_ratios_synonyms$Aspect.Ratios.Synonym))) {
    ratio <- aspect_ratios_synonyms$Aspect.Ratios.Synonym[i]
    if (length(grep(ratio, current_item$Description)) == 0)
      next
    else clean_ratio <- aspect_ratios_synonyms$Aspect.Ratios[i]
    break
  }
}

# Backlight ---------------------------------------------------------------

clean_backlight <- NA

for (i in 1:length(backlight_list$Backlights)) {
  backlight <- backlight_list$Backlights[i]
  if (length(grep(backlight, current_item$Description)) == 0)
    next
  else clean_backlight <- backlight
  break
}

if(is.na(clean_backlight)) {
  for (i in 1:length((backlight_list_synonyms$Backlights.Synonym))) {
    backlight <- backlight_list_synonyms$Backlights.Synonym[i]
    if (length(grep(backlight, current_item$Description)) == 0)
      next
    else clean_backlight <- backlight_list_synonyms$Backlights[i]
    break
  }
}


# Refresh Rate ------------------------------------------------------------

clean_refresh <- NA

for (i in 1:length(refresh_rate_list$Refresh.Rates)) {
  refresh <- refresh_rate_list$Refresh.Rates[i]
  if (length(grep(refresh, current_item$Description)) == 0)
    next
  else clean_refresh <- refresh
  break
}

if(is.na(clean_refresh)) {
  for (i in 1:length((refresh_rate_synonyms$Refresh.Rates.Synonyms))) {
    refresh <- refresh_rate_synonyms$Refresh.Rates.Synonyms[i]
    if (length(grep(refresh, current_item$Description)) == 0)
      next
    else clean_refresh <- refresh_rate_synonyms$Refresh.Rates[i]
    break
  }
}


# Platform ----------------------------------------------------------------

clean_platform <- NA

for (i in 1:length(platforms$Platforms)) {
  platform <- platforms$Platforms[i]
  if (length(grep(platform, current_item$Description)) == 0)
    next
  else clean_platform <- platform
  break
}

if(is.na(clean_platform)) {
  for (i in 1:length((platforms_synonyms$Platforms.Synonym))) {
    platform <- platforms_synonyms$Platforms.Synonym[i]
    if (length(grep(platform, current_item$Description)) == 0)
      next
    else clean_platform <- platforms_synonyms$Platforms[i]
    break
  }
}

# DVD ----------------------------------------------------------------

clean_dvd <- NA

for (i in 1:length(dvds$Media.Type)) {
  dvd <- dvds$Media.Type[i]
  if (length(grep(dvd, current_item$Description)) == 0)
    next
  else clean_dvd <- dvd
  break
}

if(is.na(clean_dvd)) {
  for (i in 1:length((dvd_synonyms$Media.Type.Syononym))) {
    dvd <- dvd_synonyms$Media.Type.Syononym[i]
    if (length(grep(dvd, current_item$Description)) == 0)
      next
    else clean_dvd <- dvd_synonyms$Media.Type[i]
    break
  }
}


# CI+ ---------------------------------------------------------------------

clean_ci <- NA

search_terms <- c("[Cc][Ii]? [Ss]lot", "[Cc][Ii]? [Pp]ort", "[Cc][Ii]? [Cc]ard", "[Cc][Ii]? [Aa]ccess")

for (i in 1:length(search_terms)) {
  search_term <- search_terms[i]
  if(length(grep(search_term, current_item$Descriptor)) == 0)
    next
    else
      clean_ci <- current_item[grep(search_term, current_item$Descriptor),2]

}


# add PCMAI to search list

clean_ci[clean_ci == "Y" | clean_ci == "YES" | is.numeric(clean_ci) & clean_ci > 0 | 
           clean_ci == "T" | clean_ci == "True" | clean_ci == "TRUE" | 
           clean_ci == "1" | clean_ci == "Si" | clean_ci == "CI+" ] <- "Yes"
clean_ci[clean_ci == "N" | clean_ci == "NO" | clean_ci == 0 | clean_ci == "F" | clean_ci == "False" | clean_ci == "FALSE"] <- "Yes"




# Ethernet ----------------------------------------------------------------

clean_ethernet <- NA

search_terms <- c("Ethernet", "RJ-45")

for (i in 1:length(search_terms)) {
  search_term <- search_terms[i]
  if(length(grep(search_term, current_item$Descriptor)) == 0)
    next
  else
    clean_ethernet <- current_item[grep(search_term, current_item$Descriptor),2]

}

if (is.na(clean_ethernet)) {
  LAN <- grep("LAN", current_item$Descriptor, value = T)
  WLAN <- grep("Wireless LAN", current_item$Descriptor, value = T)
  LAN_only <- LAN[!LAN %in% WLAN]
  if(length(LAN_only) > 0) {
    clean_ethernet <- current_item[current_item$Descriptor == LAN_only,2]
  }
}

clean_ethernet[clean_ethernet == "Y" | clean_ethernet == "YES" | 
                 is.numeric(clean_ethernet) & clean_ethernet > 0 | clean_ethernet == "T" | 
                 clean_ethernet == "True" | clean_ethernet == "TRUE" | clean_ethernet == "Ethernet" | clean_ethernet == "Si"
               ] <- "Yes"
clean_ethernet[clean_ethernet == "N" | clean_ethernet == "NO" | clean_ethernet == 0 | clean_ethernet == "F" | clean_ethernet == "False" | clean_ethernet == "FALSE"] <- "Yes"


# WiFi --------------------------------------------------------------------


clean_wifi <- NA

search_terms <- c("[Ww][Ii](-)?[Ff][Ii]", "Wireless LAN", "[Nn]etwork")

for (i in 1:length(search_terms)) {
  search_term <- search_terms[i]
  if(length(grep(search_term, current_item$Descriptor)) == 0)
    next
  else
    clean_wifi <- current_item[grep(search_term, current_item$Descriptor),2]

}


clean_wifi[clean_wifi == "Y" | clean_wifi == "YES" | is.numeric(clean_wifi) & clean_wifi > 0 | clean_wifi == "T" | clean_wifi == "True" | clean_wifi == "TRUE"] <- "Yes"
clean_wifi[clean_wifi == "N" | clean_wifi == "NO" | clean_wifi == 0 | clean_wifi == "F" | clean_wifi == "False" | clean_wifi == "FALSE"] <- "Yes"

# 3D Capable --------------------------------------------------------------

clean_3d <- NA
clean_glasses <- NA

  threeD <- grep("3[Dd]", current_item$Descriptor, value = T)
  glasses <- grep("[Gg]lass(es)?", current_item$Descriptor, value = T)
  threeD_only <- threeD[!threeD %in% glasses]

  if (length(threeD_only > 0)) {
  for (i in 1:length(threeD_only)) {
    search_term <- threeD_only[i]
    if(length(grep(search_term, current_item$Descriptor)) == 0)
      next
    else
      clean_3d <- current_item[grep(search_term, current_item$Descriptor),2]

  }
}

  if (length(glasses > 0 )) {
  for (i in 1:length(glasses)) {
    search_term <- glasses[i]
    if(length(grep(search_term, current_item$Descriptor)) == 0)
      next
    else
      clean_glasses <- current_item[grep(search_term, current_item$Descriptor),2]

  }
}

  clean_3d[clean_3d == "Y" | clean_3d == "YES" | clean_3d == "T" | clean_3d == "True" | clean_3d == "TRUE"] <- "Yes"
  clean_3d[clean_3d == "N" | clean_3d == "NO" | clean_3d == 0 | clean_3d == "F" | clean_3d == "False" | clean_3d == "FALSE"] <- "Yes"

  clean_3d <- clean_3d[1]

  clean_glasses[clean_glasses == "Y" | clean_glasses == "YES" | is.numeric(clean_glasses) & clean_glasses > 0 | clean_glasses == "T" | clean_glasses == "True" | clean_glasses == "TRUE"] <- "Yes"
  clean_glasses[clean_glasses == "N" | clean_glasses == "NO" | clean_glasses == 0 | clean_glasses == "F" | clean_glasses == "False" | clean_glasses == "FALSE"] <- "Yes"

  clean_glasses <- clean_glasses[1]
  
# Curved ------------------------------------------------------------------

clean_curved <- NA

search_terms <- c("[Cc]urve")

for (i in 1:length(search_terms)) {
  search_term <- search_terms[i]
  if(length(grep(search_term, current_item$Descriptor)) == 0)
    next
  else
    clean_curved <- current_item[grep(search_term, current_item$Descriptor),2]

}


clean_curved[clean_curved == "Y" | clean_curved == "YES" | is.numeric(clean_curved) & clean_curved > 0 | clean_curved == "T" | clean_curved == "True" | clean_curved == "TRUE"] <- "Yes"
clean_curved[clean_curved == "N" | clean_curved == "NO" | clean_curved == 0 | clean_curved == "F" | clean_curved == "False" | clean_curved == "FALSE"] <- "Yes"



# Screen Size -------------------------------------------------------------


clean_size <- NA

search_terms <- c("[Ss]creen [Ss]ize", "[Dd]iag(onal)? [Ss]ize", "[Dd]isplay [Ss]ize", "Panel Selected", "[Cc]lass [Ss]ize")

for (i in 1:length(search_terms)) {
  search_term <- search_terms[i]
  if(length(grep(search_term, current_item$Descriptor)) == 0)
    next
  else
    clean_size <- current_item[grep(search_term, current_item$Descriptor),2]

}


clean_size <- gsub("[A-Za-z]","", clean_size)
clean_size <- gsub(" ","", clean_size)
clean_size <- gsub("\"","", clean_size)



# HDMI --------------------------------------------------------------------

clean_hdmi <- NA

search_terms <- c("[Hh][Dd][Mm][Ii] [Pp]ort(s)?", "[Hh][Dd][Mm][Ii] [Cc]onnection(s)?", "HDMI")

for (i in 1:length(search_terms)) {
  search_term <- search_terms[i]
  if(length(grep(search_term, current_item$Descriptor)) == 0)
    next
  else
    if (length(grep("Standard", current_item$Descriptor)) > 0) 
    clean_hdmi <- current_item[grep(search_term, current_item$Descriptor),2]
}

clean_hdmi <- clean_hdmi[!(clean_hdmi %in% hdmi_specs)]

if (is.na(clean_hdmi)) {

for (i in 1:length(search_terms)) {
  search_term <- search_terms[i]
  if(length(grep(search_term, current_item$Description)) == 0)
    next
  else
    clean_hdmi <- current_item[grep(search_term, current_item$Descriptor),2]
}}

clean_hdmi <- gsub("[A-Za-z]","", clean_hdmi)
clean_hdmi <- gsub(" ","", clean_hdmi)
clean_hdmi <- gsub("\"","", clean_hdmi)


# Brightness --------------------------------------------------------------



clean_brightness <- NA

search_terms <- c("[Bb]rightness", "cd/m2")

for (i in 1:length(search_terms)) {
  search_term <- search_terms[i]
  if(length(grep(search_term, current_item$Descriptor)) == 0)
    next
  else
    clean_brightness <- current_item[grep(search_term, current_item$Descriptor),2]

}

clean_brightness <- max(as.numeric(clean_brightness))

clean_brightness <- gsub("[A-Za-z]","", clean_brightness)
clean_brightness <- gsub(" ","", clean_brightness)
clean_brightness <- gsub("\"","", clean_brightness)

# Power Consumption -------------------------------------------------------

clean_power <- NA

search_terms <- c("[Pp]ower [Cc]onsumption")

for (i in 1:length(search_terms)) {
  search_term <- search_terms[i]
  if(length(grep(search_term, current_item$Descriptor)) == 0)
    next
  else
    clean_power <- current_item[grep(search_term, current_item$Descriptor),2]

}



clean_power <- gsub("[A-Za-z]","", clean_power)
clean_power <- gsub(" ","", clean_power)
clean_power <- gsub("\"","", clean_power)
clean_power <- max(as.numeric(clean_power))

# USB ---------------------------------------------------------------------

clean_usb <- NA

search_terms <- c("USB")

for (i in 1:length(search_terms)) {
  search_term <- search_terms[i]
  if(length(grep(search_term, current_item$Descriptor)) == 0)
    next
  else
    clean_usb <- current_item[grep(search_term, current_item$Descriptor),2]
}

if (is.na(clean_usb)) {
  for (i in 1:length(search_terms)) {
    search_term <- search_terms[i]
    if(length(grep(search_term, current_item$Description)) == 0)
      next
    else
      clean_usb <- current_item[grep(search_term, current_item$Descriptor),2]
  }
  
  
}

clean_usb <- gsub("[A-Za-z]","", clean_usb)
clean_usb <- gsub(" ","", clean_usb)
clean_usb <- gsub("\"","", clean_usb)



# Models ------------------------------------------------------------------

clean_model <- current_item[current_item$Descriptor == "Model Name",2]

# for (i in 1:length(keywords)) {
#   search_term <- keywords[1,i]
#   clean_model <- gsub(search_term, "", clean_model)
#   
# }      




# Resolution --------------------------------------------------------------

clean_format <- NA

clean_terms <- c("SD", "FHD", "HD", "UHD", "4[Kk]", "720p", "1080p", "2160p")

for (i in 1:length(clean_terms)) {
  clean_term <- clean_terms[i]
  if(length(grep(search_term, current_item$Descriptor)) == 0)
    next
  else
    clean_format <- clean_term

}
if (is.na(clean_format)) {
for (i in 1:length(clean_terms)) {
  clean_term <- clean_terms[i]
  if(length(grep(search_term, current_item$Description)) == 0)
    next
  else
    clean_format <- clean_term

}}

search_terms <- c("[Rr]esolution", "[Dd]isplay [Ff]ormat")

if (is.na(clean_format)) {
for (i in 1:length(search_terms)) {
  search_term <- search_terms[i]
  if(length(grep(search_term, current_item$Descriptor)) == 0)
    next
  else
    clean_format <- current_item[grep(search_term, current_item$Descriptor),2]

}}

#need to add translation from resol

# Internet ----------------------------------------------------------------

clean_internet <- NA

if (is.na(clean_ethernet))

# First Pass Output -------------------------------------------------------

#needs Year, Quarter, Month, Region, and Country


new_row <- data.frame(Item=as.numeric(j),
                             Country=current_item$Country[1],
                             Brand=clean_brand[1],
                             Model=clean_model[1], 
                             Display=clean_display[1], 
                             Size=clean_size[1],
                             Ratio=clean_ratio[1],
                             Brightness=clean_brightness[1],
                             Format=clean_format[1],
                             HDMI=clean_hdmi[1],
                             CI=clean_ci[1],
                             Power=clean_power[1],
                             Backlight=clean_backlight[1],
                             Refresh=clean_refresh[1],
                             USB=clean_usb[1],
                             #Internet=character[1],
                             Ethernet=clean_ethernet[1],
                             WIFI=clean_wifi[1],
                             THREED=clean_3d[1],
                             Glasses=clean_glasses[1],
                             DVD=clean_dvd[1],
                             Curved=clean_curved[1],
                             Platform=clean_platform[1]
                             #OS=clean_os[1]
) 
             
second_pass <- rbind(second_pass, new_row)
print(paste0(round(j/k*100,2), "%"))
}                                     
                             

# second_pass -------------------------------------------------------------


second_pass$WIFI[grep("[Bb]uilt", second_pass$WIFI)] <- "Yes"
second_pass$WIFI[grep("[Ss]eperate", second_pass$WIFI)] <- "No"
second_pass$WIFI[grep("[Rr]eady", second_pass$WIFI)] <- "Yes"
second_pass$WIFI[grep("Yes", second_pass$WIFI)] <- "Yes"
second_pass$WIFI[grep("802", second_pass$WIFI)] <- "Yes"
second_pass$WIFI[grep("Inte", second_pass$WIFI)] <- "Yes"

second_pass$Ethernet[grep("1", second_pass$Ethernet)] <- "Yes"

second_pass$THREED[grep("[Pp]assive", second_pass$THREED)] <- "Yes"
second_pass$THREED[grep("[Aa]ctive", second_pass$THREED)] <- "Yes"


# writing_output ----------------------------------------------------------

wb <- createWorkbook()
addWorksheet(wb, "initial_data")
addWorksheet(wb, "clean_data")
writeData(wb, "initial_data", first_pass)
writeData(wb, "clean_data", second_pass)
saveWorkbook(wb, "Cleaned_output.xlsx", overwrite = T)


