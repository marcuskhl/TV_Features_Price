if(is.na(match(c("devtools"),installed.packages()[,"Package"]))) install.packages(new.packages) else library(devtools)
suppressMessages(devtools::install_github("marcuskhl/BasicSettings"));library(BasicSettings)

price_db_raw <- as.df(fread("M:/Technology/DATA/TV_sets_model/WW TV Price Tracker/Tracker Price Model Output/Pricing_Output.csv", header = T))
price_db <- price_db_raw # for debugging so I dont need to read the file over and over

#~~~Data Cleaning Start~~~#
names(price_db) <- gsub(" ($)","_US", names(price_db), fixed = T)
names(price_db) <- gsub(" (Local)","_Loc", names(price_db), fixed = T)
names(price_db) <- gsub(" (LAN / RJ-45)","", names(price_db), fixed = T)

x <- sapply(price_db, class)
for(i in 1:length(x)){
  if(x[i]=="integer" | x[i]=="numeric" ){
    price_db[,i] <- f2n(price_db[,i]) #somehow using apply is even slower on this line
  }else{
    price_db[,i] <- f2c(price_db[,i])
  }
}



price_db$Country <- toupper(price_db$Country)
price_db$Brand <- toupper(price_db$Brand)

# blank_reducer <- function(col){ # this caused every character to be NA
#   col <- gsub("  "," ", col)
#   col <- gsub("  "," ", col)
#   col <- gsub(" ","", col)
# }
# 
# 
# 
# for (i in 1:dim(price_db)[2]){
#   price_db[,i] <- blank_reducer(price_db[,i])
# }



price_db <- column.rm(price_db, c("Size Group (5-inch)","Size Group (10-inch)")) # this is not present in every row for some reason, recalculated later
price_db <- column.rm(price_db, c("Power Consumption (Watts)", "3D Capable", "3D Glasses", "Platform")) # aint nobody got time for this 
price_db <- column.rm(price_db, "Price_Loc")



# Brightness ---------------------------------------------------------
price_db$Brightness[price_db$Brightness>=1000] <- NA
price_db$Brightness[price_db$Brightness=="" | price_db$Brightness==" "] <- NA

# Aspect Ratio ---------------------------------------------------------
price_db$`Aspect Ratio`[price_db$`Aspect Ratio`=="" |  price_db$`Aspect Ratio`==" "] <- NA
price_db$`Aspect Ratio` <- gsub("0.67291666666666661","16:9",price_db$`Aspect Ratio`, fixed = T )

# Refresh Rate ---------------------------------------------------------
price_db$`Refresh Rate` <- gsub("H", "", price_db$`Refresh Rate`, ignore.case = T)
price_db$`Refresh Rate` <- gsub("z", "", price_db$`Refresh Rate`, ignore.case = T)
price_db$`Refresh Rate` <- gsub(" ", "", price_db$`Refresh Rate`, fixed = T)
price_db$`Refresh Rate`[price_db$`Refresh Rate`==""|price_db$`Refresh Rate`==" "] <- NA 
price_db$`Refresh Rate` <- f2n(price_db$`Refresh Rate`)
price_db$`Refresh Rate`[price_db$`Refresh Rate`>250] <- NA # rm some ridiculous refresh rates
price_db$`Refresh Rate`[!is.na(price_db$`Refresh Rate`)] <- paste0(price_db$`Refresh Rate`[!is.na(price_db$`Refresh Rate`)], "Hz")

# Display Format ---------------------------------------------------------
price_db$`Display Format`[price_db$`Display Format`=="" |  price_db$`Display Format`==" "] <- NA
price_db$`Display Format`[!(price_db$`Display Format`=="SD" | price_db$`Display Format`=="HD" | price_db$`Display Format`=="FHD" | price_db$`Display Format`=="UHD")] <- "SD"


# No of HDMI Connectors ---------------------------------------------------------
price_db$`Number of HDMI Connectors`[price_db$`Number of HDMI Connectors`=="" |  price_db$`Number of HDMI Connectors`==" "] <- NA
price_db$`Number of HDMI Connectors`[price_db$`Number of HDMI Connectors`=="Yes"] <- NA

# CI+ Module ---------------------------------------------------------
price_db$`CI+ module`[price_db$`CI+ module`==" "] <- NA


# Number of USBs ---------------------------------------------------------
price_db$`Number of USB`[price_db$`Number of USB`==" "] <- NA

# Backlight ---------------------------------------------------------
price_db$Backlight[price_db$Backlight=="" | price_db$Backlight==" "] <- NA
price_db$Backlight[price_db$Backlight=="E-LED" | price_db$Backlight=="D-LED"] <- "LED"

# Internet Features ---------------------------------------------------------
# This is combining 3 rows into 1: Internet Connectivity, Ethernet & WiFi
price_db$`Internet Connectivity`[price_db$`Internet Connectivity`=="" | price_db$`Internet Connectivity`==" "] <- NA
price_db$Ethernet[price_db$Ethernet=="" | price_db$Ethernet==" "] <- NA
price_db$WiFi[price_db$WiFi=="" | price_db$WiFi==" "] <- NA

price_db$Ethernet[price_db$Ethernet=="N"] <- "No"
price_db$Ethernet[price_db$Ethernet=="RJ-45" | price_db$Ethernet=="rj-45" | price_db$Ethernet=="Rj-45" ] <- "Yes"

price_db$WiFi[grepl("no", price_db$WiFi, ignore.case = T)] <- "No"
price_db$WiFi[grepl("Wifi", price_db$WiFi, ignore.case = T)] <- "Yes"
price_db$`Internet Connectivity`[price_db$Ethernet=="Yes"| price_db$WiFi=="Yes"] <- "Yes"
price_db <- column.rm(price_db, c("Ethernet", "WiFi"))

# Integrated DVD Player ---------------------------------------------------------
price_db$`Integrated DVD player`[price_db$`Integrated DVD player`=="" | price_db$`Integrated DVD player`==" "] <- NA
price_db$`Integrated DVD player`[(!price_db$`Integrated DVD player`== "Yes") & !is.na(price_db$`Integrated DVD player`)] <- "No"

# Curved ---------------------------------------------------------
price_db$Curved[price_db$Curved=="" | price_db$Curved==" "] <- NA
price_db$Curved[is.na(price_db$Curved)] <- "No" # <----------------------------------- Imputation, assuming they are not curved


# Platform & OS ---------------------------------------------------------
# Platform does not give much information and have huge variety
# So I try to integrate it into OS
price_db$OS[grepl("Android", price_db$OS, ignore.case = T)] <- "Android"
price_db$OS[grepl("Andriod", price_db$OS)] <- "Android"
price_db$OS[grepl("webos", price_db$OS, ignore.case = T)] <- "WebOS"
price_db$OS[grepl("web os", price_db$OS, ignore.case = T)] <- "WebOS"
price_db$OS[grepl("firefox", price_db$OS, ignore.case = T)] <- "FireFox"
price_db$OS[grepl("fire fox", price_db$OS, ignore.case = T)] <- "FireFox"
price_db$OS[grepl("firefix", price_db$OS, ignore.case = T)] <- "FireFox" # firefix really?
price_db$OS[grepl("roku", price_db$OS, ignore.case = T)] <- "Roku TV"
price_db$OS[grepl("tizen", price_db$OS, ignore.case = T)] <- "Tizen"
price_db$OS[grepl("win", price_db$OS, ignore.case = T)] <- "Windows"
price_db$OS[grepl("miui", price_db$OS, ignore.case = T)] <- "Android" # MIUI technically android but currently classified as proprietary

price_db$OS[grepl("na", price_db$OS, ignore.case = T)] <- ""
price_db$OS[grepl("no", price_db$OS, ignore.case = T)] <- ""
price_db$OS[grepl("core", price_db$OS, ignore.case = T)] <- "" # for quard core/ octa core
x <- c("Android", "WebOS", "FireFox", "Roku TV", "Tizen", "Windows")
price_db$OS[!grepl(paste(x, collapse = "|"), price_db$OS)] <- "Proprietary OS"




# Actual Cleaning

size_cat_gen <- function(df, range){
  if(class(df$`Screen Size`)!= "numeric"){print("worng class")}else{
    col <- df$`Screen Size`
    lower <- floor(col/range)*range
    upper <- ceiling(col/range)*range -1 # have error, next line is the patch
    upper[floor(col/range)*range==ceiling(col/range)*range] <- upper[floor(col/range)*range==ceiling(col/range)*range] +range
    new_col <- paste0(lower, "\"-", upper,  "\"")
    new_col <- as.df(f2c(new_col))
    names(new_col) <- paste0("Size Group (", range, "-inch)")
    return(cbind.data.frame(df, new_col))
  }
}
price_db <- size_cat_gen(price_db,5)



# Unique Value Visualisation

x <- sapply(price_db, unique)
n <- max(sapply(x, length))
for (i in 1:length(x)){
  temp <- as.array(x[[i]])
  length(temp) <- n
  if(i==1){temp_df <- temp}
  else{temp_df <- cbind(temp_df, temp)}
  rm(temp)
}
unique_df <- as.df(temp_df)
names(unique_df) <- names(price_db)




#~~~Data Cleaning End~~~#



#~~~Data Subsetting Start~~~#
price_db_2016 <- price_db[price_db$Year==2016, ]
price_db_2016 <- as.dt(price_db_2016)
price_db_2016 <- as.df(price_db_2016[, list(Price_US = mean(Price_US)), by = c(#"Year", 
                                                                               "Region", 
                                                                               #"Country", 
                                                                               "Brand", "Model", "Display", "Screen Size",
                                                                                                       "Size Group (5-inch)", "Aspect Ratio", "Brightness",
                                                                                                       "Display Format", "Number of HDMI Connectors", "CI+ module",
                                                                                                       "Backlight", "Refresh Rate", "Number of USB", "Internet Connectivity", 
                                                                                                       "Integrated DVD player", "Curved", "OS"
)])

price_db_2016$log_price <- log(price_db_2016$Price_US)
price_db_2016_complete <- price_db_2016[complete.cases(price_db_2016),]
#~~~Data Subsetting End~~~#



#~~~Regression Start~~~#

nums <- sapply(price_db_2016_complete, is.numeric)
chars <- sapply(price_db_2016_complete, is.character)
price_db_2016_complete_num <-price_db_2016_complete[ , nums]
price_db_2016_complete[,chars] <- sapply(price_db_2016_complete[,chars], as.factor)#
df.c2f <- function(df){
  for(i in 1:dim(df)[2]){
    if(class(df[,i])=="character"){
      df[,i] <- as.factor(df[,i])
    }
  }
  return(df)
}
price_db_2016_complete <- df.c2f(price_db_2016_complete)
price_db_2016_complete_num <- column.rm(price_db_2016_complete_num, "Price_US")
# vis 
pairs(price_db_2016_complete_num)


price_db_2016_complete_f2n <- price_db_2016_complete
price_db_2016_complete_f2n <-price_db_2016_complete_f2n[ , chars]
for(i in 1: dim(price_db_2016_complete_f2n)[2]){
  price_db_2016_complete_f2n[,i] <- as.numeric(price_db_2016_complete_f2n[,i])
}
cor_matrix <- cor(price_db_2016_complete_f2n)
corrplot(cor_matrix)
#~~~Regression End~~~#







