if(is.na(match(c("devtools"),installed.packages()[,"Package"]))) install.packages(new.packages) else library(devtools)
suppressMessages(devtools::install_github("marcuskhl/BasicSettings"));library(BasicSettings)

price_db <- as.df(fread("M:/Technology/DATA/TV_sets_model/WW TV Price Tracker/Tracker Price Model Output/Pricing_Output.csv", header = T))


#~~~Data Cleaning Start~~~#
names(price_db) <- gsub(" ($)","_US", names(price_db), fixed = T)
names(price_db) <- gsub(" (Local)","_Loc", names(price_db), fixed = T)
names(price_db) <- gsub(" (LAN / RJ-45)","", names(price_db), fixed = T)

price_db$Country <- toupper(price_db$Country)
price_db$Brand <- toupper(price_db$Brand)

blank_reducer <- function(col){
  col <- gsub("  "," ", col)
  col <- gsub("  "," ", col)
  col <- gsub(" ","", col)
  col[col==""] <- NA
}
blank_remover <- function(col){col[col==""| col==" "] <- NA}
for (i in 1:dim(price_db)[2]){
  price_db[,i] <- blank_reducer(price_db[,i])
  price_db[,i] <- blank_remover(price_db[,i])
}



x <- sapply(price_db, class)
for(i in 1:length(x)){
  if(x[i]=="integer" | x[i]=="numeric" ){
    price_db[,i] <- f2n(price_db[,i]) #somehow using apply is even slower on this line
  }else{
    price_db[,i] <- f2c(price_db[,i])
  }
}



price_db <- column.rm(price_db, c("Size Group (5-inch)","Size Group (10-inch)")) # this is not present in every row for some reason, recalculated later
price_db <- column.rm(price_db, "Power Consumption (Watts)") # aint nobody got time for this 



# Brightness ---------------------------------------------------------
price_db$Brightness[price_db$Brightness>=1000] <- NA
price_db$Brightness[price_db$Brightness==""| price_db$Brightness==" "] <- NA

# Aspect Ratio ---------------------------------------------------------
price_db$`Aspect Ratio`[price_db$`Aspect Ratio`==""| price_db$`Aspect Ratio`==" "] <- NA
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
price_db$`Display Format`[price_db$`Display Format`==""| price_db$`Display Format`==" "] <- NA
price_db$`Display Format`[!(price_db$`Display Format`=="SD" | price_db$`Display Format`=="HD" | price_db$`Display Format`=="FHD" | price_db$`Display Format`=="UHD")] <- "SD"


# No of HDMI Connectors ---------------------------------------------------------
price_db$`Number of HDMI Connectors`[price_db$`Number of HDMI Connectors`==""| price_db$`Number of HDMI Connectors`==" "] <- NA
price_db$`Number of HDMI Connectors`[price_db$`Number of HDMI Connectors`=="Yes"] <- NA

# CI+ Module ---------------------------------------------------------
price_db$`CI+ module`[price_db$`CI+ module`==""| price_db$`CI+ module`==" "] <- NA

# Backlight ---------------------------------------------------------
price_db$Backlight[price_db$Backlight==""| price_db$Backlight==" "] <- NA
price_db$Backlight[price_db$Backlight=="E-LED" | price_db$Backlight=="D-LED"] <- "LED"

# Internet Features ---------------------------------------------------------
# This is combining 3 rows into 1: Internet Connectivity, Ethernet & WiFi
price_db$Internet[price_db$Backlight==""| price_db$Backlight==" "] <- NA
price_db$Ethernet[price_db$Ethernet==""| price_db$Ethernet==" "] <- NA
price_db$WiFi[price_db$WiFi==""| price_db$WiFi==" "] <- NA



# Actual Cleaning
# price_db$`Size Group (5-inch)` <- dt.double_quote_fix(price_db$`Size Group (5-inch)`)
# price_db$`Size Group (10-inch)` <- dt.double_quote_fix(price_db$`Size Group (10-inch)` )

size_cat_gen <- function(df, range){
  if(class(df$`Screen Size`)!= "numeric"){print("worng class")}else{
    col <- df$`Screen Size`
    lower <- floor(col/range)*range
    upper <- ceiling(col/range)*range -1 # have error, next line is the patch
    upper[floor(col/range)*range==ceiling(col/range)*range] <- upper[floor(col/range)*range==ceiling(col/range)*range] +range
    new_col <- paste0(lower, "\"-", upper,  "\"")
    new_col <- as.df(new_col)
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

# use complete.cases to rule out rows with NAs

#~~~Data Cleaning End~~~#

price_db_2016 <- price_db[price_db$Year==2016, ]
price_db_2016 <- as.dt(price_db_2016)
price_db_2016 <- price_db_2016[, list(Price_US = mean(Price_US), Price_Loc = mean(Price_Loc) ), by = c("Year", "Region", "Country", "Brand", "Model", "Display", "Screen Size",
                                                                                                       "Size Group (5-inch)", "Aspect Ratio", "Brightness",
                                                                                                       "Display Format", "Number of HDMI Connectors", "CI+ module",
                                                                                                       "Backlight", "Refresh Rate", "Number of USB", "Internet Connectivity", 
                                                                                                       "Integrated DVD player", "Curved", "OS"
)]