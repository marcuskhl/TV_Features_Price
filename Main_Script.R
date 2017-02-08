if(is.na(match(c("devtools"),installed.packages()[,"Package"]))) install.packages(new.packages) else library(devtools)
suppressMessages(devtools::install_github("marcusihsmarkit/BasicSettings"));library(BasicSettings)

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



# Display ---------------------------------------------------------
price_db <- price_db[!price_db$Display=="PDP TV",] # according to wikipedia, us pdp market ended in 2014, anything we are still scraping for PDP
# would act as noise more than information
price_db <- price_db[!price_db$Display=="RPTV",] # old technology

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
# I think the most important division line is > 100Hz vs < 100Hz
# basically, u have the standard 50 and 60 Hz then you have the 100, 120 and 200 240
# it is often argued that >120, it difference is minimal, that is why I grouped it into a new one
price_db$`Refresh Rate Grouped` <- price_db$`Refresh Rate`
price_db$`Refresh Rate Grouped` <- gsub("Hz", "", price_db$`Refresh Rate Grouped`)
price_db$`Refresh Rate Grouped`[is.na(price_db$`Refresh Rate Grouped`)] <- 60 # IMPUTATION again
price_db$`Refresh Rate Grouped` <- as.numeric(price_db$`Refresh Rate Grouped`)
price_db$`Refresh Rate Grouped`[price_db$`Refresh Rate Grouped`>100] <- ">100Hz"
price_db$`Refresh Rate Grouped`[!price_db$`Refresh Rate Grouped`==">100Hz"] <- "=<100Hz"


# Display Format ---------------------------------------------------------
price_db$`Display Format`[price_db$`Display Format`=="" |  price_db$`Display Format`==" "] <- NA
price_db$`Display Format`[!(price_db$`Display Format`=="SD" | price_db$`Display Format`=="HD" | price_db$`Display Format`=="FHD" | price_db$`Display Format`=="UHD")] <- "FHD" # Imputation

# No of HDMI Connectors ---------------------------------------------------------
price_db$`Number of HDMI Connectors`[price_db$`Number of HDMI Connectors`=="" |  price_db$`Number of HDMI Connectors`==" "] <- 1 # imputation
price_db$`Number of HDMI Connectors`[price_db$`Number of HDMI Connectors`=="Yes"] <- 1 # imputation

# CI+ Module ---------------------------------------------------------
price_db$`CI+ module`[price_db$`CI+ module`==" "] <- NA


# Number of USBs ---------------------------------------------------------
price_db$`Number of USB`[price_db$`Number of USB`=="" | price_db$`Number of USB`==" "] <- 2 # average # of usb ports are 1.98 # this is imputation
price_db$`Number of USB`[is.na(price_db$`Number of USB`)] <- 2
price_db$`Number of USB Grouped`[price_db$`Number of USB`<=2] <- "0-2"
price_db$`Number of USB Grouped`[price_db$`Number of USB`>2] <- ">=3"

# Backlight ---------------------------------------------------------
price_db$Backlight[price_db$Backlight=="" | price_db$Backlight==" "] <- NA
price_db$Backlight[is.na(price_db$Backlight)] <- price_db$Display[is.na(price_db$Backlight)] 
# only a handful of each for UHP and DLP, and they are outdated technologies. CCFL - just outdated
price_db <- price_db[!(price_db$Backlight=="UHP" | price_db$Backlight=="DLP" | price_db$Backlight=="CCFL"),] 
price_db$Backlight[price_db$Backlight=="E-LED" | price_db$Backlight=="D-LED"] <- "LED" # E-LED wasnt significant



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
price_db$`Integrated DVD player`[price_db$`Integrated DVD player`=="" | price_db$`Integrated DVD player`==" "] <- "No"# Imputation! Assume no if not specified
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


# Brand ----------------------------------------------------------------
price_db$Brand[grepl("orion", price_db$Brand, ignore.case = T)] <- "ORION"
brand_list <- c("Changhong", "Haier", "Hisense", "LG", "Orion", "Panasonic", "Philips", "Samsung", "Sharp", "Skyworth", "Sony", "TCL", "Toshiba", "Vizio") # brand list to track
# anything else will be Others
brand_list <- toupper(brand_list)
price_db$Brand[!price_db$Brand%in%brand_list] <- "OTHERS"

#~~~Manual Research Start~~~#
price_db$Curved[price_db$Model=="65EG9600"] <- "Yes"
price_db$Curved[price_db$Model=="KN55S9C"] <- "Yes"
price_db$`Number of USB Grouped`[price_db$Model=="UN60F6300"] <- ">=3"
#~~~Manual Research End~~~#




# Actual Cleaning

# range_gen <- function(df, col_ref, range){
#   df <- fn.is.dt.start(df)
#   if(class(df[,match(col_ref, names(df))])!= "numeric"){print("worng class")}else{
#     col <- df[,match(col_ref, names(df))]
#     lower <- floor(col/range)*range
#     upper <- ceiling(col/range)*range -1 # have error, next line is the patch
#     upper[lower==(upper+1)] <- upper[lower==(upper+1)] +range
#     new_col <- paste0(lower, "\"-", upper,  "\"")
#     new_col <- as.df(f2c(new_col))
#     names(new_col) <- paste0("Size Group (", range, "-inch)")
#     df <- cbind.data.frame(df, new_col)
#     return(fn.is.dt.end(df,dt_flag))
#   }
# }
price_db <- BasicSettings::range_gen(price_db, "Screen Size",5)

price_db$Brightness[is.na(price_db$Brightness)] <- 0
price_db <- BasicSettings::range_gen(price_db,"Brightness", 200)
price_db$Brightness[price_db$Brightness==0] <- NA
price_db <- df.name.change(price_db, "Size Group (200-inch)", "Brightness Group (200-nit)") # was 100 nit but they are insig. beyond 500nit, which makes a lot of sense given how human eye works
price_db$`Brightness Group (200-nit)` <- gsub("\"", "", price_db$`Brightness Group (200-nit)`)


# Unique Value Visualisation

# wrote all this for nothing! not until I try to write a function with that exact name did I realise
# x <- sapply(price_db, unique)
# n <- max(sapply(x, length))
# for (i in 1:length(x)){
#   temp <- as.array(x[[i]])
#   length(temp) <- n
#   if(i==1){temp_df <- temp}
#   else{temp_df <- cbind(temp_df, temp)}
#   rm(temp)
# }
# unique_df <- as.df(temp_df)
# names(unique_df) <- names(price_db)
unique_df <- unique.data.frame(price_db)

#~~~Data Cleaning End~~~#



#~~~Data Subsetting + 2016 Cleaning Start~~~#

# sorry for the confusing naming, decided to regress with more data, hence taking year into account 
# next step would be taking only 2014 onwards, where UHD really start to boom
# could determine it by outlier formula when we have time, but right, now, this is a straight cut from intuition
#price_db_2016 <- price_db[price_db$Year==2016 & price_db$Country=="US", ]
price_db_2016 <- price_db[ price_db$Country=="US", ] # in order to increase the amount of training data, I left out Year ==2016 filter
# negative effect of taking all years into account:
# overpredicts SD & HD price by coefficient (remember its logged) 0.1 and 0.02



# there are only 6 SD entries, group them into HD
# price_db_2016$`Display Format`[price_db_2016$`Display Format`=="SD"] <- "HD"

df_process <- function(df){
  
  df <- as.dt(df)
  return(as.df(df[, list(Price_US = mean(Price_US)), by = c("Year", 
                                                                                 #"Region", 
                                                                                 #"Country", 
                                                                                 "Brand", 
                                                                                 "Model", "Display", "Screen Size",
                                                                                 "Size Group (5-inch)", 
                                                                                 "Aspect Ratio", 
                                                                                 "Brightness Group (200-nit)",
                                                                                 "Display Format", 
                                                                                 "Number of HDMI Connectors", 
                                                                                 # "CI+ module", # this doesnt do anything
                                                                                 "Backlight", 
                                                                                 "Refresh Rate Grouped", 
                                                                                 "Number of USB Grouped", 
                                                                                 "Internet Connectivity", 
                                                                                 "Integrated DVD player", 
                                                                                 "Curved", 
                                                                                 "OS"
  )]))
}

price_db_2016 <- df_process(price_db_2016)
price_db_2016$log_price <- log(price_db_2016$Price_US)
price_db_2016_complete <- price_db_2016[complete.cases(price_db_2016),]
#~~~Data Subsetting + 2016 Cleaning End~~~#



#~~~Regression Start~~~#

#~~~Regression Vis Start~~~#
price_db_2016_complete_num <- df.class.extract(price_db_2016_complete,"numeric")
price_db_2016_complete[is.na(price_db_2016_complete)] <- ""
price_db_2016_complete <- df.c2f(price_db_2016_complete)

price_db_2016[is.na(price_db_2016)] <- ""
price_db_2016 <- df.c2f(price_db_2016)

price_db_2016_complete_num <- column.rm(price_db_2016_complete_num, "Price_US")
# vis 
pairs(price_db_2016_complete_num)


price_db_2016_complete_f2n <- price_db_2016_complete
price_db_2016_complete_f2n <- lapply(price_db_2016_complete_f2n, as.numeric)


price_db_2016_complete_f2n <- as.df(price_db_2016_complete_f2n)
cor_matrix <- cor(price_db_2016_complete_f2n)
x11()
corrplot(cor_matrix)
cor_matrix[lower.tri(cor_matrix,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
cor_matrix=as.data.frame(as.table(cor_matrix))  #Turn into a 3-column table
cor_matrix=na.omit(cor_matrix)  #Get rid of the junk we flagged above
cor_list <-as.df(cor_matrix[order(-abs(cor_matrix$Freq)),])    #Sort by highest correlation (whether +ve or -ve)
#~~~Regression Vis End~~~#

#~~~Real Deal Start~~~#

#training <- price_db_2016_complete 

best_tv_regression <- function(df, no_dvd = F){
  training <-  df

  if(no_dvd == F){
    pricing_lm <- lm(log_price ~ Year+ Brand +
                       `Size Group (5-inch)` + Display + Backlight + `Display Format` + `Number of HDMI Connectors` + `Brightness Group (200-nit)` + `Refresh Rate Grouped`
                     + `Number of USB Grouped` + `Internet Connectivity` + `Curved` 
                     # + `Integrated DVD player` # this doesnt do jack, having DVD player actually makes the price marginally cheaper which makes no sense
                     +OS,
                     training)
  }else{
    pricing_lm <- lm(log_price ~ Year+ Brand +
                       `Size Group (5-inch)` + Display + Backlight + `Display Format` + `Number of HDMI Connectors` + `Brightness Group (200-nit)` + `Refresh Rate Grouped`
                     + `Number of USB Grouped` + `Internet Connectivity` + `Curved` +OS,
                     training)
  }
  
  return(pricing_lm)
}  
  
#~~~Prediction Start~~~#
price_db_2016_posh <- price_db_2016[price_db_2016$Price_US>10000,]
price_db_2016_posh <- column.rm(price_db_2016_posh, "Integrated DVD player" ) # all no
price_db_2016_not_posh <- price_db_2016[price_db_2016$Price_US<=10000,]
pricing_lm_posh <- best_tv_regression(price_db_2016_posh, no_dvd = T)
pricing_lm_not_posh <- best_tv_regression(price_db_2016_not_posh, no_dvd = F)

post_reg_vis <- function(lm, df, title){
  df$Modelled.Price <- exp(predict(lm, df))
  x11()
  plot(df$Price_US,df$Modelled.Price,main = title)
  abline(0,1,col="red")
  df$delta_price <- df$Modelled.Price - df$Price_US 
  return(df)
}
price_db_2016_posh <- post_reg_vis(pricing_lm_posh, price_db_2016_posh, "training posh")
price_db_2016_not_posh <- post_reg_vis(pricing_lm_not_posh, price_db_2016_not_posh, "training not posh")



price_db_US_2016 <- price_db[price_db$Year==2016 & price_db$Country=="US", ]
price_db_US_2016 <- df_process(price_db_US_2016)



price_db_US_2016_not_posh <- price_db_US_2016[price_db_US_2016$Price_US<=10000,]
price_db_US_2016_posh <- price_db_US_2016[price_db_US_2016$Price_US>10000,]
price_db_US_2016_not_posh <- post_reg_vis(pricing_lm_not_posh, price_db_US_2016_not_posh, "prediction not posh")
price_db_US_2016_posh <- post_reg_vis(pricing_lm_posh, price_db_US_2016_posh, "prediction posh")

US_2016_pricing <- rbind.data.frame(price_db_US_2016_posh, price_db_US_2016_not_posh)


coeff_list <- as.df(pricing_lm_not_posh$coefficients)
coeff_list$coeff.name <- row.names(coeff_list)
coeff_list <- coeff_list[order(-abs(coeff_list$dt)),] # this tells us the most important factors
#~~~Prediction End~~~#

#~~~Real Deal End~~~#
#~~~Regression End~~~#



#~~~Output Start~~~#
brand_list <- as.df(brand_list)
names(brand_list) <- "Brand"

df.unique <- fn.piper(as.df, unique)

display <- df.unique(US_2016_pricing$Display)
names(display) <- "Display"

big_table <- brand_list[rep(row.names(brand_list),dim(display)[1] ),]
big_table <- cbind.data.frame(big_table,display)

# screen_size_group <- df.unique(US_2016_pricing$`Size Group (5-inch)`)
# names(screen_size_group) <- "Size Group (5-inch)"
# 
# big_table <- big_table[rep(row.names(big_table),dim(screen_size_group)[1] ),]
# big_table <- cbind.data.frame(big_table,screen_size_group)
# 
# aspect_ratio <- df.unique(US_2016_pricing$`Aspect Ratio`)
# names(aspect_ratio) <- "Aspect Ratio"
# 
# big_table <- big_table[rep(row.names(big_table),dim(aspect_ratio)[1] ),]
# big_table <- cbind.data.frame(big_table,aspect_ratio)
# 
# backlight <- df.unique(US_2016_pricing$Backlight)
# names(backlight) <- "Backlight"
# 
# big_table <- big_table[rep(row.names(big_table),dim(backlight)[1] ),]
# big_table <- cbind.data.frame(big_table,backlight)


col_list <- c("Size Group (5-inch)", "Aspect Ratio", "Backlight", "Display Format", "Number of HDMI Connectors", "Brightness Group (200-nit)",
  "Refresh Rate Grouped", "Number of USB Grouped", "Internet Connectivity", "Curved", "OS")
for (i in 1:3){
  temp <- df.unique(US_2016_pricing[,match(col_list[i],names(US_2016_pricing))])
  names(temp) <- col_list[i]
  big_table <- big_table[rep(row.names(big_table),dim(temp)[1] ),]
  big_table <- cbind.data.frame(big_table,temp)
  
}
#~~~Output End~~~#




















