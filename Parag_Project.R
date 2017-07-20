install.packages('foreign')# for combining dbf files
install.packages('reshape')# for merging dbf files
install.packages('ggplot')
install.packages('data.table')
install.packages("lattice")
install.packages("ddply")
install.packages("maps")
install.packages("googleway")
install.packages("jpeg")

library(reshape)
library(ggplot2)
library(foreign)
library(data.table)
library(lattice)
library(jpeg)

setwd("D:\\Project\\parag Personal\\aegis\\TrafficProjec\\New folder\\corrected_file")

files = list.files(pattern = "\\.dbf$")
all.the.data <- lapply(files,read.dbf, as.is=FALSE)
merged <- merge_recurse(all.the.data)
write.csv(merged, "merged_vehicle.csv")

accident = read.csv("merged_Accident1.csv")
summary(accident)
str(accident)

accident$YEAR[accident$YEAR == 95] = 1995
accident$YEAR[accident$YEAR == 96] = 1996
accident$YEAR[accident$YEAR == 97] = 1997

subdata =  accident[which(accident$MONTH == '9'),]
length(subdata$YEAR)

# Remove Outliers
accident = subset(accident, DAY != "99")



# Fatalaties by each month of years from 1995 to 2005
month_name = c( 'Jan', 'Feb', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')

for (i in seq (1,12)) {
  #Automatically saye plots during execution. Need to restart r session later using ctrl+shift+F10
  mypath <- file.path("D:","Project","parag Personal","aegis","TrafficProjec","New folder","corrected_file","plots2", paste("Fatalaties_", month_name[i], ".jpeg", sep = ""))
  

   ggplot(accident[which(accident$MONTH == i),], aes(factor(YEAR), FATALS, group = MONTH  )) + 
           stat_summary(fun.y  = sum, geom = "line") + xlab("Year") +
           ylab("Fatalities")+ ggtitle(month_name[i])
  ggsave(file = mypath)   
}



#Fatalities by days each year



for (i in seq (1,12)) {
  
  #Automatically saye plots during execution. Need to restart r session later using ctrl+shift+F10
  mypath <- file.path("D:","Project","parag Personal","aegis","TrafficProjec","New folder","corrected_file","plots", paste("Fatalaties_Days_", month_name[i], ".jpeg", sep = ""))
  

  ggplot(accident[which(accident$MONTH == i),], aes(factor(DAY), FATALS, group = MONTH  )) + 
           stat_summary(fun.y  = sum, geom = "line") + xlab("Days") +
           ylab("Fatalities")+ ggtitle(paste("Fataities in ", month_name[i], " accross days, 1995-2005"))
  ggsave(file = mypath)  
}


# Aggregate graph of fatalaties by days in a year

df <- data.frame(date = numeric(), month = numeric(), year = numeric(),fatals = numeric())
for (y in seq(1995,2005)){
  for (m in seq(1,12)) {
    temp1 = accident[which(accident$MONTH == m),]
    temp2= temp1[which(temp1$YEAR == y),]
    for (d in seq(1,31)) {
      temp3= temp2[which(temp2$DAY == d),]
      f = (sum(temp3$FATALS))
      
      df[nrow(df) + 1, ] <- c( d,m,y,f) # add results of one loop to dataftame
      df_total = df[with(df, order(fatals)), ]
    }
  }
} 


ggplot(df_total, aes(factor(month), fatals, group = date  )) + 
  stat_summary(fun.y  = sum, geom = "path") + xlab("Month") +
  ylab("Fatalities")+ ggtitle(paste("Total fatalaties by day, 1995-2005"))
 


# Top 10 days of highest fatalities (1996-2000)


df <- data.frame(date = numeric(), month = numeric(), year = numeric(),fatals = numeric())
for (y in seq(1996,2000)){
  for (m in seq(1,12)) {
    temp1 = accident[which(accident$MONTH == m),]
    temp2= temp1[which(temp1$YEAR == y),]
    for (d in seq(1,31)) {
      temp3= temp2[which(temp2$DAY == d),]
      f = (sum(temp3$FATALS))
      
      df[nrow(df) + 1, ] <- c( d,m,y,f)
      df_00 = df[with(df, order(fatals)), ]
    }
  }
}
tail(df_00,10)

# Top 10 days of highest fatalities (2001-2005)


df <- data.frame(date = numeric(), month = numeric(), year = numeric(),fatals = numeric())
for (y in seq(2001,2005)){
  for (m in seq(1,12)) {
    temp1 = accident[which(accident$MONTH == m),]
    temp2= temp1[which(temp1$YEAR == y),]
    for (d in seq(1,31)) {
      temp3= temp2[which(temp2$DAY == d),]
      f = (sum(temp3$FATALS))
      
      df[nrow(df) + 1, ] <- c( d,m,y,f)
      df_05 = df[with(df, order(fatals)), ]
    }
  }
}
tail(df_05,10)


# Top 10 days of highest fatalities (1996-2005)


df <- data.frame(date = numeric(), month = numeric(), year = numeric(),fatals = numeric())
for (y in seq(1995,2005)){
  for (m in seq(1,12)) {
    temp1 = accident[which(accident$MONTH == m),]
    temp2= temp1[which(temp1$YEAR == y),]
    for (d in seq(1,31)) {
      temp3= temp2[which(temp2$DAY == d),]
      f = (sum(temp3$FATALS))
      
      df[nrow(df) + 1, ] <- c( d,m,y,f) # add results of one loop to dataftame
      df_total = df[with(df, order(fatals)), ]
    }
  }
} 

# Remove fatals which are 0. this indicates 30th feb as per the code.
df_total = subset(df, fatals !=0 )


# dispay top 10 days where fatalities were maximum
tail(df_total,10)


# Highest single day fatalaties 1996-2005

dt <- data.table(df_total, key="year") # Using data.table package to arrange by year
high_fat = as.data.frame(dt[, .SD[fatals %in% max(fatals)], by=year]) # Converting table to dataframe
high_fat[order(-high_fat$fatals),] # SOrt dataframe function
#create a new column in form of date from other colums

high_fat$col3 <- paste(high_fat$year, high_fat$month, high_fat$date,sep="/")

ggplot(high_fat, aes(reorder(factor(col3),fatals), fatals )) +geom_bar(stat ="identity") + geom_text(label =high_fat$fatals, hjust=-0.5 )+ 
  xlab("Date") + ylab("Fatalaties")+ ggtitle("Highest single day fatalaties 1996-2005") +coord_flip()

# Get lowest value of high_fat table
high_fat_min = min(high_fat$fatals)


# lowest single day fatalaties 1996-2005

dt <- data.table(df_total, key="year") # Using data.table package to arrange by year
low_fat = as.data.frame(dt[, .SD[fatals %in% min(fatals)], by=year]) # # Converting table to dataframe
low_fat[order(low_fat$fatals),] # SOrt dataframe function

#create a new column in form of date from other colums


low_fat$col3 <- paste(low_fat$year, low_fat$month, low_fat$date, sep="/")

ggplot(low_fat, aes(reorder(factor(col3),fatals), fatals )) +geom_bar(stat ="identity") + geom_text(label =low_fat$fatals, hjust=-0.5 )+ 
  xlab("Date") + ylab("Fatalaties")+ ggtitle("lowest single day fatalaties 1996-2005") +coord_flip()

#################################################################

# Highest & Lowest single day fatalaties

high_low_fat = rbind(high_fat, low_fat)
high_low_fat$col3 <- as.Date((high_low_fat$col3))


#Arrange Year column in ascending order
high_low_fat = high_low_fat[order(year),]
high_low_fat_final = high_low_fat[with(high_low_fat, order(col3)), ]


ggplot(high_low_fat_final, aes(factor(col3),fatals, fill=fatals >= high_fat_min )) +geom_bar(stat ="identity") + geom_text(label =high_low_fat_final$fatals, hjust=-0.5 )+ 
  xlab("Date") + ylab("Fatalaties")+ ggtitle("Highest & Lowest single day fatalaties 1996-2005") +coord_flip()


###########################################################################

# RaTIO of highest to lowest single dat fatalaties


#################################################################################

#Trends by each day of week on different graphs

week_name = c( 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')

for (i in seq (1,7)) {
  #Automatically saye plots during execution. Need to restart r session later using ctrl+shift+F10
  mypath <- file.path("D:","Project","parag Personal","aegis","TrafficProjec","New folder","corrected_file","plots", paste(week_name[i],"_Fatalaties", ".jpeg", sep = ""))
  
  ggplot(accident[which(accident$DAY_WEEK == i),], aes(factor(YEAR), FATALS, group = DAY_WEEK  )) + 
           stat_summary(fun.y  = sum, geom = "line") + xlab("Year") +
           ylab("Fatalities")+ ggtitle(week_name[i])
  ggsave(file = mypath, width=7, height=7)
}


#################################################################################

#Trends by each day of week on same graphs


ggplot() + 
  stat_summary(aes(factor(YEAR), FATALS, group = DAY_WEEK, color = "Monday"  ),data=accident[which(accident$DAY_WEEK == 1),],fun.y=sum,geom='line') +
  stat_summary(aes(factor(YEAR), FATALS, group = DAY_WEEK, color = "Tuesday"   ),data=accident[which(accident$DAY_WEEK == 2),],fun.y=sum,geom='line')+
  stat_summary(aes(factor(YEAR), FATALS, group = DAY_WEEK, color = "Wednesday"   ),data=accident[which(accident$DAY_WEEK == 3),],fun.y=sum,geom='line')+
  stat_summary(aes(factor(YEAR), FATALS, group = DAY_WEEK, color = "Thursday"   ),data=accident[which(accident$DAY_WEEK == 4),],fun.y=sum,geom='line')+
  stat_summary(aes(factor(YEAR), FATALS, group = DAY_WEEK, color = "Friday"   ),data=accident[which(accident$DAY_WEEK == 5),],fun.y=sum,geom='line')+
  stat_summary(aes(factor(YEAR), FATALS, group = DAY_WEEK, color = "Saturday"   ),data=accident[which(accident$DAY_WEEK == 6),],fun.y=sum,geom='line')+
  stat_summary(aes(factor(YEAR), FATALS, group = DAY_WEEK, color = "Sundays"   ),data=accident[which(accident$DAY_WEEK == 7),],fun.y=sum,geom='line')+
  
  theme(legend.title=element_blank())

# Average fatalaties per day by day of week

day_week <- data.frame(day = numeric(),fatals = numeric())

  for (m in seq(1,7)) {
    temp1 = accident[which(accident$DAY_WEEK == m),]
    
      f = (sum(temp1$FATALS))
      
      day_week[nrow(day_week) + 1, ] <- c( week_name[m],f) # add results of one loop to dataftame
      day_week = day_week[with(day_week, order(fatals)), ]
    }
  




######################################################################################
# Monthly Drunk driving cases

month_name = c( 'Jan', 'Feb', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')

for (i in seq (1,12)) {
  #Automatically saye plots during execution. Need to restart r session later using ctrl+shift+F10
  
  mypath <- file.path("D:","Project","parag Personal","aegis","TrafficProjec","New folder","corrected_file","plots", paste(month_name[i],"_Drunk_Fatalaties", ".jpeg", sep = ""))
  ggplot(accident[which(accident$MONTH == i),], aes(factor(YEAR), DRUNK_DR, group = MONTH  )) + 
         stat_summary(fun.y  = sum, geom = "line") + xlab("Year") +
         ylab("Fatalities")+ ggtitle(paste("Total Drunk Driving cases in ", month_name[i], "(1996-2005)"))
  ggsave(file = mypath, width=7, height=7 )
}

#  drunk driving cases per day 

ggplot() + 
  stat_summary(aes(factor(YEAR), DRUNK_DR, group = DAY_WEEK, col = "Monday"  ),data=accident[which(accident$DAY_WEEK == 1),],fun.y=sum,geom='line') +
  stat_summary(aes(factor(YEAR), DRUNK_DR, group = DAY_WEEK, col = "Tuesday"   ),data=accident[which(accident$DAY_WEEK == 2),],fun.y=sum,geom='line')+
  stat_summary(aes(factor(YEAR), DRUNK_DR, group = DAY_WEEK, col = "Wednesday"   ),data=accident[which(accident$DAY_WEEK == 3),],fun.y=sum,geom='line')+
  stat_summary(aes(factor(YEAR), DRUNK_DR, group = DAY_WEEK, col = "Thursday"   ),data=accident[which(accident$DAY_WEEK == 4),],fun.y=sum,geom='line')+
  stat_summary(aes(factor(YEAR), DRUNK_DR, group = DAY_WEEK, col = "Friday"   ),data=accident[which(accident$DAY_WEEK == 5),],fun.y=sum,geom='line')+
  stat_summary(aes(factor(YEAR), DRUNK_DR, group = DAY_WEEK, col = "Saturday"   ),data=accident[which(accident$DAY_WEEK == 6),],fun.y=sum,geom='line')+
  stat_summary(aes(factor(YEAR), DRUNK_DR, group = DAY_WEEK, col = "Sundays"   ),data=accident[which(accident$DAY_WEEK == 7),],fun.y=sum,geom='line')+
  
  theme(legend.title=element_blank())
#################################################################################################

week_name = c( 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')

for (i in seq (1,7)) {
  print (ggplot(accident[which(accident$DAY_WEEK == i),], aes(factor(YEAR), DRUNK_DR, group = DAY_WEEK  )) + 
           stat_summary(fun.y  = sum, geom = "line") + xlab("Year") +
           ylab("Fatalities")+ ggtitle(week_name[i])) 
}

#################################################################################################
# % of fatalaties due to drunk driving per year

# Creating a new data frame which calculates the percentage
fatals.drunk = data.frame(drunk.year = numeric(), drunk.fatals = numeric())
for (y in seq(1996,2005)) {
  temp4 = accident[which(accident$YEAR == y),]
  avg = round((sum(temp4$DRUNK_DR)/sum(temp4$FATALS))* 100)
  fatals.drunk[nrow(fatals.drunk) + 1, ] <- c( y,avg)
  
} 

# Plotting the graph
ggplot(fatals.drunk, aes(factor(drunk.year), drunk.fatals, group =1 )) + 
  geom_line()+ xlab("Year") +
  ylab("Percent")+ ggtitle("Percentage of accident fatalaties due to drunk driving")

#################################################################################################

# Total fatalaties in all US states Map view


# Data preparation for Maps. 
library(maps)
# This will calculate sum f fatals per state in US
value.vector = c()
for (n in seq(1,56)) { 
  temp5 = accident[which(accident$STATE == n),]
  data = sum(temp5$FATALS)
  value.vector = append(value.vector, data)
}
print (value.vector)

value.vector1 = value.vector[value.vector != 0] # Remove Puertorico and other which are 0
value.vector2 = value.vector1[c(-15,-2)] #Remove Hawaii, Alaska and puorto rico since they are not part of US map in R

# Defining regions vector
usstates = readLines("USSTATES.csv") # reads CSV file as character vector
world.frame =  data.frame(states = usstates, values = (value.vector2))
region_char = as.character(world.frame$states)
values_char =  world.frame$values

map.text("state",regions= region_char, labels = as.character(value.vector2))
         
# Heat map 
world.frame =  data.frame(states = usstates, values = (value.vector2))

# define colors
colors <- heat.colors (6)
# categorize in different class for yvar
world.frame$colorBuckets <- as.numeric(cut(world.frame$values, c(0, 1000, 5000, 15000,20000, 25000,30000,35000,40000,45000)))
# corresponding legend text
legdtxt <- c("<1000", "1000-5000", "5000-15000", "15000-20000", "20000-25000", "25000-30000", "30000-35000", "35000-40000", "40000-45000")
legdtxt = rev(legdtxt)

install.packages("mapproj")
library(mapproj)

# plot map 
dev.new(width=50,height=50) # Set dimensions of Plot window

map("state", col = colors[world.frame$colorBuckets], fill = TRUE, lty = 1, lwd = 0.2,    projection="polyconic")
legend("bottomright", legdtxt, horiz = FALSE, fill = colors)

###############################################################################
# Reason for accidents and fatalaties

actual_reason= read.csv("AccidentReason.csv", header = TRUE, as.is = TRUE ) # Read reasons file from csv with headers
actual_reaso_vector = actual_reason$header# Convert to vecore


reason_df = data.frame(reason = character(), sum_fatals = numeric(),stringsAsFactors=FALSE)
for (y in seq(1,50)) {
  temp5 = accident[which(accident$HARM_EV == y),]
  result = sum(temp5$FATALS)
  reason_df[nrow(reason_df) + 1, ] <- c( (actual_reaso_vector[y]), result)
}

#print (reason_df)
#class(reason_df$sum_fatals)

dt <- data.table(reason_df, key="reason") # Using data.table package to arrange by year
high_reason = as.data.frame(dt[, .SD[sum_fatals %in% max(sum_fatals)], by=reason]) # Converting table to dataframe
high_reason = high_reason[order(-(as.numeric(high_reason$sum_fatals))),] # SOrt dataframe function

# Top 10 reason for  fatalaties and accident
head(high_reason,10)

# Top 10 low reason for fatalaies and accident
tail(high_reason,10)

###################################################################
#Fatalaties at construction/maintainence site over 10 years

# To avoid data frame to create factor variables, we write stringsAsFactors=FALSE.

const.main.fatals = data.frame(site = character(), sum_fatals = numeric(), stringsAsFactors=FALSE)
const.main.vector = c("Construction", "Maintainence")

for (y in seq(1,2)) {
  temp6 = accident[which(accident$C_M_ZONE == y),]
  result = sum(temp6$FATALS)
  const.main.fatals[nrow(const.main.fatals) + 1, ] <- c( const.main.vector[y], result)
}
print (const.main.fatals)

#############################################################################
# number of Accidents due to lighting condition

lights.fatals = data.frame(light_conditions = character(), sum_fatals = numeric(), stringsAsFactors=FALSE)
light.vector = c("Daylight", "Dark", "Dark but Lighted", "Dawn", "Dusk" )

#Remove outliers. Fatalaties for which lighting conditions are not known
accident.light = subset(accident, LGT_COND != "9")

for (y in seq(1,5)) {
  temp7 = accident[which(accident$LGT_COND == y),]
  result = sum(temp7$FATALS)
  lights.fatals[nrow(lights.fatals) + 1, ] <- c( light.vector[y], result)
}
print (lights.fatals)

#############################################################################
# number of Accidents due to Weather condition

weather.fatals = data.frame(weather_conditions = character(), sum_fatals = numeric(), stringsAsFactors=FALSE)
weather.vector = c("No Adverse atmospheric conditions", "Rain", "Sleet(Hail)", "Snow", "Fog", "Rain & Fog", "Sleet & Fog", "Other: Smog, Smoke, Blowing Sand or Dust" )

#Remove outliers. Fatalaties for which weather conditions are not known
accident.weather = subset(accident, WEATHER != "9")


for (y in seq(1,8)) {
  temp8 = accident.weather[which(accident.weather$WEATHER == y),]
  result = sum(temp8$FATALS)
  weather.fatals[nrow(weather.fatals) + 1, ] <- c( weather.vector[y], result)
}

print (weather.fatals)




