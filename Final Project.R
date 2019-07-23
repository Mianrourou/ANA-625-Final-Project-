#packages
library(caret)
library(gpairs)
library(corrplot)
library(lattice)
library(stringr)
library(ggplot2)
library(reshape2)
library(sqldf)
library(lubridate)
library(e1071)
library(hglm)
library(gpairs)
library(corrplot)
library(lattice)
library(stringr)
library(ggplot2)
library(reshape2)
library(sqldf)
library(Hmisc)
library(psych)
library(moments)
library(caret)
library(HistData)
library(vcd)
library(vcdExtra)
library(ggcorrplot)
library(modelr)
library(lubridate)
library(MASS)
library(gpairs)
library(corrplot)
library(data.table)
library(dplyr)
library(e1071)
library(psych)
library(car)
library(Metrics)
library(stats)

#rename the data before merging two original data together
data1=treas_parking_payments_2018_datas
data2=treas_parking_meters_loc_datasd

#rename variable "pole" into "pole_id" in data 2
setnames(data2, "pole", "pole_id")

#merging data1 and data2 by "pole_id"
Merged_Data <- merge(data1,data2,by="pole_id")

#Filter to Downtown
Merged_Data <- Merged_Data %>%
  filter(Merged_Data$zone=="Downtown")
range(Merged_Data$trans_amt)

#adding a new variable called time_start and time_end to calculate duration
Merged_Data$time_start=format(as.POSIXct(Merged_Data$trans_start,"%Y-%M-%D %H:%M:%S") ,format = "%H:%M:%S")
Merged_Data$time_end=format(as.POSIXct(Merged_Data$meter_expire,"%Y-%M-%D %H:%M:%S") ,format = "%H:%M:%S")

#adding a new variable called date to see the relationship between date and other variables in the future coding
Merged_Data$date=format(as.POSIXct(Merged_Data$trans_start,"%Y-%M-%D %H:%M:%S") ,format = "%D")

#adding a new variable called week to see the relationship between week and other variables in the future coding
Merged_Data$week <- weekdays(as.POSIXct(Merged_Data$trans_start),abbreviate = F)

#adding a new variable called old_duration(there are going to be 2 different kinds of duration due to different ways of calculating)
Merged_Data$old_duration <- as.numeric(difftime(strptime(paste(Merged_Data$time_end),"%H:%M:%S"),
                                                strptime(paste(Merged_Data$time_start),"%H:%M:%S")))

#creating variable called config_name_fixed (the raw data variable is not good formatted, basically this step is to format them to prepare for future use)
Merged_Data$config_name_fixed=Merged_Data$config_name

#creating new dataset called dt(Downtown)since we are only going to focus on DT area
dt=Merged_Data

#change format of config name fixed into character so function “replace” will work without error, and then run the replace function
dt$config_name_fixed=as.character(dt$config_name_fixed)
dt$config_name_fixed=as.factor(dt$config_name_fixed)
levels(dt$config_name_fixed)

str(dt$config_name_fixed)
#sample code
dt$config_name_fixed=
  replace(dt$config_name_fixed,
          dt$config_name_fixed=="1 Hour Max $1.25 HR 10am-8pm Mon-Sat",
          "1.00 Hour Max $1.25 HR 10:00am-08:00pm Mon-Sat ")
dt$config_name_fixed=
  replace(dt$config_name_fixed,
          dt$config_name_fixed=="15 Min Max $1.25 7am-7pm Daily",
          "0.25 Hour Max $1.25 HR 07:00am-07:00pm Mon-Sat ")
dt$config_name_fixed=
  replace(dt$config_name_fixed,
          dt$config_name_fixed=="2 Hour Max $1.25 HR 8am-6pm Mon-Fri (Mobile Pay) ",
          "2.00 Hour Max $1.25 HR 08:00am-06:00pm Mon-Fri")
dt$config_name_fixed=
  replace(dt$config_name_fixed,
          dt$config_name_fixed=="2 Hour Max $1.25 HR 9am-4pm Mon-Fri (Mobile Pay)",
          "2.00 Hour Max $1.25 HR 09:00am-04:00pm Mon-Fri")
dt$config_name_fixed=
  replace(dt$config_name_fixed,
          dt$config_name_fixed=="MSPM 8 Hour Max $0.50 HR 10am-8pm Mon-Sat (Normal Street)",
          "8.00 Hour Max $0.50 HR 10:00am-08:00pm Mon-Sat")
#1                  
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "1 Hour Max $1.25 HR 10am-8pm Mon-Sat (Mobile Pay)" ,   "1.00 Hour Max $1.25 HR 10:00am-08:00pm Mon-Sat (Mobile Pay)"   )
#2                  
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "1 Hour Max $1.25 HR 8am-4pm Mon-Fri 8am-6pm Sat"   ,   "1.00 Hour Max $1.25 HR 08:00am-04:00pm Mon-Fri 08:00am-06:00pm Sat"    )
#3                  
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "1 Hour Max $1.25 HR 8am-4pm Mon-Sat"   ,   "1.00 Hour Max $1.25 HR 08:00am-04:00pm Mon-Sat"    )
#4                  
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "1 Hour Max $1.25 HR 8am-5pm Mon-Sat, No Parking 5pm-2am, 3 Min Passenger Loading"  ,   "1.00 Hour Max $1.25 HR 08:00am-05:00pm Mon-Sat, No Parking 5pm-2am, 3 Min Passenger Loading"   )
#5                  
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "1 Hour Max $1.25 HR 8am-6pm Mon-Sat"   ,   "1.00 Hour Max $1.25 HR 08:00am-06:00pm Mon-Sat"    )
#6                  
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "1 Hour Max $1.25 HR 8am-6pm Mon-Sat (Mobile Pay)"  ,   "1.00 Hour Max $1.25 HR 08:00am-06:00pm Mon-Sat (Mobile Pay)"   )
#7                  
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "15 Min Max $1.25 HR 10am-8pm Mon-Fri  (Mobile Pay)"    ,   "0.25 Hour Max $1.25 HR 10:00am-08:00pm Mon-Fri  (Mobile Pay)"  )
#8                  
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "15 Min Max $1.25 HR 10am-8pm Mon-Sat  (Mobile Pay)"    ,   "0.25 Hour Max $1.25 HR 10:00am-08:00pm Mon-Sat  (Mobile Pay)"  )
#9                  
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "15 Min Max $1.25 HR 10am-8pm Mon-Sat (Mobile Pay)" ,   "0.25 Hour Max $1.25 HR 10:00am-08:00pm Mon-Sat (Mobile Pay)"   )
#10                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "15 Min Max $1.25 HR 10am-8pm Mon-Sat (Mobile Pay), 3 Min Passenger Loading"    ,   "0.25 Hour Max $1.25 HR 10:00am-08:00pm Mon-Sat (Mobile Pay), 3 Min Passenger Loading"  )
#11                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "15 Min Max $1.25 HR 8am-6pm M-Sat" ,   "0.25 Hour Max $1.25 HR 08:00am-06:00pm Mon-Sat"    )
#12                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "15 Min Max $1.25 HR 8am-6pm Mon-Sat"   ,   "0.25 Hour Max $1.25 HR 08:00am-06:00pm Mon-Sat"    )
#13                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "15 Min Max $1.25 HR 8am-6pm Mon-Sat  (Mobile Pay)" ,   "0.25 Hour Max $1.25 HR 08:00am-06:00pm Mon-Sat  (Mobile Pay)"  )
#14                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "15 Min Max $1.25 HR 8am-6pm Mon-Sat (Mobile Pay)"  ,   "0.25 Hour Max $1.25 HR 08:00am-06:00pm Mon-Sat (Mobile Pay)"   )
#15                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "2 Hour Max $1.25 HR 10am-5pm Mon-Sat (Mobile Pay)" ,   "2.00 Hour Max $1.25 HR 10:00am-05:00pm Mon-Sat (Mobile Pay)"   )
#16                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "2 Hour Max $1.25 HR 10am-8pm Mon-Sat"  ,   "2.00 Hour Max $1.25 HR 10:00am-08:00pm Mon-Sat"    )
#17                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "2 Hour Max $1.25 HR 10am-8pm Mon-Sat (Mobile Pay)" ,   "2.00 Hour Max $1.25 HR 10:00am-08:00pm Mon-Sat (Mobile Pay)"   )
#18                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "2 Hour Max $1.25 HR 10am-8pm Mon-Sat (Mobile Pay), 3 Min Passenger Loading"    ,   "2.00 Hour Max $1.25 HR 10:00am-08:00pm Mon-Sat (Mobile Pay), 3 Min Passenger Loading"  )
#19                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "2 Hour Max $1.25 HR 8:30am-6:30pm Mon-Sat  (Mobile Pay)"   ,   "2.00 Hour Max $1.25 HR 08:30am-06:30pm Mon-Sat  (Mobile Pay)"  )
#20                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "2 Hour Max $1.25 HR 8am-4pm Mon-Fri 8am-6pm Sat"   ,   "2.00 Hour Max $1.25 HR 08:00am-04:00pm Mon-Fri 08:00am-06:00pm Sat"    )
#21                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "2 Hour Max $1.25 HR 8am-4pm Mon-Sat"   ,   "2.00 Hour Max $1.25 HR 08:00am-04:00pm Mon-Sat"    )
#22                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "2 Hour Max $1.25 HR 8am-4pm Mon-Sat  (Mobile Pay) "    ,   "2.00 Hour Max $1.25 HR 08:00am-04:00pm Mon-Sat  (Mobile Pay) " )
#23                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "2 Hour Max $1.25 HR 8am-5pm Mon-Sat, No Parking 5pm-2am, 3 Min Passenger Loading"  ,   "2.00 Hour Max $1.25 HR 08:00am-05:00pm Mon-Sat, No Parking 5pm-2am, 3 Min Passenger Loading"   )
#24                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "2 Hour Max $1.25 HR 8am-6pm Mon-Sat"   ,   "2.00 Hour Max $1.25 HR 08:00am-06:00pm Mon-Sat"    )
#25                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "2 Hour Max $1.25 HR 8am-6pm Mon-Sat      (Mobile Pay) "    ,   "2.00 Hour Max $1.25 HR 08:00am-06:00pm Mon-Sat      (Mobile Pay) " )
#26                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "2 Hour Max $1.25 HR 8am-6pm Mon-Sat     (Mobile Pay) " ,   "2.00 Hour Max $1.25 HR 08:00am-06:00pm Mon-Sat     (Mobile Pay) "  )
#27                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "2 Hour Max $1.25 HR 8am-6pm Mon-Sat    (Mobile Pay) "  ,   "2.00 Hour Max $1.25 HR 08:00am-06:00pm Mon-Sat    (Mobile Pay) "   )
#28                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "2 Hour Max $1.25 HR 8am-6pm Mon-Sat (Mobile Pay)"  ,   "2.00 Hour Max $1.25 HR 08:00am-06:00pm Mon-Sat (Mobile Pay)"   )
#29                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "2 Hour Max $1.25 HR 8am-6pm Mon-Sat, No Parking 6pm-2am, 3 Min Passenger Loading  (Mobile Pay)"    ,   "2.00 Hour Max $1.25 HR 08:00am-06:00pm Mon-Sat, No Parking 6pm-2am, 3 Min Passenger Loading  (Mobile Pay)" )
#30                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "2 Hour Max $1.25 HR 9am-6pm Mon-Fri 8am-6pm Sat"   ,   "2.00 Hour Max $1.25 HR 09:00am-06:00pm Mon-Fri 08:00am-06:00pm Sat"    )
#31                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "30 Min Max $1.25 HR 10am-8pm Mon-Sat  (Mobile Pay)"    ,   "0.50 Hour Max $1.25 HR 10:00am-08:00pm Mon-Sat  (Mobile Pay)"  )
#32                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "30 Min Max $1.25 HR 10am-8pm Mon-Sat (Mobile Pay)" ,   "0.50 Hour Max $1.25 HR 10:00am-08:00pm Mon-Sat (Mobile Pay)"   )
#33                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "30 Min Max $1.25 HR 10am-8pm Mon-Sat (Mobile Pay), 3 Min Passenger Loading"    ,   "0.50 Hour Max $1.25 HR 10:00am-08:00pm Mon-Sat (Mobile Pay), 3 Min Passenger Loading"  )
#34                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "30 Min Max $1.25 HR 8am-5pm Mon-Sat, No Parking 5pm-2am, 3 Min Passenger Loading"  ,   "0.50 Hour Max $1.25 HR 08:00am-05:00pm Mon-Sat, No Parking 5pm-2am, 3 Min Passenger Loading"   )
#35                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "30 Min Max $1.25 HR 8am-6pm Mon-Sat"   ,   "0.50 Hour Max $1.25 HR 08:00am-06:00pm Mon-Sat"    )
#36                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "30 Min Max $1.25 HR 8am-6pm Mon-Sat  (Mobile Pay) "    ,   "0.50 Hour Max $1.25 HR 08:00am-06:00pm Mon-Sat  (Mobile Pay) " )
#37                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "30 Min Max $1.25 HR 8am-6pm Mon-Sat (Mobile Pay)"  ,   "0.50 Hour Max $1.25 HR 08:00am-06:00pm Mon-Sat (Mobile Pay)"   )
#38                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "30 Min Max $1.25 HR 8am-8pm Mon-Sat (Mobile Pay)"  ,   "0.50 Hour Max $1.25 HR 08:00am-08:00pm Mon-Sat (Mobile Pay)"   )
#39                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "33333---2 Hour Max $1.25 HR 8am-4pm Mon-Fri 4pm-6pm \"TOW AWAY\" 8am-6pm Sat"  ,   "2.00 Hour Max $1.25 HR 08:00am-04:00pm Mon-Fri 4pm-6pm \"TOW AWAY\" 08:00am-06:00pm Sat"   )
#40                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "4 Hour Max $1.00 HR 8am-6pm Mon-Sat"   ,   "4.00 Hour Max $1.00 HR 08:00am-06:00pm Mon-Sat"    )
#41                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "4 Hour Max $1.00 HR 8am-6pm Mon-Sat (Mobile Pay)"  ,   "4.00 Hour Max $1.00 HR 08:00am-06:00pm Mon-Sat (Mobile Pay)"   )
#42                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "4 Hour Max $1.00 HR 8am-6pm Mon-Sat (Mobile Pay) " ,   "4.00 Hour Max $1.00 HR 08:00am-06:00pm Mon-Sat (Mobile Pay) "  )
#43                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "9 Hour Max $0.50 HR 8am-6pm M-Sat" ,   "9.00 Hour Max $0.50 HR 08:00am-06:00pm Mon-Sat"    )
#44                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "9 Hour Max $0.50 HR 8am-6pm M-Sat  (Mobile Pay)"   ,   "9.00 Hour Max $0.50 HR 08:00am-06:00pm Mon-Sat  (Mobile Pay)"  )
#45                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "9 Hour Max $0.50 HR 8am-6pm Mon-Sat"   ,   "9.00 Hour Max $0.50 HR 08:00am-06:00pm Mon-Sat"    )
#46                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "9 Hour Max $0.50 HR 8am-6pm Mon-Sat  (Mobile Pay) "    ,   "9.00 Hour Max $0.50 HR 08:00am-06:00pm Mon-Sat  (Mobile Pay) " )
#47                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "9 Hour Max $0.50 HR 8am-6pm Mon-Sat (Mobile Pay) " ,   "9.00 Hour Max $0.50 HR 08:00am-06:00pm Mon-Sat (Mobile Pay) "  )
#48                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "9 Hour Max $0.50 HR 8am-6pm Mon-Sat."  ,   "9.00 Hour Max $0.50 HR 08:00am-06:00pm Mon-Sat."   )
#49                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "9 Hour Max $0.50 HR 8am-6pm Mon-Sat. (Mobile Pay) "   ,   "9.00 Hour Max $0.50 HR 08:00am-06:00pm Mon-Sat. (Mobile Pay) "    )
#50                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "9 Hour Max $0.75 HR 8am-6pm Mon-Sat"   ,   "9.00 Hour Max $0.75 HR 08:00am-06:00pm Mon-Sat"    )
#51                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "9 Hour Max $0.75 HR 8am-6pm Mon-Sat  (Mobile Pay) "    ,   "9.00 Hour Max $0.75 HR 08:00am-06:00pm Mon-Sat  (Mobile Pay) " )
#52                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "9 Hour Max $0.75 HR 8am-6pm Mon-Sat (Mobile Pay)"  ,   "9.00 Hour Max $0.75 HR 08:00am-06:00pm Mon-Sat (Mobile Pay)"   )
#53                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "MSPM 1 Hour Max $1.25 HR 10am-8pm Mon-Sat" ,   "1.00 Hour Max $1.25 HR 10:00am-08:00pm Mon-Sat"    )
#54                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "MSPM 1 Hour Max $1.25 HR 9am-6pm Mon-Sat"  ,   "1.00 Hour Max $1.25 HR 09:00am-06:00pm Mon-Sat"    )
#55                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "MSPM 2 Hour Max $1.25 HR 10am-8pm Mon-Sat" ,   "2.00 Hour Max $1.25 HR 10:00am-08:00pm Mon-Sat"    )
#56                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "MSPM 2 Hour Max $1.25 HR 8am-6pm Mon-Fri"  ,   "2.00 Hour Max $1.25 HR 08:00am-06:00pm Mon-Fri"    )
#57                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "MSPM 2 Hour Max $1.25 HR 8am-6pm Mon-Sat"  ,   "2.00 Hour Max $1.25 HR 08:00am-06:00pm Mon-Sat"    )
#58                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "MSPM 4 Hour Max $1.00 HR 10am-8pm Mon-Sat" ,   "4.00 Hour Max $1.00 HR 10:00am-08:00pm Mon-Sat"    )
#59                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "MSPM 4 Hour Max $1.00 HR 8:30am-6pm Mon-Fri/Sat 8am-6pm"   ,   "4.00 Hour Max $1.00 HR 08:30am-06:00pm Mon-Fri/Sat 08:00am-06:00pm"    )
#60                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "MSPM 4 Hour Max $1.00 HR 8am-6pm Mon-Sat"  ,   "4.00 Hour Max $1.00 HR 08:00am-06:00pm Mon-Sat"    )
#61                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "MSPM 4 Hour Max $1.25 HR 10am-8pm Mon-Sat" ,   "4.00 Hour Max $1.25 HR 10:00am-08:00pm Mon-Sat"    )
#62                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "MSPM 4 Hour Max $1.25 HR 8am-6pm Mon-Sat"  ,   "4.00 Hour Max $1.25 HR 08:00am-06:00pm Mon-Sat"    )
#63                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "MSPM 9 Hour Max $0.50 HR 8:30am-6pm Mon-Sat"   ,   "9.00 Hour Max $0.50 HR 08:30am-06:00pm Mon-Sat"    )
#64                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "MSPM 9 Hour Max $0.50 HR 8am-6pm Mon-Sat"  ,   "9.00 Hour Max $0.50 HR 08:00am-06:00pm Mon-Sat"    )
#65                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "MSPM 9 Hour Max $0.75 HR 10am-8pm Mon-Sat" ,   "9.00 Hour Max $0.75 HR 10:00am-08:00pm Mon-Sat"    )
#66                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "MSPM 9 Hour Max $0.75 HR 8am-6pm Mon-Sat"  ,   "9.00 Hour Max $0.75 HR 08:00am-06:00pm Mon-Sat"    )
#67                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "MSPM Ballpark Special Event"   ,   "Ballpark Special Event"    )
#68                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "Single Space 2 hour meters Petco Special Event (Mobile Pay)"   ,   "Single Space 2.00 Hour meters Petco Special Event (Mobile Pay)"    )
#69                 
dt$config_name_fixed=               
  replace(dt$config_name_fixed,             
          dt$config_name_fixed==    "Single Space 30 min meters Petco Special Event Parking (Mobile Pay)"   ,   "Single Space 0.50 Hour meters Petco Special Event Parking (Mobile Pay)"    )

#filter to SS only
dt <- dt %>%
  filter(dt$meter_type=="SS")
levels(dt$meter_type)

#filter dt without 67-69 because these parking meters are not ones that we need to study
dt <- dt %>%
  filter(dt$config_name!="MSPM Ballpark Special Event")
dt <- dt %>%
  filter(dt$config_name!="Single Space 2 hour meters Petco Special Event (Mobile Pay)")
dt <- dt %>%
  filter(dt$config_name!="Single Space 30 min meters Petco Special Event Parking (Mobile Pay)"  )
dt <- dt %>%
  filter(dt$config_name!="San Diego Default"  )

#adding a new variable called max_hours to know how long each meter is operating everyday
dt$max_hours <- as.factor(substring(dt$config_name_fixed, 1, 5))
levels(dt$max_hours)

#adding a new variable called rate, which make a new variable that we know the price for each meter
dt$rate <- as.factor(substr(dt$config_name_fixed, 16, 19))
levels(dt$rate)
#check the structure of data dt 
str(dt)

#adding a new variable called new_duration (in hours), and this is another duration we use with different way of calculation. This one is more accurate than the old one
dt$new_duration=(as.numeric(dt$trans_amt)/100)/as.numeric(as.character(dt$rate))

#adding a new variable called timerange to see how long each meter is running everyday
dt$timerange <- as.factor(substring(dt$config_name_fixed, 24, 38))
levels(dt$timerange)

#creating variable called capacity_sec, make timerage into seconds which is creating a numeric variable to contribute future regression line
dt$capacity_sec=dt$timerange
levels(dt$capacity_sec)

#renaming capacity_sec into sec
dt$capacity_sec=as.character(dt$capacity_sec)
dt$capacity_sec=
  replace(dt$capacity_sec,
          dt$capacity_sec=="07:00am-07:00pm",
          "43200")
dt$capacity_sec=
  replace(dt$capacity_sec,
          dt$capacity_sec=="09:00am-04:00pm",
          "25200")
dt$capacity_sec=
  replace(dt$capacity_sec,
          dt$capacity_sec=="08:00am-04:00pm",
          "28800")

dt$capacity_sec=
  replace(dt$capacity_sec,
          dt$capacity_sec=="08:00am-05:00pm",
          "32400")

dt$capacity_sec=
  replace(dt$capacity_sec,
          dt$capacity_sec=="08:00am-06:00pm",
          "36000")

dt$capacity_sec=
  replace(dt$capacity_sec,
          dt$capacity_sec=="08:00am-08:00pm",
          "43200")

dt$capacity_sec=
  replace(dt$capacity_sec,
          dt$capacity_sec=="08:30am-06:00pm",
          "34200")

dt$capacity_sec=
  replace(dt$capacity_sec,
          dt$capacity_sec=="08:30am-06:30pm",
          "36000")

dt$capacity_sec=
  replace(dt$capacity_sec,
          dt$capacity_sec=="09:00am-06:00pm",
          "32400")

dt$capacity_sec=
  replace(dt$capacity_sec,
          dt$capacity_sec=="10:00am-05:00pm",
          "25200")

dt$capacity_sec=
  replace(dt$capacity_sec,
          dt$capacity_sec=="10:00am-08:00pm",
          "36000")



dt$capacity_sec=as.factor(dt$capacity_sec)

levels(dt$capacity_sec)

#creating new variable called capacity_hr (hour)
dt$capacity_sec=as.numeric(as.character(dt$capacity_sec))
dt$capacity_hr<-dt$capacity_sec/3600
range(dt$capacity_sec)
dt1=dt

#deleting all duration = 0
str(dt$new_duration)
dt <- subset(dt, new_duration!= 0)

#creating new variable called occupancy_rate, which is a new variable showing how many percent of usage of each meter daily
dt$occupancy_rate_hr=(dt$new_duration/dt$capacity_hr)

#filter out january due to data collecting error
dt1=dt
str(dt$date)
dt$date=format(as.POSIXct(dt$trans_start,"%Y-%M-%D %H:%M:%S") ,format = "%Y:%m:%d")
dt<- subset(dt, date > "2018:01:31")

#creating trans_amt_dll to know how much people paid for each transaction
dt$trans_amt_dll<- dt$trans_amt/100
range(dt$trans_amt_dll)

#creating new variable called weekend to see the relationship between weekend/weekday and other independent variables
dt$weekend <- dt$week
str(dt$weekend)
dt$weekend=             
  replace(dt$weekend,               
          dt$weekend == "Sunday"    ,   "Weekend")

dt$weekend=             
  replace(dt$weekend,   
          dt$weekend==  "Monday"    ,   "Weekday")

dt$weekend=             
  replace(dt$weekend,
          dt$weekend==  "Tuesday"   ,   "Weekday")

dt$weekend=             
  replace(dt$weekend,
          dt$weekend==  "Wednesday" ,   "Weekday")

dt$weekend=             
  replace(dt$weekend,
          dt$weekend==  "Thursday"  ,   "Weekday")

dt$weekend=             
  replace(dt$weekend,
          dt$weekend==  "Friday"    ,   "Weekday")

dt$weekend=             
  replace(dt$weekend,
          dt$weekend==  "Saturday"  ,   "Weekend")
levels(as.factor(dt$weekend))

#creating new variable called annual_capacity to make capacity yearly based
dt$annual_capacity<- dt$capacity_hr*272

#creating new dataset called annual_or to make occupancy rate yearly based
dt$annual_or<- dt$new_duration/dt$annual_capacity

#creating new variable called annual_aor to add all year occupancy rate for each parking meter
annual_aor <- aggregate(annual_or~pole_id, data=dt, FUN=sum)
dt<- merge(dt, annual_aor, by = c('pole_id') ,  all.x = T)
colnames(dt)
names(dt)[31]<-"annual_or"
names(dt)[32]<-"annual_aor"

#running for scatterplot matrix for data visualization
scatterplot_data=data.frame()

library(psych)
pairs.panels(scatterplot_data,method = "pearson",
             hits.col="#00AFBB",
             density=TRUE,
             ellipses = TRUE)
dt1=dt

#subset dt undefor annual_aor<1 since due to the data error, we have to avoid any annual occupancy rate bigger than 1
range(dt$annual_aor)
dt <- subset(dt, annual_aor < 1) 

# Annual_aor EDA to find the lowest skewness variable which is normally distributed to fit the regression

skewness(1/sqrt(dt$annual_aor)) 
skewness(sqrt(dt$annual_aor)) 
skewness(log(dt$annual_aor)) 
skewness(dt$annual_aor) 

hist(1/sqrt(dt$annual_aor)) 
hist(sqrt(dt$annual_aor)) 
hist(log(dt$annual_aor)) 
hist(dt$annual_aor) 

skewness(1/sqrt(dt$max_hours)) 
skewness(sqrt(dt$max_hours)) 
skewness(log(dt$max_hours)) 
skewness(dt$max_hours) 

#creating new variable called log_max_hours since log has the lowest skewness
dt$log_max_hours=log(dt$max_hours)

#changing variable type in regression 
str(dt$rate)
range(dt$rate)
dt$rate=as.numeric(as.character(dt$rate))
str(dt$weekend)
dt$weekend=as.factor(dt$weekend)
str(dt$area)
str(dt$annual_aor)
str(dt$max_hours)
dt$max_hours=as.numeric(as.character(dt$max_hours))
range(dt$max_hours)

#creating trainning and testing dataset
set.seed(9999) 
split <- sample(2, nrow(dt), replace = TRUE, prob = c(0.75,0.25)) 
train <- dt[split==1,] 
test <- dt[split==2,] 

#set training control
set.seed(9999) 
train.control <- trainControl(method = "cv", number = 5) 

#creating model using cross validation
model <- train(rate~area+log_max_hours+weekend+annual_aor, data = train, method = "lm", trControl = train.control) 
modelfull <- lm(rate~area+log_max_hours+weekend+annual_aor, data = dt1) 
summary(model) 
summary(modelfull) 
confint(modelfull) 
p=predict(model, test) 
predict(model,test, interval = 'confidence') 
summary(p) 

#running another model with aggregated area
levels(dt1$sub_area)
dt1=subset(dt1,zone=="Downtown")
subarea_aor <- aggregate(annual_or~sub_area, data=dt1, FUN=sum)
dt1<- merge(dt1, subarea_aor, by = c('sub_area') ,  all.x = T)
colnames(dt1)
names(dt1)[31]<-"annual_or"
names(dt1)[35]<-"subarea_aor"

#creating trainning and testing dataset
set.seed(9999) 
second_split <- sample(2, nrow(dt1), replace = TRUE, prob = c(0.75,0.25)) 
second_train <- dt[split==1,] 
second_test <- dt[split==2,] 

#set training control
set.seed(9999) 

train.control <- trainControl(method = "cv", number = 5) 

#creating second_model using cross validation
second_model <- train(rate~area+log_max_hours+weekend+area_aor, data = second_train, method = "lm", trControl = train.control) 
range(dt1$area_aor)
range(dt1$subarea_aor)
modelfull <- lm(rate~area+log_max_hours+weekend+subarea_aor, data = dt1) 
summary(model) 
summary(modelfull) 
confint(modelfull) 
p=predict(model, test) 
predict(model,test, interval = 'confidence') 
summary(p) 


#Generate two models for comparision
library(MASS) 
modela <- lm(rate~area+log_max_hours+weekend+annual_aor, data = train)
modelb <- lm(rate~area+annual_aor, data = train)
#renaming for consistency
model1 <- modela
model2 <- modelb

#Model summaries
summary(model1)
summary(model2)
#Variance Analysis
anova(modela, modelb)
AIC(modela, modelb)
