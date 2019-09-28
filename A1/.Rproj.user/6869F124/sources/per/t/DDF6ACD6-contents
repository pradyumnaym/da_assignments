#install.packages("psych") #removed because it causes error while knitting
library("psych")

# path to the Dataset
path='D:/DA/A1'

#set the current working directory to the 'path' 
setwd(path)

#read the csv files
generalData <- read.csv("general_data.csv")
employee_survey_data <- read.csv("employee_survey_data.csv")
manager_survey_data <- read.csv("manager_survey_data.csv")


#omit NA values(rows) in each of the dataframes
generalData <- na.omit(generalData)
employee_survey_data <- na.omit(employee_survey_data)
manager_survey_data <- na.omit(manager_survey_data)

#use the merge() function to merge the general data and employee data. call merge on the result, with manager data.
combined_data <- merge( merge(generalData,employee_survey_data, by = "EmployeeID"), manager_survey_data , by = "EmployeeID")

#draw the histogram, for people with age >=20, since that is the starting of our intervals.
#PLEASE SCROLL DOWN FOR THE DESCRIPTION OF THE PLOT
hist( combined_data$Age [combined_data$Age>=20],
      breaks = seq(20, 70, by = 10 ),
      right = F,
      col="pink",
      main = "Histogram of Ages",
      xlab = "Age",
      ylab = "Number of Employees"
)

#split the dataframe into a list of dataframes based on the given sequence.
#right = F is used to make the intervals [a,b) from (a,b]
split_data <- split(combined_data, cut(combined_data$Age, seq(20, 70, by = 10 ), right = F))

#lapply is similar to map of python, used to apply a function on a list.
averages <- lapply(split_data , function(a) mean(a$MonthlyIncome))

#the index which has the maximum Average in the list
index <- which(unlist(averages) == max(unlist(averages))) 

#print the interval with the highest avg income.
print(paste("The Interval number with the maximum average income is:",
            index*10 + 10,
            "-",
            index*10 + 19
)
)

#use this interval to analyse in the next 2 subquestions.
selected_interval <- split_data[[index]]


#Sort the dataframe selected_interval in decreasing order of the employees' heights, and choose all columns.
selected_interval <- selected_interval[order(selected_interval$MonthlyIncome , decreasing = T),]

#select the fifth highest salary, by using the unique function to remove duplicates.
fifth_highest_salary <- unique(selected_interval$MonthlyIncome)[5]

#choose all those people who have income>fifth_highest_salary, and choose the required columns.
highest_paid_people <- selected_interval[selected_interval$MonthlyIncome>=fifth_highest_salary,
                                         c("EmployeeID","MonthlyIncome","TotalWorkingYears")]
print("The Highest paid employees based on top 5 salaries are:")
#please refer to the cell above
highest_paid_people

#split the dataframe based on the Job roles, into a list of dataframes, one for each role.
job_roles <- split(selected_interval, selected_interval$JobRole)

#use lapply to find the average income corresponding to each dataframe,ie each jobRole.
average_income <- lapply(job_roles , function(a) mean(a$MonthlyIncome))

#get the labels for the barplot using another apply() call.
xlabels <- sapply(job_roles , function(a) a[1,]$JobRole)

#function to rotate the labels -- credits --> StackOverflow.
rotate_x <- function( labels_vec, rot_angle) {
  plt <- barplot(unlist(average_income),
                 main="Bar plot of average income of each job role",
                 xlab="Job Role",
                 ylab="Average Monthly Income",
                 las=2,
                 names.arg = c(""),
                 cex.axis=0.7,
  )
  text(plt, par("usr")[3], labels = labels_vec, srt = rot_angle, adj = c(1.1,1.1), xpd= TRUE, cex=0.6) 
}

#call the function to plot the graph.
rotate_x(xlabels,43)

#select only rows that correspond to females and select only the marital status and satisfaction level columns.
#table is used to aggregate(sum up/total) the dataframe rows.
required_data <- table(combined_data[combined_data$Gender == "Female",c("MaritalStatus","EnvironmentSatisfaction")])
print("The Summary of the number of females in each group is as follows:")
required_data
#plot the barplot with the below params.
barplot(as.matrix(required_data[,c(1,2,3,4)]) , 
        beside = T,
        col = c("#648a3a","pink","deepskyblue"),
        xlab = "Environment Satisfaction Level",
        ylab = "Number of Women",
        main = "Barplot of Environment Satisfaction vs frequency",
        ylim = c(0,300))

#draw a legend at position 1,265, which shows the satisfaction lavels corresponding to each color.
legend(1,265,
       title = "Marital Status",
       legend = c("Divorced","Married","Single"),
       fill =  c("#648a3a","pink","deepskyblue"),
       cex = 0.7
)

#select only the attrition and department columns, and all rows of the original dataset.
#table is used to aggregate the rows of the dataframe(sum up/total)
attrition_data <- table(combined_data[,c("Attrition","Department")])

#get the department names from the table.
departments<- colnames(attrition_data)

#number of yes's and no's for attrition.
#each of these is a vector which has the yes/no counts for each department.
nos <- attrition_data[1,]
yesses <- attrition_data[2,]
rates<- list()

#loop through the departments and find the attrition rates for each department.
for(i in 1:length(nos)){
  rates[i] <- yesses[i]/(yesses[i]+nos[i])
}

#print the department name and attrition details.
#min - find the minimum value. #which - used to find the index of an element in a vector.
print(
  paste("The department with the least attrition rate is:",
        departments[which(unlist(rates) == min(unlist(rates)))],
        "with attrition rate",
        round((min(unlist(rates)) *100),2),
        "percent"
  )
)

#get only the Monthly income and education field columns
education_data <- combined_data[,c("MonthlyIncome","EducationField")]

#split the DF based on field, into multiple dataframes
dflist = split(education_data[,1],education_data$EducationField)

#PLEASE SCROLL DOWN FOR THE DESCRIPTION OF THE PLOT
boxplot(dflist,
        main = "Boxplot of Income for Various Education Fields",
        xlab = "Education Field",
        ylab = "Monthly Income",
        cex.axis = 0.6
)

print("The summary statistics of the Monthly income for various education fields are")

#print the summary of all fields, by using summary() and tapply() 

#extract the unique field names.
fields <- unique(combined_data$EducationField)

#function to find mode in a vector of integers..
findmode <- function(x){
  maxnum <- max(x)
  index <- 1
  freq <- integer(max(x))
  res <- vector()
  for(i in 1:length(x)){
    freq[x[i]] <- freq[x[i]] +1
  }
  maxfreq <- max(freq)
  for(i in 1:maxnum){
    if(freq[i]==maxfreq){
      res[index]<- i
      index<-index+1
      #print(freq[])
    }
  }
  res
}

#print mode and IQR for all the fields
for(i in 1:length(fields)){
  y <- combined_data[combined_data$EducationField == fields[i],"MonthlyIncome"]
  print(
    paste(
      "Field: ",
      fields[i],
      ", IQR:",
      summary(y)[5]-summary(y)[2],
      ", Q1:",
      summary(y)[2],
      ", Q3:",
      summary(y)[5],
      ", Mean :",
      round(summary(y)[4]),
      ", Mode(s):",
      toString(findmode(y))
    )
  )
}

print(paste("The Frequencies of values in monthly incomes of Human Resources are: (see cell below). This is the reason for so many modes"))
#print the frequencies (Please refer to the cell above for info)
table(combined_data[combined_data$EducationField == "Human Resources","MonthlyIncome"])


#Split the original dataframe into 2 dataframes, each corresponding to a gender.
#select only the monthly income column, since we know that all rows in a DataFrame are of 1 gender.
gender_data <- split(combined_data[,c("MonthlyIncome")] , combined_data$Gender)

#compute the average income for females and males.
female_average <- mean(gender_data[[1]])
male_average <- mean(gender_data[[2]])

print(paste(
  "The average income for females is:",
  round(female_average,2),
  "and average income for males is: ",
  round(male_average,2)
))

print(paste(
  "The average monthly income for males is higher than that for females by:",
  round(male_average-female_average,2)
))

#Plot the boxplot of income.
boxplot(gender_data,
        main = "Boxplot of Monthly Incomes of Male and female employees",
        xlab = "Gender",
        ylab = "Income")

#get the rows from the original dataset, which have Medical as their field.
medical_data <- combined_data[combined_data$EducationField=="Medical",]

#sort the medical data frame based on the Monthly income column.
ordered_data <- medical_data[order(medical_data$MonthlyIncome),]

#calculate the required index. Here, I have used the formula :
#percentile = (p/100)*(n+1) . if percentile is integer, choose percentile^th element, if not average the integers 
#on either side of the number.
required_index <-  (0.95) * (nrow(ordered_data)+1)

#if percentile is integer, choose percentile^th element, if not average the integers 
#on either side of the number.

if(round(required_index)==required_index){
  #whole number, so take the element directly
  percentile <- ordered_data[required_index,]$MonthlyIncome
}else{
  # not whole number take average of numbers on either side..
  percentile <- (ordered_data[floor(required_index),]$MonthlyIncome + ordered_data[ceiling(required_index),]$MonthlyIncome)/2
}

print(
  paste(
    "The 95th percentile Monthly income in Medical field is:",
    percentile
  )
)

print(
  paste(
    "The 95th percentile Monthly income in Medical field using inbuilt quantile method is:",
    quantile(medical_data$MonthlyIncome,probs = c(0.95)),
    "Hence, verified.."
  )
)



#limiting Monthly income to simplify the histogram for explanation..
income_limited <- combined_data[combined_data$MonthlyIncome<=150000,]

#plot hist
hist(income_limited$MonthlyIncome,
     breaks = 8,
     main = "A Histogram of Monthly Incomes",
     xlab = "Monthly Income",
     ylab = "Frequency"
)

print(
  paste(
    "The skew is",
    round(describe(income_limited$MonthlyIncome)$skew,3)
  )
)
print(
  paste("The kurtosis is",
        round(describe(income_limited$MonthlyIncome)$kurtosis,3)
  )
)

