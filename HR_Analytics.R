##################################################################################################
#                                  HR ANALYTICS CASE STUDY                                       #
##################################################################################################

#Loading the required libraries
library("ggplot2")
library("MASS")
library("ROCR")

#loading the datasets
emp_survey_df <- read.csv("employee_survey_data.csv",header = T)
gen_data_df <- read.csv("general_data.csv",header = T)
manager_survey_df <- read.csv("manager_survey_data.csv",header = T)
in_time_df <- read.csv("in_time.csv",header = T)
out_time_df <- read.csv("out_time.csv",header = T)

#Assigning column name to in_time and out_time data
colnames(in_time_df) [1]  <- "EmployeeID"
colnames(out_time_df) [1] <- "EmployeeID"

#check for duplicates
sum(duplicated(gen_data_df$EmployeeID))       #0 duplicates found
sum(duplicated(emp_survey_df$EmployeeID))    #0 duplicates found
sum(duplicated(manager_survey_df$EmployeeID))     #0 duplicates found
sum(duplicated(in_time_df$EmployeeID))            #0 duplicates found
sum(duplicated(out_time_df$EmployeeID))           #0 duplicates found

#To calculate average working hours for employees
in_time_df<- sapply(in_time_df, function(x) as.POSIXlt(x, origin="1960-01-01","%Y-%m-%d %H:%M:%S"))
in_time_df<- as.data.frame(in_time_df)
out_time_df<- sapply(out_time_df, function(x) as.POSIXlt(x, origin="1960-01-01","%Y-%m-%d %H:%M:%S"))
out_time_df<- as.data.frame(out_time_df)

#Remove first columns from in and out time data
in_time_df<- in_time_df[,-1]
out_time_df<-out_time_df[,-1]

#Calculating the number of hours per day for employee
hours_per_day<- out_time_df-in_time_df

#Remove all the columns with NA throughout
hours_per_day<- hours_per_day[, colSums(is.na(hours_per_day)) != nrow(hours_per_day)]

#Converting values to numeric
hours_per_day<-sapply(hours_per_day,function(x) as.numeric(x))
hours_per_day<-as.data.frame(hours_per_day)

#Store Employee id in new vector
EmployeeID<-seq(from = 1, to = 4410, by = 1)

#Calculating mean for each row
hours_per_day$AvgWorkHours<-apply(hours_per_day,1,mean,na.rm=TRUE)

#Creating Average work hours per employeee data frame
AvgWorkHours<-cbind(EmployeeID,hours_per_day$AvgWorkHours)
AvgWorkHours<-as.data.frame(AvgWorkHours)

#setdiff to check all the data is for same employees
setdiff(emp_survey_df$EmployeeID,manager_survey_df$EmployeeID) 
setdiff(manager_survey_df$EmployeeID,gen_data_df$EmployeeID) 
setdiff(gen_data_df$EmployeeID,AvgWorkHours$EmployeeID) 
#setdiff function gives 0 which specifies all the data is for same employees

#Merging data at one place for analysis
employee_attrition<- merge(gen_data_df, AvgWorkHours, by="EmployeeID")
employee_attrition<-merge(employee_attrition, emp_survey_df, by="EmployeeID")
employee_attrition<-merge(employee_attrition, manager_survey_df, by="EmployeeID")
colnames(employee_attrition)[25]<- "AvgWorkHours"

#merging the datasets emp_survey and general_data
#merged_df <- merge(emp_survey_df,gen_data_df,all = T)

#merging the result of above merge and manager_survey
#final_df <- merge(merged_df,manager_survey_df,all = T)
#checking for NA values
sum(is.na(employee_attrition))


#removing columns with NA values
employee_attrition <- na.omit(employee_attrition)
str(employee_attrition)

#2. Data Preparation:

#Scaling the continuous variables for Regression analysis

employee_attrition$Age<- scale(employee_attrition$Age)
employee_attrition$DistanceFromHome<- scale(employee_attrition$DistanceFromHome)
employee_attrition$JobLevel<- scale(employee_attrition$JobLevel)
employee_attrition$MonthlyIncome<- scale(employee_attrition$MonthlyIncome)
employee_attrition$NumCompaniesWorked<- scale(employee_attrition$NumCompaniesWorked)
employee_attrition$PercentSalaryHike<- scale(employee_attrition$PercentSalaryHike)
employee_attrition$StockOptionLevel<- scale(employee_attrition$StockOptionLevel)
employee_attrition$TotalWorkingYears<- scale(employee_attrition$TotalWorkingYears)
employee_attrition$TrainingTimesLastYear<- scale(employee_attrition$TrainingTimesLastYear)
employee_attrition$YearsAtCompany<- scale(employee_attrition$YearsAtCompany)
employee_attrition$YearsSinceLastPromotion<- scale(employee_attrition$YearsSinceLastPromotion)
employee_attrition$YearsWithCurrManager<- scale(employee_attrition$YearsWithCurrManager)
employee_attrition$AvgWorkHours<- scale(employee_attrition$AvgWorkHours)

#creating dummy variable for Attrition 0-No and 1-Yes
levels(employee_attrition$Attrition) <- c(0,1)
employee_attrition$Attrition <- as.numeric(levels(employee_attrition$Attrition))[employee_attrition$Attrition]

#creating dummy variable for gender 0-male and 1-female
levels(employee_attrition$Gender) <- c(1,0)
employee_attrition$Gender <- as.numeric(levels(employee_attrition$Gender))[employee_attrition$Gender]

#creating dummy variables for EnvironmentSatisfaction
employee_attrition$EnvironmentSatisfaction <- as.factor(employee_attrition$EnvironmentSatisfaction)
dummy_1 <- data.frame(model.matrix(~EnvironmentSatisfaction,data = employee_attrition))
View(dummy_1)
dummy_1 <- dummy_1[,-1]
employee_attrition <- cbind(employee_attrition[,-26],dummy_1)

#creating dummy variable for JobSatisfaction
employee_attrition$JobSatisfaction <- as.factor(employee_attrition$JobSatisfaction)
dummy_2 <- data.frame(model.matrix(~JobSatisfaction,data = employee_attrition))
View(dummy_2)
dummy_2 <- dummy_2[,-1]
employee_attrition <- cbind(employee_attrition[,-26],dummy_2)


#creating dummy variable for WorkLifeBalance
employee_attrition$WorkLifeBalance <- as.factor(employee_attrition$WorkLifeBalance)
dummy_3 <- data.frame(model.matrix(~WorkLifeBalance,data = employee_attrition))
View(dummy_3)
dummy_3 <- dummy_3[,-1]
employee_attrition <- cbind(employee_attrition[,-26],dummy_3)

#creating dummy variable for BusinessTravel
dummy_4 <- data.frame(model.matrix(~BusinessTravel,data = employee_attrition))
View(dummy_4)
dummy_4 <- dummy_4[,-1]
employee_attrition <- cbind(employee_attrition[,-4],dummy_4)

#creating dummy variable for Department
dummy_5 <- data.frame(model.matrix(~Department,data = employee_attrition))
View(dummy_5)
dummy_5 <- dummy_5[,-1]
employee_attrition <- cbind(employee_attrition[,-4],dummy_5)

#creating a dummy variable for Education
employee_attrition$Education <- as.factor(employee_attrition$Education)
dummy_6 <- data.frame(model.matrix(~Education,data=employee_attrition))
View(dummy_6)
dummy_6 <- dummy_6[,-1]
employee_attrition <- cbind(employee_attrition[,-5],dummy_6)

#creating a dummy variable for EducationField
dummy_7 <- data.frame(model.matrix(~EducationField,data = employee_attrition))
View(dummy_7)
dummy_7 <- dummy_7[,-1]
employee_attrition <- cbind(employee_attrition[,-5],dummy_7)

#creating dummy variable for JobLevel
employee_attrition$JobLevel <- as.factor(employee_attrition$JobLevel)
dummy_8 <- data.frame(model.matrix(~JobLevel,data = employee_attrition))
View(dummy_8)
dummy_8 <- dummy_8[,-1]
employee_attrition <- cbind(employee_attrition[,-7],dummy_8)

#creating dummy variable for JobRole
dummy_9 <- data.frame(model.matrix(~JobRole,data = employee_attrition))
View(dummy_9)
dummy_9 <- dummy_9[,-1]
employee_attrition <- cbind(employee_attrition[,-7],dummy_9)

#creating dummy variable for Over18
employee_attrition$Over18 <- ifelse(employee_attrition$Over18=='Y',1,0)
employee_attrition$Over18 <- as.numeric(employee_attrition$Over18)

#creating the dummy variable MaritalStatus
dummy_10 <- data.frame(model.matrix(~MaritalStatus,data = employee_attrition))
View(dummy_10)
dummy_10 <- dummy_10[,-1]
employee_attrition <- cbind(employee_attrition[,-7],dummy_10)

#creating the dummy variable for JobInvolvement
employee_attrition$JobInvolvement <- as.factor(employee_attrition$JobInvolvement)
dummy_11 <- data.matrix(model.matrix(~JobInvolvement,data = employee_attrition))
View(dummy_11)
dummy_11 <- dummy_11[,-1]
employee_attrition <- cbind(employee_attrition[,-19],dummy_11)

#setting the seed
set.seed(100)

#creating the test and train dataset
train <- sample(1:nrow(employee_attrition),0.7*nrow(employee_attrition))
train_df <- employee_attrition[train,]
test_df <- employee_attrition[-train,]

#creating the first model
model_1 <- glm(Attrition~.,data = train_df,family = "binomial")
summary(model_1)

#removing employeeID
model_2 <- glm(Attrition~ Age + DistanceFromHome + EmployeeCount + Gender + MonthlyIncome + NumCompaniesWorked +
               Over18 + PercentSalaryHike + StandardHours + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
               YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + PerformanceRating + EnvironmentSatisfaction2 +
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
               WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                 DepartmentSales + Education2 + Education3 + Education4 + Education5 + EducationFieldLife.Sciences +
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                 JobLevel2 + JobLevel3 + JobLevel4 + JobLevel5 + JobRoleHuman.Resources + JobRoleLaboratory.Technician +
                 JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + JobRoleResearch.Scientist +
                 JobRoleSales.Executive + JobRoleSales.Representative + MaritalStatusMarried + MaritalStatusSingle +
                 JobInvolvement3 + JobInvolvement4 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_2)

#removing EmployeeCount

model_3 <- glm(Attrition~ Age + DistanceFromHome + Gender + MonthlyIncome + NumCompaniesWorked +
                 Over18 + PercentSalaryHike + StandardHours + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + PerformanceRating + EnvironmentSatisfaction2 +
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                 WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                 DepartmentSales + Education2 + Education3 + Education4 + Education5 + EducationFieldLife.Sciences +
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                 JobLevel2 + JobLevel3 + JobLevel4 + JobLevel5 + JobRoleHuman.Resources + JobRoleLaboratory.Technician +
                 JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + JobRoleResearch.Scientist +
                 JobRoleSales.Executive + JobRoleSales.Representative + MaritalStatusMarried + MaritalStatusSingle +
                 JobInvolvement3 + JobInvolvement4 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_3)

#removing Over18
model_4 <- glm(Attrition~ Age + DistanceFromHome + Gender + MonthlyIncome + NumCompaniesWorked +
                 PercentSalaryHike + StandardHours + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + PerformanceRating + EnvironmentSatisfaction2 +
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                 WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                 DepartmentSales + Education2 + Education3 + Education4 + Education5 + EducationFieldLife.Sciences +
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                 JobLevel2 + JobLevel3 + JobLevel4 + JobLevel5 + JobRoleHuman.Resources + JobRoleLaboratory.Technician +
                 JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + JobRoleResearch.Scientist +
                 JobRoleSales.Executive + JobRoleSales.Representative + MaritalStatusMarried + MaritalStatusSingle +
                 JobInvolvement3 + JobInvolvement4 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_4)

#removing StandardHours
model_5 <- glm(Attrition~ Age + DistanceFromHome + Gender + MonthlyIncome + NumCompaniesWorked +
                 PercentSalaryHike + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + PerformanceRating + EnvironmentSatisfaction2 +
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                 WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                 DepartmentSales + Education2 + Education3 + Education4 + Education5 + EducationFieldLife.Sciences +
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                 JobLevel2 + JobLevel3 + JobLevel4 + JobLevel5 + JobRoleHuman.Resources + JobRoleLaboratory.Technician +
                 JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + JobRoleResearch.Scientist +
                 JobRoleSales.Executive + JobRoleSales.Representative + MaritalStatusMarried + MaritalStatusSingle +
                 JobInvolvement3 + JobInvolvement4 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_5)


#removing DepartmentSales
model_6 <- glm(Attrition~ Age + DistanceFromHome + Gender + MonthlyIncome + NumCompaniesWorked +
                 PercentSalaryHike + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + PerformanceRating + EnvironmentSatisfaction2 +
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                 WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                 Education2 + Education3 + Education4 + Education5 + EducationFieldLife.Sciences +
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                 JobRoleHuman.Resources + JobRoleLaboratory.Technician + JobLevel2 + JobLevel3 + JobLevel4 + JobLevel5 +
                 JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + JobRoleResearch.Scientist +
                 JobRoleSales.Executive + JobRoleSales.Representative + MaritalStatusMarried + MaritalStatusSingle +
                 JobInvolvement3 + JobInvolvement4 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_6)

#removing JobRoleHuman.Resource
model_7 <- glm(Attrition~ Age + DistanceFromHome + Gender + MonthlyIncome + NumCompaniesWorked +
                 PercentSalaryHike + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + PerformanceRating + EnvironmentSatisfaction2 +
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                 WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                 Education2 + Education3 + Education4 + Education5 + EducationFieldLife.Sciences +
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                 JobRoleLaboratory.Technician + JobLevel2 + JobLevel3 + JobLevel4 + JobLevel5 +
                 JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + JobRoleResearch.Scientist +
                 JobRoleSales.Executive + JobRoleSales.Representative + MaritalStatusMarried + MaritalStatusSingle +
                 JobInvolvement3 + JobInvolvement4 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_7)

#removing JobLevel4
model_8 <- glm(Attrition~ Age + DistanceFromHome + Gender + MonthlyIncome + NumCompaniesWorked +
                 PercentSalaryHike + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + PerformanceRating + EnvironmentSatisfaction2 +
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                 WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                 Education2 + Education3 + Education4 + Education5 + EducationFieldLife.Sciences +
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                 JobRoleLaboratory.Technician + JobLevel2 + JobLevel3 + JobLevel5 +
                 JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + JobRoleResearch.Scientist +
                 JobRoleSales.Executive + JobRoleSales.Representative + MaritalStatusMarried + MaritalStatusSingle +
                 JobInvolvement3 + JobInvolvement4 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_8)

#removing JobLevel3
model_9 <- glm(Attrition~ Age + DistanceFromHome + Gender + MonthlyIncome + NumCompaniesWorked +
                 PercentSalaryHike + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + PerformanceRating + EnvironmentSatisfaction2 +
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                 WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                 Education2 + Education3 + Education4 + Education5 + EducationFieldLife.Sciences +
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                 JobRoleLaboratory.Technician + JobLevel2 + JobLevel5 +
                 JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + JobRoleResearch.Scientist +
                 JobRoleSales.Executive + JobRoleSales.Representative + MaritalStatusMarried + MaritalStatusSingle +
                 JobInvolvement3 + JobInvolvement4 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_9)


#removing Gender
model_10 <- glm(Attrition~ Age + DistanceFromHome + MonthlyIncome + NumCompaniesWorked +
                  PercentSalaryHike + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                  YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + PerformanceRating + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                  Education2 + Education3 + Education4 + Education5 + EducationFieldLife.Sciences +
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                  JobRoleLaboratory.Technician + JobLevel2 + JobLevel5 +
                  JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + JobRoleResearch.Scientist +
                  JobRoleSales.Executive + JobRoleSales.Representative + MaritalStatusMarried + MaritalStatusSingle +
                  JobInvolvement3 + JobInvolvement4 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_10)

#removing JobInvolvement4
model_11 <- glm(Attrition~ Age + DistanceFromHome + MonthlyIncome + NumCompaniesWorked +
                  PercentSalaryHike + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                  YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + PerformanceRating + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                  Education2 + Education3 + Education4 + Education5 + EducationFieldLife.Sciences +
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                  JobRoleLaboratory.Technician + JobLevel2 + JobLevel5 +
                  JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + JobRoleResearch.Scientist +
                  JobRoleSales.Executive + JobRoleSales.Representative + MaritalStatusMarried + MaritalStatusSingle +
                  JobInvolvement3 + AvgWorkHours,data = train_df,family = "binomial")


summary(model_11)

#removing JobRoleManager
model_12 <- glm(Attrition~ Age + DistanceFromHome + MonthlyIncome + NumCompaniesWorked +
                  PercentSalaryHike + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                  YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + PerformanceRating + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                  Education2 + Education3 + Education4 + Education5 + EducationFieldLife.Sciences +
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                  JobRoleLaboratory.Technician + JobLevel2 + JobLevel5 +
                  JobRoleManufacturing.Director + JobRoleResearch.Director + JobRoleResearch.Scientist +
                  JobRoleSales.Executive + JobRoleSales.Representative + MaritalStatusMarried + MaritalStatusSingle +
                  JobInvolvement3 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_12)


#removing PerformanceRating
model_13 <- glm(Attrition~ Age + DistanceFromHome + MonthlyIncome + NumCompaniesWorked +
                  PercentSalaryHike + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                  YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                  Education2 + Education3 + Education4 + Education5 + EducationFieldLife.Sciences +
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                  JobRoleLaboratory.Technician + JobLevel2 + JobLevel5 +
                  JobRoleManufacturing.Director + JobRoleResearch.Director + JobRoleResearch.Scientist +
                  JobRoleSales.Executive + JobRoleSales.Representative + MaritalStatusMarried + MaritalStatusSingle +
                  JobInvolvement3 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_13)


#removing Distance from home
model_14 <- glm(Attrition~ Age + MonthlyIncome + NumCompaniesWorked +
                  PercentSalaryHike + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                  YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                  Education2 + Education3 + Education4 + Education5 + EducationFieldLife.Sciences +
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                  JobRoleLaboratory.Technician + JobLevel2 + JobLevel5 +
                  JobRoleManufacturing.Director + JobRoleResearch.Director + JobRoleResearch.Scientist +
                  JobRoleSales.Executive + JobRoleSales.Representative + MaritalStatusMarried + MaritalStatusSingle +
                  JobInvolvement3 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_14)


#removing Education3
model_15 <- glm(Attrition~ Age + MonthlyIncome + NumCompaniesWorked +
                  PercentSalaryHike + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                  YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                  Education2 + Education4 + Education5 + EducationFieldLife.Sciences +
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                  JobRoleLaboratory.Technician + JobLevel2 + JobLevel5 +
                  JobRoleManufacturing.Director + JobRoleResearch.Director + JobRoleResearch.Scientist +
                  JobRoleSales.Executive + JobRoleSales.Representative + MaritalStatusMarried + MaritalStatusSingle +
                  JobInvolvement3 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_15)


#removing PerformanceRating
model_16 <- glm(Attrition~ Age + MonthlyIncome + NumCompaniesWorked +
                  PercentSalaryHike + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                  YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                  Education2 + Education5 + EducationFieldLife.Sciences +
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                  JobRoleLaboratory.Technician + JobLevel2 + JobLevel5 +
                  JobRoleManufacturing.Director + JobRoleResearch.Director + JobRoleResearch.Scientist +
                  JobRoleSales.Executive + JobRoleSales.Representative + MaritalStatusMarried + MaritalStatusSingle +
                  JobInvolvement3 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_16)


#removing JobRoleSales.Representative
model_17 <- glm(Attrition~ Age + MonthlyIncome + NumCompaniesWorked +
                  PercentSalaryHike + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                  YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                  Education2 + Education5 + EducationFieldLife.Sciences +
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                  JobRoleLaboratory.Technician + JobLevel2 + JobLevel5 +
                  JobRoleManufacturing.Director + JobRoleResearch.Director + JobRoleResearch.Scientist +
                  JobRoleSales.Executive + MaritalStatusMarried + MaritalStatusSingle +
                  JobInvolvement3 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_17)


#removing StockOptionLevel
model_18 <- glm(Attrition~ Age + MonthlyIncome + NumCompaniesWorked +
                  PercentSalaryHike + TotalWorkingYears + TrainingTimesLastYear +
                  YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                  Education2 + Education5 + EducationFieldLife.Sciences +
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                  JobRoleLaboratory.Technician + JobLevel2 + JobLevel5 +
                  JobRoleManufacturing.Director + JobRoleResearch.Director + JobRoleResearch.Scientist +
                  JobRoleSales.Executive + MaritalStatusMarried + MaritalStatusSingle +
                  JobInvolvement3 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_18)

#removing MonthlyIncome
model_19 <- glm(Attrition~ Age + NumCompaniesWorked +
                  PercentSalaryHike + TotalWorkingYears + TrainingTimesLastYear +
                  YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                  Education2 + Education5 + EducationFieldLife.Sciences +
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                  JobRoleLaboratory.Technician + JobLevel2 + JobLevel5 +
                  JobRoleManufacturing.Director + JobRoleResearch.Director + JobRoleResearch.Scientist +
                  JobRoleSales.Executive + MaritalStatusMarried + MaritalStatusSingle +
                  JobInvolvement3 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_19)


#removing Education5
model_20 <- glm(Attrition~ Age + NumCompaniesWorked +
                  PercentSalaryHike + TotalWorkingYears + TrainingTimesLastYear +
                  YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                  Education2 + EducationFieldLife.Sciences +
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                  JobRoleLaboratory.Technician + JobLevel2 + JobLevel5 +
                  JobRoleManufacturing.Director + JobRoleResearch.Director + JobRoleResearch.Scientist +
                  JobRoleSales.Executive + MaritalStatusMarried + MaritalStatusSingle +
                  JobInvolvement3 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_20)


#removing YearsAtCompany
model_21 <- glm(Attrition~ Age + NumCompaniesWorked +
                  PercentSalaryHike + TotalWorkingYears + TrainingTimesLastYear +
                  YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                  Education2 + EducationFieldLife.Sciences +
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                  JobRoleLaboratory.Technician + JobLevel2 + JobLevel5 +
                  JobRoleManufacturing.Director + JobRoleResearch.Director + JobRoleResearch.Scientist +
                  JobRoleSales.Executive + MaritalStatusMarried + MaritalStatusSingle +
                  JobInvolvement3 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_21)

#removing Education2
model_22 <- glm(Attrition~ Age + NumCompaniesWorked +
                  PercentSalaryHike + TotalWorkingYears + TrainingTimesLastYear +
                  YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                  EducationFieldLife.Sciences +
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                  JobRoleLaboratory.Technician + JobLevel2 + JobLevel5 +
                  JobRoleManufacturing.Director + JobRoleResearch.Director + JobRoleResearch.Scientist +
                  JobRoleSales.Executive + MaritalStatusMarried + MaritalStatusSingle +
                  JobInvolvement3 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_22)


#removing JobRoleLaboratory.Technician
model_23 <- glm(Attrition~ Age + NumCompaniesWorked +
                  PercentSalaryHike + TotalWorkingYears + TrainingTimesLastYear +
                  YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                  EducationFieldLife.Sciences +
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                  JobLevel2 + JobLevel5 +
                  JobRoleManufacturing.Director + JobRoleResearch.Director + JobRoleResearch.Scientist +
                  JobRoleSales.Executive + MaritalStatusMarried + MaritalStatusSingle +
                  JobInvolvement3 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_23)

#removing JobRoleResearch.Scientist
model_24 <- glm(Attrition~ Age + NumCompaniesWorked +
                  PercentSalaryHike + TotalWorkingYears + TrainingTimesLastYear +
                  YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                  EducationFieldLife.Sciences +
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                  JobLevel2 + JobLevel5 +
                  JobRoleManufacturing.Director + JobRoleResearch.Director +
                  JobRoleSales.Executive + MaritalStatusMarried + MaritalStatusSingle +
                  JobInvolvement3 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_24)


#removing MaritalStatusMarried
model_25 <- glm(Attrition~ Age + NumCompaniesWorked +
                  PercentSalaryHike + TotalWorkingYears + TrainingTimesLastYear +
                  YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                  EducationFieldLife.Sciences +
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                  JobLevel2 + JobLevel5 +
                  JobRoleManufacturing.Director + JobRoleResearch.Director +
                  JobRoleSales.Executive + MaritalStatusSingle +
                  JobInvolvement3 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_25)

#Removing PercentSalaryHike
model_26 <- glm(Attrition~ Age + NumCompaniesWorked +
                  TotalWorkingYears + TrainingTimesLastYear +
                  YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                  EducationFieldLife.Sciences +
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                  JobLevel2 + JobLevel5 +
                  JobRoleManufacturing.Director + JobRoleResearch.Director +
                  JobRoleSales.Executive + MaritalStatusSingle +
                  JobInvolvement3 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_26)


#removing JobRoleSales.Executive
model_27 <- glm(Attrition~ Age + NumCompaniesWorked +
                  TotalWorkingYears + TrainingTimesLastYear +
                  YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                  EducationFieldLife.Sciences +
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                  JobLevel5 +
                  JobRoleManufacturing.Director + JobRoleResearch.Director +
                  JobRoleSales.Executive + MaritalStatusSingle +
                  JobInvolvement3 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_27)

#removing JobRoleSales.Executive
model_28 <- glm(Attrition~ Age + NumCompaniesWorked +
                  TotalWorkingYears + TrainingTimesLastYear +
                  YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                  EducationFieldLife.Sciences +
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                  JobLevel5 +
                  JobRoleManufacturing.Director + JobRoleResearch.Director +
                  MaritalStatusSingle + JobInvolvement3 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_28)

#removing JobRole.ResearchDirector
model_29 <- glm(Attrition~ Age + NumCompaniesWorked +
                  TotalWorkingYears + TrainingTimesLastYear +
                  YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                  EducationFieldLife.Sciences +
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                  JobLevel5 +
                  JobRoleManufacturing.Director + MaritalStatusSingle + JobInvolvement3 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_29)

#removing JobLevel5
model_30 <- glm(Attrition~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                  EducationFieldLife.Sciences + EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                  JobRoleManufacturing.Director + MaritalStatusSingle + JobInvolvement3 + AvgWorkHours,data = train_df,family = "binomial")

summary(model_30)


#removing JobRoleManufacturing.Director
model_31 <- glm(Attrition~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
                  EducationFieldLife.Sciences + EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                  JobRoleManufacturing.Director + MaritalStatusSingle + AvgWorkHours,data = train_df,family = "binomial")

summary(model_31)

#removing BusinessTravelTravel_Rarely
model_32 <- glm(Attrition~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently +
                  EducationFieldLife.Sciences + EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                  JobRoleManufacturing.Director + MaritalStatusSingle + AvgWorkHours,data = train_df,family = "binomial")

summary(model_32)

#removing JobRoleManufacturing.Director
model_33 <- glm(Attrition~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently +
                  EducationFieldLife.Sciences + EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                  MaritalStatusSingle + AvgWorkHours,data = train_df,family = "binomial")


summary(model_33)


#removing EducationFieldMarketing
model_34 <- glm(Attrition~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently +
                  EducationFieldLife.Sciences + EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                  MaritalStatusSingle + AvgWorkHours,data = train_df,family = "binomial")

summary(model_34)

#removing EducationFieldLife.Sciences
model_35 <- glm(Attrition~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently +
                  EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree +
                  MaritalStatusSingle + AvgWorkHours,data = train_df,family = "binomial")

summary(model_35)

#removing EducationFieldMedical
model_36 <- glm(Attrition~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently +
                  EducationFieldOther + EducationFieldTechnical.Degree +
                  MaritalStatusSingle + AvgWorkHours,data = train_df,family = "binomial")

summary(model_36)

#removing EducationFieldTechnical.Degree
model_37 <- glm(Attrition~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently +
                  EducationFieldOther + MaritalStatusSingle + AvgWorkHours,data = train_df,family = "binomial")

summary(model_37)


#removing EducationFieldOther
model_38 <- glm(Attrition~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently +
                  MaritalStatusSingle + AvgWorkHours,data = train_df,family = "binomial")

summary(model_38)


#removing JobSatisfaction2
model_39 <- glm(Attrition~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction3 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently +
                  MaritalStatusSingle + AvgWorkHours,data = train_df,family = "binomial")
summary(model_39)

#removing JobSatisfaction3
model_40 <- glm(Attrition~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently +
                  MaritalStatusSingle + AvgWorkHours,data = train_df,family = "binomial")
summary(model_40)

#removing TrainingTimesLastYear
model_41 <- glm(Attrition~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction2 +
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction4 +
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently +
                  MaritalStatusSingle + AvgWorkHours,data = train_df,family = "binomial")
summary(model_41)


#predicting the Attrition
predict_attrition <- predict(model_41, newdata = test_df,type = "response")


#confusionmatrix
tab <- table(test_df$Attrition,predict_attrition > 0.5)

#accuracy of the model
accuracy <- (tab[1,1]+tab[2,2])/(tab[1,1] + tab[1,2] + tab[2,1] + tab[2,2])
specificity <- tab[2,2]/(tab[2,1] + tab[2,2])
sensetivity <- tab[1,1]/(tab[1,1] + tab[2,1])
#####################################################################
#  accuracy = true positive + true negetive/TP + TN + FP + FN       #
#           =  0.85                                                 #
#  specificity = TN/TN + FP                                         #
#              =   0.24                                             #
#  sensitivity = TP/TP+FN                                           #
#              =  0.86                                              #
#####################################################################


#creating ROC curve to check the model performance
ROCRpred <- prediction(predict_attrition,test_df$Attrition)
ROCRperf <- performance(ROCRpred,'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

#AUC
ROCRauc <- performance(ROCRpred,measure = "auc")
ROCRauc <- ROCRauc@y.values[[1]]
ROCRauc
accuracy
specificity
sensetivity

#KS statistic

ks_table_test <- attr(ROCRperf, "y.values")[[1]] - 
  (attr(ROCRperf, "x.values")[[1]])

max(ks_table_test)

#Lift & Gain chart 

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_df$Attrition, predict_attrition, groups = 10)

#Plotting the gain and lift chart
plot_grid(ggplot(Attrition_decile,aes(x=Attrition_decile$bucket,y=Attrition_decile$Gain, color=""))+geom_line()+geom_point(),
          ggplot(Attrition_decile,aes(x=Attrition_decile$bucket,y=Attrition_decile$Cumlift))+geom_line()+geom_point(), 
          align = "h",ncol = 1)

