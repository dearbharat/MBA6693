dem <- read.csv("Demographic data.csv")
credit <- read.csv("Credit Bureau data.csv")

#  summary(dem)
#  summary(credit)

#  str(dem)
#  str(credit)

# Demographic Dataset
length(unique(dem$Application.ID))

# 71292

# Credit Dataset
length(unique(credit$Application.ID))

# 71292

dem %>%
  group_by(Application.ID) %>%
  filter(n() > 1)

credit %>%
  group_by(Application.ID) %>%
  filter(n() > 1)

# We could see that few data with same Application ID is for different persons.
# So removing all the duplicate rows

dem <- dem %>%
  group_by(Application.ID) %>%
  filter(n() == 1)

credit <- credit %>%
  group_by(Application.ID) %>%
  filter(n() == 1)

# Total 71289

# Merging the datasets
merged_data <- merge(dem, credit, by=c("Application.ID", "Performance.Tag"))

#===========================#
#   Changing Column Names   #
#===========================#
# 5 , 11:25
names(merged_data)[c(1:2, 5:6, 10:29)] <- c("Application_ID", "Performance_Tag", "Marital_Status", "No_Of_Dependents", "Type_Of_Residence", "Months_In_Current_Residence", "Months_In_Current_Company", "No_Of_90_DPD_6_months", "No_Of_60_DPD_6_months", "No_Of_30_DPD_6_months", "No_Of_90_DPD_12_months","No_Of_60_DPD_12_months","No_Of_30_DPD_12_months", "Avg_CC_Utilization_12_months", "Trades_6_months", "Trades_12_months", "PL_Trades_6_months", "PL_Trades_12_months", "Inquiries_6_months", "Inquiries_12_months", "Open_Home_Loan", "Outstanding_Balance", "Total_No_of_trades", "Open_Auto_Loan")

#=====================#
#   Performance Tag   #
#=====================#

# NA count
merged_data$Performance_Tag %>%
  is.na() %>%
  sum()

# Summary for Gender
merged_data$Gender %>%
  summary()

# Converting NA for Gender variable to "M"
levels(merged_data$Gender)[1] <- "M"
#===================#
#   Color palette   #
#===================#

cp_2 <- c("#FEA47F", "#F97F51")
cp_3 <- c("#2A363B", "#E84A5F", "#FF847C")
cp_5 <- c("#2A363B", "#E84A5F", "#FF847C", "#FECEAB", "#99B898")
cp_8 <- c("#FEA47F", "#F97F51", "#B33771", "#3B3B98", "#58B19F", "#BDC581", "#2C3A47", "#82589F")



# Plot for frequency of each Gender
ggplot(merged_data, aes(x=Gender, y=..count../1000, fill=Gender)) +
  geom_bar() +
  scale_fill_manual(values = cp_2)+
  labs(x="Gender", y="Frequency in 1000s", fill="Gender", title="Frequency of different Gender") +
  theme_minimal()

#=====================#
#   Marital Status   #
#=====================#

# Summary for Marital status at time of application
merged_data$Marital_Status %>%
  summary()
# 6 NA's

# Converting NA for Marital status at time of application variable to "Married"
levels(merged_data$Marital_Status)[1] <- "Married"

# Plot for Marital status at time of application frquency
ggplot(merged_data, aes(x=Marital_Status, y=..count../1000, fill=Marital_Status)) +
  geom_bar()+
  scale_fill_manual(values = cp_8)+
  labs(x="Marital Status at time of application", y="Frequency in 1000s", fill="Marital Status", title="Frequency of different Marital Status") +
  theme_minimal()

#===============#
#   Education   #
#===============#

# checking for NA values
merged_data$Education %>%
  is.na() %>%
  sum()

# 0

# Checking for blank rows
merged_data$Education %>%
  summary()

levels(merged_data$Education)[1] <- "Professional"

# Plot for Education Frequency
ggplot(merged_data, aes(x=Education, y=..count../1000, fill=Education)) +
  geom_bar() +
  scale_fill_manual(values=cp_5)+
  labs(x="Education", y="Frequency in 1000s", fill="Education", title="Frequency of Education") +
  theme_minimal()

#Converting the variable into factor type
merged_data$No_Of_Dependents <- merged_data$No_Of_Dependents %>% as.factor()

class(merged_data$No_Of_Dependents)

#===================================#
#   Correlation of trades opened    #
#===================================#

trades_opened <- merged_data[, c(20, 21)]
cor(trades_opened, use="complete.obs")

