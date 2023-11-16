setwd('/Users/kevinledezma/Downloads')
mydata<-read.csv('Surveys-All Records_All_Fields.csv')
table(mydata$English.Spanish)
clean_data1<-mydata[mydata$Movement.Work != 'no', ]
clean_data1<- clean_data1[-c(136,137,138,139,140,141,142,143,144,145,152,153,154,155,156,157,158,159,160,162:207,208:237,285:305,392:411,392:401),]
View(clean_data1)
table(clean_data1$English.Spanish)
table(clean_data1$Movement.Work)
table(clean_data1$How.Many.Jobs.Applied)
table(clean_data1$Demographics..Race)
library(dplyr)

### for race, demogrpahics get proportion of those who are white only.
# Combine the selected columns into a single data frame
combined <- bind_cols(clean_data1, active_columns, no_active_columns)

# View the combined data frame
View(combined)
function(data) {
  data <- data[, !(colnames(data) %in% c("Name", "First.Name", "Last.Name", "Form.Responder.Email", "Spanish..Form.Responder.Email", "Spanish..First.Name", "Spanish..Last.Name", "Last.name", "Spanish..Last.name", "Form.Submitted.Date"
  ))]
  
  column_counts <- list()
  
  for (column_name in colnames(data)) {
    column_counts[[column_name]] <- table(data[[column_name]])
  }
  
  return(column_counts)
}


clean_survey_data <- function(csv_file, column_name) {
  # Read the CSV file
  data <- read.csv(csv_file)
  
  # Identify the repeating pattern and split the data
  split_data <- strsplit(data$V1, "\\d+\n")
  
  # Create a data frame from the split data
  clean_data <- data.frame(V1 = trimws(split_data), stringsAsFactors = FALSE)
  
  # Separate the columns based on delimiter (assuming it's a comma)
  clean_data <- separate(clean_data, V1, into = c("ID", column_name), sep = ",", fill = "right")
  
  # Remove empty rows
  clean_data <- clean_data[complete.cases(clean_data), ]
  
  # Count the occurrences of the desired column
  column_counts <- clean_data %>%
    count({{ column_name }})
  
  # Return the cleaned data and column counts
  list(clean_data = clean_data, column_counts = column_counts)
}
location<-clean_data1$Demographics..Location
local1<-as.data.frame(location)
local1_counts<-local1 %>% group_by(location) %>% tally()
fig <- plot_ly(local1_counts, labels = ~local1_counts$location, values = local1_counts$n, type = 'pie')
sal_now<-clean_data1$Salary.level.now.working.in.movement..Active.Job.Search.
race<-clean_data1$Demographics..Race
sal_now<-as.data.frame(sal_now)
by_sal <- sal_now %>% group_by(sal_now)
by_sal %>% count(sal_now)
race<-as.data.frame(race)
race_c <- race %>% group_by(race)
race_c %>% count(race)
fig2 <- plot_ly(
  x = by_sal,
  y = race_c,
  name = "SF Zoo",
  type = "bar"
)

ggplot(by_sal, aes(x = sal_now, fill = sal_now)) +
     geom_bar() +
       labs(x = "Salary Level", y = "Count") +
     ggtitle("Distribution of Salary Levels")
ggplot(race_c, aes(x = race, fill = race)) +
  geom_bar() +
  labs(x = "Race", y = "Count") +
  ggtitle("Distribution of Race")
race_c1 <- factor(race_c$race)
# Ungroup the data frame
ungrouped_data <- ungroup(by_sal)

# Count the number of occurrences for each combination of salary and race
counts <- ungrouped_data %>% group_by(sal_now, race$race) %>% count()

# Plot the counts of salary by race using a bar plot
ggplot(counts_filtered, aes(x = counts_filtered$`race$race`, y = n, fill = counts_filtered$sal_now)) +
    geom_bar(stat = "identity") +
   labs(x = "Race", y = "Count", fill = "Salary Level") +
   ggtitle("Counts of Salary by Race")
counts_filtered$sal_now <- gsub(" or .*", "", counts_filtered$sal_now)
counts_filtered$sal_now <- gsub("(/hr|/hour)", "", counts_filtered$sal_now)
counts_filtered$sal_now <- gsub("Over", ">", counts_filtered$sal_now)
counts_filtered$sal_now <- gsub("[^0-9.-]", "", counts_filtered$sal_now)
as.numeric(counts_filtered$sal_now)


# Assign the numeric values to the column
counts_filtered$sal_now <- numeric_values
average_salary <- counts_filtered %>%
  group_by(`race$race`) %>%
  summarize(average_sal = mean(sal_now, na.rm = TRUE))
# Create a table breakdown of salaries by race
salary_breakdown <- table(counts_filtered$`race$race`, counts_filtered$sal_now1)

# Print the table breakdown
View(salary_breakdown)
# Calculate the average salary by race
average_salary_by_race <- counts_filtered %>%
  group_by(`race$race`) %>%
  summarize(average_salary = mean(sal_now))


# Create the plot
ggplot(counts_filtered, aes(x = `race$race`, fill = sal_now1)) +
  geom_bar(position = "stack") +
  labs(x = "Race", y = "Count", fill = "Salary Range") +
  ggtitle("Salary Breakdown by Race") +
  theme_minimal() +
  geom_text(aes(label = paste0("$", average_salary_white)), 
            stat = "count", vjust = -0.5, size = 3, color = "black")
# Clean and simplify race labels
counts_filtered$`race$race` <- gsub(",.*", "", counts_filtered$`race$race`)  # Remove everything after the comma
counts_filtered$`race$race` <- gsub(".* / ", "", counts_filtered$`race$race`)  # Remove everything before and including the forward slash
counts_filtered$`race$race` <- gsub(",.*", "", counts_filtered$`race$race`)  # Remove everything after the comma (again)

# Plot the data with cleaned race labels
ggplot(counts_filtered, aes(x = `race$race`, fill = sal_now1)) +
  geom_bar(position = "stack") +
  labs(x = "Race", y = "Count", fill = "Salary Range") +
  ggtitle("Salary Breakdown by Race") +
  theme_minimal()

average_salary_by_race_agr <- aggregate(as.numeric(counts_filtered$sal_now), 
                            by = list(counts_filtered$`race$race`), 
                            FUN = mean, na.rm = TRUE)
colnames(average_salary_by_race_agr) <- c("Race", "Average Salary")
ggplot(counts_filtered, aes(x = `race$race`, y = as.numeric(sal_now), fill = `race$race`)) +
  geom_boxplot() +
  labs(x = "Race", y = "Average Salary (Now)", fill = "Race") +
  ggtitle("Average Salary by Race") +
  theme_minimal() 
average_salary_now_by_race_agr
### POSSIBLE MECHANISM: as we can see, there is a significant chance that the sampling methodology of this survey made it quite biased...
## for example, Asians and Black Americans were quite wealthy but underrepresented as participants. 

### y-axis; starting salary, ##x-axis; how many years in the movement
sal_then<-clean_data1$Salary.level.first.working.in.movement..Active.Job.Search.
years20<-clean_data1$How.long.in.movement.work..Active.Job.Search.
years20<-gsub(" ", "-", years20)
sal_then <- gsub("", "", sal_then)  # Remove everything after the comma
sal_then <- sal_then[sal_then != ""]
sal_then <- gsub("[$,hr]", "", sal_then)
sal_then <- gsub("[a-zA-Z/\\s]", "", sal_then)
sal_then <- gsub("hourly", "", sal_then)   # Remove "hourly"
sal_then <- gsub("/", "", sal_then)       # Remove "/"
sal_then <- gsub("o", "", sal_then)        # Remove "o"
sal_then <- gsub("1\\s*t\\s*9\\.50", "", sal_then)
sal_then <- gsub("\\D*(\\d+)-(\\d+).*", "\\1-\\2", sal_then)
sal_then <- gsub("[^0-9-]+", "", sal_then)
sal_then <- sal_then[grep("^\\d+-\\d+$", sal_then)]

# Convert to numeric range (optional)
sal_then <- as.numeric(sal_then)
# Print the cleaned variable
sal_then
as.numeric(sal_then)
sal_then_table<-table(sal_then)
average_salary_by_race_agr <- aggregate(as.numeric(counts_filtered$sal_now), 
                                        by = list(counts_filtered$`race$race`), 
                                        FUN = mean, na.rm = TRUE)

View(clean_data1$Benefits..Active.Job.Search.)
