##cross-tabs of salary starting and ending salary by categories..... comms, org, policy, dev, ops
salary_first<- c(clean_data1$Salary.level.first.working.in.movement..No.Active.Job.Search.,
 clean_data1$Salary.level.first.working.in.movement..Active.Job.Search) 

salary_now<-c(clean_data1$Salary.level.now.working.in.movement..Active.Job.Search., clean_data1$Salary.level.now.working.in.movement..No.Active.Job.Search.)

work<-c(clean_data1$Work.Currently.Doing..Other.Org..Other,
clean_data1$Work.Currently.Doing..Other.Org)

# Clean the 'work' variable
library(stringr)
work <- work[nzchar(work)]
work <- str_trim(work)  # Remove leading and trailing whitespace
work_df<-as.data.frame(work)
salary_now<-salary_now[nzchar(salary_now)]
salary_now<-str_trim(salary_now)
salary_first<-salary_first[nzchar(salary_first)]
salary_first<-str_trim(salary_first)
# Drop columns that are not "Organizing" or "Organizing, Other"
organizing_combinations <- work_df[grepl("Organizing", work_df$work), ]
organizing_combinations <- gsub(".*Organizing.*", "Organizing", organizing_combinations)
data <- data.frame(organizing_combinations, salary_now)
# Create a new vector with the length of the smaller vector
shorter_length <- min(length(organizing_combinations), length(salary_now))

# Subset the vectors to the shorter length
organizing_combinations <- organizing_combinations[1:shorter_length]
salary_now <- salary_now[1:shorter_length]

# Create the data frame
data <- data.frame(organizing_combinations, salary_now)

ggplot(data, aes(x = organizing_combinations, y = salary_now)) +
   geom_boxplot() +
   labs(x = "Organizing", y = "Salary Now") +
    ggtitle("Boxplot: Salary Now by Organizing")
 ggplot(data, aes(x = organizing_combinations, y = salary_first)) +
  geom_boxplot() +
    labs(x = "Organizing", y = "Salary First") +
    ggtitle("Boxplot: Salary Now by Organizing"
ggplot(data) +
  geom_boxplot(aes(x = organizing_combinations, y = salary_now, color = "Salary Now", fill = "Salary Now")) +
  geom_boxplot(aes(x = organizing_combinations, y = salary_first, color = "Salary First", fill = "Salary First")) +
  labs(x = "Organizing", y = "Salary") +
  ggtitle("Boxplot: Salary Now and Salary First by Organizing") +
  scale_color_manual(values = c("blue", "red"), labels = c("Salary Now", "Salary First")) +
  scale_fill_manual(values = c("lightblue", "pink"), labels = c("Salary Now", "Salary First")) +
  theme(legend.position = "top")
media_combinations <- work_df[grepl("Communications|Digital/ Social media", work_df$work), ]
media_combinations <- ifelse(grepl("Communications,Digital / Social media", media_combinations),
                             "Communications and Digital Social Media",
                             "Communications, Digital/Social Media and Other")
# Create a new vector with the length of the smaller vector
shorter_length <- min(length(media_combinations), length(salary_now))

# Subset the vectors to the shorter length
media_combinations <- media_combinations[1:shorter_length]
salary_now <- salary_now[1:shorter_length]
data10 <- data.frame(media_combinations, salary_now)

# Plot the data
ggplot(data10, aes(x = media_combinations, y = salary_now)) +
  geom_boxplot() +
  labs(x = "Media Combinations", y, y = salary_first, fill = "Salary First")) +
  labs(x = "", y = "Salary") + = "Salary Now") +
  ggtitle("Boxplot: Salary Now by Media Combinations")
shorter_length <- min(length(media_combinations), length(salary_first))

# Subset the vectors to the shorter length
media_combinations <- media_combinations[1:shorter_length]
salary_first <- salary_now[1:shorter_length]
data100 <- data.frame(media_combinations, salary_first)

ggplot(data100, aes(x = media_combinations, y = salary_first)) +
  geom_boxplot() +
  labs(x = "Organizing", y = "Salary First") +
  ggtitle("Boxplot: Salary Now by Organizing")
data_subset <- data.frame(organizing_combinations, salary_now, salary_first)
shorter_length <- min(length(media_combinations), length(salary_first))
media_combinations <- media_combinations[1:shorter_length]
salary_first <- salary_first[1:shorter_length]
salary_now <- salary_now[1:shorter_length]
data_subset <- data.frame(media_combinations, salary_now, salary_first)
ggplot(data_subset) +
  geom_boxplot(aes(x = media_combinations, y = salary_now, fill = "Salary Now")) +
  geom_boxplot(aes(x = media_combinations
  ggtitle("Boxplot: Salary Now and Salary First by Media Combinations") +
  scale_fill_manual(values = c("Salary Now" = "purple", "Salary First" = "darkgreen")) + labels = c("Salary Now", "Salary First")) +
  scale_fill_manual(values = c("lightblue", "pink"), labels = c("Salary Now", "Salary First")) +
  theme(legend.position = "top") +
  scale_x_discrete(labels = c("Communications and Media", "Communications, Media and Other"))

ggplot(data_subset) +
  geom_boxplot(aes(x = media_combinations, y = salary_now, fill = "Salary Now")) +
  geom_boxplot(aes(x = media_combinations, y = salary_first, fill = "Salary First")) +
  labs(x = "", y = "Salary") +
  ggtitle("Boxplot: Salary Now and Salary First by Media Combinations") +
  scale_fill_manual(values = c("Salary Now" = "purple", "Salary First" = "darkgreen"), labels = c("Salary Now", "Salary First")) +
  theme(legend.position = "top") +
  scale_x_discrete(labels = c("Communications and Media", "Communications, Media and Other"))
data_combined <- rbind(
     data.frame(media_combinations, salary = salary_now, category = "Salary Now"),
     data.frame(media_combinations, salary = salary_first, category = "Salary First"))
data_combined$salary <- factor(data_combined$salary, levels = c(
  "$20,001-30,000 or $9.51-$14.50/hr",
  "$31,001-$40,000 or $14.51-$19.25/hr",
  "$40,001-$50,000 or $19.26-$24.00/hr",
  "$50,001-$60,000 or $24.01-$28.85/hr",
  "$60,001-$70,000 or $28.86-$33.65/hr",
  "$70,001-$80,000 or $33.66-$38.50/hr",
  "$80,001-$90,000 or $38.51-$43.30/hr",
  "$90,001-$100,000 or $43.31-$48.08/hr",
  "$100,001-$150,000 or $48.09-$72.11/hour",
  "Over $150,000 or over $72.11/hr"
))

ggplot(data_combined) +
  geom_boxplot(aes(x = media_combinations, y = salary, color = category, fill = category), position = "dodge") +
  labs(x = "", y = "Salary") +
  ggtitle("Boxplot: Salary Now and Salary First by Media Combinations") +
  scale_color_manual(values = c("orange", "darkgreen"), labels = c("Salary Now", "Salary First")) +
  scale_fill_manual(values = c("orange", "darkgreen"), labels = c("Salary Now", "Salary First")) +
  theme(legend.position = "top") +
  scale_x_discrete(labels = c("Communications and Media", "Communications, Media and Other"))

### by time... have salaries in comms increased disproportionally to other work categories?... for comms/digital people in 6-15 and 16+ years, show average salary when starting and starting now
#### one plot each for kinds of work done, one for comms/diigtal, one for org, one for policy/political, one for dev/fundraising, one for field/canvass; average starting salary for 6-16 years,and average salary now 
comms_work <- work_df[work_df$work %in% c("Communications", "Digital / Social media"), ]
salary_first <- salary_first %>%
  str_replace("\\s+or\\s+\\$\\d+(,\\d+)?\\.\\d+-\\$\\d+(,\\d+)?\\.\\d+/hr", "") %>%
  str_replace("\\s*or\\s*\\$\\d+\\s*to\\s*\\$\\d+/hr", "") %>%
  str_replace("\\s*or\\s*\\$[\\d,]+\\s*to\\s*\\$[\\d,]+/hr", "") %>%
  str_replace("\\s*or\\s*\\$[\\d,]+\\s*to\\s*\\$[\\d,]+/hr\\s*", "") %>%
  gsub(" / or \\$1 to \\$9.50/hr", "", .) %>%
  gsub(" or \\over \\$72.11/hr", "", .) %>%
  gsub(" or over \\$72.11/hr", "", .) %>%
  gsub(" or \\$48.09-\\$72.11/hour", "", .) %>%
  gsub("/", "", .) %>%
  gsub("\\$", "", .) %>%
  gsub(",", "", .) %>%
  gsub(" or 1 to 9\\.50/hr", "", .) %>%
  gsub(" or 1 to 9\\.50hr", "", .)
salary_first <- salary_first[nzchar(salary_first)]
data_frame10<-c(starting_salary, years_in_movement)
plot_starting_salary <- ggplot(data_frame10, aes(x = years_in_movement, y = starting_salary)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(x = "Years in Movement", y = "Average Starting Salary") +
  ggtitle("Average Starting Salary by Years in Movement") +
  facet_wrap(~ work, ncol = 1)
