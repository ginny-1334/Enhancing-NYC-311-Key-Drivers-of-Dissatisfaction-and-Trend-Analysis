

#Final R Code


#### Load required library ####
library(tidyverse)
library(data.table)
library(ggplot2)
library(lubridate)
library(broom)
library(forcats)
library(tidyr)
library(tidytext)
library(tm)
library(SnowballC)
library(topicmodels)
library(wordcloud)
library(stringr)
library(scales)
library(forecast)
library(ggthemes)
library(gridExtra)
library(quantmod)
library(xts)
library(zoo)
library(fpp2)
library(tseries)

############## Data Preparation and exploration for Analysis ###################

#First Raw dataset

#Import 311 survey dataset to R
project_data <- read.csv("311_Survey_RAW.csv")
project_data


#Summary the data set, check the variables
summary(project_data)

#Check the missing values
print(colSums(is.na(project_data)))

#We can see that Agency.Name,Descriptor,Borough,Resolution.Description and Dissatisfaction.Reason has the missing values.
#To clean the data, empty strings in character columns are converted to NA
project_data[project_data == ""] <- NA


#Pick the numeric variables from the data set
numeric_cols <- project_data %>%
  select(where(is.numeric))

#Standardize the numeric variables for better comparability in visualization
numeric_scaled <- numeric_cols %>%
  mutate(across(everything(), ~ scale(.)[,1]))

#Convert the scaled data to long format for plotting
numeric_long <- pivot_longer(numeric_scaled,
                             cols = everything(),
                             names_to = "Variable",
                             values_to = "Value")

#Create a  boxplot to visually inspect potential outliers in numeric variables
ggplot(numeric_long, aes(x = Value, y = fct_rev(factor(Variable)))) +
  geom_boxplot(outlier.color = "red", fill = "gray90", color = "black", width = 0.4) +
  geom_jitter(alpha = 0.15, color = "steelblue", size = 0.5, height = 0.1) +
  labs(title = "Standardized Boxplot for Numeric Variables",
       x = "Standardized Value (Z-score)",
       y = "Variables") +
  theme_minimal(base_size = 13)
#No significant outliers were found

#Rename selected columns for clarity and consistency
project_data2 <- project_data%>%
  rename(
    Unique_ID = Unique.Key,
    Agency_Acronym = Agency.Acronym,
    Agency_Name = Agency.Name,
    Complaint_Type = Complaint.Type,
    Resolution_Description = Resolution.Description,
    Survey_Month = Survey.Month,
    Survey_Year = Survey.Year,
    Satisfaction_Response = Satisfaction.Response,
    Dissatisfaction_Reason = Dissatisfaction.Reason
  )

#Save cleaned dataset as a new csv file
write.csv(project_data2,file="311_Survey.csv",row.names = FALSE)


#Second dataset

#Import 311 request dataset to R

new_data <- read.csv("311_Requests_RAW.csv")
head(new_data)

#To clean the data, empty strings in character columns are converted to NA
new_data[new_data == ""] <- NA
str(new_data)

#Extract datetime information from the original date columns and separate the datetime into date and time components
new_data$Created.DateTime <- as.POSIXct(new_data$`Created.Date`, format = "%m/%d/%Y %I:%M:%S %p")
new_data$Closed.DateTime <- as.POSIXct(new_data$`Closed.Date`, format = "%m/%d/%Y %I:%M:%S %p")
new_data<- new_data%>%
  mutate(
    Created_Date = as.Date(Created.DateTime),
    Created_Time = format(Created.DateTime,"%H:%M:%S"),
    Closed_Date = as.Date(Closed.DateTime),
    Closed_Time = format(Closed.DateTime,"%H:%M:%S")
  )

#Remove the original datetime columns after splitting them into date and time
new_data <- new_data%>%
  select(-c(Created.Date,Created.DateTime,Closed.Date,Closed.DateTime))

#Rename selected columns for consistency and easier reference
new_data <- new_data%>%
  rename(
    Agency_Name = Agency.Name,
    Complaint_Type = Complaint.Type,
    Resolution_Description = Resolution.Description
    )

#Save cleaned dataset as a new csv file
write.csv(new_data,file="311_Requests.csv",row.names = FALSE)

#Create summary tables for future analysis

#Complaint type frequency summary
summary_by_type <- new_data %>%
  group_by(Complaint_Type) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

print(summary_by_type)

#Complaint type by borough summary
summary_by_type_borough <- new_data %>%
  group_by(Complaint_Type, Borough) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

print(summary_by_type_borough)

#Complaint status summary
summary_by_status <- new_data %>%
  group_by(Status) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

print(summary_by_status)

#################### RQ 1 Analysis ###########################

#Import the dataset we cleaned above
data1 <- read.csv("311_Requests.csv")
data2 <- read.csv("311_Survey.csv")


# Create binary target variable for dissatisfaction and fit Logistic Regression Model.
data2 <- data2%>%
  mutate(is_dissatisfied = as.integer(Satisfaction_Response %in% c("Disagree","Strongly Disagree")),Complaint_Type = as.factor(Complaint_Type),Borough = as.factor(Borough))

# Fit the logistic regression
model_satisfaction <- glm(is_dissatisfied ~ Complaint_Type + Borough, data=data2, family = binomial)
summary(model_satisfaction)

tidy_model <- tidy(model_satisfaction) %>%
  mutate(odds_ratio = exp(estimate))

top10_complaint_types <- tidy_model %>%
  filter(p.value < 0.05,grepl("^Complaint_Type", term)) %>%
  arrange(p.value) %>%
  slice_head(n = 10)%>%
  select(term, estimate, std.error, statistic, p.value, odds_ratio)
print(top10_complaint_types)

#General Anaylsis about the dataset
heat_data <- data2 %>%
  filter(!is.na(Complaint_Type), !is.na(Borough)) %>%
  group_by(Complaint_Type, Borough) %>%
  summarise(count = n()) %>%
  ungroup()

top_complaints <- heat_data %>%
  group_by(Complaint_Type) %>%
  summarise(total = sum(count)) %>%
  arrange(desc(total)) %>%
  slice_head(n = 10) %>%
  pull(Complaint_Type)

heat_plot_data <- heat_data %>%
  filter(Complaint_Type %in% top_complaints)

ggplot(heat_plot_data, aes(x = Borough, y = Complaint_Type, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#CCE5FF", high = "#003366") +
  labs(title = "Top Complaint Types Across Boroughs",
       x = "Borough", y = "Complaint Type") +
  theme_minimal()

borough_count <- data1 %>%
  filter(!is.na(Borough)) %>%
  group_by(Borough) %>%
  summarise(count = n())

ggplot(borough_count, aes(x = Borough, y = count, fill = count)) +
  geom_col() +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(title = "Number of 311 Complaints by Borough",
       x = "Borough", y = "Count") +
  theme_minimal()

# Plot the odds ratio
ggplot(top10_complaint_types, aes(x = fct_reorder(term, odds_ratio), y = odds_ratio, fill = odds_ratio)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "#CCE5FF", high = "#003366") +
  labs(
    title = "Top 10 Complaint Types with Dissatisfaction",
    x = "Complaint Type",
    y = "Odds Ratio (exp(beta))"
  ) +
  theme_minimal()

# Now we do some Descriptive Statistics - Satisfaction
type_summary <- data2 %>%
  group_by(Complaint_Type) %>%
  summarise(
    count = n(),
    dissatisfied_rate = mean(is_dissatisfied, na.rm = TRUE)
  ) %>%
  arrange(desc(dissatisfied_rate))

top_types <- type_summary %>%
  slice_max(count, n = 15)

ggplot(top_types, aes(x = fct_reorder(Complaint_Type, dissatisfied_rate),
                      y = dissatisfied_rate,
                      fill = dissatisfied_rate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "#CCE5FF", high = "#003366") +
  labs(
    title = "Top 15 Complaint Types by Dissatisfaction Rate",
    x = "Complaint Type",
    y = "Dissatisfaction Rate"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

borough_summary <- data2 %>%
  filter(!is.na(Borough))%>%
  group_by(Borough) %>%
  summarise(
    total = n(),
    dissatisfied = sum(is_dissatisfied, na.rm = TRUE),
    dissatisfied_rate = mean(is_dissatisfied, na.rm = TRUE)
  )

ggplot(borough_summary, aes(x = Borough, y = dissatisfied_rate, fill = Borough)) +
  geom_col() +
  labs(title = "Dissatisfaction Rate by Borough", y = "Dissatisfaction Rate") +
  theme_minimal()

#Analysis the request data
borough_unresolved <- data1 %>%
  filter(Borough != "Unspecified" & !is.na(Borough)) %>%
  group_by(Borough) %>%
  summarise(
    total = n(),
    unresolved = sum(is.na(Closed_Date)),
    unresolved_rate = mean(is.na(Closed_Date))
  )

ggplot(borough_unresolved, aes(x = Borough, y = unresolved_rate, fill = Borough)) +
  geom_col() +
  labs(title = "Unresolved Rate by Borough", y = "Unresolved Rate") +
  theme_minimal()


#Find the cross tab
type_borough_unresolved <- data1 %>%
  group_by(Complaint_Type, Borough) %>%
  summarise(
    total = n(),
    unresolved = sum(is.na(Closed_Date)),
    unresolved_rate = mean(is.na(Closed_Date))
  ) %>%
  filter(total >= 20) %>%
  arrange(desc(unresolved_rate))
head(type_borough_unresolved, 10)

#Unresolved Rate by Borough
complaint_unresolved_summary <- data1 %>%
  filter(!is.na(Complaint_Type)) %>%
  group_by(Complaint_Type) %>%
  summarise(
    total = n(),
    unresolved = sum(Status != "Closed", na.rm = TRUE),
    unresolved_rate = mean(Status != "Closed", na.rm = TRUE)
  ) %>%
  arrange(desc(unresolved_rate)) %>%
  slice_max(total, n = 15)

#Unresolved Rate by Borough
ggplot(complaint_unresolved_summary,
       aes(x = fct_reorder(Complaint_Type, unresolved_rate),
           y = unresolved_rate,
           fill = unresolved_rate)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "#CCE5FF", high = "#003366") +
  labs(
    title = "Top 15 Complaint Types by Unresolved Rate",
    x = "Complaint Type",
    y = "Unresolved Rate"
  ) +
  theme_minimal()






#################### RQ 2 Analysis ###########################

#Load data
survey_data <- read_csv("311_Survey.csv")

#Detect vague phrases before data cleaning

vague_phrases <- c(
  "no violation observed", "attempted to inspect", "unable to locate",
  "no evidence found", "could not verify", "resolved prior to arrival",
  "duplicate complaint", "no access", "attempted contact",
  "not found", "case closed", "insufficient evidence", "not able to determine",
  "issue not observed", "no further action"
)

survey_data <- survey_data %>%
  mutate(has_vague_phrase = str_detect(tolower(Resolution_Description),
                                       regex(paste(vague_phrases, collapse = "|"), ignore_case = TRUE)))

#Preprocess Resolution Description (for LDA)
survey_data_clean <- survey_data %>%
  mutate(Resolution_Description_clean = Resolution_Description %>%
           str_to_lower() %>%
           removePunctuation() %>%
           removeNumbers() %>%
           removeWords(stopwords("en")) %>%
           wordStem())

#Tokenize for LDA
tokens <- survey_data_clean %>%
  unnest_tokens(word, Resolution_Description_clean)

# Remove sparse words and very short tokens
tokens <- tokens %>% filter(str_length(word) > 2)

# Create DTM
tokens_grouped <- tokens %>%
  count(Unique_ID, word) %>%
  ungroup()

sparse_dtm <- tokens_grouped %>%
  cast_dtm(document = Unique_ID, term = word, value = n)

#Run LDA with 5 topics
lda_model <- LDA(sparse_dtm, k = 5, control = list(seed = 1234))

#Get top terms per topic
top_terms <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup()

# Visual: Top Terms per Topic (Figure 1)

topic_labels <- c(
  "1" = "1.Vague Inspection Outcome",
  "2" = "2.Previously Addressed",
  "3" = "3.Agency Referral",
  "4" = "4.Unsuccessful Outreach",
  "5" = "5.Resolved Action Taken"
)

top_terms_labeled <- top_terms %>%
  mutate(topic_label = factor(topic, labels = topic_labels)) %>%
  mutate(term = reorder_within(term, beta, topic_label))

ggplot(top_terms_labeled, aes(x = term, y = beta, fill = beta)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic_label, scales = "free", ncol = 2) +
  coord_flip() +
  scale_x_reordered() +
  scale_fill_gradient(low = "#ADD8E6", high = "#001f4d") +
  labs(
    title = "Top Terms per Topic",
    subtitle = "Each topic captures a recurring theme in resolution language",
    x = "Top Words",
    y = "Term Importance (β)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold", color = "#001f4d"),
    plot.title = element_text(face = "bold", size = 16),
    axis.text.y = element_text(size = 8)  # 🔽 This line makes the y-axis (term labels) smaller
  )

#Assign dominant topic to each document
topic_gamma <- tidy(lda_model, matrix = "gamma") %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  ungroup()

survey_data <- survey_data %>%
  left_join(topic_gamma, by = c("Unique_ID" = "document")) %>%
  rename(dominant_topic = topic) %>%
  mutate(dominant_topic = as.factor(dominant_topic))

#Label vague topics (based on inspection, topic 1 and 4)
survey_data <- survey_data %>%
  mutate(topic_vague = if_else(dominant_topic %in% c("1", "4"), 1, 0))

#Create satisfaction outcome variable
survey_data <- survey_data %>%
  mutate(satisfied = if_else(Satisfaction_Response == "Yes", 1, 0))

#Filter for top 10 agencies for stability in regression
top_agencies <- survey_data %>%
  count(Agency_Name, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  pull(Agency_Name)

model_data <- survey_data %>%
  filter(Agency_Name %in% top_agencies)

#Logistic regression

model <- glm(satisfied ~ has_vague_phrase + topic_vague + Agency_Name + Complaint_Type,
             data = model_data, family = "binomial")
summary(model)


#Visualization Script

#additional: Vague phrase rate by agency
top_vague_by_agency <- model_data %>%
  group_by(Agency_Name) %>%
  summarise(vague_rate = mean(has_vague_phrase, na.rm = TRUE)) %>%
  arrange(desc(vague_rate))
print(top_vague_by_agency)

# additional: visualization of Wordcloud for top resolution terms
top_words <- tokens %>%
  count(word, sort = TRUE) %>%
  filter(n > 100)
wordcloud(words = top_words$word, freq = top_words$n, max.words = 100)

# additional: Visualization of most frequent resolution terms
top_terms <- tokens %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n = 10)  # Now showing top 10 terms

ggplot(top_terms, aes(x = reorder(word, n), y = n, fill = n)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "#ADD8E6", high = "#001f4d") +
  labs(
    title = "Most Frequent Terms in 311 Resolution Descriptions",
    x = "Term",
    y = "Frequency",
    fill = "Count"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 11)
  )


#LDA Analysis based on complaint type

# 1. Extract document-topic probabilities
topic_gamma <- tidy(lda_model, matrix = "gamma") %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  ungroup() %>%
  rename(Unique_ID = document, dominant_topic = topic)

# 2. Join with original survey_data (must match on Unique_ID)
survey_data <- survey_data %>%
  left_join(topic_gamma, by = "Unique_ID") %>%
  mutate(dominant_topic = as.factor(dominant_topic))

#3. Plot
blue_shades <- c(
  "Resolved Action Taken" = "#b0cde5",
  "Agency Referral" = "#6baed6",
  "Previously Addressed" = "#4292c6",
  "Unsuccessful Outreach" = "#2171b5",
  "Vague Inspection Outcome" = "#08306b"
)

ggplot(topic_complaint_dist, aes(x = reorder(Complaint_Type, -prop), y = prop, fill = Topic_Label)) +
  geom_col(position = "fill") +
  labs(
    title = "Distribution of LDA Topics by Complaint Type",
    subtitle = "Top 10 Complaint Types — Stacked by Dominant Topic in Resolution",
    x = "Complaint Type",
    y = "Proportion of Resolutions",
    fill = "Topic"
  ) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = blue_shades) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom"
  )



#LDA Analysis based on agency

# step 1: Extract gamma (document-topic probabilities)
topic_gamma <- tidy(lda_model, matrix = "gamma") %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%     # Get dominant topic
  ungroup() %>%
  rename(Unique_ID = document, dominant_topic = topic) %>%
  mutate(
    Unique_ID = as.character(Unique_ID),
    dominant_topic = as.factor(dominant_topic)
  )

# step 2: Ensure survey_data$Unique_ID is also character
survey_data <- survey_data %>%
  mutate(Unique_ID = as.character(Unique_ID))

# step 3: Join dominant topic into survey_data
survey_data <- survey_data %>%
  left_join(topic_gamma %>% select(Unique_ID, dominant_topic), by = "Unique_ID")

# step 4: Identify top 10 agencies
top_agencies <- survey_data %>%
  count(Agency_Name, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  pull(Agency_Name)

# step 5: Compute topic distribution by agency
topic_agency_dist <- survey_data %>%
  filter(Agency_Name %in% top_agencies & !is.na(dominant_topic)) %>%
  group_by(Agency_Name, dominant_topic) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Agency_Name) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# step 6: Add topic labels
topic_labels <- c(
  "1" = "Vague Inspection Outcome",
  "2" = "Previously Addressed",
  "3" = "Agency Referral",
  "4" = "Unsuccessful Outreach",
  "5" = "Resolved Action Taken"
)

topic_agency_dist$Topic_Label <- topic_labels[as.character(topic_agency_dist$dominant_topic)]

# step 7: plot
ggplot(topic_agency_dist, aes(x = reorder(Agency_Name, -prop), y = prop, fill = Topic_Label)) +
  geom_col(position = "fill") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(
    values = c(
      "Vague Inspection Outcome" = "#08306b",
      "Previously Addressed" = "#2171b5",
      "Agency Referral" = "#4292c6",
      "Unsuccessful Outreach" = "#6baed6",
      "Resolved Action Taken" = "#c6dbef"
    )
  ) +
  labs(
    title = "Distribution of LDA Topics by Responding Agency",
    subtitle = "Top 10 Agencies — Dominant Topics in Resolution Language",
    x = "Agency",
    y = "Proportion of Resolutions",
    fill = "Topic"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    legend.position = "bottom"
  )

# Create acronym mappings (customize as needed)
agency_acronyms <- c(
  "New York City Police Department" = "NYPD",
  "Department of Sanitation" = "DSNY",
  "Department of Housing Preservation and Development" = "HPD",
  "Department of Transportation" = "DOT",
  "Department of Buildings" = "DOB",
  "Department of Health and Mental Hygiene" = "DOHMH",
  "Department of Environmental Protection" = "DEP",
  "Department of Parks and Recreation" = "DPR",
  "Department of Consumer and Worker Protection" = "DCWP",
  "Department of Finance" = "DOF"
)

# Apply the acronym labels
topic_agency_dist <- topic_agency_dist %>%
  mutate(Agency_Label = recode(Agency_Name, !!!agency_acronyms))

# Plot with acronym labels
ggplot(topic_agency_dist, aes(x = reorder(Agency_Label, -prop), y = prop, fill = Topic_Label)) +
  geom_col(position = "fill") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(
    values = c(
      "Vague Inspection Outcome" = "#08306b",
      "Previously Addressed" = "#2171b5",
      "Agency Referral" = "#4292c6",
      "Unsuccessful Outreach" = "#6baed6",
      "Resolved Action Taken" = "#c6dbef"
    )
  ) +
  labs(
    title = "Distribution of LDA Topics by Responding Agency (Top 10)",
    subtitle = "Using dominant topics in resolution text (LDA)",
    x = "Agency (Acronym)",
    y = "Proportion of Resolutions",
    fill = "Topic"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    legend.position = "bottom"
  )



#################### RQ 3 Analysis ###########################

# Load cleaned NYC 311 Service Requests dataset
data <- read.csv("311_Requests.csv")
data$Created_Date <- as.Date(data$Created_Date)

# Inspect structure and remove missing values
str(data)
View(data)
colSums(is.na(data))
data <- data |> filter(!is.na(Created_Date), !is.na(Complaint_Type))

# ============================
# Monthly Complaint Volume (All Types)
# ============================

# Aggregate monthly complaint volume by type
data_monthly <- data %>%
  mutate(month = floor_date(Created_Date, "month")) %>%
  count(month, Complaint_Type)
data_monthly

# ============================
# Identify Top 10 Complaint Types
# ============================

# Identify top 10 complaint types by overall frequency
complaint_counts <- data |> count(Complaint_Type, sort = TRUE)
total_n <- nrow(data)

top10_sum <- complaint_counts |> slice_head(n = 10) |> summarise(total = sum(n)) |> pull(total)

percentage_top10 <- round((top10_sum / total_n) * 100, 2)
cat("Top 10 complaint types account for", percentage_top10, "% of all 311 service requests.\n")

top10_types <- data_monthly %>%
  group_by(Complaint_Type) %>%
  summarise(total = sum(n)) %>%
  arrange(desc(total)) %>%
  slice_head(n = 10) %>%
  pull(Complaint_Type)

# Visualization of top 10 complyint types
top10_counts <- complaint_counts |> 
  slice_head(n = 10) |> 
  mutate(Complaint_Type = reorder(Complaint_Type, n)) 

# Plot
ggplot(top10_counts, aes(x = Complaint_Type, y = n, fill = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Top 10 Complaint Types in NYC 311",
    x = "Complaint Type",
    y = "Number of Requests"
  ) +
  scale_y_continuous(labels = label_number(scale = 1/1000, suffix = "K")) +
  scale_fill_gradient(low = "#cce5ff", high = "#003366") +
  theme_minimal(base_size = 20) +
  theme(legend.position = "right")

# ============================
# Time Series Construction
# ============================

# Create a monthly time series object for each top 10 complaint type
ts_list <- list()

for (ctype in top10_types) {
  cat("Processing:", ctype, "\n")
  
  ts_data <- data_monthly |> filter(Complaint_Type == ctype)
  if (nrow(ts_data) == 0 || all(is.na(ts_data$month))) next
  
  ts_data <- ts_data |> arrange(month)
  all_months <- seq(min(ts_data$month), max(ts_data$month), by = "month")
  
  ts_data_full <- data.frame(month = all_months) |> 
    left_join(ts_data, by = "month") |> 
    mutate(Complaint_Type = ctype, n = tidyr::replace_na(n, 0))
  
  start_year <- year(min(ts_data_full$month))
  start_month <- month(min(ts_data_full$month))
  
  ts_list[[ctype]] <- ts(ts_data_full$n, start = c(start_year, start_month), frequency = 12)
}

# ============================
# Time Series Analysis with Vsualizations
# ============================

# Line plots of monthly volume over time for each type
plot_list <- lapply(top10_types, function(ctype) {
  autoplot(ts_list[[ctype]]) +
    xlab("") + ylab("Requests") + ggtitle(ctype) +
    scale_y_continuous(labels = label_number(scale = 1/1000, suffix = "K"))
})

# Year-over-year seasonal plots
plot_list_season <- lapply(top10_types, function(ctype) {
  ggseasonplot(ts_list[[ctype]]) +
    xlab("") + ylab("Requests") + ggtitle(ctype) +
    scale_y_continuous(labels = label_number(scale = 1/1000, suffix = "K"))
})

do.call(grid.arrange, c(plot_list, ncol = 2))
do.call(grid.arrange, c(plot_list_season, ncol = 2))

# ============================
# STL Decomposition (Trend/Seasonal/Residual)
# ============================

# Perform STL decomposition on each of the top 10 complaint types
target_types <- top10_types
stl_list <- list()
plot_list <- list()

for (ctype in target_types) {
  ts_data <- ts_list[[ctype]]
  stl_result <- stl(ts_data, s.window = "periodic")
  stl_list[[ctype]] <- stl_result
  
  plot_list[[ctype]] <- autoplot(stl_result) +
    ggtitle(paste(ctype))
}

grid.arrange(grobs = plot_list, ncol = 4)

# ============================
# Time Series by Borough (Top 3 Complaints)
# ============================

# Clean Borough field and extract top 3 complaints per borough
data_clean <- data %>%
  filter(!is.na(Borough), Borough != "", Borough != "Unspecified")

top3_by_borough <- data_clean %>%
  group_by(Borough, Complaint_Type) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(Borough, desc(count)) %>%
  group_by(Borough) %>%
  slice_head(n = 3)

# Aggregate monthly complaint volume for top 3 types per borough
data_monthly_borough3 <- data_clean %>%
  mutate(month = floor_date(Created_Date, "month")) %>%
  inner_join(top3_by_borough, by = c("Borough", "Complaint_Type")) %>%
  group_by(month, Complaint_Type, Borough) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(Borough, Complaint_Type, month)

# Time series plot for each borough's top 3 complaint types
borough_list <- unique(top3_by_borough$Borough)

plot_borough_ts <- function(boro_name) {
  ggplot(
    data_monthly_borough3 %>% filter(Borough == boro_name),
    aes(x = month, y = n, color = Complaint_Type)
  ) +
    geom_line(size = 1.1) +
    labs(
      title = paste("Top 3 Complaints in", boro_name),
      x = "Month", y = "Number of Complaints", color = "Complaint Type"
    ) +
    theme_minimal()
}

borough_plots <- lapply(borough_list, plot_borough_ts)
do.call(grid.arrange, c(borough_plots, ncol = 2))

# ============================
# Forecasting Selected Complaint Types (ETS vs ARIMA)
# ============================

# Define selected complaint types based on prior analysis
selected_types <- c(
  "Noise - Commercial",
  "Noise - Street/Sidewalk",
  "Street Condition",
  "Blocked Driveway",
  "UNSANITARY CONDITION"
)

# Initialize lists to store results
forecast_results <- list()
plot_list <- list()

# Loop through each selected type to fit and forecast
for (ctype in selected_types) {
  ts_data <- ts_list[[ctype]]
  n <- length(ts_data)
  h <- 6  # forecast horizon (last 6 months as test set)
  
  # Split into training and test sets
  train <- window(ts_data, end = time(ts_data)[n - h])
  test <- window(ts_data, start = time(ts_data)[n - h + 1])
  
  # ETS model
  ets_model <- ets(train)
  ets_forecast <- forecast(ets_model, h = h)
  acc_ets <- accuracy(ets_forecast, test)
  
  # ARIMA model with exhaustive search
  arima_model <- auto.arima(train, stepwise = FALSE, approximation = FALSE)
  arima_forecast <- forecast(arima_model, h = h)
  acc_arima <- accuracy(arima_forecast, test)
  
  # Save results
  forecast_results[[ctype]] <- list(
    ets_model = ets_model,
    arima_model = arima_model,
    ets_forecast = ets_forecast,
    arima_forecast = arima_forecast,
    acc_ets = acc_ets,
    acc_arima = acc_arima
  )
  
  # Visualization of actual + forecast
  split_time <- time(ts_data)[n - h + 1]
  
  p <- autoplot(ts_data, series = "Actual") +
    autolayer(ets_forecast$mean, series = "ETS Forecast", PI = FALSE) +
    autolayer(arima_forecast$mean, series = "ARIMA Forecast", PI = FALSE) +
    geom_vline(xintercept = split_time, linetype = "dashed", color = "grey40") +
    ggplot2::annotate("text", x = split_time, y = max(ts_data, na.rm = TRUE), 
             label = "Forecast", hjust = -0.1, vjust = 1.2, size = 3, color = "grey40") +
    ggtitle(paste(ctype)) +
    xlab("Time") + ylab("Requests") +
    scale_y_continuous(labels = label_number(scale = 1/1000, suffix = "K")) +
    scale_color_manual(
      values = c("Actual" = "black", "ETS Forecast" = "steelblue", "ARIMA Forecast" = "red")
    ) +
    theme_minimal() +
    theme(legend.title = element_blank())
  
  plot_list[[ctype]] <- p
}

# Display all forecast plots
do.call(grid.arrange, c(plot_list, ncol = 2))

# ============================
# Accuracy Comparison Table
# ============================

# Collect model accuracy results into a single summary table
accuracy_summary <- map_dfr(selected_types, function(ctype) {
  result <- forecast_results[[ctype]]
  
  tibble(
    Complaint_Type = c(ctype, ctype),
    Model = c("ETS", "ARIMA"),
    ME = c(result$acc_ets["Test set", "ME"], result$acc_arima["Test set", "ME"]),
    RMSE = c(result$acc_ets["Test set", "RMSE"], result$acc_arima["Test set", "RMSE"]),
    MAE = c(result$acc_ets["Test set", "MAE"], result$acc_arima["Test set", "MAE"]),
    MAPE = c(result$acc_ets["Test set", "MAPE"], result$acc_arima["Test set", "MAPE"]),
    MASE = c(result$acc_ets["Test set", "MASE"], result$acc_arima["Test set", "MASE"])
  )
})

# Print summary
print(accuracy_summary)


