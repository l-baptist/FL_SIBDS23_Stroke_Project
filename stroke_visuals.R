install.packages("tidyverse")
install.packages("tidyr")
install.packages("dyplr")
install.packages("Hmisc")
library(tidyverse)
library(tidyr)
library(dplyr)
library(Hmisc)

# read in data
data <- read.csv("/Users/janneese/Desktop/healthcare-dataset-stroke-data.csv")

# mutate data so visuals are easier to read
stroke_data <- data %>%
  filter(gender != "Other") %>%
  mutate(hypertension = ifelse(hypertension == 0, "No", "Yes")) %>%
  mutate(heart_disease = ifelse(heart_disease == 0, "No", "Yes")) %>%
  mutate(stroke = ifelse(stroke == 0, "No", "Yes"))

# change Residennce_type to residence_type
names(stroke_data) <- tolower(names(stroke_data))

stroke_data$bmi <- as.numeric(stroke_data$bmi)

### AGE

# group stroke by age
stroke_by_age <- stroke_data %>%
  group_by(age, stroke) %>%
  count(stroke)

# which age group is more likely to experience a stroke
stroke_by_age %>%
  filter(stroke == "Yes") %>%
  ggplot(mapping = aes(x = age, y = n)) + geom_point() + 
  geom_smooth(method = "loess", span = 0.3) +
  labs(subtitle = "Age Distribution of Stroke", x = "Age", y = "Number of Strokes")

### GENDER
## BOX PLOTS

# without stroke fill
stroke_data %>%
  ggplot(mapping = aes(x = gender, y = age)) + 
  geom_boxplot() + labs(title = "Age Distribution - Male vs Female", x = "Gender", y = "Age")

# with stroke fill
stroke_data %>%
  ggplot(mapping = aes(x = gender, y = age, fill = stroke)) + 
  geom_boxplot() + labs(title = "Age Distribution - Male vs Female", x = "Gender", y = "Age")

# group strokes by gender
stroke_by_gender <- stroke_data %>%
  group_by(gender, stroke) %>%
  count(stroke)

## GENDER BAR PLOT
stroke_by_gender %>%
  ggplot(mapping = aes(x = gender, y = n, fill = stroke)) +
  geom_bar(stat = "identity", position = "fill") + labs(title = "Percentage of Female vs Male", x = "Gender", y = "Percentage")

### HYPERTENSION

## BOX PLOTS
# without stroke fill
stroke_data %>%
  ggplot(mapping = aes(x = hypertension, y = age)) + 
  geom_boxplot() + labs(title = "Age Distribution - Hypertension", x = "Hypertension", y = "Age")

# with stroke fill
stroke_data %>%
  ggplot(mapping = aes(x = hypertension, y = age, fill = stroke)) + 
  geom_boxplot() + labs(title = "Age Distribution - Hypertension", x = "Hypertension", y = "Age")

# with stroke fill and gender
#stroke_data %>%
 # ggplot(mapping = aes(x = hypertension, y = age, fill = stroke, color = gender)) + 
  #geom_boxplot() + labs(title = "Age Distribution - Hypertension", x = "Hypertension", y = "Age")

## HYPERTENSION BAR PLOT
# group strokes by hypertension (yes/no)
stroke_by_hypertension <- stroke_data %>%
  group_by(hypertension, stroke) %>%
  count(stroke)

stroke_by_hypertension %>%
  ggplot(mapping = aes(x = hypertension, y = n, fill = stroke)) +
  geom_bar(stat = "identity", position = "fill") + labs(title = "Percentage of Without Hypertension vs With Hypertension", x = "Hypertension", y = "Percentage")

### HEART DISEASE

## BOX PLOTS
# without stroke fill
stroke_data %>%
  ggplot(mapping = aes(x = heart_disease, y = age)) + 
  geom_boxplot() + labs(title = "Age Distribution - Heart Disease", x = "Heart Disease", y = "Age")

# with stroke fill
stroke_data %>%
  ggplot(mapping = aes(x = heart_disease, y = age, fill = stroke)) + 
  geom_boxplot() + labs(title = "Age Distribution - Heart Disease", x = "Heart Disease", y = "Age")

# with stroke fill and gender
#stroke_data %>%
#  ggplot(mapping = aes(x = heart_disease, y = age, fill = stroke, color = gender)) + 
#  geom_boxplot() + labs(title = "Age Distribution - Heart Disease", x = "Heart Disease", y = "Age")

## HEART DISEASE BAR PLOT
# group strokes by heart disease (yes/no)
stroke_by_heart_disease <- stroke_data %>%
  group_by(heart_disease, stroke) %>%
  count(stroke)

stroke_by_heart_disease %>%
  ggplot(mapping = aes(x = heart_disease, y = n, fill = stroke)) +
  geom_bar(stat = "identity", position = "fill") + labs(title = "Percentage of Without Heart Disease vs With Heart Disease", x = "Heart Disease", y = "Percentage")

### EVER MARRIED

## BOX PLOT
# without stroke fill
stroke_data %>%
  ggplot(mapping = aes(x = ever_married, y = age)) +
  geom_boxplot() + labs(title = "Age Distribution - Ever Married", x = "Ever Married", y = "Age")


# with stroke fill
stroke_data %>%
  ggplot(mapping = aes(x = ever_married, y = age, fill = stroke)) +
  geom_boxplot() + labs(title = "Age Distribution - Ever Married", x = "Ever Married", y = "Age")


## EVER MARRIED BAR PLOT
# group strokes by marriage status
stroke_by_ever_married <- stroke_data %>%
  group_by(ever_married, stroke) %>%
  count(stroke)

stroke_by_ever_married %>%
  ggplot(mapping = aes(x = ever_married, y = n, fill = stroke)) +
  geom_bar(stat = "identity", position = "fill") + labs(title = "Unmarried vs Married", x = "Ever Married", y = "Percentage")


### WORK TYPE

## BOX PLOT
# without stroke fill
stroke_data %>%
  ggplot(mapping = aes(x = work_type, y = age)) +
  geom_boxplot() + labs(title = "Age Distribution - Work Type", x = "Work Type", y = "Age")

# with stroke fill
stroke_data %>%
  ggplot(mapping = aes(x = work_type, y = age, fill = stroke)) +
  geom_boxplot() + labs(title = "Age Distribution - Work Type", x = "Work Type", y = "Age")

# group strokes by work type
stroke_by_work_type <- stroke_data %>%
  group_by(work_type, stroke) %>%
  count(stroke)

## WORK TYPE BAR PLOT
stroke_by_work_type %>%
  ggplot(mapping = aes(x = work_type, y = n, fill = stroke)) +
  geom_bar(stat = "identity", position = "fill") + labs(title = "Which Work Type is More at Risk of Getting a Stroke?", x = "Work Type", y = "Percentage")


### RESIDENCE TYPE

## RESIDENCE TYPE BAR PLOT
# group strokes by residence type
stroke_by_residence_type <- stroke_data %>%
  group_by(residence_type, stroke) %>%
  count(stroke)

stroke_by_residence_type %>%
  ggplot(mapping = aes(x = residence_type, y = n, fill = stroke)) +
  geom_bar(stat = "identity", position = "fill") + labs(title = "Urban vs Rural", x = "Residence Type", y = "Percentage")


### AVERAGE GLUCOSE LEVEL

# without stroke fill
stroke_data %>%
  ggplot(mapping = aes(x = age, y = avg_glucose_level)) +
  geom_smooth(method = "loess", span = 0.3 ) + labs(title = "Age Distribution - Average Glucose Level", x = "Age", y = "Average Glucose Level")

# with stroke fill
stroke_data %>%
  ggplot(mapping = aes(x = age, y = avg_glucose_level, fill = stroke)) +
  geom_smooth(method = "loess", span = 0.3 ) + labs(title = "Age Distribution - Average Glucose Level", x = "Age", y = "Average Glucose Level")

## BOX PLOT
stroke_data %>%
  ggplot(mapping = aes(x = stroke, y = avg_glucose_level, fill = stroke)) + 
  geom_boxplot() + labs(title = "Average Glucose Level Boxplot", x = " Stroke", y = "Average Glucose Level")

## BMI

## SCATTER GRAPH
stroke_data %>%
  ggplot(mapping = aes(x = age, y = bmi, color = stroke)) +
  geom_point(alpha = 0.5) + labs(title = "Age Distribution - BMI", x = "Age", y = "BMI")

## BOX PLOT
# with stroke fill
stroke_data %>%
  ggplot(mapping = aes(x = stroke, y = bmi, fill = stroke)) + 
  geom_boxplot() + labs(title = "BMI Boxplot", x = " Stroke", y = "BMI")

# with stroke fill and gender
stroke_data %>%
  ggplot(mapping = aes(x = stroke, y = bmi, fill = stroke, color = gender)) + 
  geom_boxplot() + labs(title = "BMI Boxplot", x = "Stroke", y = "BMI")

## SMOKING STATUS

## SMOKING STATUS BAR PLOT
# group strokes by smoking status
stroke_by_smoking_status <- stroke_data %>%
  group_by(smoking_status, stroke) %>%
  count(stroke)

stroke_by_smoking_status %>%
  ggplot(mapping = aes(x = smoking_status, y = n, fill = stroke)) +
  geom_bar(stat = "identity", position = "fill") + labs(title = "Percentage by Smoking Status", x = "Smoking Status", y = "Percentage")






