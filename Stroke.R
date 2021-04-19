setwd("/Users/seominji/Desktop/Unstruct_DA")

stroke_data<- read.csv("healthcare-dataset-stroke-data.csv")

head(stroke_data)
summary(stroke_data)
str(stroke_data)
stroke_data$bmi
# check unique values of categorical values
cat("Gender:") ;unique(stroke_data$gender)
cat("Married:");unique(stroke_data$ever_married)
cat("Work type:");unique(stroke_data$work_type)
cat("Residence type:");unique(stroke_data$Residence_type)
cat("Smoking:");unique(stroke_data$smoking_status)


# load libraries
library(tidyverse) # metapackage of all tidyverse packages
library(naniar) # handling missing data
library(skimr) # quick overview over the dataset
library(caret) # ML toolkit
library(MLmetrics) # F1 Score
library(imbalance) # algorithms to deal with imbalanced datasets
library(gridExtra) # display plots in grids
library(patchwork) # arrange plots side by side

# 결측값 확인
miss_scan_count(data = stroke_data, search = list("N/A", "Unknown"))

# smoking_status plot
stroke_data %>% 
  group_by(smoking_status) %>% 
  summarise(count = length(smoking_status)) %>% 
  mutate(smoking_status = factor(smoking_status)) %>% 
  ggplot(aes(x = fct_reorder(smoking_status, count), y = count, fill = factor(ifelse(smoking_status=="Unknown","Unknown","Known"))))+
  geom_col()+
  geom_text(aes(label=count, x= smoking_status, y = count), size = 6, hjust = 1.5)+
  coord_flip()+
  scale_fill_manual(values = c("Unknown" = "red", "Known" = "darkgrey")) +
  labs(x = "smoking status") +
  theme(legend.position = "none") # 범례제거
  
# 알려지지 않은 값들이 Unknown 으로 존재 이 값을 결측지로 대체 #

# replace the "N/A" in bmi
stroke_data_clean <- replace_with_na(data = stroke_data, replace = list(bmi = c("N/A"), smoking_status = c("Unknown"))) %>%
  # change bmi to numeric 
  mutate(bmi = as.numeric(bmi))

# check
summary(stroke_data_clean)
unique(stroke_data_clean$smoking_status)

# custom plot size function
fig <- function(width, heigth){
  options(repr.plot.width = width, repr.plot.height = heigth)
}

## ggplot custom theme
theme_bigfont <- theme(plot.title = element_text(size=22),
                       axis.text.x= element_text(size=15),
                       axis.text.y= element_text(size=15), 
                       axis.title=element_text(size=18),
                       legend.text = element_text(size = 14))

# visualize the missing values
vis_miss(stroke_data_clean, cluster = TRUE)

fig(15, 8)

# visualize the missing values
vis_miss(stroke_data_clean, cluster = TRUE) +
  theme_bigfont
######분석중
fig(10, 8)

# check distribution of bmi
ggplot(stroke_data_clean, aes(x = bmi)) +
  geom_histogram() +
  labs(title = "Distribution of BMI") +
  theme_bigfont


fig(10,8)

stroke_data_clean

# impute median and bind shadow to evaluate imputation
stroke_data_imp <- bind_shadow(stroke_data_clean) %>% 
  impute_median_at(.vars = c("bmi")) %>%
  add_label_shadow()



# Explore the median values in bmi in the imputed dataset
ggplot(stroke_data_imp, 
       aes(x = bmi_NA, y = bmi)) + 
  geom_boxplot() +
  labs(title = "Comparison, no-missing vs. imputed values for BMI") +
  theme_bigfont
