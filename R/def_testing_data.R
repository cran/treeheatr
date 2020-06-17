library(dplyr)
testing_data <- iris %>%
  group_by(Species) %>%
  slice(1:5) %>%
  ungroup()
# print(testing_data)


