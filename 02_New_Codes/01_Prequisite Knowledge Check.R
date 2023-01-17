library(tidyverse)
library(dslabs)
data(heights)

head(heights)

# Question 1
class(heights$height)
class(heights$sex)

# Question 2
nrow(heights)

# Question 3
heights[777,]

# Question 4
heights$sex[777]
heights[1, 777]
heights[777,1]

# Question 5
max(heights$height)
which.min(heights$height)

# Question 6
summary(heights$height)

# Question 7
nrow(heights[heights$sex == "Male",])/nrow(heights)

# Question 8
heights %>% filter(height > 78) %>% nrow()

# Question 9
heights %>% filter(height > 78 & sex == "Female") %>% nrow()
