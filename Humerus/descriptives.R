# Library loading

library(readr)
library(tidyverse)

# Data loading and preparation
data <- read.csv("CSG data.csv")
sex <- read.csv("humerus_sex.csv")

sex$Sex <- factor(sex$Sex, levels = c("Male", "Female"))
sex$Bone.ID <- substr(sex$Bone.ID, 1, nchar(sex$Bone.ID)-4)

data <- subset(data, (sample %in% sex$Bone.ID))
data$sex <- sex$Sex

##### Descriptives per sex 

# General descriptives for the male sample
data_m <- data[data$sex=="Male",2:47]

des <- matrix(NA, nrow=ncol(data_m), ncol=4)
des[,1] <- round(colMeans(data_m),2)
des[,2] <- round(apply(data_m, 2, sd),2)
des[,3] <- round(apply(data_m, 2, min),2)
des[,4] <- round(apply(data_m, 2, max), 2)

colnames(des) <- c("Mean", "SD", "Min.", "Max.")
rownames(des) <- names(data[-c(1,48)])

write.csv(des, "descriptives_males.csv")

# General descriptives for the female sample
data_f <- data[data$sex=="Female",2:47]

des <- matrix(NA, nrow=ncol(data_f), ncol=4)
des[,1] <- round(colMeans(data_f),2)
des[,2] <- round(apply(data_f, 2, sd),2)
des[,3] <- round(apply(data_f, 2, min),2)
des[,4] <- round(apply(data_f, 2, max), 2)

colnames(des) <- c("Mean", "SD", "Min.", "Max.")
rownames(des) <- names(data[-c(1,48)])

write.csv(des, "descriptives_females.csv")


##### Descriptives per anatomical side

# General descriptives for the left side
left <- read.csv("left.csv")
des <- matrix(NA, nrow=ncol(left)-1, ncol=4)
des[,1] <- round(colMeans(left[,-1]),2)
des[,2] <- round(apply(left[,-1], 2, sd),2)
des[,3] <- round(apply(left[,-1], 2, min),2)
des[,4] <- round(apply(left[,-1], 2, max), 2)

colnames(des) <- c("Mean", "SD", "Min.", "Max.")
rownames(des) <- names(left[-1])

write.csv(des, "descriptives_left.csv")

# General descriptives for the right side
right <- read.csv("right.csv")
des <- matrix(NA, nrow=ncol(right)-1, ncol=4)
des[,1] <- round(colMeans(right[,-1]),2)
des[,2] <- round(apply(right[,-1], 2, sd),2)
des[,3] <- round(apply(right[,-1], 2, min),2)
des[,4] <- round(apply(right[,-1], 2, max), 2)

colnames(des) <- c("Mean", "SD", "Min.", "Max.")
rownames(des) <- names(right[-1])

write.csv(des, "descriptives_right.csv")


##### Descriptives for the differences between matched pairs

# Create a function to calculate the differences between the absolute values
diff <- function(x,y)
  { d = abs(x) - abs(y)
    return(d)
}

# Merge the two datasets
data <- merge(left, right, all = TRUE)

# Create all possible pairs in the dataset
k=2

rows <- data %>% group_by_all() %>% group_split()
row_combinations <- t(combn(x = 1:nrow(data), m = k)) %>% as_tibble()

test<- row_combinations %>%
          mutate_all(~ map(., ~ pluck(rows, .x))) %>% 
          unnest()

# Rearrange the columns so that the names are right next to each other,
# then the variables of the first column sample, then the sample from the
# right column sample
test <- test[,c(1,48,2:47,49:94)]

row_rem <-c(NA)

# Remove the combinations from two individuals from the same side
for (i in 1:nrow(test))
  { 
  if (substring(test$sample[i], nchar(test$sample[i])) == substring(test$sample1[i], nchar(test$sample1[i])))
  {row_rem[i] <- i}
  }

row_rem <- na.omit(row_rem)

test <- test[-row_rem,]

# Save all the remaining possible combinations in a dataframe
mismatched <- cbind(test[,1],test[,2])

# Calculate the |L| - |R| difference for all mismatched combinations
# while taking into account the two possibilities (L individual on the first
# column and R individual on the first column)
for (i in 3:48)
{for (j in 1:nrow(test))
{ if (substring(test$sample[j], nchar(test$sample[j])) == "L")
{mismatched[j,i] <- diff(test[j,i],test[j,i+46])
print(j)}
  else if (substring(test$sample[j], nchar(test$sample[j])) == "R")
  {mismatched[j,i] <- -diff(test[j,i],test[j,i+46])}
}
}

# make a vector of all the names for the combinations ("ID_1" - "ID_2")
p_mism = paste(mismatched[,1], mismatched[,2], sep="-")

# True pairs preparation
true = read.csv2("true_pairs.csv", sep=",", na.strings=c("","NA"))

pairs_true = true[-which(is.na(true[,3])), ]
pairs_true = pairs_true[-which(is.na(pairs_true[,4])), ]

# Prepare two vectors for the chances of (L-R) and (R-L)
p_true_a = paste(pairs_true[,3], pairs_true[,4], sep="-")
p_true_b = paste(pairs_true[,4], pairs_true[,3], sep="-")

row_rem <- NA

# Counter for the number of true pairs in the mismatched vector
for (i in 1:length(p_mism))
{ 
  if (p_mism[i] %in% p_true_a || p_mism[i] %in% p_true_b)
  {row_rem[i] <- i
  print(p_mism[i])}
}

row_rem <- na.omit(row_rem)

# Save the matched pairs into a dataframe
matched <- mismatched[row_rem,]

# General descriptives for matched pairs differences

des <- matrix(NA, nrow=ncol(matched)-2, ncol=2)
des[,1] <- round(colMeans(matched[,-c(1,2)]),2)
des[,2] <- round(apply(matched[,-c(1,2)], 2, sd),2)

colnames(des) <- c("Mean", "SD")
rownames(des) <- names(matched[-c(1,2)])

write.csv(des, "descriptives_diffs.csv")