## Library loading

library(readr)

###### SVM #####

#Load and format the true sex for each individual
true <- read.csv("tibia_sex.csv")
true <- true[,-1]

true$Sex <- factor(true$Sex, levels = c("Male", "Female"))
true$Bone.ID <- substr(true$Bone.ID, 1, nchar(true$Bone.ID)-4)

# The goal is to create a for-loop that checks the svm results and 
# lda results (which are in different files) for the three classifiers

method <- c("svm", "lda")

for (k in 1:2)
{
  predictions <- read.csv(paste0(method[k], "_results.csv"))
  predictions <- predictions[-1,]
  
  # Keep only the individuals that the bone model and the information is
  # available
  predictions <- subset(predictions, (sample %in% true$Bone.ID))
  true <- subset(true, (Bone.ID %in% predictions$sample))

  true_m <- length(which(true$Sex == "Male")) # Store the number of true males
  true_f <- length(which(true$Sex == "Female")) # Same for females
  
  # Prepare an empty matrix to store the results
  res <- matrix(, nrow=7, ncol=3)
  colnames(res) <- c("Classifier 1", "Classifier 2", "Classifier 3")
  rownames(res) <- c("% Accuracy total", "% Accuracy males", "% Accuracy females",
                   "% Unclassified males", "% Unclassified females",
                   "Specificity", "Sensitivity")

  ### Classifier 1

  # Initialize the counters
  counter_f <- 0 # correctly classified females
  f_unc <- 0 # unclassified females (safety measure)
  counter_m <- 0 # correctly classified males
  m_unc <- 0 # unclassified males (safety measure)
  mtf <- 0 # males that were classified as females
  ftm <- 0 # females that were classified as males
  idx <- 0 # index for true matches
  
  # Male Counter
  for (i in 1:nrow(true))
  {
    if (true$Sex[i] == "Male")
      {if (predictions$Classifier.1[i] == 1 && predictions$X[i] >= 0.65)
      {counter_m = counter_m + 1
          idx[i] = i}
      else if (predictions$X[i] < 0.65)
        {m_unc = m_unc +1
          print(true[i,1])}
      }
  }
  
  # Store the IDs of the missed males
  idx <- idx[!is.na(idx)]
  mcl <- true[-idx,]
  mcl <- mcl[mcl$Sex=="Male",]
  idx <- 0
  
  # Female Counter
  for (i in 1:nrow(true))
  {
    if (true$Sex[i] == "Female")
    {if (predictions$Classifier.1[i] == 2 && predictions$X[i] >= 0.65)
    {counter_f = counter_f + 1
    idx[i] = i}
      else if (predictions$X[i] < 0.65)
      {f_unc = f_unc +1
      print(true[i,1])}
    }
  }
  
  # Store the IDs of the missed females
  idx <- idx[!is.na(idx)]
  mcl <- true[-idx,]
  mcl <- mcl[mcl$Sex=="Female",]
  
  # Calculate the misclassified individuals
  mtf <- true_m - counter_m - m_unc
  ftm <- true_f - counter_f - f_unc
  
  # Store and save the confusion matrix
  tab <- rbind(c(counter_m, mtf, m_unc, true_m), 
             c(ftm, counter_f, f_unc, true_f))
  colnames(tab) <- c("Pred Males", "Pred Females", "Unclassified", "Total")
  rownames(tab) <- c("True Males", "True Females")

  write.csv(tab, paste0("tab_", method[k], "_classifier_1.csv"))

  # Calculate the misclassified individuals (including the unclassified)
  mtf <- true_m - counter_m #- m_unc
  ftm <- true_f - counter_f #- f_unc
  
  # Store the total accuracy, per sex accuracy, % of potential unclassified
  # individuals, sensitivity and specificty 
  res[,1] <- c( (counter_m + counter_f)/nrow(true) * 100, 
              counter_m/true_m * 100, counter_f/true_f * 100,
              m_unc/true_m * 100, f_unc/true_f * 100,
              counter_f/(counter_f + ftm),
              counter_m /(counter_m + mtf))
  
  ## The same process is followed for the other two classifiers

  ### Classifier 2

  counter_f <- 0
  f_unc <- 0
  counter_m <- 0
  m_unc <- 0
  mtf <- 0
  ftm <- 0
  idx <- 0

  for (i in 1:nrow(true))
  {
    if (true$Sex[i] == "Male")
    {if (predictions$Classifier.2[i] == 1 && predictions$X.1[i] >= 0.65)
    {counter_m = counter_m + 1
    idx[i] = i}
      else if (predictions$X.1[i] < 0.65)
      {m_unc = m_unc +1
      print(true[i,1])}
    }
  }

  idx <- idx[!is.na(idx)]
  mcl <- true[-idx,]
  mcl <- mcl[mcl$Sex=="Male",]
  idx <- 0

  for (i in 1:nrow(true))
  {
    if (true$Sex[i] == "Female")
    {if (predictions$Classifier.2[i] == 2 && predictions$X.1[i] >= 0.65)
    {counter_f = counter_f + 1
    idx[i] = i}
      else if (predictions$X.1[i] < 0.65)
      {f_unc = f_unc +1
      print(true[i,1])}
    }
  }

  idx <- idx[!is.na(idx)]
  mcl <- true[-idx,]
  mcl <- mcl[mcl$Sex=="Female",]

  mtf <- true_m - counter_m - m_unc
  ftm <- true_f - counter_f - f_unc

  tab <- rbind(c(counter_m, mtf, m_unc, true_m), 
             c(ftm, counter_f, f_unc, true_f))
  colnames(tab) <- c("Pred Males", "Pred Females", "Unclassified", "Total")
  rownames(tab) <- c("True Males", "True Females")

  write.csv(tab, paste0("tab_", method[k], "_classifier_2.csv"))

  mtf <- true_m - counter_m #- m_unc
  ftm <- true_f - counter_f #- f_unc

  res[,2] <- c( (counter_m + counter_f)/nrow(true) * 100, 
              counter_m/true_m * 100, counter_f/true_f * 100,
              m_unc/true_m * 100, f_unc/true_f * 100,
              counter_f/(counter_f + ftm),
              counter_m /(counter_m + mtf))

  ### Classifier 3

  counter_f <- 0
  f_unc <- 0
  counter_m <- 0
  m_unc <- 0
  mtf <- 0
  ftm <- 0
  idx<- 0

  for (i in 1:nrow(true))
  {
    if (true$Sex[i] == "Male")
    {if (predictions$Classifier.3[i] == 1 && predictions$X.2[i] >= 0.65)
    {counter_m = counter_m + 1
    idx[i] = i}
      else if (predictions$X.2[i] < 0.65)
      {m_unc = m_unc +1
      print(true[i,1])}
    }
  }

  idx <- idx[!is.na(idx)]
  mcl <- true[-idx,]
  mcl <- mcl[mcl$Sex=="Male",]
  idx <- 0

  for (i in 1:nrow(true))
  {
    if (true$Sex[i] == "Female")
    {if (predictions$Classifier.3[i] == 2 && predictions$X.2[i] >= 0.65)
    {counter_f = counter_f + 1
    idx[i] = i}
      else if (predictions$X.2[i] < 0.65)
      {f_unc = f_unc +1
      print(true[i,1])}
    }
  }

  idx <- idx[!is.na(idx)]
  mcl <- true[-idx,]
  mcl <- mcl[mcl$Sex=="Female",]

  mtf <- true_m - counter_m - m_unc
  ftm <- true_f - counter_f - f_unc

  tab <- rbind(c(counter_m, mtf, m_unc, true_m), 
             c(ftm, counter_f, f_unc, true_f))
  colnames(tab) <- c("Pred Males", "Pred Females", "Unclassified", "Total")
  rownames(tab) <- c("True Males", "True Females")

  write.csv(tab, paste0("tab_", method[k], "_classifier_3.csv"))

  mtf <- true_m - counter_m #- m_unc
  ftm <- true_f - counter_f #- f_unc

  res[,3] <- c( (counter_m + counter_f)/nrow(true) * 100, 
              counter_m/true_m * 100, counter_f/true_f * 100,
              m_unc/true_m * 100, f_unc/true_f * 100,
              counter_f/(counter_f + ftm),
              counter_m /(counter_m + mtf))

  # Save the results for all three classifier of the chosen method
  write.csv(res, paste0("accuracy_", method[k], "_sexing.csv"))

}

