# Loading the libraries

library(readr)

## Data prep and cleaning
predictions = read.csv2("sorted.csv", sep=",", na.string=c("nan", "NA"))
true = read.csv2("true_pairs.csv", sep=",", na.strings=c("","NA"))
true = true[,1:4]

## Make the predicted three datasets (left, right, pairs)
spL = predictions[-which(is.na(predictions[,1])),]
single_pred_L = spL[which(is.na(spL[,2])),1]


spR = predictions[-which(is.na(predictions[,2])),]
single_pred_R = spR[which(is.na(spR[,1])),2]

remove(spL, spR)

pairs_pred = predictions[-which(is.na(predictions[,1])), ]
pairs_pred = pairs_pred[-which(is.na(pairs_pred[,2])), ]
p_pred = paste(pairs_pred[,1], pairs_pred[,2], sep="-")


## Same for the true pairs
stL = true[-which(is.na(true[,3])),]
single_true_L = stL[which(is.na(stL[,4])),3]

stR = true[-which(is.na(true[,4])),]
single_true_R = stR[which(is.na(stR[,3])),4]

remove(stL, stR)

pairs_true = true[-which(is.na(true[,3])), ]
pairs_true = pairs_true[-which(is.na(pairs_true[,4])), ]
p_true = paste(pairs_true[,3], pairs_true[,4], sep="-")


## Counter for single left elements
counter_singles_L = 0

for (i in 1:length(single_pred_L))
{ if (single_pred_L[i] %in% single_true_L)
  { counter_singles_L = counter_singles_L + 1}
  print(i)
}


## Counter for single right elements
counter_singles_R = 0

for (i in 1:length(single_pred_R))
{ if (single_pred_R[i] %in% single_true_R)
{ counter_singles_R = counter_singles_R + 1}
  print(i)
}

## Counter for true pairs 
counter = 0
for (i in 1:length(p_pred))
{ if (p_pred[i] %in% p_true)
{ counter = counter + 1
  print(i)
  print(p_pred[i])}
}

## Check the score of the unsorted true pairs?
unsorted = read.csv2("unsorted.csv", sep=",", header=FALSE)
p_plaus = paste(unsorted[,1], unsorted[,2], sep="-")

## Counter for plausible pairs that ARE true 
counter_plaus = 0
score = 0
for (i in 1:length(p_plaus))
{ if (p_plaus[i] %in% p_true)
{ counter_plaus = counter_plaus + 1
  score[i] = unsorted[i,3]
  print(p_plaus[i])}
}


## Summary
res <- matrix(, nrow=3, ncol=4)
colnames(res) <- c("Left Single", "Right Single", "True Pairs", "Plausible Pairs")
rownames(res) <- c("N", "# of elements", "% accuracy")

res[1,] <- c(counter_singles_L, counter_singles_R, counter, counter_plaus)
res[2,] <- c(length(single_true_L), length(single_true_R), length(p_true), 
             length(p_plaus))
res[3,] <- (res[1,]/res[2,]) * 100
write.csv(res, "accuracy_sorting.csv")
