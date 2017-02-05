#imports metric_functions
source('metric_functions.R')
library(ggplot2)

##reads all the fileNames in prepped folder
files <- list.files('./data/prepped', full=TRUE)

#reads all the data in the folder
race.data <- lapply (files, read.csv, header = TRUE)

dissimilarities <- lapply(race.data, dissimilarity)
isolations <- lapply(race.data, isolation)
correlations <- lapply(race.data, correlation)
#gets name of the cities from the file name
cityNames <- gsub("_race.csv", '', basename(files))

#dataframe of name of city, dissimilarities, interactions, isolations rate
df <- data.frame('City' = unlist(cityNames), 'Dissimilarity' = unlist(dissimilarities),
                 'Isolation' = unlist(isolations),
                 'Correlation' = unlist(correlations))


#Dissimilarity
dMin <- min(df$Dissimilarity)
dMax <- max(df$Dissimilarity)
dMed <- median((df$Dissimilarity))
dAvg <- sum(df$Dissimilarity) / nrow(df)
dMinCit <- df$City[df$Dissimilarity == dMin]
dMaxCit <- df$City[df$Dissimilarity == dMax]

#Isolations
isMin <- min(df$Isolation)
isMax <- max(df$Isolation)
isAvg <- sum(df$Isolation) / nrow(df)
isMed <- median(df$Isolation)
isMinCit <- df$City[df$Isolation == isMin]
isMaxCit <- df$City[df$Isolation == isMax]

#Correlations
coMin <- min(df$Correlation)
coMax <- max(df$Correlation)
coMed <- median(df$Correlation)
coAvg <- sum(df$Correlation) / nrow(df)
coMinCit <- df$City[df$Correlation == coMin]
coMaxCit <- df$City[df$Correlation == coMax]

Min <- c(dMin, isMin, coMin)
Avg <- c(dAvg, isAvg, coAvg)
Med <- c(dMed, isMed, coMed)
Max <- c(dMax, isMax, coMax)
MinCit <- c(dMinCit, isMinCit, coMinCit)
MinCit <- lapply(MinCit, function(city) {
    city <- cityNames[city]
})
MaxCit <- c(dMaxCit, isMaxCit, coMaxCit)
MaxCit <- lapply(MaxCit, function(city) {
  city <- cityNames[city]
})
agg.dataframe <- data.frame('MinIndex' = unlist(Min), 'MedIndex' = unlist(Med), 'AvgIndex' = unlist(Avg), 'MaxIndex' = unlist(Max),
                                'MinCity' = unlist(MinCit), 'MaxCity' = unlist(MaxCit))
rownames(agg.dataframe) <- c("Dissimilarity", ' Isolation', 'Correlation')


DvsI <- ggplot(data = df) +
  geom_point(mapping = aes(x = df$Dissimilarity, y = df$Isolation)) +
  geom_smooth(mapping = aes(x = df$Dissimilarity, y = df$Isolation), method = "lm", se = FALSE) +
  ggtitle('Dissimilarity vs Isolation') +
  xlab('Dissimilarity') + ylab('Isolation')


DvsCo <- ggplot(data = df) +
  geom_point(mapping = aes(x = df$Dissimilarity, y = df$Correlation)) +
  geom_smooth(mapping = aes(x = df$Dissimilarity, y = df$Correlation), method = "lm", se = FALSE) +
  ggtitle('Dissimilarity vs Correlation') +
  xlab('Dissimilarity') + ylab('Correlation')


IvsCo <- ggplot(data = df) +
  geom_point(mapping = aes(x = df$Isolation, y = df$Correlation)) +
  geom_smooth(mapping = aes(x = df$Isolation, y = df$Correlation), method = "lm", se = FALSE) +
  ggtitle('Isolation vs Correlation') +
  xlab('Isolation') + ylab('Correlation')

boxG <- boxData <- data.frame(MEASURE = factor(rep(c('Dissimilarity','Isolation', 'Correlation'))), INDEX = c(df$Dissimilarity, df$Isolation, df$Correlation) )
boxG <- ggplot(boxData, aes(x = MEASURE, y = INDEX)) + geom_boxplot()
