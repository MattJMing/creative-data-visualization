# Code to visualize the most common letters in the most common words of the
# English language

# We will get a plot which shows which letters are the most common first letter,
# second letter, third letter, etc., in the list of the 10,000 most common
# words in English, as well as draw lines showing frequency of moving from one
# letter in one position to the next

# For example:
# 1) see how frequently "A" is the first letter in a word
# 2) see how frequently "A" is followed by "A","B","C", etc.

# Initialize all necessary packages
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(data.table)

# First, load in data set
fi <- "https://raw.githubusercontent.com/difiore/ada-2022-datasets/main/google-10000-english-usa-no-swears.txt"
d <- read_csv(fi,col_names = TRUE)
words <- d$words

# The 1821st most common word in English is "NA" which is recognized by R as NA
# rather than as a character string, so I need to convert this entry to the
# character string "NA"
words[1821] <- "NA"

# Figure out the longest word in the list
max(nchar(words)) # Longest words is "TELECOMMUNICATIONS" with 18 chars

# Create a dataframe to store every letter of every position of every word

# Empty positions (i.e., positions after the length of the words, such as
# positions 5-18 in the word "boat") are stored as NA
l <- data.frame(matrix(NA,nrow = 9884,ncol = 18))

# Iterate through all words in list, putting each letter into its respective
# column in dataframe (e.g., 1st letter in first column, etc.)
for(i in 1:length(words)){
  s <- strsplit(words[i],"")[[1]]
  l[i,1:length(s)] <- s
}

# Now, we need to get a data frame which will store the frequency of each letter
# at each position in each word
f <- data.frame(letter = rep(NA,26*18),position = rep(NA,26*18),count = rep(NA,26*18))
r <- 1
for(i in LETTERS){
  for(j in 1:18){
    f[r,1] <- i
    f[r,2] <- j
    f[r,3] <- length(which(l[[j]] == i))
    r = r+1
  }
}

# Converting the data to factors
f$position <- factor(f$position)
f$letter <- factor(f$letter,levels = rev(LETTERS))

# Now, getting a dataframe to store the number of connections from one letter
# to the next for each position (e.g., for position 1, how often is A followed
# # by B, how often A followed by C, etc.)
# f2 <- data.frame(let1 = rep(rep(LETTERS,each = 26),17),
#                  let2 = rep(rep(LETTERS,26),17),
#                  pos1 = rep(NA,26*26*17),
#                  count = rep(NA,26*26*17))
# r <- 1
# for(p in 1:17){
#   for(i in LETTERS){
#     for(j in LETTERS){
#       f2[r,1] <- i
#       f2[r,2] <- j
#       f2[r,3] <- p
#       f2[r,4] <- length(which(l[[p]] == i & l[[p+1]] == j))
#       r <- r+1
#     }
#   }
# }
# f2$let1 <- factor(f2$let1,levels = rev(LETTERS))
# f2$let2 <- factor(f2$let2,levels = rev(LETTERS))
# f2$pos1 <- factor(f2$pos1)

l2 <- rbind(1:18,l)
l2 <- data.frame(t(l2))
l2$X1 <- as.numeric(l2$X1)
l2$X1 <- factor(l2$X1)
l2 <- melt(as.data.table(l2),"X1")
l2$value <- factor(l2$value,levels = rev(LETTERS))
l2$variable <- factor(l2$variable,levels = rev(levels(l2$variable)))
l2$variable <- as.numeric(l2$variable)
l2 <- na.omit(l2)

# Plotting the frequency of all letters, including adding in lines showing the
# transitions between letters
p <- ggplot(data = f,aes(x = position,y = letter)) +
  geom_line(data = l2,aes(x = X1,y = value,group = variable,color = variable),
            alpha = 0.01) +
  geom_text(aes(size = count,label = rep(LETTERS,each = 18))) +
  scale_size_continuous(range = c(0,10)) +
  scale_color_gradient(low = "blue",high = "red") +
  ggtitle("Most Common Letters and their Positions in the 10,000 Most Common English Words") +
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.text.y.left = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x.bottom = element_line("black"),
        plot.title = element_text(hjust = 0.5))

ggsave("creative-data-visualization.jpg",p,device = "jpeg",width = 10)

