library(RCurl)
library(foreign)

#Loading data gfrom github
#Instructions from: http://www.r-bloggers.com/data-on-github-the-easy-way-to-make-your-data-available/
#Reports pulled from : https://github.com/muc-fluechtlingsrat/bamf-asylgeschaeftsstatistik/tree/master/report
#url <- "https://raw.githubusercontent.com/muc-fluechtlingsrat/bamf-asylgeschaeftsstatistik/master/report/201604.csv"
#url <- "https://raw.githubusercontent.com/muc-fluechtlingsrat/bamf-asylgeschaeftsstatistik/master/report/201603.csv"
#url <- "https://raw.githubusercontent.com/muc-fluechtlingsrat/bamf-asylgeschaeftsstatistik/master/report/201602.csv"
url <- "https://raw.githubusercontent.com/muc-fluechtlingsrat/bamf-asylgeschaeftsstatistik/master/report/201601.csv"

graphTitle <- "Protection Rates for Top 10 Countries (2016-01)"

data <- getURL(url)                
data <- read.csv(textConnection(data, encoding = "UTF-8"), header = F, encoding = "UTF-8", na.strings = "-")
str(data)

#Removing the unnecessary columns
data <-  data[,-c(1,3,4,5,6,8,9,10,11)]
#adding column names
colnames(data) <- c("Country","Positive","Negative","Undecided")

#Converting NA to 0
data[is.na(data)] <- 0

#Calculating new fields
data$protectionRate <- data$Positive / (data$Positive + data$Negative + data$Undecided) * 100
data$cleanProtectionRate <- (data$Positive / (data$Positive + data$Negative)) * 100 - data$protectionRate

#Removing the sum of top ten and the total counts before width calc


#Generating barplot
#widthData <- data[(data$Country != "Summe Top Ten"),]
#widthData <- widthData[(widthData$Country != "Herkunftsländer gesamt"),]
width <- (data$Positive / sum(data[,2:4]))^(1/4)
#Adding additional widths for last two sums we ignored
#width <-  c(width, .9, .9)

#Finding the order of the indices

#Setting order of Country
#data$Country <- factor(data$Country, levels = rev(c(as.vector(widthData$Country[order(widthData$Country)]), "Summe Top Ten","Herkunftsländer gesamt")))
require(reshape2)
plotData <- melt(data, measure.vars = c("protectionRate", "cleanProtectionRate"))

require(ggplot2)
ggplot(plotData, aes(x = Country, y = value, fill = variable)) + 
  geom_bar(stat = "identity", 
           width = width) + 
  geom_text(aes(x = Country, y = value, ymax = value, label=Positive),
            position = position_fill(),
            hjust = -.1,
            check_overlap = T)+
  scale_fill_manual(labels = c("Unadjusted", "Adjusted"),
                    name = "Protection Rate",
                    values = c(protectionRate = "#a6bddb", cleanProtectionRate = "#1c9099")) +
  ggtitle(graphTitle) +
  ylab("Percent of Applicants Protected") +
  xlab("") +
  labs(caption = "Width of bars adjusted for proportion of contribution to total Sum") +
  theme(plot.title = element_text(family = "calibri", color="#666666", face="bold", size=20, hjust=0)) +
  theme(axis.title = element_text(family = "calibri", color="#666666", face="bold", size=15)) +
  coord_flip()

