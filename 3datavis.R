## ----results='asis',warning=FALSE,echo=FALSE-----------------------------
library(knitr)
library(rgl)
knit_hooks$set(rgl=hook_rgl)
knit_hooks$set(webgl = hook_webgl)
cat('<script type="text/javascript">', readLines(system.file('WebGL', 
    'CanvasMatrix.js', package = 'rgl')), '</script>', sep = '\n')
hook_plot = knit_hooks$get('plot')
knit_hooks$set(plot = function(x, options) paste('\n', hook_plot(x, options), sep = ''))

## ------------------------------------------------------------------------
senicData <- read.table("C:/Users/Larry/OneDrive/Documents/GitHub/Test02/SENIC.csv", header=TRUE, row.names=1, sep=",",
                        colClasses=c(rep("numeric",7),rep("factor",2),
                        rep("numeric",9), rep("factor",2)))
senicData <- senicData[,-ncol(senicData)]

## ------------------------------------------------------------------------
load("C:/Users/Larry/OneDrive/Documents/GitHub/Test02/lendingData.rda")

## ------------------------------------------------------------------------
hist(senicData$Length_stay)

## ------------------------------------------------------------------------
hist(senicData$Length_stay, col="red",border="blue", 
     xlab="Average Length of Stay", main="Histogram of Average Length of Stay") 

## ----eval=FALSE----------------------------------------------------------
## dev.off()

## ----message=FALSE, results="hide"---------------------------------------
pdf("lengthHist.pdf")
hist(senicData$Length_stay, col="red",border="blue", 
     xlab="Average Length of Stay", main="Histogram of Average Length of Stay") 
dev.off()

## ----message=FALSE, results="hide"---------------------------------------
png("lengthHist.png")
hist(senicData$Length_stay, col="red",border="blue", 
     xlab="Average Length of Stay", main="Histogram of Average Length of Stay") 
dev.off()

## ------------------------------------------------------------------------
plot(ecdf(senicData$Length_stay[senicData$Region_Name == "NorthCentral"]), 
     verticals=TRUE, main="ECDF of Length of Stay")
lines(ecdf(senicData$Length_stay[senicData$Region_Name == "NorthEast"]), col=2, 
      verticals=TRUE)
lines(ecdf(senicData$Length_stay[senicData$Region_Name == "South"]), col=3, 
      verticals=TRUE)
lines(ecdf(senicData$Length_stay[senicData$Region_Name == "West"]), col=4, 
      verticals=TRUE)
legend("bottomright", c("NorthCentral", "NorthEast", "South", "West"), 
       col=c(1,2,3,4), pch=1)

## ------------------------------------------------------------------------
regionCounts <- table(senicData$Region_Name)
barplot(regionCounts)

## ------------------------------------------------------------------------
barplot(regionCounts, xlab="Region", ylab="Frequency", 
        col=rainbow(unique(senicData$Region_Name)), main="Bar Plot of Region")

## ------------------------------------------------------------------------
boxplot(senicData$Length_stay ~ senicData$Region_Name)

## ------------------------------------------------------------------------
boxplot(senicData$Length_stay ~ senicData$Region_Name, xlab="Region", 
        ylab="Average Length of Stay", main = "Boxplot of Average 
        Length of Stay by Region", col=3, outline=FALSE)

## ------------------------------------------------------------------------
medschoolRegionTable <- table(senicData$Region_Name, senicData$Medical_School)
mosaicplot(medschoolRegionTable, color=c(1:2), xlab="Region",
           ylab="Medical School",main="")

## ------------------------------------------------------------------------
plot(senicData$Age_years, senicData$Length_stay)

## ------------------------------------------------------------------------
par(bg="lightgray")
plot(senicData$Age_years, senicData$Length_stay, xlab="Average Age", 
     ylab="Average Length of Stay", col="blue", pch=16, 
     main="Plot of Average Length of Stay versus Average Age")

## ------------------------------------------------------------------------
plot(senicData$Age_years, senicData$Length_stay, type="n", xlab="Average Age", 
     ylab="Average Length of Stay", main="Plot of Average Length of Stay 
     versus Average Age")
points(senicData$Age_years[senicData$Medical_School=="Yes"], 
       senicData$Length_stay[senicData$Medical_School == "Yes"], col=2, pch=2)
points(senicData$Age_years[senicData$Medical_School=="No"], 
       senicData$Length_stay[senicData$Medical_School == "No"], col=3, pch=3)

## ----eval=FALSE----------------------------------------------------------
## windows()

## ----echo=FALSE----------------------------------------------------------
x11()

## ------------------------------------------------------------------------
plot(senicData$Age_years, senicData$Length_stay, type="n", xlab="Average Age", 
     ylab="Average Length of Stay", main="Plot of Average Length of Stay 
           versus Average Age")
points(senicData$Age_years[senicData$Medical_School=="Yes"], 
       senicData$Length_stay[senicData$Medical_School == "Yes"], col=2, pch=2)
points(senicData$Age_years[senicData$Medical_School=="No"], 
       senicData$Length_stay[senicData$Medical_School == "No"], col=3, pch=3)
legend("topleft", title="Med School Affiliation?", legend=c("Yes", "No"), 
       col=c(2,3), pch=c(2,3)) 

## ----eval=FALSE----------------------------------------------------------
## install.packages("rgl")

## ------------------------------------------------------------------------
library(rgl)

## ----rgl=TRUE------------------------------------------------------------
plot3d(senicData$Length_stay, senicData$Age_years, senicData$Infection_pct, 
       col=as.numeric(senicData$Region_Name)+1, type="s", 
       xlab="Average Length of Stay",ylab="Average Age", 
       zlab="Average Prob. of Infection")
legend3d("topright", levels(senicData$Region_Name), 
         col=1:length(levels(senicData$Region_Name))+1, pch=16)

## ------------------------------------------------------------------------
hist(senicData$Culture_ratio, col="green", xlab="Culture Ratio", 
     ylab="Frequency", main="Histogram of Culture Ratio")

## ----eval=FALSE----------------------------------------------------------
## ?boxplot

## ------------------------------------------------------------------------
boxplot(senicData$Infection_pct ~ senicData$Medical_School, 
        xlab="Medical School?", ylab="Infection percentage", main="", col=4)

## ------------------------------------------------------------------------
homeownerTermTable <- table(lendingData[,c("home_ownership", "term")])
mosaicplot(homeownerTermTable, color=rainbow(3), xlab="home ownership status", 
           ylab="term", main="", las=2)

## ------------------------------------------------------------------------
library(dplyr)
lendingData$issue_d <- as.POSIXct(lendingData$issue_d)
loansByMonth <- lendingData %>%
                 select(loan_amnt, issue_d) %>% 
                 group_by(issue_d) %>%
                 arrange(issue_d) %>%
                 summarise(mean(loan_amnt))

plot(loansByMonth, type="l", col="red", xlab="Month", ylab="Total Loans")

## ------------------------------------------------------------------------
boxplot(lendingData$funded_amnt ~ 
        lendingData$home_ownership, xlab="Home Ownership Status",
        ylab="Funded Amount", main="", col=5)

## ----eval=FALSE----------------------------------------------------------
## install.packages("ggplot2")

## ----results="hide", warning=FALSE---------------------------------------
library(ggplot2)  
library(reshape2) 
fundedData <- lendingData[, c("funded_amnt", "home_ownership", "term")]

## ------------------------------------------------------------------------
fundedMeltedData <- melt(fundedData)

## ----eval=FALSE----------------------------------------------------------
## dev.off()

## ------------------------------------------------------------------------
myBoxPlot <- ggplot(fundedMeltedData, aes(x=home_ownership, y=value), group=term) + 
             geom_boxplot(aes(fill=term))
print(myBoxPlot)

## ------------------------------------------------------------------------
ggsave(myBoxPlot, file="fundedHomeTerm.jpg", width=10, height=5)

## ----echo=FALSE, message=FALSE, warning=FALSE, results="hide"------------
purl("3datavis.Rmd")

