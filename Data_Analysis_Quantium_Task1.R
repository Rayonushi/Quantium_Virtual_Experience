install.packages("data.table")
#### Loading required libraries
library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)

#### Loading chips transaction data
transaction_data <- read.csv("R/transaction_data.csv", header=TRUE)
head(transaction_data)

#### Loading Customer Purchase data
customer_data = read.csv("R/PurchaseBehaviour.csv", header=TRUE)

#### Exploratory Data Analysis

##Examine transaction data
str(transaction_data)

##Convert DATE column to a date format
transaction_data$DATE <- as.Date(transaction_data$DATE, origin = "1899-12-30")
head(transaction_data$DATE)
head(transaction_data)

##Examine PROD_NAME
summary(transaction_data$PROD_NAME)

##Examine the words in PROD_NAME to see if there are any incorrect entries 
##such as products that are not chips
productWords <- data.table(unlist(strsplit(unique(transaction_data[, "PROD_NAME"]), " ")))
setnames(productWords, 'words')
head(productWords, 15)

##Removing digits
productWords[, SPECIAL := grepl("[[:digit:]]", words)]
productWords <- productWords[SPECIAL == FALSE,] [,SPECIAL := NULL]

head(productWords,15)
##Removing Special Characters
##Removing punctuation
productWords[,SPECIAL := grepl("[[:punct:]]", words)]
productWords <- productWords[SPECIAL == FALSE,] [,SPECIAL := NULL]

##changing empty strings to NA
productWords[words == ""] <- NA

head(productWords, 15)
##removing all empty cells 
productWords <- productWords[complete.cases(productWords),]
head(productWords,15)

##creating a frequency table for out set of words, sorted

productWords <- data.frame(sort(table(productWords), decreasing = TRUE))
head(productWords)
##Remove salsa products
transaction_data <- data.table(transaction_data)
transaction_data[, SALSA := grepl("salsa", tolower(PROD_NAME))]
transaction_data <- transaction_data[SALSA == FALSE,][, SALSA := NULL]

##Summarise data to check for nulls and possible outliers
summary(transaction_data)

sum(is.na(transaction_data))

##Filter the dataset to find the outlier
outlier <- transaction_data[PROD_QTY == 200,]
print(outlier)

##Filter out the customers based on the loyalty card number
outlierTransactions <- transaction_data[LYLTY_CARD_NBR == 226000,]
outlierTransactions

#Filter out the customers based on the loyalty card number
transaction_data <- transaction_data[LYLTY_CARD_NBR != 226000,]
##Re-examine transaction data
summary(transaction_data)

##Count the number of transactions by data
transaction_data[, .N, by= DATE]

###Create a sequence of dates and join this the count of transactions by date
allDates <- data.table(seq(as.Date("2018/07/01"), as.Date("2019/06/30"), by = "day"))
setnames(allDates, "DATE")
transactions_by_data <- merge(allDates, transaction_data[, .N, by=DATE], all.x=TRUE)

##Setting plot themes to format graphs
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust =0.5))

##Plot transactions over time
ggplot(transactions_by_data, aes(x=DATE, y=N))+
  geom_line()+
  labs(x = "Day", y="Number of transactions", title="transaction over time")+
  scale_x_date(breaks = "1 month")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


## Filter to December and look at individual days
ggplot(transactions_by_data[month(DATE) == 12, ], aes(x = DATE, y = N)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions overtime") +
  scale_x_date(breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

###Pack_size
## We can work this out by taking the digits that are in the PROD_NAME
transaction_data[, PACK_SIZE := parse_number(PROD_NAME)]

###Always check your output
transaction_data[, .N, PACK_SIZE][order(PACK_SIZE)]


#### Let's check the output of the first few rows to see if we have indeed ↪ picked out pack ####size.
transaction_data

#let’s plot the a histogram of pack size since we know that it is a categorical variable not a 
##continuous variable even though it is numeric.
hist(transaction_data[, PACK_SIZE])

###Brands
transaction_data[, BRAND := toupper(substr(PROD_NAME, 1, regexpr(patter=' ', PROD_NAME) - 1))]

## Checking brands
transaction_data[, .N, by = BRAND][order(-N)]

###Cleaning brand names
transaction_data[BRAND == "RED", BRAND := "RRD"]
transaction_data[BRAND == "SNBTS", BRAND := "SUNBITES"]
transaction_data[BRAND == "INFSNS", BRAND := "INFUZIONS"]
transaction_data[BRAND == "WW", BRAND := "WOOLWORTHS"]
transaction_data[BRAND == "SMITH", BRAND := "SMITHS"]
transaction_data[BRAND == "NCC", BRAND := "NATURAL"]
transaction_data[BRAND == "DORITO", BRAND := "DORITOS"]
transaction_data[BRAND == "GRAIN", BRAND := "GRNWVES"]

##Check again
transaction_data[, .N, by = BRAND][order(BRAND)]
########################################################################

##Examing Customer data
str(customer_data)

##Some basic summaries of dataset
summary(customer_data)

library(data.table)
#customer_data <- as.data.table(customer_data) # Convert data.frame to data.table if necessary

#Examining the values of lifestage and premium customers
customer_data[, .N, by = LIFESTAGE][order(-N)]

customer_data[, .N, by = PREMIUM_CUSTOMER][order(-N)]


##merging transaction data to customer data
data <- merge(transaction_data, customer_data, all.x = TRUE)

head(data)

#checking for nulls
data[is.null(LIFESTAGE), .N]

data[is.null(PREMIUM_CUSTOMER), .N]

##################################################################

###Data analysis on Customer Segments

##Total Sales by LIFESTAGE and PREMIUM_CUSTOMER
sales <- data[, .(SALES = sum(TOT_SALES)), .(LIFESTAGE, PREMIUM_CUSTOMER)]

##Create plot

p <- ggplot(data = sales)+
  geom_mosaic(aes(weight = SALES, x = product(PREMIUM_CUSTOMER, LIFESTAGE),
                  fill = PREMIUM_CUSTOMER))+
  labs(x = "Lifestage", y="Premium customer flag", title="Proportion of sales")+
  theme(axis.text.x = element_text(angle= 90, vjust = 0.5))

## Plot and label with proportion of sales
p + geom_text(data = ggplot_build(p)$data[[1]], aes(x = (xmin + xmax)/2 , y =
                                                      (ymin + ymax)/2, label = as.character(paste(round(.wt/sum(.wt),3)*100,
                                                                                                  '%'))))

#### Number of customers by LIFESTAGE and PREMIUM_CUSTOMER
customer <- data[, .(CUSTOMERS = uniqueN(LYLTY_CARD_NBR)), .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-CUSTOMERS)]

#### Create plot
p = ggplot(data = customer) +
  geom_mosaic(aes(weight = CUSTOMERS, x = product(PREMIUM_CUSTOMER, LIFESTAGE), fill = PREMIUM_CUSTOMER)) +
  labs(x = "Lifestage", y = "Premium customer flag", title = "Proportion of customers") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#### Plot and label with proportion of customers
p + geom_text(data = ggplot_build(p)$data[[1]], aes(x = (xmin + xmax)/2 , y =
                                                      (ymin + ymax)/2, label = as.character(paste(round(.wt/sum(.wt),3)*100,
                                                                                                  '%'))))

#### Average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMER
avg_units = data[, .(AVG = sum(PROD_QTY)/uniqueN(LYLTY_CARD_NBR)), .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-AVG)]

#### Create plot

ggplot(data = avg_units, aes(weight = AVG, x = LIFESTAGE, fill = PREMIUM_CUSTOMER)) +
  geom_bar(position = position_dodge()) +
  labs(x = "Lifestage", y = "Avg units per transaction", title = "Units per customer") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


#### Average price per unit by LIFESTAGE and PREMIUM_CUSTOMER
avg_price = data[, .(AVG = sum(TOT_SALES)/sum(PROD_QTY)), .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-AVG)]
#### Create plot
ggplot(data = avg_price, aes(weight = AVG, x = LIFESTAGE, fill = PREMIUM_CUSTOMER)) +
  geom_bar(position = position_dodge()) +
  labs(x = "Lifestage", y = "Avg price per unit", title = "Price per unit") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#### Perform an independent t‐test between mainstream vs premium and budget midage and
#### young singles and couples
pricePerUnit = data[, price := TOT_SALES/PROD_QTY]
t.test(data[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES") & PREMIUM_CUSTOMER == "Mainstream", price]
       , data[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES") & PREMIUM_CUSTOMER != "Mainstream", price]
       , alternative = "greater")
####################################################################################################################################

#### Deep dive into Mainstream, young singles/couples
segment1 <- data[LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream",]
other <- data[!(LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream"),]

#### Brand affinity compared to the rest of the population
quantity_segment1 <- segment1[, sum(PROD_QTY)]
quantity_other <- other[, sum(PROD_QTY)]
quantity_segment1_by_brand <- segment1[, .(targetSegment = sum(PROD_QTY)/quantity_segment1), by = BRAND]
quantity_other_by_brand <- other[, .(other = sum(PROD_QTY)/quantity_other), by = BRAND]
brand_proportions <- merge(quantity_segment1_by_brand, quantity_other_by_brand)[, affinityToBrand := targetSegment/other]
brand_proportions[order(-affinityToBrand)]


#### Preferred pack size compared to the rest of the population
quantity_segment1_by_pack <- segment1[, .(targetSegment = sum(PROD_QTY)/quantity_segment1), by = PACK_SIZE]
quantity_other_by_pack <- other[, .(other = sum(PROD_QTY)/quantity_other), by = PACK_SIZE]
pack_proportions <- merge(quantity_segment1_by_pack, quantity_other_by_pack)[, affinityToPack := targetSegment/other]
pack_proportions[order(-affinityToPack)]

data[PACK_SIZE == 270, unique(PROD_NAME)]

