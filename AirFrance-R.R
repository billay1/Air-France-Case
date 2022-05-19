########## SETTING UP R PACKAGES ##########

# Importing required packages
library(readxl)
library(tidyr)
library(dplyr)
library(tidyverse)
library(ROCR)
library(ggplot2)
library(caret)
library(gdtools)
library(sqldf)
library(dplyr)
library(tidyverse)
library(readxl)
library(skimr) 
library(kableExtra) 
library(here)
library(pivottabler)
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)
library(tm)
library(SnowballC)
library(topicmodels)
library(plotly)


########## UNDERSTANDING THE DATA ##########

# Importing the data set to R
library(readxl)
Air_France <- read_excel("C:/Users/Probook G3/Desktop/University/Hult Int Business School/R Statistics/Datasets/Air France Case Spreadsheet Supplement.xls", 
                                                     sheet = "DoubleClick")

# Visualizing the data
View(Air_France)


########## MASSAGING THE DATA ##########

# DEFINING THE BINARY VARIABLE (BUSINESS SUCCESS)

# RENAMING THE COLUMNS

names(Air_France)[names(Air_France) == "Publisher ID"] <- "publisher_id"
names(Air_France)[names(Air_France) == "Publisher Name"] <- "publisher_name"
names(Air_France)[names(Air_France) == "Keyword ID"] <- "keyword_id"
names(Air_France)[names(Air_France) == "Keyword"] <- "keyword"
names(Air_France)[names(Air_France) == "Match Type"] <- "match_type"
names(Air_France)[names(Air_France) == "Campaign"] <- "campaign"
names(Air_France)[names(Air_France) == "Keyword Group"] <- "keyword_group"
names(Air_France)[names(Air_France) == "Category"] <- "category"
names(Air_France)[names(Air_France) == "Bid Strategy"] <- "bid_strategy"
names(Air_France)[names(Air_France) == "Keyword Type"] <- "keyword_type"
names(Air_France)[names(Air_France) == "Status"] <- "status"
names(Air_France)[names(Air_France) == "Search Engine Bid"] <- "se_bid"
names(Air_France)[names(Air_France) == "Clicks"] <- "clicks"
names(Air_France)[names(Air_France) == "Click Charges"] <- "click_charges"
names(Air_France)[names(Air_France) == "Avg. Cost per Click"] <- "avg_cpc"
names(Air_France)[names(Air_France) == "Impressions"] <- "impressions"
names(Air_France)[names(Air_France) == "Engine Click Thru %"] <- "click_thru_pct"
names(Air_France)[names(Air_France) == "Avg. Pos."] <- "avg_position"
names(Air_France)[names(Air_France) == "Trans. Conv. %"] <- "trans_conv_pct"
names(Air_France)[names(Air_France) == "Total Cost/ Trans."] <- "total_cpt"
names(Air_France)[names(Air_France) == "Amount"] <- "amount"
names(Air_France)[names(Air_France) == "Total Cost"] <- "total_cost"
names(Air_France)[names(Air_France) == "Total Volume of Bookings"] <- "booking_vol"


# FIXING BID STRATEGY

# Combining both 1-4 bid positions
Air_France$bid_strategy_new <- (gsub("Postiion 1-4 Bid Strategy", "Position 1-4 Bid Strategy", 
                                 Air_France$bid_strategy))

# Combining both 1-2 bid positions
Air_France$bid_strategy_new <- gsub("Position 1 -2 Target", "Position 1-2 Target", 
                                    Air_France$bid_strategy_new)

# Replacing NAs with Any Position
Air_France$bid_strategy_new <- replace_na(Air_France$bid_strategy_new, "Any Position")


# FIXING THE MATCH TYPE

# Using the gsub function to replace N/A with Other
Air_France$match_type <- gsub("N/A", "Other", Air_France$match_type)


# CALCULATING THE PROFIT
Air_France$profit <-  Air_France$amount - Air_France$total_cost


# DEFINING OUR BUSINESS SUCCESS: HAS A POSITIVE ROA

# Calculating the ROA
Air_France$ROA <-  Air_France$profit / Air_France$total_cost

# Defining the business success as a binary variable
Air_France$ROA_binary <- ifelse(Air_France$ROA > 0, 1, 0)


# COMBINING CAMPAIGNS

# Combining all the Geo Targeted strategies into one group for a global analysis
Air_France$campaign_new <- gsub("Geo Targeted Detroit", "Geo Targeted", Air_France$campaign)
Air_France$campaign_new <- gsub("Geo Targeted Chicago", "Geo Targeted", Air_France$campaign_new)
Air_France$campaign_new <- gsub("Geo Targeted DC", "Geo Targeted", Air_France$campaign_new)
Air_France$campaign_new <- gsub("Geo Targeted New York", "Geo Targeted", Air_France$campaign_new)
Air_France$campaign_new <- gsub("Geo Targeted Boston", "Geo Targeted", Air_France$campaign_new)
Air_France$campaign_new <- gsub("Geo Targeted Seattle", "Geo Targeted", Air_France$campaign_new)
Air_France$campaign_new <- gsub("Geo Targeted Atlanta", "Geo Targeted", Air_France$campaign_new)
Air_France$campaign_new <- gsub("Geo Targeted San Francisco", "Geo Targeted", Air_France$campaign_new)
Air_France$campaign_new <- gsub("Geo Targeted Miami", "Geo Targeted", Air_France$campaign_new)
Air_France$campaign_new <- gsub("Geo Targeted Houston", "Geo Targeted", Air_France$campaign_new)
Air_France$campaign_new <- gsub("Geo Targeted Philadelphia", "Geo Targeted", Air_France$campaign_new)
Air_France$campaign_new <- gsub("Geo Targeted Los Angeles", "Geo Targeted", Air_France$campaign_new)
Air_France$campaign_new <- gsub("Geo Targeted Cincinnati", "Geo Targeted", Air_France$campaign_new)


# CREATING DUMMIES FOR BID STRATEGY
Air_France$bid_1to2 <- ifelse(Air_France$bid_strategy_new == "Position 1-2 Target", 1, 0)
Air_France$bid_1to3 <- ifelse(Air_France$bid_strategy_new == "Position 1- 3", 1, 0)
Air_France$bid_1to4 <- ifelse(Air_France$bid_strategy_new == "Position 1-4 Bid Strategy", 1, 0)
Air_France$bid_2to5 <- ifelse(Air_France$bid_strategy_new == "Position 2-5 Bid Strategy", 1, 0)
Air_France$bid_3to6 <- ifelse(Air_France$bid_strategy_new == "Pos 3-6", 1, 0)
Air_France$bid_5to10 <- ifelse(Air_France$bid_strategy_new == "Position 5-10 Bid Strategy", 1, 0)


# CREATING DUMMIES FOR MATCH TYPE
Air_France$match_adv <- ifelse(Air_France$match_type  == "Advanced", 1, 0)
Air_France$match_brd <- ifelse(Air_France$match_type  == "Broad", 1, 0)
Air_France$match_xct <- ifelse(Air_France$match_type  == "Exact", 1, 0)
Air_France$match_std <- ifelse(Air_France$match_type  == "Standard", 1, 0)


# CREATING DUMMIES FOR PUBLISHER
Air_France$google_us <- ifelse(Air_France$publisher_name  == "Google - US", 1, 0)
Air_France$yahoo_us <- ifelse(Air_France$publisher_name  == "Yahoo - US", 1, 0)
Air_France$google_glb <- ifelse(Air_France$publisher_name  == "Google - Global", 1, 0)
Air_France$overt_glb <- ifelse(Air_France$publisher_name  == "Overture - Global", 1, 0)
Air_France$overt_us <- ifelse(Air_France$publisher_name  == "Overture - US", 1, 0)
Air_France$msn_glb <- ifelse(Air_France$publisher_name  == "MSN - Global", 1, 0)
Air_France$msn_us <- ifelse(Air_France$publisher_name  == "MSN - US", 1, 0)


# SUMMARIZING METRICS BY PUBLISHER AND ADDING KAYAK

# Constructing the pivot table by Publisher and adding Kayak
AF_Publisher <- Air_France %>%
  group_by(publisher_name) %>%
  summarize(sum(clicks),
            sum(click_charges),
            sum(impressions),
            sum(amount),
            sum(total_cost),
            sum(booking_vol))

# Renaming the columns
names(AF_Publisher)[names(AF_Publisher) == "publisher_name"] <- "publisher"
names(AF_Publisher)[names(AF_Publisher) == "sum(clicks)"] <- "clicks"
names(AF_Publisher)[names(AF_Publisher) == "sum(click_charges)"] <- "click_charges"
names(AF_Publisher)[names(AF_Publisher) == "sum(impressions)"] <- "impressions"
names(AF_Publisher)[names(AF_Publisher) == "sum(amount)"] <- "amount"
names(AF_Publisher)[names(AF_Publisher) == "sum(total_cost)"] <- "total_cost"
names(AF_Publisher)[names(AF_Publisher) == "sum(booking_vol)"] <- "booking_vol"

# Importing the Kayak data to R
library(readxl)
Kayak <- read_excel("C:/Users/Probook G3/Desktop/University/Hult Int Business School/R Statistics/Datasets/Air France Case Spreadsheet Supplement.xls", 
                                                     sheet = "Kayak")
View(Kayak)

# Sub-setting the required rows
Kayak <- Kayak[3,]

# Renaming the columns
names(Kayak)[names(Kayak) == "Sponsored Links - Air France"] <- "publisher"
names(Kayak)[names(Kayak) == "...2"] <- "clicks"
names(Kayak)[names(Kayak) == "...3"] <- "click_charges"
names(Kayak)[names(Kayak) == "...4"] <- "booking_vol"
names(Kayak)[names(Kayak) == "...5"] <- "avg_ticket"
names(Kayak)[names(Kayak) == "...6"] <- "amount"
names(Kayak)[names(Kayak) == "...7"] <- "profit"

# Adding a column with value zero as impression, because the information is 
# missing but we require it to keep the structure of the data.
Kayak$impressions = 0

# Transforming the other columns to numerics
Kayak$clicks <- as.numeric(Kayak$clicks)
Kayak$click_charges <- as.numeric(Kayak$click_charges)
Kayak$booking_vol <- as.numeric(Kayak$booking_vol)
Kayak$avg_ticket <- as.numeric(Kayak$avg_ticket)
Kayak$amount <- as.numeric(Kayak$amount)
Kayak$profit <- as.numeric(Kayak$profit)

# Adding the total cost column for kayak to keep the structure of the set
Kayak$total_cost = Kayak$amount - Kayak$profit

# Saving the column order
col_order <- c("publisher", "clicks", "click_charges", "impressions", "amount",
               "total_cost", "booking_vol")

# Ordering the column names
Kayak <- Kayak[ , col_order]

# Combining the two dataframes to have all the pulishers
AF_Publisher <- rbind(AF_Publisher, Kayak)

# Creating new columns for the metrics by publisher
AF_Publisher$profit <- AF_Publisher$amount - AF_Publisher$total_cost
AF_Publisher$ROA <- AF_Publisher$profit / AF_Publisher$total_cost
AF_Publisher$avg_cpc <- AF_Publisher$click_charges / AF_Publisher$clicks
AF_Publisher$click_thru_pct <- AF_Publisher$clicks / AF_Publisher$impressions
AF_Publisher$trans_conv_pct <- AF_Publisher$booking_vol / AF_Publisher$clicks
AF_Publisher$total_cpt <- AF_Publisher$total_cost / AF_Publisher$booking_vol

# Visualizing the new data frame
view(AF_Publisher)


# SUMMARIZING THE METRICS BY CAMPAIGN

# Constructing the pivot table by Campaign
AF_Campaign <- Air_France %>%
  group_by(campaign_new) %>%
  summarize(sum(clicks),
            sum(click_charges),
            sum(impressions),
            sum(amount),
            sum(total_cost),
            sum(booking_vol))

# Renaming the columns
names(AF_Campaign)[names(AF_Campaign) == "campaign_new"] <- "campaign"
names(AF_Campaign)[names(AF_Campaign) == "sum(clicks)"] <- "clicks"
names(AF_Campaign)[names(AF_Campaign) == "sum(click_charges)"] <- "click_charges"
names(AF_Campaign)[names(AF_Campaign) == "sum(impressions)"] <- "impressions"
names(AF_Campaign)[names(AF_Campaign) == "sum(amount)"] <- "amount"
names(AF_Campaign)[names(AF_Campaign) == "sum(total_cost)"] <- "total_cost"
names(AF_Campaign)[names(AF_Campaign) == "sum(booking_vol)"] <- "booking_vol"

# Creating new columns for the metrics by campaign
AF_Campaign$profit <- AF_Campaign$amount - AF_Campaign$total_cost
AF_Campaign$ROA <- AF_Campaign$profit / AF_Campaign$total_cost
AF_Campaign$avg_cpc <- AF_Campaign$click_charges / AF_Campaign$clicks
AF_Campaign$click_thru_pct <- AF_Campaign$clicks / AF_Campaign$impressions
AF_Campaign$trans_conv_pct <- AF_Campaign$booking_vol / AF_Campaign$clicks
AF_Campaign$total_cpt <- AF_Campaign$total_cost / AF_Campaign$booking_vol

# Visualizing the data frame
view(AF_Campaign)


# OBTAINING IMPRESSIONS, CLICKS AND BOOKINGS FOR EACH PUBLISHER
# ALONG WITH CLICK THRU RATE AND CONVERSION RATE

# Creating a new data frame with the metrics by publisher
AF_Publisher_Funnel <- 
  AF_Publisher[ , c("publisher", "impressions", "clicks", "booking_vol")]

# Calculating the click thru rate (percentage of impressions that became clicks)
AF_Publisher_Funnel$click_thru_rate <- 
  round((AF_Publisher_Funnel$clicks / AF_Publisher_Funnel$impressions), digits = 3)

# Calculating the conversion rate (percentage of clicks that turned into bookings)
AF_Publisher_Funnel$convs_rate <- 
  round((AF_Publisher_Funnel$booking_vol / AF_Publisher_Funnel$clicks), digits = 3)

# Visualizing the data frame
view(AF_Publisher_Funnel) # This is the table that accompanies the funnel


# OBTAINING THE TOP 3 PROFITABLE WORDS FOR EACH OF THE PUBLISHERS

# Top 3 keywords that generated the most bookings for Google - US
Top3_Google_US <- sqldf('SELECT publisher_name AS Publisher, keyword AS Keyword, booking_vol AS Bookings
                         FROM Air_France
                         WHERE publisher_name == "Google - US"
                         ORDER BY booking_vol DESC
                         LIMIT 3')

# Top 3 keywords that generated the most bookings for Google - Global
Top3_Google_Global <- sqldf('SELECT publisher_name AS Publisher, keyword AS Keyword, booking_vol AS Bookings
                         FROM Air_France
                         WHERE publisher_name == "Google - Global"
                         ORDER BY booking_vol DESC
                         LIMIT 3')

# Top 3 keywords that generated the most bookings for Yahoo - US
Top3_Yahoo_US <- sqldf('SELECT publisher_name AS Publisher, keyword AS Keyword, booking_vol AS Bookings
                         FROM Air_France
                         WHERE publisher_name == "Yahoo - US"
                         ORDER BY booking_vol DESC
                         LIMIT 3')

# Top 3 keywords that generated the most bookings for MSN - US
Top3_MSN_US <- sqldf('SELECT publisher_name AS Publisher, keyword AS Keyword, booking_vol AS Bookings
                         FROM Air_France
                         WHERE publisher_name == "MSN - US"
                         ORDER BY booking_vol DESC
                         LIMIT 3')

# Top 3 keywords that generated the most bookings for MSN - Global
Top3_MSN_Global <- sqldf('SELECT publisher_name AS Publisher, keyword AS Keyword, booking_vol AS Bookings
                         FROM Air_France
                         WHERE publisher_name == "MSN - Global"
                         ORDER BY booking_vol DESC
                         LIMIT 3')

# Top 3 keywords that generated the most bookings for Overture - US
Top3_Over_US <- sqldf('SELECT publisher_name AS Publisher, keyword AS Keyword, booking_vol AS Bookings
                         FROM Air_France
                         WHERE publisher_name == "Overture - US"
                         ORDER BY booking_vol DESC
                         LIMIT 3')

# Top 3 keywords that generated the most bookings for Overture - Global
Top3_Over_Global <- sqldf('SELECT publisher_name AS Publisher, keyword AS Keyword, booking_vol AS Bookings
                         FROM Air_France
                         WHERE publisher_name == "Overture - Global"
                         ORDER BY booking_vol DESC
                         LIMIT 3')

# Combine the top 3 data for each publisher
Top3_ProfitKeywords <- rbind(Top3_Google_US,
                            Top3_Google_Global,
                            Top3_MSN_US,
                            Top3_MSN_Global,
                            Top3_Over_US,
                            Top3_Over_Global,
                            Top3_Yahoo_US)

# Visualizing the data
view(Top3_ProfitKeywords)


# OBTAINING INFORMATION THROUGH SQL AT THE PUBLISHER LEVEL

# SQL query to get publisher info for Google - US
google_us_pub_info <- sqldf('SELECT impressions, clicks, booking_vol
                          FROM AF_Publisher
                          WHERE publisher LIKE "%Google - US%"')

# SQL query to get publisher info for Google - Global
google_global_pub_info <- sqldf('SELECT impressions, clicks, booking_vol
                          FROM AF_Publisher
                          WHERE publisher LIKE "%Google - Global%"')

# SQL query to get publisher info for Yahoo - US
yahoo_us_pub_info <- sqldf('SELECT impressions, clicks, booking_vol
                          FROM AF_Publisher
                          WHERE publisher LIKE "%Yahoo - US%"')

# SQL query to get publisher info for MSN - US
msn_us_pub_info <- sqldf('SELECT impressions, clicks, booking_vol
                          FROM AF_Publisher
                          WHERE publisher LIKE "%MSN - US%"')


# SQL query to get publisher info for MSN - Global
msn_global_pub_info <- sqldf('SELECT impressions, clicks, booking_vol
                          FROM AF_Publisher
                          WHERE publisher LIKE "%MSN - Global%"')

# SQL query to get publisher info for Overture - US
over_us_pub_info <- sqldf('SELECT impressions, clicks, booking_vol
                          FROM AF_Publisher
                          WHERE publisher LIKE "%Overture - US%"')

# SQL query to get publisher info for Overture - Global
over_global_pub_info <- sqldf('SELECT impressions, clicks, booking_vol
                          FROM AF_Publisher
                          WHERE publisher LIKE "%Overture - Global%"')


########## PREDICTIVE MODELING ##########

##### SAMPLING

# Step 1: Training Index
training_index <- sample(1:nrow(Air_France), size = 0.8*nrow(Air_France))

# Step 2: Splitting into train and test sets
Air_France_train <- Air_France[training_index, ]
Air_France_test <- Air_France[-training_index, ]

##### LOGISTIC REGRESSION

# Step 1: Creating the model with the glm function
my_logit <- glm(ROA_binary ~ bid_1to2 +
                             bid_1to4 +
                             bid_2to5 +
                             bid_5to10 +
                             match_brd +
                             match_xct +
                             google_us +
                             google_glb +
                             overt_us, # y ~ x variables
                data = Air_France_train, # defining the data set
                family = "binomial") # family binomial for logistic

# We decided to create a model to predict whether a keyword will generate ROA,
# taking into account only the variables that we have control over, such as the
# publishers we choose to work with, the bid strategies that are set to reach 
# a certain position in the search engine, and the type of match that each 
# keyword will be assigned when researched.


# Step 2: Reviewing the results with the summary function
summary(my_logit)


# Step 3: Interpreting the coefficients

# Creating a function to ease interpretation of the coefficients
coef <- function(x){
  odds <- exp(x) - 1
  return(odds)
} # closing function

# Interpreting the coefficients 
coef(1.3693) #bid 1 to 2

# Choosing the bid strategy to position the keyword between places 1 and 2,
# increases the odds of it generating ROA by about 293%.

coef(1.0577) #bid 1 to 4

# Choosing the bid strategy to position the keyword between places 1 and 4
# increases the odds of it generating ROA by about 188%.

coef(-1.6249) #bid 2 to 5

# Choosing the bid strategy to position the keyword between places 2 and 5
# decreases the odds of it generating ROA by about 80%.

coef(-2.7289) #bid 5 to 10

# Choosing the bid strategy to position the keyword between places 5 and 10
# decreases the odds of it generating ROA by about 93%.

coef(0.9635) #match_brd

# Choosing broad as the match type for a keyword increases the odds of it 
# generating ROA by about 162%. This seems to be the most cost effective option,
# since the average cost per keyword when choosing this match type is about $181.

coef(2.2525) #match_xct

# Choosing exact as the match type for a keyword increases the odds of it 
# generating ROA by about 851%. However, this is also the most expensive choice, 
# as this match type has an average cost per keyword of about $1,533.

coef(1.3909) #google_us

# Choosing Google US as the publisher increases the odds of a keyword generating
# ROA by about 302%.

coef(0.9356) #google_glb

# Choosing Google Global as the publisher increases the odds of a keyword 
# generating ROA by about 155%.

coef(1.3696) #overture_us

# Choosing Overture US as the publisher increases the odds of a keyword 
# generating ROA by about 293%.

# Step 4: Plotting the Confusion Matrix and AUC
###### Interpret the AUC in a business-friendly manner #########

# A: Training Set 

# Predicting
my_pred_train  <- predict(my_logit, Air_France_train,
                                  type = "response")

# Creating the confusion matrix
conf_train <- confusionMatrix(data = as.factor(as.numeric(my_pred_train > 0.5)), 
                reference = as.factor(as.numeric(Air_France_train$ROA_binary)))

#Visualizing the confusion matrix
print(conf_train)

# Calculating the AUC
pred_val_train <- prediction(my_pred_train, Air_France_train$ROA_binary)
auc_train <- performance(pred_val_train, "tpr", "fpr")
plot(auc_train)


# B: Testing Set 

# Predicting
my_pred_test  <- predict(my_logit, Air_France_test,
                          type = "response")

# Creating the confusion matrix
conf_test <- confusionMatrix(data = as.factor(as.numeric(my_pred_test > 0.5)), 
                reference = as.factor(as.numeric(Air_France_test$ROA_binary)))

#Visualizing the confusion matrix
print(conf_test)

# Calculating the AUC
pred_val_train <- prediction(my_pred_test, Air_France_test$ROA_binary)
auc_test <- performance(pred_val_train, "tpr", "fpr")
plot(auc_test)


########## DATA VISUALIZATION ##########

##### CHART: TOP KEYWORDS BY FREQUENCY - WORD CLOUD

# Fetching only the keywords that we will use
Only_Keywords <- Air_France$keyword

# Converting the keyword vector into a corpus format
Only_Keywords <- Corpus(VectorSource(Only_Keywords))

# Coding required to set up the keywords for the word cloud
Document_Matrix <- TermDocumentMatrix(Only_Keywords) 
Keyword_Matrix <- as.matrix(Document_Matrix) 
Keywords <- sort(rowSums(Keyword_Matrix),decreasing = TRUE) 
Keyword_Cloud_df <- data.frame(word = names(Keywords),freq = Keywords)

# Setting up and running the word cloud
cloud <- wordcloud(words = Keyword_Cloud_df$word, freq = Keyword_Cloud_df$freq, min.freq = 1,
                   max.words = 200, random.order = FALSE, rot.per = 0.35,
                   colors=brewer.pal(8, "Dark2"))

# Visualizing the data
cloud


##### CHART: CUSTOMER JOURNEY: IMPRESSIONS TO CLICKS TO BOOKINGS - FUNNEL

# Creating the funnel
fig <- plot_ly(
  type = "funnel",
  name = 'Google - US',
  y = c("impressions", "clicks", "booking_vol"),
  x = c(3855689, 192109, 1550)) 
fig <- fig %>%
  add_trace(
    type = "funnel",
    name = 'Google - Global',
    orientation = "h",
    y = c("impressions", "clicks", "booking_vol"),
    x = c(1808326, 72895, 797)) 
fig <- fig %>%
  add_trace(
    type = "funnel",
    name = 'MSN - US',
    orientation = "h",
    y = c("impressions", "clicks", "booking_vol"),
    x = c(170120, 10808, 140)) 
fig <- fig %>%
  add_trace(
    type = "funnel",
    name = 'MSN - Global',
    orientation = "h",
    y = c("impressions", "clicks", "booking_vol"),
    x = c(139979, 11217, 129)) 
fig <- fig %>%
  add_trace(
    type = "funnel",
    name = 'Overture - US',
    orientation = "h",
    y = c("impressions", "clicks", "booking_vol"),
    x = c(17062488, 119323, 289))
fig <- fig %>%
  add_trace(
    type = "funnel",
    name = 'Overture - Global',
    orientation = "h",
    y = c("impressions", "clicks", "booking_vol"),
    x = c(17898727, 60899, 372)) 
fig <- fig %>%
  add_trace(
    type = "funnel",
    name = 'Yahoo - US',
    orientation = "h",
    y = c("impressions", "clicks", "booking_vol"),
    x = c(933345, 45598, 662)) 
fig <- fig %>%
  layout(yaxis = list(categoryarray = c("impressions", "clicks", "booking_vol")))

# Displaying the funnel
fig


##### CHART: BOOKINGS BY PUBLISHER AND CAMPAING - BARS

# Creating the bar chart with the ggplot2 function
ggplot(Air_France, # dataset
       aes(x = reorder(publisher_name, +booking_vol), # x variable
           y = booking_vol, # y variable
           fill = campaign_new)) + # third dimension
  geom_bar(stat = "identity", alpha = 0.8) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 0.5),
        panel.grid.major = element_blank(), # deleting the horizontal grid
        panel.grid.minor = element_blank()) + # deleting the vertical grid
  labs(title = "Bookings by Publisher", # title of the chart
       x = "Publisher", # name of the x axis
       y = "Number of Bookings") + #name of the y axis
  labs(fill = "Campaign")


##### CHART: ROA BY CAMPAIGN - BARS

# Creating the bar chart with the ggplot2 function
ggplot(AF_Campaign, # data set
       aes(x = reorder(campaign, -ROA), # x variable
           y = ROA,
           fill = campaign,
           label = ROA)) + # y variable
  geom_bar(stat = "identity", alpha = 0.8) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid.major = element_blank(), # deleting the horizontal grid
        panel.grid.minor = element_blank()) + # deleting the vertical grid
  labs(title = "ROA by Campaign", # title of the chart
       x = "Campaign", # name of the x axis
       y = "ROA") + # name of the y axis
  geom_text(aes(label = round(ROA, digits = 2), # label variable
                vjust = -0.5))


##### CHART: PROFIT BY PUBLISHER - DOUGHNUT

# Creating the share for each of the Publishers
AF_Publisher$profit_share <- AF_Publisher$profit / sum(AF_Publisher$profit)

# Calculating the cumulative percentages
AF_Publisher$p_ymax = cumsum(AF_Publisher$profit_share)

# Calculating the minimum
AF_Publisher$p_ymin = c(0, head(AF_Publisher$p_ymax, n=-1))

# Calculating measure for the label position
AF_Publisher$LabelPosition <- (AF_Publisher$p_ymax + AF_Publisher$p_ymin) / 2

# Defining a good-looking label
AF_Publisher$Label <- paste0(AF_Publisher$publisher, "\n share: ", round(AF_Publisher$profit_share, digits = 2))

# Visualizing the plot
ggplot(AF_Publisher, aes(ymax = p_ymax, 
                 ymin = p_ymin, 
                 xmax = 4, 
                 xmin = 3, 
                 fill = publisher)) +
  geom_rect() +
  geom_label(x = 3.5, aes(y = LabelPosition, label = Label), size = 3) +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) + # to transform pie to doughnut
  theme_void()


##### CHART: PROFIT, ROA AND BOOKINGS BY PUBLISHER - CIRCLES

# Creating the chart
plot_ly(AF_Publisher, x = ~profit, y = ~ROA,
        textposition = "auto",
        type = 'scatter', 
        mode = 'markers', 
        size = ~booking_vol, 
        color = ~publisher, 
        colors = 'Paired',
        marker = list(opacity = 0.6, sizemode = 'dict')) %>%
  layout(title = 'Profit, ROA and Bookings by Publisher',
         xaxis = list(title = "profit", showgrid = TRUE),
         yaxis = list(title = "ROA", showgrid = TRUE),
         showlegend = TRUE)
