# Load and check ----------------------------------------------------------
bqdata <- data.table::fread("data/bq_result.csv")


# Explore -----------------------------------------------------------------
# print
bqdata
# save default numeric and characters
columns_numeric <- names(bqdata)[sapply(bqdata, is.numeric)]
columns_character <- names(bqdata)[sapply(bqdata, is.character)]

# Assessing features for missing and unique values
sapply(bqdata, function(x){sum(is.na(x))}) # NAs
sapply(bqdata, function(x){uniqueN(x)}) # duplicated

# average mean, median, sd after Na omit
sapply(bqdata[, ..columns_numeric], function(x){c(mean = mean(x, na.rm = TRUE),
                                                  median = median(x, na.rm = TRUE), 
                                                  sd = sd(x, na.rm = TRUE),
                                                  min = min(x, na.rm = TRUE), 
                                                  max = max(x, na.rm = TRUE))})

sapply(bqdata[, ..columns_character], table)


# Process -----------------------------------------------------------------
bqdata[, .(.N, 
           TimeOnSiteAvg = mean(TimeOnSite, na.rm = TRUE),
           PageviewsTotal = sum(Pageviews, na.rm = TRUE),
           BouncesTotal = sum(Bounces, na.rm = TRUE),
           TransactionRevenueTotal = sum(TransactionRevenue, na.rm = TRUE),
           ProductRevenueTotal = sum(ProductRevenue, na.rm = TRUE),
           ProductQuantityTotal = sum(ProductQuantity, na.rm = TRUE),
           ReturnVisitTotal = sum(ReturnVisit)
           ),
       by = .(FullVisitorID, VisitID)]



dcast(bqdata, FullVisitorID + VisitID ~ ChannelGrouping , fun = sum)
dcast(bqdata, FullVisitorID + VisitID ~ DeviceCategory , fun = sum)
