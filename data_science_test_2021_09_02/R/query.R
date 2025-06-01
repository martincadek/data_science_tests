cli::cli_alert_info("Loading query.R.")
cli::cli_alert_warning("Please note you need to access and customise your setup for {.url https://console.cloud.google.com/}")

# Auth --------------------------------------------------------------------
if (checkmate::test_file_exists("data/bq_result.csv")) {
  cli::cli_alert_warning("Data are already present. No need to continue.")
  stop("Stopping the script.", call. = FALSE)
}

# set authentication
bq_auth() # set your own Auth

# link to project id
project_id <- "ranalytics-153016" # replace with other project ID


# Call --------------------------------------------------------------------
query_call <- "SELECT a.*, CASE WHEN c.VisitStartTime IS NOT NULL THEN 1 ELSE 0 END AS ReturnVisit
FROM 
  (
    SELECT DISTINCT
      visitNumber as VisitNumber,
      CAST(visitId as string) as VisitID,
      CAST(fullVisitorId as string) as FullVisitorID,
      TIMESTAMP_MICROS((CAST(visitStartTime as INT64))*1000000) as VisitStartTime,
      channelGrouping as ChannelGrouping,
      /* totals */
      totals.pageviews as Pageviews,
      totals.bounces as Bounces,
      totals.timeOnSite as TimeOnSite,
      /* device */
      device.deviceCategory as DeviceCategory,
      /* hits */
      TIMESTAMP_MICROS(((CAST(visitStartTime as INT64))*1000000) + (CAST(hit.time*1000 as INT64))) as HitTime,
      hit.type as HitType,
      /* hit.page */
      hit.page.pagetitle as PageTitle,
      /* hit.transaction */
      hit.transaction.transactionId as TransactionID,
      (hit.transaction.transactionRevenue)/1000000 as TransactionRevenue,
      /* hit.product */
      product.productSKU as ProductSKU,
      product.v2ProductName as V2ProductName,
      product.v2ProductCategory as V2ProductCategory,
      product.productRevenue/1000000 as ProductRevenue,
      product.productQuantity as ProductQuantity,
    FROM `bigquery-public-data.google_analytics_sample.ga_sessions_*`
    LEFT JOIN UNNEST(hits) as hit
    LEFT JOIN UNNEST(hit.product) as product
    WHERE _TABLE_SUFFIX BETWEEN '20170601' AND '20170731'
  ) a
INNER JOIN (
  SELECT DISTINCT FullVisitorID, MIN(VisitStartTime) AS MinVisit 
  FROM (
    SELECT DISTINCT
      visitNumber as VisitNumber,
      CAST(visitId as string) as VisitID,
      CAST(fullVisitorId as string) as FullVisitorID,
      TIMESTAMP_MICROS((CAST(visitStartTime as INT64))*1000000) as VisitStartTime,
      channelGrouping as ChannelGrouping,
      /* totals */
      totals.pageviews as Pageviews,
      totals.bounces as Bounces,
      totals.timeOnSite as TimeOnSite,
      /* device */
      device.deviceCategory as DeviceCategory,
      /* hits */
      TIMESTAMP_MICROS(((CAST(visitStartTime as INT64))*1000000) + (CAST(hit.time*1000 as INT64))) as HitTime,
      hit.type as HitType,
      /* hit.page */
      hit.page.pagetitle as PageTitle,
      /* hit.transaction */
      hit.transaction.transactionId as TransactionID,
      (hit.transaction.transactionRevenue)/1000000 as TransactionRevenue,
      /* hit.product */
      product.productSKU as ProductSKU,
      product.v2ProductName as V2ProductName,
      product.v2ProductCategory as V2ProductCategory,
      product.productRevenue/1000000 as ProductRevenue,
      product.productQuantity as ProductQuantity,
    FROM `bigquery-public-data.google_analytics_sample.ga_sessions_*`
    LEFT JOIN UNNEST(hits) as hit
    LEFT JOIN UNNEST(hit.product) as product
    WHERE _TABLE_SUFFIX BETWEEN '20170601' AND '20170731'
  )
  GROUP BY 1
) b
ON a.FullVisitorID = b.FullVisitorID
AND a.VisitStartTime = MinVisit
LEFT JOIN (
  SELECT DISTINCT FullVisitorID, VisitStartTime
  FROM (
    SELECT DISTINCT
      visitNumber as VisitNumber,
      CAST(visitId as string) as VisitID,
      CAST(fullVisitorId as string) as FullVisitorID,
      TIMESTAMP_MICROS((CAST(visitStartTime as INT64))*1000000) as VisitStartTime,
      channelGrouping as ChannelGrouping,
      /* totals */
      totals.pageviews as Pageviews,
      totals.bounces as Bounces,
      totals.timeOnSite as TimeOnSite,
      /* device */
      device.deviceCategory as DeviceCategory,
      /* hits */
      TIMESTAMP_MICROS(((CAST(visitStartTime as INT64))*1000000) + (CAST(hit.time*1000 as INT64))) as HitTime,
      hit.type as HitType,
      /* hit.page */
      hit.page.pagetitle as PageTitle,
      /* hit.transaction */
      hit.transaction.transactionId as TransactionID,
      (hit.transaction.transactionRevenue)/1000000 as TransactionRevenue,
      /* hit.product */
      product.productSKU as ProductSKU,
      product.v2ProductName as V2ProductName,
      product.v2ProductCategory as V2ProductCategory,
      product.productRevenue/1000000 as ProductRevenue,
      product.productQuantity as ProductQuantity,
    FROM `bigquery-public-data.google_analytics_sample.ga_sessions_*`
    LEFT JOIN UNNEST(hits) as hit
    LEFT JOIN UNNEST(hit.product) as product
    WHERE _TABLE_SUFFIX BETWEEN '20170601' AND '20170731'
  )
) c
ON a.FullVisitorID = c.FullVisitorID
AND TIMESTAMP_ADD(a.VisitStartTime, INTERVAL 31 DAY) > c.VisitStartTime 
AND a.VisitStartTime < c.VisitStartTime
WHERE a.VisitStartTime BETWEEN '2017-06-01' AND '2017-06-30'
ORDER BY FullVisitorID, HitTime"


# Save --------------------------------------------------------------------
# Create table in BigQuery to save the result in (needs to be done via the command line)
bq_dest <- bq_table(project_id, "ds_2021_09_02", "dsdata")

# Billed: 210.76 MB
table_result <- bq_project_query(project_id, query_call, destination_table = bq_dest)

# As the query above is large, it is needed to go to https://console.cloud.google.com/
# and export the query to GCS (or use gcsutils), once exported to CSV, into a google cloud
# bucket, once exported the results can be downloaded locally, and read as per fread function below.
bqdata <- data.table::fread("data/bq_result.csv")

cli::cli_alert_success("query.R finished.")