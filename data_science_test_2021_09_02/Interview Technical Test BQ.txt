#standardSQL
SELECT a.*, CASE WHEN c.VisitStartTime IS NOT NULL THEN 1 ELSE 0 END AS ReturnVisit
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
ORDER BY FullVisitorID, HitTime