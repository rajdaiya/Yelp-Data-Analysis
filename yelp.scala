import scala.collection.mutable.WrappedArray
 import spark.implicits._
 import org.apache.spark.sql.functions._
 val business = spark.read.json("/Users/Raj/Downloads/dataset/business.json")
 val ans1 = business.withColumn("category", explode(

when(col("categories").isNotNull,col("categories") .otherwise(array(lit(null).cast("string)
)))

 ans1.registerTempTable("business")
spark.sql("SELECT city,category, SUM(review_count) AS total_review FROM business
group by category,city order by city").show



import scala.collection.mutable.WrappedArray
import spark.implicits._
import org.apache.spark.sql.functions._
val business = spark.read.json("/Users/Raj/Downloads/dataset/business.json")
val b = business.withColumn("category", explode(
when(col("categories").isNotNull,col("categories")).otherwise(array(lit(null).cast("string"
)))))

b.registerTempTable("business")
val df = sqlContext.sql("SELECT category,city,avg(stars) as avg_stars from business
group by category,city order by category asc, avg_stars desc")
df.write.csv("/Users/Raj/Downloads/dataset/ans2.csv")



import scala.collection.mutable.WrappedArray
 import spark.implicits._
 import org.apache.spark.sql.functions._
 val business = spark.read.json("/Users/Raj/Downloads/dataset/business.json")
val lat_long_business = spark.sql("select *, (latitude*3.14/180) lat_rad,
(longitude*3.14/180) long_rad from business")
lat_long_business.createOrReplaceTempView("lat_long_business")
spark.sql("SELECT categories, avg(stars), avg(review_count) FROM lat_long_business
WHERE ACOS( SIN(lat_rad) * SIN(0.76189206903) + COS(lat_rad) * 
COS(0.76189206903) * COS(long_rad + 1.385498211 )) * 6371 <= 15 group by
categories").show