--EXPLORATORY DATA ANALYSIS IN SQL
--Reminders:
/*
  * NULL is missing value in SQL
  * Use IS NULL or IS NOT NULL instead of =NULL
  * Use COUNT(*) for number of ROWS
  * COUNT(field) number of non-NULL values in the field
  * COUNT( DISTINCT field) number of distinct vlaues including NULL
 
KEYS IN A DATABASE
Primary keys are fields that uniquely identify each record in a table.
There are also foreign keys whose values reference another row in a different table. 
*/
--COALESCE FUNCTION
SELECT COALESCE(column_1, column_2)
FROM prices;
--This checks if the value of the first column is null if it is not NULL retuns the value in column1
--if it is NULL checks column2 and so on.
/*
COLUMN TYPES AND CONSTRINTS
Foreign and primary keys are one type of constraints that limit the type of values in a column.
Unique values. These fields must be all different except for NULL
Not null. The column cannot contain NULL values.
Check constraints. Conditions on values (Saldo > 0) (Monto > Saldo)
DATA TYPES
numeric, character, data/time/ boolean.
Special types: arrays, monetary, binary, geometric, network address, XML, JSON, etc.
NUMERIC TYPES
smallint -32768 to +32767
INTEGER -2147483648 to +2147483647
bigint -9223372036854775808 to -9223372036854775807
decimal up to 131072 digits before the decimal point, up to 16383 after the decimal point
numeric up to 131072 digits before the decimal point, up to 16383 after the decimal point
real 6 decimal digits precision
double precision 15 decimal digits precision
smallserial 1 to 32767
serial 1 to 2147483647
bigserial 1 to 9223372036854775807
CAST()
This serves to convert one type of value to a different type.
*/
SELECT CAST(value AS new_type);
SELECT CAST(3.7 AS integer);
SELECT value::new_type --For PostgreSQL
/*
Numeric data and Summary functions.
NOTE: Serial columns are used to create Id columns because they autoincrement.
AVG() gets the average or mean value.
VAR_POP() gets the population variance.
VAR_SAMP() also VARIANCE() gets the sample variance.
STDDEV_POP() and STDDEV_SAMP also STDDEV() get population and sample standard deviation
ROUND(value, decimals)
*/
/*
TRUNCATE is not the same as rounding. You never get a result with a larger abs value than the original number.
*/
SELECT TRUNC(42.1256,2); --This returns 42.12
SELECT TRUNC(42.1256,-3);--This returns 12000
--GENERATE_SERIES() 
SELECT GENERATE_SERIES(1,10,2); --This returns an array 1,3,5,7,9
--This can be used to transform values into bins.
WITH bins AS (
	SELECT 	generate_series(30,60,5) AS lower,
			generate_series(35,65,5) AS upper),
ebs AS (SELECT unanswered_count
		FROM stackoverflow
		WHERE tag='amazon_ebs')

SELECT lower, upper, count(unanswered_count)
FROM bins
LEFT JOIN ebs
ON unanswered_count>=lower AND unanswered_count < upper
GROUP BY lower,upper
ORDER BY lower;
/*
More summary functions.
CORRELATION
MEDIAN

*/
SELECT corr(column1, column2)
SELECT percentile_disc(percentile) WITHIN GROUP (ORDER BY column_name)
FROM table;-- Since the 
--also
SELECT percentile_cont(percentile) WITHIN GROUP (ORDER BY column_name)
FROM table;
--Common issues. sometimes 9, 99 and -99 are error codes.
--Missing value codes: NA, NaN, N/A, #N/A, i 0 missing?
/*
Creating temporary tables. These tables only last the duration of the query
*/
--Create table as
CREATE TEMP TABLE new_tablename as
--Query results to store in the table
SELECT col, col2 FROM table;

--With a different syntax
SELECT col1, col2
INTO TEMP TABLE new_tablename
FROM table;

--To insert records use INSERT INTO table_name followed by the query with the same columns
--To delete a table: DROP TABLE table_name;

DROP TABLE IF EXISTS table_name;
--EXAMPLE. Find the Fortune 500 Companies htat have profits in the top 10% for their sector.
-- Code from previous step
DROP TABLE IF EXISTS profit80;

CREATE TEMP TABLE profit80 AS
  SELECT sector, 
         percentile_disc(0.8) WITHIN GROUP (ORDER BY profits) AS pct80
    FROM fortune500 
   GROUP BY sector;

-- Select columns, aliasing as needed
SELECT title, fortune500.sector, 
       profits, profits/pct80 AS ratio
-- What tables do you need to join?  
  FROM fortune500 
       LEFT JOIN profit80
-- How are the tables joined?
       ON fortune500.sector=profit80.sector
-- What rows do you want to select?
 WHERE profits > pct80;
 /*
 CHARACTER DATA. These can be CHAR(n), VARCHAR(n), TEXT.
 CHAR stores strings with the same length filling with blanks that are omitted when comparing. 
 VARCHAR stores strings with varying length up to the specified maximum length.
 TEXT stores unlimited length strings.

CASES AND SPACES
We can clean category names with functions like TRIM, lower and upper
NOTE: LIKE is case-sensitive, but ILIKE is not
*/
SELECT *
FROM fruit
WHERE fav_fruit ILIKE '%apple%';
--Be careful as pineppale will also be included, therfore it is better
--to remove only leading white spaces with TRIM.
SELECT street, TRIM(street, '0123456789#/.')
FROM city;--Notice that TRIM erases any of the characters in the second argument (not necesarily in that order)
/*
SPLITTING AND CONCATENATING
LEFT(string, num_char), RIGHT(string, num_char)
SUBSTRING(string FROM start FOR length)

SPLIT based on a delimiter
split_part(string, delimiter, part);--where parte is the part of the string to return (before/after the split)
*/
SELECT split_part('a,bc,d', ',', 2); --This returns 'bc'
/*
CONCATENATING TEXT
CONCAT(col1, 'string', col2) this can concatenate any type of data
We can also use || notation:
SELECT 'a' || NULL || 'cc'
The differences is that the CONCAT omits NULL while || keeps this.

STRATEGIES FOR MULTIPLE TRANSFORMATIONS
__EXAMPLE. There is a column whose delimiter is not consistent across all records.
We could use a CASE WHEN:
SELECT CASE WHEN category LIKE '%: %' THEN split_part(category, ': ',1)
			WHEN category LIKE '% - %' THEN split_part(category, ' - ',1)
			ELSE split_part(category, ' | ',1)
			END AS major_category,
FROM table;

RECODING TABLE. To solve messy columns create a temporary table to fix messy values.
CREATE TEMP TABLE recode AS
	SELECT DISTINCT fav_fruit AS original, 
			fav_fruit AS standardized
	FROM fruit;
	
Then UPDATE:
UPDATE recode 
	SET standardized = trim(lower(original));
UPDATE recode
	SET standardized='banana'
	WHERE standardized LIKE '%nn%';

The join original and recode tables
SELECT standardized, COUNT(*)
FROM fruit
	LEFT JOIN recode 
	ON fav_fruit=original
GROUP BY standardized;
*/
/*
DATE/TIME DATA
Date only includes y,m,d
Timestamp include y,m,d and hour to the precision of microsecods.
Intervals are time durations. These usually are a result of substracting times.
PostgreSQL stores date and time in the format ISO 8601
ISO = International Organization for Standards.
YYYY-MM-DD HH:MM:SS
Timezones. PostgreSQL uses UTC, but it can include the timezone as shown:
YYYY-MM-DD HH:MM:SS+HH

Date and timestamps can be compared with >, <, =, etc.
now() returns the current time
SELECT '2010.01.01'::date +1 --Adding integer to dates will increment days.
However, adding integers to timestamps returns an erro.
but we can add intervals:
SELECT '2018-12-10'::date +'1 year'::interval;
SELECT '2018-12-10'::date +'1 year 2 days 3 minutes'::interval;

NOTE: In spelling plural units of time the 's' is optional, that is minute or minutes gives the same results.

EXTRACTING COMPONENTS OF DATE/TIME DATA. Some common fields are:
  * century: 2019-01-01=century 21
  * decade: 2019-01-01=decade201
  * year, month, day
  * hour, minute, second
  * week
  * dow: day of week. Starts with sunday with a value of 0 and ends at saturday with a value of 6.
date_part('field', timestamp)
EXTRACT(FIELD FROM timestamp)
TRUNCATING DATE:
date_trunC('field', timestamp)
SELECT date_trunc('month', now());
*/

-- Select name of the day of the week the request was created 
SELECT to_char(date_created, 'day') AS day, --This is to convert days to the name of the day Monday, Tuesday, etc.
       -- Select avg time between request creation and completion
       AVG(date_completed - date_created) AS duration
  FROM evanston311 
 -- Group by the name of the day of the week and 
 -- integer value of day of week the request was created
 GROUP BY day, EXTRACT(DOW FROM date_created)--The to_char() will sort the days alphabetically, instead use EXTRACT()
 -- Order by integer value of the day of the week 
 -- the request was created
 ORDER BY EXTRACT(DOW FROM date_created);
 
 --IMPORTANT NOTE: A difference between date_part and date trunc() is that
 --date_trunc() will keep the larger parts of the date, but date_part()
 --will only extract the specific part requested. Thus if order is to be
 --preserved use date_trunc()
 --EXAMPLE. Use date_trunc() to find the average number of requests per day for each month of the data.
 -- Aggregate daily counts by month
SELECT date_trunc('month', day) AS month,
       AVG(count)
  -- Subquery to compute daily counts
  FROM (SELECT date_trunc('day', date_created) AS day,
               COUNT(*) AS count
          FROM evanston311
         GROUP BY date_trunc('day', date_created)) AS daily_count
 GROUP BY date_trunc('month', day)
 ORDER BY month;
 
 /*
AGGREGATING with DATE/TIME SERIES
Dealing periods with no observations could be done with:
 */
 SELECT generate_series('2018-01-01', '2018-01-02', '5 hours'::interval)
--To generate a series with the last day of each month
--Generate a series with the beginning of each month and the substract 1 day
SELECT generate_series('2018-02-01', 
						'2019-01-01', 
						'1 month'::interval) - '1 day'::interval
--To include hours with no records in a database we can use the following approach:
WITH hour_series AS(
SELECT generate_series('2018-04-23 09:00:00',
						'2018-04-23 14:00:00',
						'1 hour'::interval) AS hours
)
SELECT hours, count(date) --Count date to not include null values
FROM hour_series
LEFT JOIN sales
ON hours= date_trunc('hour', date)
GROUP BY hours
ORDER BY hours;

--Technique for working with bins of time or continuous data
--Generate a series of lower and upper bounds:
WITH bins AS(
SELECT generate_series('2018-04-23 09:00:00',
						'2018-04-23 15:00:00',
						'3 hour'::interval) AS lower,
		generate_series('2018-04-23 12:00:00',
						'2018-04-23 18:00:00',
						'3 hour'::interval) AS upper
)

SELECT lower, upper, count(date)
FROM bins
LEFT JOIN sales
ON date>=lower AND date < upper
GROUP BY lower, upper
ORDER BY lower;

--__EXAMPLE. Create the median of resquests per day by each semester from I/2016 to I/2018
-- Bins from Step 1
WITH bins AS (
	 SELECT generate_series('2016-01-01',
                            '2018-01-01',
                            '6 months'::interval) AS lower,
            generate_series('2016-07-01',
                            '2018-07-01',
                            '6 months'::interval) AS upper),
-- Daily counts from Step 2
     daily_counts AS (
     SELECT day, count(date_created) AS count
       FROM (SELECT generate_series('2016-01-01',
                                    '2018-06-30',
                                    '1 day'::interval)::date AS day) AS daily_series
            LEFT JOIN evanston311
            ON day = date_created::date
      GROUP BY day)
-- Select bin bounds 
SELECT lower, 
       upper, 
       -- Compute median of count for each bin
       percentile_disc(0.5) WITHIN GROUP (ORDER BY count) AS median
  -- Join bins and daily_counts
  FROM bins
       LEFT JOIN daily_counts
       -- Where the day is between the bin bounds
       ON day >= lower
          AND day < upper
 -- Group by bin bounds
 GROUP BY lower, upper
 ORDER BY lower;
 
 --TIME BETWEEN EVENTS
 --USE LEAD() AND LAG() to get difference between events.
 