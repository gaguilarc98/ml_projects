--__JOINING TABLES 
--It is common to start building the query by first making
--the join and then selecting the fields.
SELECT primer_ministers.country, prime_minister.continent, prime_minister, president
FROM prime_minister
INNER JOIN presidents
ON prime_minister.country = predsidents.country;

--When selecteing fields whose names exist on both TABLES
--Dot notation should be used to avoid errors.
--ALIASING TABLES
--When joining tables it is handy to use aliases to reference both the left and right table.
SELECT p1.country, p1.continent, prime_minister, president
FROM prime_ministers AS p1
INNER JOIN presidents AS  p2
ON p1.country = p2.country
--__When joining with identical field names we can use the USING commnad
SELECT p1.country, p1.continent, prime_minister, president
FROM prime_ministers AS p1
INNER JOIN presidents AS  p2
USING(country)
--MULTIPLE JOINS
--We can make joins with multiple tables by chaining the join statements
SELECT *
FROM left_table
INNER JOIN right_table
ON left_table.id = right_table.id
INNER JOIN another_table
ON left_table.id = another_table.id;
--MULTIPLE FIELDS FOR join
--We can use several fields to uniquely join records from different tables
--This by adding an AND statement
SELECT *
FROM left_tabñe
INNER JOIN right_table
ON left_table.id = right_table-id 
	AND left_table.date = right_table.date

--LEFT JOIN aka LEFT OUTER JOIN
--The syntax is similar to the INNER JOIN but it keeps all the records 
--available in the left table. The right table records not available are filled with null.
--RIGHT JOIN aka RIGHT OUTER JOIN

--FULL JOIN is the last of the 3 types of OUTER JOINS. It combines left and right joins
--You can also use FULL OUTER JOIN
--CROSS JOIN-
--__They create all possible combinations sof two tables.
--__Since all possible combinations are included, no ON or USING statement is required.
--EXAMPLE: CREATE MEETINGS FOR PRIME MINISTERES IN ASIA AND PRESIDENTS IN SOUTH AMERICA
SELECT prime_minister, president
FROM prime_ministers AS p1
CROSS JOIN presidents AS p2
WHERE p1.continent IN ('Asia')
	AND p2.continent IN ('South America')

--SELF JOIN. A table is joined with itself.
--There is no SELF JOIN keyword in SQL, for this we have to use ALIASING

--SET OPERATIONS FOR SQL
--__We will develop the theory for set operations in SQL and compare them to joins.
--SQL has 3 basic set operacions:
--UNION. Joins all records from two sets.
--INTERSECT. Joins records present in both sets.
--EXCEPT. Filters recordes present in only one set.
--__There is a variant of UNION called UNION ALL. 
--UNION joins two tables keeping one record if there are duplicates. While UNION ALL keeps all the data.
--The names of left and right fields need not be the same, they just need to be of the same type.
--The names will pe pulled out from the left table.
SELECT *
FROM economies2015
UNION
SELECT *
FROM economies2019
ORDER BY code, year;
--INTERSECT requires all fields to match just as in UNION. It requires to have the same number of columns.
--An INNER JOIN set up to work as an ITERSECT will return duplicate records, while INTERSECT will not.
--Also an INNER JOIN adds new fields
--EXCEPT allows us to identify records present in one tablel but not the other.
--Note that for INTERSECT, EXCEPT, all fields from a single record mmust match in order to be left in or out.

--__All joins so far have been aditive. This means that they aggregate fields to the left table.
--SEMI JOIN returns all values from the left table where the values for a field are in the right table equivalent field.

--SUBQUERIES
--We can use the result of a query as a table to filter results from
--Another table. Thus simulating a SEMI JOIN.
SELECT *
FROM cities
WHERE country_code  IN (SELECT code
						FROM countries 
						WHERE continent = "Asia")

--Similarly ANTI JOIN keeps records from table 1 when column1 does not find a match in column 2
SELECT *
FROM cities
WHERE country_code  NOT IN (SELECT code
						FROM countries 
						WHERE year_foundation = 1800)

--So far we have seen subqueries inside WHERE to filter observations meeting complex criteria
--SUBQUERIES inside SELECT.
--This is useful for selecting data not present in the original table.
SELECT DISTINCT continent,
(SELECT COUNT(*) FROM monarchs WHERE states.continent = monarch.continent) AS monarch_count
FROM states;

--EXAMPLE-- Find top nine countries with the most cities
SELECT countries.name AS country, COUNT(cities.name) AS cities_num
FROM countries
LEFT JOIN cities
ON countries.code = cities.country_code
GROUP BY countries.name
-- Order by count of cities as cities_num
ORDER BY cities_num DESC, country ASC
LIMIT 9;

--Using a subquery inside SELECT
SELECT countries.name AS country,
-- Subquery that provides the count of cities   
  (SELECT COUNT(*)
   FROM cities
   WHERE cities.country_code = countries.code) AS cities_num
FROM countries
ORDER BY cities_num DESC, country
LIMIT 9;
--QUERY inside a FROM clause
--EXAMPLE CREATE a table showing the name of each country and the number of languages spoken there.
-- Select local_name and lang_num from appropriate tables
SELECT local_name, sub.lang_num
FROM countries,
  (SELECT code, COUNT(*) AS lang_num
  FROM languages
  GROUP BY code) AS sub
-- Where codes match
WHERE countries.code = sub.code
ORDER BY lang_num DESC;

--CHALLENGE EXAMPLE
--Select the inflation rate and unemployment rate for countries whose government form
--does not contain the words "Republic" or "Monarchy"
-- Select relevant fields
SELECT code, inflation_rate, unemployment_rate
FROM economies
WHERE year = 2015 
  AND code NOT IN
-- Subquery returning country codes filtered on gov_form
	(SELECT code
  FROM countries
  WHERE gov_form LIKE '%Republic%' OR gov_form LIKE '%Monarchy%')
ORDER BY inflation_rate;

--CHALLENGE EXAMPLE
--Show the top 10 capital cities with the highest percentage of proper city population.
-- Select fields from cities
SELECT name, country_code, city_proper_pop, metroarea_pop, city_proper_pop / metroarea_pop * 100 AS city_perc
FROM cities
-- Use subquery to filter city name
WHERE name IN (SELECT capital
                FROM countries
                WHERE continent ='Europe' OR continent LIKE '%America')
-- Add filter condition such that metroarea_pop does not have null values
AND metroarea_pop IS NOT NULL
-- Sort and limit the result
ORDER BY city_perc DESC
LIMIT 10