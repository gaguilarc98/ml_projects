--DATA DRIVEN DECISION MAKING IN SQL
/*
Learn about new SQL statements to summarize data:
  * OLAP extensions 
  * CUBE, ROLLUP and GROUPING SETS
*/
/*
FILTERING AND ORDERING
When a WHERE caluse is added the records are filtered out.
We can use  comparison operators: <, >, >=, <=, =, <>, BETWEEN, IN, IS NULL, IS NOT NULL
When coamparing to dates use SINGLE QUOTES only, and in general use single QUOTES always.

GROUPING AND SUMMARIZING
The aggregating functions AVG, SUM, MIN, MAX always ignore null values.
DISTINCT includes null as different values.
When ordering null is taken as the largest value.
HAVING is always used alongised group by and performs filters alongside groups

JOINING TABLES
LEFT JOIN is an outer join and joins information from a right table to a left table

IMPORTANT NOTE: When aliasing a table we can omit the AS and the query will work just fine
but we keep the word for clarity.

SUBSEQUENT SELECT STATEMENTS (SUBQUERIES)
Inner queries are executed first.
Make sure to give aliases to the subqueries if they are in the FROM statement.
*/
/*
NESTED QUERIES
It is one where the SELECT block is in WHERE or HAVING clauses.
It is possible to have several levels of nesting.

CORRELATED NESTED QUERIES.
When the inner query cannot be executed independtly (because it calls fields from outer tables)
The query is called a CORRELATED NESTED QUERY.
Nested query is executed in a loop.
*/
SELECT *
FROM movies AS m
WHERE 5 < (SELECT COUNT(*)
			FROM renting AS r
			WHERE r.movie_id = m.movie_id)
/*
QUERIES with EXISTS
Special case of a correlated nested query. It is useed to check if result of a correlated nested query is empty.
*/
SELECT *
FROM movies AS m
WHERE EXISTS
	(SELECT *
	FROM renting AS r
	WHERE rating IS NOT null
	AND r.movie_id = m.movie_id)
--EXISTS check if the table exists or has at least one record
/*
QUERIES WITH UNION AND INTERSECT
UNION gets records that are in either of 2 tables.
We do not have any duplicates with UNION.
It is crucial that the same columns exists in both tables.

INTERSECT gets records that are in goth tables, removing duplicates.
*/
/*
OLAP: CUBE operator.
OLAP: Online analytical processing. Used to interact with data.
CUBE, ROLLUP AND GROUPING SET OPERATIONS.

PIVOT TABLE
*/
SELECT  country, genre, COUNT(*)
FROM renting_extended
GROUP BY CUBE (country, genre);
--CUBE makes aggregations by each combination of country and genre, and country/genre alone, then no all records.

SELECT  country, genre, COUNT(*)
FROM renting_extended
GROUP BY ROLLUP (country, genre);
--ROLLUP makes aggregations by each combination of country, genre then aggregation by country, then all records.
--ROLLUP is hierarchical, CUBE is not
/*
GROUPING SETS
GROUP BY GROUPING SETS
This is the most flexible OLAP operator.
*/
SELECT country, genre COUNT(*)
FROM rentings_extended
GROUP BY GROUPING SETS ((country, genre), (country), (genre), ())
--GROUPING SETS can be seen as a UNION over group by operators.
