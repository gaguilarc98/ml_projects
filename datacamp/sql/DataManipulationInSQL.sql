--DATA MANIPULATION IN SQL
--CASE STATEMENT
--Supposse we have a soccer database with the number of goals from the home and the away team
--We would like to add a column that indicates if the home team won, lost or tied.
--We could make separate queries for each case, but this would be inefficient.
--Instead we could use a CASE STATEMENT, they have three parts:
--__WHEN clause
--__THEN clause
--__ELSE clause
--__finish with END
CASE WHEN x = 'N' THEN FALSE
	WHEN x = 'S' THEN TRUE
	ELSE FALSE END AS new_column
--Do not forget to put an alias after the END
--CASE WHEN WITH WHERE CLAUSES
--Sometimes we wish to filter out some records that coe from certain cases
--For that since SELECT IS EXCEUTED AFTER THE WHERE CLAUSE, we cannot use 
--The CASE variable alias created in SELECT to filter in WHERE
--Instead we have to copy the entire CASE WHEN END clause.
--EXAMPLE: Select matches won by Bolivia either as home or away team
SELECT date, home_goal, away_goal
FROM matches
WHERE CASE WHEN home_team = "Bolivia" AND home_goal>away_goal THEN 'Bolivia won'
			WHEN away_team = "Bolivia" AND away_team>home_team THEN 'Bolivia won'
			END NOT NULL

--CASE WHEN with aggregate functions
--CASE is used to categorized data, filter data or aggregate data.
--EXAMPLE create number of matches won by Bolivia as a host each season
SELECT season, COUNT(CASE WHEN hometeam = "Bolivia" AND home_goal>away_goal THEN id END) AS home_wins
FROM matches
GROUP BY season;
--EXAMPLE. Calculate the percentage of ties in each season rounded to 2 decimal places
SELECT 
	c.name AS country,
    -- Round the percentage of tied games to 2 decimal points
	ROUND(AVG(CASE WHEN m.season='2013/2014' AND m.home_goal = m.away_goal THEN 1
			 WHEN m.season='2013/2014' AND m.home_goal != m.away_goal THEN 0
			 END),2) AS pct_ties_2013_2014,
	ROUND(AVG(CASE WHEN m.season='2014/2015' AND m.home_goal = m.away_goal THEN 1
			 WHEN m.season='2014/2015' AND m.home_goal != m.away_goal THEN 0
			 END),2) AS pct_ties_2014_2015
FROM country AS c
LEFT JOIN matches AS m
ON c.id = m.country_id
GROUP BY country;

--SUBQUERIES
--__A subquery is  a nested  query inside another query.
--__Often when retrieving data that we want we have to make intermediate transformations
--__before selecting or filtering. It can be put into SELECT, FROM, WHERE or GROUP BY clauses.
--__It can return a variety of results such as
--__*Scalars
--__*Lists (for filtering or joining information)
--__*Tables for further transform data.
--They allow to compare groups of summarized data, to reshape data, to combine data that cannot be joined.
--SIMPLE SUBQUERY. Is a query that can be run independently on its own. It is only processed once.
--When it is used in the WHERE clause is used for filtering.
--EXAMPLE: Get matches where the sum of goals is greater thatn 3 times that average
SELECT 
	-- Select the date, home goals, and away goals scored
    date,
	home_goal,
	away_goal
FROM  matches_2013_2014
-- Filter for matches where total goals exceeds 3x the average
WHERE (home_goal + away_goal) > 
       (SELECT 3 * AVG(home_goal + away_goal)
        FROM matches_2013_2014);

--SUBQUERIES in the FROM statement. It is used for restructuring and transforming data.
--*Prefiltering data
--*Calculating aggregate of aggregates
--Remember to give an alias to the subquery if it is in the FROM statement
--You can also use the subquery to JOIN to the FROM table.
--SUBQUERIES in SELECT statement. Used to return a single aggregate value.
--Since we cannot select an aggregate value in query that is not grouped.
--NOTE: We need a single value to be returned in the subquery inside SELECT.
--__Otherwise an error will be thrown beacuse each value is assgined to a single record.

--BEST PRACTICE: Format queries: align SELECT, FROM, GROUP BY AND WHERE statements
/* Anotate comments for queries
*/
SELECT -- Add quick comments
--Indent subqueries.
--Watch that filters are placed in the main queries and subqueries.

--CORRELATED SUBQUERIES
--__Use values from the outer query to generate a result.
--__Re-run for every row generated in the final data set. They are used for advanced joining and filtering.
--In essence a correlated subquery is a query that references outer tables (tables outside the subquery)
--DIFFERENCES:
/*
|			Simple			|			Correlated			|
|Can be run independently	|Dependent on the main query	|
|Evaluated once in the whole query|Evalueated in loops once for each row|
So correlated queries will slow down the query.
*/
--EXAMPLE. What is the avergae number of goals scored in each country?
--With single subquery:
SELECT c.name AS country, AVG(m.home_goal + m_away_goal) AS avg_goals
FROM country AS c
LEFT JOIN match AS m
ON c.id = m.country_id
GROUP BY country
--With correlated subquery:
SELECT c.name AS country,	(SELECT AVG(home_goal+away_goal)
							FROM match AS m
							WHERE m.country_id = c.id) AS avg_goals
FROM country AS c
GROUP BY country;
					
--NESTED SUBQUERIES
--__Subqueries nested inside another subquery. Since a question may require multiple layers
--__of transformation and filtering the data.
--Nested subqueries can be correlated or uncorrelated.

--IMPROVING READABILITY
--Common Table Expressions (CTEs) Table declared before the main query
--Use a WITH statment give it an alias an use it as if it where any other table
WITH cte AS (SELECT col1, col2, FROM table)
SELECT AVG(col1) AS avg_col
FROM cte;

--EXAMPLE join with a table that gets matches where the total number of goals 
--Is greater than or equal to 10.
WITH s AS (SELECT country_id, id, FROM match WHERE (home_goal + away_goal)>=10)

SELECT c.name, AS country
		COUNTR(s.id) AS matches
FROM country AS c
LEFT JOIN s
ON c.id = s.country_id
GROUP BY country;

--Using multiple tables can be done by listing them in WITH:
WITH s1 AS (),
	s2 AS ()
SELECT *
FROM table
INNER JOIN s1 
INNER JOIN s2
GROUP BY

--Common table expressions have advantages:
--__* CTE is only executed once, then is stored in memory.
--__* Organizing queries
--__* Referencing other CTEs
--__* CTEs cana reference themselves 

--Deciding techniques
--Many techniques can be used interchangeably without compromising perforance or accuracy.
--JOINS combine tables already present
--Correlated subqueries. Combine table to subqueries, but with high processing time.
--Multiple Nested Subqueries. Multi-step transformations
--Common table expressions. Orgenize tables and able to be referenced.

--__Think of which alternatives can be used and reused efficiently.

--WINDOW FUNCTIONS
--When dealing with aggregate functions we have to group the data with the rest of the variables
--Not included in the aggregations, otherwise an error will be thrown. Window functions are a way to
--get around this problem.
--EXAMPLE. Select the date and  number of goals and create a column
--with the average number of goals, everything in the season 2011/2012
SELECT date, (home_goal + away_goal) AS goals,
		(SELECT AVG(home_goal + away_goal) 
		FROM match
		WHERE season = '2011/2012') AS overall_avg
FROM match
WHERE season = '2011/2012'

--Using the OVER() statement to avoid the subquery:
SELECT date, (home_goal + away_goal) AS goals,
		AVG(home_goal + away_goal) OVER() AS overall_avg
FROM match
WHERE season = '2011/2012'
--OVER() tells SQL pass the agregate value over the result set.

--Another window function is RANK that creates a ranking order based on a field.
--EXAMPLE. Rank the matches in season 2011/2012 based on number of goals scored.
SELECT date, (home_goal + away_goal) AS goals
		RANK() OVER(ORDER BY home_goal + away_goal DESC) AS goals_rank
FROM match
WHERE season = '2011/2012'
--Key differences:
--__1. Window functions are processed after every part of query except ORDER BY.
--__  thus it uses the result set rather than database.
--__2. Some may not be available in all SQL flavors.
--EXAMPLE. Rank the matches in season 2011/2012 according to the number of goals 
--         and average number of goals in each league.
SELECT 
	-- Select the league name and average goals scored
	l.name AS league,
    AVG(m.home_goal + m.away_goal) AS avg_goals,
    -- Rank each league according to the average goals
    RANK() OVER(ORDER BY AVG(m.home_goal + m.away_goals) DESC) AS league_rank
FROM league AS l
LEFT JOIN match AS m 
ON l.id = m.country_id
WHERE m.season = '2011/2012'
GROUP BY l.name
-- Order the query by the rank you created
ORDER BY league_rank;

--OVER with a PARTITION.
--Use this to calculate separate values for different categories
--established in the partiition by.
--EXAMPLE Calulate the average value for each league in each season 
SELECT l.name AS league
		AVG(m.home_goal + m.away_goal) OVER(PARTITION BY season) AS league_avg_season
FROM league as L
LEFT JOIN match AS m
ON l.id = m.country_id

--PARTITION BY can also include multiple field to partition on.
SELECT l.name AS league
		AVG(m.home_goal + m.away_goal) OVER(PARTITION BY m.season, l.name) AS league_avg_season
FROM league as L
LEFT JOIN match AS m
ON l.id = m.country_id
--SLIDING WINDOWS. Perform calculation relative to the current row of the dataset.
--They can also be partitioned. The keywords are ROWS BETWEEN <start> AND <finish>
--the values can be PRECEDING, FOLLOWING, UNBOUNDED PRECEDING, UNBOUNDED FOLLOWING, CURRENT ROW.

--EXAMPLE. Calculate the cumulate number of goals for league 8456 in season 2011/2012
SELECT date, home_goal, away_goal, 
		SUM(home_goal)
		OVER(ORDER BY date, ROWS BETWEEN
			UNBOUNDED PRECEDING AND CURRENT ROW) AS runing_total
FROM match
WHERE hometeam_id = 8456 AND season = '2011/2012'
--EXAMPLE Calulate the number of goals every 2 maaches for league 8456 in season 2011/2012
SELECT date, home_goal, away_goal, 
		SUM(home_goal) 
		OVER(ORDER BY date
		ROWS BETWEEN 1 PRECEDING --1 PRECEDING refers to one row before the current
		AND CURRENT ROW) AS last2
FROM match
WHERE hometeam_id = 8456 AND season = '2011/2012'

