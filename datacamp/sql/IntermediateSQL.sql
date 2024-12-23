--MULTIPLE FILTERS
--__WHERE cond1 OR cond2
--__WHERE cond1 AND cond2
--__MULTIPLE CONDITIONS
--__WHERE (cond1 OR cond2) AND (cond3 OR cond4)
--__However for filtering a range of values writing the name of the field multiple times can be cumbersome
--__Therefore there is the BETWEEN operator:
SELECT *
FROM tab
WHERE yearX >= 2000 AND yearX <=2010;
--With BETWEEN:
SELECT *
FROM tab
WHERE yearX BETWEEN 2000 AND 2010 --The limits are inclusive
--FILTERING TEXT
--__LIKE, NOT LIKE (for patterns) and IN
--__For LIKE there are jackcards those are: % and _
--__% matches zero, one or many characters, while _ matches one CHARACTER
SELECT *
FROM tab
WHERE name LIKE 'Ev%' AND album LIKE 'Ri__'
--__LIKE and NOT LIKE are case sensitive.
--__IN allows us to specify multiple values 
SELECT title
FROM films
WHERE release_year IN (1920, 1930, 1940);
--MISSING VALUES
--In SQL null represents empty values: human error, unknown, not available
--__One way to narrow down lists with nulls is to use IS NULL alongside with WHERE
SELECT name
FROM people
WHERE birthdate IS NULL-- this returns names whose birthdates are unknown
SELECT COUNT(*) AS no_birthdates
FROM people
WHERE birthdate IS NULL--to count how many records have no birthdate
--__We could use IS NOT NULL to filter out missing values
--ARITHMETIC
--__We can add, substract, multiply and divide are given by +, -, *, /
--__Careful when using / as using integers returns an INTEGER
SELECT (4/3); --This will return 1 as SQL nderstands that an integer must be returned.
--AGGREGATING functiions perform calculations by columns, while ARITHMETIC functions
--__perform calculations by rows.
--ALIASING WITH ARITHMETIC
SELECT (gross-budget) AS profit
FROM films;
--ORDER BY function
--This sorts the records by a given field_name. The default is smallest to biggest.
--We could use the ASC to indicate ascending order or DESC for descending order:
SELECT name, budget
FROM films
WHERE budget IS NOT NULL
ORDER BY budget DESC;
--__We do not need to select the field we are ordering on.
--__It can be used to select multiple fields separated by a comma, where following fields
--__are used as tie-breakers.
--__ORDER OF EXECUTION: FROM >WHERE >SELECT >ORDER BY >LIMIT
SELECT name, budget
FROM films
WHERE budget IS NOT NULL
ORDER BY budget DESC, name;
--GROUP BY
--__Allows to make calculations within certain groups of data.
SELECT COUNT(deathdate) AS number_deaths
FROM people
GROUP BY country;
--__We can combine the results with the ORDER BY function and use aliases in SELECT as ORDER BY exceutes after 
SELECT COUNT(deathdate) AS number_deaths
FROM people
GROUP BY country
ORDER BY number_deaths;
--NOTE: ORDER BY is always written after GROUP BY
--HAVING. This is used for filtering groups after they are conformed.
--__WE CANNOT USE WHERE TO FILTER GROUPS BECAUSE OF THE ORDER OF EXECUTION
--__since the ORDER OF EXECUTION OOE is: FROM WHERE GROUP BY SELECT ORDER BY LIMIT
--__WHERE will not be able to filter groups so we reserve the word HAVING for this purpose
SELECT release_year, COUNT(DISTINCT language) AS no_lang
FROM films
WHERE release_year>1990
GROUP BY country
HAVING COUNT(DISTINCT language) > 1
ORDER BY no_lan DESC;
--WHERE filters individual records while HAVING filters grouped records.


