--POSTGRESQL SUMMARY STATS AND WINDOW FUNCTIONS
--__*Database:
--__Each row represents a medal awarded in the Summer Olympics Game
--Columns: Year, City, Sport, Discipline, Event, Athlete, Country, Gender, Medal
--__*WINDOW functions perform an operation ACROSS a set of rows (somehow) related to the current row.
--__*Similar to GROUP BY aggregate functions without removing records in the output.
--__*Fecth values from preceding or following rows.
--__*Calculate growth over time.
--__*Assigning ordinal ranks based on their position on a list.
--__*Running totals and averages.

--FUNCTIONS:::::ROW_NUMBER() Adds a column with each row's index
SELECT Year, Event, Country, 
		ROW_NUMBER() OVER() AS Row_N
FROM Summer_Medals
WHERE Medal = 'Gold'
--Anatomy of WINDOW FUNCITONS:
--FUNCTION_NAME() OVER(...)
--Where ... can be: ORDER BY, PARTITION BY, ROWS/RANGE, PRECEDING/FOLLOWING/UNBOUNDED.

--ORDER BY inside OVER() orders the row related to the current row.
--It is possible to ORDER inside and outside OVER
SELECT Year, Event, Country,
	ROW_NUMBER() OVER(ORDER BY Year DESC, Event ASC) AS Row_N
FROM Summer_Medals
WHERE Medal = 'Gold'
ORDER BY COuntry ASC, Row_N ASC;
--The order inside OVER takes place before the order outside.

--FUNCTION::::::LAG(column, n) OVER(...)
--This returns column's value at the row  before the current row.
WITH Discus_Gold AS(
	SELECT Year, Country AS Champion
	FROM Summer_Medals
	WHERE Year IN (1996, 2000, 2004, 2008, 2012)
	AND Gender = 'Men' AND Medal = 'Gold'
	AND Event = 'Discus Throw')
SELECT Year, Champion, LAG(Champion, 1) OVER (ORDER BY Year ASC) AS Last_Champion
FROM Discus_Gold
ORDER BY Year ASC;

--PARTITION BY inside OVER()
--EXAMPLE. Create the lag of each Event by year, without overalpping events.
--__NOTE: Basically, PARTITION BY lets you create columns in groups without summarizing the groups.
SELECT Year, Event, Champion, LAG(Champion) 
		OVER(PARTITION BY Event ORDER BY Event ASC Year ASC) AS Last Champion
FROM Discus_Gold
ORDER BY Event ASC, Year ASC;

--Fetching. Fetch values from different parts of the table into one row.
--__Relative functions:
--FUNCTION:::::LEAD(column, n) | LAG(column, n)
--__Absolute functions:
--FUNCTION:::::FIRST_VALUE(column) | LAST_VALUE(column)
--Working with absolute functions:
SELECT Year, City,
		FIRST_VALUE(City) OVER(ORDER BY Year ASC) AS FirstCity,
		LAST_VALUE(City) 
		OVER(ORDER BY Year ASC RANGE BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS Last_City
FROM Hosts
ORDER BY Year ASC;
--See that the LAST_VALUE NEEDS the RANGE BETWEEN otherwise the LAST_VALUE() will be tha same as the value in the current row.

--RANKING FUNCTIONS: ROW_NUMBER() | RANK() | DENSE_RANK() (this one does not skips values when there are ties)
--PAGING. This splits the data into approximately equal chunks.
--FUNCTION::::.NTILE(k-number)
--__This splits the data into a k-number of chunks with almost equal size.
WITH Disciplines AS(SELECT DISTINCT Discipline FROM Summer_Medals)
SELECT Discipline, NTILE(15) OVER(ORDER BY Medals DESC) AS Page
FROM Disciplines
ORDER BY Page ASC;

 
		



