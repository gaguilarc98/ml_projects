--FUNCTIONS FOR MANIPULATING DATA IN PostgreSQL
--Extend the capabilities of PostgreSQL
--__* Common data types
--__* Date and time FUNCTIONS
--__* Parsing and man´pulating text
--__* Full text search
--DATA TYPES
--* TEXT: CHAR, VARCHAR, TEXT (Fixed or varied number of string data)
--* NUMERIC: INT, DECIMAL
--* DATE/TIME: DATE, TIME, TIMESTAMP,INTERVAL
--* Arrays
--EXAMPLE: Look at the data types in the schema
 -- Select all columns from the TABLES system database
 SELECT * 
 FROM INFORMATION_SCHEMA.TABLES
 -- Filter by schema
 WHERE table_schema = 'public';
 --EXAMPLE: -- Get the column name and data type
SELECT
 	column_name, 
    data_type
-- From the system database information schema
FROM INFORMATION_SCHEMA.COLUMNS 
-- For the customer table
WHERE table_name = 'customer';

--DATE AND TIME DATA TYPES
--TIMESTAMPS have the date value and the time value with microsecond precision.
--SQL uses ISO 8601 format yyyy-mm-dd
--DATE and TIME can be gotten from TIMESTAMPS separaing DATE and TIME respectively.
--INTERVAL is a period of time in years, months, days, etc.
SELECT rental_Date + INTERVAL '3 days' AS expected_return
FROM rental;

--ARRAY data types. Multidimensional arrays of any length.
--They are usually needed when creating a table and aggregating an entire column at once:
CREATE TABLE my_first_table(
	first_column_text,
	second_column integer
);
INSERT INTO my_first_table 
	(first_column, second_column) VALUE ('text value',12)--Adds a record to the created table.
--To create an array we have to add square brackets:
CREATE TABLE grade(
	student_id int,
	email text[][],--to store the email and the adress
	test_scores int[]
);
INSERT INTO grades VALUES 
(1, '{{"work","wortk1@datacamp.com"},{"other","other1@datacamp.com"}}',
'{92,85,96,88}')
--ACCESSING ARRAYS IN SQL
SELECT email[1][1] AS type,
		email[1][2] AS address,
		test_scores[1]
FROM grades;
--The same notation is used for filtering:
SELECT email[1][1] AS type,
		email[1][2] AS address,
		test_scores[1]
FROM grades;
WHERE email[1][1] = 'work';
--ANY function, to return any column of the array meeting a criteria
SELECT email[2][1] AS type,
		email[2][2] AS address,
		test_scores[1]
FROM grades;
WHERE 'other' = ANY(email);

 --CONTAINS OPERATOR:
 SELECT email[2][1] AS type,
		email[2][2] AS address,
		test_scores[1]
FROM grades;
WHERE email @> ARRAY['other'];

--ADD AND SUBSTRACT DATE AND TIME VALUES
--__Retireve CURRENT_DATE, CURRENT_TIMESTAMP, NOW()
--__AGE()
--__EXTRACT(), DATE_PART(), DATE_TRUNC()
--Adding and substracting dates:
SELECT date '2005-09-11' - date '2005-09-10';--This gives the time difference in days.
SELECT date '2005-09-11' -integer '3';--This substracts 3 days fromt the date.
--Adding and substracting timestamps:
SELECT date '2005-9-11 00:00:00' - date '2005-9-09 12:00:00'--This gives an interval
--AGE(timestamp1, timestamp2)gives the difference between 2 timestamps.
SELECT AGE(timestamp '2005-9-11 00:00:00', timestamp '2005-9-09 12:00:00')--This gives an interval
--__Arithmetic using INTERVALS
--To add or substract time to dates other than days we could use INTERVALS 
--together with multiplication and division as shown:
SELECT timestamp '2019-05-01' + 21 * INTERVAL '1 day'
--GETTING CURRENT TIME AND DATE at different levels of precission.
--NOW() gets the current timestamp at a microsecond precision.
SELECT NOW()
SELECT NOW::timestamp; -- to explicitly get a timsetamp without time zone.
--______________
--NOTE: This syntax is specific to PostgreSQL and is not part of the standard.
--______________
--CAST() allows to convert one data type to another.
SELECT CAST(NOW() AS timestamp);
--Specific to PostgreSQL:
SELECT CURRENT_TIMESTAMP; --Same output as NOW()
--However with CURRENT_TIMESTAMP(precision) we can put a precission integer paremeter.
--Similarly:
SELECT CURRENT_TIME;--With timezone and without the date.
SELECT CURRENT_DATE;

--EXTRACTING AND TRANSFORMING DATE AND TIME
--Sometimes we want to make analysis based on date parts such as month, year.
--EXTRACT(field FROM source) 
--DATE_PART('field', source)
--Where field can be 'year', 'month', 'quarter', 'day of week', etc.
--and source is a proper timestamp.
--DATE_TRUNC('field', timestamp)
SELECT DATE_TRUNC('year', TIMESTAMP '2005-05-21 15:30:30')

--REFORMATTING STRING AND CHARACTER DATA
--Parse string and caharacter data. Determine length and truncate.
--STRING CONCATENATION
SELECT first_name, last_name, first_name || ' ' || last_name AS full_name
FROM customer
--PostgreSQL has a built-in function:
SELECT CONCAT(first_name,' ',last_name) AS full_name
FROM customer;
--SQL also allows to concatenate string and non-string data.
--CHANGING THE CASE OF STRING
SELECT INITCAP(name), LOWER(email), UPPER(title)-- To change case of strings
FROM customer;
--REPLACING CHARACTERS IN A STRING
SELECT REPLACE(description, 'A Astounding', 'An Astounding') AS description
FROM film;
--REVERSE function to get a string in reverse order.
SELECT title, REVERSE(title)
FROM film as f;
--CHAR_LENGTH(string) returns the length of a string
--LENGTH() also provides the same output
--POSITION('char' IN string) returns the position at which the char is located in the string
SELECT email, POSITION('@' IN email)
FROM customer;
--STRPOS(email, '@') performs the same operation.
--PARSING STRINGS
--The following functions are self explanatory.
SELECT LEFT(description, 50), RIGHT(description, 50), SUBSTRING(decritpion, 10, 50)
FROM film;
--We could combine POSITION with SUBSTRING
--EXAMPLE: Select substring to the left of '@'
SELECT SUBTRING(email FROM 0 FOR POSITION('@' IN  email))
--where FROM defines the staring parameter and the FOR the ending parameter.
FROM customer;
--EXMAPLE: Select the substring to the right of '@'
SELECT SUBSTRING(email FROM POSITION('@' IN email)+1 FOR CHAR_LENGTH(email))
FROM customer;
--SUBSTR() is analogous to SUBSTRING() but does not allow the FROM FOR syntax.

--REMOVING WHITESPACES FROM STRINGS
--TRIM([leading | trailing | both] [characters] from string)
--Removes characters from either the start or end of a string or both.
--The first argument is optional (default:both). 
--The second parameter is also optional and is the character to be removed form the string (default: ' ')
--LTRIM() RTRIM() are analogous but only remove from the left or the right.

--LPAD(string, length, 'fill') this fills the string with fill so as to be of specified length (fills from the left)
--NOTE: The default fill is ' ' and if the length is less than the string length the string is parsed.
/*
CHALLENGE EXAMPLE:  truncate text fields like the film table's description column without cutting off a word.
To accomplish this we will use the REVERSE() function to help determine the position of the
last whitespace character in the description before we reach 50 characters. 
This technique can be used to determine the position of the last character that you want 
to truncate and ensure that it is less than or equal to 50 characters AND does not cut off a word.
*/
SELECT 
  UPPER(c.name) || ': ' || f.title AS film_category, 
  -- Truncate the description without cutting off a word
  LEFT(description, 50 - 
    -- Subtract the position of the first whitespace character
    POSITION(
      ' ' IN REVERSE(LEFT(description, 50))
    )
  ) 
FROM 
  film AS f 
  INNER JOIN film_category AS fc 
  	ON f.film_id = fc.film_id 
  INNER JOIN category AS c 
  	ON fc.category_id = c.category_id;

--FULL-TEXT SEARCH
--Remember thath LIKE operator can be used to search for a pattern in strings
--Using wildcards % and _ as placeholders.
--However LIKE is case-sensitive to the pattern you give it to.
SELECT title, description
FROM film
WHERE title LIKE '@elf@' --returns no records.
--In contrast:
SELECT title, description
FROM film
WHERE to_tsvector(title) @@ to_tsquery('elf')
/*
Full text search provides a means for performing natural language 
queries of text data by using: stemming, fuzzy string matching (for catching spelling mistakes)
and a mechanism to rank results by similarity. 
*/
SELECT to_tsvector(description)--to see how a tsvector object looks like
FROM film;

--PostgreSQL provides a way to create custom functions
--USER DEFINED DATA TYPE
CREATE TYPE dayofweek AS ENUM(
	'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday', 
)
--THEN 
SELECT typname typcategory
FROM pg_type
WHERE typname='dayofweek'

--USER defined function is equivalent of a stored procedure where we can 
--bundle several SQL queries and statements into a single package.
CREATE FUNCTION squared(i integer) RETURNS integer AS $$
	BEGIN 
		RETURN i*i;
	END;
$$ LANGUAGE plpgsql;

SELECT squared(10);

--PostgreSQL extensions.
--The most common are: PostGIS, PosrtPic, fuzzystrmatch, pg_trgm
--Any extension can be loaded into SQL by adding a CREATE EXTENSION command
CREATE EXTENSION IF NOT EXISTS fuzzystrmatch;
SELECT extname FROM pg_extension;
--FROM FUZZY STRING MATCH:
SELECT levenshtein('GUMBO', 'GAMBOL'); --Returns the number of steps to change from string1 to string 2.
SELECT similarity('GUMBO', 'GAMBOL'); --returns a number between 0 and 1 of the similarity based on tri






