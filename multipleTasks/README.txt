This .Rmd file is for doing a quick analysis of up to 7 tasks.

For the date.csv, ensure:
- col1 is the class
- col2 is the first_name
- col3 is the last_name
- col4 is task1 result
- col5 is task2 result
- col6 is task3 result
- col7 is task4 result
- col8 is task5 result
- col9 is task6 result
- col10 is task7 result

The results do not need to be percentages. The document will use z-scores.

In the .Rmd you can do quick configurations to the boundaries for the tables. Simply adjust the numbers in lines 10-13. The defaults are as follows:

	outstanding_lower_boundary <- 1.5
	high_lower_boundary <- .5
	basic_upper_boundary <- -.5
	limited_upper_boundary <- -1.5

Note these numbers are z-scores.

*** If it doesn't knit check to make sure the working directory is set to source file location. Else you can manually set the path to your data.csv in line 21. ***