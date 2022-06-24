This .Rmd file is for doing a quick analysis of two assessment tasks.

For the date.csv, ensure:
- col1 is the class
- col2 is the name
- col3 is task1 result (out of 100)
- col4 is task2 result (out of 100)

In the .Rmd you can do quick configurations of the groupings in the first graph of high achieving, improved, worse and under achieving students. To do so just adjust the numbers in lines 23-26

	high <- 1.5
	increase <- 1
	decrease <- 1
	low <- -1.5

The numbers are z-scores. i.e. by default the improved students (increase) that get listed are the ones that have a z-score increase of 1.

The clusters down the bottom are k-means clusters.

*** If it doesn't knit check to make sure the working directory is set to source file location. Else you can manually set the path to your data.csv in line 14. ***