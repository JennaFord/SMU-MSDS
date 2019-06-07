# Homework for Week 5
Doing Data Science

This is the homework for week 5 with all associated files.

Here are a list of the files in this repository:
* yob2015.txt - input dataset
* yob2016.txt - input dataset
* top_10_girls.csv - output dataset
* Week5.Rmd - RMarkdown code
* Week5.html - html code

Two datasets were used in this assignment:
* https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/Unit%205/yob2015.txt
* https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/Unit%205/yob2016.txt

Both datasets have popular names given to infants. This homework is to help a client identify the top 10 most popular girl names from 2015 and 2016.

The yob2016.txt dataset is read into a dataframe labeled 'df'. The following column names are assigned:
* firstName
* gender
* numberChildren

One record with a firstName ending in 'yyy' is removed from the dataset. This output is stored in a dataframe labeled 'y2016'.

The yob2015.txt dataset is read into a dataframe labeled 'y2015'. The following column names are assigned (to match the 'y2016' dataframe):
* firstName
* gender
* numberChildren

A 'final' dataframe is created from the merge (inner join) between 'y2016' and 'y2015'. The keys for the merge are 'firstName' and 'gender'.

'final_girls' is created to contain the top 10 most populat girl names for 2015 and 2016 combined. This is printed out to a csv file 'top_to_girls.csv'.

For questions, please contact me at jennaf@mail.smu.edu.
