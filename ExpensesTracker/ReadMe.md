# Expenses Tracker in Python

Hello everyone! This week I decide to create something that is relevant to my daily life -- Expenses tracker. It is a py.script to read and write expenses record in an excel file called "Expenses.xlsx". No worries! The script will create an excel workbook for you if it does not exist in your directory.

### Inputs
1. No of items of expenses for recording
1. Date of expenses
1. Name of expenses
1. Quantity of expenses
1. Amount of expenses
1. Category of expenses (can be customised under the list called "categories" inside the script)

### Outputs
1. Pandas' dataframe of the monthly expenses record
1. Donut chart of the monthly expenses record (in terms of Category)

### Extra features or suggestions
1. Line plot of plotting the expenses across each month in terms of each category
1. Setting up limit (or budget) in each category
1. Warning message if the limit (budget) in each category exceeds
1. Donut chart in terms of months (subplots)

### Intuition
This script is to show that:
1. It is quite straightforward to create your own expenses tracker
1. It is a good way to understand your previous consumption pattern
1. It is a cornerstone of planning how much to spend or save in the following months

### References
I am new to Python and expenses recording, so I looked for some suggestions from following websites:
1. https://stackoverflow.com/questions/20219254/how-to-write-to-an-existing-excel-file-without-overwriting-data-using-pandas
1. https://localfirstbank.com/content/personal-budget-categories/
