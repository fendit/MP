# (Monthly) Expenses Tracker in Python

Hello everyone! This week I decide to create something that is relevant to my daily life -- Expenses Tracker. It is a py.script can (i) read and write expenses record in an excel file called "Expenses.xlsx", (ii) show the expenses distribution in a donut chart. 

No worries! The script will, at your service, create an excel workbook for you if it does not exist in your directory.

* [Purposes](https://github.com/fendit/MP/tree/master/ExpensesTracker#purposes)
* [Objectives](https://github.com/fendit/MP/tree/master/ExpensesTracker#objectives)
* [Inputs](https://github.com/fendit/MP/tree/master/ExpensesTracker#inputs)
* [Outputs](https://github.com/fendit/MP/tree/master/ExpensesTracker#outputs)
* [Extra features or suggestions](https://github.com/fendit/MP/tree/master/ExpensesTracker#extra-features-or-suggestions)
* [References](https://github.com/fendit/MP/tree/master/ExpensesTracker#references)

### Purposes
This script is to show that:
1. It is quite straightforward to create your own expenses tracker
1. It is a good way to understand your previous consumption pattern
1. It is a cornerstone of planning how much to spend or save in upcoming months

### Objectives
1. Create a workbook with 12 sheets (each sheet represents a month)
1. Create a dataframe in Pandas that can store inputs
1. Export the dataframe to the designated sheet
1. Allow users to select between options (e.g. select which month to record expenses, which category the expenses belongs to)
1. Generate a donut chart of expenses distribution in terms of category

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
1. Workbook ("Expenses.xlsx")

### Extra features or suggestions
1. Connecting to google sheets (a way, I think, to get around with this is to store "Expenses.xlsx" in the google sync folder in the computer. Any changes in that workbook will be synchronised automatically)
1. Line plot of plotting the expenses across each month in terms of each category
1. Setting up limit (or budget) in each category
1. Warning message if the limit (budget) in each category is exceeded
1. Donut chart in terms of months (subplots)
1. GUI of Expenses Tracker with PyQt, tkinter etc

### References
I am new to Python and expenses recording, so I looked for some suggestions from following websites:
1. https://stackoverflow.com/questions/34927479/command-line-show-list-of-options-and-let-user-choose
1. https://stackoverflow.com/questions/20219254/how-to-write-to-an-existing-excel-file-without-overwriting-data-using-pandas
1. https://localfirstbank.com/content/personal-budget-categories/
