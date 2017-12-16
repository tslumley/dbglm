This package fits generalised linear models to moderately large data sets stored in a relational database (preferably 
MonetDB, but anything with an EXP function will do). 

The code takes a subsample of the data, fits the model in memory, then improves the estimate with one step of Fisher scoring computed with a single SQL aggregation query.

