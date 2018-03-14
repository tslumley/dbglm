This package fits generalised linear models to moderately large data sets stored in a relational database.  The code has implementations for MonetDB and SQLite, but should be easy to adapt to any other database that has EXP and RAND. 

The code takes a subsample of the data, fits the model in memory, then improves the estimate with one step of Fisher scoring computed with a single SQL aggregation query.

