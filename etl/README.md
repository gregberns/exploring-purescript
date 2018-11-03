# ETL

Lets use an ETL process to explore how PureScript works.

Here are the basic requirements:

* Read a CSV
* Parse CSV
* Transform from one layout to another 
  * Map column names from one name to another
* Validate Data
  * Assign types to columns
  * Ensure data abides by the data type requirements
  * Support basic types (string, int, double)
  * Support complex data types (ZIP, Phone, CC)
* Check column IDs exist in the database


## Sample dataset

```
Customer_Id, Name, Phone, Email
1234, John Doe, 800-777-7890, jdoe@gmail.com
2345, Jane Wilson, 312-748-2100, janed@gmail.com
3456, Gregory House, 734-432-8345, ghouse@princetonhc.org
```





# Notes:

* Getting a project up, building and the repl working was totally pain-free and took 5-10 minutes at most.
* Array to List was painful to understand
* List of Lists was painful
* imported `purescript-sequences` and it asked me to resolve what it should use... painful


## Issues

#### 

`Unable to find a suitable version for purescript-prelude`

`bower install purescript-sequences` and `bower install purescript-foldable-traversable`

Solution
Deleting `bower_components/` did it
