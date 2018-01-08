# Portfolio dashboard
A dashboard for the World Bank / IFC. Built in collaboration with [Databrew](http://datbrew.cc)

## Data sources

Data were emailed to developers on January 7, 2018. Data are downloadable (to authorized collaborators) at https://drive.google.com/open?id=1EtT8CmyL3XktXs49YXI-XhYtMcSfFhYY. 

## Getting data  

- Download the two `.zip` files from the above URL into the `data` directory of this repository.
- Extract the `.zip` files' contents in place. 
- Delete the `.zip` files.

## Setting up the database

- Run `psql` to get into an interactive postgresql console.
- Create a `portfolio` database by running: `CREATE DATABASE portfolio`.
- Connect to the database: `\connect portfolio;`
- Create a `portfolio` schema: `create schema portfolio;`
- Ctrl+d to get out of interactive psql session.
- Go into the data directory: `cd data`
- Extract the tables: `psql -d portfolio -f run_insert.sql`
- Go back up a level: `cd ..`
- Open a psql session in portfolio db: `psql portfolio`
- Set the search path for the schema: `SET search_path TO portfolio;`
- Confirm that the tables are there: `\dt` should return:
```
             List of relations
  Schema   |     Name     | Type  |  Owner  
-----------+--------------+-------+---------
 portfolio | as_portfolio | table | joebrew
 portfolio | as_results   | table | joebrew
(2 rows)
```
- Having confirmed the above, permanently alter the `search_path` for the portfolio database: `ALTER DATABASE portfolio SET search_path TO portfolio;`