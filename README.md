# Portfolio dashboard
A dashboard for the World Bank / IFC. Built in collaboration with [Databrew](http://datbrew.cc)

## Installing this package

Clone this repository, so that you can make modifications to some of the underlying code, data, and credentials. Do so like this:

```
$ git clone https://github.com/databrew/portfoliodash
```


## Building the app as a package

Having now set up your credentials, document and install the package from within R like this (make sure you're in the "portfoliodash" directory):

```
library(devtools)
document('.')
install('.')
```

You'll now have the package on your system. You can confirm this by running:
```
library(portfoliodash)
```

## Connecting to the database

Next `cd` into the `portfoliodash` directory. You'll note a `credentials/credentials.yaml` file. This should have one of the w following formats:


*Format 1: for running on WB servers:* 

```
dbname: portfolio
host: "w0lxsfigssa01"
port: 5432
user: "rscript"
password: <PASSWORD GOES HERE>
```

*Format 2: for running on AWS servers:* 

```
host: "databrewdb.cfejspjhdciw.us-east-2.rds.amazonaws.com"
port: 8080
dbname: portfolio
user: "worldbank"
password: <PASSWORD GOES HERE>
```


If you're using a version control system (like git), **be careful**: the `credentials/credentials.yaml` file is not explicitly git-ignored, so you should be sure not to `git add` it, less you risk exposing your credentials to people who shouldn't have them.

## Getting data onto local database

(The below only applies to database managers, not an R-only programmer or someone just running the app.)

You've now set up the package, but you also need to set up the data on which this package relies. There are two data dependencies: some flat files, and a PostgreSQL database.

### Flat files

- Much of the underlying data for this application relies on the World Bank's OneDrive. 
- If you have access to the World Bank's Sharepoint, copy the contents of [this sharepoint url](https://worldbankgroup-my.sharepoint.com/personal/sburi_ifc_org/Documents/FIG%20SSA%20MEL/MEL%20Program%20Operations/Knowledge%20Products/Dashboards%20%26%20Viz/Portfolio%20Dashboard/portfolio_dashboard?csf=1&e=BZReQ1) to the main directory. Note, this comes with a lot of extra stuff.
- If you don't have access to the World Bank's Sharepoint, copy the flat files from this [google drive folder](https://drive.google.com/open?id=16dc1BUjjtSTc372U2qLcgMtlG5Hc-ZwS) to the main directory
- Then, move all `.csv` and files to `flat_files`:
```
├── factors.csv
├── fig_ssa_addtional_details.csv
├── longevity_data.csv
├── portfolio_funding_data.csv
└── portfolio_volume.csv
```

### The database

In production the database will be maintained by the World Bank. For development purposes, one can create a database locally for testing.

#### Production

The production database is expected to be named "portfolio", and to have the following 7 relations:

```
                 List of relations
  Schema   |         Name         | Type  |  Owner  
-----------+----------------------+-------+---------
 portfolio | as_portfolio         | table | joebrew
 portfolio | as_results           | table | joebrew
 portfolio | portfolio_indicators | table | joebrew
 portfolio | portfolio_projects   | table | joebrew
 portfolio | portfolio_users      | table | joebrew
 portfolio | portfolios           | table | joebrew
 portfolio | users                | table | joebrew

```

If you don't have a "portfolio" database set up, or if you do but need to create one of the above relations, continue reading.

##### Setting up the database from scratch.

Data were emailed to developers on January 7, 2018. Data are downloadable (to authorized collaborators) at https://drive.google.com/open?id=1EtT8CmyL3XktXs49YXI-XhYtMcSfFhYY. 

- The below assumes you are running linux and have postgresql on your system.
- Download the `as_portfolio (with data).zip` file from the above URL into the `data` directory of this repository.
- Extract the `.zip` file's contents in place. 
- Delete the `.zip` file.
- Run `psql` to get into an interactive postgresql console.
- Create a `portfolio` database by running: `CREATE DATABASE portfolio`.
- Connect to the database: `\connect portfolio;`
- Create a `portfolio` schema: `create schema portfolio;`
- Ctrl+d to get out of interactive psql session.

##### Setting up the as_portfolio table

- Go into the data directory: `cd data`
- Extract the tables: `psql -d portfolio -f as_portfolio\ \(with\ data\).sql`
- Go back up a level: `cd ..`
- Open a psql session in portfolio db: `psql portfolio`
- Confirm that the tables are there: `\dt portfolio.*` should return:
```
             List of relations
  Schema   |     Name     | Type  |  Owner  
-----------+--------------+-------+---------
 portfolio | as_portfolio | table | joebrew
(1 row)
```

##### Setting up the as_results table

- Go back into the data directory: `cd data`
- Load the as_results table into the database: `psql -d portfolio -f run_insert.sql`
- Open an interactive psql session (`psql portfolio`) and confirm the presence of the `as_results` relation: `\dt portfolio.*` should return
```
                 List of relations
  Schema   |         Name         | Type  |  Owner  
-----------+----------------------+-------+---------
 portfolio | as_portfolio         | table | joebrew
 portfolio | as_results           | table | joebrew
```

Then, populate the `as_results` relation with a csv sent by Soren.

```
> library(portfoliodash)
> portfoliodash::populate_as_results()
```

##### Setting up the users table

The users table contains information on application users: id, email address, upi, etc. To create an initial users table, run the following in R:

```
> portfoliodash::create_users_db()
```

##### Setting up the portfolio_projects table

The `portfolio_projects` table is meant to store portfolios are associated with which projects. Each portfolio is associated with > 0 projects. To set up an initial table in R, run: 

```
> portfoliodash::create_portfolio_projects_db()
```

##### Setting up the portfolio_users table

The `portfolio_users` table is meant to store which portfolios are associated with each user (a user having between 0 and inf portfolios). To create an initial table, run the following in R:

```
> portfoliodash::create_portfolio_users_db()
```
##### Setting up the portfolios table

The `portfolios` table is meant to store which projects are associated with which portfolios. To set it up initially, run the following in R:

```
> portfoliodash::create_portfolios_db()
```

##### Setting up the portfolio_indicators table

The `portfolio_indicators` table needs to be set up as well. 

```
> portfoliodash::create_portfolio_indicators_db()
```

##### Inspecting the database

The database is now set up and ready for use. To inspect the entire schema run the following from the psql console:

```
select table_schema, table_name, column_name, data_type from information_schema.columns where table_schema = 'portfolio';
```

##### Creating a dump

Having now created the database from scratch, consider generating a dump using the pg_dump utility (for the purposes of backup or upload to AWS servers):

```
pg_dump -d portfolio -f /path/to/local/destination.sql
```


##### Getting data onto AWS database

This section only applies to the person managing the AWS database. Post-dump (ie, the above steps), data can be uploaded to the AWS database.

- Open a psql session within our AWS DB instance.

```
psql --host=portfolio.cfejspjhdciw.us-east-2.rds.amazonaws.com --port=8080 --username=joebrew --dbname=portfolio 
```

- Restore the locally created dump from within psql
``` 
\i /path/to/dump/portfolio.sql
```


Create a user named worldbank, and grant privileges.

```
create role worldbank with password '<PASSWORD HERE>' login;
grant rds_superuser to worldbank;
```

Ctrl+d to log out, and then log in as worldbank to confirm it's working:

```
psql --host=portfolio.cfejspjhdciw.us-east-2.rds.amazonaws.com --port=8080 --username=worldbank --dbname=portfolio 
```


## Other utilities

The `portfoliodash` package contains utilities for accessing the database. For further information, read the documentation associated with `credentials_extract`, `credentials_connect` and `get_data`. Here is a typical use case:

```
# Get credentials
creds <- credentials_extract()
# Connect to the database
co <- credentials_connect(creds)
# Get data
up <- get_data(query = 'SELECT * FROM portfolios.user_portfolio', connection_object = co)
```
