# Portfolio dashboard
A dashboard for the World Bank / IFC. Built in collaboration with [Databrew](http://datbrew.cc)

## Installing this package

Clone this repository, so that you can make modifications to some of the underlying code, data, and credentials. Do so like this:

```
$ git clone https://github.com/databrew/portfoliodash
```

Then `cd` into the `portfoliodash` directory. You'll note a `credentials/credentials.yaml` file. This is set up to assume an accessible, non-password protected "portfolio" database. If your database requires credentials, is running on a specific port, etc, add to the file in this format:

```
host: some.host.net
port: 5230 # or some other port number
user: ausername
password: apassword
```

If you're using a version control system (like git), **be careful**: the `credentials/credentials.yaml` file is not explicitly git-ignored, so you should be sure not to `git add` it, less you risk exposing your credentials to people who shouldn't have them.

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

Even though the library is set up, the app is not quite ready to run. Since some of the data is private, we don't store it as part of the library code - it needs to be set up separately. That's what the next section covers.

## Getting data  

Data were emailed to developers on January 7, 2018. Data are downloadable (to authorized collaborators) at https://drive.google.com/open?id=1EtT8CmyL3XktXs49YXI-XhYtMcSfFhYY. 

- Download the `as_portfolio (with data).zip` file from the above URL into the `data` directory of this repository.
- Extract the `.zip` file's contents in place. 
- Delete the `.zip` file.

## Setting up the database

- The below assumes you are running linux and have postgresql on your system.
- Run `psql` to get into an interactive postgresql console.
- Create a `portfolio` database by running: `CREATE DATABASE portfolio`.
- Connect to the database: `\connect portfolio;`
- Create a `portfolio` schema: `create schema portfolio;`
- Ctrl+d to get out of interactive psql session.
- Go into the data directory: `cd data`
- Extract the tables: `psql -d portfolio -f as_portfolio\ \(with\ data\).sql`
- Go back up a level: `cd ..`
- Open a psql session in portfolio db: `psql portfolio`
- Set the search path for the schema: `SET search_path TO portfolio;`
- Confirm that the tables are there: `\dt` should return:
```
             List of relations
  Schema   |     Name     | Type  |  Owner  
-----------+--------------+-------+---------
 portfolio | as_portfolio | table | joebrew
(1 row)
```
- Having confirmed the above, permanently alter the `search_path` for the portfolio database: `ALTER DATABASE portfolio SET search_path TO portfolio;`
- To query the table, you can use SQL as such:
```
SELECT * FROM portfolio.as_portfolio LIMIT 5;
```
- Go back into the data directory: `cd data`
- Load the as_results table into the database: `psql -d portfolio -f run_insert.sql`
- Open an interactive psql session (`psql portfolio`) and confirm the presence of the `as_results` relation: `\dt` should return
```
             List of relations
  Schema   |     Name     | Type  |  Owner  
-----------+--------------+-------+---------
 portfolio | as_portfolio | table | joebrew
 portfolio | as_results   | table | joebrew
(2 rows)
```

## Setting up the connections to OneDrive

- Much of the underlying data for this application relies on the World Bank's OneDrive. To get started, copy the contents of [this sharepoint url](https://worldbankgroup-my.sharepoint.com/personal/sburi_ifc_org/Documents/FIG%20SSA%20MEL/MEL%20Program%20Operations/Knowledge%20Products/Dashboards%20%26%20Viz/Portfolio%20Dashboard/portfolio_dashboard?csf=1&e=BZReQ1) to `inst/shiny`. 
- Then, move all `.csv` and files to `inst/shiny/data`:
```
├── factors.csv
├── fig_ssa_addtional_details.csv
├── longevity_data.csv
├── portfolio_funding_data.csv
└── portfolio_volume.csv
```

