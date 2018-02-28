--drop view portfolio.view_reporting_quarterly_timeseries;
create or replace view portfolio.view_reporting_quarterly_timeseries as
-- WARNING NOTE: This expects that portfolio data has a dataset FOR EACH month uploaded!  Ensure database is kept up-to-date
with quarter_dates as 
(
select distinct dataset_date, date_trunc('quarter',asp.dataset_date) as qtr_date,
dense_rank() over(partition by date_trunc('quarter',asp.dataset_date) order by dataset_date desc) as qtr_rank  from 
portfolio.as_portfolio asp
)
select
 asp.project_id,
 date_trunc('quarter',asp.dataset_date) as qtr_date,
 date_part('quarter',asp.dataset_date) as qtr,
 'CY' || (date_part('year',asp.dataset_date)-2000) || 'Q' || date_part('quarter',asp.dataset_date) as cy_qtr,

 case when lower(asp.project_stage) = 'completed' then 1 else 0 end as is_completed,
 case when
   (lower(asp.project_stage) = 'portfolio') or
	 (lower(asp.project_stage) = 'pipeline' and lower(asp.project_classification_type) = 'umbrella') -- Soren: note, to confirm that other project types be included?  Eg, 'business development'?
 then 1 else 0 end as is_active,
 case when lower(asp.project_stage) = 'pipeline' and lower(asp.project_classification_type) <> 'umbrella' then 1 else 0 end as is_pipeline,
 asp.total_funds_managed_by_ifc as project_size,
 asp.itd_expenditures
 -- Do we really need the extra meta data?  Might be faster to aggregate by period for selected portfolio and join meta data after
 -- 24.45s for full query to return dataset.  18.2s to return full query without meta data.  Although few queries should ask for 'all' anyway
 -- keep out for now
 --asp.primary_business_line_code,
 --asp.region_code,
 --asp.country
from 
portfolio.as_portfolio asp
where 
lower(asp.project_stage) in ('pipeline','portfolio','completed') and 
-- qtr_rank=1 gives us the end-quarter dataset (or most recently avilable dataset for the quarter, most applicable for start-of-quarter times)
exists(select * from quarter_dates qd where qd.dataset_date = asp.dataset_date and qd.qtr_rank = 1)
order by project_id,qtr_date asc