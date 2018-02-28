drop MATERIALIZED view if exists view_portfolio_project_products;
create materialized view view_portfolio_project_products as
with product_splice as 
(
	select dataset_date,project_id,unnest(string_to_array(business_line_product_pcts,';')) as bl_p_pct
	from portfolio.as_portfolio
),
product_data_splice as
(
select 
dataset_date,
project_id,
bl_p_pct,
string_to_array(regexp_replace(trim(bl_p_pct),E'^(\\w{2,3})\\s+-\\s+(.+)(\\([-\\w]{1,3}\\)|\\(\\))\\s+(\\d{1,3})%$','\1#\2#\3#\4'),'#') pds
from product_splice
),
--business line in index because FIG and FAM share identical product types, see for example project_id=600611
--Dunno what this weird 'Â' character comes from -- but it's in there...
product_data_agg as
(
select dataset_date,project_id,coalesce(regexp_replace(trim(pds[2]),'Â',''),'UNKNOWN') as product_name,trim(pds[3]) as product_stage,sum(trim(coalesce(pds[4],'0'))::numeric(5,2)) as product_pct
from product_data_splice
group by dataset_date,project_id,coalesce(regexp_replace(trim(pds[2]),'Â',''),'UNKNOWN'),trim(pds[3])

)
select dataset_date,project_id,product_name,product_stage,product_pct,
dense_rank() over(partition by dataset_date,project_id order by product_pct desc) as pct_rank
from product_data_agg;

CREATE UNIQUE INDEX view_portfolio_project_products_index
  ON view_portfolio_project_products (dataset_date, project_id,product_name);

