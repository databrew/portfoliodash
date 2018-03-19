-- This should be called after uploading new monthly portfolio datasets
create or replace function portfolio.util_update_as_portfolio() returns void as
$body$begin

raise notice 'Refreshing view_portfolio_project_products';
-- Ensure portfolio_project_products is up-to-date
REFRESH MATERIALIZED VIEW portfolio.view_portfolio_project_products;

raise notice 'Updating portfolio with primary products in comma-separated-string values';

-- Will look across all datasets and update, including newest -- any that are null
with new_primary_products as
(
select 
asp.dataset_date,
asp.project_id,
coalesce(array_agg(distinct vppp.product_name order by vppp.product_name asc),
array['UNKNOWN']) as product
from portfolio.as_portfolio asp
left join portfolio.view_portfolio_project_products vppp on vppp.dataset_date = asp.dataset_date and vppp.project_id = asp.project_id and vppp.pct_rank = 1
where asp.primary_products is null 
group by asp.dataset_date,asp.project_id
)

update portfolio.as_portfolio
set primary_products = npp.product
from new_primary_products npp
where npp.dataset_date = as_portfolio.dataset_date and npp.project_id = as_portfolio.project_id;

raise notice 'Updating view_current_as_portfolio';

-- If new dataset updated primary_products[] then ensure the current_as_portfolio view is also updated
REFRESH MATERIALIZED VIEW portfolio.view_current_as_portfolio;


end$body$
language plpgsql;