create or replace view portfolio.view_reporting_portfolio_longevity as
SELECT 
    casp.dataset_date,
    casp.project_id,
    casp.project_name,
		lower(casp.project_stage) as project_stage,
    least(casp.implementation_start_date,casp.as_implementation_plan_approval_date) as project_start_date,
    CASE
        WHEN lower(casp.project_stage) = 'completed' THEN casp.as_completion_approval_date
        ELSE casp.implementation_end_date
    END AS project_end_date,
		casp.implementation_financial_close_date as financial_end_date,
    casp.project_status,
    casp.country,
    casp.region_code,
    casp.primary_business_line_code,
    casp.total_funds_managed_by_ifc as total_project_size,
    casp.itd_expenditures as total_project_expenditures
   FROM portfolio.view_current_as_portfolio casp