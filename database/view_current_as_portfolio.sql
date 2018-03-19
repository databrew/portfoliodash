DROP MATERIALIZED VIEW "portfolio"."view_current_as_portfolio";

CREATE MATERIALIZED VIEW "portfolio"."view_current_as_portfolio"
AS
SELECT as_portfolio.dataset_date,
    as_portfolio.project_id,
    as_portfolio.project_name,
    as_portfolio.project_stage,
    as_portfolio.project_status,
    as_portfolio.project_type,
    as_portfolio.project_classification_type,
    as_portfolio.project_tier,
    as_portfolio.region_code,
    as_portfolio.region_name,
    as_portfolio.country,
    as_portfolio.geographic_focus,
    as_portfolio.regional_project_country_names,
    as_portfolio.pipeline_12_months,
    as_portfolio.related_project_id,
    as_portfolio.primary_business_line_code,
    as_portfolio.primary_business_line_name,
    as_portfolio.primary_business_area,
    as_portfolio.business_area_level1_classification,
    as_portfolio.business_area_level2_classification,
    as_portfolio.business_area_level3_classification,
    as_portfolio.business_area_level4_classification,
    as_portfolio.business_line_product_pcts,
    as_portfolio.primary_products,
    as_portfolio.tertiary_sector_pct,
    as_portfolio.ifc_wbg_collaboration,
    as_portfolio.project_leader,
    as_portfolio.manager,
    as_portfolio.finance_officer,
    as_portfolio.owning_department_code,
    as_portfolio.owning_division_code,
    as_portfolio.as_concept_note_or_as_implementation_plan_approved,
    as_portfolio.as_concept_note_approval_date,
    as_portfolio.as_implementation_plan_approval_date,
    as_portfolio.fy_of_as_implementation_plan_approval,
    as_portfolio.pre_implementation_start_date,
    as_portfolio.pre_implementation_end_date,
    as_portfolio.pre_implementation_financial_close_date,
    as_portfolio.implementation_start_date,
    as_portfolio.implementation_end_date,
    as_portfolio.implementation_financial_close_date,
    as_portfolio.implementation_duration_days,
    as_portfolio.post_implementation_start_date,
    as_portfolio.post_implementation_end_date,
    as_portfolio.post_implementation_financial_close_date,
    as_portfolio.as_completion_approval_date,
    as_portfolio.fy_of_as_completion_approval,
    as_portfolio.dropped_terminated_date,
    as_portfolio.crm_go_date,
    as_portfolio.crm_decision_type,
    as_portfolio.total_project_size,
    as_portfolio.total_funds_managed_by_ifc,
    as_portfolio.total_funding,
    as_portfolio.pooled_trust_funds,
    as_portfolio.proposed_partner_donor_funding,
    as_portfolio.ifc_funding,
    as_portfolio.client_cash_contribution_funding,
    as_portfolio.unidentified_funding,
    as_portfolio.other_sources,
    as_portfolio.reimbursable_fees,
    as_portfolio.advisory_fees,
    as_portfolio.success_fees,
    as_portfolio.total_additional_contributions,
    as_portfolio.total_in_kind_contributions,
    as_portfolio.client_in_kind_contribution,
    as_portfolio.client_parallel_contribution,
    as_portfolio.total_client_additional_contributions,
    as_portfolio.third_party_in_kind_contribution,
    as_portfolio.third_party_parallel_contribution,
    as_portfolio.proposed_total_client_cash_fees,
    as_portfolio.total_client_contributions,
    as_portfolio.total_parallel_contributions,
    as_portfolio.other_cash_contribution,
    as_portfolio.secured_funding_amount,
    as_portfolio.secured_cash_contributions,
    as_portfolio.secured_parallel_contributions,
    as_portfolio.secured_in_kind_contributions,
    as_portfolio.total_fytd_actual_cash_contributions,
    as_portfolio.total_itd_actual_cash_contributions,
    as_portfolio.total_fytd_expenditures,
    as_portfolio.itd_expenditures,
    as_portfolio.fy_18_ida_lending_eligibility,
    as_portfolio.share_of_funds_managed_by_ifc_in_ida_countries,
    as_portfolio.share_of_ida_countries_in_total_countries,
    as_portfolio.share_of_sub_projects_in_ida_countries,
    as_portfolio.ida_in_fy_of_approval,
    as_portfolio.ida_pct,
    as_portfolio.ida_total_funds_managed_by_ifc,
    as_portfolio.ida_fytd_expenditure,
    as_portfolio.fy_18_fcs_flag,
    as_portfolio.fcs_in_fy_of_approval,
    as_portfolio.regional_fcs_pct,
    as_portfolio.fcs_pct,
    as_portfolio.fcs_total_funds_managed_by_ifc,
    as_portfolio.fcs_total_fytd_expenditures,
    as_portfolio.climate_adaptation_pct,
    as_portfolio.climate_mitigation_pct,
    as_portfolio.special_green_pct,
    as_portfolio.climate_change_pct,
    as_portfolio.climate_change_total_funds_managed_by_ifc,
    as_portfolio.climate_change_total_fytd_expenditures,
    as_portfolio.frontier_regions_pct_of_non_ida_country_specific,
    as_portfolio.frontier_regions_pct_of_non_ida_regional_and_country_specific,
    as_portfolio.frontier_regions_of_non_ida_countries_total_funds_managed_ifc,
    as_portfolio.frontier_regions_of_non_ida_countries_fytd_expenditures,
    as_portfolio.sme_flag,
    as_portfolio.sme_total_funds_managed_by_ifc,
    as_portfolio.sme_total_fytd_expenditures,
    as_portfolio.gender_pct,
    as_portfolio.gender_total_funds_managed_by_ifc,
    as_portfolio.gender_total_fytd_expenditures,
    as_portfolio.infra_pct,
    as_portfolio.infra_total_funds_managed_by_ifc,
    as_portfolio.infra_total_fytd_expenditures,
    as_portfolio.agri_pct,
    as_portfolio.agri_total_funds_managed_by_ifc,
    as_portfolio.agri_total_fytd_expenditures,
    as_portfolio.job_flag,
    as_portfolio.job_flag_comment,
    as_portfolio.frontier_markets_pct,
    as_portfolio.frontier_markets_total_funds_managed_by_ifc,
    as_portfolio.frontier_markets_fytd_expenditures,
    as_portfolio.health_and_education_total_funds_managed_by_ifc,
    as_portfolio.health_and_education_total_fytd_expenditures,
    as_portfolio.financial_markets_total_funds_managed_by_ifc,
    as_portfolio.financial_markets_total_fytd_expenditures,
    as_portfolio.firm_specific_pct,
    as_portfolio.individual_firms_pct,
    as_portfolio.ifc_investee_pct,
    as_portfolio.potential_investee_pct,
    as_portfolio.other_firms_pct,
    as_portfolio.groups_of_firms_pct,
    as_portfolio.priority_sector_pct,
    as_portfolio.other_groups_of_firms_pct,
    as_portfolio.enabling_environment_pct,
    as_portfolio.industry_or_sector_specific_pct,
    as_portfolio.priority_sector_specific_pct,
    as_portfolio.other_sector_pct,
    as_portfolio.economy_wide_pct,
    as_portfolio.knowledge_management_pct,
    as_portfolio.product_development_pct,
    as_portfolio.other_pdp_pct
   FROM portfolio.as_portfolio
  WHERE (as_portfolio.dataset_date = ( SELECT max(as_portfolio_1.dataset_date) AS max
           FROM portfolio.as_portfolio as_portfolio_1));

ALTER MATERIALIZED VIEW "portfolio"."view_current_as_portfolio" OWNER TO "pguser";

CREATE UNIQUE INDEX "view_current_as_portfolio_project_id" ON "portfolio"."view_current_as_portfolio" USING btree (
  "project_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);