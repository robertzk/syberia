# TODO: Make these dynamic
raw_data <- read.csv("~/avant-credit-model/data/modeling_query_results_10_09_13_loan_purpose.csv")

bad_variables <- c( #'customer_id',
  'transunion_credit_report_id', 'transunion_credit_report_id.1',  'transunion_credit_report_id.2', 'transunion_credit_report_id.3',
  'id', 'id.1', 'credit_report_id', 'credit_report_id.1', 'credit_report_id.2', 'credit_report_id.3', 'credit_report_id.4', 'id.2', 'id.3', 'id.4',
  'id.6', 'id.7', 'created_at', 'created_at.1', 'created_at.2', 'created_at.3', 'created_at.4', 'updated_at', 'updated_at.1', 'updated_at.2',
  'updated_at.3', 'updated_at.4', 'updated_at', 'created_at.1', 'updated_at.1', 'id', 'id.1', 'state.1', 'ssn', 'decision_timestamp',
  'first_name', 'first_name.1', 'first_name.2','last_name', 'last_name.1','last_name.2', 'street', 'street.1','street.2', 'city','city.1',
  'score.1', 'ssn_valid_code', 'address_valid_code','phone_valid_code', 'ssn_name_match','ssn_address_match', 'ssn_dob_warning',
  'name_address_match', 'name_phone_match', 'phone_address_match', 'deceased_indicator', 'fraud_warning',
  'ofac_alert', 'dl_name_match', 'dl_valid_format', 'dldob_match', 'name_work_phone_match', 'dl_verification_code',
  'work_phone_valid_code', 'decision', 'type','credit_report_id.5', 'id.5', 'count_nintendobrowser',
  'credit_report_id.6', 'mbb_has_application_aba_record', 'mbb_max_app_aba_default_differential',
  "count_jointcontractural_type_3m", "count_jointcontractural_type_3m.1",
  "updated_at", "updated_at.1", "updated_at.2", "last_inquiry_date",
  "updated_at.3", "last_loan_date", "bad_loan_date"
)

date_cols <- c("mbb_time_since_most_recent_seen", "mbb_time_since_app_aba_seen",
"mbb_time_since_first_seen", "mbb_time_since_app_aba_first_seen")

imputed_cols <- 
  c("limit_amount_au", "balance_amount_au", "total_credit_limit",
    "total_available_credit", "total_monthly_obligations", "max_credit_limit",
    "credit_directional_test", "min_monthly_debt_burden", "mbb_total_accounts",
    "mbb_max_times_seen", "mbb_total_times_seen", "mbb_max_default_differential",
    "mbb_min_default_differential", "mbb_average_default_differential",
    "mbb_app_aba_max_times_seen", "mbb_app_aba_total_times_seen",
    "mbb_min_app_aba_default_differential", "mbb_average_app_aba_default_differential"
  )

do_not_discretize <- -c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 303L)
     #  c("source", "loan_id", "dep_var", "state", "zip","clarity_report_id", "customer_id" ,"loan_purpose", "product_name")
discretized_columns <- do_not_discretize

library(mungebits)
library(tundra)
