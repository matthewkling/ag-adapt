
process_rma <- function(){
      
      ## COVERAGE data ##
      
      # https://www.rma.usda.gov/en/Information-Tools/Summary-of-Business/State-County-Crop-Summary-of-Business
      # https://www.rma.usda.gov/-/media/RMA/SCC-SOB/State-County-Crop-Coverage/sobsccc_1989forward-pdf.ashx?la=en
      
      f <- list.files("data/ignore/rma/", full.names = T, pattern = "sobcov")
      vars <- c("year", "state_fips", "state", "county_fips", "county",
                "commodity_code", "commodity", "ins_plan_code", "ins_plan",
                "cov_cat", "delivery_type", "cov_level",
                "n_policies_sold", "n_policies_premium", "n_policies_indem",
                "n_units_premium", "n_units_indem",
                "quantity_type", "net_quantity",
                "endorsed_acres",
                "liability_amount", "premium_amount", "subsidy_amount",
                "subsidy_amount_state_private", "subsidy_amount_additional",
                "efa_discount", "indemnity_amount", "loss_ratio")
      d <- f %>% 
            map_dfr(read_delim, delim = "|", 
                    col_names = vars, col_type = "iccccicicccdiiiiicdddddddddd") %>% 
            mutate_at(vars(county, commodity, ins_plan, cov_cat, quantity_type), str_trim) %>%
            mutate(fips = paste0(state_fips, county_fips))
      write_csv(d, "data/output/rma_coverage.csv")
      
}