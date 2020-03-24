library(shiny)
#devtools::install_github("shiny", "rstudio")
#library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {


	output$o_cost_bt<-renderUI({

		if (is.null(input$i_cost_bt))
		{
			sliderInput(inputId="cost_bt", label="", min=5000, max=23000, step=1, value=14000)
			
		} 
	})
	output$o_cost_om<-renderUI({

		if (is.null(input$i_cost_om))
		{
			sliderInput(inputId="cost_om", label="", min=5000, max=30000, step=1, value=22701)
		} 
	})
	output$o_cost_ex_oc<-renderUI({

		if (is.null(input$i_cost_ex_oc))
		{
    			sliderInput(inputId="cost_ex_oc", label="", min=60, max=200, step=1, value=130)
		} 
	})
	output$o_cost_ex_ed<-renderUI({

		if (is.null(input$i_cost_ex_ed))
		{
	    		sliderInput(inputId="cost_ex_ed", label="", min=400, max=800, step=1, value=594)
		} 
	})
	output$o_cost_ex_hosp<-renderUI({

		if (is.null(input$i_cost_ex_hosp))
		{
	    		sliderInput(inputId="cost_ex_hosp", label="", min=7000, max=12800, step=1, value=9904)
		} 
	})





	output$o_utility_chronic_us<-renderUI({

		if (is.null(input$i_utility_chronic_us))
		{
			sliderInput(inputId="utility_chronic_us", label="", min=(0.669-0.1*0.669), max=(0.669+0.1*0.669), step=0.001, value=0.669)
		} 
	})
	output$o_utility_chronic_bt<-renderUI({

		if (is.null(input$i_utility_chronic_bt))
		{
			sliderInput(inputId="utility_chronic_bt", label="", min=(0.702-0.1*0.702), max=(0.702+0.1*0.702), step=0.001, value=0.702)

		} 
	})
	output$o_utility_chronic_om<-renderUI({

		if (is.null(input$i_utility_chronic_om))
		{
			sliderInput(inputId="utility_chronic_om", label="", min=(0.708-0.1*0.708), max=(0.708+0.1*0.708), step=0.001, value=0.708)
		} 
	})






	output$o_rate_ex_oc<-renderUI({

		if (is.null(input$i_rate_ex_oc))
		{
			sliderInput(inputId="rate_ex_oc", label="", min=0.8, max=1.8, step=0.001, value=1.346)
		} 
	})
	output$o_rate_ex_ed<-renderUI({

		if (is.null(input$i_rate_ex_ed))
		{
	    		sliderInput(inputId="rate_ex_ed", label="", min=0.03, max=0.09, step=0.001, value=0.066)
		} 
	})
	output$o_rate_ex_hosp<-renderUI({

		if (is.null(input$i_rate_ex_hosp))
		{
	    		sliderInput(inputId="rate_ex_hosp", label="", min=0.03, max=0.09, step=0.001, value=0.062)
		} 
	})
	output$o_rr_ex_oc<-renderUI({

		if (is.null(input$i_rr_ex_oc))
		{
	    		sliderInput(inputId="rr_ex_oc", label="", min=0.1, max=0.9, step=0.001, value=0.589)
		} 
	})
	output$o_rr_ex_ed<-renderUI({

		if (is.null(input$i_rr_ex_ed))
		{
	    		sliderInput(inputId="rr_ex_ed", label="", min=0.1, max=0.9, step=0.001, value=0.202)	
		} 
	})
	output$o_rr_ex_hosp<-renderUI({

		if (is.null(input$i_rr_ex_hosp))
		{
	    		sliderInput(inputId="rr_ex_hosp", label="", min=0.1, max=0.9, step=0.001, value=0.312)
		} 
	})
	output$o_decline_rate_bt<-renderUI({

		if (is.null(input$i_decline_rate_bt))
		{
	    		sliderInput(inputId="decline_rate_bt", label="", min=0, max=0.2, step=0.01, value=0)
		} 
	})
	output$o_early_hosp_bt<-renderUI({

		if (is.null(input$i_early_hosp_bt))
		{
	    		sliderInput(inputId="early_hosp_bt", label="", min=0, max=0.5, step=0.01, value=0.08)
		} 
	})
	output$o_death_after_hosp<-renderUI({

		if (is.null(input$i_death_after_hosp))
		{
	    		sliderInput(inputId="death_after_hosp", label="", min=0, max=0.5, step=0.0001, value=0.0248)
		} 
	})





	output$o_time_horizon<-renderUI({

		if (is.null(input$i_time_horizon))
		{
			sliderInput(inputId="time_horizon", label="", min=1, max=15, step=1, value=5)
		} 
	})
	output$o_discount_rate<-renderUI({

		if (is.null(input$i_discount_rate))
		{
			sliderInput(inputId="discount_rate", label="", min=0, max=0.05, step=0.01, value=0.03)
		} 
	})
	output$o_baseline_age<-renderUI({

		if (is.null(input$i_baseline_age))
		{
			sliderInput(inputId="baseline_age", label="", min=20, max=60, step=1, value=40)
		} 
	})






















	output$figure<-renderPlot({




		if (!is.null(input$i_time_horizon))
		{
			cc_time_horizon<<-5
			index_time_horizon<<-1

		} else if (is.null(input$i_time_horizon) && index_time_horizon==1)
		{
			cc_time_horizon<<-input$time_horizon
			index_time_horizon<<-2

		} else if (is.null(input$i_time_horizon) && index_time_horizon==2)
		{
			cc_time_horizon<<-input$time_horizon
			index_time_horizon<<-3
		}

		if (is.null(input$i_time_horizon) && index_time_horizon==2)
		{
			cc_time_horizon<<-5

		} else if (is.null(input$i_time_horizon) && index_time_horizon==3)
		{
			cc_time_horizon<<-input$time_horizon
		}






		if (!is.null(input$i_cost_bt))
		{
			cc_costBT_multiply<<-1
			index_cost_bt<<-1

		} else if (is.null(input$i_cost_bt) && index_cost_bt==1)
		{
			cc_costBT_multiply<<-input$cost_bt/14000
			index_cost_bt<<-2

		} else if (is.null(input$i_cost_bt) && index_cost_bt==2)
		{
			cc_costBT_multiply<<-input$cost_bt/14000
			index_cost_bt<<-3
		}

		if (is.null(input$i_cost_bt) && index_cost_bt==2)
		{
			cc_costBT_multiply<<-1

		} else if (is.null(input$i_cost_bt) && index_cost_bt==3)
		{
			cc_costBT_multiply<<-input$cost_bt/14000
		}







		if (!is.null(input$i_cost_om))
		{
			cc_costOM_multiply<<-1
			index_cost_om<<-1

		} else if (is.null(input$i_cost_om) && index_cost_om==1)
		{
			cc_costOM_multiply<<-input$cost_om/22701
			index_cost_om<<-2
		} else if (is.null(input$i_cost_om) && index_cost_om==2)
		{
			cc_costOM_multiply<<-input$cost_om/22701
			index_cost_om<<-3
		}

		if (is.null(input$i_cost_om) && index_cost_om==2)
		{
			cc_costOM_multiply<<-1

		} else if (is.null(input$i_cost_om) && index_cost_om==3)
		{
			cc_costOM_multiply<<-input$cost_om/22701
		}








		if (!is.null(input$i_cost_ex_oc))
		{
			cc_costEX_oc_multiply<<-1
			index_cost_ex_oc<<-1

		} else if (is.null(input$i_cost_ex_oc) && index_cost_ex_oc==1)
		{
			cc_costEX_oc_multiply<<-input$cost_ex_oc/130
			index_cost_ex_oc<<-2
		} else if (is.null(input$i_cost_ex_oc) && index_cost_ex_oc==2)
		{
			cc_costEX_oc_multiply<<-input$cost_ex_oc/130
			index_cost_ex_oc<<-3
		}

		if (is.null(input$i_cost_ex_oc) && index_cost_ex_oc==2)
		{
			cc_costEX_oc_multiply<<-1

		} else if (is.null(input$i_cost_ex_oc) && index_cost_ex_oc==3)
		{
			cc_costEX_oc_multiply<<-input$cost_ex_oc/130
		}





		if (!is.null(input$i_cost_ex_ed))
		{
			cc_costEX_ed_multiply<<-1
			index_cost_ex_ed<<-1

		} else if (is.null(input$i_cost_ex_ed) && index_cost_ex_ed==1)
		{
			cc_costEX_ed_multiply<<-input$cost_ex_ed/594
			index_cost_ex_ed<<-2
		} else if (is.null(input$i_cost_ex_ed) && index_cost_ex_ed==2)
		{
			cc_costEX_ed_multiply<<-input$cost_ex_ed/594
			index_cost_ex_ed<<-3
		}

		if (is.null(input$i_cost_ex_ed) && index_cost_ex_ed==2)
		{
			cc_costEX_ed_multiply<<-1

		} else if (is.null(input$i_cost_ex_ed) && index_cost_ex_ed==3)
		{
			cc_costEX_ed_multiply<<-input$cost_ex_ed/594
		}







		if (!is.null(input$i_cost_ex_hosp))
		{
			cc_costEX_hosp_multiply<<-1
			index_cost_ex_hosp<<-1

		} else if (is.null(input$i_cost_ex_hosp) && index_cost_ex_hosp==1)
		{
			cc_costEX_hosp_multiply<<-input$cost_ex_hosp/9904
			index_cost_ex_hosp<<-2
		} else if (is.null(input$i_cost_ex_hosp) && index_cost_ex_hosp==2)
		{
			cc_costEX_hosp_multiply<<-input$cost_ex_hosp/9904
			index_cost_ex_hosp<<-3
		}

		if (is.null(input$i_cost_ex_hosp) && index_cost_ex_hosp==2)
		{
			cc_costEX_hosp_multiply<<-1

		} else if (is.null(input$i_cost_ex_hosp) && index_cost_ex_hosp==3)
		{
			cc_costEX_hosp_multiply<<-input$cost_ex_hosp/9904
		}




		if (!is.null(input$i_discount_rate))
		{
			cc_discountR_multiply<<-1
			index_discount_rate<<-1

		} else if (is.null(input$i_discount_rate) && index_discount_rate==1)
		{
			cc_discountR_multiply<<-input$discount_rate/0.03
			index_discount_rate<<-2
		} else if (is.null(input$i_discount_rate) && index_discount_rate==2)
		{
			cc_discountR_multiply<<-input$discount_rate/0.03
			index_discount_rate<<-3
		}

		if (is.null(input$i_discount_rate) && index_discount_rate==2)
		{
			cc_discountR_multiply<<-1

		} else if (is.null(input$i_discount_rate) && index_discount_rate==3)
		{
			cc_discountR_multiply<<-input$discount_rate/0.03
		}






		if (!is.null(input$i_utility_chronic_us))
		{
			cc_utility_us_sum<<-0
			index_utility_chronic_us<<-1

		} else if (is.null(input$i_utility_chronic_us) && index_utility_chronic_us==1)
		{
			cc_utility_us_sum<<-input$utility_chronic_us-0.669
			index_utility_chronic_us<<-2
		} else if (is.null(input$i_utility_chronic_us) && index_utility_chronic_us==2)
		{
			cc_utility_us_sum<<-input$utility_chronic_us-0.669
			index_utility_chronic_us<<-3
		}

		if (is.null(input$i_utility_chronic_us) && index_utility_chronic_us==2)
		{
			cc_utility_us_sum<<-0

		} else if (is.null(input$i_utility_chronic_us) && index_utility_chronic_us==3)
		{
			cc_utility_us_sum<<-input$utility_chronic_us-0.669
		}






		if (!is.null(input$i_utility_chronic_bt))
		{
			cc_utility_bt_sum<<-0
			index_utility_chronic_bt<<-1

		} else if (is.null(input$i_utility_chronic_bt) && index_utility_chronic_bt==1)
		{
			cc_utility_bt_sum<<-input$utility_chronic_bt-0.702
			index_utility_chronic_bt<<-2
		} else if (is.null(input$i_utility_chronic_bt) && index_utility_chronic_bt==2)
		{
			cc_utility_bt_sum<<-input$utility_chronic_bt-0.702
			index_utility_chronic_bt<<-3
		}

		if (is.null(input$i_utility_chronic_bt) && index_utility_chronic_bt==2)
		{
			cc_utility_bt_sum<<-0

		} else if (is.null(input$i_utility_chronic_bt) && index_utility_chronic_bt==3)
		{
			cc_utility_bt_sum<<-input$utility_chronic_bt-0.702
		}






		if (!is.null(input$i_utility_chronic_om))
		{
			cc_utility_om_sum<<-0
			index_utility_chronic_om<<-1

		} else if (is.null(input$i_utility_chronic_om) && index_utility_chronic_om==1)
		{
			cc_utility_om_sum<<-input$utility_chronic_om-0.708
			index_utility_chronic_om<<-2
		} else if (is.null(input$i_utility_chronic_om) && index_utility_chronic_om==2)
		{
			cc_utility_om_sum<<-input$utility_chronic_om-0.708
			index_utility_chronic_om<<-3
		}

		if (is.null(input$i_utility_chronic_om) && index_utility_chronic_om==2)
		{
			cc_utility_om_sum<<-0

		} else if (is.null(input$i_utility_chronic_om) && index_utility_chronic_om==3)
		{
			cc_utility_om_sum<<-input$utility_chronic_om-0.708
		}





		if (!is.null(input$i_rr_ex_oc))
		{
			cc_exRR_OC_multiply<<-1
			index_rr_ex_oc<<-1

		} else if (is.null(input$i_rr_ex_oc) && index_rr_ex_oc==1)
		{
			cc_exRR_OC_multiply<<-input$rr_ex_oc/0.589
			index_rr_ex_oc<<-2
		} else if (is.null(input$i_rr_ex_oc) && index_rr_ex_oc==2)
		{
			cc_exRR_OC_multiply<<-input$rr_ex_oc/0.589
			index_rr_ex_oc<<-3
		}

		if (is.null(input$i_rr_ex_oc) && index_rr_ex_oc==2)
		{
			cc_exRR_OC_multiply<<-1

		} else if (is.null(input$i_rr_ex_oc) && index_rr_ex_oc==3)
		{
			cc_exRR_OC_multiply<<-input$rr_ex_oc/0.589
		}





		if (!is.null(input$i_rr_ex_ed))
		{
			cc_exRR_ED_multiply<<-1
			index_rr_ex_ed<<-1

		} else if (is.null(input$i_rr_ex_ed) && index_rr_ex_ed==1)
		{
			cc_exRR_ED_multiply<<-input$rr_ex_ed/0.202
			index_rr_ex_ed<<-2
		} else if (is.null(input$i_rr_ex_ed) && index_rr_ex_ed==2)
		{
			cc_exRR_ED_multiply<<-input$rr_ex_ed/0.202
			index_rr_ex_ed<<-3
		}

		if (is.null(input$i_rr_ex_ed) && index_rr_ex_ed==2)
		{
			cc_exRR_ED_multiply<<-1

		} else if (is.null(input$i_rr_ex_ed) && index_rr_ex_ed==3)
		{
			cc_exRR_ED_multiply<<-input$rr_ex_ed/0.202
		}





		if (!is.null(input$i_rr_ex_hosp))
		{
			cc_exRR_hosp_multiply<<-1
			index_rr_ex_hosp<<-1

		} else if (is.null(input$i_rr_ex_hosp) && index_rr_ex_hosp==1)
		{
			cc_exRR_hosp_multiply<<-input$rr_ex_hosp/0.312
			index_rr_ex_hosp<<-2
		} else if (is.null(input$i_rr_ex_hosp) && index_rr_ex_hosp==2)
		{
			cc_exRR_hosp_multiply<<-input$rr_ex_hosp/0.312
			index_rr_ex_hosp<<-3
		}

		if (is.null(input$i_rr_ex_hosp) && index_rr_ex_hosp==2)
		{
			cc_exRR_hosp_multiply<<-1

		} else if (is.null(input$i_rr_ex_hosp) && index_rr_ex_hosp==3)
		{
			cc_exRR_hosp_multiply<<-input$rr_ex_hosp/0.312
		}





		if (!is.null(input$i_rate_ex_oc))
		{
			cc_exRATEusual_OC_multiply<<-1
			index_rate_ex_oc<<-1

		} else if (is.null(input$i_rate_ex_oc) && index_rate_ex_oc==1)
		{
			cc_exRATEusual_OC_multiply<<-input$rate_ex_oc/1.346
			index_rate_ex_oc<<-2
		} else if (is.null(input$i_rate_ex_oc) && index_rate_ex_oc==2)
		{
			cc_exRATEusual_OC_multiply<<-input$rate_ex_oc/1.346
			index_rate_ex_oc<<-3
		}

		if (is.null(input$i_rate_ex_oc) && index_rate_ex_oc==2)
		{
			cc_exRATEusual_OC_multiply<<-1

		} else if (is.null(input$i_rate_ex_oc) && index_rate_ex_oc==3)
		{
			cc_exRATEusual_OC_multiply<<-input$rate_ex_oc/1.346
		}





		if (!is.null(input$i_rate_ex_ed))
		{
			cc_exRATEusual_ED_multiply<<-1
			index_rate_ex_ed<<-1

		} else if (is.null(input$i_rate_ex_ed) && index_rate_ex_ed==1)
		{
			cc_exRATEusual_ED_multiply<<-input$rate_ex_ed/0.066
			index_rate_ex_ed<<-2
		} else if (is.null(input$i_rate_ex_ed) && index_rate_ex_ed==2)
		{
			cc_exRATEusual_ED_multiply<<-input$rate_ex_ed/0.066
			index_rate_ex_ed<<-3
		}

		if (is.null(input$i_rate_ex_ed) && index_rate_ex_ed==2)
		{
			cc_exRATEusual_ED_multiply<<-1

		} else if (is.null(input$i_rate_ex_ed) && index_rate_ex_ed==3)
		{
			cc_exRATEusual_ED_multiply<<-input$rate_ex_ed/0.066
		}





		if (!is.null(input$i_rate_ex_hosp))
		{
			cc_exRATEusual_hosp_multiply<<-1
			index_rate_ex_hosp<<-1

		} else if (is.null(input$i_rate_ex_hosp) && index_rate_ex_hosp==1)
		{
			cc_exRATEusual_hosp_multiply<<-input$rate_ex_hosp/0.062
			index_rate_ex_hosp<<-2
		} else if (is.null(input$i_rate_ex_hosp) && index_rate_ex_hosp==2)
		{
			cc_exRATEusual_hosp_multiply<<-input$rate_ex_hosp/0.062
			index_rate_ex_hosp<<-3
		}

		if (is.null(input$i_rate_ex_hosp) && index_rate_ex_hosp==2)
		{
			cc_exRATEusual_hosp_multiply<<-1

		} else if (is.null(input$i_rate_ex_hosp) && index_rate_ex_hosp==3)
		{
			cc_exRATEusual_hosp_multiply<<-input$rate_ex_hosp/0.062
		}






		if (!is.null(input$i_death_after_hosp))
		{
			cc_deathHOSP_sum<<-0
			index_death_after_hosp<<-1

		} else if (is.null(input$i_death_after_hosp) && index_death_after_hosp==1)
		{
			cc_deathHOSP_sum<<-input$death_after_hosp-0.0248
			index_death_after_hosp<<-2
		} else if (is.null(input$i_death_after_hosp) && index_death_after_hosp==2)
		{
			cc_deathHOSP_sum<<-input$death_after_hosp-0.0248
			index_death_after_hosp<<-3
		}

		if (is.null(input$i_death_after_hosp) && index_death_after_hosp==2)
		{
			cc_deathHOSP_sum<<-0

		} else if (is.null(input$i_death_after_hosp) && index_death_after_hosp==3)
		{
			cc_deathHOSP_sum<<-input$death_after_hosp-0.0248
		}




		if (!is.null(input$i_baseline_age))
		{
			cc_age_sum<<-0
			index_baseline_age<<-1

		} else if (is.null(input$i_baseline_age) && index_baseline_age==1)
		{
			cc_age_sum<<-input$baseline_age-40
			index_baseline_age<<-2
		} else if (is.null(input$i_baseline_age) && index_baseline_age==2)
		{
			cc_age_sum<<-input$baseline_age-40
			index_baseline_age<<-3
		}

		if (is.null(input$i_baseline_age) && index_baseline_age==2)
		{
			cc_age_sum<<-0

		} else if (is.null(input$i_baseline_age) && index_baseline_age==3)
		{
			cc_age_sum<<-input$baseline_age-40
		}




		if (!is.null(input$i_early_hosp_bt))
		{
			cc_hospBT_firstweek<<-0.08
			index_early_hosp_bt<<-1

		} else if (is.null(input$i_early_hosp_bt) && index_early_hosp_bt==1)
		{
			cc_hospBT_firstweek<<-input$early_hosp_bt
			index_early_hosp_bt<<-2
		} else if (is.null(input$i_early_hosp_bt) && index_early_hosp_bt==2)
		{
			cc_hospBT_firstweek<<-input$early_hosp_bt
			index_early_hosp_bt<<-3
		}

		if (is.null(input$i_early_hosp_bt) && index_early_hosp_bt==2)
		{
			cc_hospBT_firstweek<<-0.08

		} else if (is.null(input$i_early_hosp_bt) && index_early_hosp_bt==3)
		{
			cc_hospBT_firstweek<<-input$early_hosp_bt
		}

	


		if (!is.null(input$i_decline_rate_bt))
		{
			cc_lambda<<-0
			index_decline_rate_bt<<-1

		} else if (is.null(input$i_decline_rate_bt) && index_decline_rate_bt==1)
		{
			cc_lambda<<-input$decline_rate_bt
			index_decline_rate_bt<<-2
		} else if (is.null(input$i_decline_rate_bt) && index_decline_rate_bt==2)
		{
			cc_lambda<<-input$decline_rate_bt
			index_decline_rate_bt<<-3
		}

		if (is.null(input$i_decline_rate_bt) && index_decline_rate_bt==2)
		{
			cc_lambda<<-0

		} else if (is.null(input$i_decline_rate_bt) && index_decline_rate_bt==3)
		{
			cc_lambda<<-input$decline_rate_bt
		}








		source("./defined_functions_d.R")



		final(treatment="usual", time_horizon=5, c_costOM_multiply=1, c_costBT_multiply=1, c_costEX_multiply=c(1, 1, 1), c_discountR_multiply=1, c_utility_us_sum=0, c_utility_bt_sum=0, c_utility_om_sum=0, c_exRR_OC_multiply=1, c_exRR_ED_multiply=1, c_exRR_hosp_multiply=1, c_exRATEusual_OC_multiply=1, c_exRATEusual_ED_multiply=1, c_exRATEusual_hosp_multiply=1, c_deathHOSP_sum=0, c_age_sum=0, c_hospBT_firstweek=0.08, c_lambda=0)
		c_def_us<-outcome[["cost"]]["usual"]
		q_def_us<-outcome[["QALY"]]["usual"]

		final(treatment="thermoplasty", time_horizon=5, c_costOM_multiply=1, c_costBT_multiply=1, c_costEX_multiply=c(1, 1, 1), c_discountR_multiply=1, c_utility_us_sum=0, c_utility_bt_sum=0, c_utility_om_sum=0, c_exRR_OC_multiply=1, c_exRR_ED_multiply=1, c_exRR_hosp_multiply=1, c_exRATEusual_OC_multiply=1, c_exRATEusual_ED_multiply=1, c_exRATEusual_hosp_multiply=1, c_deathHOSP_sum=0, c_age_sum=0, c_hospBT_firstweek=0.08, c_lambda=0)
		c_def_bt<-outcome[["cost"]]["thermoplasty"]
		q_def_bt<-outcome[["QALY"]]["thermoplasty"]

		final(treatment="omalizumab", time_horizon=5, c_costOM_multiply=1, c_costBT_multiply=1, c_costEX_multiply=c(1, 1, 1), c_discountR_multiply=1, c_utility_us_sum=0, c_utility_bt_sum=0, c_utility_om_sum=0, c_exRR_OC_multiply=1, c_exRR_ED_multiply=1, c_exRR_hosp_multiply=1, c_exRATEusual_OC_multiply=1, c_exRATEusual_ED_multiply=1, c_exRATEusual_hosp_multiply=1, c_deathHOSP_sum=0, c_age_sum=0, c_hospBT_firstweek=0.08, c_lambda=0)
		c_def_om<-outcome[["cost"]]["omalizumab"]
		q_def_om<-outcome[["QALY"]]["omalizumab"]	
		



		final(treatment="usual", time_horizon=cc_time_horizon, c_costOM_multiply=cc_costOM_multiply, c_costBT_multiply=cc_costBT_multiply, c_costEX_multiply=c(cc_costEX_oc_multiply, cc_costEX_ed_multiply, cc_costEX_hosp_multiply), c_discountR_multiply=cc_discountR_multiply, c_utility_us_sum=cc_utility_us_sum, c_utility_bt_sum=cc_utility_bt_sum, c_utility_om_sum=cc_utility_om_sum, c_exRR_OC_multiply=cc_exRR_OC_multiply, c_exRR_ED_multiply=cc_exRR_ED_multiply, c_exRR_hosp_multiply=cc_exRR_hosp_multiply, c_exRATEusual_OC_multiply=cc_exRATEusual_OC_multiply, c_exRATEusual_ED_multiply=cc_exRATEusual_ED_multiply, c_exRATEusual_hosp_multiply=cc_exRATEusual_hosp_multiply, c_deathHOSP_sum=cc_deathHOSP_sum, c_age_sum=cc_age_sum, c_hospBT_firstweek=cc_hospBT_firstweek, c_lambda=cc_lambda)
		c_us<-outcome[["cost"]]["usual"]
		q_us<-outcome[["QALY"]]["usual"]

		final(treatment="thermoplasty", time_horizon=cc_time_horizon, c_costOM_multiply=cc_costOM_multiply, c_costBT_multiply=cc_costBT_multiply, c_costEX_multiply=c(cc_costEX_oc_multiply, cc_costEX_ed_multiply, cc_costEX_hosp_multiply), c_discountR_multiply=cc_discountR_multiply, c_utility_us_sum=cc_utility_us_sum, c_utility_bt_sum=cc_utility_bt_sum, c_utility_om_sum=cc_utility_om_sum, c_exRR_OC_multiply=cc_exRR_OC_multiply, c_exRR_ED_multiply=cc_exRR_ED_multiply, c_exRR_hosp_multiply=cc_exRR_hosp_multiply, c_exRATEusual_OC_multiply=cc_exRATEusual_OC_multiply, c_exRATEusual_ED_multiply=cc_exRATEusual_ED_multiply, c_exRATEusual_hosp_multiply=cc_exRATEusual_hosp_multiply, c_deathHOSP_sum=cc_deathHOSP_sum, c_age_sum=cc_age_sum, c_hospBT_firstweek=cc_hospBT_firstweek, c_lambda=cc_lambda)
		c_bt<-outcome[["cost"]]["thermoplasty"]
		q_bt<-outcome[["QALY"]]["thermoplasty"]

		final(treatment="omalizumab", time_horizon=cc_time_horizon, c_costOM_multiply=cc_costOM_multiply, c_costBT_multiply=cc_costBT_multiply, c_costEX_multiply=c(cc_costEX_oc_multiply, cc_costEX_ed_multiply, cc_costEX_hosp_multiply), c_discountR_multiply=cc_discountR_multiply, c_utility_us_sum=cc_utility_us_sum, c_utility_bt_sum=cc_utility_bt_sum, c_utility_om_sum=cc_utility_om_sum, c_exRR_OC_multiply=cc_exRR_OC_multiply, c_exRR_ED_multiply=cc_exRR_ED_multiply, c_exRR_hosp_multiply=cc_exRR_hosp_multiply, c_exRATEusual_OC_multiply=cc_exRATEusual_OC_multiply, c_exRATEusual_ED_multiply=cc_exRATEusual_ED_multiply, c_exRATEusual_hosp_multiply=cc_exRATEusual_hosp_multiply, c_deathHOSP_sum=cc_deathHOSP_sum, c_age_sum=cc_age_sum, c_hospBT_firstweek=cc_hospBT_firstweek, c_lambda=cc_lambda)
		c_om<-outcome[["cost"]]["omalizumab"]
		q_om<-outcome[["QALY"]]["omalizumab"]






		q_min<-min(c(q_def_us, q_def_bt, q_def_om, q_us, q_bt, q_om))
		q_max<-max(c(q_def_us, q_def_bt, q_def_om, q_us, q_bt, q_om))
		c_min<-min(c(c_def_us, c_def_bt, c_def_om, c_us, c_bt, c_om))
		c_max<-max(c(c_def_us, c_def_bt, c_def_om, c_us, c_bt, c_om))

		plot.new()
		rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="yellow", border="black") 
		par(new=T)
		plot(c(q_def_us, q_us), c(c_def_us, c_us), pch=21, cex=2, type="o", bg=c("grey", "blue"), xlab="Quality adjusted life years (QALY)", ylab="Costs (adjusted to US 2013)", xlim=range((q_min-0.1), (q_max+0.1)), ylim=range((c_min-20000), (c_max+20000)), font.lab=3, fg="black", col.axis="black", col.lab="black", main="Cost and QALY (grey color represents the base-case values)", font.main=2, col.main="red", cex.main=1.5, font.main=3)
		par(new=T)
		abline(h=seq(c_min-2000, c_max+2000, by=(c_max-c_min+4000)/10), v=seq(q_min-0.1, q_max+0.1, by=(q_max-q_min+0.2)/10), col = "lightgray", lty = 3)
		par(new=T)
		plot(c(q_def_bt, q_bt), c(c_def_bt, c_bt), pch=22, cex=2, type="o", bg=c("grey", "blue"), xlab="Quality adjusted life years (QALY)", ylab="Costs (adjusted to US 2013)", xlim=range((q_min-0.1), (q_max+0.1)), ylim=range((c_min-20000), (c_max+20000)),  font.lab=3, fg="black", col.axis="black", col.lab="black")
		par(new=T)
		plot(c(q_def_om, q_om), c(c_def_om, c_om), pch=23, cex=2, type="o", bg=c("grey", "blue"), xlab="Quality adjusted life years (QALY)", ylab="Costs (adjusted to US 2013)", xlim=range((q_min-0.1), (q_max+0.1)), ylim=range((c_min-20000), (c_max+20000)),  font.lab=3, fg="black", col.axis="black", col.lab="black")

		if (c_us>=c_def_us)
		{
			text(q_us, (c_us+10000), "standard therapy",  cex=1.25, col="blue")
		} else
		{
			text(q_us, (c_us-10000), "standard therapy",  cex=1.25, col="blue")
		}
		if (c_bt>=c_def_bt)
		{
			text(q_bt, (c_bt+10000), "BT",  cex=1.25, col="blue")
		} else
		{
			text(q_bt, (c_bt-10000), "BT",  cex=1.25, col="blue")
		}
		if (c_om>=c_def_om)
		{
			text(q_om, (c_om+10000), "omalizumab", cex=1.25, col="blue")
		} else
		{
			text(q_om, (c_om-10000), "omalizumab",  cex=1.25, col="blue")
		}


	})
































	output$table<-renderTable({



		if (!is.null(input$i_time_horizon))
		{
			cc_time_horizon<<-5
			index2_time_horizon<<-1

		} else if (is.null(input$i_time_horizon) && index2_time_horizon==1)
		{
			cc_time_horizon<<-input$time_horizon
			index2_time_horizon<<-2

		} else if (is.null(input$i_time_horizon) && index2_time_horizon==2)
		{
			cc_time_horizon<<-input$time_horizon
			index2_time_horizon<<-3
		}

		if (is.null(input$i_time_horizon) && index2_time_horizon==2)
		{
			cc_time_horizon<<-5

		} else if (is.null(input$i_time_horizon) && index2_time_horizon==3)
		{
			cc_time_horizon<<-input$time_horizon
		}






		if (!is.null(input$i_cost_bt))
		{
			cc_costBT_multiply<<-1
			index2_cost_bt<<-1

		} else if (is.null(input$i_cost_bt) && index2_cost_bt==1)
		{
			cc_costBT_multiply<<-input$cost_bt/14000
			index2_cost_bt<<-2

		} else if (is.null(input$i_cost_bt) && index2_cost_bt==2)
		{
			cc_costBT_multiply<<-input$cost_bt/14000
			index2_cost_bt<<-3
		}

		if (is.null(input$i_cost_bt) && index2_cost_bt==2)
		{
			cc_costBT_multiply<<-1

		} else if (is.null(input$i_cost_bt) && index2_cost_bt==3)
		{
			cc_costBT_multiply<<-input$cost_bt/14000
		}







		if (!is.null(input$i_cost_om))
		{
			cc_costOM_multiply<<-1
			index2_cost_om<<-1

		} else if (is.null(input$i_cost_om) && index2_cost_om==1)
		{
			cc_costOM_multiply<<-input$cost_om/22701
			index2_cost_om<<-2
		} else if (is.null(input$i_cost_om) && index2_cost_om==2)
		{
			cc_costOM_multiply<<-input$cost_om/22701
			index2_cost_om<<-3
		}

		if (is.null(input$i_cost_om) && index2_cost_om==2)
		{
			cc_costOM_multiply<<-1

		} else if (is.null(input$i_cost_om) && index2_cost_om==3)
		{
			cc_costOM_multiply<<-input$cost_om/22701
		}








		if (!is.null(input$i_cost_ex_oc))
		{
			cc_costEX_oc_multiply<<-1
			index2_cost_ex_oc<<-1

		} else if (is.null(input$i_cost_ex_oc) && index2_cost_ex_oc==1)
		{
			cc_costEX_oc_multiply<<-input$cost_ex_oc/130
			index2_cost_ex_oc<<-2
		} else if (is.null(input$i_cost_ex_oc) && index2_cost_ex_oc==2)
		{
			cc_costEX_oc_multiply<<-input$cost_ex_oc/130
			index2_cost_ex_oc<<-3
		}

		if (is.null(input$i_cost_ex_oc) && index2_cost_ex_oc==2)
		{
			cc_costEX_oc_multiply<<-1

		} else if (is.null(input$i_cost_ex_oc) && index2_cost_ex_oc==3)
		{
			cc_costEX_oc_multiply<<-input$cost_ex_oc/130
		}





		if (!is.null(input$i_cost_ex_ed))
		{
			cc_costEX_ed_multiply<<-1
			index2_cost_ex_ed<<-1

		} else if (is.null(input$i_cost_ex_ed) && index2_cost_ex_ed==1)
		{
			cc_costEX_ed_multiply<<-input$cost_ex_ed/594
			index2_cost_ex_ed<<-2
		} else if (is.null(input$i_cost_ex_ed) && index2_cost_ex_ed==2)
		{
			cc_costEX_ed_multiply<<-input$cost_ex_ed/594
			index2_cost_ex_ed<<-3
		}

		if (is.null(input$i_cost_ex_ed) && index2_cost_ex_ed==2)
		{
			cc_costEX_ed_multiply<<-1

		} else if (is.null(input$i_cost_ex_ed) && index2_cost_ex_ed==3)
		{
			cc_costEX_ed_multiply<<-input$cost_ex_ed/594
		}







		if (!is.null(input$i_cost_ex_hosp))
		{
			cc_costEX_hosp_multiply<<-1
			index2_cost_ex_hosp<<-1

		} else if (is.null(input$i_cost_ex_hosp) && index2_cost_ex_hosp==1)
		{
			cc_costEX_hosp_multiply<<-input$cost_ex_hosp/9904
			index2_cost_ex_hosp<<-2
		} else if (is.null(input$i_cost_ex_hosp) && index2_cost_ex_hosp==2)
		{
			cc_costEX_hosp_multiply<<-input$cost_ex_hosp/9904
			index2_cost_ex_hosp<<-3
		}

		if (is.null(input$i_cost_ex_hosp) && index2_cost_ex_hosp==2)
		{
			cc_costEX_hosp_multiply<<-1

		} else if (is.null(input$i_cost_ex_hosp) && index2_cost_ex_hosp==3)
		{
			cc_costEX_hosp_multiply<<-input$cost_ex_hosp/9904
		}




		if (!is.null(input$i_discount_rate))
		{
			cc_discountR_multiply<<-1
			index2_discount_rate<<-1

		} else if (is.null(input$i_discount_rate) && index2_discount_rate==1)
		{
			cc_discountR_multiply<<-input$discount_rate/0.03
			index2_discount_rate<<-2
		} else if (is.null(input$i_discount_rate) && index2_discount_rate==2)
		{
			cc_discountR_multiply<<-input$discount_rate/0.03
			index2_discount_rate<<-3
		}

		if (is.null(input$i_discount_rate) && index2_discount_rate==2)
		{
			cc_discountR_multiply<<-1

		} else if (is.null(input$i_discount_rate) && index2_discount_rate==3)
		{
			cc_discountR_multiply<<-input$discount_rate/0.03
		}






		if (!is.null(input$i_utility_chronic_us))
		{
			cc_utility_us_sum<<-0
			index2_utility_chronic_us<<-1

		} else if (is.null(input$i_utility_chronic_us) && index2_utility_chronic_us==1)
		{
			cc_utility_us_sum<<-input$utility_chronic_us-0.669
			index2_utility_chronic_us<<-2
		} else if (is.null(input$i_utility_chronic_us) && index2_utility_chronic_us==2)
		{
			cc_utility_us_sum<<-input$utility_chronic_us-0.669
			index2_utility_chronic_us<<-3
		}

		if (is.null(input$i_utility_chronic_us) && index2_utility_chronic_us==2)
		{
			cc_utility_us_sum<<-0

		} else if (is.null(input$i_utility_chronic_us) && index2_utility_chronic_us==3)
		{
			cc_utility_us_sum<<-input$utility_chronic_us-0.669
		}






		if (!is.null(input$i_utility_chronic_bt))
		{
			cc_utility_bt_sum<<-0
			index2_utility_chronic_bt<<-1

		} else if (is.null(input$i_utility_chronic_bt) && index2_utility_chronic_bt==1)
		{
			cc_utility_bt_sum<<-input$utility_chronic_bt-0.702
			index2_utility_chronic_bt<<-2
		} else if (is.null(input$i_utility_chronic_bt) && index2_utility_chronic_bt==2)
		{
			cc_utility_bt_sum<<-input$utility_chronic_bt-0.702
			index2_utility_chronic_bt<<-3
		}

		if (is.null(input$i_utility_chronic_bt) && index2_utility_chronic_bt==2)
		{
			cc_utility_bt_sum<<-0

		} else if (is.null(input$i_utility_chronic_bt) && index2_utility_chronic_bt==3)
		{
			cc_utility_bt_sum<<-input$utility_chronic_bt-0.702
		}






		if (!is.null(input$i_utility_chronic_om))
		{
			cc_utility_om_sum<<-0
			index2_utility_chronic_om<<-1

		} else if (is.null(input$i_utility_chronic_om) && index2_utility_chronic_om==1)
		{
			cc_utility_om_sum<<-input$utility_chronic_om-0.708
			index2_utility_chronic_om<<-2
		} else if (is.null(input$i_utility_chronic_om) && index2_utility_chronic_om==2)
		{
			cc_utility_om_sum<<-input$utility_chronic_om-0.708
			index2_utility_chronic_om<<-3
		}

		if (is.null(input$i_utility_chronic_om) && index2_utility_chronic_om==2)
		{
			cc_utility_om_sum<<-0

		} else if (is.null(input$i_utility_chronic_om) && index2_utility_chronic_om==3)
		{
			cc_utility_om_sum<<-input$utility_chronic_om-0.708
		}





		if (!is.null(input$i_rr_ex_oc))
		{
			cc_exRR_OC_multiply<<-1
			index2_rr_ex_oc<<-1

		} else if (is.null(input$i_rr_ex_oc) && index2_rr_ex_oc==1)
		{
			cc_exRR_OC_multiply<<-input$rr_ex_oc/0.589
			index2_rr_ex_oc<<-2
		} else if (is.null(input$i_rr_ex_oc) && index2_rr_ex_oc==2)
		{
			cc_exRR_OC_multiply<<-input$rr_ex_oc/0.589
			index2_rr_ex_oc<<-3
		}

		if (is.null(input$i_rr_ex_oc) && index2_rr_ex_oc==2)
		{
			cc_exRR_OC_multiply<<-1

		} else if (is.null(input$i_rr_ex_oc) && index2_rr_ex_oc==3)
		{
			cc_exRR_OC_multiply<<-input$rr_ex_oc/0.589
		}





		if (!is.null(input$i_rr_ex_ed))
		{
			cc_exRR_ED_multiply<<-1
			index2_rr_ex_ed<<-1

		} else if (is.null(input$i_rr_ex_ed) && index2_rr_ex_ed==1)
		{
			cc_exRR_ED_multiply<<-input$rr_ex_ed/0.202
			index2_rr_ex_ed<<-2
		} else if (is.null(input$i_rr_ex_ed) && index2_rr_ex_ed==2)
		{
			cc_exRR_ED_multiply<<-input$rr_ex_ed/0.202
			index2_rr_ex_ed<<-3
		}

		if (is.null(input$i_rr_ex_ed) && index2_rr_ex_ed==2)
		{
			cc_exRR_ED_multiply<<-1

		} else if (is.null(input$i_rr_ex_ed) && index2_rr_ex_ed==3)
		{
			cc_exRR_ED_multiply<<-input$rr_ex_ed/0.202
		}





		if (!is.null(input$i_rr_ex_hosp))
		{
			cc_exRR_hosp_multiply<<-1
			index2_rr_ex_hosp<<-1

		} else if (is.null(input$i_rr_ex_hosp) && index2_rr_ex_hosp==1)
		{
			cc_exRR_hosp_multiply<<-input$rr_ex_hosp/0.312
			index2_rr_ex_hosp<<-2
		} else if (is.null(input$i_rr_ex_hosp) && index2_rr_ex_hosp==2)
		{
			cc_exRR_hosp_multiply<<-input$rr_ex_hosp/0.312
			index2_rr_ex_hosp<<-3
		}

		if (is.null(input$i_rr_ex_hosp) && index2_rr_ex_hosp==2)
		{
			cc_exRR_hosp_multiply<<-1

		} else if (is.null(input$i_rr_ex_hosp) && index2_rr_ex_hosp==3)
		{
			cc_exRR_hosp_multiply<<-input$rr_ex_hosp/0.312
		}





		if (!is.null(input$i_rate_ex_oc))
		{
			cc_exRATEusual_OC_multiply<<-1
			index2_rate_ex_oc<<-1

		} else if (is.null(input$i_rate_ex_oc) && index2_rate_ex_oc==1)
		{
			cc_exRATEusual_OC_multiply<<-input$rate_ex_oc/1.346
			index2_rate_ex_oc<<-2
		} else if (is.null(input$i_rate_ex_oc) && index2_rate_ex_oc==2)
		{
			cc_exRATEusual_OC_multiply<<-input$rate_ex_oc/1.346
			index2_rate_ex_oc<<-3
		}

		if (is.null(input$i_rate_ex_oc) && index2_rate_ex_oc==2)
		{
			cc_exRATEusual_OC_multiply<<-1

		} else if (is.null(input$i_rate_ex_oc) && index2_rate_ex_oc==3)
		{
			cc_exRATEusual_OC_multiply<<-input$rate_ex_oc/1.346
		}





		if (!is.null(input$i_rate_ex_ed))
		{
			cc_exRATEusual_ED_multiply<<-1
			index2_rate_ex_ed<<-1

		} else if (is.null(input$i_rate_ex_ed) && index2_rate_ex_ed==1)
		{
			cc_exRATEusual_ED_multiply<<-input$rate_ex_ed/0.066
			index2_rate_ex_ed<<-2
		} else if (is.null(input$i_rate_ex_ed) && index2_rate_ex_ed==2)
		{
			cc_exRATEusual_ED_multiply<<-input$rate_ex_ed/0.066
			index2_rate_ex_ed<<-3
		}

		if (is.null(input$i_rate_ex_ed) && index2_rate_ex_ed==2)
		{
			cc_exRATEusual_ED_multiply<<-1

		} else if (is.null(input$i_rate_ex_ed) && index2_rate_ex_ed==3)
		{
			cc_exRATEusual_ED_multiply<<-input$rate_ex_ed/0.066
		}





		if (!is.null(input$i_rate_ex_hosp))
		{
			cc_exRATEusual_hosp_multiply<<-1
			index2_rate_ex_hosp<<-1

		} else if (is.null(input$i_rate_ex_hosp) && index2_rate_ex_hosp==1)
		{
			cc_exRATEusual_hosp_multiply<<-input$rate_ex_hosp/0.062
			index2_rate_ex_hosp<<-2
		} else if (is.null(input$i_rate_ex_hosp) && index2_rate_ex_hosp==2)
		{
			cc_exRATEusual_hosp_multiply<<-input$rate_ex_hosp/0.062
			index2_rate_ex_hosp<<-3
		}

		if (is.null(input$i_rate_ex_hosp) && index2_rate_ex_hosp==2)
		{
			cc_exRATEusual_hosp_multiply<<-1

		} else if (is.null(input$i_rate_ex_hosp) && index2_rate_ex_hosp==3)
		{
			cc_exRATEusual_hosp_multiply<<-input$rate_ex_hosp/0.062
		}






		if (!is.null(input$i_death_after_hosp))
		{
			cc_deathHOSP_sum<<-0
			index2_death_after_hosp<<-1

		} else if (is.null(input$i_death_after_hosp) && index2_death_after_hosp==1)
		{
			cc_deathHOSP_sum<<-input$death_after_hosp-0.0248
			index2_death_after_hosp<<-2
		} else if (is.null(input$i_death_after_hosp) && index2_death_after_hosp==2)
		{
			cc_deathHOSP_sum<<-input$death_after_hosp-0.0248
			index2_death_after_hosp<<-3
		}

		if (is.null(input$i_death_after_hosp) && index2_death_after_hosp==2)
		{
			cc_deathHOSP_sum<<-0

		} else if (is.null(input$i_death_after_hosp) && index2_death_after_hosp==3)
		{
			cc_deathHOSP_sum<<-input$death_after_hosp-0.0248
		}




		if (!is.null(input$i_baseline_age))
		{
			cc_age_sum<<-0
			index2_baseline_age<<-1

		} else if (is.null(input$i_baseline_age) && index2_baseline_age==1)
		{
			cc_age_sum<<-input$baseline_age-40
			index2_baseline_age<<-2
		} else if (is.null(input$i_baseline_age) && index2_baseline_age==2)
		{
			cc_age_sum<<-input$baseline_age-40
			index2_baseline_age<<-3
		}

		if (is.null(input$i_baseline_age) && index2_baseline_age==2)
		{
			cc_age_sum<<-0

		} else if (is.null(input$i_baseline_age) && index2_baseline_age==3)
		{
			cc_age_sum<<-input$baseline_age-40
		}




		if (!is.null(input$i_early_hosp_bt))
		{
			cc_hospBT_firstweek<<-0.08
			index2_early_hosp_bt<<-1

		} else if (is.null(input$i_early_hosp_bt) && index2_early_hosp_bt==1)
		{
			cc_hospBT_firstweek<<-input$early_hosp_bt
			index2_early_hosp_bt<<-2
		} else if (is.null(input$i_early_hosp_bt) && index2_early_hosp_bt==2)
		{
			cc_hospBT_firstweek<<-input$early_hosp_bt
			index2_early_hosp_bt<<-3
		}

		if (is.null(input$i_early_hosp_bt) && index2_early_hosp_bt==2)
		{
			cc_hospBT_firstweek<<-0.08

		} else if (is.null(input$i_early_hosp_bt) && index2_early_hosp_bt==3)
		{
			cc_hospBT_firstweek<<-input$early_hosp_bt
		}

	


		if (!is.null(input$i_decline_rate_bt))
		{
			cc_lambda<<-0
			index2_decline_rate_bt<<-1

		} else if (is.null(input$i_decline_rate_bt) && index2_decline_rate_bt==1)
		{
			cc_lambda<<-input$decline_rate_bt
			index2_decline_rate_bt<<-2
		} else if (is.null(input$i_decline_rate_bt) && index2_decline_rate_bt==2)
		{
			cc_lambda<<-input$decline_rate_bt
			index2_decline_rate_bt<<-3
		}

		if (is.null(input$i_decline_rate_bt) && index2_decline_rate_bt==2)
		{
			cc_lambda<<-0

		} else if (is.null(input$i_decline_rate_bt) && index2_decline_rate_bt==3)
		{
			cc_lambda<<-input$decline_rate_bt
		}


		source("./defined_functions_d.R")

		final(treatment="usual", time_horizon=cc_time_horizon, c_costOM_multiply=cc_costOM_multiply, c_costBT_multiply=cc_costBT_multiply, c_costEX_multiply=c(cc_costEX_oc_multiply, cc_costEX_ed_multiply, cc_costEX_hosp_multiply), c_discountR_multiply=cc_discountR_multiply, c_utility_us_sum=cc_utility_us_sum, c_utility_bt_sum=cc_utility_bt_sum, c_utility_om_sum=cc_utility_om_sum, c_exRR_OC_multiply=cc_exRR_OC_multiply, c_exRR_ED_multiply=cc_exRR_ED_multiply, c_exRR_hosp_multiply=cc_exRR_hosp_multiply, c_exRATEusual_OC_multiply=cc_exRATEusual_OC_multiply, c_exRATEusual_ED_multiply=cc_exRATEusual_ED_multiply, c_exRATEusual_hosp_multiply=cc_exRATEusual_hosp_multiply, c_deathHOSP_sum=cc_deathHOSP_sum, c_age_sum=cc_age_sum, c_hospBT_firstweek=cc_hospBT_firstweek, c_lambda=cc_lambda)
		c_us<-round(outcome[["cost"]]["usual"], 0)
		q_us<-round(outcome[["QALY"]]["usual"], 3) 
		oc_us<-round(outcome[["n_OC"]]["usual"], 3)
		ed_us<-round(outcome[["n_ED"]]["usual"], 3)
		hosp_us<-round(outcome[["n_hosp"]]["usual"], 3)

		final(treatment="thermoplasty", time_horizon=cc_time_horizon, c_costOM_multiply=cc_costOM_multiply, c_costBT_multiply=cc_costBT_multiply, c_costEX_multiply=c(cc_costEX_oc_multiply, cc_costEX_ed_multiply, cc_costEX_hosp_multiply), c_discountR_multiply=cc_discountR_multiply, c_utility_us_sum=cc_utility_us_sum, c_utility_bt_sum=cc_utility_bt_sum, c_utility_om_sum=cc_utility_om_sum, c_exRR_OC_multiply=cc_exRR_OC_multiply, c_exRR_ED_multiply=cc_exRR_ED_multiply, c_exRR_hosp_multiply=cc_exRR_hosp_multiply, c_exRATEusual_OC_multiply=cc_exRATEusual_OC_multiply, c_exRATEusual_ED_multiply=cc_exRATEusual_ED_multiply, c_exRATEusual_hosp_multiply=cc_exRATEusual_hosp_multiply, c_deathHOSP_sum=cc_deathHOSP_sum, c_age_sum=cc_age_sum, c_hospBT_firstweek=cc_hospBT_firstweek, c_lambda=cc_lambda)
		c_bt<-round(outcome[["cost"]]["thermoplasty"], 0)
		q_bt<-round(outcome[["QALY"]]["thermoplasty"], 3)
		oc_bt<-round(outcome[["n_OC"]]["thermoplasty"], 3)
		ed_bt<-round(outcome[["n_ED"]]["thermoplasty"], 3)
		hosp_bt<-round(outcome[["n_hosp"]]["thermoplasty"], 3)

		final(treatment="omalizumab", time_horizon=cc_time_horizon, c_costOM_multiply=cc_costOM_multiply, c_costBT_multiply=cc_costBT_multiply, c_costEX_multiply=c(cc_costEX_oc_multiply, cc_costEX_ed_multiply, cc_costEX_hosp_multiply), c_discountR_multiply=cc_discountR_multiply, c_utility_us_sum=cc_utility_us_sum, c_utility_bt_sum=cc_utility_bt_sum, c_utility_om_sum=cc_utility_om_sum, c_exRR_OC_multiply=cc_exRR_OC_multiply, c_exRR_ED_multiply=cc_exRR_ED_multiply, c_exRR_hosp_multiply=cc_exRR_hosp_multiply, c_exRATEusual_OC_multiply=cc_exRATEusual_OC_multiply, c_exRATEusual_ED_multiply=cc_exRATEusual_ED_multiply, c_exRATEusual_hosp_multiply=cc_exRATEusual_hosp_multiply, c_deathHOSP_sum=cc_deathHOSP_sum, c_age_sum=cc_age_sum, c_hospBT_firstweek=cc_hospBT_firstweek, c_lambda=cc_lambda)
		c_om<-round(outcome[["cost"]]["omalizumab"], 0)
		q_om<-round(outcome[["QALY"]]["omalizumab"], 3)
		oc_om<-round(outcome[["n_OC"]]["omalizumab"], 3)
		ed_om<-round(outcome[["n_ED"]]["omalizumab"], 3)
		hosp_om<-round(outcome[["n_hosp"]]["omalizumab"], 3)

		aa<-matrix(c(c_us, q_us, oc_us, ed_us, hosp_us, "-", "-", c_bt, q_bt, oc_bt, ed_bt, hosp_bt, "-", "-", c_om, q_om, oc_om, ed_om, hosp_om, "-", "-"), nrow=7, ncol=3)
		rownames(aa)<-c("Cost ($)", "QALY", "Number of exacerbation requiring OCS", "Number of exacerbation requiring ED", "Number of exacerbation requiring hospitalization", "ICER ($/QALY) (BT relative to standard therapy)", "ICER ($/QALY) (omalizumab relative to BT)")
		colnames(aa)<-c("standard therapy", "BT", "omalizumab")
		aa[6,1]<-"Reference"
		aa[6,2]<-round((round(c_bt,0)-round(c_us,0))/(round(q_bt,3)-round(q_us,3)), 0)
		aa[7,2]<-"Reference"
		aa[7,3]<-round((round(c_om,0)-round(c_bt,0))/(round(q_om,3)-round(q_bt,3)), 0)
		return(aa)

		#data.frame(aa)
	
	
	})




































	output$ranking<-renderTable({





		if (!is.null(input$i_time_horizon))
		{
			cc_time_horizon<<-5
			index3_time_horizon<<-1

		} else if (is.null(input$i_time_horizon) && index3_time_horizon==1)
		{
			cc_time_horizon<<-input$time_horizon
			index3_time_horizon<<-2

		} else if (is.null(input$i_time_horizon) && index3_time_horizon==2)
		{
			cc_time_horizon<<-input$time_horizon
			index3_time_horizon<<-3
		}

		if (is.null(input$i_time_horizon) && index3_time_horizon==2)
		{
			cc_time_horizon<<-5

		} else if (is.null(input$i_time_horizon) && index3_time_horizon==3)
		{
			cc_time_horizon<<-input$time_horizon
		}






		if (!is.null(input$i_cost_bt))
		{
			cc_costBT_multiply<<-1
			index3_cost_bt<<-1

		} else if (is.null(input$i_cost_bt) && index3_cost_bt==1)
		{
			cc_costBT_multiply<<-input$cost_bt/14000
			index3_cost_bt<<-2

		} else if (is.null(input$i_cost_bt) && index3_cost_bt==2)
		{
			cc_costBT_multiply<<-input$cost_bt/14000
			index3_cost_bt<<-3
		}

		if (is.null(input$i_cost_bt) && index3_cost_bt==2)
		{
			cc_costBT_multiply<<-1

		} else if (is.null(input$i_cost_bt) && index3_cost_bt==3)
		{
			cc_costBT_multiply<<-input$cost_bt/14000
		}







		if (!is.null(input$i_cost_om))
		{
			cc_costOM_multiply<<-1
			index3_cost_om<<-1

		} else if (is.null(input$i_cost_om) && index3_cost_om==1)
		{
			cc_costOM_multiply<<-input$cost_om/22701
			index3_cost_om<<-2
		} else if (is.null(input$i_cost_om) && index3_cost_om==2)
		{
			cc_costOM_multiply<<-input$cost_om/22701
			index3_cost_om<<-3
		}

		if (is.null(input$i_cost_om) && index3_cost_om==2)
		{
			cc_costOM_multiply<<-1

		} else if (is.null(input$i_cost_om) && index3_cost_om==3)
		{
			cc_costOM_multiply<<-input$cost_om/22701
		}








		if (!is.null(input$i_cost_ex_oc))
		{
			cc_costEX_oc_multiply<<-1
			index3_cost_ex_oc<<-1

		} else if (is.null(input$i_cost_ex_oc) && index3_cost_ex_oc==1)
		{
			cc_costEX_oc_multiply<<-input$cost_ex_oc/130
			index3_cost_ex_oc<<-2
		} else if (is.null(input$i_cost_ex_oc) && index3_cost_ex_oc==2)
		{
			cc_costEX_oc_multiply<<-input$cost_ex_oc/130
			index3_cost_ex_oc<<-3
		}

		if (is.null(input$i_cost_ex_oc) && index3_cost_ex_oc==2)
		{
			cc_costEX_oc_multiply<<-1

		} else if (is.null(input$i_cost_ex_oc) && index3_cost_ex_oc==3)
		{
			cc_costEX_oc_multiply<<-input$cost_ex_oc/130
		}





		if (!is.null(input$i_cost_ex_ed))
		{
			cc_costEX_ed_multiply<<-1
			index3_cost_ex_ed<<-1

		} else if (is.null(input$i_cost_ex_ed) && index3_cost_ex_ed==1)
		{
			cc_costEX_ed_multiply<<-input$cost_ex_ed/594
			index3_cost_ex_ed<<-2
		} else if (is.null(input$i_cost_ex_ed) && index3_cost_ex_ed==2)
		{
			cc_costEX_ed_multiply<<-input$cost_ex_ed/594
			index3_cost_ex_ed<<-3
		}

		if (is.null(input$i_cost_ex_ed) && index3_cost_ex_ed==2)
		{
			cc_costEX_ed_multiply<<-1

		} else if (is.null(input$i_cost_ex_ed) && index3_cost_ex_ed==3)
		{
			cc_costEX_ed_multiply<<-input$cost_ex_ed/594
		}







		if (!is.null(input$i_cost_ex_hosp))
		{
			cc_costEX_hosp_multiply<<-1
			index3_cost_ex_hosp<<-1

		} else if (is.null(input$i_cost_ex_hosp) && index3_cost_ex_hosp==1)
		{
			cc_costEX_hosp_multiply<<-input$cost_ex_hosp/9904
			index3_cost_ex_hosp<<-2
		} else if (is.null(input$i_cost_ex_hosp) && index3_cost_ex_hosp==2)
		{
			cc_costEX_hosp_multiply<<-input$cost_ex_hosp/9904
			index3_cost_ex_hosp<<-3
		}

		if (is.null(input$i_cost_ex_hosp) && index3_cost_ex_hosp==2)
		{
			cc_costEX_hosp_multiply<<-1

		} else if (is.null(input$i_cost_ex_hosp) && index3_cost_ex_hosp==3)
		{
			cc_costEX_hosp_multiply<<-input$cost_ex_hosp/9904
		}




		if (!is.null(input$i_discount_rate))
		{
			cc_discountR_multiply<<-1
			index3_discount_rate<<-1

		} else if (is.null(input$i_discount_rate) && index3_discount_rate==1)
		{
			cc_discountR_multiply<<-input$discount_rate/0.03
			index3_discount_rate<<-2
		} else if (is.null(input$i_discount_rate) && index3_discount_rate==2)
		{
			cc_discountR_multiply<<-input$discount_rate/0.03
			index3_discount_rate<<-3
		}

		if (is.null(input$i_discount_rate) && index3_discount_rate==2)
		{
			cc_discountR_multiply<<-1

		} else if (is.null(input$i_discount_rate) && index3_discount_rate==3)
		{
			cc_discountR_multiply<<-input$discount_rate/0.03
		}






		if (!is.null(input$i_utility_chronic_us))
		{
			cc_utility_us_sum<<-0
			index3_utility_chronic_us<<-1

		} else if (is.null(input$i_utility_chronic_us) && index3_utility_chronic_us==1)
		{
			cc_utility_us_sum<<-input$utility_chronic_us-0.669
			index3_utility_chronic_us<<-2
		} else if (is.null(input$i_utility_chronic_us) && index3_utility_chronic_us==2)
		{
			cc_utility_us_sum<<-input$utility_chronic_us-0.669
			index3_utility_chronic_us<<-3
		}

		if (is.null(input$i_utility_chronic_us) && index3_utility_chronic_us==2)
		{
			cc_utility_us_sum<<-0

		} else if (is.null(input$i_utility_chronic_us) && index3_utility_chronic_us==3)
		{
			cc_utility_us_sum<<-input$utility_chronic_us-0.669
		}






		if (!is.null(input$i_utility_chronic_bt))
		{
			cc_utility_bt_sum<<-0
			index3_utility_chronic_bt<<-1

		} else if (is.null(input$i_utility_chronic_bt) && index3_utility_chronic_bt==1)
		{
			cc_utility_bt_sum<<-input$utility_chronic_bt-0.702
			index3_utility_chronic_bt<<-2
		} else if (is.null(input$i_utility_chronic_bt) && index3_utility_chronic_bt==2)
		{
			cc_utility_bt_sum<<-input$utility_chronic_bt-0.702
			index3_utility_chronic_bt<<-3
		}

		if (is.null(input$i_utility_chronic_bt) && index3_utility_chronic_bt==2)
		{
			cc_utility_bt_sum<<-0

		} else if (is.null(input$i_utility_chronic_bt) && index3_utility_chronic_bt==3)
		{
			cc_utility_bt_sum<<-input$utility_chronic_bt-0.702
		}






		if (!is.null(input$i_utility_chronic_om))
		{
			cc_utility_om_sum<<-0
			index3_utility_chronic_om<<-1

		} else if (is.null(input$i_utility_chronic_om) && index3_utility_chronic_om==1)
		{
			cc_utility_om_sum<<-input$utility_chronic_om-0.708
			index3_utility_chronic_om<<-2
		} else if (is.null(input$i_utility_chronic_om) && index3_utility_chronic_om==2)
		{
			cc_utility_om_sum<<-input$utility_chronic_om-0.708
			index3_utility_chronic_om<<-3
		}

		if (is.null(input$i_utility_chronic_om) && index3_utility_chronic_om==2)
		{
			cc_utility_om_sum<<-0

		} else if (is.null(input$i_utility_chronic_om) && index3_utility_chronic_om==3)
		{
			cc_utility_om_sum<<-input$utility_chronic_om-0.708
		}





		if (!is.null(input$i_rr_ex_oc))
		{
			cc_exRR_OC_multiply<<-1
			index3_rr_ex_oc<<-1

		} else if (is.null(input$i_rr_ex_oc) && index3_rr_ex_oc==1)
		{
			cc_exRR_OC_multiply<<-input$rr_ex_oc/0.589
			index3_rr_ex_oc<<-2
		} else if (is.null(input$i_rr_ex_oc) && index3_rr_ex_oc==2)
		{
			cc_exRR_OC_multiply<<-input$rr_ex_oc/0.589
			index3_rr_ex_oc<<-3
		}

		if (is.null(input$i_rr_ex_oc) && index3_rr_ex_oc==2)
		{
			cc_exRR_OC_multiply<<-1

		} else if (is.null(input$i_rr_ex_oc) && index3_rr_ex_oc==3)
		{
			cc_exRR_OC_multiply<<-input$rr_ex_oc/0.589
		}





		if (!is.null(input$i_rr_ex_ed))
		{
			cc_exRR_ED_multiply<<-1
			index3_rr_ex_ed<<-1

		} else if (is.null(input$i_rr_ex_ed) && index3_rr_ex_ed==1)
		{
			cc_exRR_ED_multiply<<-input$rr_ex_ed/0.202
			index3_rr_ex_ed<<-2
		} else if (is.null(input$i_rr_ex_ed) && index3_rr_ex_ed==2)
		{
			cc_exRR_ED_multiply<<-input$rr_ex_ed/0.202
			index3_rr_ex_ed<<-3
		}

		if (is.null(input$i_rr_ex_ed) && index3_rr_ex_ed==2)
		{
			cc_exRR_ED_multiply<<-1

		} else if (is.null(input$i_rr_ex_ed) && index3_rr_ex_ed==3)
		{
			cc_exRR_ED_multiply<<-input$rr_ex_ed/0.202
		}





		if (!is.null(input$i_rr_ex_hosp))
		{
			cc_exRR_hosp_multiply<<-1
			index3_rr_ex_hosp<<-1

		} else if (is.null(input$i_rr_ex_hosp) && index3_rr_ex_hosp==1)
		{
			cc_exRR_hosp_multiply<<-input$rr_ex_hosp/0.312
			index3_rr_ex_hosp<<-2
		} else if (is.null(input$i_rr_ex_hosp) && index3_rr_ex_hosp==2)
		{
			cc_exRR_hosp_multiply<<-input$rr_ex_hosp/0.312
			index3_rr_ex_hosp<<-3
		}

		if (is.null(input$i_rr_ex_hosp) && index3_rr_ex_hosp==2)
		{
			cc_exRR_hosp_multiply<<-1

		} else if (is.null(input$i_rr_ex_hosp) && index3_rr_ex_hosp==3)
		{
			cc_exRR_hosp_multiply<<-input$rr_ex_hosp/0.312
		}





		if (!is.null(input$i_rate_ex_oc))
		{
			cc_exRATEusual_OC_multiply<<-1
			index3_rate_ex_oc<<-1

		} else if (is.null(input$i_rate_ex_oc) && index3_rate_ex_oc==1)
		{
			cc_exRATEusual_OC_multiply<<-input$rate_ex_oc/1.346
			index3_rate_ex_oc<<-2
		} else if (is.null(input$i_rate_ex_oc) && index3_rate_ex_oc==2)
		{
			cc_exRATEusual_OC_multiply<<-input$rate_ex_oc/1.346
			index3_rate_ex_oc<<-3
		}

		if (is.null(input$i_rate_ex_oc) && index3_rate_ex_oc==2)
		{
			cc_exRATEusual_OC_multiply<<-1

		} else if (is.null(input$i_rate_ex_oc) && index3_rate_ex_oc==3)
		{
			cc_exRATEusual_OC_multiply<<-input$rate_ex_oc/1.346
		}





		if (!is.null(input$i_rate_ex_ed))
		{
			cc_exRATEusual_ED_multiply<<-1
			index3_rate_ex_ed<<-1

		} else if (is.null(input$i_rate_ex_ed) && index3_rate_ex_ed==1)
		{
			cc_exRATEusual_ED_multiply<<-input$rate_ex_ed/0.066
			index3_rate_ex_ed<<-2
		} else if (is.null(input$i_rate_ex_ed) && index3_rate_ex_ed==2)
		{
			cc_exRATEusual_ED_multiply<<-input$rate_ex_ed/0.066
			index3_rate_ex_ed<<-3
		}

		if (is.null(input$i_rate_ex_ed) && index3_rate_ex_ed==2)
		{
			cc_exRATEusual_ED_multiply<<-1

		} else if (is.null(input$i_rate_ex_ed) && index3_rate_ex_ed==3)
		{
			cc_exRATEusual_ED_multiply<<-input$rate_ex_ed/0.066
		}





		if (!is.null(input$i_rate_ex_hosp))
		{
			cc_exRATEusual_hosp_multiply<<-1
			index3_rate_ex_hosp<<-1

		} else if (is.null(input$i_rate_ex_hosp) && index3_rate_ex_hosp==1)
		{
			cc_exRATEusual_hosp_multiply<<-input$rate_ex_hosp/0.062
			index3_rate_ex_hosp<<-2
		} else if (is.null(input$i_rate_ex_hosp) && index3_rate_ex_hosp==2)
		{
			cc_exRATEusual_hosp_multiply<<-input$rate_ex_hosp/0.062
			index3_rate_ex_hosp<<-3
		}

		if (is.null(input$i_rate_ex_hosp) && index3_rate_ex_hosp==2)
		{
			cc_exRATEusual_hosp_multiply<<-1

		} else if (is.null(input$i_rate_ex_hosp) && index3_rate_ex_hosp==3)
		{
			cc_exRATEusual_hosp_multiply<<-input$rate_ex_hosp/0.062
		}






		if (!is.null(input$i_death_after_hosp))
		{
			cc_deathHOSP_sum<<-0
			index3_death_after_hosp<<-1

		} else if (is.null(input$i_death_after_hosp) && index3_death_after_hosp==1)
		{
			cc_deathHOSP_sum<<-input$death_after_hosp-0.0248
			index3_death_after_hosp<<-2
		} else if (is.null(input$i_death_after_hosp) && index3_death_after_hosp==2)
		{
			cc_deathHOSP_sum<<-input$death_after_hosp-0.0248
			index3_death_after_hosp<<-3
		}

		if (is.null(input$i_death_after_hosp) && index3_death_after_hosp==2)
		{
			cc_deathHOSP_sum<<-0

		} else if (is.null(input$i_death_after_hosp) && index3_death_after_hosp==3)
		{
			cc_deathHOSP_sum<<-input$death_after_hosp-0.0248
		}




		if (!is.null(input$i_baseline_age))
		{
			cc_age_sum<<-0
			index3_baseline_age<<-1

		} else if (is.null(input$i_baseline_age) && index3_baseline_age==1)
		{
			cc_age_sum<<-input$baseline_age-40
			index3_baseline_age<<-2
		} else if (is.null(input$i_baseline_age) && index3_baseline_age==2)
		{
			cc_age_sum<<-input$baseline_age-40
			index3_baseline_age<<-3
		}

		if (is.null(input$i_baseline_age) && index3_baseline_age==2)
		{
			cc_age_sum<<-0

		} else if (is.null(input$i_baseline_age) && index3_baseline_age==3)
		{
			cc_age_sum<<-input$baseline_age-40
		}




		if (!is.null(input$i_early_hosp_bt))
		{
			cc_hospBT_firstweek<<-0.08
			index3_early_hosp_bt<<-1

		} else if (is.null(input$i_early_hosp_bt) && index3_early_hosp_bt==1)
		{
			cc_hospBT_firstweek<<-input$early_hosp_bt
			index3_early_hosp_bt<<-2
		} else if (is.null(input$i_early_hosp_bt) && index3_early_hosp_bt==2)
		{
			cc_hospBT_firstweek<<-input$early_hosp_bt
			index3_early_hosp_bt<<-3
		}

		if (is.null(input$i_early_hosp_bt) && index3_early_hosp_bt==2)
		{
			cc_hospBT_firstweek<<-0.08

		} else if (is.null(input$i_early_hosp_bt) && index3_early_hosp_bt==3)
		{
			cc_hospBT_firstweek<<-input$early_hosp_bt
		}

	


		if (!is.null(input$i_decline_rate_bt))
		{
			cc_lambda<<-0
			index3_decline_rate_bt<<-1

		} else if (is.null(input$i_decline_rate_bt) && index3_decline_rate_bt==1)
		{
			cc_lambda<<-input$decline_rate_bt
			index3_decline_rate_bt<<-2
		} else if (is.null(input$i_decline_rate_bt) && index3_decline_rate_bt==2)
		{
			cc_lambda<<-input$decline_rate_bt
			index3_decline_rate_bt<<-3
		}

		if (is.null(input$i_decline_rate_bt) && index3_decline_rate_bt==2)
		{
			cc_lambda<<-0

		} else if (is.null(input$i_decline_rate_bt) && index3_decline_rate_bt==3)
		{
			cc_lambda<<-input$decline_rate_bt
		}



		source("./defined_functions_d.R")

		final(treatment="usual", time_horizon=cc_time_horizon, c_costOM_multiply=cc_costOM_multiply, c_costBT_multiply=cc_costBT_multiply, c_costEX_multiply=c(cc_costEX_oc_multiply, cc_costEX_ed_multiply, cc_costEX_hosp_multiply), c_discountR_multiply=cc_discountR_multiply, c_utility_us_sum=cc_utility_us_sum, c_utility_bt_sum=cc_utility_bt_sum, c_utility_om_sum=cc_utility_om_sum, c_exRR_OC_multiply=cc_exRR_OC_multiply, c_exRR_ED_multiply=cc_exRR_ED_multiply, c_exRR_hosp_multiply=cc_exRR_hosp_multiply, c_exRATEusual_OC_multiply=cc_exRATEusual_OC_multiply, c_exRATEusual_ED_multiply=cc_exRATEusual_ED_multiply, c_exRATEusual_hosp_multiply=cc_exRATEusual_hosp_multiply, c_deathHOSP_sum=cc_deathHOSP_sum, c_age_sum=cc_age_sum, c_hospBT_firstweek=cc_hospBT_firstweek, c_lambda=cc_lambda)
		nb_us_50<-50000*outcome[["QALY"]]["usual"]-outcome[["cost"]]["usual"]
		nb_us_100<-100000*outcome[["QALY"]]["usual"]-outcome[["cost"]]["usual"]


		final(treatment="thermoplasty", time_horizon=cc_time_horizon, c_costOM_multiply=cc_costOM_multiply, c_costBT_multiply=cc_costBT_multiply, c_costEX_multiply=c(cc_costEX_oc_multiply, cc_costEX_ed_multiply, cc_costEX_hosp_multiply), c_discountR_multiply=cc_discountR_multiply, c_utility_us_sum=cc_utility_us_sum, c_utility_bt_sum=cc_utility_bt_sum, c_utility_om_sum=cc_utility_om_sum, c_exRR_OC_multiply=cc_exRR_OC_multiply, c_exRR_ED_multiply=cc_exRR_ED_multiply, c_exRR_hosp_multiply=cc_exRR_hosp_multiply, c_exRATEusual_OC_multiply=cc_exRATEusual_OC_multiply, c_exRATEusual_ED_multiply=cc_exRATEusual_ED_multiply, c_exRATEusual_hosp_multiply=cc_exRATEusual_hosp_multiply, c_deathHOSP_sum=cc_deathHOSP_sum, c_age_sum=cc_age_sum, c_hospBT_firstweek=cc_hospBT_firstweek, c_lambda=cc_lambda)
		nb_bt_50<-50000*outcome[["QALY"]]["thermoplasty"]-outcome[["cost"]]["thermoplasty"]
		nb_bt_100<-100000*outcome[["QALY"]]["thermoplasty"]-outcome[["cost"]]["thermoplasty"]

		final(treatment="omalizumab", time_horizon=cc_time_horizon, c_costOM_multiply=cc_costOM_multiply, c_costBT_multiply=cc_costBT_multiply, c_costEX_multiply=c(cc_costEX_oc_multiply, cc_costEX_ed_multiply, cc_costEX_hosp_multiply), c_discountR_multiply=cc_discountR_multiply, c_utility_us_sum=cc_utility_us_sum, c_utility_bt_sum=cc_utility_bt_sum, c_utility_om_sum=cc_utility_om_sum, c_exRR_OC_multiply=cc_exRR_OC_multiply, c_exRR_ED_multiply=cc_exRR_ED_multiply, c_exRR_hosp_multiply=cc_exRR_hosp_multiply, c_exRATEusual_OC_multiply=cc_exRATEusual_OC_multiply, c_exRATEusual_ED_multiply=cc_exRATEusual_ED_multiply, c_exRATEusual_hosp_multiply=cc_exRATEusual_hosp_multiply, c_deathHOSP_sum=cc_deathHOSP_sum, c_age_sum=cc_age_sum, c_hospBT_firstweek=cc_hospBT_firstweek, c_lambda=cc_lambda)
		nb_om_50<-50000*outcome[["QALY"]]["omalizumab"]-outcome[["cost"]]["omalizumab"]
		nb_om_100<-100000*outcome[["QALY"]]["omalizumab"]-outcome[["cost"]]["omalizumab"]

		nb_sort_50<-sort(c(nb_us_50, nb_bt_50, nb_om_50))
		if (nb_sort_50[1]==nb_us_50)
		{
			if (nb_sort_50[2]==nb_bt_50)
			{
				names(nb_sort_50)<-c("standard therapy", "BT", "omalizumab")
			} else
			{
				names(nb_sort_50)<-c("standard therapy", "oamlizumab", "BT")
			}
		} else if (nb_sort_50[1]==nb_bt_50)
		{
			if (nb_sort_50[2]==nb_us_50)
			{
				names(nb_sort_50)<-c("BT", "standard therapy", "omalizumab")

			} else
			{
				names(nb_sort_50)<-c("BT", "omalizumab", "standard therapy")
			}
		
		} else 
		{
			if (nb_sort_50[2]==nb_us_50)
			{
				names(nb_sort_50)<-c("omalizumab", "standard therapy", "BT")

			} else
			{
				names(nb_sort_50)<-c("omalizumab", "BT", "standard therapy")

			}
		}



		nb_sort_100<-sort(c(nb_us_100, nb_bt_100, nb_om_100))
		if (nb_sort_100[1]==nb_us_100)
		{
			if (nb_sort_100[2]==nb_bt_100)
			{
				names(nb_sort_100)<-c("standard therapy", "BT", "omalizumab")
			} else
			{
				names(nb_sort_100)<-c("standard therapy", "oamlizumab", "BT")
			}
		} else if (nb_sort_100[1]==nb_bt_100)
		{
			if (nb_sort_100[2]==nb_us_100)
			{
				names(nb_sort_100)<-c("BT", "standard therapy", "omalizumab")

			} else
			{
				names(nb_sort_100)<-c("BT", "omalizumab", "standard therapy")
			}
		
		} else 
		{
			if (nb_sort_100[2]==nb_us_100)
			{
				names(nb_sort_100)<-c("omalizumab", "standard therapy", "BT")

			} else
			{
				names(nb_sort_100)<-c("omalizumab", "BT", "standard therapy")

			}
		}




		bb<<-matrix(c(names(which(nb_sort_50==nb_sort_50[3])), names(which(nb_sort_50==nb_sort_50[2])), names(which(nb_sort_50==nb_sort_50[1])), names(which(nb_sort_100==nb_sort_100[3])), names(which(nb_sort_100==nb_sort_100[2])), names(which(nb_sort_100==nb_sort_100[1]))), nrow=3, ncol=2)
		rownames(bb)<<-c("Rank-1st", "Rank-2nd", "Rank-3rd")
		colnames(bb)<<-c("WTP=$50,000/QALY", "WTP=$100,000/QALY")
		return(bb)
	
	
	})

})


























#data1<-c("Names", rep(0,6))
#save(data1, file="./soccer/analysis.RData")

#b<-loadWorkbook ( "fafa.xls" , create = TRUE )
#createSheet(object=b, name="Sheet1")
#writeWorksheet(object=b, c(1,2,3), sheet = "Sheet1", startRow = 3, startCol = 4)
#saveWorkbook(b)