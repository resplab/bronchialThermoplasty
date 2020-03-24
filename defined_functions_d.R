loadParameters<-function(c_costOM_multiply, c_costBT_multiply, c_costEX_multiply, c_discountR_multiply, c_utility_us_sum, c_utility_bt_sum,
				c_utility_om_sum, c_exRR_OC_multiply, c_exRR_ED_multiply, c_exRR_hosp_multiply, c_exRATEusual_OC_multiply, 
				c_exRATEusual_ED_multiply, c_exRATEusual_hosp_multiply,c_deathHOSP_sum, c_age_sum, c_hospBT_firstweek, c_lambda)
{
	age<<-40+c_age_sum

	discountRate<<-0.03/52*c_discountR_multiply

     	die_index<<-read.table(file="./death_prob.csv",header=TRUE,sep=",");
	die<<-die_index[,2]
    	dieWeekly<<-1-exp((log(1-die))*1/52)

	Death_HOPS_monthProb<<-0.0248+c_deathHOSP_sum 

	initialDist_om<<-c(1, 0, 0, 0, 0)
	names(initialDist_om)<<-c("chronic", "OC", "ED", "hosp", "death")


	initialDist_bt<<-c(1, 0, 0, 0, 0)
	names(initialDist_bt)<<-c("chronic", "OC", "ED", "hosp", "death")

	initialDist_us<<-c(1, 0, 0, 0, 0)
	names(initialDist_us)<<-c("chronic", "OC", "ED", "hosp", "death")


	c_lambda<<-c_lambda

	rate_RR<<-matrix(0, nrow=2, ncol=3)
	rownames(rate_RR)<<-c("omalizumab", "thermoplasty")
	colnames(rate_RR)<<-c("OC", "ED", "hosp")
	rate_RR["omalizumab", ]<<-c(0.634, 0.397, 0.489)
	rate_RR["thermoplasty", ]<<-c(0.482, 0.194, 0.297)*c(1/0.482-(1/0.482-1)*exp(-c_lambda*0), 1/0.194-(1/0.194-1)*exp(-c_lambda*0), 1/0.297-(1/0.297-1)*exp(-c_lambda*0))

	rate_year<<-matrix(0, nrow=3, ncol=3)
	rownames(rate_year)<<-c("usual", "omalizumab", "thermoplasty")
	colnames(rate_year)<<-c("OC", "ED", "hosp")
	rate_year["usual", ]<<-c((1.346*c_exRATEusual_OC_multiply), (0.066*c_exRATEusual_ED_multiply), (0.062*c_exRATEusual_hosp_multiply))	
	rate_year["omalizumab", ]<<-rate_year["usual", ]*rate_RR["omalizumab", ]
	rate_year["thermoplasty", ]<<-rate_year["usual", ]*rate_RR["thermoplasty", ]*c(c_exRR_OC_multiply, c_exRR_ED_multiply, c_exRR_hosp_multiply)

	tp<<-list(usual=matrix(0, nrow=5, ncol=5), omalizumab=matrix(0, nrow=5, ncol=5), thermoplasty=matrix(0, nrow=5, ncol=5))
	rownames(tp[["usual"]])<<-c("chronic", "OC", "ED", "hosp", "death")
	colnames(tp[["usual"]])<<-c("chronic", "OC", "ED", "hosp", "death")
	rownames(tp[["omalizumab"]])<<-c("chronic", "OC", "ED", "hosp", "death")
	colnames(tp[["omalizumab"]])<<-c("chronic", "OC", "ED", "hosp", "death")
	rownames(tp[["thermoplasty"]])<<-c("chronic", "OC", "ED", "hosp", "death")
	colnames(tp[["thermoplasty"]])<<-c("chronic", "OC", "ED", "hosp", "death")



	costPar<<-list(treatCost=rep(0, 3), otherCost=rep(0, 3))
	costPar[["treatCost"]]<<-c(2614/52, 22701/52*c_costOM_multiply, 14100*c_costBT_multiply)
	costPar[["otherCost"]]<<-c(130, 594, 9904)*c_costEX_multiply
	names(costPar[["treatCost"]])<<-c("usual", "omalizumab", "thermoplasty")
	names(costPar[["otherCost"]])<<-c("OC", "ED", "hosp")

	cost_bt_0<<-c_hospBT_firstweek*costPar[["otherCost"]]["hosp"]+costPar[["treatCost"]]["thermoplasty"]+0.1*66


	utility<<-matrix(0, nrow=3, ncol=5)
	rownames(utility)<<-c("usual", "omalizumab", "thermoplasty")
	colnames(utility)<<-c("chronic", "OC", "ED", "hosp", "death")
	utility["usual", ]<<-c(0.669+c_utility_us_sum, 0.572, 0.449, 0.326, 0)
	utility["omalizumab", "chronic"]<<-0.708+c_utility_us_sum+c_utility_om_sum  
	utility["thermoplasty", "chronic"]<<-0.702+c_utility_us_sum+c_utility_bt_sum 
	utility["omalizumab", 2:5]<<-utility["usual", 2:5]
	utility["thermoplasty", 2:5]<<-utility["usual", 2:5]

}

















calculation<-function(treatment, time_horizon)
{

	outcome_weekly<<-list(cost_weekly=matrix(0, nrow=3, ncol=(time_horizon*52)), QALY_weekly=matrix(0, nrow=3, ncol=(time_horizon*52)), n_OC_weekly=matrix(0, nrow=3, ncol=(time_horizon*52)), n_ED_weekly=matrix(0, nrow=3, ncol=(time_horizon*52)), n_hosp_weekly=matrix(0, nrow=3, ncol=(time_horizon*52)))
	rownames(outcome_weekly[["cost_weekly"]])<<-c("usual", "omalizumab", "thermoplasty")
	rownames(outcome_weekly[["QALY_weekly"]])<<-c("usual", "omalizumab", "thermoplasty")
	rownames(outcome_weekly[["n_OC_weekly"]])<<-c("usual", "omalizumab", "thermoplasty")
	rownames(outcome_weekly[["n_ED_weekly"]])<<-c("usual", "omalizumab", "thermoplasty")
	rownames(outcome_weekly[["n_hosp_weekly"]])<<-c("usual", "omalizumab", "thermoplasty")


	age_counter<<-c()

	rate0_year_thermoplasty<<-rate_year["thermoplasty", ]

	
	for (j in 1:(time_horizon*52))
	{

		age_x<<-age+floor((j-1)/52)



		if (treatment=="thermoplasty")
		{
			
			if (j<=(5*52))
			{
				t1<<-0
				rate_year[treatment, ]<<-rate0_year_thermoplasty*c(1/0.589-(1/0.589-1)*exp(-c_lambda*t1), 1/0.202-(1/0.202-1)*exp(-c_lambda*t1), 1/0.312-(1/0.312-1)*exp(-c_lambda*t1))

			} else
			{
				t1<<-(j-5*52)/52
				rate_year[treatment, ]<<-rate0_year_thermoplasty*c(1/0.589-(1/0.589-1)*exp(-c_lambda*t1), 1/0.202-(1/0.202-1)*exp(-c_lambda*t1), 1/0.312-(1/0.312-1)*exp(-c_lambda*t1))

			}


			tp[[treatment]]["chronic", "death"]<<-dieWeekly[floor(age_x)]
			tp[[treatment]]["chronic", "chronic"]<<-(1-dieWeekly[floor(age_x)])*(exp(-(rate_year[treatment, "OC"]+rate_year[treatment, "ED"]+rate_year[treatment, "hosp"])*1/52))
			tp[[treatment]]["chronic", "OC"]<<-(1-dieWeekly[floor(age_x)])*(1-exp(-(rate_year[treatment, "OC"]+rate_year[treatment, "ED"]+rate_year[treatment, "hosp"])*1/52))*rate_year[treatment, "OC"]/(rate_year[treatment, "OC"]+rate_year[treatment, "ED"]+rate_year[treatment, "hosp"])
			tp[[treatment]]["chronic", "ED"]<<-(1-dieWeekly[floor(age_x)])*(1-exp(-(rate_year[treatment, "OC"]+rate_year[treatment, "ED"]+rate_year[treatment, "hosp"])*1/52))*rate_year[treatment, "ED"]/(rate_year[treatment, "OC"]+rate_year[treatment, "ED"]+rate_year[treatment, "hosp"])
			tp[[treatment]]["chronic", "hosp"]<<-(1-dieWeekly[floor(age_x)])*(1-exp(-(rate_year[treatment, "OC"]+rate_year[treatment, "ED"]+rate_year[treatment, "hosp"])*1/52))*rate_year[treatment, "hosp"]/(rate_year[treatment, "OC"]+rate_year[treatment, "ED"]+rate_year[treatment, "hosp"])

			tp[[treatment]]["OC", "death"]<<-dieWeekly[floor(age_x)]
			tp[[treatment]]["OC", "chronic"]<<-(1-dieWeekly[floor(age_x)])

			tp[[treatment]]["ED", "death"]<<-dieWeekly[floor(age_x)]
			tp[[treatment]]["ED", "chronic"]<<-(1-dieWeekly[floor(age_x)])
	
			tp[[treatment]]["hosp", "death"]<<-dieWeekly[floor(age_x)]+1-exp(log(1-Death_HOPS_monthProb)*7/30)
			tp[[treatment]]["hosp", "chronic"]<<-1-(dieWeekly[floor(age_x)]+1-exp(log(1-Death_HOPS_monthProb)*7/30))

			tp[[treatment]]["death", "death"]<<-1;


			newDist_bt<<-initialDist_bt%*%tp[[treatment]]

			if (j==1)
			{
				outcome_weekly[["cost_weekly"]][treatment, j]<<-initialDist_bt[1:4]%*%c(costPar[["treatCost"]]["usual"], costPar[["otherCost"]])+cost_bt_0
				outcome_weekly[["cost_weekly"]][treatment, j]<<-outcome_weekly[["cost_weekly"]][treatment, j]/((1+discountRate)^(j-1))
			} else 
			{
				outcome_weekly[["cost_weekly"]][treatment, j]<<-initialDist_bt[1:4]%*%c(costPar[["treatCost"]]["usual"], costPar[["otherCost"]])
				outcome_weekly[["cost_weekly"]][treatment, j]<<-outcome_weekly[["cost_weekly"]][treatment, j]/((1+discountRate)^(j-1))
			}


			outcome_weekly[["QALY_weekly"]][treatment, j]<<-(initialDist_bt%*%utility[treatment, ])/52
			outcome_weekly[["QALY_weekly"]][treatment, j]<<-outcome_weekly[["QALY_weekly"]][treatment, j]/((1+discountRate)^(j-1))

			outcome_weekly[["n_OC_weekly"]][treatment, j]<<-initialDist_bt[2]

			outcome_weekly[["n_ED_weekly"]][treatment, j]<<-initialDist_bt[3]

			outcome_weekly[["n_hosp_weekly"]][treatment, j]<<-initialDist_bt[4]


			initialDist_bt<<-newDist_bt

			age_counter[j]<<-age_x
		}


		if (treatment=="omalizumab")
		{

			tp[[treatment]]["chronic", "death"]<<-dieWeekly[floor(age_x)]
			tp[[treatment]]["chronic", "chronic"]<<-(1-dieWeekly[floor(age_x)])*(exp(-(rate_year[treatment, "OC"]+rate_year[treatment, "ED"]+rate_year[treatment, "hosp"])*1/52))
			tp[[treatment]]["chronic", "OC"]<<-(1-dieWeekly[floor(age_x)])*(1-exp(-(rate_year[treatment, "OC"]+rate_year[treatment, "ED"]+rate_year[treatment, "hosp"])*1/52))*rate_year[treatment, "OC"]/(rate_year[treatment, "OC"]+rate_year[treatment, "ED"]+rate_year[treatment, "hosp"])
			tp[[treatment]]["chronic", "ED"]<<-(1-dieWeekly[floor(age_x)])*(1-exp(-(rate_year[treatment, "OC"]+rate_year[treatment, "ED"]+rate_year[treatment, "hosp"])*1/52))*rate_year[treatment, "ED"]/(rate_year[treatment, "OC"]+rate_year[treatment, "ED"]+rate_year[treatment, "hosp"])
			tp[[treatment]]["chronic", "hosp"]<<-(1-dieWeekly[floor(age_x)])*(1-exp(-(rate_year[treatment, "OC"]+rate_year[treatment, "ED"]+rate_year[treatment, "hosp"])*1/52))*rate_year[treatment, "hosp"]/(rate_year[treatment, "OC"]+rate_year[treatment, "ED"]+rate_year[treatment, "hosp"])

			tp[[treatment]]["OC", "death"]<<-dieWeekly[floor(age_x)]
			tp[[treatment]]["OC", "chronic"]<<-(1-dieWeekly[floor(age_x)])

			tp[[treatment]]["ED", "death"]<<-dieWeekly[floor(age_x)]
			tp[[treatment]]["ED", "chronic"]<<-(1-dieWeekly[floor(age_x)])
	
			tp[[treatment]]["hosp", "death"]<<-dieWeekly[floor(age_x)]+1-exp(log(1-Death_HOPS_monthProb)*7/30)
			tp[[treatment]]["hosp", "chronic"]<<-1-(dieWeekly[floor(age_x)]+1-exp(log(1-Death_HOPS_monthProb)*7/30))

			tp[[treatment]]["death", "death"]<<-1;

			newDist_om<<-initialDist_om%*%tp[[treatment]]

			outcome_weekly[["cost_weekly"]][treatment, j]<<-initialDist_om[1:4]%*%c((costPar[["treatCost"]][treatment]+costPar[["treatCost"]]["usual"]), costPar[["otherCost"]])
			outcome_weekly[["cost_weekly"]][treatment, j]<<-outcome_weekly[["cost_weekly"]][treatment, j]/((1+discountRate)^(j-1))

			outcome_weekly[["QALY_weekly"]][treatment, j]<<-(initialDist_om%*%utility[treatment, ])/52
			outcome_weekly[["QALY_weekly"]][treatment, j]<<-outcome_weekly[["QALY_weekly"]][treatment, j]/((1+discountRate)^(j-1))

			outcome_weekly[["n_OC_weekly"]][treatment, j]<<-initialDist_om[2]

			outcome_weekly[["n_ED_weekly"]][treatment, j]<<-initialDist_om[3]

			outcome_weekly[["n_hosp_weekly"]][treatment, j]<<-initialDist_om[4]


			initialDist_om<<-newDist_om

			age_counter[j]<<-age_x
		}


		if (treatment=="usual")
		{

			tp[[treatment]]["chronic", "death"]<<-dieWeekly[floor(age_x)]
			tp[[treatment]]["chronic", "chronic"]<<-(1-dieWeekly[floor(age_x)])*(exp(-(rate_year[treatment, "OC"]+rate_year[treatment, "ED"]+rate_year[treatment, "hosp"])*1/52))
			tp[[treatment]]["chronic", "OC"]<<-(1-dieWeekly[floor(age_x)])*(1-exp(-(rate_year[treatment, "OC"]+rate_year[treatment, "ED"]+rate_year[treatment, "hosp"])*1/52))*rate_year[treatment, "OC"]/(rate_year[treatment, "OC"]+rate_year[treatment, "ED"]+rate_year[treatment, "hosp"])
			tp[[treatment]]["chronic", "ED"]<<-(1-dieWeekly[floor(age_x)])*(1-exp(-(rate_year[treatment, "OC"]+rate_year[treatment, "ED"]+rate_year[treatment, "hosp"])*1/52))*rate_year[treatment, "ED"]/(rate_year[treatment, "OC"]+rate_year[treatment, "ED"]+rate_year[treatment, "hosp"])
			tp[[treatment]]["chronic", "hosp"]<<-(1-dieWeekly[floor(age_x)])*(1-exp(-(rate_year[treatment, "OC"]+rate_year[treatment, "ED"]+rate_year[treatment, "hosp"])*1/52))*rate_year[treatment, "hosp"]/(rate_year[treatment, "OC"]+rate_year[treatment, "ED"]+rate_year[treatment, "hosp"])

			tp[[treatment]]["OC", "death"]<<-dieWeekly[floor(age_x)]
			tp[[treatment]]["OC", "chronic"]<<-(1-dieWeekly[floor(age_x)])

			tp[[treatment]]["ED", "death"]<<-dieWeekly[floor(age_x)]
			tp[[treatment]]["ED", "chronic"]<<-(1-dieWeekly[floor(age_x)])
	
			tp[[treatment]]["hosp", "death"]<<-dieWeekly[floor(age_x)]+1-exp(log(1-Death_HOPS_monthProb)*7/30)
			tp[[treatment]]["hosp", "chronic"]<<-1-(dieWeekly[floor(age_x)]+1-exp(log(1-Death_HOPS_monthProb)*7/30))

			tp[[treatment]]["death", "death"]<<-1;

			newDist_us<<-initialDist_us%*%tp[[treatment]]

			outcome_weekly[["cost_weekly"]][treatment, j]<<-initialDist_us[1:4]%*%c(costPar[["treatCost"]][treatment], costPar[["otherCost"]])
			outcome_weekly[["cost_weekly"]][treatment, j]<<-outcome_weekly[["cost_weekly"]][treatment, j]/((1+discountRate)^(j-1))

			outcome_weekly[["QALY_weekly"]][treatment, j]<<-(initialDist_us%*%utility[treatment, ])/52
			outcome_weekly[["QALY_weekly"]][treatment, j]<<-outcome_weekly[["QALY_weekly"]][treatment, j]/((1+discountRate)^(j-1))

			outcome_weekly[["n_OC_weekly"]][treatment, j]<<-initialDist_us[2]

			outcome_weekly[["n_ED_weekly"]][treatment, j]<<-initialDist_us[3]

			outcome_weekly[["n_hosp_weekly"]][treatment, j]<<-initialDist_us[4]


			initialDist_us<<-newDist_us

			age_counter[j]<<-age_x
		}
	}
}
















final<-function(treatment, time_horizon, c_costOM_multiply, c_costBT_multiply, c_costEX_multiply, c_discountR_multiply, c_utility_us_sum, 
			c_utility_bt_sum, c_utility_om_sum, c_exRR_OC_multiply, c_exRR_ED_multiply, c_exRR_hosp_multiply, c_exRATEusual_OC_multiply, 
			c_exRATEusual_ED_multiply, c_exRATEusual_hosp_multiply, c_deathHOSP_sum, c_age_sum, c_hospBT_firstweek, c_lambda)
{
	
	outcome<<-list(cost=rep(0, 3), QALY=rep(0, 3), n_OC=rep(0, 3), n_ED=rep(0, 3), n_hosp=rep(0, 3))
	names(outcome[["cost"]])<<-c("usual", "omalizumab", "thermoplasty")
	names(outcome[["QALY"]])<<-c("usual", "omalizumab", "thermoplasty")
	names(outcome[["n_OC"]])<<-c("usual", "omalizumab", "thermoplasty")
	names(outcome[["n_ED"]])<<-c("usual", "omalizumab", "thermoplasty")
	names(outcome[["n_hosp"]])<<-c("usual", "omalizumab", "thermoplasty")


	loadParameters(c_costOM_multiply, c_costBT_multiply, c_costEX_multiply, c_discountR_multiply, c_utility_us_sum, c_utility_bt_sum, c_utility_om_sum,
				c_exRR_OC_multiply, c_exRR_ED_multiply, c_exRR_hosp_multiply, c_exRATEusual_OC_multiply, c_exRATEusual_ED_multiply, 
				c_exRATEusual_hosp_multiply, c_deathHOSP_sum, c_age_sum, c_hospBT_firstweek, c_lambda)

	calculation(treatment, time_horizon)

	outcome[["cost"]][treatment]<<-sum(outcome_weekly[["cost_weekly"]][treatment, ])
	outcome[["QALY"]][treatment]<<-sum(outcome_weekly[["QALY_weekly"]][treatment, ])
	outcome[["n_OC"]][treatment]<<-sum(outcome_weekly[["n_OC_weekly"]][treatment, ])
	outcome[["n_ED"]][treatment]<<-sum(outcome_weekly[["n_ED_weekly"]][treatment, ])
	outcome[["n_hosp"]][treatment]<<-sum(outcome_weekly[["n_hosp_weekly"]][treatment, ])
}





#final(treatment="usual", time_horizon=5, c_costOM_multiply=1, c_costBT_multiply=1, c_costEX_multiply=1, c_discountR_multiply=1, c_utilityDIFF_sum=0, c_exRR_OC_multiply=1, c_exRR_ED_multiply=1, c_exRR_hosp_multiply=1, c_exRATEusual_OC_multiply=1, c_exRATEusual_ED_multiply=1, c_exRATEusual_hosp_multiply=1, c_deathHOSP_sum=0, c_age_sum=0, c_hospBT_firstweek=0.08, c_lambda=0)
#cost=15403.44
#QALYs=3.076358




#final(treatment="omalizumab", time_horizon=5, c_costOM_multiply=1, c_costBT_multiply=1, c_costEX_multiply=1, c_discountR_multiply=1, c_utilityDIFF_sum=0, c_exRR_OC_multiply=1, c_exRR_ED_multiply=1, c_exRR_hosp_multiply=1, c_exRATEusual_OC_multiply=1, c_exRATEusual_ED_multiply=1, c_exRATEusual_hosp_multiply=1, c_deathHOSP_sum=0, c_age_sum=0, c_hospBT_firstweek=0.08, c_lambda=0)
#cost=116994.7 
#QALYs=3.260219



#final(treatment="thermoplasty", time_horizon=5, c_costOM_multiply=1, c_costBT_multiply=1, c_costEX_multiply=1, c_discountR_multiply=1, c_utilityDIFF_sum=0, c_exRR_OC_multiply=1, c_exRR_ED_multiply=1, c_exRR_hosp_multiply=1, c_exRATEusual_OC_multiply=1, c_exRATEusual_ED_multiply=1, c_exRATEusual_hosp_multiply=1, c_deathHOSP_sum=0, c_age_sum=0, c_hospBT_firstweek=0.08, c_lambda=0)
#cost=28050.61 
#QALYs=3.234791 


#BT-usual=54579.35
#(28050.61-15403.44)/(3.234791-3.076358)

#BT-om=3655187
#(24050.61-116994.7)/(3.234791-3.260219)



#c_bt<-outcome[["cost"]]["thermoplasty"]
#c_us<-outcome[["cost"]]["usual"]
#q_bt<-outcome[["QALY"]]["thermoplasty"]
#q_us<-outcome[["QALY"]]["usual"]


#(c_bt-c_us)/(q_bt-q_us)






































