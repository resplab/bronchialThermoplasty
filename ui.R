library(shiny)
#devtools::install_github("shiny", "rstudio")
#library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
	headerPanel("Cost-effectiveness of standard therapy, bronchial thermoplasty, and omalizumab"),
  

	sidebarPanel(

		h4("Developed by Zafar Zafari"),
		br(),
		h5("This web is based on our paper entitled 'Cost-effectiveness of bronchial thermoplasty, omalizumab, and standard therapy for moderate-to-severe allergic asthma'. 
			Authors: Zafar Zafari, Mohsen Sadatsafavi, Carlo Marra, Wenjia Chen, and Mark Fitzgerald. Please cite this paper upon using the results of this Web App."),


		br(),
		br(),

		h4("Introduction"),

		#br() and hr() produce extra horizontal space, and hr()>br()

		h5("Patients with moderate-to-severe allergic asthma have significant impairment and consume much more resources
			compared to typical asthma patients. Some patients do not achieve optimal asthma control even after being 
			adherent to their high-dose standard controller medications. For this population there have been recently some new interventions such 
			as omalizumab and bronchial thermoplasty (BT) available. BT is a technique with three bronchoscopies 
			whereby radiofrequency radiation is applied sequentially to the peripheral sub-segmental airways. The objective of this study was 
			to, comprehensively, compare the cost-effectiveness of these interventions."),

		#br() and hr() produce extra horizontal space, and hr()>br()

		br(),
		br(),



		tabsetPanel(id = "tab",       
        		tabPanel("Costs", 
				list( checkboxGroupInput("i_cost_bt", "", "Default value-Cost of BT", selected="Default value-Cost of BT"), 
					uiOutput("o_cost_bt"),
					br(),
					checkboxGroupInput("i_cost_om", "", "Default value-Cost of omalizumab (per year)", selected="Default value-Cost of omalizumab (per year)"),
					uiOutput("o_cost_om"),
					br(),
					checkboxGroupInput("i_cost_ex_oc", "", "Default value-Cost of exacerbation requiring oral corticosteroids", selected="Default value-Cost of exacerbation requiring oral corticosteroids"),
					uiOutput("o_cost_ex_oc"),
					br(),
					checkboxGroupInput("i_cost_ex_ed", "", "Default value-Cost of exacerbation requiring ED visit", selected="Default value-Cost of exacerbation requiring ED visit"),
					uiOutput("o_cost_ex_ed"),
					br(),
					checkboxGroupInput("i_cost_ex_hosp", "", "Default value-Cost of exacerbation requiring hospitalization", selected="Default value-Cost of exacerbation requiring hospitalization"),
					uiOutput("o_cost_ex_hosp")
				)
			),


        		tabPanel("Utilities", 
				list(checkboxGroupInput("i_utility_chronic_us", "", "Default value-HSUV for exacerbation-free state in standard therapy", selected="Default value-HSUV for exacerbation-free state in standard therapy"), 
					uiOutput("o_utility_chronic_us"),
					br(),
					checkboxGroupInput("i_utility_chronic_bt", "", "Default value-HSUV for exacerbation-free state in BT", selected="Default value-HSUV for exacerbation-free state in BT"), 
					uiOutput("o_utility_chronic_bt"),
					br(),
					checkboxGroupInput("i_utility_chronic_om", "", "Default value-HSUV for exacerbation-free state in omalizumab", selected="Default value-HSUV for exacerbation-free state in omalizumab"), 
					uiOutput("o_utility_chronic_om")
				)
			),





        		tabPanel("Rates and Probabilities", 
				list(checkboxGroupInput("i_rate_ex_oc", "", "Default value-Rate of exacerbation requiring oral corticosteroids in standard therapy (per year)", selected="Default value-Rate of exacerbation requiring oral corticosteroids in standard therapy (per year)"), 
					uiOutput("o_rate_ex_oc"),
					br(),
					checkboxGroupInput("i_rate_ex_ed", "", "Default value-Rate of exacerbation requiring ED visit in standard therapy (per year)", selected="Default value-Rate of exacerbation requiring ED visit in standard therapy (per year)"), 
					uiOutput("o_rate_ex_ed"),
					br(),
					checkboxGroupInput("i_rate_ex_hosp", "", "Default value-Rate of exacerbation requiring hospitalization in standard therapy (per year)", selected="Default value-Rate of exacerbation requiring hospitalization in standard therapy (per year)"), 
					uiOutput("o_rate_ex_hosp"),
					br(),
					checkboxGroupInput("i_rr_ex_oc", "", "Default value-Relative rate of exacerbation requiring oral corticosteroids in BT vs standard therapy", selected="Default value-Relative rate of exacerbation requiring oral corticosteroids in BT vs standard therapy"), 
					uiOutput("o_rr_ex_oc"),
					br(),
					checkboxGroupInput("i_rr_ex_ed", "", "Default value-Relative rate of exacerbation requiring ED visit in BT vs standard therapy", selected="Default value-Relative rate of exacerbation requiring ED visit in BT vs standard therapy"), 
					uiOutput("o_rr_ex_ed"),
					br(),
					checkboxGroupInput("i_rr_ex_hosp", "", "Default value-Relative rate of exacerbation requiring hospitalization in BT vs standard therapy", selected="Default value-Relative rate of exacerbation requiring hospitalization in BT vs standard therapy"), 
					uiOutput("o_rr_ex_hosp"),
					br(),
					checkboxGroupInput("i_decline_rate_bt", "", "Default value-Rate of declining effect associated with BT's effect size after fifth year", selected="Default value-Rate of declining effect associated with BT's effect size after fifth year"), 
					uiOutput("o_decline_rate_bt"),
					br(),
					checkboxGroupInput("i_early_hosp_bt", "", "Default value-Proportion of patients hospitalized early post BT", selected="Default value-Proportion of patients hospitalized early post BT"), 
					uiOutput("o_early_hosp_bt"),
					br(),
					checkboxGroupInput("i_death_after_hosp", "", "Default value-Probability of death within 30 days post hospitalization", selected="Default value-Probability of death within 30 days post hospitalization"), 
					uiOutput("o_death_after_hosp")
				)
			),






        		tabPanel("other parameters", 
				list(checkboxGroupInput("i_time_horizon", "", "Default value-Time horizon of the model", selected="Default value-Time horizon of the model"), 
					uiOutput("o_time_horizon"),
					br(),
					checkboxGroupInput("i_discount_rate", "", "Default value-Future discount rate", selected="Default value-Future discount rate"), 
					uiOutput("o_discount_rate"),
					br(),
					checkboxGroupInput("i_baseline_age", "", "Default value-Baseline age", selected="Default value-Baseline age"), 
					uiOutput("o_baseline_age")
				)
			)
		),




		br(),
		br()
		#submitButton("xx")
   	 ),


    	# Show a summary of the dataset and an HTML table with the
    	# requested number of observations. Note the use of the h4
    	# function to provide an additional header above each output
    	# section.
   	 mainPanel(

		plotOutput("figure"),
		br(),
		br(),
		br(),
		br(),
		br(),
		br(),
		paste("Table 1. Ranking of the interventions based on their net benefit"),
		tableOutput("ranking"),
		br(),
		br(),
		br(),
		br(),
		br(),
		br(),
		paste("Table 2. Model Outcomes"),
		tableOutput("table")


    )
 
))


