This exercise is based on synthetic data created on purpose in 0_create_dataset.R. 

We pretend we have found a dataset collected 25 years ago during a security check at an airport. For one day, data were saved on everything that travelers had in their pockets. We also pretent that we are now able to link that dataset with cancer diagnoses collected from that day onward. 

Exercise A
==========

As we start analyzing some variables in the dataset, we find an alarming result regarding people who carried a lighter: they have a higher incidence of cancer. Should we advise people to stop carrying lighters?

Open script A_1_analysis_counfounded.R and run it. The script loads in the environment a dataset -data- and generates the frequency of cancer stratified per presence/absence of lighter

We then realise that the dataset is actually richer. Besides lighters, we also have a variable that stores whether the traveler had cigarettes in the pocket.

Open script A_2_analysis_no_counfounding.R and run it progressively. It will illustrate how to ascertain that the previous finding was confounded.

Exercise B
==========

We now see that we also have information on whether the cancer was lung cancer, and on death. We find that among those who had lung cancer, those with cigarettes have a lower probability of dying. 

Open B_1_analysis_with_selection_bias.R to explore this association.

This association is not causal and is due to selection bias. Addressing this bias requires more theory: to properly frame a causal question in an observational study, you must think as if you were conducting a trial. This is the framework of the Target Trial Emulation.  