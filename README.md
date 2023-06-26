# LATE IV: Distributional Implications of LATE 

It is common to use instrumental variable regression for analyzing experiments, e.g., GOTV, and lotteries, where only a fraction of the 
people comply. One of the standard assumptions behind the standard LATE estimate is that only compliers are going to see a treatment effect. 
For instance, we don't expect GOTV efforts over the phone to affect people we aren't able to reach. Or we don't expect people who were 
merely drawn up in the Vietnam Draft Lottery to have different attitudes towards minorities. Whatever effects we see, we expect them to 
result from service. All of this implies that the distribution of ITT looks closer to Figure 1d 
[here (pdf)](http://www.stat.columbia.edu/~gelman/research/unpublished/causal_quartets.pdf)

One way to check if the data are consistent with the implications of how the effect is distributed is to simulate the lumpy treatment 
effect and then check how closely the empirical distribution matches the theorized.

