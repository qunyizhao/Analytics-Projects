*SAS Codes;


*PCA and K-Means Clustering for CRM Project;

*First, we will run PCA for the variables below (listed in code line 21) to derive PCs against
which we will examine our clusters;

*Principal Component Analysis;
proc princomp data=WORK.FASTCLUS_SCORES plots(only)=(scree) 
out=work.Princomp_scores;
var time_in_days total_amount reward difficulty duration age income;
run;

*Now, we will run a K-Means analysis for the same variables and obtain cluster scores and 
information on how they are distinct from each other;

*K-Means Clustering;
proc stdize data=WORK.CRM out=Work._std_ method=range;
var time_in_days total_amount reward difficulty duration age income;
run;

proc fastclus data=Work._std_ maxclusters=3 out=work.Fastclus_scores;
var time_in_days total_amount reward difficulty duration age income;
run;

proc delete data=Work._std_;
run;

*Lastly, we will use PCA and K-Means data/analysis to visualise it in a meaningful way through
scatter plots - we will use the 2 PCs that explain the most variance combined, and plot them
with the cluster scores;

*Scatter Plot;
proc sgplot data=WORK.PRINCOMP_SCORES;
title height=12pt "K-Means Clustering";
scatter x=Prin1 y=Prin2 / group=CLUSTER markerattrs=(size=1);
xaxis label="Principal Component 1";
yaxis label="Principal Component 2";
keylegend / location=inside;
run;

*The End;
