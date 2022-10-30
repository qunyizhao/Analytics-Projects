*Scatter Plot between Number of Reviews and Price;

ods graphics / reset width=6.4in height=4.8in imagemap;
proc sgplot data=WORK.DSB;
	title height=5pt "Scatter Plot (Number of Reviews V. Price)";
	scatter x=Price y=AvgRev /;
	xaxis grid;
	yaxis grid;
run;
ods graphics / reset;
title;


*Scatter Plot between Positivity Score and Price;
ods graphics / reset width=6.4in height=4.8in imagemap;
proc sgplot data=WORK.DSB;
	title height=12pt "Scatter Plot (Positivity Score V. Price)";
	scatter x=PosNeu y=AvgRev /;
	xaxis grid;
	yaxis grid;
run;
ods graphics / reset;
title;


*Examining Correlations between Price and Positivity/#Reviews;
ods noproctitle;
ods graphics / imagemap=on;
proc corr data=WORK.DSB pearson plots(maxpoints=none)=matrix;
	var Price;
	with PosNeu AvgRev;
run;

*K-Means Clustering;
ods noproctitle;
proc stdize data=WORK.DSB out=Work._std_ method=range;
	var Prin1 Prin2;
run;
proc fastclus data=Work._std_ maxclusters=3 distance;
	var Prin1 Prin2;
run;
proc delete data=Work._std_;
run;


*Principal Component Analyses for Cluster Plot;
ods noproctitle;
ods graphics / imagemap=on;
proc princomp data=WORK.FASTCLUS_SCORES plots(only)=(scree) 
		out=work.Princomp_scores;
	var Neg Pos Neu Comp AvgRev Price;
run;


*Scatter Plot with 3 Clusters (PCA + K-Means);
ods graphics / reset width=6.4in height=4.8in imagemap;
proc sgplot data=WORK.PRINCOMP_SCORES;
	title height=12pt "Scatter Plot (Positivity Score V. Price)";
	scatter x=Prin1 y=Prin2 / group=CLUSTER;
	xaxis grid;
	yaxis grid;
run;
ods graphics / reset;
title;






