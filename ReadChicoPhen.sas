/* Author: Robin Jeffries
/* Date: 5/5/15
/* Purpose: Data management for Oak tree phenom data
******************************************************/


%let libname = C:\GitHub\OakResearchRepoSP2015;
libname s "&libname";


DATA phen ;
format
	section $6.
 	row $2.
 	TREE $16.
 	ID_notes $26.
 	Use_this_tree $16.
 	site $10.
 	Notes $33.
 	day $7.
;

INFILE  "ChicoPhenv2.csv"  delimiter=',' missover DSD firstobs=2;
     
INPUT
 section $
 row $
 Line $
 SERP 
 TREE $
 ID_notes $
 Use_this_tree $
 site $
 acc
 tree_num
 week_1
 week_2
 week_3
 week_4
 Week_5
 Week_6
 Week_7
 week_8
 week_9
 week_10
 Notes $
 day $
;
run;


data phen2;
	set phen;

	* Clean up site names;
	if (index(site, "TAC")>0) then site = "TAC";

	* Add a count of weeks from planting until bud burst (First recorded 1);
	array b(*) week_1--week_10;
	bb = .; fl = .; 
	if week_1>0 then bb=1;		if week_1=5 then fl=1;
	do i = 2 to dim(b);
		if (b[i-1]=0 & b[i]>0 ) then bb = i;
		if (b[i-1]<5 & b[i]=5) then fl = i;
	end;

	drop i ;
run;

* Merge with latlong data;
