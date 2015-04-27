* Written by R;
*  write.foreign(datav2, "ChicoPhen.txt", "ReadChicoPhen.sas", package = "SAS") ;

DATA  rdata ;
LENGTH
 section $ 6
 row $ 2
 TREE $ 16
 ID_notes $ 26
 Use_this_tree $ 16
 site $ 10
 week_9 $ 30
 Notes $ 33
 day $ 7
;

INFILE  "ChicoPhen.txt" 
     DSD 
     LRECL= 141 ;
INPUT
 section
 row
 Line
 SERP
 TREE
 ID_notes
 Use_this_tree
 site
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
 Notes
 day
 budburst
 days_to_leaf
;
LABEL  ID_notes = "ID.notes" ;
LABEL  Use_this_tree = "Use.this.tree" ;
LABEL  tree_num = "tree.num" ;
LABEL  week_1 = "week.1" ;
LABEL  week_2 = "week.2" ;
LABEL  week_3 = "week.3" ;
LABEL  week_4 = "week.4" ;
LABEL  Week_5 = "Week.5" ;
LABEL  Week_6 = "Week.6" ;
LABEL  Week_7 = "Week.7" ;
LABEL  week_8 = "week.8" ;
LABEL  week_9 = "week.9" ;
LABEL  week_10 = "week.10" ;
LABEL  days_to_leaf = "days.to.leaf" ;
RUN;
