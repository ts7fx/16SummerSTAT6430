* SAS Project
Kevin Sun (kws8de)
Tianye Song (Ts7fx)
Fandi Lin (Fl3bf)
Yizhe Ge (Yg2kj);

libname analysis 'C:\LocalData';
*STEP 1;
*noduprecs test;
*taking unique assign/projnum from master;
title 'assignProjMaster';
data assignProjMaster;
set analysis.master(keep = assign projnum);
run;
proc sort data=assignProjMaster noduprecs;
by projnum;
run;
proc print data = assignProjMaster;
run;
*STEP 2;
*making new complete assign;
title 'complete assign';
data completeAssign;
set assignProjMaster analysis.assign;
run;
proc sort data = completeAssign noduprecs;
by projnum;
run;
proc print data = completeAssign;
run;

*STEP 3;
*merging completeAssign to newForms;
title 'complete new forms';
proc sort data = analysis.newforms;
by projnum;
run;
data completeNewForms;
merge analysis.newforms(in=inA) completeAssign(in=inB);
if inA and inB;
by projnum;
run;
proc print data = completeNewForms;
run;
*STEP 4;
*merging completeNewForms to Master;
title 'complete Master';
proc sort data = analysis.master;
by projnum;
run;
data completeMaster;
set analysis.master completeNewForms;
run;
proc sort data = completeMaster;
by projnum date;
run;
proc print data = completeMaster;
run;
*STEP 5;
*merging completeMaster with correct;
title 'almost new master';
proc sort data = analysis.correct;
by projnum date;
run;
data almostNewMaster;
update completeMaster analysis.correct(in=inC);
corrected = inC;
by projnum date;
label corrected = "Data Correct?"; 
run;
proc print data = almostNewMaster label;
run;
*Step 6;
*merging type to newmaster;
title 'analysis.newmaster';
proc sort data = analysis.type;
by projnum;
run;
data analysis.newmaster;
merge almostNewMaster analysis.type;
by projnum;
run;
proc print data = analysis.newmaster label;
run;

*Part B part i;
title "STAT 6430 Statistical Computing for Data Science";
title2 "ACL Activity Reports";
title3 "List of Ongoing Projects";
data analysis.ongoing;
set analysis.newmaster;
keep projnum;
by projnum;
if last.projnum & closed = 0 then output;
run;
proc print data = analysis.ongoing label;
run;
*Part B part ii;
title3 'Transition Data';
data transitionData;
set analysis.newmaster;
drop hours date activity closed corrected;
format stdate fndate date9.;
label ongoing = "Ongoing?" stdate = "Start Date" fndate = "Finish Date" tothours = "Total Hours"; 
by projnum;
tothours + hours;
retain stdate;
if last.projnum & closed = 0 then 
ongoing = 1;
if last.projnum & closed = 1 then
ongoing = 0;
if first.projnum then do
stdate = date;
tothours = hours;
end;
if last.projnum then do 
fndate = date;
output;
end;
run;
proc print data = transitionData label;
run;

data analysis.smith analysis.jones analysis.brown;
set transitionData;
if assign = "Ms. Smith" then output analysis.smith;
if assign = "Mr. Jones" then output analysis.jones;
if assign = "Ms. Brown" then output analysis.brown;
run;

title3 "Activity Report for Ms. Smith";
proc print data = analysis.smith label;
var projnum type tothours stdate fndate ongoing;
run;
title3 "Activity Report for Mr. Jones";
proc print data = analysis.jones label;
var projnum type tothours stdate fndate ongoing;
run;
title3 "Activity Report for Ms. Brown";
proc print data = analysis.brown label;
var projnum type tothours stdate fndate ongoing;
run;

*Part3 iii;
title3 "Overall Activity Report";
proc sort data = analysis.smith;
by tothours;
run;
proc sort data = analysis.jones;
by tothours;
run;
proc sort data = analysis.brown;
by tothours;
run;

data analysis.overall (rename=(totHours2=totHours)) ;
set analysis.smith(in=inS) analysis.jones(in=inJ) analysis.brown(in=inB);
keep assign totProj totHours2 avgHours minHours MaxHours;
label assign = "Consultant" totProj = "Total Projects" totHours2 = "Total Project Hours" avgHours = "Average Hours" minHours = "Minimum Hours" maxHours = "Maximum Hours";
by assign totHours;
retain totProj 0;
retain totHours2 0;
retain minHours;
totProj = totProj+1;
totHours2 = totHours2 + tothours;
avghours = totHours2/totProj;
if first.assign then
minHours = totHours;
if last.assign  then do
maxHours = totHours;
output;
end;
run;

proc print data = analysis.overall label;
run;
