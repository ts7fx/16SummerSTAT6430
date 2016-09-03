/*
*	Probelm Set 2
*	Tianye (Jack) Song, ts7fx
*	7/14/2016
*/

TITLE 'Problem Set 2';
options nocenter;
*FILENAME global definitions;
FILENAME problem1 'C:\LocalData\PS2Prob1.txt';
FILENAME problem3 'C:\LocalData\PS2Prob3.csv';
FILENAME problem4 'C:\LocalData\PS2Prob4.txt';
FILENAME problem6 'C:\LocalData\PS2Prob6.txt';

FILENAME prob7a 'C:\LocalData\PS2Prob7A.csv';
FILENAME prob7b 'C:\LocalData\PS2Prob7B.txt';
FILENAME prob7c 'C:\LocalData\PS2Prob7C.txt';


*Problem 1 Begins;
TITLE2 'Problem 1';
*Part A begins;
TITLE3 'P1 PartA';
data expt;
keep ID Gender Pre5 Time;
length ID $ 7
	Gender Pre5 $ 1;
infile problem1;
input ID $ Gender $ Weight Age Pulse Pre1 Pre2 Pre3 Pre4 Pre5 $ Expt $ 
Time Post1 Post2 Post3 Post4 Post5;
/*use IF stmt here to filter the data*/
IF _n_ >= 235 or Pre5 = '1' or Pre5 = '2' or Pre5 = '3' THEN Delete;
run;

proc print data=expt;
run;
*Part A ends;

*Part B begins;
TITLE3 'P1 PartB';

data expt;
length ID $ 7
	Gender Pre1-Pre5 Expt Post1-Post5 $ 1;
infile problem1;
input ID $ Gender $ Weight Age Pulse Pre1-Pre5 $ Expt $
Time Post1-Post5 $;
run;
/*getting rid of the last 15 observations*/
data subset;
set work.expt;
IF _n_ < 235;
run;
/*use the keep= & where= SAS data set options to print desired observations*/
proc print data=subset (keep= ID Gender Pre5 Time where=(Pre5='4' or Pre5='5'));
run;
*Part B ends;
*Problem 1 ends;

*Problem 2 begins;
TITLE2 'Problem 2';
TITLE3 ;
data preds;
input x1 x2;
y = 96.0240 - 1.8245*x1 + 0.5652*x2 + 
	0.0247*x1*x2 +0.014*x1**2 - 0.0118*x2**2;
datalines;
5 10
5 30
5 50
20 10
20 30
20 50
40 10
40 30
40 50
;
run;

proc print data=preds;
run;
*Problem 2 ends;

*Problem 3 begins;
TITLE2 'Problem 3'; 
data vote;
infile problem3 DSD;
input state $ party $ age;
run;

proc print data = vote;
run;
*Problem 3 ends;

*Problem 4 begins;
TITLE2 'Problem 4';
*Part A begins;
TITLE3 'P4 Part A';
*Using column input;
data bank;
infile problem4;
input name $ 1-15 acct $ 16-20 balance 21-26 rate 27-30;
interest = balance * rate / 100;
run;

proc print data=bank;
run;
*Part A ends;

*Part B begins;
TITLE3 'P4 Part B';
*Using formatted input;
data bank;
infile problem4;
input 	@1 name $15.
		@16 acct $5.
		@21 balance 6.
		@27 rate 4.2;
interest = balance * rate / 100;
run;

proc print data=bank;
run;

*Part B ends;
*Problem 4 ends;

*Problem 5 begins;
TITLE2 'Problem 5';
TITLE3 ;
data expt;
infile problem1;
input ID $ 1-7 Gender $ 9 Pre5 $ 29 Time 33-36;
IF _n_ >= 235 or Pre5 = '1' or Pre5 = '2' or Pre5 = '3' THEN Delete;
run;

proc print data=expt;
run;
*Problem 5 ends;

*Problem 6 begins;
TITLE2 'Problem 6';
TITLE3 ;

data stocks;
infile problem6;
input 	@1 stock $4.
		@5 purdate mmddyy10.
		@15 purprice Dollar6.
		@21 number 4.
		@25 selldate mmddyy10.
		@35 sellprice Dollar6.;
totalpur = number * purprice;
totalsell = number * sellprice;
profit = totalsell - totalpur;
run;

proc print data=stocks;
run;
*Problem 6 ends;


*Problem 7 begins;
TITLE2 'Problem 7';
TITLE3 'P7 Part A';

data emplA;
length name $ 15;
infile prob7a DSD truncover;
input ID $ name $ dept $ datehire mmddyy10. salary dollar8.0;
run;
/*put everything in emplA into sas data set employee*/
data employee;
set work.emplA;
put ID $ name $ dept $ datehire mmddyy10. salary dollar8.0;
run;

proc print data=employee;
run;



TITLE3 'P7 Part B'; 
data emplB;
length name $ 15;
infile prob7b dlm='$"' truncover;
input ID $ name $ dept $ datehire mmddyy10. salary dollar8.0;
run;

data employee;
set work.emplB;
put ID $ name $ dept $ datehire mmddyy10. salary dollar8.0;
run;

proc print data=employee;
run;


TITLE3 'P7 Part C';
data emplC;
length name $ 15;
infile prob7c dlm="*" truncover;
input ID $ name $ dept $ datehire: mmddyy10. salary dollar10.0;
run;

data employee;
set work.emplC;
put ID $ name $ dept $ datehire mmddyy10. salary dollar8.0;
run;

proc print data=employee;
run;

*Problem 7 ends;
