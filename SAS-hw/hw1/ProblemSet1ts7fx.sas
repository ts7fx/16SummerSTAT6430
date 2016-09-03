/*
 * Stat6430 Problem Set 1
 * Tianye (Jack) Song, ts7fx
 * 7/12/2016
 */

*Probelm 1 begins;
Title1 'Problem Set 1';
Title2 'Problem 1';
Options nocenter;

Data Prob1Data;
Infile 'C:\LocalData\PS1Prob1.txt';
Input obj $ hgt wid;
Run;

Proc print;
Var obj wid hgt;
Run;
*Problem 1 ends;


*Problem 5 begins;
Title2 'Problem 5';

Data expt;
Infile 'C:\LocalData\PS1Prob5.txt';
Input sample $ temp_degF press_psi;
temp_degC = 5/9 * (temp_degF - 32);
press_Pa = 6894.757 * press_psi;
quadT2 = temp_degC * temp_degC;
quadP2 = press_Pa * press_Pa;
quadTP = temp_degC * press_Pa;
Run;
Proc print;
Run;
*Problem 5 ends;


*Problem 7 begins;
Title2 'Problem 7';
Data PS1Prob7;
Infile 'C:\LocalData\PS1Prob7.txt';
Input class $ x1 y1 y2;
Run;

Proc means data = PS1Prob7 mean median;
var x1 y1 y2;
run;

Proc freq data=PS1Prob7;
table class / nocum nopercent;
run;

Proc univariate data=PS1Prob7;
var x1;
qqplot x1;
run;

Proc corr data=PS1Prob7 noprob;
var x1 y1;
run;
*Problem 7 ends;