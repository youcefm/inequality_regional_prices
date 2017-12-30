clear matrix
clear
cd "E:\Inequality_regionalprices"
set memory 100m

do "E:\Inequality_regionalprices\2012marchCPSformating.do"

drop if inctot==99999999
drop if inctot<=0
drop if age<24
drop if age>70

gen stateRPP=0


replace stateRPP= 93.2 if statefip==01
replace stateRPP= 113.4 if statefip==02
replace stateRPP= 103.8 if statefip==04
replace stateRPP= 92.7 if statefip==05
replace stateRPP= 119.5 if statefip==06
replace stateRPP= 107.6 if statefip==08
replace stateRPP= 115.8 if statefip==09
replace stateRPP= 108.3 if statefip==10
replace stateRPP= 125.1 if statefip==11
replace stateRPP= 104.6 if statefip==12
replace stateRPP= 97.3 if statefip==13
replace stateRPP= 124.1 if statefip==15
replace stateRPP= 99.0 if statefip==16
replace stateRPP= 106.4 if statefip==17
replace stateRPP= 96.4 if statefip==18
replace stateRPP= 94.7 if statefip==19
replace stateRPP= 95.1 if statefip==20
replace stateRPP= 94.0 if statefip==21
replace stateRPP= 96.7 if statefip==22
replace stateRPP= 104.1 if statefip==23
replace stateRPP= 117.8 if statefip==24
replace stateRPP= 113.4 if statefip==25
replace stateRPP= 99.9 if statefip==26
replace stateRPP= 103.1 if statefip==27
replace stateRPP= 91.5 if statefip==28
replace stateRPP= 93.3 if statefip==29
replace stateRPP= 99.7 if statefip==30
replace stateRPP= 95.4 if statefip==31
replace stateRPP= 103.9 if statefip==32
replace stateRPP= 112.4 if statefip==33
replace stateRPP= 120.7 if statefip==34
replace stateRPP= 100.4 if statefip==35
replace stateRPP= 122.1 if statefip==36
replace stateRPP= 96.9 if statefip==37
replace stateRPP= 95.6 if statefip==38
replace stateRPP= 94.4 if statefip==39
replace stateRPP= 95.1 if statefip==40
replace stateRPP= 104.6 if statefip==41
replace stateRPP= 104.4 if statefip==42
replace stateRPP= 104.5 if statefip==44
replace stateRPP= 96.0 if statefip==45
replace stateRPP= 93.3 if statefip==46
replace stateRPP= 96.0 if statefip==47
replace stateRPP= 102.2 if statefip==48
replace stateRPP= 102.5 if statefip==49
replace stateRPP= 106.8 if statefip==50
replace stateRPP= 109.2 if statefip==51
replace stateRPP= 109.2 if statefip==53
replace stateRPP= 93.7 if statefip==54
replace stateRPP= 98.3 if statefip==55
replace stateRPP= 102.0 if statefip==56


gen inctotRPP = 100*(inctot/stateRPP) // price corrected income

pctile inc_pt = inctot, nquantiles(10) // national income decile thresholds
pctile incRPP_pt = inctotRPP, nquantiles(10) // national corrected income deciles thresholds

gen bottom_ten = inctot<= inc_pt[1] // indicates if individual is in national bottom decile
gen top_ten = inctot>= inc_pt[9] // indicates if individual is in national top decile
gen bottomRPP_ten = inctotRPP<= incRPP_pt[1] // indicates if individual is in national corrected bottom decile
gen topRPP_ten = inctotRPP>= incRPP_pt[9]  // indicates if individual is in national corrected top decile

*outsheet using RegionalInequality.csv, comma //deprecated
export delimited using RegionalInequality.csv, delimiter(",") replace



