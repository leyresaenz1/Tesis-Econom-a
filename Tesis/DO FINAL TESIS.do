******************************
*    Applied Econometrics    *
*      Tesis de Grado        *
*******************************
clear all
set more off
cd "/Users/apple/Desktop/TESIS GRADO/Microdatos EPH"
/*
import excel "/Users/apple/Desktop/TESIS GRADO/Microdatos EPH/usu_individual_t216.xls", sheet("Sheet 1") clear

forvalues v=16/16 {
forvalues x=2/4 { 
	import excel "/Users/apple/Desktop/TESIS GRADO/Microdatos EPH/usu_individual_t`x'`v'.xls", sheet("Sheet 1") firstrow clear
	keep if CH06==-1
	save eph_t`x'`v', replace
	} 
} 

forvalues v=17/21 { 
	forvalues x=1/4 { 
	import excel "/Users/apple/Desktop/TESIS GRADO/Microdatos EPH/usu_individual_t`x'`v'.xls", sheet("Sheet 1") firstrow clear
	keep if CH06==-1
	save eph_t`x'`v', replace
	} 
} 
*/
set more off
use eph_t216 , clear
append using eph_t316, force
append using eph_t416, force
append using eph_t117, force
append using eph_t217, force
append using eph_t317, force
append using eph_t417, force
append using eph_t118, force
append using eph_t218, force
append using eph_t318, force
append using eph_t418, force
append using eph_t119, force
append using eph_t219, force
append using eph_t319, force
append using eph_t419, force
append using eph_t120, force
append using eph_t220, force
append using eph_t320, force
append using eph_t420, force
append using eph_t121, force
append using eph_t221, force
append using eph_t321, force
append using eph_t421, force

unique CODUSU CH05 COMPONENTE

duplicates drop CODUSU CH05 COMPONENTE, force

rename ANO4 año
rename TRIMESTRE trimestre
rename AGLOMERADO aglomerado
rename REGION region
rename CH03 parentesco
rename CH04 sexo
rename CH05 mesnac
rename CH06 edad
rename CH07 estadocivil
rename CH08 cob_medica
rename CH09 alfabetismo
rename CH10 educ
rename CH11 educpubli
rename CH12 niveleduc
rename CH13 fineduc

label variable parentesco "Relación de parentesco con el jefe de hogar"
label variable cob_medica "Posee algún tipo de cobertura médica por la que paga o le descuentan"
label variable alfabetismo "Sabe leer y escribir"
label variable educ "Asiste o no a un establecimiento educativo"
label variable educpubli "Establecimiento educativo público o privado"
label variable niveleduc "Nivel educativo alcanzado"
label variable fineduc "Finalizó último nivel educativo o no"

** Genero variable de niños nacidos

gen nacimientos = edad

recode nacimientos (-1 = 1) (else=0) if nacimientos!=.

label variable nacimientos "Total de nacimientos por mes y aglomerado"


** Genero variable mes 

generate date = string(mesnac, "%td")

gen yy=substr(date,-2,.)

gen mmm=substr(date,-7,3)

replace mmm="01" if mmm=="jan"
replace mmm="02" if mmm=="feb"
replace mmm="03" if mmm=="mar"
replace mmm="04" if mmm=="apr"
replace mmm="05" if mmm=="may"
replace mmm="06" if mmm=="jun"
replace mmm="07" if mmm=="jul"
replace mmm="08" if mmm=="aug"
replace mmm="09" if mmm=="sep"
replace mmm="10" if mmm=="oct"
replace mmm="11" if mmm=="nov"
replace mmm="12" if mmm=="dec"

drop if yy=="00"
drop if yy=="06"
drop if yy=="09"
drop if yy=="14"
drop if yy=="15"
drop if yy=="99"
drop if yy=="89"

replace yy="2016" if yy=="16"
replace yy="2017" if yy=="17"
replace yy="2018" if yy=="18"
replace yy="2019" if yy=="19"
replace yy="2020" if yy=="20"
replace yy="2021" if yy=="21"

egen yymmm=concat(yy mmm)
tab yymmm

rename yy year
rename mmm month

**unique CODUSU nacimientos COMPONENTE

**duplicates drop CODUSU nacimientos COMPONENTE, force

save microdatoseph, replace 

use microdatoseph, clear

drop if yymmm==""


** Genero variable trimestral
drop trimestre
*destring year, replace
*destring month, replace

** Loop--> agrupamos nacimientos por trimestre
** creo variables cuatrimestral
gen trimestre = month
replace trimestre = "1" if trimestre =="01"
replace trimestre = "1" if trimestre =="02"
replace trimestre = "1" if trimestre =="03"
replace trimestre = "2" if trimestre =="04"
replace trimestre = "2" if trimestre =="05"
replace trimestre = "2" if trimestre =="06"
replace trimestre = "3" if trimestre =="07"
replace trimestre = "3" if trimestre =="08"
replace trimestre = "3" if trimestre =="09"
replace trimestre = "4" if trimestre =="10"
replace trimestre = "4" if trimestre =="11"
replace trimestre = "4" if trimestre =="12"

destring trimestre, replace
destring year, replace

gen time_linear=.
local z=0
forv y=2016/2021 {
	forv t=1/4{
		local z=`z'+1
		replace time_linear=`z' if trimestre==`t' & year==`y'
	}
}


/////////////////////////////////////////////////////////////////////////////

** Collapseo por vars importantes

collapse (sum) nacimientos [fw=PONDERA], by(time_linear aglomerado)

unique time_linear aglomerado 

*tostring yymmm, gen (yymmm_s)
*encode yymmm, gen (id_yymmm)

** Genero variable decreto--> toma valor 0 para los controles y 1 para tratados. 

gen decreto=0

replace decreto=1 if time_linear>20 & (aglomerado==2|aglomerado==3|aglomerado==4|aglomerado==5|aglomerado==6|aglomerado==7|aglomerado==8|aglomerado==9|aglomerado==10|aglomerado==12|aglomerado==14|aglomerado==15|aglomerado==17|aglomerado==18|aglomerado==19|aglomerado==20|aglomerado==22|aglomerado==23|aglomerado==25|aglomerado==26|aglomerado==27|aglomerado==29|aglomerado==30|aglomerado==31|aglomerado==34|aglomerado==36|aglomerado==38|aglomerado==93)

label variable decreto "Dummy=1 si se aplicó el decreto en dicho aglomerado" 

** Variable explicada --> le pongo logaritmo

gen ln_nacimientos = log(nacimientos)

drop nacimientos

rename ln_nacimientos nacimientos

** Genero variable año-mes decreto 
gen year_decret = decreto
recode year_decret (0 = .) (else=21) if year_decret!=.

///////Chequear Tendencias Previas///////

* Generate variable eventually treated

bys aglomerado: egen eventually=max(year_decret)
replace eventually=1 if eventually!=.
recode eventually (.=0)

unique aglomerado, by(eventually)

*Genero variable año tratamiento
gen treatment_year = 21 if eventually==1
replace treatment_year=. if eventually==0

* Basic Graph (pre treatment by year)--> gráfico para esos 7 años de nunca tratados

bysort time_linear even: egen mean=mean(nacimientos)
sort time_linear even

twoway (connected mean time_linear if even==1 & time_linear<21) (connected mean time_linear if even==0 & time_linear<21, lpattern(longdash)), ytitle(Mean Outcome) xtitle (Year) legend(order(1 "Eventually treated" 2 "Controls")) 
*twoway (connected mean time_linear if even==1 & time_linear<21), ytitle(Mean Outcome) xtitle (Year) legend(order(1 "Eventually treated")) name(g1,replace)
*twoway (connected mean time_linear if even==0 & time_linear<21, lpattern(longdash)), ytitle(Mean Outcome) xtitle (Year) legend(order(1 "Controls")) name(g2,replace)

** Event Study Graph (Stata Command)
local treat_id aglomerado
local time time_linear
local treatment decreto
local outcome nacimientos
local treatment_year treatment_year

xtset aglomerado time_linear 

gen timeToTreat = `time' - `treatment_year'

save temp.dta, replace

eventdd `outcome' i.`time', timevar(timeToTreat) method(fe,cluster(`treat_id')) keepdummies graph_op(ytitle("Outcome") name(g1, replace))
	
estat eventdd

estat eventdd, wboot seed(1234)

** Event Study Graph (by hand)
ssc install reghdfe 
ssc install ftools
ssc install coefplot

local treat_id aglomerado
local time time_linear
local treatment decreto
local outcome nacimientos

bys `treat_id': egen ev_treat=max(`treatment')
bys `treat_id': gen tdate=`time' if `treatment'==1
bys `treat_id': egen treatdate=min(tdate)
gen window=`time'-treatdate if ev_treat==1
tab window, gen(dummy)

** CHOOSE j
local j = -21
forvalues i=1/`r(r)'{
replace dummy`i'=0 if ev_treat==0
local j = `j'+1
label var dummy`i' "`j'"
}

local treat_id aglomerado
local time time_linear
local treatment decreto
local outcome nacimientos
local treatment_year treatment_year

** Normal

reghdfe `outcome' dummy1-dummy20 dummy22-dummy25, absorb(`time' `treat_id') cluster(`treat_id')

coefplot , keep(dummy1 dummy2 dummy3 dummy4 dummy5 dummy6 dummy7 dummy8 dummy9 dummy10 dummy11 dummy12 dummy13 dummy14 dummy15 dummy16 dummy17 dummy18 dummy19 dummy20 dummy22 dummy23 dummy24 dummy25) vertical yline(0) xline(20) ci(95) xtitle("Tiempo desde decreto") ytitle("Nacimientos") ciopts(recast(rcap)) baselevels omitted legend(off) name(g2, replace) 

** Limitar hasta el primer periodo 

reghdfe `outcome' dummy1-dummy20, absorb(`time' `treat_id') cluster(`treat_id')

coefplot , keep(dummy1 dummy2 dummy3 dummy4 dummy5 dummy6 dummy7 dummy8 dummy9 dummy10 dummy11 dummy12 dummy13 dummy14 dummy15 dummy16 dummy17 dummy18 dummy19 dummy20) vertical yline(0) xline(20) ci(95) xtitle("Tiempo desde decreto") ytitle("Nacimientos") ciopts(recast(rcap)) baselevels omitted legend(off) name(g2, replace) 

** Elimino SD grande

reghdfe `outcome' dummy1-dummy19, absorb(`time' `treat_id') cluster(`treat_id')

coefplot , keep(dummy1 dummy2 dummy3 dummy4 dummy5 dummy6 dummy7 dummy8 dummy9 dummy10 dummy11 dummy12 dummy13 dummy14 dummy15 dummy16 dummy17 dummy18 dummy19) vertical yline(0) xline(20) ci(95) xtitle("Tiempo desde decreto") ytitle("Nacimientos") ciopts(recast(rcap)) baselevels omitted legend(off) name(g2, replace) 

** Primeros trimestres

*PORQUE SE ME DUPLICA DUMMY1 Y DUMMY2???

reghdfe `outcome' dummy2 dummy6 dummy10 dummy14 dummy18 dummy22, absorb(`time' `treat_id') cluster(`treat_id')

coefplot , keep(dummy2 dummy6 dummy10 dummy14 dummy18 dummy22) vertical yline(0) ci(95) xtitle("Decreto") ytitle("Nacimientos") ciopts(recast(rcap)) baselevels omitted legend(off) name(g2, replace) xline(5) 

////////////////////////////////////////////////////////////////////////////////

** Regresión: estimamos el diff-in-diffs tradicional

xtset aglomerado time_linear 

xtreg nacimientos decreto i.time_linear, fe i(aglomerado) vce(boot)

xtreg nacimientos decreto i.time_linear if time_linear<22, fe i(aglomerado) vce(boot)

outreg2 using regresión.xls, excel ctitle(Nacimientos por trimestre)

** LEAVE ONE OUT

tostring aglo, replace
encode aglomerado, gen(id)

forv y=1/32 {
preserve
drop if id==`y'
local treat_id aglomerado
local time time_linear
local treatment decreto
local outcome nacimientos

*drop ev_treat tdate treatdate window
bys `treat_id': egen ev_treat=max(`treatment')
bys `treat_id': gen tdate=`time' if `treatment'==1
bys `treat_id': egen treatdate=min(tdate)
gen window=`time'-treatdate if ev_treat==1
tab window, gen(dummy)

** CHOOSE j
local j = -21
forvalues i=1/`r(r)'{
replace dummy`i'=0 if ev_treat==0
local j = `j'+1
label var dummy`i' "`j'"
}

local treat_id aglomerado
local time time_linear
local treatment decreto
local outcome nacimientos
local treatment_year treatment_year

reghdfe `outcome' dummy1-dummy20 dummy22-dummy25 , absorb(`time' `treat_id') cluster(`treat_id')

coefplot , keep (dummy1 dummy2 dummy3 dummy4 dummy5 dummy6 dummy7 dummy8 dummy9 dummy10 dummy11 dummy12 dummy13 dummy14 dummy15 dummy16 dummy17 dummy18 dummy19 dummy20 dummy22 dummy23 dummy24 dummy25) vertical yline(0) xline(20) ci(95) xtitle("Tiempo desde decreto") ytitle("Nacimientos") ciopts(recast(rcap)) baselevels omitted legend(off) name(g2, replace) 

restore
}




