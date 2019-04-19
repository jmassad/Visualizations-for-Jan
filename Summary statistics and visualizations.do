//basic summaries by independent dataset

cd "D:\WB Tax Consultancy"
use "Master Dataset.dta", clear

/************************/
/**********CPIA**********/
/************************/

//variables of interest: ermrating (efficiency of revenue mobilization), fispolrating (fiscal policy), and qualpubadminrating (quality of public administration)

//region-wide summary statistics in 2015
sum ermrating fispolrating qualpubadminrating if Reg==7 & year==2015

//histograms in 2015
kdensity ermrating if Reg==7 & year==2015
kdensity fispolrating if Reg==7 & year==2015
kdensity qualpubadminrating if Reg==7 & year==2015

//region-wide averages and distribution over time
twoway (scatter ermrating year if Reg==7 & year>=2005, jitter(3))(lfitci ermrating year if Reg==7 & year>=2005), ytitle("DRM Efficiency (CPIA)") note("Each marker represents a different country.""The fitline shows a 95% confidence interval around the mean.") legend(off) title("DRM Efficiency in SSA") subtitle("The distribution has changed, but the average has not.") ylabel(1(1)5) scheme(sj)
twoway (scatter fispolrating year if Reg==7 & year>=2005, jitter(3))(lfitci fispolrating year if Reg==7 & year>=2005), ytitle("Fiscal Policy (CPIA)") note("Each marker represents a different country.""The fitline shows a 95% confidence interval around the mean.") legend(off) title("Fiscal Policy in SSA") subtitle("The distribution has changed, but the average has not.") ylabel(1(1)5) scheme(sj)
twoway (scatter qualpubadminrating year if Reg==7 & year>=2005, jitter(3))(lfitci qualpubadminrating year if Reg==7 & year>=2005), ytitle("Public Administration (CPIA)") note("Each marker represents a different country.""The fitline shows a 95% confidence interval around the mean.") legend(off) title("Quality of Public Administration in SSA") subtitle("The distribution has changed, but the average has not.") ylabel(1(1)5) scheme(sj)

/***relationships***/

//democracy
cap gen democratic=.
replace democratic=1 if democracy>0
replace democratic=0 if democracy<=0

*just in 2015
ttest ermrating if year==2015 & Reg==7, by(democratic) level(90) //not significant
ttest fispolrating if year==2015 & Reg==7, by(democratic) level(90) //significant
ttest qualpubadminrating if year==2015 & Reg==7, by(democratic) level(90) //not significant

*pooled 2005-2015 (makes the results significant, but the statistical merit is dubious)
//ttest ermrating if year>=2005 & Reg==7, by(democratic) //significant
//ttest fispolrating if year>=2005 & Reg==7, by(democratic) //significant
//ttest qualpubadminrating if year>=2005 & Reg==7, by(democratic) //significant

*bar graphs
cap gen demotemp=""
replace demotemp="Democracy" if democratic==1
replace demotemp="Non-Democracy" if democratic==0
//graph bar ermrating          if year>=2005 & Reg==7, over(demotemp, gap(*1.5)) ytitle("Average DRM Efficiency (CPIA)") title("Democracies mobilize revenue more efficiently in SSA", size(medsmall)) subtitle("p<0.001", size(small)) ylabel(1(1)5) scheme(economist)
//graph bar fispolrating       if year>=2005 & Reg==7, over(demotemp, gap(*1.5)) ytitle("Average Fiscal Policy Rating (CPIA)") title("Democracies have better fiscal policy in SSA", size(medsmall)) subtitle("p<0.001", size(small)) ylabel(1(1)5) scheme(economist)
//graph bar qualpubadminrating if year>=2005 & Reg==7, over(demotemp, gap(*1.5)) ytitle("Average Administrative Quality (CPIA)") title("Democracies have higher-quality administrations in SSA", size(medsmall)) subtitle("p<0.01", size(small)) ylabel(1(1)5) scheme(economist)
bysort Reg year democratic: egen avefispol=mean(fispolrating)
gen democilb=.
gen demociup=.
ci mean fispolrating if democratic==1 & Reg==7 & year==2015, level(90)
replace democilb = r(mean) - r(se) if democratic==1 & Reg==7 & year==2015
replace demociup = r(mean) + r(se) if democratic==1 & Reg==7 & year==2015
ci mean fispolrating if democratic==0 & Reg==7 & year==2015, level(90)
replace democilb = r(mean) - r(se) if democratic==0 & Reg==7 & year==2015
replace demociup = r(mean) + r(se) if democratic==0 & Reg==7 & year==2015
graph twoway (bar avefispol democratic if year==2015 & Reg==7)(rcap democilb demociup democratic), ytitle("Average Fiscal Policy Rating (CPIA)") title("Fiscal Policy in SSA") subtitle("Democracies have better fiscal policy in SSA*") note("*bars show standard errors, p<.1") xtitle("") ylabel(1(1)5) legend(off) xlabel( 0 "Undemocratic" 1 "Democratic") //scheme(sj)

bysort Reg year democratic: egen aveerm=mean(ermrating)
replace democilb=.
replace demociup=.
ci mean ermrating if democratic==1 & Reg==7 & year==2015, level(90)
replace democilb = r(mean) - r(se) if democratic==1 & Reg==7 & year==2015
replace demociup = r(mean) + r(se) if democratic==1 & Reg==7 & year==2015
ci mean ermrating if democratic==0 & Reg==7 & year==2015, level(90)
replace democilb = r(mean) - r(se) if democratic==0 & Reg==7 & year==2015
replace demociup = r(mean) + r(se) if democratic==0 & Reg==7 & year==2015
graph twoway (bar aveerm democratic if year==2015 & Reg==7)(rcap democilb demociup democratic), ytitle("Average DRM Efficiency (CPIA)") title("DRM Efficiency in SSA") subtitle("DRM Efficiency Does Not Differ Between Democracies and Nondemocracies", size(medsmall)) note("*bars show standard errors") xtitle("") ylabel(1(1)5) legend(off) xlabel( 0 "Undemocratic" 1 "Democratic") //scheme(sj)


bysort Reg year democratic: egen avequalpub=mean(qualpubadminrating)
replace democilb=.
replace demociup=.
ci mean qualpubadminrating if democratic==1 & Reg==7 & year==2015, level(90)
replace democilb = r(mean) - r(se) if democratic==1 & Reg==7 & year==2015
replace demociup = r(mean) + r(se) if democratic==1 & Reg==7 & year==2015
ci mean qualpubadminrating if democratic==0 & Reg==7 & year==2015, level(90)
replace democilb = r(mean) - r(se) if democratic==0 & Reg==7 & year==2015
replace demociup = r(mean) + r(se) if democratic==0 & Reg==7 & year==2015
graph twoway (bar avequalpub democratic if year==2015 & Reg==7)(rcap democilb demociup democratic), ytitle("Average Administrative Quality (CPIA)") title("Administrative Quality in SSA") subtitle("Administration Quality Does Not Differ Between Democracies and Nondemocracies", size(small)) note("*bars show standard errors") xtitle("") ylabel(1(1)5) legend(off) xlabel( 0 "Undemocratic" 1 "Democratic") //scheme(sj)


//oil and non-oil

*just in 2005
ttest ermrating if year==2015 & Reg==7, by(oil_gas_dum) //not significant
ttest ermrating if year==2015 & Reg==7, by(oil_gas_dum) //not significant
ttest ermrating if year==2015 & Reg==7, by(oil_gas_dum) //not significant

*pooled 2005-2015
//ttest ermrating if year>=2005 & Reg==7, by(oil_gas_dum) //significant
//ttest ermrating if year>=2005 & Reg==7, by(oil_gas_dum) //significant
//ttest ermrating if year>=2005 & Reg==7, by(oil_gas_dum) //significant

*bar graphs
cap gen oiltemp=""
replace oiltemp="Oil and Gas" if oil_gas_dum==1
replace oiltemp="Non Oil and Gas" if oil_gas_dum==0
//graph bar ermrating          if year>=2005 & Reg==7, over(oiltemp) ytitle("Average DRM Efficiency (CPIA)") title("Non oil-rich countries mobilize revenue more efficiently in SSA", size(medsmall)) subtitle("p<0.001", size(small)) xsize(2.5) ylabel(1(1)5) scheme(economist)
//graph bar fispolrating       if year>=2005 & Reg==7, over(oiltemp, gap(*1.5)) ytitle("Average Fiscal Policy Rating (CPIA)") title("Non oil-rich countries have better fiscal policy in SSA", size(medsmall)) subtitle("p<0.001", size(small)) ylabel(1(1)5) scheme(economist)
//graph bar qualpubadminrating if year>=2005 & Reg==7, over(oiltemp) ytitle("Average Administrative Quality (CPIA)") title("Non oil-rich countries have higher-quality administrations in SSA", size(medsmall)) subtitle("p<0.001", size(small)) xsize(2.5) ylabel(1(1)5) scheme(economist)

//multicolinearity
pwcorr ermrating fispolrating qualpubadminrating, star(.05)

//bivariate relationships
reg Tax_Revenue ermrating if Reg==7
twoway (scatter Tax_Revenue ermrating if Reg==7)(lfitci Tax_Revenue ermrating if Reg==7), xlabel(1(1)5) legend(off) ytitle("Tax Revenue as a % of GDP") title("DRM Efficiency and Tax Revenue in SSA")

reg Tax_Revenue fispolrating if Reg==7
twoway (scatter Tax_Revenue fispolrating if Reg==7)(lfitci Tax_Revenue fispolrating if Reg==7), xlabel(1(1)5) legend(off) ytitle("Tax Revenue as a % of GDP") title("Fiscal Policy Rating and Tax Revenue in SSA")

reg Tax_Revenue qualpubadminrating if Reg==7
twoway (scatter Tax_Revenue qualpubadminrating if Reg==7)(lfitci Tax_Revenue qualpubadminrating if Reg==7), xlabel(1(1)5) legend(off) ytitle("Tax Revenue as a % of GDP") title("Quality of Public Administration and Tax Revenue in SSA")

//reg ermrating ln_GDP_PC if Reg==7
//twoway (scatter ermrating ln_GDP_PC if Reg==7)(lfitci ermrating ln_GDP_PC if Reg==7), ylabel(1(1)5)

//reg fispolrating ln_GDP_PC if Reg==7
//twoway (scatter fispolrating ln_GDP_PC if Reg==7)(lfitci fispolrating ln_GDP_PC if Reg==7), ylabel(1(1)5)

//reg qualpubadminrating ln_GDP_PC if Reg==7
//twoway (scatter qualpubadminrating ln_GDP_PC if Reg==7)(lfitci qualpubadminrating ln_GDP_PC if Reg==7), ylabel(1(1)5)

//world benchmarking
reg ln_GDP_PC qualpubadminrating if year<2015 & year>=2011
twoway (scatter qualpubadminrating ln_GDP_PC if Reg<7 & year<2015 & year>=2011, mcolor(blue) jitter(5))(scatter qualpubadminrating ln_GDP_PC if Reg==7 & year<2015 & year>=2011, mcolor(red) jitter(5))(lfit qualpubadminrating ln_GDP_PC if year<2015 & year>=2011), ytitle("Quality of Public Administration (CPIA)") ylabel(1(1)5) legend(off) note("Pooled panel data from 2011-2015""SSA ordered pairs are in red") title("GDP per Capita and Quality of Public Administration", size(medlarge)) subtitle("The relationship in SSA is largely consistent with the relationship in the rest of the world.", size(small)) xtitle("Log of GDP per Capita")

/************************/
/**********PEFA**********/
/************************/

//exporting relevant data to excel to put into radar charts
export excel Country PI_13_1 PI_13_2 PI_13_3 PI_14_1 PI_14_2 PI_14_3 PI_15_1 PI_15_2 PI_15_3 democratic oil_gas_dum incomegroup lendingcategory fcv19 if year==2011 & Reg==7 using radar.xlsx, firstrow(varl) replace

/*************************/
/******Afrobarometer******/
/*************************/

/*Trust in the Tax Department*/

//by Country
graph bar trust_tax_dept_fac4 trust_tax_dept_fac3 trust_tax_dept_fac2 trust_tax_dept_fac1 if Reg==7 & trust_tax_dept_fac4!=., over(Country_Code, sort(1) label(labsize(tiny))) percentages title("Trust in the Tax Department by Country") ytitle("Percent of Respondents") caption("Afrobarometer") legend(label(1 "High Trust") label(2 "Somewhat Trust") label(3 "Somewhat Distrust") label(4 "Distrust")) stack

//by oil-rich vs non-oil-rich
gen lowtaxtrust = trust_tax_dept_fac1 + trust_tax_dept_fac2
gen hightaxtrust = trust_tax_dept_fac3 + trust_tax_dept_fac4
ttest hightaxtrust if Reg==7, by(oil_gas_dum) level(95) //significant
graph bar trust_tax_dept_fac4 trust_tax_dept_fac3 trust_tax_dept_fac2 trust_tax_dept_fac1 if Reg==7, over(oiltemp) percentages blabel(bar, pos(center) color(bg) format(%9.1f)) title("Trust in the Tax Department") subtitle("Citizens in Oil-Rich Countries Trust the Tax Department less than Citizens in Non-Oil-Rich Countries", size(small)) ytitle("Percent of Respondents") caption("Afrobarometer""p<0.05") legend(label(1 "High Trust") label(2 "Somewhat Trust") label(3 "Somewhat Distrust") label(4 "Distrust")) stack //scheme(economist)

//by democracy vs non-democracy
ttest trust_tax_dept_fac4 if Reg==7, by(democratic) level(95) //significant
ttest hightaxtrust if Reg==7, by(democratic) level(95) //not significant
graph bar trust_tax_dept_fac4 trust_tax_dept_fac3 trust_tax_dept_fac2 trust_tax_dept_fac1 if Reg==7, over(demotemp) percentages blabel(bar, pos(center) color(bg) format(%9.1f)) title("Trust in the Tax Department") subtitle("Trust in the Tax Department does not Differ between Democracies and Non-Democracies", size(small)) ytitle("Percent of Respondents") caption("Afrobarometer") legend(label(1 "High Trust") label(2 "Somewhat Trust") label(3 "Somewhat Distrust") label(4 "Distrust")) stack //scheme(economist)

//by fcv vs non-fcv
gen conflicttemp = ""
replace conflicttemp = "FCV Countries" if fcv19==1
replace conflicttemp = "Non-FCV Countries" if fcv19==0
ttest hightaxtrust if Reg==7, by(fcv19)
graph bar trust_tax_dept_fac4 trust_tax_dept_fac3 trust_tax_dept_fac2 trust_tax_dept_fac1 if Reg==7, over(conflicttemp) percentages blabel(bar, pos(center) color(bg) format(%9.1f)) title("Trust in the Tax Department") subtitle("Trust in the Tax Department does not Differ between FCV- and Non-FCV Countries", size(small)) ytitle("Percent of Respondents") caption("Afrobarometer") legend(label(1 "High Trust") label(2 "Somewhat Trust") label(3 "Somewhat Distrust") label(4 "Distrust")) stack //scheme(economist)

//by lending category
graph bar trust_tax_dept_fac4 trust_tax_dept_fac3 trust_tax_dept_fac2 trust_tax_dept_fac1 if Reg==7, over(lendingcategory) percentages blabel(bar, pos(center) color(bg) format(%9.1f)) title("Trust in the Tax Department") ytitle("Percent of Respondents") caption("Afrobarometer") legend(label(1 "High Trust") label(2 "Somewhat Trust") label(3 "Somewhat Distrust") label(4 "Distrust") cols(2)) stack scheme(economist)

//by income group
graph bar trust_tax_dept_fac4 trust_tax_dept_fac3 trust_tax_dept_fac2 trust_tax_dept_fac1 if Reg==7, over(incomegroup, label(labsize(small))) percentages blabel(bar, pos(center) color(bg) format(%9.1f)) title("Trust in the Tax Department") ytitle("Percent of Respondents") caption("Afrobarometer") legend(label(1 "High Trust") label(2 "Somewhat Trust") label(3 "Somewhat Distrust") label(4 "Distrust") cols(2)) stack scheme(economist)

/*Corruption of Tax Officials*/

//by Country
graph bar corrupt_tax_offic_fac4 corrupt_tax_offic_fac3 corrupt_tax_offic_fac2 corrupt_tax_offic_fac1 if Reg==7 & trust_tax_dept_fac4!=., over(Country_Code, sort(1) label(labsize(tiny))) percentages title("Share of Tax Officials Who Are Corrupt by Country (Perceptions)", size(medlarge)) subtitle("Question: What share of tax officials are corrupt?") ytitle("Percent of Respondents") caption("Afrobarometer") legend(label(1 "All of Them") label(2 "A Lot of Them") label(3 "Some of Them") label(4 "None of Them")) stack

//by oil-rich vs non-oil-rich
gen highcorruptionperception = corrupt_tax_offic_fac4 + corrupt_tax_offic_fac3
gen lowcorruptionperception = corrupt_tax_offic_fac2 + corrupt_tax_offic_fac1
ttest highcorruption if Reg==7, by(oil_gas_dum) level(95) //significant
graph bar corrupt_tax_offic_fac4 corrupt_tax_offic_fac3 corrupt_tax_offic_fac2 corrupt_tax_offic_fac1 if Reg==7, over(oiltemp) percentages blabel(bar, pos(center) color(bg) format(%9.1f)) title("Share of Tax Officials Who Are Corrupt (Perceptions)", size(large)) subtitle("Oil-Rich Countries Perceive more Tax Official Corruption than Non-Oil-Rich Countries", size(small)) ytitle("Percent of Respondents") caption("Afrobarometer""p<0.05") legend(label(1 "All of Them") label(2 "A Lot of Them") label(3 "Some of Them") label(4 "None of Them")) stack //scheme(economist)

//by democracy vs non-democracy
ttest highcorruption if Reg==7, by(democratic) level(95) //not significant
graph bar corrupt_tax_offic_fac4 corrupt_tax_offic_fac3 corrupt_tax_offic_fac2 corrupt_tax_offic_fac1 if Reg==7, over(demotemp) percentages blabel(bar, pos(center) color(bg) format(%9.1f)) title("Share of Tax Officials Who Are Corrupt (Perceptions)", size(large)) subtitle("Perceptions of Tax Offical Corruption do not Differ between Democracies and Non-Democracies", size(small)) ytitle("Percent of Respondents") caption("Afrobarometer") legend(label(1 "All of Them") label(2 "A Lot of Them") label(3 "Some of Them") label(4 "None of Them")) stack //scheme(economist)

//by fcv vs non-fcv
ttest highcorruption if Reg==7, by(fcv19) level(90) //significant
graph bar corrupt_tax_offic_fac4 corrupt_tax_offic_fac3 corrupt_tax_offic_fac2 corrupt_tax_offic_fac1 if Reg==7, over(conflicttemp) percentages blabel(bar, pos(center) color(bg) format(%9.1f)) title("Share of Tax Officials Who Are Corrupt (Perceptions)", size(large)) subtitle("Perceptions of Tax Official Corruption are Greater in FCV Countries than in Non-FCV Countries", size(small)) ytitle("Percent of Respondents") caption("Afrobarometer""p<0.1") legend(label(1 "All of Them") label(2 "A Lot of Them") label(3 "Some of Them") label(4 "None of Them")) stack //scheme(economist)

//by lending category
graph bar corrupt_tax_offic_fac4 corrupt_tax_offic_fac3 corrupt_tax_offic_fac2 corrupt_tax_offic_fac1 if Reg==7, over(lendingcategory) percentages blabel(bar, pos(center) color(bg) format(%9.1f)) title("Share of Tax Officials Who Are Corrupt (Perceptions)", size(medium)) ytitle("Percent of Respondents") caption("Afrobarometer") legend(label(1 "All of Them") label(2 "A Lot of Them") label(3 "Some of Them") label(4 "None of Them") cols(2) size(small)) stack scheme(economist)

//by income group
graph bar corrupt_tax_offic_fac4 corrupt_tax_offic_fac3 corrupt_tax_offic_fac2 corrupt_tax_offic_fac1 if Reg==7, over(incomegroup, label(labsize(small))) percentages blabel(bar, pos(center) color(bg) format(%9.1f)) title("Share of Tax Officials Who Are Corrupt (Perceptions)", size(medium)) ytitle("Percent of Respondents") caption("Afrobarometer") legend(label(1 "All of Them") label(2 "A Lot of Them") label(3 "Some of Them") label(4 "None of Them") cols(2) size(small)) stack scheme(economist)

/*Views on Size of Government*/

//by Country
graph bar hightax_vs_lowtax_fac1 hightax_vs_lowtax_fac2 hightax_vs_lowtax_fac5 hightax_vs_lowtax_fac3 hightax_vs_lowtax_fac4 if Reg==7 & trust_tax_dept_fac4!=., over(Country_Code, sort(1) label(labsize(tiny))) percentages title("Views on Size of Government") subtitle("Statement: It is better to have higher taxes with more government services than lower taxes with fewer services.", size(vsmall)) ytitle("Percent of Respondents") caption("Afrobarometer") legend(label(1 "Strongly Agree") label(2 "Agree") label(3 "Neither Agree nor Disagree") label(4 "Disagree") label(5 "Strongly Disagree")) stack

//by oil-rich vs non-oil-rich
gen biggovt = hightax_vs_lowtax_fac1 + hightax_vs_lowtax_fac2
gen smallgovt = hightax_vs_lowtax_fac3 + hightax_vs_lowtax_fac4
ttest biggovt if Reg==7, by(oil_gas_dum) level(95) // not significant
graph bar hightax_vs_lowtax_fac1 hightax_vs_lowtax_fac2 hightax_vs_lowtax_fac5 hightax_vs_lowtax_fac3 hightax_vs_lowtax_fac4 if Reg==7, over(oiltemp) percentages blabel(bar, pos(center) color(bg) format(%9.1f)) title("Views on Size of Government") subtitle("Statement: It is better to have higher taxes with more government services than lower taxes with fewer services.", size(vsmall)) ytitle("Percent of Respondents") caption("Afrobarometer""Perceptions do not differ significantly between oil-rich and non-oil-rich countries (p>0.1).", size(small)) legend(label(1 "Strongly Agree") label(2 "Agree") label(3 "Neither Agree nor Disagree") label(4 "Disagree") label(5 "Strongly Disagree")) stack //scheme(economist)

//by democracy vs non-democracy
ttest biggovt if Reg==7, by(democratic) level(95) //not significant
graph bar hightax_vs_lowtax_fac1 hightax_vs_lowtax_fac2 hightax_vs_lowtax_fac5 hightax_vs_lowtax_fac3 hightax_vs_lowtax_fac4 if Reg==7, over(demotemp) percentages blabel(bar, pos(center) color(bg) format(%9.1f)) title("Views on Size of Government") subtitle("Statement: It is better to have higher taxes with more government services than lower taxes with fewer services.", size(vsmall)) ytitle("Percent of Respondents") caption("Afrobarometer""Perceptions do not differ significantly between democracies and non-democracies (p>0.1).", size(small)) legend(label(1 "Strongly Agree") label(2 "Agree") label(3 "Neither Agree nor Disagree") label(4 "Disagree") label(5 "Strongly Disagree")) stack //scheme(economist)

//by fcv vs non-fcv
ttest biggovt if Reg==7, by(fcv19) level(90) //not significant
graph bar hightax_vs_lowtax_fac1 hightax_vs_lowtax_fac2 hightax_vs_lowtax_fac5 hightax_vs_lowtax_fac3 hightax_vs_lowtax_fac4 if Reg==7, over(conflicttemp) percentages blabel(bar, pos(center) color(bg) format(%9.1f)) title("Views on Size of Government") subtitle("Statement: It is better to have higher taxes with more government services than lower taxes with fewer services.", size(vsmall)) ytitle("Percent of Respondents") caption("Afrobarometer""Perceptions do not differ significantly between FCV and non-FCV countries (p>0.1).", size(small)) legend(label(1 "Strongly Agree") label(2 "Agree") label(3 "Neither Agree nor Disagree") label(4 "Disagree") label(5 "Strongly Disagree")) stack //scheme(economist)

//by lending category
graph bar hightax_vs_lowtax_fac1 hightax_vs_lowtax_fac2 hightax_vs_lowtax_fac5 hightax_vs_lowtax_fac3 hightax_vs_lowtax_fac4 if Reg==7, over(lendingcategory) percentages blabel(bar, pos(center) color(bg) format(%9.1f)) title("Views on Size of Government") subtitle("Statement: It is better to have high taxes with many govt. services than low taxes with few services.", size(vsmall)) ytitle("Percent of Respondents") caption("Afrobarometer") legend(label(1 "Strongly Agree") label(2 "Agree") label(3 "Neither Agree nor Disagree") label(4 "Disagree") label(5 "Strongly Disagree") cols(2) size(small)) stack scheme(economist)

//by income group
graph bar hightax_vs_lowtax_fac1 hightax_vs_lowtax_fac2 hightax_vs_lowtax_fac5 hightax_vs_lowtax_fac3 hightax_vs_lowtax_fac4 if Reg==7, over(incomegroup, label(labsize(small))) percentages blabel(bar, pos(center) color(bg) format(%9.1f)) title("Views on Size of Government") subtitle("Statement: It is better to have high taxes with many govt. services than low taxes with few services.", size(vsmall)) ytitle("Percent of Respondents") caption("Afrobarometer") legend(label(1 "Strongly Agree") label(2 "Agree") label(3 "Neither Agree nor Disagree") label(4 "Disagree") label(5 "Strongly Disagree") cols(2) size(small)) stack scheme(economist)

/*Relationships*/

//tax revenue
*views on size of government
gen likesbiggov=.
replace likesbiggov=1 if biggovt>=.5 & biggovt!=.
replace likesbiggov=0 if biggovt<.5
ttest Tax_Revenue if Reg==7, by(likesbiggov) //not significant
bysort Reg likesbiggov: egen avetax=mean(Tax_Revenue)
gen upperbound=.
gen lowerbound=.
ci mean Tax_Revenue if likesbiggov==1 & Reg==7, level(90)
replace lowerbound = r(mean) - r(se) if likesbiggov==1 & Reg==7
replace upperbound = r(mean) + r(se) if likesbiggov==1 & Reg==7
ci mean Tax_Revenue if likesbiggov==0 & Reg==7, level(90)
replace lowerbound = r(mean) - r(se) if likesbiggov==0 & Reg==7
replace upperbound = r(mean) + r(se) if likesbiggov==0 & Reg==7
graph twoway (bar avetax likesbiggov if Reg==7)(rcap lowerbound upperbound likesbiggov), ytitle("Tax Revenue as a % of GDP") title("Size of Govt and Tax Revenue") subtitle("SSA countries that want bigger government do not pay more in taxes than countries that do not want bigger government.", size(vsmall)) note("Afrobarometer""Bars show standard errors.", size(vsmall)) xtitle("") legend(off) xlabel( 0 "Countries that Dislikes Big Gov" 1 "Countries that Like Big Gov") ylabel(6(2)20)

*views on corruption
gen thinksofficialscorrupt=.
replace thinksofficialscorrupt=1 if highcorruptionperception>=.35 & biggovt!=.
replace thinksofficialscorrupt=0 if highcorruptionperception<.35
ttest Tax_Revenue if Reg==7, by(thinksofficialscorrupt) //not significant
drop avetax upperbound lowerbound
bysort Reg thinksofficialscorrupt: egen avetax=mean(Tax_Revenue)
gen upperbound=.
gen lowerbound=.
ci mean Tax_Revenue if thinksofficialscorrupt==1 & Reg==7, level(90)
replace lowerbound = r(mean) - r(se) if thinksofficialscorrupt==1 & Reg==7
replace upperbound = r(mean) + r(se) if thinksofficialscorrupt==1 & Reg==7
ci mean Tax_Revenue if thinksofficialscorrupt==0 & Reg==7, level(90)
replace lowerbound = r(mean) - r(se) if thinksofficialscorrupt==0 & Reg==7
replace upperbound = r(mean) + r(se) if thinksofficialscorrupt==0 & Reg==7
graph twoway (bar avetax thinksofficialscorrupt if Reg==7)(rcap lowerbound upperbound thinksofficialscorrupt), ytitle("Tax Revenue as a % of GDP") title("Perceptions of Corruption and Tax Revenue") subtitle("SSA countries that perceive tax officials to be corrupt do not pay more in taxes than countries that do not perceive tax officials to be corrupt.", size(vsmall)) note("Afrobarometer""Bars show standard errors.", size(vsmall)) xtitle("") legend(off) xlabel( 0 "Countries with Low Corruption" 1 "Countries with High Corruption") ylabel(6(2)20)
