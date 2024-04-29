cd "/Users/justingeorge/Dropbox/Research/Defence Economics/NATO_JIBE_special_issue_2022/replication files"
use nato_me_data,clear

encode ccode, gen (ccode_new)

tsset ccode_new year
xi i.ccode_new

eststo clear
eststo:ivreg2 logme (me_sl_nato= gdp_sl_nato pop_sl_nato)loggdp logpop terror_attacks russia_trade i.ccode_new if nato==1,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)
eststo:ivreg2 logme (me_sl_cont= gdp_sl_cont pop_sl_cont)loggdp logpop terror_attacks russia_trade i.ccode_new i.year if nato==1,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)
eststo:ivreg2 logme (me_sl_dist= gdp_sl_dist pop_sl_dist)loggdp logpop terror_attacks russia_trade i.ccode_new i.year if nato==1,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)
eststo:ivreg2 logme (me_sl_lagtrade= gdp_sl_lagtrade pop_sl_lagtrade)loggdp logpop terror_attacks russia_trade i.ccode_new i.year if nato==1,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)
eststo:ivreg2 logme (me_sl_us= gdp_sl_us pop_sl_us)loggdp logpop terror_attacks russia_trade i.ccode_new i.year if nato==1,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)
eststo:ivreg2 logme (me_sl_rus= gdp_sl_rus pop_sl_rus)loggdp logpop terror_attacks i.ccode_new i.year if nato==1,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)

esttab using table_2.csv,compress starlevels(* 0.10 ** 0.05 *** 0.01) stats(r2 F idstat jp N)  replace


eststo clear
eststo:ivreg2 logme (me_sl_nato= gdp_sl_nato pop_sl_nato)loggdp logpop terror_attacks russia_trade i.ccode_new if nato==1&year>2001,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)
eststo:ivreg2 logme (me_sl_cont= gdp_sl_cont pop_sl_cont)loggdp logpop terror_attacks russia_trade i.ccode_new i.year if nato==1&year>2001,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)
eststo:ivreg2 logme (me_sl_dist= gdp_sl_dist pop_sl_dist)loggdp logpop terror_attacks russia_trade i.ccode_new i.year if nato==1&year>2001,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)
eststo:ivreg2 logme (me_sl_lagtrade= gdp_sl_lagtrade pop_sl_lagtrade)loggdp logpop terror_attacks russia_trade i.ccode_new i.year if nato==1&year>2001,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)
eststo:ivreg2 logme (me_sl_us= gdp_sl_us pop_sl_us)loggdp logpop terror_attacks russia_trade i.ccode_new i.year if nato==1&year>2001,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)
eststo:ivreg2 logme (me_sl_rus= gdp_sl_rus pop_sl_rus)loggdp logpop terror_attacks i.ccode_new i.year if nato==1&year>2001,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)
esttab using table_3.csv,compress starlevels(* 0.10 ** 0.05 *** 0.01) stats(r2 F idstat jp N)  replace


eststo clear
eststo:ivreg2 logme (me_sl_nato= gdp_sl_nato pop_sl_nato)loggdp logpop terror_attacks russia_trade i.ccode_new if new_nato==1,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)
eststo:ivreg2 logme (me_sl_cont= gdp_sl_cont pop_sl_cont)loggdp logpop terror_attacks russia_trade i.ccode_new i.year if new_nato==1,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)
eststo:ivreg2 logme (me_sl_dist= gdp_sl_dist pop_sl_dist)loggdp logpop terror_attacks russia_trade i.ccode_new i.year if new_nato==1,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)
eststo:ivreg2 logme (me_sl_lagtrade= gdp_sl_lagtrade pop_sl_lagtrade)loggdp logpop terror_attacks russia_trade i.ccode_new i.year if new_nato==1,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)
eststo:ivreg2 logme (me_sl_us= gdp_sl_us pop_sl_us)loggdp logpop terror_attacks russia_trade i.ccode_new i.year if new_nato==1,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)
eststo:ivreg2 logme (me_sl_rus= gdp_sl_rus pop_sl_rus)loggdp logpop terror_attacks i.ccode_new i.year if new_nato==1,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)

esttab using table_4.csv,compress starlevels(* 0.10 ** 0.05 *** 0.01) stats(r2 F idstat jp N)  replace

eststo clear
eststo:ivreg2 logme (me_sl_nato post_14_nato= gdp_sl_nato pop_sl_nato) loggdp logpop terror_attacks russia_trade i.ccode_new if new_nato==1,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)
eststo:ivreg2 logme (me_sl_cont post_14_cont= gdp_sl_cont pop_sl_cont)loggdp logpop terror_attacks russia_trade i.ccode_new i.year if new_nato==1,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)
eststo:ivreg2 logme (me_sl_dist post_14_dist = gdp_sl_dist pop_sl_dist) loggdp logpop terror_attacks russia_trade i.ccode_new i.year if new_nato==1,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)
eststo:ivreg2 logme (me_sl_lagtrade post_14_lagtrade= gdp_sl_lagtrade pop_sl_lagtrade) loggdp logpop terror_attacks russia_trade i.ccode_new i.year if new_nato==1,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)
eststo:ivreg2 logme (me_sl_us post_14_us= gdp_sl_us pop_sl_us) loggdp logpop terror_attacks russia_trade i.ccode_new i.year if new_nato==1,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)
eststo:ivreg2 logme (me_sl_rus post_14_rus= gdp_sl_rus pop_sl_rus) loggdp logpop terror_attacks i.ccode_new i.year if new_nato==1,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)

esttab using table_5.csv,compress starlevels(* 0.10 ** 0.05 *** 0.01) stats(r2 F idstat jp N)  replace

eststo clear
eststo:ivreg2 logme (me_sl_nato= gdp_sl_nato pop_sl_nato) loggdp logpop terror_attacks logme_russia i.ccode_new if nato==1,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)
eststo:ivreg2 logme (me_sl_cont= gdp_sl_cont pop_sl_cont)loggdp logpop terror_attacks logme_russia i.ccode_new i.year if nato==1,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)
eststo:ivreg2 logme (me_sl_dist = gdp_sl_dist pop_sl_dist) loggdp logpop terror_attacks logme_russia i.ccode_new i.year if nato==1,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)
eststo:ivreg2 logme (me_sl_lagtrade= gdp_sl_lagtrade pop_sl_lagtrade) loggdp logpop terror_attacks logme_russia i.ccode_new i.year if nato==1,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)
eststo:ivreg2 logme (me_sl_us= gdp_sl_us pop_sl_us) loggdp logpop terror_attacks logme_russia i.ccode_new i.year if nato==1,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)
eststo:ivreg2 logme (me_sl_rus= gdp_sl_rus pop_sl_rus) loggdp logpop terror_attacks i.ccode_new i.year if nato==1,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)
eststo:ivreg2 logme (me_sl_rusnew= gdp_sl_rusnew pop_sl_rusnew) loggdp logpop terror_attacks i.ccode_new i.year if nato==1,  gmm2s bw(3) kernel(bartlett) robust partial(i.ccode_new)

esttab using table_6.csv,compress starlevels(* 0.10 ** 0.05 *** 0.01) stats(r2 F idstat jp N)  replace
