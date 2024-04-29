**********************************
** NATO Security Burden Sharing **
**   Wukki Kim and Todd Sandler **
**   Defence and Peace Economics *
**     STATA version 16.0       **
**********************************

use replication_data.dta,clear

tsset ccode year

xi i.ccode

global x1 loggdppc logpop lognonstate_conflict _Iccode* 
global x2 logborder trans_terror 

global iv_nato gdppc_sl_nato  pop_sl_nato 
global iv_dis gdppc_sl_dis pop_sl_dis 
global iv_con gdppc_sl_con pop_sl_con 

***  Table 1 ***
summarize logss loggdppc logpop lognonstate_conflict logborder trans_terror logme_russia


***  Table 2 ***
eststo clear

eststo n1:ivreg2 logss (ss_sl_nato = $iv_nato ) $x1,  gmm2s robust  partial(_Iccode*)

eststo n2:ivreg2 logss (ss_sl_con = $iv_con ) $x1 , gmm2s robust  partial(_Iccode*)

eststo n3:ivreg2 logss (ss_sl_dis = $iv_dis ) $x1  ,  gmm2s   robust partial(_Iccode*)

estout *, cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) stats(F N, fmt(%9.0g %9.3f))  varwidth(20) modelwidth(10)

***  Table 3 ***
eststo clear

eststo n4:ivreg2 logss (ss_sl_nato = $iv_nato) $x1 $x2 ,  gmm2s  robust  partial(_Iccode*)

eststo n5:ivreg2 logss (ss_sl_con = $iv_con ) $x1 $x2 , small  gmm2s  robust  partial(_Iccode*)

eststo n6:ivreg2 logss (ss_sl_dis = $iv_dis ) $x1 $x2 , gmm2s robust  partial(_Iccode*)

estout *, cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) stats(F N, fmt(%9.0g %9.3f))  varwidth(20) modelwidth(10) 

***  Table A1 ***
eststo clear

eststo a1:ivreg2 logss (ss_sl_nato = $iv_nato) $x1 $x2 logme_russia ,  gmm2s  robust partial(_Iccode*)

eststo a2:ivreg2 logss (ss_sl_con = $iv_con) $x1 $x2 logme_russia ,  gmm2s  robust partial(_Iccode*)

eststo a3:ivreg2 logss (ss_sl_dis = $iv_dis) $x1 $x2 logme_russia ,  gmm2s  robust partial(_Iccode*)

estout *, cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) stats(F N, fmt(%9.0g %9.3f))  varwidth(20) modelwidth(10)

***  Table A2 ***
global x11 loggdppc logpop lognonstate_conflict 

eststo clear

eststo a1:ivreg2 logss (ss_sl_nato = $iv_nato) $x11 $x2 inv_dis_russia ,  gmm2s  robust

eststo a2:ivreg2 logss (ss_sl_con = $iv_con) $x11 $x2 inv_dis_russia ,  gmm2s  robust

eststo a3:ivreg2 logss (ss_sl_dis = $iv_dis) $x11 $x2 inv_dis_russia ,  gmm2s  robust

estout *, cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) stats(F N, fmt(%9.0g %9.3f))  varwidth(20) modelwidth(10) 

***  Table A3 ***
eststo clear

eststo a4:ivreg2 logss (ss_sl_nato = $iv_nato) $x11 $x2 contiguity_russia ,  gmm2s  robust

eststo a5:ivreg2 logss (ss_sl_con = $iv_con) $x11 $x2 contiguity_russia ,  gmm2s  robust

eststo a6:ivreg2 logss (ss_sl_dis = $iv_dis) $x11 $x2 contiguity_russia ,  gmm2s  robust

estout *, cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) stats(F N, fmt(%9.0g %9.3f))  varwidth(20) modelwidth(10) 
