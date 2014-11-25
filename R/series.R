#' Run The Standard Invariance Series
#'
#' This function runs the series of difference tests typically run when evaluating measurement invariance for a psychometric instrument.
#' 
#' @param model A CFA model specified in \code{lavaan} syntax
#' @param data A data.frame
#' @param group A string identifying the grouping variable in \code{data}#' @return A data.frame containing the scaled and unscaled chi-square difference, degrees of freedom associated with the difference test, and the pvalues associated with the scaled and unscaled tests.
#' @export
#' @examples
#' ana.model<-'
#' f1=~ y1 + y2 + y3
#' f2=~ y4 + y5 + y6
#' '
#' invarianceSeries(ana.model,data)
invarianceSeries<-function(model,data,group="group"){
	#validity checks aren't really necessary here
	#theyre all in lavaan or the difference test functions
	
	#initial tests of fit
	m0<-m0run(model,data,group)
	m1<-m1run(model,data,group)
	m2<-m2run(model,data,group)
	m3<-m3run(model,data,group)
	m4<-m4run(model,data,group)
	m5<-m5run(model,data,group)
	
	#difference tests
	diff01<-DSB(m0,m1)
	diff12<-DSB(m1,m2)
	diff23<-DSB(m2,m3)
	diff34<-DSB(m3,m4)
	diff45<-DSB(m4,m5)
	
	#creates output
	output<-rbind(diff01,diff12,diff23,diff34,diff45)
	rownames(output)<-c("Weak","Strong","Strict","Beyond","Full")
	return(output)
}