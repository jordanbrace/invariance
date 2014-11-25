#' Chi-Square Difference Test
#'
#' This function calculates the chi-square difference of two nested CFA models,
#' and tests its significance.
#' 
#' @param msmall A \code{lavaan} object
#' @param mbig A \code{lavaan} object containing a model run nested within \code{msmall}. \code{mbig} is the model run with the larger chi-square.
#' @return A data.frame containing the chi-square difference, degrees of freedom associated with the difference test, and a pvalue.
#' @export
#' @examples
#' ana.model<-'
#' f1=~ y1 + y2 + y3
#' f2=~ y4 + y5 + y6
#' '
#' m0<-cfa(ana.model,data=data1,group="group")
#' m1<-cfa(ana.model,data=data1,group="group",group.equal=c("loadings"))
#' Dnorm(m0,m1)
Dnorm<-function(msmall,mbig){
	#extracts necessary values from lavaan models
	chibig<-inspect(mbig,"fit.indices")["chisq"]
	chismall<-inspect(msmall,"fit.indices")["chisq"]
	
	#validity check
	assert_that(chibig>=chismall)
	
	if(chismall==chibig){
		warning("Warning: Identical Models Being Compared")
	}
	
	assert_that(class(msmall)=="lavaan")
	
	assert_that(class(mbig)=="lavaan")
	
	#additional values extracted from lavaan object
	dfbig<-inspect(mbig,"fit.indices")["df"]
	dfsmall<-inspect(msmall,"fit.indices")["df"]
	
	#computes differences
	chidiff<-chibig-chismall
	dfdiff<-dfbig-dfsmall
	
	#creates output
	output<-as.data.frame(cbind(chidiff,dfdiff,1-pchisq(chidiff,dfdiff)))
	colnames(output)<-c("chidiff","df","pvalue")
	rownames(output)<-NULL
	return(output)
}

#' Scaled Chi-Square Difference Test
#'
#' This function calculates the Satorra-Bentler scaled chi-square difference of two nested CFA models,
#' and tests its significance. This difference test is robust to non-normality of data.
#' 
#' @param msmall A \code{lavaan} object
#' @param mbig A \code{lavaan} object containing a model run nested within \code{msmall}. \code{mbig} is the model run with the larger chi-square.
#' @return A data.frame containing the scaled and unscaled chi-square difference, degrees of freedom associated with the difference test, and the pvalues associated with the scaled and unscaled tests.
#' @export
#' @examples
#' ana.model<-'
#' f1=~ y1 + y2 + y3
#' f2=~ y4 + y5 + y6
#' '
#' m0<-cfa(ana.model,data=data1,group="group")
#' m1<-cfa(ana.model,data=data1,group="group",group.equal=c("loadings"))
#' DSB(m0,m1)
DSB<-function(msmall,mbig){
	#extracts necessary values from lavaan models
	chibig<-inspect(mbig,"fit.indices")["chisq"]
	chismall<-inspect(msmall,"fit.indices")["chisq"]
	
	#validity check
	assert_that(chibig>=chismall)
	
	if(chismall==chibig){
		warning("Warning: Identical Models Being Compared")
	}
	
	assert_that(class(msmall)=="lavaan")
	
	assert_that(class(mbig)=="lavaan")
	
	#additional values extracted from lavaan output
	SBbig<-inspect(mbig,"fit.indices")["chisq.scaled"]
	dfbig<-inspect(mbig,"fit.indices")["df"]
	SBsmall<-inspect(msmall,"fit.indices")["chisq.scaled"]
	dfsmall<-inspect(msmall,"fit.indices")["df"]
	
	#additional computations necessary to calculate DSB
	cbig<-chibig/SBbig
	csmall<-chismall/SBsmall
	cd<-(cbig*dfbig-csmall*dfsmall)/(dfbig-dfsmall)
	chidiff<-chibig-chismall
	dfdiff<-dfbig-dfsmall
	Dscaled<-chidiff/cd
	
	#creates output
	output<-as.data.frame(cbind(chidiff, 1-pchisq(chidiff,dfdiff), dfdiff, Dscaled, 1-pchisq(Dscaled,dfdiff)))
	colnames(output)<-c("chidiff","pvalue","df","chidiff.scaled","pvalue.scaled")
	rownames(output)<-NULL
	return(output)
}
