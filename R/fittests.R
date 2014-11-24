#' Configural Invariance Fit Test
#'
#' This function calls the lavaan cfa function, and runs the test of 
#' configural invariance on a multiple group dataset.
#' 
#' This function runs the first stage of the standard invariance series.
#' 
#' @param model A CFA model specified in \code{lavaan} syntax
#' @param data A data.frame
#' @param group A string identifying the grouping variable in \code{data}
#' @return An object of class \code{lavaan}, for which several methods are available, 
#' including a \code{summary} method.
#' @examples
#' ana.model<-'
#' f1=~ y1 + y2 + y3
#' f2=~ y4 + y5 + y6
#' '
#' m0<-m0run(ana.model,data)
#' summary(m0)
m0run<-function(model,data,group="group"){
	ana.model<-model
	data1<-data
	cfa(ana.model,data=data1,group="group",estimator="mlm",mimic="EQS")
}

#' Weak Invariance Fit Test
#'
#' This function calls the lavaan cfa function, and runs the test of 
#' weak invariance on a multiple group dataset.
#' 
#' This function runs the second stage of the standard invariance series.
#' 
#' @param model A CFA model specified in \code{lavaan} syntax
#' @param data A data.frame
#' @param group A string identifying the grouping variable in \code{data}
#' @return An object of class \code{lavaan}, for which several methods are available, 
#' including a \code{summary} method.
#' @examples
#' ana.model<-'
#' f1=~ y1 + y2 + y3
#' f2=~ y4 + y5 + y6
#' '
#' m1<-m1run(ana.model,data)
#' summary(m1)
m1run<-function(model,data,group="group"){
	ana.model<-model
	data1<-data
	cfa(ana.model,data=data1,group="group",estimator="mlm",mimic="EQS",group.equal=c("loadings"))
}


#' Strong Invariance Fit Test
#'
#' This function calls the lavaan cfa function, and runs the test of 
#' strong invariance on a multiple group dataset.
#' 
#' This function runs the third stage of the standard invariance series.
#' 
#' @param model A CFA model specified in \code{lavaan} syntax
#' @param data A data.frame
#' @param group A string identifying the grouping variable in \code{data}
#' @return An object of class \code{lavaan}, for which several methods are available, 
#' including a \code{summary} method.
#' @examples
#' ana.model<-'
#' f1=~ y1 + y2 + y3
#' f2=~ y4 + y5 + y6
#' '
#' m2<-m2run(ana.model,data)
#' summary(m2)
m2run<-function(model,data,group="group"){
	ana.model<-model
	data1<-data
	cfa(ana.model,data=data1,group="group",estimator="mlm",mimic="EQS",group.equal=c("loadings","intercepts"))
}

#' Strict Invariance Fit Test
#'
#' This function calls the lavaan cfa function, and runs the test of 
#' strict invariance on a multiple group dataset.
#' 
#' This function runs the fourth stage of the standard invariance series.
#' 
#' @param model A CFA model specified in \code{lavaan} syntax
#' @param data A data.frame
#' @param group A string identifying the grouping variable in \code{data}
#' @return An object of class \code{lavaan}, for which several methods are available, 
#' including a \code{summary} method.
#' @examples
#' ana.model<-'
#' f1=~ y1 + y2 + y3
#' f2=~ y4 + y5 + y6
#' '
#' m3<-m3run(ana.model,data)
#' summary(m3)
m3run<-function(model,data,group="group"){
	ana.model<-model
	data1<-data
	cfa(ana.model,data=data1,group="group",estimator="mlm",mimic="EQS",group.equal=c("loadings","intercepts","residuals"))
}

#' Beyond Strict Invariance Fit Test
#'
#' This function calls the lavaan cfa function, and runs the test of 
#' beyond strict invariance on a multiple group dataset.
#' 
#' This function runs the fifth stage of the standard invariance series.
#' 
#' @param model A CFA model specified in \code{lavaan} syntax
#' @param data A data.frame
#' @param group A string identifying the grouping variable in \code{data}
#' @return An object of class \code{lavaan}, for which several methods are available, 
#' including a \code{summary} method.
#' @examples
#' ana.model<-'
#' f1=~ y1 + y2 + y3
#' f2=~ y4 + y5 + y6
#' '
#' m4<-m4run(ana.model,data)
#' summary(m4)
m4run<-function(model,data,group="group"){
	ana.model<-model
	data1<-data
	cfa(ana.model,data=data1,group="group",estimator="mlm",mimic="EQS",group.equal=c("loadings","intercepts","residuals","lv.variances","lv.covariances"))
}

#' Full Mean and Covariance Structure Invariance Fit Test
#'
#' This function calls the lavaan cfa function, and runs the test of 
#' full mean and covariance structure invariance on a multiple group dataset.
#' 
#' This function runs the sixth stage of the standard invariance series.
#' 
#' @param model A CFA model specified in \code{lavaan} syntax
#' @param data A data.frame
#' @param group A string identifying the grouping variable in \code{data}
#' @return An object of class \code{lavaan}, for which several methods are available, 
#' including a \code{summary} method.
#' @examples
#' ana.model<-'
#' f1=~ y1 + y2 + y3
#' f2=~ y4 + y5 + y6
#' '
#' m5<-m5run(ana.model,data)
#' summary(m5)
m5run<-function(model,data,group="group"){
	ana.model<-model
	data1<-data
	cfa(ana.model,data=data1,group="group",estimator="mlm",mimic="EQS",group.equal=c("loadings","intercepts","residuals","lv.variances","lv.covariances","means"))
}