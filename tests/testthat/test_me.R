data<-read.csv("https://dl.dropboxusercontent.com/u/11692532/la05noninvcovs2k15n110data1.csv", header=T)
ana.model<-"
f1=~y1+y2+y3+y4
f2=~y5+y6+y7+y8
"
m0<-cfa(ana.model,data=data,group="group",estimator="mlm",mimic="EQS")
m1<-cfa(ana.model,data=data,group="group",estimator="mlm",mimic="EQS",group.equal=c("loadings"))

#tests that Dnorm returns an error if the chi-square for the specified msmall is greater than that for msmall
test_that("order matters",{
	expect_error(Dnorm(m1,m0))
}
)

#tests that Dnorm returns a warning if chi-squares for specified runs are identical, suggesting erroneous comparison of identical models.
test_that("different models",{
	expect_warning(Dnorm(m0,m0))
})

#tests that supplied arguments are lavaan objects
test_that("m0 is lavaan",{
	expect_error(Dnorm(1,m1))
})

test_that("m1 is lavaan",{
	expect_error(Dnorm(m0,1))
})

#tests that DSB returns an error if the chi-square for the specified msmall is greater than that for msmall
test_that("order matters",{
	expect_error(DSB(m1,m0))
}
)

#tests that DSB returns a warning if chi-squares for specified runs are identical, suggesting erroneous comparison of identical models.
test_that("different models",{
	expect_warning(DSB(m0,m0))
})

#tests that supplied arguments are lavaan objects
test_that("m0 is lavaan",{
	expect_error(DSB(1,m1))
})

test_that("m1 is lavaan",{
	expect_error(DSB(m0,1))
})

#tests that the default value of group behaves properly
test_that("group default",{
	expect_equal(invarianceSeries(ana.model,data),invarianceSeries(ana.model,data,group="group"))
})

