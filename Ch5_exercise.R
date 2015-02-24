require(boot)

load("/home/zampeta/Documents/Courses/Statistical_Learning/5.R.RData")
fit = glm(y~X1+X2,data=Xy)
summary(fit)


matplot(Xy,type="l")
legend()
Xy[1,]
boot(Xy,function(Xy,idx){coef(glm(y~X1+X2,data=Xy[idx,]))[[2]]},1000)

new.rows = c(101:200, 401:500, 101:200, 901:1000, 301:400, 1:100, 1:100, 801:900, 201:300, 701:800)

new.rows = rep(sample(0:9,10,replace=T) * 100,each= 100) + 1:100

new.Xy = Xy[new.rows, ]


boot(Xy
     ,function(tptp,idx){
       idx <- rep(sample(0:9,10,replace=T) * 100,each= 100) + 1:100
       
       coef(glm(y~X1+X2,data=tptp[idx,]))[[2]]
       }
     
     
     ,R=10000)






