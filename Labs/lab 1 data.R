my_vec=1:3
mat_1=matrix(my_vec)
mat_2=matrix(my_vec,nrow=2,ncol=3,byrow=TRUE)
mat_3=matrix(my_vec,nrow=3,ncol=2,byrow=TRUE)
mat_4=matrix(my_vec,nrow=7,ncol=5,byrow=TRUE)
my_list_1=list("two"= 5.2,"one"="five point two","three"= c(0:5))
my_list_1[[1]]
my_list_1[[as.numeric("1")]]
my_list_1[["1"]]
my_list_1[["one"]]
my_list_1$one
my_list_1$"one"
my_list_1$1
my_list_1$"1"