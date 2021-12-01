n = 12345
vec_1 = sample(12, n, replace = TRUE)
vec_1 = sample(12, n, replace = FALSE) 
head(vec_1) 
vec_2 <- c(vec_1==3)
vec_2
head(vec_2)
tail(vec_2)
n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)
length(vec_1)
sum(vec_1==3)
n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))
for (i in 1:10)
{
  print(i)
}
print_number=function(i)
{
  print(paste0("This is loop iteration:", i))
}
print_number(1)
for (n in 1:7)
{
  print(n)
}
print_number=function(n)
{
  print(paste0("This is loop iteration:", n))
}
print_number(n)
n = 17
vec_1 = c(sample(10, n, replace = TRUE))
for (i in 1:n)
{
  print(
    paste('The element of vec_1 at index', i, 'is', vec_1[i]))
}
create_and_print_vec = function(n, min = 1, max = 10)
{n = sample(min:max, 1)
vec_1 = c(sample(min:max, n, replace = TRUE))
for (i in 1:n)
{
  print(
    paste('The element of vec_1 at index', i, 'is', vec_1[i]))
}}
create_and_print_vec(n)
