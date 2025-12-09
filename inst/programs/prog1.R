
is_valid_triangle <- function(a,b,c){
return((a+b>c)& (b+c>a)& (a+c>b))
}
triangle_type <- function(a,b,c){
if (a==b && b==c){
return("Equilateral")
} else if(a==b || b==c || a==c){
return("Isosceles")
} else{
return("Scalene")
}
}
triangle_area <- function(a,b,c){
s <- (a+b+c) /2
area <- sqrt(s*(s-a)*(s-b)*(s-c))
return(area)
}
validate_input <- function(x){
if(!is.numeric(x) || x<=0){
stop("Error: input must be positive")
}
return(TRUE)
}
cat("Enter the lengths of the side of the triangle:\n")
a <- 3
b <- 4
c <- 5
tryCatch({
validate_input(a)
validate_input(b)
validate_input(c)
if(!is_valid_triangle(a,b,c)){
stop("Error: The given sides do not form a valid triangle")
}
type_of_triangle <- triangle_type(a,b,c)
cat("The triangle is:", type_of_triangle, "\n")
area_of_triangle <- triangle_area(a,b,c)
cat("area is:", area_of_triangle,"\n"
)
}, error= function(e){
cat(e$message, "\n")
})
