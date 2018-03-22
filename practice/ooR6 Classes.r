# R6 Classes
library(R6)
VectorStack <- R6Class("VectorStack",
private = list(elements = NULL),
public = list(
top = function() {
private$elements[1]
},
pop = function() {
private$elements <-
private$elements[-1]
invisible(self)
},
push = function(e) {
private$elements <-
c(e, private$elements)
invisible(self)
},
is_empty = function() {
length(private$elements) == 0
}
))

VectorStack <- R6Class("VectorStack",
private = list(elements = NULL),
public = list(
# ... rest of the methods
print = function() {
cat("Stack elements:\n")
print(private$elements)
}
))
(stack <- VectorStack$new())

(stack <- VectorStack$new())
stack$push(1)$push(2)$push(3)
stack
while (!stack$is_empty()) stack$pop()
stack <- VectorStack$new(elements = 1:4)

VectorStack <- R6Class("VectorStack",
private = list(elements = NULL),
public = list(
initialize = function(elements = NULL) {
private$elements <- elements
},
# ... rest of the methods
))

(stack <- VectorStack$new(elements = 1:4))
stack$elements
list()$elements
A <- R6Class("A", public = list(x = 5), private = list(y = 13))

a <- A$new()
a$x
a$y
a$y <- 12
a$y
a$z <- "foo"


VectorStack <- R6Class("VectorStack",
private = list(elements_ = NULL),
public = list(
# ... methods
),
active = list(
elements = function(value) {
if (!missing(value))
stop("elements are read-only")
private$elements_
}
))

stack <- VectorStack$new(elements = 1:4)
stack$elements
stack$elements <- rev(1:3)

A <- R6Class("A",
public = list(
f = function() print("A::f"),
g = function() print("A::g"),
h = function() print("A::h")
))
B <- R6Class("B", inherit = A,
public = list(
g = function() print("B::g"),
h = function() print("B::h")
))
C <- R6Class("C", inherit = B,
public = list(
h = function() print("C::h")
))

x <- A$new()
y <- B$new()
z <- C$new()

x$f()
y$f()
x$g()
y$g()
x$h()
y$h()

A <- R6Class("A", public = list(x = 1:5))
B <- R6Class("B",
public = list(
x = 1:5,
a = A$new()
))

x <- B$new()
y <- B$new()
x$x
y$x

x$x <- 1:3
x$x
y$x

x$a$x <- 1:3
x$a$x

z <- B$new()
z$a$x

w <- x
B <- R6Class("B",
public = list(
x = 1:5,
a = NULL,
initialize = function() {
self$a <- A$new()
}))

x <- B$new()
y <- B$new()
x$a$x

z <- x$clone()
z$x

modulus <- R6Class("modulus",
private = list(
value_ = c(),
n_ = c()
),
public = list(
initialize = function(value, n) {
private$value_ <- value
private$n_ <- n
},
print = function() {
cat("Modulus", private$n_, "values:\n")print(private$value_)
}
),
active = list(
value = function(value) {
if (missing(value)) private$value_
else private$value_ <- value %% private$n_
},
n = function(value) {
if (!missing(value)) stop("Cannot change n")private$n_
}
))

(x <- modulus$new(value = 1:6, n = 3))
class(x)

Ops.modulus <- function(e1, e2) {
nx <- ny <- NULL
if (inherits(e1, "modulus")) nx <- e1$n
if (inherits(e2, "modulus")) ny <- e2$n
if (!is.null(nx) && !is.null(ny) && nx != ny)
stop("Incompatible types")
n <- ifelse(!is.null(nx), nx, ny)
v1 <- e1
v2 <- e2
if (inherits(e1, "modulus")) v1 <- e1$value
if (inherits(e2, "modulus")) v2 <- e2$value
e1 <- v1 ; e2 <- v2
result <- NextMethod() %% n
modulus$new(result, n)
}

x + 1:6
2 * x

modulus2 <- R6Class("modulus2", inherit = modulus)
y <- modulus2$new(value = 1:2, n = 3)

class(y)
x + y



