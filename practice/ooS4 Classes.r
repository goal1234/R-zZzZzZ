# S4 Classes
library(methods)
Stack <- setClass("Stack")

VectorStack <- setClass("VectorStack",
                        slots = c(
                        elements = "vector"
                        ),
                        contains = "Stack")

(vs <- VectorStack())
(vs <- VectorStack(elements = 1:4))
vs@elements
new("VectorStack", elements = 1:4)

# Generic Functions
setGeneric("top",
    def = function(stack) standardGeneric("top"))
setGeneric("pop",
    def = function(stack) standardGeneric("pop"))
setGeneric("push",
    def = function(stack, element) standardGeneric("push"))setGeneric("is_empty",
    def = function(stack) standardGeneric("is_empty"))

setMethod("top", signature = "VectorStack",
  definition = function(stack) stack@elements[1])setMethod("pop", signature = "VectorStack",
    definition = function(stack) {
    VectorStack(elements = stack@elements[-1])
})
setMethod("push", signature = "VectorStack",
    definition = function(stack, element) {
    VectorStack(elements = c(element, stack@elements))
})
setMethod("is_empty", signature = "VectorStack",
definition = function(stack) length(stack@elements) == 0)

stack <- VectorStack()
stack <- push(stack, 1)
stack <- push(stack, 2)
stack <- push(stack, 3)
stack


while (!is_empty(stack)) {
stack <- pop(stack)
}
stack


NaturalNumber <- setClass("NaturalNumber",
                            slots = c(
                            n = "integer"
                            ))
(n <- NaturalNumber())

NaturalNumber <- setClass("NaturalNumber",
                            1lots = c(
                            n = "integer"
                            ),
                            prototype = list(
                            n = as.integer(0)
                            ))

(n <- NaturalNumber())
(n <- NaturalNumber(n = as.integer(1)))
n@n <- as.integer(-1)
NaturalNumber <- setClass("NaturalNumber",
                            slots = c(
                            n = "integer"
                            ),
                            prototype = list(
                            n = as.integer(0)
                            ),
                            validity = function(object) {
                            object@n >= 0
                            })
n <- NaturalNumber(n = as.integer(-1))
n@n <- as.integer(-1)
validObject(n)

A <- setClass("A", contains = "NULL")
B <- setClass("B", contains = "A")
C <- setClass("C", contains = "B")

x <- A()
y <- B()
z <- C()

setGeneric("f", def = function(x) standardGeneric("f"))
setMethod("f", signature = "A",
                definition = function(x) print("A::f"))
f(x)

setGeneric("g", def = function(x) standardGeneric("g"))
setMethod("g", signature = "A",
definition = function(x) print("A::g"))
setMethod("g", signature = "B",
definition = function(x) print("B::g"))

setGeneric("h", def = function(x) standardGeneric("h"))
setMethod("h", signature = "A",
definition = function(x) print("A::h"))
setMethod("h", signature = "B",
definition = function(x) print("B::h"))

setMethod("h", signature = "C",
definition = function(x) print("C::h"))
h(x)

setMethod("h", signature = "A",
definition = function(x) {
print("A::h")
})
setMethod("h", signature = "B",
definition = function(x) {
print("B::h")
callNextMethod()
})
setMethod("h", signature = "C",
definition = function(x) {
print("C::h")
callNextMethod()
})

h(x);h(y);h(z)

d <- function(x) print("default::d")
setGeneric("d")
d(x);d(y);d(z)

setMethod("d", signature = "A",
definition = function(x) {
print("A::d")
callNextMethod()
})
setMethod("d", signature = "B",
definition = function(x) {
print("B::d")
callNextMethod()
})
setMethod("d", signature = "C",
definition = function(x) {
print("C::d")
callNextMethod()
})
d(x);d(y);d(z)

ListStack <- setClass("ListStack", contains = "Stack")

requireMethods(functions = c("top", "pop", "push", "is_empty"),
signature = "Stack")
pop(stack)


# Constructors
A <- setClass("A", slots = list(x = "numeric", y = "numeric"))
B <- setClass("B", contains = "A", slots = list(z = "numeric"))
setMethod("initialize", signature = "A",
definition = function(.Object, x, y) {
print("A initialize")
.Object@x <- x
.Object@y <- y
.Object
})

setMethod("initialize", signature = "B",
definition = function(.Object, z) {
.Object <- callNextMethod(.Object, x = z, y = z)
.Object@z <- z
.Object
})

(a <- A(x = 1:3, y = 4:6))

# Dispatching on Type-Signatures

setGeneric("f", def = function(x, y) standardGeneric("f"))
setMethod("f", signature = c("numeric", "numeric"),
definition = function(x, y) x + y)
setMethod("f", signature = c("logical", "logical"),
definition = function(x, y) x & y)
f(TRUE, FALSE)
f(2,3)

setMethod("f", signature = c("integer", "complex"),
definition = function(x, y) x - y)
f(2,2)

f(as.integer(2), 2)

setMethod("f", signature = c("integer", "numeric"),
definition = function(x, y) 2*x + y)
f(as.integer(2), 2)
setMethod("f", signature = "character",
definition = function(x, y) x)
f("foo", "bar")

setGeneric("g", def = function(x, y, z) standardGeneric("g"))setMethod("g", signature = "character",
definition = function(x, y, z) "g(character)")
setMethod("g", signature = c("numeric", "character"),
definition = function(x, y, z) "g(numeric, character)")

g("foo", NA, NA)
g(12, "bar", NA)

setMethod("f", signature = "ANY",
definition = function(x, y) "any")
f(list(), NULL)

setMethod("f", signature = c("ANY", "missing"),
definition = function(x, y) "missing")
f(list(), NULL)
f(list())

modulus <- setClass("modulus",
slots = c(
value = "numeric",
n = "numeric"
))
setMethod("show", signature = "modulus",
definition = function(object) {
cat("Modulus", object@n, "values:\n")
print(object@value)
})
(x <- modulus(value = 1:6, n = 3))


setMethod("+", signature = c("modulus", "modulus"),
definition = function(e1, e2) {
if (e1@n != e2@n) stop("Incompatible modulus")
modulus(value = e1@value + e2@value,
n = e1@n)
})
setMethod("+", signature = c("modulus", "numeric"),
definition = function(e1, e2) {
modulus(value = e1@value + e2,
n = e1@n)
})
setMethod("+", signature = c("numeric", "modulus"),
definition = function(e1, e2) {
modulus(value = e1 + e2@value,
n = e2@n)
})

x + 1:6

y <- modulus(value = 1:6, n = 2)

setMethod("Arith",
signature = c("modulus", "modulus"),
definition = function(e1, e2) {
if (e1@n != e2@n) stop("Incompatible modulus")modulus(value = callGeneric(e1@value, e2@value),
n = e1@n)
})
setMethod("Arith",
signature = c("modulus", "numeric"),
definition = function(e1, e2) {
modulus(value = callGeneric(e1@value, e2),
n = e1@n)
})
setMethod("Arith",
signature = c("numeric", "modulus"),
definition = function(e1, e2) {
modulus(value = callGeneric(e1, e2@value),
n = e2@n)
})

x * y
2 * x


X <- function(x) {
structure(list(x = x), class = "X")
}
foo <- function(x) UseMethod("foo")
bar <- function(x) UseMethod("bar")
foo.X <- function(x) "foo"
bar.X <- function(x) x$x

x <- X(5)
foo(x)
bar(x)
setOldClass("X")
Y <- setClass("Y", contains = "X")
y <- Y()
foo(y)

foo.Y <- function(x) "Y::foo"
foo(y)

bar(y)

Y <- setClass("Y", contains = "X", slots = c(x = "ANY"))
setMethod("bar", signature = "Y",
definition = function(x) x@x)

y <- Y(x = 13)
bar(y)












