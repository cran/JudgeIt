`nacheck` <-
function (stuff) {
  work <- function(stiff) all(is.na(stiff))
  any(apply(stuff,2,work))
}

