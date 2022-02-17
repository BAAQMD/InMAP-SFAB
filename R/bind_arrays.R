#' bind_arrays
#'
#' Like [abind::abind()], but preserves `dimnames`
#'
#' @param x (list) arrays to bind
#' @param along (integer) dimension along which to bind (default: last)
#'
#' @export
bind_arrays <- function (x, along = NULL) {

  # Use the first array as a prototype
  proto <- x[[1]]

  # The dimnames of the prototype
  proto_dn <- dimnames(proto)

  # If `along` is NULL, then default to the last dimension
  if (is.null(along)) {
    along <- length(dim(proto))
  } else if (is.character(along)) {
    along <- which(names(proto_dn) == along)
  }
  stopifnot(length(along) == 1)

  # Every dimension except `along`
  dn_asis <- proto_dn[-along]

  # The new `along` dimension
  along_chr <- unname(map_chr(x, function (x) dimnames(x)[[along]]))
  along_dn <- names(proto_dn[along])
  dn_new <- set_names(list(along_chr), along_dn)

  # Make sure the sizes are identical, save for the `along` dimension,
  # which can be larger in some than in others
  ranks <- map(x, function (x) dim(x)[-along])
  stopifnot(all(unlist(map(ranks, ~ . == ranks[[1]]))))

  # Do the binding
  combined <- abind::abind(x, along = along)

  # Apply the combined dimnames
  dimnames(combined) <- append(dn_asis, dn_new)

  return(combined)

}
