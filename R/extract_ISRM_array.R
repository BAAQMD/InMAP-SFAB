#' extract_ISRM_array
#'
#' Extract an arbitrary slab of source-receptor deltas for one layer and one variable.
#'
#' @param ncdf4_obj (ncdf4) created with [ncdf4::nc_open()]
#' @param source (integer) source cell IDs
#' @param receptor (integer) receptor cell IDs
#' @param varid (character) as in [ncdf4::ncvar_get()]
#' @param layer (integer) layer ID
#'
#' @importFrom ncdf4 ncvar_get
#'
#' @return array with dimensions `source`, `receptor`, `layer`, and `varid`
#'
#' @export
extract_ISRM_array <- function (
  ncdf4_obj,
  source,
  receptor = source,
  varid = "PrimaryPM25",
  layer = 1
) {

  # First extract a "hull" slab, with indices ranged from min to max
  # of `source` and `receptor`, respectively. This is going to be larger
  # than the exact solution, but it's a fast first cut when extracting
  # a small chunk with locality.
  min_source <- min(source)
  min_receptor <- min(receptor)
  start <- c(min_source, min_receptor, layer)
  count <- c(max(source) - min_source + 1, max(receptor) - min_receptor + 1, 1)
  hull <- ncdf4::ncvar_get(ncdf4_obj, varid = varid, start = start, count = count, collapse_degen = FALSE)

  # Now extract the cells corresponding exactly to `source` and `receptor`,
  # for however many layers were requested (default 1)
  exact <- hull[source - min_source + 1, receptor - min_receptor + 1, , drop = FALSE]

  # Tack on one more dimension, for `varid`.
  arr <- array(exact, dim = c(dim(exact), 1))

  # The result has four dimensions: `source`, `receptor`, `layer`, and `varid`.
  dimnames(arr) <- list(
    "source" = paste0("S", source),
    "receptor" = paste0("R", receptor),
    "layer" = str_c("L", layer),
    "varid" = varid)

  return(arr)

}
