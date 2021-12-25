#' Converts the IPv4 address into binary values
#' @noRd
coerce_ipv4_to_bits <- function(ipv4) {
  ipv4 |>
    stringr::str_remove('/.*$') |>
    stringr::str_split('\\.') |>
    purrr::map(function(x) {
      unlist(purrr::map(
        x,
        function(x) rev(as.integer(intToBits(x)[0:8]))
      ))
    })
}


#' Find subnet of the given IPv4 addresses
#'
#' @param ipv4 must be suffixed by prefix-size.
#' @noRd
find_subnet <- function(ipv4) {
  prefix_size <- stringr::str_extract(ipv4, '[0-9]+$')
  purrr::map2_chr(
    coerce_ipv4_to_bits(ipv4),
    prefix_size,
    function(addr, size) paste(addr[1:size], collapse = "")
  )
}

#' @param str A binary string with 32bits.
#' @noRd
find_network_internal <- function(str) {
  start = seq(0L, 3L) * 8L + 1L
  str |>
    substring(start, start + 7L) |>
    strtoi(base = 2L) |>
    paste(collapse = ".")
}

#' Find network address
#'
find_network <- function(subnet) {
  subnet |>
    stringr::str_pad("0", width = 32L, side = "right") |>
    purrr::map_chr(find_network_internal) |>
    paste0("/", stringr::str_length(subnet))
}

#' Collect dead address by accumulating address and ping values
#'
#' On timeout, an address is added to the set of currently dead.
#' Otherwise, the address removed from the set.
collect_dead_address <- function(address, ping) {
  purrr::accumulate2(
    address,
    ping,
    function(x, y, p) `if`(is.na(p), union, setdiff)(x, y),
    .init = character()
  )[-1L]
}

#' Measure timeout of one subnet, or switch
#' @noRd
measure_subnet_timeout_one_switch <- function(
  log,
  N,
  address_list = NULL
) {
  subnet <- unique(find_subnet(log$address))
  if (length(subnet) != 1L) stop("Subnet must be uniuqe")
  network <- find_network(subnet)
  member <- `if`(
    is.null(address_list),
    unique(log$address),
    address_list[[subnet]]
  )
  log |>
    dplyr::arrange(timestamp) |>
    dplyr::mutate(
      dead = collect_dead_address(address, ping),
      ping = dplyr::if_else(
        purrr::map_lgl(dead, setequal, member),
        NA_real_,
        dplyr::coalesce(ping, 0)
      ),
      address = network,
      dead = NULL
    ) |>
    measure_timeout(N = 1L) |>
    dplyr::mutate(n_timeout = as.integer(ceiling(n_timeout / 3)))
}

#' Converts the IPv4 addresses into a list of character based on subnet
#' @noRd
group_address_by_subnet <- function(ipv4) {
  ipv4 |>
    unique() |>
    purrr::set_names(find_subnet) |>
    tibble::enframe() |>
    tidyr::chop(value) |>
    tibble::deframe()
}

#' Converts the log data frame into a list of data frame based on subnet
#' @noRd
group_log_by_subnet <- function(log) {
  log |>
    dplyr::mutate(subnet = find_subnet(address)) |>
    tidyr::nest(data = !subnet) |>
    tibble::deframe()
}

#' Measure timeout of subnet, or switch.
#'
#' @inheritParams measure_timeout
#' @param address_all
#' A character vector of all the IPv4 addresses to be logged.
#' They must be annotated by prefix size (e.g., '192.168.1.1/24').
measure_subnet_timeout <- function(log, N = 1L, address_all = NULL) {
  group_log_by_subnet(log) |>
    purrr::map_dfr(
      measure_subnet_timeout_one_switch,
      address_list = group_address_by_subnet(c(address_all, log$address))
    ) |>
    dplyr::arrange(start, end)
}
