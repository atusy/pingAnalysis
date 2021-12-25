#' Converts the IPv4 address into binary values
#' @noRd
coerce_ipv4_to_bits <- function(ipv4) {
  ipv4 |>
    stringr::str_remove("/.*$") |>
    stringr::str_split("\\.") |>
    purrr::map(function(x) {
      unlist(purrr::map(x, function(x) rev(as.integer(intToBits(x)[0:8]))))
    })
}


#' Find subnet of the given IPv4 addresses
#'
#' @param ipv4 must be suffixed by prefix-size.
#' @noRd
find_subnet <- function(ipv4) {
  prefix_size <- stringr::str_extract(ipv4, "[0-9]+$")
  purrr::map2_chr(
    coerce_ipv4_to_bits(ipv4),
    prefix_size,
    function(addr, size) paste(addr[1:size], collapse = "")
  )
}

#' @param str A binary string with 32bits.
#' @noRd
find_network_internal <- function(str) {
  start <- seq(0L, 3L) * 8L + 1L
  str |>
    substring(start, start + 7L) |>
    strtoi(base = 2L) |>
    paste(collapse = ".")
}

#' Find network address
#' @noRd
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
#'
#' @param address,ping the corresponding columns from the `log_df`
#'
#' @return list of character representing addresses not responding.
#'
#' @noRd
collect_dead_address <- function(address, ping) {
  purrr::accumulate2(
    address,
    ping,
    function(x, y, p) `if`(is.na(p), union, setdiff)(x, y),
    .init = character()
  )[-1L]
}

#' Measure timeout of one subnet, or switch
#'
#' @inheritParams measure_timeout
#' @param address_list A named list of IPv4 addresses that defines the addresses
#'   which should be present in each network represented by their names.
#'   The value is typically created by `group_address_by_subnet()`.
#'   Internally, required addresses are also collected from `log_df`.
#'
#' @noRd
measure_subnet_timeout_one_switch <- function(
  log_df,
  N = 1L,
  address_list = NULL
) {
  subnet <- unique(find_subnet(log_df$address))
  if (length(subnet) != 1L) stop("Subnet must be uniuqe")
  network <- find_network(subnet)
  member <- `if`(
    is.null(address_list),
    unique(log_df$address),
    address_list[[subnet]]
  )
  log_df |>
    dplyr::arrange(timestamp) |>
    dplyr::mutate(
      ping = dplyr::if_else(
        purrr::map_lgl(collect_dead_address(address, ping), setequal, member),
        NA_real_,
        dplyr::coalesce(ping, 0)
      ),
      address = network,
    ) |>
    measure_timeout(N = 1L) |>
    dplyr::mutate(n_timeout = as.integer(ceiling(n_timeout / 3)))
}

#' Converts the IPv4 addresses into a list of character based on subnet
#'
#' @param ipv4 A character vector of IPv4 addresses suffixed by prefix-size.
#'
#' @return A named list of character vectors.
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
#'
#' @return A named list of `log_df`-like data frames.
#' @noRd
group_log_by_subnet <- function(log_df) {
  log_df |>
    dplyr::mutate(subnet = find_subnet(address)) |>
    tidyr::nest(data = !subnet) |>
    tibble::deframe()
}

#' Measure timeout of subnet, or switch.
#'
#' @inheritParams measure_all_timeout
#' @param address_all
#'   A character vector of all the IPv4 addresses to be logged.
#'   They must be suffixed by prefix-size (e.g., '192.168.1.1/24').
#'
#' @examples
#' # Prepare data
#' basetime <- 20200101000000
#' pings <- c(
#'   # 1  2  3  4  5  6  7  8  9 10
#'   1, 0, 0, 0, 0, 0, 0, 0, 0, 1,
#'   1, 1, 0, 0, 0, 0, 1, 1, 0, 1,
#'   1, 1, 1, 0, 0, 0, 0, 1, 0, 1
#' )
#' N <- length(pings) / 3L
#' log_df <- tibble::tibble(
#'   timestamp = lubridate::ymd_hms(basetime + seq(N)) |> rep(3L),
#'   address = paste0("192.168.1.", 1L:3L, "/24") |> rep(each = N),
#'   ping = ifelse(pings == 0, NA_real_, pings)
#' )
#'
#' # Measure
#' measure_subnet_timeout(
#'   log_df,
#'   N = 2L,
#'   address_all = unique(log_df$address)
#' )
#'
#' @inherit measure_all_timeout return
#' @export
measure_subnet_timeout <- function(log_df, N = 1L, address_all = NULL) {
  group_log_by_subnet(log_df) |>
    purrr::map_dfr(
      measure_subnet_timeout_one_switch,
      address_list = group_address_by_subnet(c(address_all, log_df$address))
    ) |>
    dplyr::arrange(start, end)
}
