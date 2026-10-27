# Tiny stored ZIPs with explicit directory metadata, including malicious names.
# These are synthetic archives, never official observations.
mpr_test_zip <- function(names, bodies = rep(list(charToRaw("fixture")), length(names)),
                         declared_sizes = lengths(bodies), attributes = rep(0, length(names))) {
  uint <- function(x, n) as.raw(floor(x / 256^(0:(n - 1))) %% 256)
  local <- directory <- raw(); offset <- 0
  for (i in seq_along(names)) {
    name <- charToRaw(names[[i]]); body <- bodies[[i]]
    crc <- digest::digest(body, algo = "crc32", serialize = FALSE)
    crc <- rev(as.raw(strtoi(substring(crc, seq(1, 7, 2), seq(2, 8, 2)), 16L)))
    sizes <- c(uint(length(body), 4), uint(declared_sizes[[i]], 4))
    header <- c(as.raw(c(0x50,0x4b,3,4)), uint(20,2), raw(8), crc, sizes, uint(length(name),2), raw(2))
    local <- c(local, header, name, body)
    directory <- c(directory, as.raw(c(0x50,0x4b,1,2)), uint(788,2), uint(20,2), raw(8), crc,
      sizes, uint(length(name),2), raw(8), uint(attributes[[i]],4), uint(offset,4), name)
    offset <- length(local)
  }
  eocd <- c(as.raw(c(0x50,0x4b,5,6)), raw(4), uint(length(names),2), uint(length(names),2),
    uint(length(directory),4), uint(length(local),4), raw(2))
  path <- tempfile(fileext = ".zip")
  writeBin(c(local, directory, eocd), path)
  path
}

mpr_mutate_xlsx <- function(path, entry, transform) {
  listing <- utils::unzip(path, list = TRUE)
  bodies <- lapply(seq_len(nrow(listing)), function(i) {
    con <- unz(path, listing$Name[[i]], open = "rb")
    on.exit(close(con), add = TRUE)
    readBin(con, "raw", n = listing$Length[[i]])
  })
  idx <- match(entry, listing$Name)
  bodies[[idx]] <- charToRaw(transform(rawToChar(bodies[[idx]])))
  mpr_test_zip(listing$Name, bodies)
}
