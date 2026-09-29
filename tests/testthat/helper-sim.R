# Dane binarne z modelu 2PL (mirt::simdata)
sim_binary <- function(n, p, seed) {
  set.seed(seed)
  dat <- as.data.frame(mirt::simdata(
    a = matrix(stats::rlnorm(p, 0.2, 0.2)),
    d = matrix(stats::rnorm(p)),
    N = n,
    itemtype = "dich"
  ))
  names(dat) <- paste0("it_", seq_len(p))
  dat
}

# Dwie wersje testu po 200 osob: A = it_1-it_6, B = it_7-it_12,
# plus itemy wspolne `anchors` rozwiazywane w obu wersjach
sim_booklets <- function(seed, anchors = integer(0)) {
  dat <- sim_binary(400, 12, seed = seed)
  zeszyt <- rep(c("A", "B"), each = 200)
  dat[zeszyt == "A", setdiff(7:12, anchors)] <- NA
  dat[zeszyt == "B", setdiff(1:6, anchors)] <- NA
  list(dat = dat, zeszyt = zeszyt)
}
