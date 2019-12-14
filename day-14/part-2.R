input <- 
  "1 JKXFH => 8 KTRZ
11 TQGT, 9 NGFV, 4 QZBXB => 8 MPGLV
8 NPDPH, 1 WMXZJ => 7 VCNSK
1 MPGLV, 6 CWHX => 5 GDRZ
16 JDFQZ => 2 CJTB
1 GQNQF, 4 JDFQZ => 5 WJKDC
2 TXBS, 4 SMGQW, 7 CJTB, 3 NTBQ, 13 CWHX, 25 FLPFX => 1 FUEL
3 WMXZJ, 14 CJTB => 5 FLPFX
7 HDCTQ, 1 MPGLV, 2 VFVC => 1 GSVSD
1 WJKDC => 2 NZSQR
1 RVKLC, 5 CMJSL, 16 DQTHS, 31 VCNSK, 1 RKBMX, 1 GDRZ => 8 SMGQW
2 JDFQZ, 2 LGKHR, 2 NZSQR => 9 TSWN
34 LPXW => 8 PWJFD
2 HDCTQ, 2 VKWN => 8 ZVBRF
2 XCTF => 3 QZBXB
12 NGFV, 3 HTRWR => 5 HDCTQ
1 TSWN, 2 WRSD, 1 ZVBRF, 1 KFRX, 5 BPVMR, 2 CLBG, 22 NPSLQ, 9 GSVSD => 5 NTBQ
10 TSWN => 9 VFVC
141 ORE => 6 MKJDZ
4 NPSLQ, 43 VCNSK, 4 PSJL, 14 KTRZ, 3 KWCDP, 3 HKBS, 11 WRSD, 3 MXWHS => 8 TXBS
8 VCNSK, 1 HDCTQ => 7 MXWHS
3 JDFQZ, 2 GQNQF => 4 XJSQW
18 NGFV, 4 GSWT => 5 KFRX
2 CZSJ => 7 GMTW
5 PHKL, 5 VCNSK, 25 GSVSD => 8 FRWC
30 FRWC, 17 GKDK, 8 NPSLQ => 3 CLBG
8 MXWHS, 3 SCKB, 2 NPSLQ => 1 JKXFH
1 XJSQW, 7 QZBXB => 1 LGKHR
115 ORE => 6 GQNQF
12 HTRWR, 24 HDCTQ => 1 RKBMX
1 DQTHS, 6 XDFWD, 1 MXWHS => 8 VKWN
129 ORE => 3 XCTF
6 GQNQF, 7 WJKDC => 5 PHKL
3 NZSQR => 2 LPXW
2 FLPFX, 1 MKLP, 4 XDFWD => 8 NPSLQ
4 DQTHS, 1 VKWN => 1 BPVMR
7 GMTW => 1 TXMVX
152 ORE => 8 JDFQZ
21 LGKHR => 9 NPDPH
5 CJTB, 1 QZBXB, 3 KFRX => 1 GTPB
1 MXWHS => 3 CWHX
3 PHKL => 1 NGFV
1 WMXZJ => 7 XDFWD
3 TSWN, 1 VKWN => 8 GKDK
1 ZVBRF, 16 PWJFD => 8 CMJSL
3 VCNSK, 7 GDRZ => 4 HKBS
20 XJSQW, 6 HTRWR, 7 CJTB => 5 WMXZJ
12 ZVBRF, 10 FRWC, 12 TSWN => 4 WRSD
16 HDCTQ, 3 GTPB, 10 NGFV => 4 KWCDP
3 TXMVX, 1 NPDPH => 8 HTRWR
9 NPDPH, 6 LPXW => 8 GSWT
4 MKLP => 1 TQGT
34 GTPB => 3 RVKLC
25 VFVC, 5 RVKLC => 8 DQTHS
7 KWCDP => 3 SCKB
6 LGKHR => 8 MKLP
39 MKJDZ => 9 CZSJ
2 TSWN, 1 WMXZJ => 3 PSJL"

input_lines <- strsplit(input, "\n")[[1]]

read_input_lines <- function(input_lines) {
  reactions <- strsplit(input_lines, " => ")
  do.call(c, lapply(reactions, function(reaction) {
    chemical_output <- strsplit(reaction[2], " ")[[1]][[2]]
    output_quantity <- as.integer(strsplit(reaction[2], " ")[[1]][[1]])
    inputs <- do.call(c, lapply(strsplit(strsplit(reaction[1], ", ")[[1]], " "), function(x) {
      res <- list(as.integer(x[1]))
      names(res) <- x[2]
      res
    }))
    form <- list(quantity = output_quantity, inputs = inputs)
    res <- list(form)
    names(res) <- chemical_output
    res
  }))  
}

reactions <- read_input_lines(input_lines)

compute_chemicals_orders <- function(reactions) {
  orders <- c(c(ORE = 0L), vapply(reactions, function(x) {
    if (is.null(x$order)) NA_integer_ else x$order
  }, integer(1)))
  
  if (all(!is.na(orders))) return(reactions)
  
  reactions <- lapply(reactions, function(reaction) {
    reaction$order <- max(orders[names(reaction$inputs)]) + 1L
    reaction
  })
  
  compute_chemicals_orders(reactions)
}

reactions <- compute_chemicals_orders(reactions)

required <- reactions$FUEL$inputs

add_lists <- function(list1, list2) {
  commons <- intersect(names(list1), names(list2))
  inter <- lapply(commons, function(x) {
    list1[[x]] + list2[[x]]
  })
  names(inter) <- commons
  list1[commons] <- NULL
  list2[commons] <- NULL
  c(list1, list2, inter)
}

reduce_required_once <- function(required, residual) {
  orders <- vapply(reactions[names(required)], function(x) x$order, integer(1))
  max_order <- max(orders)
  required_to_be_reduced <- required[orders == max_order]
  for (i in seq.int(1, length.out = length(required_to_be_reduced))) {
    reaction <- reactions[[names(required_to_be_reduced[i])]]
    previous_residual <- unlist(residual[names(required_to_be_reduced[i])])
    if (is.null(previous_residual)) {
      previous_residual <- 0L
    }
    needed <- unlist(required_to_be_reduced[i]) - previous_residual
    rest <- needed %% reaction$quantity
    n_reaction <- needed %/% reaction$quantity
    next_residual <- 0L
    if (rest > 0) {
      n_reaction <- n_reaction + 1L
      next_residual <- reaction$quantity - rest
    }
    next_residual <- as.list(next_residual)
    names(next_residual) <- names(required_to_be_reduced[i])
    required[names(required_to_be_reduced[i])] <- NULL
    residual[names(required_to_be_reduced[i])] <- NULL
    required <- add_lists(required, as.list(unlist(reaction$inputs) * n_reaction))
    residual <- add_lists(residual, next_residual)
  }
  list(required = required, residual = residual)
}

reduce_required <- function(required, residual) {
  orders <- vapply(reactions[names(required)], function(x) x$order, integer(1))
  max_order <- max(orders)
  while (max_order > 1) {
    res <- reduce_required_once(required, residual)
    required <- res$required
    residual <- res$residual
    orders <- vapply(reactions[names(required)], function(x) x$order, integer(1))
    max_order <- max(orders)
  }
  reduce_required_once(required, residual)
}

residual <- list()
n_fuels <- 0
ore <- 0
max_ore <- 1000000000000 

while (TRUE) {
  res <- reduce_required(required, residual)
  residual <- res$residual
  ore <- ore + as.numeric(res$required$ORE)
  if (ore > max_ore) break
  n_fuels <- n_fuels + 1
}
