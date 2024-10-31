# This code is licensed under a permissive open-source license.
# 
# Copyright (c) Michael Hönig, 2024.
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this code, to use, copy, modify, merge, publish, and distribute the code, 
# provided that proper attribution is given to the original author, Michael Hönig.
# Any derivative works or redistributed versions of this code must be provided
# under the same open-source license with identical permissions.
#
# Disclaimer: This code is provided "as-is" without any warranties or guarantees
# of any kind. The author assumes no responsibility or liability for any errors
# or issues that arise from the use, modification, or distribution of this code

generate_transformer <- function(breakpoints, scales) {
  # Prüfe, ob die Anzahl der Breakpoints und Skalen zueinander passt
  if (length(scales) != length(breakpoints) + 1) {
    stop("Die Länge von 'scales' muss um 1 größer sein als die Länge von 'breakpoints'.")
  }
  if (is.unsorted(breakpoints)) {
    stop("Die breakpoints müssen sortiert sein.")
  }
  
  # Füge 0 als Breakpoint ein und unterteile dafür das entsprechende Interval
  intervals <- c(-Inf, breakpoints, Inf)
  if (!(0 %in% breakpoints)) {
    int0 <- findInterval(0, intervals)
    intervals <- sort(c(0, intervals))
    scales <- append(scales, scales[int0], int0)
    pos0 <- int0 + 1
  } else {
    pos0 <- which(0 == intervals)
  }
  
  
  mapped_sizes <- diff(intervals) * scales
  mapped_offsets = c(
    rev(cumsum(-mapped_sizes[seq(pos0-1, 2, -1)])),
    0, 0,
    cumsum(mapped_sizes[-(1:pos0-1)])
  )
  unmapped_offsets = append(intervals, 0, after = pos0)[-1]
  
  rm(breakpoints, mapped_sizes, int0, pos0)
  
  transform <- function(x) {
    i = findInterval(x, intervals, rightmost.closed=TRUE)
    return((x - unmapped_offsets[i])*scales[i] + mapped_offsets[i])
  }
  
  mapped_intervals = transform(intervals)
  
  inv_transform <- function(y) {
    i = findInterval(y, mapped_intervals, rightmost.closed=TRUE)
    return((y - mapped_offsets[i])/scales[i] + unmapped_offsets[i])
  }
  
  return(list(transform=transform, inverse=inv_transform))
}

create_interval_scales <- function(breakpoints, scaleingfactors, name) {
  transformations = generate_transformer(breakpoints, scaleingfactors)
  return(scales::trans_new(name, transformations[["transform"]], transformations[["inverse"]]))
}


                
