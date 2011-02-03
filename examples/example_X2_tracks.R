# Example showing the use of tracks in tree/alignment plots.

library(phylosim)

pfam.aln <- "data/PF02171_seed.fasta"
pfam.tree <- "data/PF02171_seed.nh"

sim <- PhyloSim()
readTree(sim, pfam.tree)
readAlignment(sim, pfam.aln)
aln.length <- getAlignmentLength(sim)

# Create a list to hold the tracks.
tracks <- list()

### Highlight a number of residues with red markers, positioned just above the alignment.
important.positions <- c(1, 10, 23, 24, 25, 28, 35, 100, 110:120)
track <- data.frame(
  id = 'Important Sites',
  pos = important.positions,
  color = 'red',
  layout = 'above'
)
# Add this track to the list
tracks <- c(tracks, list(track))

### Plot some brownian motion as a color gradient
brownian.trail <- function(n, start=0.5, sd=0.2) {
  x <- start
  trail <- c()
  for (i in 1:n) {
    x <- x + rnorm(1, sd=sd)
    if (x >= 0.9) { x <- 0.9 }
    if (x <= 0.1) { x <- 0.1 }
    trail <- c(trail, x)
  }
  return(trail)
}
trail <- brownian.trail(aln.length)
track <- data.frame(
  id = 'Snail trail',
  pos = 1:length(trail),
  layout = 'above',
  score = trail,
  color.gradient = 'blue,red'
)
# Add this track to the list
tracks <- c(tracks, list(track))

### Plot more brownian motion 
trail <- brownian.trail(aln.length)
dy <- 0.05
y_lo <- trail - dy
y_lo <- pmax(0, y_lo) # Don't plot below 0
y_hi <- trail + dy
y_hi <- pmin(1, y_hi) # Don't plot above 1
track <- data.frame(
  id = 'Snail trail 2',
  pos = 1:length(trail),
  layout = 'above',
  color = 'orange',
  y_lo = y_lo,
  y_hi = y_hi
)
tracks <- c(tracks, list(track))

### Add a spacer track with a gray background
track <- data.frame(
  id = "Empty Space",
  layout = 'above',
  background = 'gray'
)
tracks <- c(tracks, list(track))

### Add a very tall track with sinusoidal values
sin_y <- sin( (1:aln.length) / 20 ) * .5 + .5
cos_y <- cos( (1:aln.length) / 10 ) * .5 + .5
track <- data.frame(
  id = "Sine wave",
  pos = 1:aln.length,
  layout = 'above',
  color.gradient = c('blue,green,red'),
  score = cos_y,
  y_lo = pmin(0.5, sin_y),
  y_hi = pmax(0.5, sin_y),
  height = 10
)
tracks <- c(tracks, list(track))

plot(sim, tracks=tracks)