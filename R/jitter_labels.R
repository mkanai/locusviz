jitter_labels = function(label.pos, xscale) {
  grid::pushViewport(grid::viewport(xscale=xscale))
  lineW = as.numeric(grid::convertX(unit(1, "line"), "npc"))
  label.pos = trackViewer:::jitterLables(label.pos, xscale, lineW)
  label.pos = trackViewer:::reAdjustLabels(label.pos, lineW)
  grid::popViewport()
  return(label.pos)
}
