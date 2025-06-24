##------------------------------------------------------------------------
## Author  : izi (izi31416@protonmail.com)
## Project : axis cut
## Created : quarta set 29, 2021 15:21:04 -03
## License : MIT
## Updated : sex 15 out 2021 18:22:42 -03
##------------------------------------------------------------------------

##===========================================
##
##===========================================
ggname <- utils::getFromNamespace("ggname", "ggplot2")
flip_axis_labels <- utils::getFromNamespace("flip_axis_labels", "ggplot2")

##===========================================
##
##===========================================
rotate = function(x, angle = c("180", "90"), clockwise = TRUE)
{
  rot = function(x, clockwise) {
    if (clockwise)
      t( apply(x, 2, rev))
    else
      apply( t(x),2, rev)
  }#end ratate

  angle = match.arg(angle)
  out = switch(angle,
               "90" = rot(x, clockwise = clockwise),
               "180" = t(rot(x, clockwise = clockwise)))
  return(out)
}#end flip

##===========================================
##
##===========================================
is.even = function(x) x %% 2 == 0
is.odd = function(x) x %% 2 != 0

##===========================================
##
##===========================================
swap = function(x)
{
  d1 = dim(x)[1]
  d2 = dim(x)[2]
  if(is.vector(x)) h1 = 1:length(x) else h1 = 1:d1
  h = h1[is.odd(h1)]
  a = NULL
  for(i in h) a = c(a, c(h1[i+1], h1[i]))
  if(is.vector(x)) x = x[a] else x[,1:d2] = x[a,]
  x
}

##===========================================
##
##===========================================
pos = function(cut.axis)ifelse(cut.axis == "y", "left", "bottom")

##===========================================
##
##===========================================
gap_end = function(g, w)
{
  ng = length(g)
  nw = length(w)

  x = NULL
  if(nw < ng) w = rep(w[1], ng)

  for(i in 1 : ng) {
    x1 = g[i] + g[i] * w[i]
    x = c(x, x1)
  }#end for
  return(x)
}



##===========================================
##
##===========================================
value = function(z, vec, value.default = 0)
{
  lv = length(vec)
  lz = length(z)
  nz = names(z)

  if(lz > 1 & lz < lv & is.null(nz)) stop("name all or none", call. = F)

  nv = NULL
  v = NULL
  for(i in 1 : lv)
  {
    v1 = nz[i]
    nv = c(nv, v1)
    v = c(v, z[i])
  }#end for

  d = nz %in% vec

  if(lz == 1)
  {
    z = rep(z, lv)
    lz = length(z)
  }#end if

  if(lz == lv & is.null(nz))
  {
    names(z) = vec
    return(z)
  }#end if

  if(any(nv == "", na.rm = T)) stop("name all or none", call. = F)

  v[is.na(v)] = value.default

  if(!all(d))
  {
    d1 = paste0(nz[!d], collapse=", ")
    d2 = paste0(vec, collapse=", ")
    msg1 = paste0("names (", d1, ") do not match in (", d2, ")")
    stop(msg1, call. = F)
  }#end if

  s = vec[!(vec %in% nv)]

  j = 1
  for(i in 1 : length(nv))
  {
    if(is.na(nv[i]))
    {
      nv[i] = s[j]
      j = j + 1
    }#end if
  }#end for

  names(v) = nv

  return(v)
}

##===========================================
##
##===========================================
check_value = function(linetype)
{
  if(any(linetype < 1))
  {
    lt = linetype[linetype < 1]
    linetype[linetype < 1] = 1
    msg3 = paste0("linetype = ", lt, ": only integers greater than zero (converted to 1)")
    warning(msg3, call. = FALSE)
  }#end if

  list(linetype = linetype)
}#end check_value

##===========================================
##
##===========================================
line_zero = function(size = c(h = 0, v = 0),
                     color = c(h = "gray55", v = "gray55"),
                     linetype = c(h = 1, v = 1))
{
  vec = c("h", "v")

  msg1 = paste("line_zero: size =", paste(vec, collapse=","))
  msg2 = paste("line_zero: color =", paste(vec, collapse=","))
  msg3 = paste("line_zero: linetype =", paste(vec, collapse=","))

  size = tryCatch(value(size, vec, value.default = 0),
                  error = function(err){
                    message(msg1)
                    stop(err)
                  })

  color = tryCatch(value(color, vec, value.default = NA),
                   error = function(err){
                    message(msg2)
                    stop(err)
                   })

  linetype = tryCatch({
    linetype = as.integer(linetype)
    if(any(linetype < 1)) linetype = check_value(linetype)[["linetype"]]
    value(linetype, vec, value.default = 1L)
  }, error = function(err){
    message(msg3)
    stop(err)
  })

  return(list(size=size, color=color, linetype = linetype))
}

##===========================================
##
##===========================================
line_axis = function(size = c(l = 1, b = 1, r = 1, t = 1),
                     color=c(l = "gray55",b = "gray55", r = "gray55", t = "gray55"),
                     linetype = c(l = 1, b = 1, r = 1, t = 1),
                     grid.axis = TRUE, zero = FALSE)
{
  vec = c("l", "b", "r", "t")

  msg1 = paste("line_axis: size =", paste(vec, collapse=","))
  msg2 = paste("line_axis: color =", paste(vec, collapse=","))
  msg3 = paste("line_axis: linetype =", paste(vec, collapse=","))

  size = tryCatch(value(size, vec, value.default = 1),
                  error = function(err){
                    message(msg1)
                    stop(err)
                  })

  color = tryCatch(value(color, vec, value.default = NA),
                   error = function(err){
                    message(msg2)
                    stop(err)
                   })

#   linetype = check_value(linetype)[["linetype"]]

  linetype = tryCatch({
    linetype = as.integer(linetype)
    if(any(linetype < 1)) linetype = check_value(linetype)[["linetype"]]
    value(linetype, vec, value.default = 1)
  }, error = function(err){
    message(msg3)
    stop(err)
  })

  return(list(size=size, color=color, linetype = linetype, grid.axis = grid.axis, zero = zero))
}

##===========================================
##
##===========================================
gap_box = function(fill = "white", size = 1, color = "gray55", linetype = 1)
{
  linetype = check_value(linetype)[["linetype"]]
  list(fill = fill, size = size, color = color, linetype = linetype)
}

##===========================================
##
##===========================================
flip_rev_zero = function(cut.axis, x0, y0, flip, reverse, zero, data)
{
  if(cut.axis == "x") flip = !flip

  logi = c(flip, reverse, zero)

  logi = paste(as.integer(logi), collapse="")

  if(all(data$group == -1))
  {
    xr = c(1,1)
    yr = c(1,1)
  } else{
    yr = range(data$y, na.rm = TRUE)
    xr = range(data$x, na.rm = TRUE)
  }#end if

  xnp = xr[1] < 0 & xr[2] > 0
  ynp = yr[1] < 0 & yr[2] > 0
  xn =  xr[1] < 0 & xr[2] < 0
  xnr = xr[1] < 0 & xr[2] < 0 & reverse
  yn =  yr[1] < 0 & yr[2] < 0
  ynr = yr[1] < 0 & yr[2] < 0 & reverse

  xrv = if(xnr & x0 > .5) c(0,x0) else c(x0,1)
  yrv = if(ynr & y0 > .5) c(0,y0) else c(y0,1)

  out = switch(logi,
               "000" = c(0,1), # 1
               "100" = c(0,1), # 2
               "010" = c(0,1), # 3
               "001" = {
                 if(cut.axis == "x")
                   if(xnp) c(0,1) else if(xn) c(0,x0) else c(x0,1)
                 else
                   if(ynp) c(0,1) else if(yn) c(0,y0) else c(y0,1)
               }, # 4
               "110" = c(0,1), # 5
               "101" = {
                 if(cut.axis == "x")
                   if(xnp) c(0,1) else if(xn) c(0,y0) else c(y0,1)
                 else
                   if(ynp) c(0,1) else if(yn) c(0, x0) else c(x0,1)
               }, # 6
               "011" = {c(0,1)
                 if(cut.axis == "x")
                   if(xnp) c(0,1) else xrv
                 else
                   if(ynp) c(0,1) else yrv
               }, # 7
               "111" = {
                 if(cut.axis == "x")
                   if(xnp) c(0,1) else if(xn & y0 > .5) c(0,y0) else c(y0,1)
                 else
                   if(ynp) c(0,1) else if(ynr & x0 > .5) c(0,x0) else c(x0,1)
               }  # 8
  )#end switch
  out
}#end flip_rev_zero

##===========================================
##
##===========================================
draw_panel_fun =  function(data, panel_scales, coord,
                           cuts, k,
                           flip,
                           cut.axis,
                           gap,
                           gap.box,
                           line.zero,
                           line.axis,
                           reverse)
{
  ## line axis
  size.axis = line.axis[['size']]
  col.axis = line.axis[["color"]]
  lty.axis = line.axis[["linetype"]]
  zero = line.axis[["zero"]]
  ## line zero
  size.zero = line.zero[["size"]]
  col.zero = line.zero[["color"]]
  lty.zero = line.zero[["linetype"]]

  ry = panel_scales$y.range
  rx = panel_scales$x.range

  if(cut.axis == "x")
  {
    ss = swap(names(size.axis))
    sc = swap(names(col.axis))
    sl = swap(names(lty.axis))
    names(size.axis) = ss
    names(col.axis) = sc
    names(lty.axis) = sl

    flip = !flip
  }#end if

  if(flip) pr = rx else pr = ry

  if(length(cuts) < length(gap)) gap = gap[1]

  if(reverse) cuts = -1 * cuts

  ge = gap_end(cuts, gap)

  s = scales::rescale(transf(cuts, k)(cuts), from = pr)
  e = scales::rescale(transf(cuts, k)(ge), from = pr)

  ## center blank box
  mse = apply(rbind(s,e),2,mean,na.rm = TRUE)
  p = sapply(gap, function(x)if(x == 0) p1 = 0.003 else p1 = 0)
  dse = abs(s - e + 2*p)

  ## horizontal lines
  HL = rbind(x0 = 0, x1 = 1, y0 = c(s - p, e + p), y1 = c(s - p, e + p))
  ## blank box
  RC = rbind(x = 0.5, y = mse, width = 1, height = dse)

  # zero
  y0 = scales::rescale(0, from = ry)
  x0 = scales::rescale(0, from = rx)

  ZX = rbind(x = c(0, 1), y = c(y0, y0))
  ZY = rbind(y = c(x0, x0), x = c(0, 1))

  if(cut.axis == "x") col.zero = rev(col.zero)

  ## size cut.axis
  l = size.axis["l"] / 1000
  r = size.axis["r"] / 1000
  ## cut.axis
  se = sort(c(s, e))
  num = 1:length(se)
  a = num[is.even(num)]
  b = num[is.odd(num)]
  ya = se[a]
  yb = se[b]

  zz = flip_rev_zero(cut.axis = cut.axis, x0 = x0, y0 = y0,
                     reverse = reverse, flip = flip, zero = zero, data)

  z1 = zz[1]
  z2 = zz[2]

  ## left
  V1 = rbind(x0 = 0, x1 = 0, y0 = c(z1, ya + l), y1 = c(yb - l, z2))
  ## down
  V2 = rbind(x0 = 0, x1 = 1, y0 = 0, y1 = 0)
  ## right
  V3 = rbind(x0 = 1, x1 = 1, y0 = c(z1, ya + r), y1 = c(yb - r, z2))
  ## top
  V4 = rbind(x0 = 0, x1 = 1, y0 = 1, y1 = 1)

  if(flip)
  {
    HL = rotate(HL)  # horizontal lines
    RC = swap(RC)    # blank box

    V1 = rotate(V1)  # cut.axis left
    V2 = rotate(V2)  # cut.axis right
    V3 = rotate(V3)  # cut.axis down
    V4 = rotate(V4)  # cut.axis top

    size.axis = swap(size.axis) # size cut.axis; l,b,r,t
    col.axis = swap(col.axis)   # colors cut.axis: l,b,r,t
    lty.axis = swap(lty.axis)   # line type cut.axis: l,b,r,t

    size.zero = rev(size.zero)  # line zero: x,y
    col.zero = rev(col.zero)    # colors lines zero: x,y
    lty.zero = rev(lty.zero)    # types lines zero: x,y
  }#end flip

  ## panel lines
  hl = grid::segmentsGrob(x0 = HL[1,], x1 = HL[2,], y0 = HL[3,], y1 = HL[4,],
           gp = grid::gpar(lwd = gap.box[["size"]],
                           col = gap.box[["color"]],
                           lty = gap.box[["linetype"]]))

  ## blank box
  rc = grid::rectGrob(x = RC[1,], y = RC[2,], width = RC[3,], height = RC[4,],
           gp = grid::gpar(fill = gap.box[["fill"]], col = NA), just = "center")

  if(line.axis[["grid.axis"]])
  {

  ## x: down - y: left
  v1 = grid::segmentsGrob(x0 = V1[1,], x1 = V1[2,], y0 = V1[3,], y1 = V1[4,],
           gp = grid::gpar(lwd = size.axis["l"],
                           lex = 2,
                           col = col.axis["l"],
                           lty = lty.axis["l"],
                           lineend = "square", linejoin="mitre"))
  ## x: left - y: down
  v2 = grid::segmentsGrob(x0 = V2[1,], x1 = V2[2,], y0 = V2[3,], y1 = V2[4,],
           gp = grid::gpar(lwd = size.axis["b"],
                           lex = 2,
                           col = col.axis["b"],
                           lty = lty.axis["b"],
                           lineend = "square", linejoin="mitre"))
  ## cut.axis down
  v3 = grid::segmentsGrob(x0 = V3[1,], x1 = V3[2,], y0 = V3[3,], y1 = V3[4,],
           gp = grid::gpar(lwd = size.axis["r"],
                           lex = 2,
                           col = col.axis["r"],
                           lty = lty.axis["r"],
                           lineend = "square", linejoin="mitre"))
  ## x: top - y: right
  v4 = grid::segmentsGrob(x0 = V4[1,], x1 = V4[2,], y0 = V4[3,], y1 = V4[4,],
           gp = grid::gpar(lwd = size.axis["t"],
                           lex = 2,
                           col = col.axis["t"],
                           lty = lty.axis["t"],
                           lineend = "square", linejoin="mitre"))
  } else {
    v1=v2=v3=v4=NULL
  }#end if

  ## zero line x
  zx = if(size.zero["h"] > 0) grid::linesGrob(x=ZX[1,], y=ZX[2,],
       gp = grid::gpar(lwd = size.zero["h"],
                       col = col.zero["h"],
                       lty = lty.zero["h"],
                       lineend="square",linejoin="mitre")) else NULL

  ## zero line y
  zy = if(size.zero["v"] > 0) grid::linesGrob(x=ZY[1,], y=ZY[2,],
       gp = grid::gpar(lwd = size.zero["v"],
                       col = col.zero["v"],
                       lty = lty.zero["v"],
                       lineend="square",linejoin="mitre")) else NULL

#   grid::gTree(children = grid::gList(zx, zy, v1, v2, v3, v4, rc, hl))
  ggname("axis_cut", grid::grobTree(zx, zy, v1, v2, v3, v4, rc, hl))
}#end draw_panel_fun

##===========================================
##
##===========================================
GeomAxisCut <- ggproto("GeomAxisCut", GeomBlank,
                         draw_key = draw_key_blank,
                         draw_panel = draw_panel_fun)

##===========================================
##
##===========================================
StatCut <- ggproto("StatCut", Stat,
  compute_group = function(data, scales, reverse, cut.axis) {
    data
  }
)

##===========================================
##
##===========================================
axis_cut <- function(cuts = 0,
                     k = 1,
                     gap = 0,
                     type = "coord",
                     cut.axis = "y",
                     x = "identity",
                     y = "identity",
                     flip = FALSE,
                     reverse = FALSE,
                     name = waiver(),
                     breaks = breaks_transf(cuts, k, gap),
                     minor_breaks = waiver(),
                     n.breaks = NULL,
                     labels = waiver(),
                     limits = NULL,
                     xlim = NULL,
                     ylim = NULL,
                     position = pos(cut.axis),
                     expand = waiver(),
                     oob = scales::censor,
                     na.value = NA_real_,
                     guide = waiver(),
                     sec.axis = waiver(),
                     clip = "on",
                     line.zero = line_zero(),
                     line.axis = line_axis(),
                     gap.box = gap_box(),
                     ...) {

  cut.axis = match.arg(cut.axis, c("x", "y"))
  type = match.arg(type, c("coord", "scale"))

  size.axis = line.axis[["size"]]
  grid.axis = line.axis[["grid.axis"]]
  if(clip == "off") size.axis = size.axis / 2

  # The difference between transforming the scales and
  # transforming the coordinate system is that scale
  # transformation occurs BEFORE statistics, and coordinate
  # transformation afterwards.  Coordinate transformation also
  # changes the shape of geoms:

  ## FIXME: coord with sec.axis doesn't work
  ## FIXME: facet_wrap(free_y) and flip = TRUE: don't work

  if(type == "coord")
  {
    if(reverse) trans = "reverse" else trans = "identity"

    if(cut.axis == "y") {
      fun = "scale_y_continuous"
      y = if(reverse) transf_trans(-cuts, k) else transf_trans(cuts, k)
    } else {
      fun = "scale_x_continuous"
      x = if(reverse) transf_trans(-cuts, k) else transf_trans(cuts, k)
    }#end if

    if(!is.null(limits)) warning("type coord (limits): use xlim or ylim", call. = F)
    if(!is.list(sec.axis)) warning("type coord (sec.axis): change type to scale", call. = F)
  }#end if coord

  if(type == "scale")
  {
    trans = transf_trans(cuts, k)

    if(cut.axis == "y") {
      fun = "scale_y_continuous"
      y = if(reverse) "reverse" else "identity"
    } else {
      fun = "scale_x_continuous"
      x = if(reverse) "reverse" else "identity"
    }#end if
  }#end if scale

  arg.scale = list(trans = trans, breaks = breaks, labels = labels, position = position,
                   minor_breaks = minor_breaks, n.breaks = n.breaks, limits = limits,
                   expand = expand, oob = oob, na.value = na.value, guide = guide,
                   sec.axis = sec.axis, name = name)

  arg.coord = list(y = y, x = x, xlim = xlim, ylim = ylim, clip = clip, flip = flip)

  ## theme: remove axis.line and panel border
  arg.theme = list(axis.line = element_blank(), panel.border = element_blank())

  list(
       ggplot2::layer(
          data = NULL,
          stat = StatCut,#StatIdentity,
          geom = GeomAxisCut,
          position = PositionIdentity,
          inherit.aes = TRUE,
          check.aes = FALSE,
          params = list(cuts = cuts,
                        k = k,
                        flip = flip,
                        gap = gap,
                        cut.axis = cut.axis,
                        line.zero = line.zero,
                        line.axis = line.axis,
                        reverse = reverse,
                        gap.box = gap.box,
                        ...)),

       do.call("coord_trans_flip", arg.coord),
       do.call(fun, arg.scale),

       ## theme: remove axis.line and panel border
       if(grid.axis) do.call("theme", arg.theme)
  )#end list
}#end axis_cut

