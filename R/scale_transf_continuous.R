#------------------------------------------------------------------------
# Author  : izi (izi31416@protonmail.com)
# Project : HBGQ2020
# Created : sábado ago 13, 2022 09:53:32 -03
# License : MIT
# Updated : dom 23 out 2022 01:32:06 -03
#------------------------------------------------------------------------

# REFERENCIAS =============================================================
# Estudo de Distorção
# John, J. A., & Draper, N. R. (1980). An Alternative Family of Transformations.
#   Journal of the Royal Statistical Society. Series C (Applied Statistics), 29(2), 190–197.
#   https://doi.org/10.2307/2986305

# Package ggbreak
# S Xu, M Chen#, T Feng, L Zhan, L Zhou, G Yu*. Use ggbreak to effectively utilize plotting space
#    to deal with large datasets and outliers. Frontiers in Genetics. 2021, 12:774846.
#    doi: 10.3389/fgene.2021.774846

##===========================================
##
##===========================================
rotate = function(x,
                  angle = c("180", "90"),
                  clockwise = TRUE
){
  rot = function(x, clockwise) {
    if (clockwise) t( apply(x, 2, rev)) else apply( t(x),2, rev)
  }#end ratate

  angle = match.arg(angle)

  out = switch(angle,
           "90" = rot(x, clockwise = clockwise),
           "180" = t(rot(x, clockwise = clockwise))
        )
  return(out)
}#end flip

##===========================================
##
##===========================================
is.even = function(x) x %% 2 == 0 ## número par
is.odd = function(x) x %% 2 != 0 ## número  ímpar

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
}#end swap

##===========================================
##
##===========================================
position_default = function(p = c("y", "x"))
{
  p = match.arg(p)
  ifelse(p == "y", "left", "bottom")
}

##===========================================
##
##===========================================
transf = function(g = 0, k = 1)
{
  function(x,...)
  {
    ng = length(g)
    nk = length(k)

    if(nk < ng) k = rep(k[1], ng)

    for(i in 1 : ng)
    {
      if(g[i] > 0)
        x = pmin(x, g[i]) + k[i] * pmax(x - g[i], 0)
      else
        x = pmax(x, g[i]) + k[i] * pmin(x - g[i], 0)
    }#end for
    x
  }#end function
}#end transf

##===========================================
##
##===========================================
transf_inv = function(g = 0, k = 1)
{
  function(x,...)
  {
    ng = length(g)
    nk = length(k)

    if(nk < ng) k = rep(k[1], ng)

    for(i in ng : 1)
    {
      if(g[i] > 0)
        x = pmin(x, g[i]) + pmax(x - g[i], 0) / k[i]
      else
        x = pmax(x, g[i]) + pmin(x - g[i], 0) / k[i]
    }#end for
    x
  }#end function
}#end transf_inv


##===========================================
##
##===========================================
transf_trans = function(g = 0, k = 1)
{
  scales::trans_new(
      name = "transf",
      trans = transf(g, k),
      breaks = pretty_transf(g),
      inverse = transf_inv(g, k),
      minor_breaks = scales::regular_minor_breaks(),
      format = scales::label_number(),
      domain = c(-Inf, Inf)
      )
}#end transf_trans

##===========================================
##
##===========================================
gap_end = function(g, p)
{
  np = length(p)
  ng = length(g)
  if(np < ng) p = rep(p[1], ng)
  res = sapply(g, function(x,p) {x + x*p}, p)
  diag(as.matrix(res))
}

##===========================================
##
##===========================================
pretty_transf = function(cuts = 0,
                         k = 1,
                         g = 0,
                         n = 7
){
  function(x, ...)
  {
    ge = gap_end(cuts, g)
    xr = range(x, na.rm = TRUE)
    bk = scales::trans_breaks(transf(cuts,k), transf_inv(cuts,k), n)(xr)
    nb = c(cuts, ge, bk)
#     nb = sort(unique(nb[!is.na(nb)]))
    nb = sort(unique(nb))
    return(nb)
  }
}#end pretty_transf


##===========================================
##
##===========================================
set_lines = function(x, default)
{
  x = x[!is.na(x)]
  nx = length(x)
  ndef = length(default)
  dname = names(default)
  xname = xn = names(x)
  default_type = default

  if(is.null(xname))
  {
    if(nx == 1) default = rep(x[1], ndef)
    if(nx <= (ndef-1)) default = c(x, default[(nx+1):ndef])
    default = default[1:ndef]
    names(default) = dname[1:ndef]
  } else {
    xname = xname[xname!=""]
    xname = xname[(xname%in%dname)]
    default[xname] <- x[xname]
  }

  ig_default = x[!(xn %in% dname)]

  if(length(ig_default))
  {
    ignames_default = names(ig_default)
    ignames_default
    #  ignames_default[ignames_default==""] <- " "
    igs = paste0("(",ignames_default," = ",ig_default, ")", collapse = ", ")
    dn = paste(dname,collapse=" ")
    msg = sprintf("In line_axis(), ignoring parameters: '%s'\nAxis can be specified either text: %s", igs,dn)
    warning(msg, call. = FALSE)
}
  default
}#end set_lines


##===========================================
##
##===========================================
char_num_lty = function(lty)
{
  ltn = c('blank'=0, 'solid'=1, 'dashed'=2, 'dotted'=3,
          'dotdash'=4, 'longdash'=5, 'twodash'=6)
  ltc = c('blank', 'solid', 'dashed', 'dotted',
          'dotdash', 'longdash', 'twodash')
  cha = NULL
  num = NULL
  for(i in 1:length(lty))
  {
    sly = lty[[i]]
    asn = suppressWarnings(as.numeric(sly))
    asn = if(is.na(asn)) ltn[sly] else ltn[asn+1]
    cha[i] = ltc[asn+1]
    num[i] = asn
  }
  names(cha) <- names(lty)
  names(num) <- names(lty)
  list(cha, num)
}

##===========================================
##
##===========================================
line_axis = function(...,
                     size = 0.6,
                     color = "gray45",
                     linetype = 1L,
                     grid.axis = T,
                     size_default = NULL,
                     color_default = NULL,
                     linetype_default = NULL
){
  if(is.null(size_default)) size_default = c(t = 0.6, r = 0.6, b = 0.6, l = 0.6)
  if(is.null(color_default)) color_default = c(t="gray45",r="gray45", b="gray45", l="gray45")
  if(is.null(linetype_default)) linetype_default = c(t = 1, r = 1, b = 1, l = 1)

  size = set_lines(size, size_default)

  color = set_lines(color, color_default)

  linetype = set_lines(linetype, linetype_default)
  linetype = if(is.numeric(linetype)) {
    char_num_lty(linetype)[[2]]
  } else {
    char_num_lty(linetype)[[1]]
  }

  return(list(size=size, color=color, linetype = linetype, grid.axis = grid.axis))
}


##===========================================
##
##===========================================
gap_box = function(...,fill = "white", size = 1, color = "gray55", linetype = 1){
  list(fill = fill, size = size, color = color, linetype = linetype)
}#end gap_box

#===========================================
# 
#===========================================
ggname = function (prefix, grob) 
{
  grob$name <- grid::grobName(grob, prefix)
  grob
}

##===========================================
##
##===========================================
draw_panel_fun =  function(data,
                           panel_scales,
                           panel_params,
                           self,
                           cuts,
                           k,
                           cut_axis,
                           gap,
                           gap.box,
                           line.axis
){
  ## line axis
  size.axis = line.axis[['size']]
  if(panel_params$clip == "off") size.axis = size.axis / 2

  col.axis = line.axis[["color"]]
  lty.axis = line.axis[["linetype"]]

  # verifcar se é flip
  flip = flp = class(panel_params)[1] == "CoordFlip" | class(panel_params)[1] == "CoordTransFlip"

  yr = panel_scales$y.range
  xr = panel_scales$x.range

  x.major = panel_scales$x.major
  x_reverse = if(!is.null(x.major)) x.major[1] > x.major[length(x.major)] else FALSE

  y.major = panel_scales$y.major
  y_reverse = if(!is.null(y.major)) y.major[1] > y.major[length(y.major)] else FALSE

  xd = panel_scales$x$scale_is_discrete
  if(is.null(xd))
  {
    xd = attr(panel_scales$x.major, "class")
    xd = if(is.null(xd))  FALSE else TRUE
  }

  yd = panel_scales$y$scale_is_discrete
  if(is.null(yd))
  {
    yd = attr(panel_scales$y.major, "class")
    yd = if(is.null(yd)) FALSE else TRUE
  }

  if(length(cuts) < length(gap)) gap = gap[1]

  if(cut_axis == "x")
  {
    ss = swap(names(size.axis))
    sc = swap(names(col.axis))
    sl = swap(names(lty.axis))
    names(size.axis) = ss
    names(col.axis) = sc
    names(lty.axis) = sl

    if(!flip) pr = xr else pr = yr

    if(flip & x_reverse & y_reverse){
      nc = -1; lx = "xa"
    } else if(!flip & !x_reverse & !y_reverse){
      nc = 1; lx = "xb"
    } else if(flip & !x_reverse & y_reverse){
      nc = -1; lx = "xc"
    } else if(!flip & x_reverse & !y_reverse){
      nc = -1; lx = "xd"
    } else if(!flip & x_reverse & y_reverse){
      nc = -1; lx = "xe"
    } else if(!flip & !x_reverse & y_reverse){
      nc = 1; lx = "xf"
    } else if(flip & x_reverse & !y_reverse){#NOTE: confirmar
      nc = 1; lx = "xg"
    } else {
      nc = 1; lx = "xh"
    }

    ppx = panel_params$trans$x[[1]]

    if(!is.null(ppx))
    {
      fun = gsub("-.*", "", ppx)
      base = gsub("log-", "", ppx)
      if(fun == "log") base = as.numeric(base)
      if(fun == "log") cuts = do.call(fun, list(cuts, base))
      if(fun == "sqrt") cuts = do.call(fun, list(cuts))
    }
  }

  ly =""

  if(cut_axis == "y")
  {
    if(flip) pr = xr else pr = yr

    if(flip & x_reverse & y_reverse){
      nc = -1; ly = "ya"
    } else if(!flip & !x_reverse & !y_reverse){
      nc = 1; ly = "yb"
    } else if(flip & !x_reverse & y_reverse){
      nc = 1; ly = "yc"
    } else if(!flip & x_reverse & !y_reverse){
      nc = 1; ly = "yd"
    } else if(!flip & x_reverse & y_reverse){
      nc = -1; ly = "ye"
    } else if(!flip & !x_reverse & y_reverse){
      nc = -1; ly = "yf"
    } else if(flip & x_reverse & !y_reverse){
      nc = -1; ly = "yg"
    } else {
      nc = 1; ly = "yh"
    }

    ppy = panel_params$trans$y[[1]]

    if(!is.null(ppy))
    {
      fun = gsub("-.*", "", ppy)
      base = gsub("log-", "", ppy)
      if(fun == "log") base = as.numeric(base)
      if(fun == "log") cuts = do.call(fun, list(cuts, base))
      if(fun == "sqrt") cuts = do.call(fun, list(cuts))
    }
  }

  cuts = nc * cuts

  ge = gap_end(cuts, gap)
  st = transf(cuts, k)(cuts)
  et = transf(cuts, k)(ge)

  s = scales::rescale(st, from = pr)
  e = scales::rescale(et, from = pr)

  ## center blank box
  mse = apply(rbind(s,e),2,mean,na.rm = TRUE)
  p = sapply(gap, function(x) ifelse(x == 0, 0.003, 0))
  dse = abs(s - e + 2*p)

  ## horizontal lines
  yl0 = c(s + p, e - p)
  yl1 = c(s + p, e - p)
#   HL = rbind(x0 = 0, x1 = 1, y0 = c(s - p, e + p), y1 = c(s - p, e + p))
  HL = rbind(x0 = 0, x1 = 1, y0 = yl0, y1 = yl1)

  ## blank box
  RC = rbind(x = 0.5, y = mse, width = 1, height = dse)

  ## size cut_axis
  l = size.axis["l"] / 1000
  r = size.axis["r"] / 1000
  ## cut_axis
  se = sort(c(s, e))
  num = 1:length(se)
  a = num[is.even(num)]
  b = num[is.odd(num)]
  ya = se[a]
  yb = se[b]

  zz = c(0, 1)
  z1 = zz[1] # line grid start
  z2 = zz[2] # line grid end

  ## left
  V1 = rbind(x0 = 0, x1 = 0, y0 = c(z1, ya + l), y1 = c(yb - l, z2))
  ## down
  V2 = rbind(x0 = 0, x1 = 1, y0 = 0, y1 = 0)
  ## right
  V3 = rbind(x0 = 1, x1 = 1, y0 = c(z1, ya + r), y1 = c(yb - r, z2))
  ## top
  V4 = rbind(x0 = 0, x1 = 1, y0 = 1, y1 = 1)

  if(cut_axis == "x") flip = !flip

  if(flip)
  {
    HL = rotate(HL)  # horizontal lines
    RC = swap(RC)    # blank box

    V1 = rotate(V1)  # cut_axis left
    V2 = rotate(V2)  # cut_axis right
    V3 = rotate(V3)  # cut_axis down
    V4 = rotate(V4)  # cut_axis top

    size.axis = swap(size.axis) # size cut_axis; l,b,r,t
    col.axis = swap(col.axis)   # colors cut_axis: l,b,r,t
    lty.axis = swap(lty.axis)   # line type cut_axis: l,b,r,t
  }#end flip

  ## cut lines
  hl = grid::segmentsGrob(x0 = HL[1,], x1 = HL[2,], y0 = HL[3,], y1 = HL[4,],
           gp = grid::gpar(lwd = gap.box[["size"]],
                           col = gap.box[["color"]],
                           lty = gap.box[["linetype"]]))

  ## blank box
  rc = grid::rectGrob(x = RC[1,], y = RC[2,], width = RC[3,], height = RC[4,],
           gp = grid::gpar(fill = gap.box[["fill"]], col = NA), just = "center")

  if(line.axis[["grid.axis"]])
  {
# FIXME: corrigir os tamanhos
  ## x: bottom - y: left
  v1 = grid::segmentsGrob(x0 = V1[1,], x1 = V1[2,], y0 = V1[3,], y1 = V1[4,],
           gp = grid::gpar(lwd = size.axis["l"],
                           lex = 2,
                           col = col.axis["l"],
                           lty = lty.axis["l"],
                           lineend = "square", linejoin="mitre"))
  ## x: left - y: bottom
  v2 = grid::segmentsGrob(x0 = V2[1,], x1 = V2[2,], y0 = V2[3,], y1 = V2[4,],
           gp = grid::gpar(lwd = size.axis["b"],
                           lex = 2,
                           col = col.axis["b"],
                           lty = lty.axis["b"],
                           lineend = "square", linejoin="mitre"))
  ## cut_axis down
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
    v1 = v2 = v3 = v4 = NULL
  }#end if

  ggname("scale-transf", grid::grobTree(v1, v2, v3, v4, rc, hl))

}#end draw_panel_fun

##===========================================
##
##===========================================
ScaleAxisTransf <- ggplot2::ggproto("ScaleAxisTransf", GeomBlank,
                            draw_key = draw_key_blank,
                            draw_panel = draw_panel_fun
                          )#end ggprotp

##===========================================
##
##===========================================
scale_transf_continuous = function(
                     cut_axis = c("y", "x"),
                     cuts = 0,
                     k = 1,
                     gap = 0,
                     name = waiver(),
                     breaks = pretty_transf(cuts, k, gap),
                     minor_breaks = waiver(),
                     n.breaks = NULL,
                     labels = waiver(),
                     limits = NULL,
                     position = position_default(cut_axis),
                     expand = waiver(),
                     oob = scales::censor,
                     na.value = NA_real_,
                     guide = waiver(),
                     sec.axis = waiver(),
                     line.axis = line_axis(),
                     gap.box = gap_box(),
                     ...
){

  cut_axis = match.arg(cut_axis)
  grid.axis = line.axis[["grid.axis"]]

  scale.fun = if(cut_axis == "y") { "scale_y_continuous" } else { "scale_x_continuous" }

  arg.scale = list(trans = transf_trans(cuts, k),
                   breaks = breaks, labels = labels, position = position,
                   minor_breaks = minor_breaks, n.breaks = n.breaks, limits = limits,
                   expand = expand, oob = oob, na.value = na.value, guide = guide,
                   sec.axis = sec.axis, name = name)

  ## theme: remove axis.line and panel border
  arg.theme = list(axis.line = element_blank(), panel.border = element_blank())

  list(ggplot2::layer(mapping = NULL,
                      data = NULL,
                      stat = StatIdentity,
                      geom = ScaleAxisTransf,
                      position = PositionIdentity,
                      inherit.aes = FALSE,#NOTE: verificar
                      check.aes = FALSE,
                      show.legend = FALSE,
                      params = list(cut_axis = cut_axis,
                                    cuts = cuts,
                                    k = k,
                                    gap = gap,
                                    line.axis = line.axis,
                                    gap.box = gap.box,
                                    ...)
                      ),
       do.call(scale.fun, arg.scale),
       ## theme: remove axis.line and panel border
       if(grid.axis) do.call("theme", arg.theme)
  )#end list
}#end scale_transf_continuous


