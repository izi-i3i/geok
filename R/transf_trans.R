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
breaks_transf = function(cuts = 0, k = 1, w = 0, n = 7)
{
  function(x, ...)
  {
    ge = gap_end(cuts, w)
    xr = range(x, na.rm = TRUE)
    bk = scales::trans_breaks(transf(cuts,k), transf_inv(cuts,k), n)(xr)
    nb = c(cuts, ge, bk)
    nb = sort(unique(nb))
    return(nb)
  }
}#end breaks_transf

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
  }
}

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
  }
}

##===========================================
##
##===========================================
transf_trans = function(g = 0, k = 1){
  scales::trans_new(
      name = "transf",
      trans = transf(g, k),
      breaks = breaks_transf(g,k),
      inverse = transf_inv(g, k),
      minor_breaks = scales::regular_minor_breaks(),
      format = scales::label_number(),
      domain = c(-Inf, Inf)
      )
}#end transf_trans

