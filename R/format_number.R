#-------------------------------------------
# Author  : Izi (izi31416@protonmail.com)
# Project :
# Created : sex 20 jun 2025 15:14:51
# License :
# Updated :
#-------------------------------------------


##===========================================
##
##===========================================
count_digits = function(x)
{
  out = sapply(x,
    function(x) {
      if(is.na(x) | is.infinite(x)) return(0)
      if (abs(x - round(x)) > .Machine$double.eps^0.5) {
        nchar(strsplit(sub('0+$', '', format(x, scientific = F)), ".", fixed = TRUE)[[1]][[2]])
      } else {
        return(0)
      }
    })
  return(out)
}#end count_digits

##===========================================
##
##===========================================
fnumber = function(x,
                    nsmall = NULL,
                    decimal.mark = ",",
                    preffix = NULL,
                    suffix = NULL,
                    p = FALSE,
                    parse_p = FALSE,
                    suffix_p = NULL,
                    ...)
{
  fn = "format"

  comp = is.complex(x)
  if(comp)
  {
    imag = Im(x)
    imag[imag>=0] <- paste0("+",imag[imag>=0])
    x = Re(x)
  }

  s = sign(x)
  x = abs(x)

  if(is.null(nsmall)) nsmall = max(count_digits(x))
  nn = 1/10^nsmall

  #if(is.na(x) | is.infinite(x))
  if(!is.finite(x))
  {
    return(as.character(s*x))
  } else if(x >= nn ) {

    x = round(x * s, nsmall)
    out = do.call(fn,
                  list(x,
                       nsmall = nsmall,
                       decimal.mark = decimal.mark,
                       scientific = FALSE,
                       trim = TRUE,
                       ...))

    if(p & !parse_p) out = paste("p =", out)
    if(p & parse_p) out = paste0("list(~italic(p)", suffix_p, "=='", out, "')")

    if(!is.null(preffix)) out = paste0(preffix, out)
    if(!is.null(suffix)) out = paste0(out, suffix)

  } else if(x == 0) {
    out = "0"
  } else {
    if(s > 0)
    {
      if(parse_p)
      {
        if(p)
        {
          out = paste0("~list(~italic(p)", suffix_p, "~'< ",
                 do.call(fn, list(nn, decimal.mark = decimal.mark,
                        scientific = FALSE, trim = TRUE, ...)), "')")
        } else {
          out = paste0("<",
                 do.call(fn, list(nn, decimal.mark = decimal.mark,
                        scientific = FALSE, trim = TRUE, ...)))
        }#end if

        if(!is.null(preffix)) out = paste0(preffix, out)
        if(!is.null(suffix)) out = paste0(out, suffix)

      } else {

        if(p)
        {
          out = paste0(
                       paste0("p", suffix_p), " < ",
                  do.call(fn, list(nn, decimal.mark = decimal.mark,
                         scientific = FALSE, trim = TRUE, ...)))
        } else {
          out = paste0("<",
                  do.call(fn, list(nn, decimal.mark = decimal.mark,
                         scientific = FALSE, trim = TRUE, ...)))
        }#end if

        if(!is.null(preffix)) out = paste0(preffix, out)
        if(!is.null(suffix)) out = paste0(out, suffix)
      }#end if
    }#end if

    if(s < 0)
    {
      if(p & parse_p)
      {
        if(p)
        {
          out = paste0("list(~italic(p)", suffix_p, "~'< ",
                do.call(fn, list(s*nn, decimal.mark = decimal.mark,
                       scientific = FALSE, trim = TRUE, ...), "')"))
        } else {
          out = paste0("<",
                  do.call(fn, list(s*nn, decimal.mark = decimal.mark,
                         scientific = FALSE, trim = TRUE, ...)))
        }#end if

        if(!is.null(preffix)) out = paste0(preffix, out)
        if(!is.null(suffix)) out = paste0(out, suffix)

      } else {

        if(p)
        {
          out = paste0(
                 paste0("p", suffix_p), " < ",
                  do.call(fn, list(s*nn, decimal.mark = decimal.mark,
                         scientific = FALSE, trim = TRUE, ...)))
        } else {
          out = paste0("<",
                 do.call(fn, list(s*nn, decimal.mark = decimal.mark,
                        scientific = FALSE, trim = TRUE, ...)))
        }#end if

        if(!is.null(preffix)) out = paste0(preffix, out)
        if(!is.null(suffix)) out = paste0(out, suffix)
      }#end if
    }#end if
  }#end if

    if(comp)
    {
      im = paste0(out,imag,"i")
      im[grep("NA", im)] <- "NA"
      return(im)
    }

  return(out)
}#end function fnumber

##===========================================
## FORMAT_NUMBER
##===========================================
format_number = function(x,
                         nsmall = NULL,
                         value = FALSE,
                         decimal.mark = ",",
                         preffix = NULL,
                         suffix = NULL,
                         p = FALSE,
                         parse_p = FALSE,
                         suffix_p = NULL,
                         ...
){
  if(is.null(nsmall)) nsmall = max(count_digits(x))

  if(value) {
    out = sapply(round(x, nsmall), function(x){round(x, nsmall)})
  } else {
    out = sapply(x, fnumber,
                 nsmall = nsmall,
                 decimal.mark = decimal.mark,
                 preffix = preffix,
                 suffix = suffix,
                 p = p,
                 parse_p = parse_p,
                 suffix_p = suffix_p,
                 ...)
  }#end if
  return(out)
}#end_function

