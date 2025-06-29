#-------------------------------------------
# Author  : Izi
# Project :
# Created : 8 de set de 2018
# License : MIT
# Updated : sáb 07 nov 2020 23:56:26 -03
#-------------------------------------------

##========================================
## 
##========================================
standard_error = function(x, na.rm = FALSE)
{
  if(isTRUE(na.rm)) x = x[!is.na(x)]
  n = length(x)
  sd(x) / sqrt(n)
}#end standard_error

##===========================================
##
##===========================================
indots <- function(arg, default, ...)
{
  dots = list(...)
  out = unlist(dots[arg])
  if(is.null(out)) out <- default
  out
}

##===========================================
##
##===========================================
match_dots = function(args, fn, arg.rm = "")
{
  fa = formalArgs(args(fn))
  rmv = match(arg.rm, names(args))
  fa = fa[!fa%in%arg.rm]
  ag = match(fa, names(args))
  args[ag[!is.na(ag)]]
}


##===========================================
##
##===========================================
Winsorize = function (x,
                      minval = NULL,
                      maxval = NULL,
                      probs = c(0.05, 0.95),
                      na.rm = FALSE,
                      type = 7)
{
    if (is.null(minval) || is.null(maxval)) {
        xq <- quantile(x = x, probs = probs, na.rm = na.rm, type = type)
        if (is.null(minval))
            minval <- xq[1L]
        if (is.null(maxval))
            maxval <- xq[2L]
    }
    x[x < minval] <- minval
    x[x > maxval] <- maxval
    return(x)
}

##===========================================
##
##===========================================
winvar <- function(x, trim.ci) {
  if(any(is.na(x))) return(NA_real_)
  n <- length(x)
  trn <- floor(trim.ci * n) + 1
  minval <- sort(x, partial = trn)[trn]
  maxval <- sort(x, partial = max((n - trn + 1), 1))[max((n - trn + 1), 1)]
  winvar <- var(Winsorize(x, minval = minval, maxval = maxval))
  DF <- n - 2 * (trn - 1) - 1
  return(c(var = winvar, DF = DF))
}

##===========================================
##  FRQUENCIA
##===========================================
freq_tab = function(x, na.rm = FALSE, ...)
{
  .=f=p=NULL
  h = substitute(x)
  group = as.character(substitute(h))

  if(isTRUE(na.rm))
    FQ = data.table()[,c(group) := x][!is.na(x)][, .(f = .N), by = group]
  else
    FQ = data.table()[,c(group) := x][, .(f = .N), by = group]

  setorder(FQ, -f)

  FQ[, rf := f / sum(f)][
     , p := paste0(round(rf*100),"%")][
     , paste0(group,"(f)rf") := paste0(eval(h),"(",f,")",p)]

  return(FQ[])
}#end freq_tab

##===========================================
## KURTOSIS
##===========================================
kurtosis = function(x, kurtosis.type = 2, na.rm = FALSE)
{
  #if(na.rm) x = na.omit(x)
  if(isTRUE(na.rm)) x = x[!is.na(x)]
  n <- length(x)
  if(kurtosis.type == 1) out = n * sum((x - mean(x))^4) / (sum((x - mean(x))^2)^2)
  if(kurtosis.type == 2) out = (1/n) * (sum( ((x - mean(x)) / sd(x))^4)) - 3

  return(out)
}#end kurtosis

##===========================================
## SKEWNESS
##===========================================
skewness = function(x, skewness.type = 2, na.rm = FALSE)
{
  if(isTRUE(na.rm)) x = x[!is.na(x)]
  n = length(x)
  if(skewness.type == 1)
    out = (sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^(3/2)
  if(skewness.type == 2)
    out = (1/n) * sum( ((x - mean(x)) / sd(x))^3 )

  return(out)
}#end skewness

##===========================================
## TRIMMED MEAN
##===========================================
trimmed_mean = function (x, trim = .1, na.rm = FALSE) {
  sapply(trim, FUN = mean, x = x, na.rm = na.rm, USE.NAMES = FALSE)
}

##========================================
##
##========================================
amplitude = function(x, na.rm=FALSE) return(max(x, na.rm = na.rm) - min(x, na.rm = na.rm))

##========================================
##
##========================================
coef_var = function(x, na.rm = FALSE)
  return(sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm))

##========================================
##
##========================================
missing_count = function(x, missing.val = NA, action = c("<=", "=", ">="))
{
  action <- match.arg(action)

  switch(action,
     "=" = {
       missing.val = sum(x %in% missing.val)
     },
     "<=" = {
       if(length(missing.val) == 1 & any(is.na(missing.val))) {
         missing.val = sum(is.na(x))
       } else {
         missing.val =
           sum(is.na(x)) + sum(x <= max(missing.val, na.rm = TRUE), na.rm = TRUE)
       }
     },
     ">=" = {
        if(length(missing.val) == 1 & any(is.na(missing.val))) {
          missing.val = sum(is.na(x))
        } else {
          missing.val =
            sum(is.na(x)) + sum(x >= max(missing.val, na.rm = TRUE), na.rm = TRUE)
        }
     }
  )#end switch
  return(missing.val)
}#end missing_count

##===========================================
##
##===========================================
missing_rm = function(x, missing.val = NULL, action = c("<=", "=", ">="))
{
  if(is.null(missing.val)) return(FALSE)

  action <- match.arg(action)

  out = switch(action,
     "=" = {
            x %in% missing.val
     },
     "<=" = {
       if(length(missing.val) == 1 & any(is.na(missing.val))) {
         missing.val = is.na(x)
       } else {
         x <= max(missing.val, na.rm = TRUE)
       }
     },
     ">=" = {
       if(length(missing.val) == 1 & any(is.na(missing.val))) {
         missing.val = is.na(x)
       } else {
         x >= max(missing.val, na.rm = TRUE)
       }
     },
  )#end switch
  return(out)
}#end missing_count

##===========================================
##
##===========================================
interval_range = function(x,
                          digits = 2,
                          decimal.mark = ",",
                          na.rm = FALSE
){
  if(isTRUE(na.rm)) x = x[!is.na(x)]
  if(any(is.na(x))) return(NA)

  if(is.numeric(x))
  {
    xmin = format_number(min(x),
              nsmall = digits, decimal.mark = decimal.mark)
    xmax = format_number(max(x),
              nsmall = digits, decimal.mark = decimal.mark)
    res = if(is.na(xmin) & is.na(xmax))
    {
      NA_character_
    } else if(xmin == xmax){
      xmin
    } else {
      paste0(xmin,"/",xmax)
    }

  } else {
    fac = is.factor(x)
    if(fac) warning("In interval_range, vector is factor: using levels", call. = FALSE)
    xmin = if(fac) min(levels(x)) else min(x)
    xmax = if(fac) max(levels(x)) else max(x)
    res = if(xmin == xmax) xmin else paste0(xmin,"/",xmax)
  }
  return(res)
}#end interval_range

##===========================================
##
##===========================================
coef_params = function(x, default = NULL)
{
  sx = as.character(substitute(x))

  if(is.null(default))
  {
    default = c(Total.N = "Total.N", Missing = "Missing", Valid.N="Valid.N",
                Min = "Min", Q1 = "Q1", Median = "Median",
                SAM = "SAM", TM = "TM", Q3 = "Q3", Max = "Max", MAD = "MAD",
                SD = "SD", ED = "ED", CV = "CV", 'IQR' = "IQR", Range = "Range",
                Skewness = "Skewness", Kurtosis = "Kurtosis")
  }

  if(is.null(names(default))) names(default) <- default
  if(is.null(names(x))) warning("vector elements must be named")
  if(is.null(x) | is.null(names(x))) return(list('changed'=default, 'vector'=default))

  if(class(x) != class(default))
  {
    if(is.numeric(default))
    {
      xnames = names(x)
      x = suppressWarnings(as.numeric(x))
      names(x) = xnames
      x = x[!is.na(x)]
      warning("vector coerced to numeric")
    }
  }

  non = x[names(x)==""]
  x_names = x[names(x)!=""]

  par_diff = names(x_names)[!(names(x_names) %in% names(default))]

  valid_names = x_names[!(x_names %in% x_names[par_diff])]

  if(any(duplicated(names(valid_names)))) warning("duplicated names")
  valid_names = valid_names[!duplicated(names(valid_names))]

  ign = paste(paste0(non, collapse = ", "),
              paste0(par_diff, collapse = ", "), collapse=", ")
  if(length(par_diff)) warning(sprintf("In %s, ignored: %s",sx, ign), call. = F)

  parNames = c(valid_names, default)
  parNames = parNames[!duplicated(names(parNames))]
  vec = parNames[names(default)]

  return(list('changed'=valid_names, 'vector'=vec))
}

##===========================================
##
##===========================================
quantile2 = function(x, probs = c(0.5), quantile.type = 7, na.rm = FALSE)
  quantile(x, probs = probs, type = quantile.type, na.rm = na.rm, names = FALSE)

iqr = function(x, na.rm = FALSE, quantile.type = 7)
  IQR(x, na.rm = na.rm, type = quantile.type)

##===========================================
##
##===========================================
symbs = function(symb.mean = NULL,
                 symb.median = NULL,
                 symb.five = NULL,
                 symb.custom = NULL,
                 minusplus.sign = FALSE,
                 ...
){
  smean =
    if(minusplus.sign)
      c(a = "", b = " \u00b1 ", c = " (", d = " - ", e = ") ", f = "")
    else
      c(a = "", b = " (", c = ") [", d = "; ", e = "] ", f = "")
  smean[names(symb.mean)] = symb.mean

#   smedian = c(a="", b=" [", c="] (", d=" \u2500 ", e=") ", f="")
  smedian = c(a="", b=" (", c=") [", d="; ", e="] ", f="")
  smedian[names(symb.median)] = symb.median

  sfive = c(a="", b=" | ", c=" | ", d=" | ", e=" | ", f=" | ")
  sfive[names(symb.five)] = symb.five

  scustom = c(a="", b=" | ", c=" | ", d=" | ", e=" | ", f="")
  scustom[names(symb.custom)] = symb.custom

  list(symb.mean=smean, symb.median=smedian, symb.five=sfive, symb.custom=scustom)
}

##===========================================
##
##===========================================
ad_test = function (x)
{
    DNAME <- deparse(substitute(x))
    x <- sort(x[complete.cases(x)])
    n <- length(x)

    if (n < 8) stop("sample size must be greater than 7")

    logp1 <- pnorm((x - mean(x))/sd(x), log.p = TRUE)
    logp2 <- pnorm(-(x - mean(x))/sd(x), log.p = TRUE)
    h <- (2 * seq(1:n) - 1) * (logp1 + rev(logp2))
    A <- -n - mean(h)
    AA <- (1 + 0.75/n + 2.25/n^2) * A

    if (AA < 0.2) {
        pval <- 1 - exp(-13.436 + 101.14 * AA - 223.73 * AA^2)
    }
    else if (AA < 0.34) {
        pval <- 1 - exp(-8.318 + 42.796 * AA - 59.938 * AA^2)
    }
    else if (AA < 0.6) {
        pval <- exp(0.9177 - 4.279 * AA - 1.38 * AA^2)
    }
    else if (AA < 10) {
        pval <- exp(1.2937 - 5.709 * AA + 0.0186 * AA^2)
    }
    else pval <- 3.7e-24
    RVAL <- list(statistic = c(A = A),
                 p.value = pval,
                 method = "Anderson-Darling normality test",
                 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}

##===========================================
##
##===========================================
normality_test = function(x,
            method.normality = c( "shapiro.wilk", "kolmogorov.smirnov", "anderson.darling"),
            na.rm = FALSE,
            ...
){
  if(na.rm) x = x[!is.na(x)]

  method.normality = match.arg(method.normality)

  out = switch(method.normality,
         shapiro.wilk = {
                 tryCatch({
                   shapiro.test(x)$p.value
                 }, error = function(e) {
                   warning('shapiro.wilk test: sample size must be between 3 and 5000',
                           call.=FALSE)
                   return(NA_real_)  } )
               },
         kolmogorov.smirnov = {
                 withCallingHandlers({
                   ks.test(x,
                           y = 'pnorm',
                           mean(x), sd(x),
                           alternative = "two.sided",
                           exact = NULL,
                           simulate.p.value = FALSE,
                           B = 2000)$p.value

                 }, warning = function(cond) {
                   txt <- conditionMessage(cond)
                   ## do with txt what you will, e.g.,
                   ## logwarn(txt, logger="some_log_file.log")
                   warning(txt, call. = F)
                   ## signal that the warning has been handled
                   invokeRestart("muffleWarning")
                 })
               },
         anderson.darling = {
                 tryCatch({
                   ad_test(x)$p.value
                 }, error = function(e) {
                   warning('ad_test: sample size must be greater than 7',
                           call.=FALSE)
                   return(NA_real_)  } )
               }
  )#end-switch
  return(out)
}

##===========================================
## SUMARIO
##===========================================
summarize = function(x,
                     measure.var,
                     group.by = NULL,
                     interval = NULL,
                     params = NULL,
                     params.names = NULL,
                     language = c("pt", "en"),
                     params.digits = NULL,
                     variable.name = "Params",
                     missing.val = NA,
                     missing.rm = FALSE,
                     decimal.mark = "auto",
                     digits = 3,
                     five.numbers = TRUE,
                     fun = NULL,
                     custom5numbers = NULL,
                     sample.name = NULL,
                     sample.col = FALSE,
                     symb.header = symbs(),
                     symb.body = symbs(),
                     ...
){
  .=Missing=Total.N=Valid.N=id=m_rm=NULL

  dots <- list(...)
  n_probs = length(dots$probs)
  n_trim = length(dots$trim)

  language = match.arg(language)

  if(language == "pt") if(decimal.mark == "auto") decimal.mark = ","
  if(language == "en") if(decimal.mark == "auto") decimal.mark = "."

  ##===============-----------===============##
  if(is.vector(x) | is.name(x))
  {
    if(is.name(x))
    {
      mvar = as.character(x)
    } else {
      mvar = as.character(substitute(x))
    }

    measure.var = str2lang(mvar)
    DT = data.table()[,(mvar) := eval(x)]

    if(!is.null(interval))
    {
      warning(sprintf("In interval, ignored: %s", interval), call. = FALSE)
      interval = NULL
    }#endif
  } else {
    if(!is.character(measure.var))
      stop("In measure.var: only characters", call. = FALSE)

    mvar = measure.var
    measure.var = str2lang(measure.var)
    if(!is.null(interval)) group_parse = parse(text=interval)
    setDT(x)
    DT = copy(x)
  }#endif

  if(missing.rm)
  {
    DT[, m_rm := do.call("missing_rm"
                         , c(list(x=eval(measure.var), missing.val=missing.val)
                         , match_dots(dots, missing_rm)))]
    DT = DT[m_rm == isTRUE(m_rm)]
    DT[, m_rm := NULL]
  }

  DT = DT[,.SD, .SDcols = unique(c(group.by, mvar, interval))]

  dt_names = names(copy(DT))

  # Total.N  - Total samples
  # Missing  - Missing values
  # Valid.N  - N samples
  # Min      - Minimum
  # Max      - Maximum
  # Median   - Median
  # SAM      - Simple Arithmetic Mean
  # TM10     - Trimmed Mean 10%
  # MAD      - Median Absolute Deviation
  # SD       - Standard Deviation
  # SE       - Standard Error
  # CV       - Coefficient of Variation
  # Q25      - First quartile 25%
  # Q75      - Third quartile 75%
  # IQR      - Interquartile Range
  # Range    - Range
  # Skewness - Measure of asymmetry
  # Kurtosis - Measure of the tailedness
  # Normality - Normality test

  edn = c("Total.N",
          "Missing",
          "Valid.N",
          "Min",
          "Max",
          "Median",
          "SAM",
          "TM10",
          "MAD",
          "SD",
          "SE",
          "CV",
          "Q25",
          "Q75",
          "IQR",
          "Range",
          "Skewness",
          "Kurtosis",
          "Normality"
  )

  names(edn) = edn
  equal_names = edn[(edn %in% dt_names)]
  paste_names = paste0(equal_names, ".")
  names(paste_names) = equal_names
  edn[equal_names] = paste_names

  DT[, c(edn) := list(
        .N,
        do.call("missing_count"
                , c(list(x=eval(measure.var), missing.val=missing.val)
                    , match_dots(dots, missing_count))),
        NA, # Valid.N
        do.call("min"
                , c(list(x=eval(measure.var)), match_dots(dots, min))),
        do.call("max"
                , c(list(x=eval(measure.var)), match_dots(dots, max))),
        do.call("median"
                , c(list(x=eval(measure.var)), match_dots(dots, median))),
        do.call("mean.default"
                , c(list(x=eval(measure.var), trim = 0)
                    , match_dots(dots, mean.default, arg.rm="trim"))),
        do.call("mean.default"
                , c(list(x=eval(measure.var), trim = 0.1)
                    , match_dots(dots, mean.default, arg.rm="trim"))),
        do.call("mad"
                , c(list(x=eval(measure.var)), match_dots(dots, mad))),
        do.call("sd"
                , c(list(eval(measure.var)), match_dots(dots, sd))),
        do.call("standard_error"
                , c(list(x=eval(measure.var)), match_dots(dots, standard_error))),
        do.call("coef_var"
                , c(list(x=eval(measure.var)), match_dots(dots, coef_var))),

        tryCatch({
          do.call('quantile2'
                  , c(list(x=eval(measure.var), probs = .25)
                      , match_dots(dots, quantile2, arg.rm=c("probs")) ))
                   }, error = function(e) { NA_real_  } ),

        tryCatch({
          do.call('quantile2'
                  , c(list(x=eval(measure.var), probs=.75)
                      ,match_dots(dots, quantile2, arg.rm=c("probs")) ))
                   }, error = function(e) { NA_real_ } ),

        tryCatch({
          do.call("iqr", c(list(x=eval(measure.var))
                           , match_dots(dots, iqr, arg.rm=c("probs")) ))
                   }, error = function(e) { NA_real_  } ),

        do.call("amplitude"
                , c(list(x=eval(measure.var)), match_dots(dots, amplitude))),
        do.call("skewness"
                , c(list(x=eval(measure.var)), match_dots(dots, skewness))),
        do.call("kurtosis"
                , c(list(x=eval(measure.var)), match_dots(dots, kurtosis))),

        do.call("normality_test"
                , c(list(x=eval(measure.var))
                    , match_dots(dots, normality_test, arg.rm="")))

        ), by = group.by]

  DT[, Valid.N := Total.N - Missing]

  # =========================================================================
  qdots = match_dots(dots, quantile2)
  qprob  = qdots[names(qdots)=="probs"]

  # PROBS - quantile
  # =========================================================================
  if(!is.null(dots$probs))
  {
    for(i in 1:n_probs)
      DT[,(paste0("Q",dots$probs[i]*100)) := .(
        tryCatch({
             do.call("quantile2"
                     , c(list(x=eval(measure.var), probs = qprob[[1]][i])
                         , match_dots(dots, quantile2, arg.rm=c("probs"))))
                 }, error = function(e) { return(NA_real_) })
        ), by = group.by]
  }

  # TRIM - mean.default
  # =========================================================================
  trm = match_dots(dots, mean.default)
  if("trim"%in%names(trm))
  {
    for(i in 1:n_trim)
      DT[,(paste0("TM",dots$trim[i]*100)) := .(
         do.call("mean.default"
                 , c(list(x=eval(measure.var), trim = trm[[1]][i])
                     , match_dots(dots, mean.default, arg.rm=c("trim"))
              ))), by = group.by]
  }

  r = c("trim.ci", "trim", "probs")
  if(!is.null(fun))
  {
    for(i in 1:length(fun))
    {
      fn = str2lang(fun[i])
      fun_name = gsub(".*::|:::", "", fun[i])
      DT[,c(fun_name) := .(
        do.call(eval(fn), c(list(x=eval(measure.var))
            , match_dots(dots, eval(fn), arg.rm = r)
            ))), by = group.by]
    }
  }

  # =========================================================================
  dt_new_names = names(copy(DT))
  names(dt_new_names) = dt_new_names
  parametros = c(dt_new_names[!(dt_new_names %in% dt_names)])

  param_ft = paste0(parametros,".",1)
  dig_param = rep(digits, length(param_ft))
  names(dig_param) = parametros
  dig_param[c(1:3)] <- 0

  digVec = c("Interval" = 0, dig_param)

  dig_diff = names(params.digits)[!(names(params.digits) %in% names(digVec))]

  if(length(dig_diff))
    warning(sprintf("In params.digits: ignored '%s'",
          paste(dig_diff, collapse = ", ")), call. = FALSE)

  digPar = c(params.digits, digVec)
  digPar = digPar[!duplicated(names(digPar))]

  # =========================================================================
  interval_col = NULL
  if(!is.null(interval))
  {
    interval_col = paste0(interval,1:length(interval))
    for(i in 1:length(interval_col))
    {
      DT[,interval_col[i] :=
         interval_range(eval(str2lang(interval[i])),
          digits = digPar["Interval"], decimal.mark = decimal.mark, na.rm = T),
            by = group.by]
    }
  }

  # NUM
  # =========================================================================
  if(!is.null(interval))
  {
    NUM = unique(DT[,.SD, .SDcols = c(group.by, interval_col, parametros)])
    setnames(NUM, interval_col, interval)
  } else {
    NUM = unique(DT[,.SD, .SDcols = c(group.by, parametros)])
  }
  # id auxiliar
  NUM[,id := 1:.N]
  # cabeça NUM
  HEAD = NUM[,.SD, .SDcols = !parametros]

  # =========================================================================
  for(i in 1:length(param_ft))
  {
    NUM[, c(param_ft[i]) := format_number(eval(str2lang(parametros[i])),
                digPar[parametros[i]], decimal.mark = decimal.mark)
    ,by=group.by]
  }

  # =========================================================================
  NF = NUM[,.SD, .SDcols = param_ft]
  NUM = NUM[,.SD, .SDcols = !c(param_ft),by=group.by]
  setnames(NF, parametros)
  NF[,id := 1:.N]

  # =========================================================================
  SMT = HEAD[NF,on = "id"]
  SMT = SMT[, lapply(.SD, as.character)]
  NUM[,id := NULL]

  # =========================================================================
  if(is.null(group.by))
  {
    FT = melt(SMT,
              id.vars = c("id"),
              variable.name = variable.name,
              value.factor = FALSE,
              value.name=as.character(measure.var))

    FT[,id := NULL]

  } else {

    FTmelt = melt(SMT,
                  id.vars = c(group.by),
                  value.name = "value",
                  variable.name = variable.name)

    args_dcast = paste("...", "~", paste(c(group.by), collapse = "+"))
    FT = dcast(FTmelt, args_dcast, value.variable = "value")
  }#endif

  # =========================================================================
  names(parametros) = parametros
  if(is.null(params)) params = parametros
  names(params) = params
  old_params = params
  params = coef_params(params, parametros)[[1]]

  if(language == "pt")
  {
    params_names = c(Total.N="N.Total",
                     Missing="Valor.Faltante",
                     Valid.N="N.V\u00E1lido",
                     Min="M\u00Ednimo",
                     Max="M\u00E1ximo",
                     Median="Mediana",
                     SAM="M\u00E9dia",
                     TM10="MT10",
                     MAD="DAM",
                     SD="Desvio.Padr\u00E3o",
                     SE="Erro.Padr\u00E3o",
                     CV="CV",
                     Q25="Q25",
                     Q75="Q75",
                     IQR="IIQ",
                     Range="Amplitude",
                     Skewness="Assimetria",
                     Kurtosis="Curtose",
                     Normality='Normalidade'
    )

    if(!is.null(params.names))
    {
      new_params = params[!(names(params)%in%names(params_names))]
      params_names =  c(params_names, new_params)
      params.names = coef_params(params.names, params_names)[[2]]
    } else {
      params.names = params_names
    }
  }

  params_ft = if(!is.null(interval)) c(interval, params) else params
  setkeyv(FT, variable.name)
  FT = FT[params_ft]

  names_num = copy(names(NUM))
  names(names_num) = names_num
  params_num = names_num[!(names_num%in%parametros)]
  NUM = NUM[,.SD, .SDcols = c(params_num, params)]

  if(!is.null(params.names))
  {
    names_num = names_num[!names_num%in%params.names]
    dp = duplicated(c(names_num, params.names))
    pp = any(params%in%names(params.names))

    if(any(dp) & pp)
    {
      dup = c(names_num, params.names)[dp]
      warning(sprintf("In 'params.names', duplicate names not changed: %s",
                      paste0(dup, collapse = ", ")), call. = FALSE)
      dif_dup = params.names[!params.names%in%dup]
      dup = names(dup)
      params.names = c(dup,dif_dup)
    }

    params.names = coef_params(params.names, params)[[2]]

    if(length(params.names))
    {
      setnames(NUM, names(params.names), params.names)

      for(i in 1:length(params.names))
        FT[, c(variable.name) := gsub(pattern = names(params.names[i]),
                                      replacement = params.names[i],
                                      x = eval(parse(text=variable.name)),
                                      fixed = F, ignore.case = F
                                      )]
    }
  }

  if(is.null(params.names)) params.names = params

  # FIVE_NUMBERS
  # =========================================================================
  five_numbers = function(fn, mean.sd = TRUE, symb.header, symb.body, sample.col)
  {
    par.names = paste0("`", fn, "`")

    p1 = str2lang(par.names[1])
    p2 = str2lang(par.names[2])
    p3 = str2lang(par.names[3])
    p4 = str2lang(par.names[4])
    p5 = str2lang(par.names[5])

    P5 = NUM[, fn[5],with=F]

    is_int_p5 = P5[,is.integer(eval(p5))]
    dig5 = if(is_int_p5) 0 else digits
    P5[,(fn[5]) := format_number(eval(p5),
                      nsmall=dig5, decimal.mark=decimal.mark)]

    if(is.null(group.by)) group.by = 1
    A = NUM[,.SD, .SDcols = group.by]#!is.numeric]

    def = names(fn)
    if(is.null(def) & language == "en") def = fn
    if(is.null(def) & language == "pt")
    {
      pn = params_names[params_names %in% fn]
      def = names(pn)
    }

    B = NUM[, list(
      ed = paste0(
            symb.body[1],
            format_number(eval(p1),
               nsmall=digPar[def[1]], decimal.mark = decimal.mark),
            symb.body[2],
            format_number(eval(p2),
               nsmall=digPar[def[2]], decimal.mark=decimal.mark),
            symb.body[3],
            format_number(eval(p3),
               nsmall=digPar[def[3]], decimal.mark=decimal.mark),
            symb.body[4],
            format_number(eval(p4),
               nsmall=digPar[def[4]], decimal.mark=decimal.mark),
            symb.body[5],
            if(!sample.col) P5[,eval(p5)],
            symb.body[6]
          ))]

     SMY = if(sample.col) cbind(A,P5,B) else cbind(A,B)

     if(is.null(sample.name))
     {
       sample.name = paste0(
                         symb.header[1],
                         fn[1], symb.header[2],
                         fn[2], symb.header[3],
                         fn[3], symb.header[4],
                         fn[4], symb.header[5],
                         if(!sample.col) fn[5], symb.header[6])
     }
     setnames(SMY, "ed", sample.name)
     return(SMY)
  }#end five_numbers

  mean_def = edn[c("SAM", "SD", "Min", "Max", "Valid.N")]
  median_def = edn[c("Median", "MAD", "Min", "Max", "Valid.N")]
  five_def = edn[c("Min", "Q25", "Median", "Q75", "Max")]

  mean5 = params.names[mean_def]
  median5 = params.names[median_def]
  fiven = params.names[five_def]

  MEAN5 =
    if( all(mean5 %in% params.names))
      five_numbers(fn = mean5,
                   mean.sd = TRUE,
                   symb.header = symb.header[["symb.mean"]],
                   symb.body = symb.body[["symb.mean"]],
                   sample.col = sample.col)
    else NULL

  MEDIAN5 =
    if( all(median5 %in% params.names))
      five_numbers(fn = median5,
                   mean.sd = FALSE,
                   symb.header = symb.header[["symb.median"]],
                   symb.body = symb.body[["symb.median"]],
                   sample.col = sample.col)
    else NULL

  FIVEN =
    if( all(fiven %in% params.names))
      five_numbers(fn = fiven,
                   mean.sd = FALSE,
                   symb.header = symb.header[["symb.five"]],
                   symb.body = symb.body[["symb.five"]],
                   sample.col = FALSE)
    else NULL

  CUSTOM5 = NULL

  if(!is.null(custom5numbers))
  {
    if(all(custom5numbers %in% params.names))
    {
      CUSTOM5 = five_numbers(custom5numbers,
                             mean.sd = FALSE,
                             symb.header = symb.header[["symb.custom"]],
                             symb.body = symb.body[["symb.custom"]],
                             sample.col=sample.col)
    } else {
      cust_dif = custom5numbers[!custom5numbers %in% params.names]
      warning(
        sprintf("In custom5numbers must contain 5 valid names. Coerced to NULL.\nIgnored: %s",
           paste(cust_dif,collapse = ", ")), call. = FALSE)
      CUSTOM5 = NULL
    }
  }

  if(five.numbers)
  {
    out = list('numeric' = NUM,
               'formated' = FT,
               'mean5numbers' = MEAN5,
               'median5numbers' = MEDIAN5,
               'fiveNumbers' = FIVEN
    )

    if(!is.null(custom5numbers)) out = c(out, 'custom5numbers' = list(CUSTOM5))
  } else {
    out = list('numeric' = NUM, 'formated' = FT)
  }

  return(out)
}#end summarize
