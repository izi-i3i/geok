#-------------------------------------------
# Author  : Izi
# Project :
# Created :qui 21 out 2021 08:14:15 -03
# License : MIT
# Updated : sáb 17 fev 2024 14:00:24
#-------------------------------------------
outliers = function(
          data,
          value,
          method = c("tukey", "adjbox", "hampel"),
          remove = c("none", "extreme", "outlier"),
          name.classes = NULL,
          title = NULL,
          language = c("pt", "en"),
          digits = 3,
          coef = 1.5,
          coef.extreme = 2 * coef,
          probs = c(.25, .75),
          add.thres = TRUE,
          breaks.x = 4,
          plot = TRUE,
          na.rm = FALSE,
          a = -4,
          b = 3,
          type = 7,
          ...
){#start-function

.CLASS_=.EXTREME_=.OUTLIER_=.obs.=OBS=value_y=NULL

  if(is.vector(data))
  {
      value_name = deparse(substitute(data))
    .DT. = data.table(value_y = data)
  } else {
    value_name = substitute(value)
    value_name = if(!is.call(value_name)) deparse(value_name) else eval(value_name)
    .DT. = if(is.data.table(data)) copy(data) else as.data.table(data)
    setnames(.DT., value_name, "value_y")
  }#end-if

  method = match.arg(method)
  remove = match.arg(remove)
  language = match.arg(language)
  color_class = c("black","black","black","orange")
  fill_class = c("#458B74","#BF3EFF","#FF4040","white")

  switch(language,
      'pt' = {
        lang = c("Sum\u00E1rio: ",
                 "\nDiscrepantes = ",
                 "; Extremos = ",
                 "; Total = ",
                 "\nM\u00E9todo: ",
                 "LIE = ",
                 "LI = ",
                 "Mediana = ",
                 "LS = ",
                 "LSE = ",
                 "observa\u00E7\u00F5es",
                 paste0("valores observados (", value_name, ")"))

        cap_sumario = "\nSum\u00E1rio: m\u00E9dia (desvio padr\u00E3o)[min; max] n"
        names_at = c("LSE", "LS", "Mediana", "LI", "LIE")
        if(is.null(name.classes)) name.classes = c("Regular","Discrepante","Extremo")
        names(color_class) <- c(name.classes,'NA')
        names(fill_class) <- c(name.classes,'NA')
        decimal.mark = ","
      },
      'en' = {
        lang = c("Summary: ",
                 "\nOutliers = ",
                 "; Extreme = ",
                 "; Total = ",
                 "\nMethod: ",
                 "LE = ",
                 "L = ",
                 "Median = ",
                 "U = ",
                 "UE = ",
                 "observations",
                 paste0("observed values (", value_name, ")"))

        cap_sumario = "\nSummary: mean (standard deviation)[min; max] n"
        names_at = c("UE", "U", "Median", "L", "LE")
        if(is.null(name.classes)) name.classes = c("Regular","Outlier","Extreme")
        names(color_class) <- c(name.classes,'NA')
        names(fill_class) <- c(name.classes,'NA')
        decimal.mark = "."
      })

  .classes_ = name.classes

  if(na.rm) .DT. = .DT.[is.finite(value_y)]
  if (.DT.[,anyNA(value_y)])
    warning("Data with NA/NaN/Inf!\n Consider removing using 'na.rm = TRUE'",call. = FALSE)
  .DT.[, .obs. := 1:.N]

  switch(method,
     "tukey" = {
       metodo = "Tukey"
       qnt = .DT.[,quantile(value_y, probs = probs, na.rm = TRUE, names = FALSE, type = type)]
       H = .DT.[, IQR(value_y, na.rm = TRUE, type) * coef]
       L = qnt[1] - H
       U = qnt[2] + H
       .DT.[, .OUTLIER_ := value_y < L | value_y > U]

       HE = .DT.[,IQR(value_y, na.rm = TRUE, type) * coef.extreme]
       LE = qnt[1] - HE
       UE = qnt[2] + HE
       .DT.[, .EXTREME_ := value_y < LE | value_y > UE]
     },
     "hampel" = {
       metodo = "Hampel"
       coef_mad = .DT.[, 2 * coef * mad(value_y, constant = 1, na.rm = TRUE)]
       mediana = .DT.[, median(value_y, na.rm = TRUE)]
       L = mediana - coef_mad
       U = mediana + coef_mad
       .DT.[, .OUTLIER_ := value_y < L | value_y > U]

       LE = mediana - 2 * coef_mad
       UE = mediana + 2 * coef_mad
       .DT.[, .EXTREME_ := value_y < LE | value_y > UE]
     },
     "adjbox" = {
       metodo = if(language == "pt") "Ajuste Boxplot" else "Adjust Boxplot"
       qnt = .DT.[, quantile(value_y, probs = probs, na.rm = TRUE, names = FALSE, type = type)]
       iqr = .DT.[, IQR(value_y, na.rm = TRUE, type)]
       MC = .DT.[, medcouple(value_y, do.reflect = TRUE, na.rm = TRUE)]

       if (MC < 0)
       {
         # Intervalo: [ Q25 - 1.5 exp(-3 * MC) * IQR; Q75 + 1.5 exp(4 * MC) * IQR ]
         L = qnt[1] - coef * exp(-b * MC) * iqr
         U = qnt[2] + coef * exp(-a * MC) * iqr

         LE = qnt[1] - coef.extreme * exp(-b * MC) * iqr
         UE = qnt[2] + coef.extreme * exp(-a * MC) * iqr
       } else {
         # Intervalo: [ Q25 - 1.5 exp(-4 * MC) * IQR; Q75 + 1.5 exp(3 * MC) * IQR ]
         L = qnt[1] - coef * exp(a * MC) * iqr
         U = qnt[2] + coef * exp(b * MC) * iqr

         LE = qnt[1] - coef.extreme * exp(a * MC) * iqr
         UE = qnt[2] + coef.extreme * exp(b * MC) * iqr
       }#end if

       .DT.[, .OUTLIER_ := value_y < L | value_y > U]
       .DT.[, .EXTREME_ :=  value_y < LE | value_y > UE]
   })#end-switch

  mdn = mediana = .DT.[, median(value_y, na.rm = TRUE)]
  lse = UE
  ls = U
  li = L
  lie = LE

  nat = names(add.thres)
  if(any(nat == "")) add.thres = add.thres[nat != ""]

  addthres = rep(TRUE, 5)
  names(addthres) <- names_at

  if(is.null(nat))
  {
    addthres <- rep(add.thres[1], 5)
    names(addthres) <- names_at
  } else {
    addthres[names(add.thres)] <- add.thres
  }

  UL = data.table(lim = names_at, valor = c(UE, U, mdn, L, LE))
  if(language == "en") names(UL) <- c("lim","fence")

  if(!addthres[names_at[1]]) UE = NULL
  if(!addthres[names_at[2]]) U = NULL
  if(!addthres[names_at[3]]) mediana = NULL
  if(!addthres[names_at[4]]) L = NULL
  if(!addthres[names_at[5]]) LE = NULL

  switch(remove,
   "extreme" = {
     .RM. = .DT.[.EXTREME_ == TRUE]
     .DT. = .DT.[.EXTREME_ == FALSE | is.na(.EXTREME_)]
     LE = NULL
     UE = NULL
     hline_up_e = NULL
     hline_low_e = NULL
     breaks_right = sort(c(LE, L, mediana, U, UE))
   },
   "outlier" = {
     .RM. = .DT.[.OUTLIER_ == TRUE]
     .DT. = .DT.[.OUTLIER_ == FALSE | is.na(.OUTLIER_)]
     LE = NULL
     UE = NULL
     hline_up_e = NULL
     hline_low_e = NULL
     breaks_right = sort(c(LE, L, mediana, U, UE))
   },
   "none" = {
     .RM. = NULL
     breaks_right = sort(c(LE, L, mediana, U, UE))
     hline_up_e = list(geom_hline(yintercept = c(UE), colour = "#BF3EFF", linetype = 5))
     hline_low_e = list(geom_hline(yintercept = c(LE), colour = "#BF3EFF", linetype = 5))
   })#end-switch

  .DT.[.OUTLIER_ == TRUE  & .EXTREME_ == TRUE,  .CLASS_ := .classes_[3]]
  .DT.[.OUTLIER_ == TRUE  & .EXTREME_ == FALSE, .CLASS_ := .classes_[2]]
  .DT.[.OUTLIER_ == FALSE & .EXTREME_ == FALSE, .CLASS_ := .classes_[1]]

  setindex(.DT., NULL)
  setindex(.DT., .obs.)

  sumario = summarize(.DT., measure.var = "value_y",
     digits = digits, na.rm = TRUE, language = language,
     symb.body = symbs(minusplus.sign = FALSE,symb.mean = c(e="] n = ")), ...)

  n_normal = .DT.[.CLASS_ == .classes_[1], .N]
  n_discrepante = .DT.[.CLASS_ == .classes_[2], .N]
  n_extremo = .DT.[.CLASS_ == .classes_[3], .N]
  n_total_classes = n_normal + n_extremo + n_discrepante
  n_perc = ((n_extremo + n_discrepante) / n_total_classes) * 100

  obs_min = .DT.[, min(.obs., na.rm = TRUE)]
  obs_max = .DT.[, max(.obs., na.rm = TRUE)]
  na_v = .DT.[is.na(value_y), .obs.]
  .DT.[, .CLASS_ := factor(.CLASS_, unique(.CLASS_))]
  t_class = .DT.[,as.vector(sort(unique(.CLASS_)))]
  n_class = .DT.[,length(unique(.CLASS_))]
  n_na = .DT.[is.na(value_y), .N]

  breaks_x = if(length(breaks.x) < 2) { 
    breaks_x = as.integer(seq(obs_min, obs_max, length.out = breaks.x))
  } else {
    breaks.x
  }

  lg = paste("", gsub(" = ","",lang[6:10]))

  sx_le = format_number(LE, digits, decimal.mark = decimal.mark, suffix = lg[1])
  sx_l  = format_number(L, digits, decimal.mark = decimal.mark, suffix = lg[2])
  sx_m  = format_number(mediana, digits, decimal.mark = decimal.mark, suffix = lg[3])
  sx_u  = format_number(U, digits, decimal.mark = decimal.mark, suffix = lg[4])
  sx_ue = format_number(UE, digits, decimal.mark = decimal.mark, suffix = lg[5])

  labels_right = c(sx_le,sx_l,sx_m,sx_u,sx_ue)

  hline_mediana = list(
      geom_hline(yintercept = mediana, colour = "orange", linewidth = .3, linetype = 1))
  hline_up = list(geom_hline(yintercept = c(U), colour = "#458B74", linetype = 5))
  hline_low = list(geom_hline(yintercept = c(L), colour = "#458B74", linetype = 5))

  point_na = if(n_na)
  {
    list(
      guides(fill = guide_legend(
        override.aes = list(shape = rep(21, n_class),
        size = rep(3, n_class),
        color = color_class[c(t_class,"NA")],
        fill = fill_class[c(t_class,"NA")]
        )),
      ),
      geom_point(data = .DT.[is.na(value_y)], aes(x = .obs., y = mdn),
         shape = 21, fill = "white", color = "orange", inherit.aes = T) )
  }  else {
    list(
      guides(fill = guide_legend(
         override.aes = list(shape = rep(21, n_class),
                             size = rep(3, n_class),
                             color = rep("black", n_class)
                             ))),
      geom_point(data = .DT.[is.na(value_y)], aes(x = .obs., y=mdn),
                 shape = 21, fill = "orange", color = "orange", inherit.aes = F) )
  }#end-if-point_na

  setnames(.DT., c("value_y"), c(value_name))

  dt_names = names(.DT.)
  col_names = grep("outlier|extreme|classes", dt_names, value = TRUE)

  if(length(col_names) > 0)
  {
    nc = suppressWarnings(max(as.integer(unlist(strsplit(col_names,"\\."))),na.rm=TRUE))
    if(is.infinite(nc)) nc = 0
    pnc = paste0(".", nc + 1)
    col_names = col_names[(col_names %in% c("outlier","extreme", "classes"))]
    col_names = paste0(col_names[col_names %in% dt_names], pnc)
  } else {
    col_names = c("outlier","extreme","classes")
  }

  otl = grep("outlier", col_names, value = TRUE)
  if(!length(otl)) otl = "outlier"
  ext = grep("extreme", col_names, value = TRUE)
  if(!length(ext)) ext = "extreme"
  cls = grep("classes", col_names, value = TRUE)
  if(!length(cls)) cls = "classes"

  if(length(.RM.) < 1) .RM. = NULL
  if(!is.null(.RM.))
  {
    setnames(.RM., c("value_y"), c(value_name))
    setnames(.RM., c(".OUTLIER_", ".EXTREME_"), c(otl,ext))
  }
  setnames(.DT., c(".OUTLIER_", ".EXTREME_", '.CLASS_'), c(otl,ext,cls))
  .DT.[, (cls) := factor(eval(as.name(cls)), .classes_)]


  p = ggplot(.DT., aes(x = .obs., y = eval(as.name(value_name)), fill = eval(as.name(cls)))) +
    geom_point(size = 2, shape = 21, color="black", na.rm = TRUE) +
    geom_text(data = .DT.[eval(as.name(ext)) == TRUE], aes(label = .obs.),
              size = 2.7, colour = "black", check_overlap = TRUE, vjust = -1) +
    hline_mediana +
    hline_up +
    hline_low +
    hline_up_e +
    hline_low_e +
    point_na +
    scale_fill_manual(name = "", values = c("#458B74","#BF3EFF","#FF4040"), na.value = "gray10") +
    scale_x_continuous(breaks = breaks_x) +
    scale_y_continuous(guide = guide_axis(check.overlap = TRUE),
                       sec.axis = sec_axis(~ . * 1,
                           guide = guide_axis(check.overlap = TRUE, n.dodge = 1),
                           breaks = breaks_right,
                           labels = labels_right)
                       ) +
    theme(legend.key = element_rect(fill = "transparent"),
          legend.position = "top",
          legend.justification=c(0.0, 0),
          plot.margin = unit(c(5, 5, 5, 5), units = "points"),
          legend.margin = margin(c(t = 0, r = 0, b = 0.0, l = 0), unit = 'cm'),
          legend.text = element_text(size = 11, color = "black"),
          axis.text.y.right = element_text(size = 11, color = "black"),
          axis.text.y.left = element_text(size = 12, color = "black"),
          axis.text.x = element_text(size = 12, color = "black"),
          axis.ticks = element_line(linewidth = 1.7),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size = 13, face = "bold", color="white"),
          strip.background = element_rect(fill="gray50"),
          plot.title = element_text(vjust = 0.0, hjust = 0.0, face = "bold", size = 15),
          plot.subtitle = element_text(vjust = 0.0, hjust = 0.0, size = 12),
          plot.caption = element_text(size = 10, vjust = 0, hjust = 1)
        ) +
    labs(title = title,
         subtitle = paste0(
             lang[1], sumario[[3]][[2]],
             lang[2], n_discrepante,
             lang[3], n_extremo,
             lang[4], n_extremo + n_discrepante, " (", format_number(n_perc, 0, suffix = "%"), ")",
             "\nNA = ", n_na,
             lang[5], metodo),
         caption = paste0(
             cap_sumario
                  ),
         x = lang[11],
         y = lang[12])

  if(plot) plot(p)

  out = list('plot' = p, 'dados' = .DT.[], 'removidos'=.RM., 'lim' = UL, 'sumario' = sumario[[1]])
  if(language == "en") names(out) <- c("plot","data","removed","ul","summary")

  return(out)
}#end function

