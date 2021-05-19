#' Compute and Visualize Biomarker Ordinal Estimator
#'
#' \code{mbr} calculates the Biomarker Ordinal Estimator (BOE) and visualizes
#' the perceived differences.
#'
#' @param df a data frame with the name of experimental groups or biomonitoring
#'   sites as the first column and the measurement of biomarkers as the
#'   remaining columns.
#'
#' @details The header of the first column can be any character, for example,
#'   'group' or 'site'. The first name appearing in the first column will
#'   determine the control group or the reference site. The other names will be
#'   treatment groups or test sites. The header of the remaining columns will
#'   define the list of biomarkers.
#'
#' @return \code{mbr} returns a list of length 3: \item{\code{iBOE}}{a data
#'   frame with 14 columns:\describe{ \item{\code{treatment}}{treatment groups
#'   or test sites} \item{\code{control}}{control group or reference site}
#'   \item{\code{tm_size}}{the sample size of treatment group}
#'   \item{\code{ct_size}}{the sample size of control group}
#'   \item{\code{biomarker}}{individual biomarker} \item{\code{delta}}{the
#'   Cliff's delta of treatment group} \item{\code{ci.lower}}{the lower bound of
#'   the confidence interval} \item{\code{ci.upper}}{the uppper bound of the
#'   confidence interval} \item{\code{Opt}}{the Biomarker Ordinal Estimator for
#'   individual biomarker (iBOE) in optimistic scenario} \item{\code{Pro}}{the
#'   Biomarker Ordinal Estimator for individual biomarker (iBOE) in probable
#'   scenario} \item{\code{Pes}}{the Biomarker Ordinal Estimator for individual
#'   biomarker (iBOE) in pessimistic scenario} \item{\code{optimistic}}{the
#'   perceived difference in optimistic scenario} \item{\code{probable}}{the
#'   perceived difference in probable scenario} \item{\code{pessimistic}}{the
#'   perceived difference in pessimistic scenario}}} \item{\code{BOE}}{a data
#'   frame with 7 columns:\describe{ \item{\code{treatment}}{treatment groups or
#'   test sites} \item{\code{Opt}}{the Biomarker Ordinal Estimator for combined
#'   biomarker (BOE) in optimistic scenario} \item{\code{Pro}}{the Biomarker
#'   Ordinal Estimator for combined biomarker (BOE) in probable scenario}
#'   \item{\code{Pes}}{the Biomarker Ordinal Estimator for combined biomarker
#'   (BOE) in pessimistic scenario} \item{\code{optimistic}}{the perceived
#'   difference in optimistic scenario} \item{\code{probable}}{the perceived
#'   difference in probable scenario} \item{\code{pessimistic}}{the perceived
#'   difference in pessimistic scenario} }} \item{\code{heatmap}}{a ggplot
#'   object containing 4 heatmaps of the perceived differences}
#'
#' @examples
#' \donttest{mbr(sokolova2019)} #might take more than 5s in some machines
#'
#' @export
mbr <- function(df){
  Opt <- Pro <- Pes <-
    optimistic <- pessimistic <-
    biomarker <- treatment <-
    delta <- ci.lower <- ci.upper  <-
    NULL #no visible binding for global variable

  grs <- unique(df[, 1]) %>% unlist
  ct <- grs[1]
  tms <- grs[-1]
  cell1 <- colnames(df)[1]
  bmks <- colnames(df)[-1]

  message(paste(
    c(
      "Control group or reference site:",
      ct,
      "\nOrdered list of treatment groups or test sites:",
      tms,
      "\nOrdered list of biomarkers:",
      bmks
    ),
    collapse = " "
  ))
  message(
    paste(
      "Methods: \n* Cliff's delta statistic \n* 95% bias-corrected and accelerated (BCa) bootstrap confidence intervals, 1999 samples \n* thresholds: 'No' 0 'Very small' 0.25 'Small' 0.5 'Medium' 0.75 'Large' 1"
    )
  )

  effectsize <- map(bmks, function(x) {
    s3 <-
      df %>% select(!!sym(cell1),!!sym(x)) %>% split(df[cell1])
    map(tms, function(y) {
      vec1 <- s3[[y]][2] %>% unlist %>% na.omit
      vec0 <- s3[[ct]][2] %>% unlist %>% na.omit

      cliffpie(vec1, vec0) %>%
        transmute(
          treatment = y,
          control = ct,
          tm_size = length(vec1),
          ct_size = length(vec0),
          biomarker = x,
          delta,
          ci.lower,
          ci.upper
        )
    }) %>%  rbindlist()
  }) %>%  rbindlist() %>% mutate(treatment = factor(treatment, tms),
                                 biomarker = factor(biomarker, bmks))

  threescenarios <- effectsize %>%
    mutate(
      Opt  = case_when(ci.lower * ci.upper >= 0 ~ pmin(abs(ci.lower), abs(ci.upper)),
                       ci.lower * ci.upper < 0 ~ 0),
      Pro = abs(delta),
      Pes  = pmax(abs(ci.lower), abs(ci.upper))
    )

  iBOE <- threescenarios %>% mutate(
    optimistic  = con2ord(Opt),
    probable  = con2ord(Pro),
    pessimistic  = con2ord(Pes)
  )

  iBOE

  iBOEavg <-
    iBOE %>% group_by(treatment) %>% summarise(Opt = mean(abs(Opt)),
                                                Pro = mean(abs(Pro)),
                                                Pes = mean(abs(Pes)))
  BOE <- iBOEavg %>% mutate(
    optimistic  = con2ord(Opt),
    probable = con2ord(Pro),
    pessimistic  = con2ord(Pes)
  )

  BOE

  BOElong <-
    BOE %>% select(!(Opt:Pes)) %>% gather("scenario", "summary", optimistic:pessimistic, factor_key =
                                            TRUE) %>% mutate(summary  = con2ord(summary))

  BOElong

  graph1 <- heat(iBOE, "biomarker", "treatment", "optimistic")
  graph2 <- heat(iBOE, "biomarker", "treatment", "probable")
  graph3 <- heat(iBOE, "biomarker", "treatment", "pessimistic")
  graph4 <- heat(BOElong, "scenario", "treatment", "summary")

  allgraphs <- plot_grid(graph1+ theme(legend.position="none"),
                         graph2+ theme(legend.position="none"),
                         graph3+ theme(legend.position="none"),
                         graph4+ theme(legend.position="none"),
                         labels = "AUTO", align = 'v', label_fontfamily = 'sans')

  legend <- get_legend(
    graph1 +
      labs(fill="Difference") +
      theme(legend.justification = "center")
  )

  heatmap <- plot_grid(allgraphs, legend, ncol = 1, rel_heights = c(1, .1))

  list(iBOE=iBOE, BOE=BOE, heatmap=heatmap)
}
