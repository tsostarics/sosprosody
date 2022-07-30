#' Nest two textgrid tiers
#'
#' Given two dataframes for two different tiers, will return a single
#' dataframe with the dataframes merged together. This assumes that the
#' second tier/dataframe is nested within the first, for example a phones
#' tier nested within a words tier.
#'
#' @param tg_df_list List of textgrid dataframes, the output of
#' `textgrid_to_dataframes`
#' @param nesting String of the form `"X/Y"`, read as "Y nested within X".
#' Defaults to `"words/phones"`, intended for use with montreal forced aligner
#' output.
#' @param filter_silence Character vector of silence tokens you don't want to
#' keep. Defaults to `""`, but some people may want to use `c("", "<sil>")`
#' depending on how their textgrids are set up.
#' @param .as_singular Whether to remove the "s" from "words" and "phones" in
#' column name outpouts. Eg `word_start` instead of `words_start` for the
#' interval's starting timestamp.
#'
#' @return Merged dataframe
#' @export
#' @importFrom rlang `!!` `:=`
#' @importFrom dplyr sym
nest_tiers <- function(tg_df_list,
                       nesting = "words/phones",
                       filter_silence = "",
                       .as_singular = TRUE) {
  # Note: ONLY IMPLEMENTED FOR 2 INTERVAL TIERS! NO MORE! NO POINT TIERS YET!
  nesting <-  strsplit(nesting, "/",perl = TRUE)[[1]]
  stopifnot(all(nesting %in% names(tg_df_list)))

  # Rename interval_start/end, add index, filter out silent intervals
  for (tier in names(tg_df_list)) {
    tier_df <- dplyr::filter(tg_df_list[[tier]], !.data$label %in% filter_silence)
    tiername <- ifelse(.as_singular, gsub("s$", "", tier, perl = TRUE), tier)

    tier_df[[paste0(tiername, "_i")]] <- seq.int(length.out = nrow(tier_df))
    tier_colnames <- colnames(tier_df)

    tier_colnames <- gsub("interval", tiername, tier_colnames)
    tier_colnames[tier_colnames == "label"] <- paste0(tiername, "_label")

    colnames(tier_df) <- tier_colnames
    tg_df_list[[tier]] <- tier_df
  }

  # below is only for 2 tiers, can be extended for more later but not right now

  # Get column names
  .cols1 <- colnames(tg_df_list[[nesting[1L]]])
  .cols2 <- colnames(tg_df_list[[nesting[2L]]])

  idx_1 <- .cols1[grepl("_i$", .cols1, perl = TRUE)]
  idx_2 <- .cols1[grepl("_i$", .cols2, perl = TRUE)]
  int1start <- .cols1[grepl("_start$", .cols1, perl = TRUE)]
  int1end <- .cols1[grepl("_end$", .cols1, perl = TRUE)]
  int2start <- .cols2[grepl("_start$", .cols2, perl = TRUE)]
  int2end <- .cols2[grepl("_end$", .cols2, perl = TRUE)]

  tg_df_list[[nesting[2L]]] <-
    tg_df_list[[nesting[2L]]] |>
    dplyr::group_by(!!sym(idx_2) := dplyr::row_number()) |>
    dplyr::mutate(
      word_i = tg_df_list[[nesting[1L]]][!!sym(int2start) >=
                                           tg_df_list[[nesting[1L]]][[int1start]]
                                         & !!sym(int2end) <=
                                           tg_df_list[[nesting[1L]]][[int1end]],
                                         idx_1]
    )
  tg_df_list[[nesting[2L]]][['file']] <- NULL
  tg_df_list[[nesting[1L]]][['file']] <- gsub(".TextGrid$", "", tg_df_list[[nesting[1L]]][['file']])
  dplyr::left_join(tg_df_list[[nesting[1L]]],
                   tg_df_list[[nesting[2L]]],
                   by = idx_1)
}
