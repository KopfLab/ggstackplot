# helper function to calculate whether to do an axis switch
# switch = from left to right for y-axis, from bottom to top for x axis
calculate_axis_switch <- function(var, alternate, switch) {
  # safety checks
  stopifnot(
    "`var` must be character, factor, or integer" = !missing(var) && (is.factor(var) || is_character(var) || is_integer(var)),
    "`alternate` must be TRUE or FALSE" = !missing(alternate) && is_bool(alternate),
    "`switch` must be TRUE or FALSE" = !missing(switch) && is_bool(switch)
  )

  # convert var
  var <- if(is.factor(var)) as.integer(var)
    else if (is_character(var)) factor_in_order(var) |> as.integer()
    else var

  # calculate
  if (!alternate) {
    # no alternating axis
    return(rep(switch, length(var)))
  } else if (!switch) {
    # alternating axis not switched
    return(var %% 2L == 0L)
  } else {
    # switched alternating axis
    return(var %% 2L == 1L)
  }
}

# helper function to calculate plot sizes taking overlap into consideration
calculate_plot_sizes <- function(overlap_before, overlap_after) {
  # safety checks
  stopifnot(
    "`overlap_before` must be numeric" = !missing(overlap_before) && is_double(overlap_before),
    "`overlap_after` must be numeric" = !missing(overlap_after) && is_double(overlap_after)
  )

  # calculation
  #return( 1 - overlap_before/2 - overlap_after/2)
  return( 1/ (1 + overlap_before/2 + overlap_after/2))
}
