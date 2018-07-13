

#' @export
run <- function (x, ...) { 
  UseMethod("run", x) 
}

#' @import purrr
#' @import assertthat
command_seq <- function(...) {
  dots <- list(...)
  assert_that(all(map_lgl(dots, ~ inherits(., "command"))))
  
  structure(list(cmds=dots),
            class=c("command_seq", "command"))
}

#' @import furrr
run.command_seq <- function(x, ...) {
  furrr::future_map(x, ~ run(.,...))
}


#' @export
run.command <- function(x, ...) {
  processx::run(x$program, unlist(x$opts), ...)
}

command <- function(program, ..., input=NULL, switchchar="-") {
  dots <- list(...)
  ndots <- names(dots)
 
  if (is.null(ndots)) {
    assert_that(all(map_lgl(ndots, rlang::is_formula)))
  }
  
  argcmd <- map(seq_along(dots), function(i) {
    if (is_formula(dots[[i]])) {
      paste0(switchchar, deparse(rlang::f_rhs(dots[[i]])))
    } else {
      paste(paste0(switchchar, ndots[[i]]), paste(dots[[i]], collapse=" "))
    }
  })
   
  opts <- argcmd
  argcmd <- stringr::str_trim(paste(argcmd, collapse=" "))
  #argcmd <- map2(names(opts), opts, ~ paste(paste0(switchchar, .x), paste(.y, collapse=" "))) %>% paste(collapse=" ")
  
  if (!is.null(input)) {
    argcmd <- paste(argcmd, paste(input, collapse=" "))
  }
  
  ret <- list(
    program=program,
    opts=opts,
    argcmd=stringr::str_trim(argcmd)
  )
  
  class(ret) <- c(program, "command")
  ret
}


