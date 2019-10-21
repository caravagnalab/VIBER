.onLoad <- function(libname, pkgname)
{
  # =-=-=-=-=-=-
  # Required packages will be listed here
  # =-=-=-=-=-=-
  requirements = c('tidyverse', 'pio', 'crayon', 'mobster', 'reshape2')
  
  suppressMessages(sapply(requirements, require, character.only = TRUE))
  
  # =-=-=-=-=-=-
  # Package options
  # =-=-=-=-=-=-
  options(pio.string_fg_colour = crayon::bgYellow$black)
  
  # =-=-=-=-=-=-
  # Header
  # =-=-=-=-=-=-
  
  VIBER_welcome_message =  getOption('VIBER_welcome_message', default = TRUE)
  
  if(VIBER_welcome_message)
  {
    pio::pioHdr('VIBER - Variational inference for Binomial mixtures')
    pio::pioStr("Author : ", "Giulio Caravagna <gcaravagn@gmail.com>", suffix = '\n')
    pio::pioStr("GitHub : ", "caravagn/VIBER [https://caravagn.github.io/VIBER/]", suffix = '\n')
    pio::pioStr("   WWW : ", "https://caravagn.github.io/VIBER/", suffix = '\n')
    
    cat(
      "\n > VIBER is part of the", crayon::green("\"evoverse\""),
      crayon::blue("[https://bit.ly/2orn94e]"),
      "- a collection of packages to implement Cancer Evolution analyses from cancer sequencing data.\n"
    )
    
    options(VIBER_welcome_message = FALSE)
  }
  
  invisible()
}
