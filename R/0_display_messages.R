#' Basic formattinf function for display messages
#'
#' @param text_message  character string for the text to display
#' @param colour_bg character string colour of the background
#' @param text_black boolean, TRUE to keep the text in black, FALSE to make it white
#' @param symbol_u unicode symbol to display before the message
#'
#' @return cat message with formatting
#' @export
#'
#' @importFrom crayon make_style
#' @importFrom stringr str_c
#'
message_3me <-   function(text_message, colour_bg =  "seagreen2" , text_black = TRUE , symbol_u = "\U2705"){

  bg_fn <- crayon::make_style(colour_bg, bg= TRUE)
  ct_fn <- crayon::make_style(ifelse(text_black,"black","white"), bg= FALSE)

  new_message<- stringr::str_c(text_message, "\n") %>% ct_fn %>% bg_fn
  cat(stringr::str_c("\n ",symbol_u, " " ,new_message)

  )
}





#' Preprogrammed ThreeME formatting for Ok messages
#'
#' @param txt_message text to display
#'
#' @return cat messages
#' @export
#'
message_ok <- function(txt_message= "TEST"){

  message_3me(text_message = txt_message,
              colour_bg = "seagreen2",
              text_black = TRUE,
              symbol_u = "\U2705")

}

#' Preprogrammed ThreeME formatting for NOT Ok messages
#'
#' @param txt_message text to display
#'
#' @return cat messages
#' @export
#'
message_not_ok <- function(txt_message= "TEST"){

  message_3me(text_message = txt_message,
              colour_bg = "firebrick",
              text_black = FALSE,
              symbol_u = "\U274C")

}

#' Preprogrammed ThreeME formatting for warning messages
#'
#' @param txt_message text to display
#'
#' @return cat messages
#' @export
#'
message_warning <- function(txt_message= "TEST"){

  message_3me(text_message = txt_message,
              colour_bg = "darkorange",
              text_black = TRUE,
              symbol_u = "\U2757")

}

#' Preprogrammed ThreeME formatting for STOP messages
#'
#' @param txt_message text to display
#'
#' @return cat messages
#' @export
#'
message_stopbomb <- function(txt_message= "TEST"){

  message_3me(text_message = txt_message,
              colour_bg = "firebrick",
              text_black = FALSE,
              symbol_u = "\U1F4A3 \U1F4A3")

}

#' Preprogrammed ThreeME formatting for SAVING messages
#'
#' @param txt_message text to display
#'
#' @return cat messages
#' @export
#'
message_save <- function(txt_message= "TEST"){

  message_3me(text_message = txt_message,
              colour_bg = "thistle2",
              text_black = TRUE,
              symbol_u = "\U1F4BE")

}


#' Preprogrammed ThreeME formatting for Deleting messages
#'
#' @param txt_message text to display
#'
#' @return cat messages
#' @export
#'
message_delete <- function(txt_message= "TEST"){

  message_3me(text_message = txt_message,
              colour_bg = "thistle2",
              text_black = TRUE,
              symbol_u = "\U1F6AE")

}

#' Preprogrammed ThreeME formatting for normal messages, with possibility to customize the symbol
#'
#' @param txt_message text to display
#' @param custom_symbol custom unicode symbol to display, Default displays none
#'
#' @return cat messages
#' @export
#'
message_any <- function(txt_message= "TEST", custom_symbol = ""){

  message_3me(text_message = txt_message,
              colour_bg = "thistle2",
              text_black = TRUE,
              symbol_u = custom_symbol)

}

#' Preprogrammed ThreeME formatting for MAIN STEPS messages
#'
#' @param txt_message text to display
#'
#' @return cat messages
#' @export
#'
message_main_step<-function(txt_message= "TEST"){

  message_3me(text_message = txt_message,
              colour_bg = "steelblue3",
              text_black = TRUE,
              symbol_u = "\U1F31F")

}

#' Preprogrammed ThreeME formatting for Substeps messages
#'
#' @param txt_message text to display
#'
#' @return cat messages
#' @export
#'
message_sub_step<-function(txt_message= "TEST"){

  message_3me(text_message = txt_message,
              colour_bg = "steelblue1",
              text_black = TRUE,
              symbol_u = "\U2B50")

}
