postKoBo <- function(.url, .payload, .auth) {
  if(!hasArg(.url)|!is.character(.url)) {stop("url must be a character string")}

  if(!hasArg(.payload)|is.null(.payload)) {stop("Payload must be defined")}

  if(!methods::hasArg(.auth)|!is.list(.auth)) {
    stop(".auth must be a list of credentials containing either the username and password, or the token")
  }
  if("token"%in%ls(.auth)) {
    .auth <- httr::add_headers(Authorization = paste0("Token ", .auth$token))
  } else if (c("password")%in%ls(.auth) & c("username")%in%ls(.auth)) {
    .auth <- httr::authenticate(.auth$username,
                                .auth$password)
  } else {
    stop(".auth must be a list of credentials containing either the username and password, or the token")
  }







}
