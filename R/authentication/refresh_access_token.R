
refresh_access_token <- function(client_id, client_secret, refresh_token){
  # composição da url
  url <- glue("https://accounts.zoho.com/oauth/v2/token?refresh_token={refresh_token}&client_id={client_id}&client_secret={client_secret}&grant_type=refresh_token")

  # request the access token
  response_data <- request(url) |>
    req_method("POST") |>
    req_perform() |>
    resp_body_json()

  # returns the access token
  return(response_data$access_token)
}
