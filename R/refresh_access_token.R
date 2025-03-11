#'  Refresh Access Token
#'
#' Esta função renova o access token utilizando o refresh token fornecido.
#'
#' @param client_id O ID do cliente fornecido pela Zoho.
#' @param client_secret O segredo do cliente fornecido pela Zoho.
#' @param refresh_token O token de renovação fornecido pela Zoho
#'
#' Segue https://www.zoho.com/creator/help/api/v2.1/oauth-overview.html Para ter
#' acesso ao client_id, client_secret, refresh_token.
#'
#' @return Retorna a resposta completa da requisição em formato JSON, que geralmente contém os seguintes campos:
#'
#' - `access_token`: O novo token de acesso, utilizado para autenticar as requisições à API.
#' - `expires_in`: O tempo de expiração do access token, geralmente em segundos.
#' - `token_type`: O tipo do token retornado, normalmente "Bearer".
#' - `api_domain`: O domínio da API que deve ser usado para fazer chamadas autenticadas.
#' - `error` (opcional): Caso ocorra um erro na requisição, esse campo pode conter a descrição do erro.
#'
#' @examples
#' client_id <- "seu_client_id"
#' client_secret <- "seu_client_secret"
#' refresh_token <- "seu_refresh_token"
#' response <- refresh_access_token(client_id, client_secret, refresh_token)
#'
#' # Exemplo de resposta esperada:
#' # {
#' #   "access_token": "1000.abcdef1234567890ghijklmnopqrstu",
#' #   "refresh_token": "1000.zyxwvutsrqponmlkjihgfedcba123456",
#' #   "api_domain": "https://www.zohoapis.com",
#' #   "token_type": "Bearer",
#' #   "expires_in": 3600
#' # }
#' @export
refresh_access_token <- function(client_id, client_secret, refresh_token){

  # composição da url
  url <- glue("https://accounts.zoho.com/oauth/v2/token?refresh_token={refresh_token}&client_id={client_id}&client_secret={client_secret}&grant_type=refresh_token")

  # request the access token
  response_data <- request(url) |>
    req_method("POST") |>
    req_perform() |>
    resp_body_json()

  # returns the access token
  return(response_data)
}
