#' @title Get Records for Zoho Creator
#'
#' @description
#' Esta função busca registros de um relatório no Zoho Creator usando a API.
#'
#' @param account_owner_name Nome do proprietário da conta.
#' @param app_name Nome do aplicativo dentro do Zoho Creator.
#' @param report_name Nome do relatório de onde os registros serão extraídos.
#' @param access_token Token de acesso para autenticação na API do Zoho.
#'
#' @return Um data frame contendo os registros extraídos do relatório.
#'
#' @details
#' A função realiza múltiplas requisições à API para recuperar até 1000 registros por vez,
#' continuando enquanto houver registros adicionais disponíveis. A função só para
#'  depois de buscar todos os registos do relatorio.
#'
#' #' @examples
#' # Exemplo de uso da função com autenticação via arquivo .env
#' library(dotenv)
#' load_dot_env(".env")
#'
#' account_owner <- Sys.getenv("ZOHO_ACCOUNT_OWNER")
#' app <- Sys.getenv("ZOHO_APP_NAME")
#' report <- Sys.getenv("ZOHO_REPORT_NAME")
#' token <- Sys.getenv("ZOHO_ACCESS_TOKEN")
#'
#' registros <- get_records(account_owner, app, report, token)
#' view(registros)
#'
#' @export
get_records <- function(account_owner_name, app_name, report_name, access_token){
  # Inicialização
  all_records <- list() # Lista para armazenar todos os registros
  cursor <- NULL # Inicializa o cursor
  more_records <- TRUE # Flag para o loop

  # URL da API
  url <- glue("https://zohoapis.com/creator/v2.1/data/{account_owner_name}/{app_name}/report/{report_name}")


  while (more_records) {
    # requisição para buscar os dados
    response_data <- request(url) |>
      req_headers(
        Authorization = paste("Zoho-oauthtoken", access_token),
        accept = "application/json",
        record_cursor = cursor
      ) |>
      req_url_query(
        max_records = 1000
      ) |>
      req_perform()

    if (response_data$status_code == 200){
      # Processar o JSON
      response_json <- resp_body_json(response_data)

      # Extrair registros (assumindo que os registros estão na chave 'data')
      if (!is.null(response_json$data)) {
        all_records <- append(all_records, response_json$data)
      }

      # Atualizar cursor (se disponível)
      if (!is.null(response_data$headers$record_cursor)) {
        cursor <- response_data$headers$record_cursor
        print(cursor)
        message(length(response_json$data))
      } else {
        more_records <- FALSE
      }
    } else {
      stop("Erro na requisição: ", response_data$status_code)
    }

  }

  # Converter lista de dados em um data frame final
  data <- do.call(rbind, lapply(all_records, as.data.frame))

  return (data)
}
