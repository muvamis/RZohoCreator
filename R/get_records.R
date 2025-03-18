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

get_records <- function(account_owner_name, app_name, report_name, access_token, modified_time_last = NULL) {
  # Inicialização
  all_records <- list() # Lista para armazenar todos os registros
  cursor <- NULL # Inicializa o cursor
  more_records <- TRUE # Flag para o loop

  # Construir URL base da API
  url <- glue("https://zohoapis.com/creator/v2.1/data/{account_owner_name}/{app_name}/report/{report_name}")

  while (more_records) {
    # Construir a query para filtrar registros modificados após `modified_time_last`
    query_params <- list(max_records = 1000)
    if (!is.null(modified_time_last)) {
      query_params$filter = glue('("Modified_Time" > "{modified_time_last}")')
    }

    # Fazer a requisição à API
    response_data <- request(url) |>
      req_headers(
        Authorization = paste("Zoho-oauthtoken", access_token),
        accept = "application/json"
      ) |>
      req_url_query(!!!query_params) |>  # Adicionar query dinamicamente
      req_perform()

    if (response_data$status_code == 200) {
      # Processar JSON
      response_json <- resp_body_json(response_data)

      # Extrair registros (assumindo que os registros estão na chave 'data')
      if (!is.null(response_json$data)) {
        all_records <- append(all_records, response_json$data)
      }

      # Atualizar cursor (se disponível)
      if (!is.null(response_json$headers$record_cursor)) {
        cursor <- response_json$headers$record_cursor
        message("Cursor atualizado: ", cursor)
      } else {
        more_records <- FALSE
      }
    } else {
      stop("Erro na requisição: ", response_data$status_code)
    }
  }

  # Converter lista de dados em um data frame final
  if (length(all_records) > 0) {
    data <- do.call(rbind, lapply(all_records, as.data.frame))

    # Atualizar `modified_time_last` com o maior valor encontrado
    modified_time_last <- max(data$Modified_Time, na.rm = TRUE)

    # Mensagem informando quantos registros foram baixados
    message("✅ ", nrow(data), " novos registros foram baixados.")
    message("Última modificação baixada: ", modified_time_last)

    return(list(data = data, modified_time_last = modified_time_last))
  } else {
    message("ℹ️ Nenhum novo registro encontrado após ", modified_time_last)
    return(list(data = NULL, modified_time_last = modified_time_last))
  }
}
