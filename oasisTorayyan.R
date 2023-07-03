# Carrega os pactes
library(httr)
library(jsonlite)
library(tidyverse)
library(rvest)
library(xml2)

# Primeiro é necessário buscar os ID dos registros

## O conteúdo será um json
headers <- c("accept" = "application/json")

## Parâmetros de busca
parametros <- list(
  `lookfor` = '"judicialização da política"',
  `type` = "AllFields",
  `field[]` = "id",
  `sort` = "year",
  `page` = "1",
  `limit` = "100"
)

lista <- list()

for (i in 1:11) {
  parametros$page <- i

  # Primeiro vamos buscar os ID
  resposta <- GET(
    url = "https://oasisbr.ibict.br/vufind/api/v1/search",
    add_headers(.headers = headers),
    query = parametros
  )

  # Extrai o conteúdo do site
  conteudo <- fromJSON(content(resposta, "text"))

  # salvo o ID em uma lista (isso permite salvar os 1056 registros)
  lista[[i]] <- conteudo$records$id
}

id <- unlist(lista)

# Buscar os dados dos registros
lista_registros <- list()

for (i in 1:length(id)) {

  # Busca o registro (pesquisa)
  registro <- GET(
    url = paste("https://oasisbr.ibict.br/vufind/Record/", id[i], sep = "") |>
      str_replace_all('\\s', '%20') # precisei substituir os espaços (\\s) por %20 (que é um código de espaço)
  )

  # Conteúdo do registro
  dados_registro <- content(registro)

  # Busca as tabelas
  xml_find_all(dados_registro, "//table")

  # Nos interessa a tabela 2 - possui todos os metadados do registro
  ## O pivot_wider serve para transformar a coluna X1 (variável) em que cada variável será uma nova coluna e o X2 serão os valores desses variáveis
  dados_tabela <-
    html_table(dados_registro)[[2]] |>
    pivot_wider(names_from = X1, values_from = X2)

  # salva tudo em uma lista (permite salvar os 1056 registros)
  lista_registros[[i]] <- dados_tabela

  # aqui é só um penduricário para eu acompanhar o progresso
  ## Ele retorna no console qual registro foi concluído do total de registros
  print(paste('Registro: ', i, ' de ', length(id), sep = ''))
}

# Coloca tudo em uma tabela
tabela_registros <- bind_rows(lista_registros)

# Adaptando para o formato Rayyan
## O arquivo modelo que o Rayyan fornece possui as seguintes colunas:
## key,title,authors,journal,issn,volume,issue,pages,year,publisher,url,abstract,notes,doi,keywords

## Selecionar as colunas da tabela_registros conforme o que melhor se encaixar no modelo do Rayyan
tabela_registros <- tabela_registros |>
  select(title, author_facet, description, topic, publishDate, spelling, url) |>
  rename(authors = author_facet, abstract = description, keywords = topic, notes = spelling, year = publishDate) |>
  mutate(
    authors = str_replace_all(authors, '\n', '; ') |> str_squish(),
    keywords = keywords |> str_squish()
  ) |> rownames_to_column() |> rename(key = rowname)

# salva em arquivo .csv
## Por algum motivo que eu não sei qual é, o Rayyan não lê o arquivo salvo diretamente do R, eu precisei abrir o arquivo que o R gera no libreoffice, clicar em salvar e então o Rayyan aceita... talvez um problema de codificação.
## Então: 1. salva com a funçao write.csv; 2. abre o arquivo no libreoffice; 3. clica em salvar (não precisa alterar absolutamente nada); 4. envia pro Rayyan
tabela_registros |>
  write.csv('data/oasis.csv', row.names = F, fileEncoding = 'UTF-8', quote = TRUE)
