# =============================================================================
# HARMONIZAÇÃO DOS ESTOQUES DE CARBONO ORGÂNICO DO SOLO PARA 0–30 cm
# Versão 1.0.4 | Código para dissertação e repositório
#
# Autora: Delara Maryamo Ibraimo Cassamo
# Orientador: Prof. Dr. Alessandro Samuel-Rosa
# Coorientadora: Profa. Dra. Taciara Zborowski Horst
#
# Título da dissertação:
# ESTOQUES DE CARBONO ORGÂNICO DO SOLO NA TRANSIÇÃO FLORESTA–PASTAGEM
# EM DIFERENTES UNIDADES GEOAMBIENTAIS DA AMAZÔNIA BRASILEIRA
#
# Objetivo:
# Harmonizar estoques de COS provenientes de diferentes intervalos de
# profundidade para a camada de 0–30 cm, preservando a rastreabilidade entre
# cada resultado, as camadas de origem e o procedimento aplicado.
#
# Observação: o script recebe como entrada os estoques das camadas previamente
# calculados ou reportados pelas fontes originais e considerados elegíveis.
# Ele não recalcula o teor de carbono orgânico total, a densidade do solo ou a
# espessura das camadas.
#
# Correspondência com a Seção 5.4.2 da metodologia:
#   a) obtenção do estoque sem estimativa vertical;
#      a.1) valor direto de 0–30 cm;
#      a.2) soma de camadas contíguas;
#   b) soma de camadas contíguas com corte proporcional da última camada;
#   c) corte proporcional de uma camada acumulada;
#   d) extrapolações empíricas para completar 0–30 cm;
#      d.1) estimativa da camada de 20–30 cm;
#      d.2) expansão do estoque de 0–20 para 0–30 cm;
#   e) ausência de estimativa quando nenhum procedimento é aplicável.
#
# Resultados de referência da dissertação:
#   722 linhas na base bruta;
#   344 linhas selecionadas;
#   113 perfis harmonizados antes da consolidação de P89;
#   108 perfis finais após a consolidação de P89;
#   34 cronossequências independentes;
#   74 comparações floresta–pastagem;
#   0 perfis sem estimativa.
#
# Execução padrão, a partir da pasta principal do repositório:
#   Rscript R/01_harmonizacao_COS_0_30/harmonizacao_COS_0_30.R
#
# Entrada e saída alternativas:
#   Rscript R/01_harmonizacao_COS_0_30/harmonizacao_COS_0_30.R caminho/entrada.csv caminho/saida
# =============================================================================


# 1. CONFIGURAÇÃO -------------------------------------------------------------

VERSAO_PROTOCOLO <- "1.0.4"

pacotes_necessarios <- c("tidyverse", "janitor", "writexl")
pacotes_ausentes <- pacotes_necessarios[
  !vapply(pacotes_necessarios, requireNamespace, logical(1), quietly = TRUE)
]

if (length(pacotes_ausentes) > 0) {
  stop(
    "Pacotes ausentes: ",
    paste(pacotes_ausentes, collapse = ", "),
    ". Instale-os antes da execução, por exemplo: ",
    "install.packages(c(",
    paste(sprintf('"%s"', pacotes_ausentes), collapse = ", "),
    "))"
  )
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(writexl)
})

obter_diretorio_script <- function() {
  argumentos_completos <- commandArgs(trailingOnly = FALSE)
  argumento_arquivo <- grep("^--file=", argumentos_completos, value = TRUE)

  if (length(argumento_arquivo) > 0) {
    caminho <- sub("^--file=", "", argumento_arquivo[[1]])
    return(dirname(normalizePath(caminho, winslash = "/", mustWork = FALSE)))
  }

  if (
    requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()
  ) {
    caminho <- tryCatch(
      rstudioapi::getActiveDocumentContext()$path,
      error = function(e) ""
    )

    if (nzchar(caminho)) {
      return(dirname(normalizePath(caminho, winslash = "/", mustWork = FALSE)))
    }
  }

  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

diretorio_script <- obter_diretorio_script()

obter_raiz_projeto <- function(diretorio) {
  diretorio <- normalizePath(
    diretorio,
    winslash = "/",
    mustWork = FALSE
  )

  if (basename(diretorio) == "R") {
    return(dirname(diretorio))
  }

  if (basename(dirname(diretorio)) == "R") {
    return(dirname(dirname(diretorio)))
  }

  diretorio
}

raiz_projeto <- obter_raiz_projeto(diretorio_script)
argumentos <- commandArgs(trailingOnly = TRUE)

arquivo_entrada <- if (length(argumentos) >= 1 && nzchar(argumentos[[1]])) {
  argumentos[[1]]
} else {
  file.path(
    raiz_projeto,
    "dados",
    "entrada",
    "dados_harmonizacao_COS_0_30.csv"
  )
}

pasta_saida <- if (length(argumentos) >= 2 && nzchar(argumentos[[2]])) {
  argumentos[[2]]
} else {
  file.path(
    raiz_projeto,
    "resultados",
    "tabelas"
  )
}

arquivo_entrada <- normalizePath(
  arquivo_entrada,
  winslash = "/",
  mustWork = FALSE
)
pasta_saida <- normalizePath(
  pasta_saida,
  winslash = "/",
  mustWork = FALSE
)

# Parâmetros do protocolo
TOLERANCIA_CM <- 0.5
LARGURA_MAXIMA_CORTE_CM <- 40
PROFUNDIDADE_MAXIMA_ACUMULADA_CM <- 50
N_MINIMO_FATOR_POR_USO <- 5

# Exclusões previamente justificadas na auditoria da base
CRONOSSEQUENCIAS_EXCLUIR <- c("P118")

# Verificação da reprodução dos resultados desta dissertação.
# Mantenha TRUE para reproduzir a base publicada. Em adaptações do protocolo
# a outras bases, altere para FALSE e documente as novas contagens esperadas.
VALIDAR_CONTAGENS_ESTUDO <- TRUE

CONTAGENS_ESPERADAS <- c(
  linhas_brutas = 722L,
  linhas_selecionadas = 344L,
  perfis_antes_consolidacao_P89 = 113L,
  perfis_finais = 108L,
  cronossequencias_independentes = 34L,
  comparacoes_flo_pas = 74L,
  perfis_nao_harmonizados_antes = 0L,
  perfis_nao_harmonizados_finais = 0L
)

if (!file.exists(arquivo_entrada)) {
  stop(
    "Arquivo de entrada não encontrado: ",
    arquivo_entrada,
    "\nConsulte o README para a estrutura esperada do projeto."
  )
}

dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)


# 2. FUNÇÕES AUXILIARES -------------------------------------------------------

normalizar_texto <- function(x) {
  x <- stringr::str_to_lower(stringr::str_trim(as.character(x)))
  iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
}

converter_numero <- function(x) {
  x <- stringr::str_trim(as.character(x))
  x[x %in% c("", "NA", "NaN", "#N/A", "#N/D", "-", "NULL", "na", "n/a")] <- NA_character_
  x <- stringr::str_replace_all(x, "−|–|—", "-")
  x <- stringr::str_replace_all(x, ",", ".")

  suppressWarnings(
    as.numeric(stringr::str_extract(x, "-?[0-9]+(?:\\.[0-9]+)?"))
  )
}

proximo_de <- function(x, alvo) {
  !is.na(x) & abs(x - alvo) <= TOLERANCIA_CM
}

primeiro_nao_na <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(x[NA_integer_][1])
  }

  x[[1]]
}

classificar_uso <- function(perfil_id, uso) {
  uso_norm <- normalizar_texto(uso)
  perfil <- stringr::str_to_upper(perfil_id)

  dplyr::case_when(
    stringr::str_detect(uso_norm, "floresta") ~ "FLO",
    stringr::str_detect(uso_norm, "pastagem") ~ "PAS",
    stringr::str_detect(perfil, "FLO") ~ "FLO",
    stringr::str_detect(perfil, "PAS") ~ "PAS",
    TRUE ~ NA_character_
  )
}

extrair_campanha <- function(perfil_id) {
  x <- stringr::str_to_upper(perfil_id)
  id <- stringr::str_extract(x, "^P[0-9]+[A-Z]?(?=FLO|PAS)")
  dplyr::coalesce(id, stringr::str_extract(x, "^P[0-9]+"))
}

normalizar_cronossequencia <- function(campanha_id) {
  dplyr::if_else(
    campanha_id %in% c("P89A", "P89B"),
    "P89",
    campanha_id
  )
}

normalizar_perfil_analitico <- function(perfil_campanha_id, campanha_id) {
  dplyr::if_else(
    campanha_id %in% c("P89A", "P89B"),
    stringr::str_replace(
      stringr::str_to_upper(perfil_campanha_id),
      "^P89[AB]",
      "P89"
    ),
    stringr::str_to_upper(perfil_campanha_id)
  )
}

colapsar_unicos <- function(x, separador = "; ") {
  x <- unique(x[!is.na(x) & stringr::str_trim(as.character(x)) != ""])
  if (length(x) == 0) NA_character_ else paste(x, collapse = separador)
}

media_segura <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

desvio_padrao_seguro <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) NA_real_ else stats::sd(x)
}

pior_incerteza <- function(x) {
  ordem <- c(baixa = 1L, media = 2L, alta = 3L)
  x <- x[!is.na(x) & x %in% names(ordem)]

  if (length(x) == 0) {
    return(NA_character_)
  }

  maior_nivel <- max(ordem[x])
  names(ordem)[ordem == maior_nivel][[1]]
}

classificar_correcao_massa <- function(correcao, validacao) {
  correcao <- normalizar_texto(correcao)
  validacao <- normalizar_texto(validacao)

  dplyr::case_when(
    correcao %in% c("sim", "s", "yes") ~ "sim",
    correcao %in% c("nao", "n", "no") ~ "nao",
    validacao %in% c("sim", "s", "yes") ~ "sim",
    validacao %in% c("nao", "n", "no") ~ "nao",
    TRUE ~ "desconhecida"
  )
}

# Escolhe a origem definida na coluna "tipo de cos que entra na modelagem".
# Quando o valor preferencial não existe, usa a outra origem somente como
# fallback explícito e rastreável. Estoque reportado com correção de massa
# confirmada nunca é usado.
escolher_estoque <- function(
  tipo_cos_preferido,
  estoque_calculado_t_ha,
  estoque_reportado_t_ha,
  correcao_massa
) {
  # Estoque reportado só é elegível quando a ausência de correção por massa
  # equivalente foi confirmada explicitamente. Se a condição for desconhecida,
  # o algoritmo utiliza o estoque calculado, quando disponível, e registra o
  # fallback na auditoria.
  reportado_valido <- !is.na(estoque_reportado_t_ha) & correcao_massa == "nao"

  estoque <- dplyr::case_when(
    tipo_cos_preferido == "calculado" & !is.na(estoque_calculado_t_ha) ~ estoque_calculado_t_ha,
    tipo_cos_preferido == "reportado" & reportado_valido ~ estoque_reportado_t_ha,
    tipo_cos_preferido == "calculado" & is.na(estoque_calculado_t_ha) & reportado_valido ~ estoque_reportado_t_ha,
    tipo_cos_preferido == "reportado" & !reportado_valido & !is.na(estoque_calculado_t_ha) ~ estoque_calculado_t_ha,
    is.na(tipo_cos_preferido) & !is.na(estoque_calculado_t_ha) ~ estoque_calculado_t_ha,
    is.na(tipo_cos_preferido) & reportado_valido ~ estoque_reportado_t_ha,
    TRUE ~ NA_real_
  )

  origem <- dplyr::case_when(
    tipo_cos_preferido == "calculado" & !is.na(estoque_calculado_t_ha) ~ "calculado",
    tipo_cos_preferido == "reportado" & reportado_valido ~ "reportado",
    tipo_cos_preferido == "calculado" & is.na(estoque_calculado_t_ha) & reportado_valido ~ "reportado_fallback",
    tipo_cos_preferido == "reportado" & !reportado_valido & !is.na(estoque_calculado_t_ha) ~ "calculado_fallback",
    is.na(tipo_cos_preferido) & !is.na(estoque_calculado_t_ha) ~ "calculado_sem_seletor",
    is.na(tipo_cos_preferido) & reportado_valido ~ "reportado_sem_seletor",
    TRUE ~ NA_character_
  )

  tibble::tibble(
    estoque_selecionado = estoque,
    origem_estoque = origem,
    flag_fallback_origem = dplyr::if_else(
      stringr::str_detect(dplyr::coalesce(origem, ""), "fallback|sem_seletor"),
      "sim",
      "nao"
    )
  )
}

# Mantém uma linha por intervalo de profundidade dentro de cada perfil.
# A prioridade é: sem fallback, origem preferencial e menor ordem de entrada.
resolver_camadas_duplicadas <- function(dados_camadas) {
  if (nrow(dados_camadas) == 0) return(dados_camadas)

  dados_camadas %>%
    arrange(
      prof_i,
      prof_f,
      desc(flag_fallback_origem == "nao"),
      ordem_linha
    ) %>%
    distinct(prof_i, prof_f, .keep_all = TRUE)
}

# Procura uma sequência de camadas contíguas iniciada em 0 cm.
# Quando corte = TRUE, permite utilizar apenas a fração da última camada que
# atravessa 30 cm.
buscar_sequencia <- function(dados_camadas, alvo, corte = FALSE) {
  dados_camadas <- resolver_camadas_duplicadas(dados_camadas)

  if (nrow(dados_camadas) == 0) {
    return(list(ok = FALSE))
  }

  buscar <- function(profundidade_atual) {
    if (profundidade_atual >= alvo - TOLERANCIA_CM) {
      return(tibble::tibble())
    }

    candidatos <- dados_camadas %>%
      filter(
        abs(prof_i - profundidade_atual) <= TOLERANCIA_CM,
        prof_f > profundidade_atual + TOLERANCIA_CM
      ) %>%
      arrange(prof_f)

    if (nrow(candidatos) == 0) return(NULL)

    for (i in seq_len(nrow(candidatos))) {
      camada <- candidatos[i, , drop = FALSE]
      largura <- camada$prof_f - camada$prof_i

      # Camada inteiramente contida no intervalo desejado
      if (camada$prof_f <= alvo + TOLERANCIA_CM) {
        proxima_profundidade <- ifelse(
          proximo_de(camada$prof_f, alvo),
          alvo,
          camada$prof_f
        )

        restante <- buscar(proxima_profundidade)

        if (!is.null(restante)) {
          camada_formatada <- camada %>%
            transmute(
              id_final,
              prof_i,
              prof_f,
              estoque = estoque_selecionado,
              origem = origem_estoque,
              fracao = 1
            )

          return(bind_rows(camada_formatada, restante))
        }
      }

      # Corte proporcional da última camada que ultrapassa o alvo
      if (
        corte &&
        camada$prof_f > alvo + TOLERANCIA_CM &&
        is.finite(largura) &&
        largura > 0 &&
        largura <= LARGURA_MAXIMA_CORTE_CM
      ) {
        fracao <- (alvo - camada$prof_i) / largura

        if (is.finite(fracao) && fracao > 0 && fracao <= 1) {
          return(
            camada %>%
              transmute(
                id_final,
                prof_i,
                prof_f,
                estoque = estoque_selecionado,
                origem = origem_estoque,
                fracao
              )
          )
        }
      }
    }

    NULL
  }

  caminho <- buscar(0)

  if (is.null(caminho) || nrow(caminho) == 0) {
    return(list(ok = FALSE))
  }

  list(
    ok = TRUE,
    estoque = sum(caminho$estoque * caminho$fracao, na.rm = TRUE),
    usados = caminho,
    usou_corte = any(caminho$fracao < 1 - 1e-12, na.rm = TRUE)
  )
}

resultado_harmonizacao <- function(
  estoque = NA_real_,
  metodo = "sem_estimativa",
  incerteza = NA_character_,
  usados = NULL,
  motivo = NA_character_,
  fator_20_30 = NA_real_,
  fator_0_20_0_30 = NA_real_,
  classe_fator = NA_character_,
  n_fator = NA_integer_
) {
  tem_camadas <- !is.null(usados) && nrow(usados) > 0

  tibble::tibble(
    estoque_0_30 = estoque,
    metodo_0_30 = metodo,
    incerteza_0_30 = incerteza,
    motivo_sem_estimativa = motivo,
    flag_extrapolacao = if_else(
      stringr::str_detect(metodo, "extrapolacao"),
      "sim",
      "nao"
    ),
    fator_empirico_20_30 = fator_20_30,
    fator_empirico_0_20_para_0_30 = fator_0_20_0_30,
    classe_fator_empirico = classe_fator,
    n_perfis_fator_empirico = n_fator,
    origem_camadas = if (tem_camadas) {
      paste(sort(unique(usados$origem)), collapse = "+")
    } else {
      NA_character_
    },
    ids_usados = if (tem_camadas) {
      paste(usados$id_final, collapse = "; ")
    } else {
      NA_character_
    },
    camadas_usadas = if (tem_camadas) {
      paste0(usados$prof_i, "-", usados$prof_f, collapse = "; ")
    } else {
      NA_character_
    },
    fracoes_usadas = if (tem_camadas) {
      paste(round(usados$fracao, 4), collapse = "; ")
    } else {
      NA_character_
    }
  )
}

fator_valido <- function(x) {
  length(x) == 1 && !is.na(x) && is.finite(x) && x > 0
}


# 3. PREPARAÇÃO DA BASE -------------------------------------------------------

dados_brutos <- readr::read_csv(
  arquivo_entrada,
  show_col_types = FALSE,
  progress = FALSE
) %>%
  janitor::clean_names()

colunas_obrigatorias <- c(
  "entra_ou_nao_na_modelagem",
  "tipo_de_cos_que_entra_na_modelagem",
  "id_final",
  "latitude",
  "longitude",
  "uso",
  "idade_da_pastagem",
  "prof_inicial_cm",
  "prof_final_cm",
  "estoque_calculado_t_ha",
  "estoque_reportado_t_ha",
  "correcao_reportada",
  "validacao_correcao"
)

colunas_ausentes <- setdiff(colunas_obrigatorias, names(dados_brutos))

if (length(colunas_ausentes) > 0) {
  stop("Colunas obrigatórias ausentes: ", paste(colunas_ausentes, collapse = ", "))
}

colunas_opcionais <- c(
  "estado_da_pastagem",
  "tipo_de_trab",
  "ano_de_publicacao",
  "estado",
  "titulo",
  "link",
  "status",
  "notas"
)

for (coluna in colunas_opcionais) {
  if (!coluna %in% names(dados_brutos)) dados_brutos[[coluna]] <- NA
}

dados_preparados <- dados_brutos %>%
  mutate(
    ordem_linha = row_number(),
    entra_norm = normalizar_texto(entra_ou_nao_na_modelagem),
    tipo_cos_preferido = normalizar_texto(tipo_de_cos_que_entra_na_modelagem),
    id_final = stringr::str_replace_all(as.character(id_final), "\\s+", ""),
    linha_agregada = stringr::str_detect(
      id_final,
      stringr::regex("C[0-9]+a$", ignore_case = TRUE)
    ),
    perfil_campanha_id = stringr::str_remove(
      id_final,
      stringr::regex("C[0-9]+[A-Za-z]?$", ignore_case = TRUE)
    ),
    campanha_id = extrair_campanha(perfil_campanha_id),
    cronossequencia_id = normalizar_cronossequencia(campanha_id),
    perfil_analitico_id = normalizar_perfil_analitico(
      perfil_campanha_id,
      campanha_id
    ),
    # Durante a harmonização, perfil_id identifica cada campanha separadamente.
    perfil_id = perfil_campanha_id,
    latitude = converter_numero(latitude),
    longitude = converter_numero(longitude),
    idade_da_pastagem = converter_numero(idade_da_pastagem),
    prof_inicial_cm = converter_numero(prof_inicial_cm),
    prof_final_cm = converter_numero(prof_final_cm),
    estoque_calculado_t_ha = converter_numero(estoque_calculado_t_ha),
    estoque_reportado_t_ha = converter_numero(estoque_reportado_t_ha),
    classe_uso = classificar_uso(perfil_campanha_id, uso),
    correcao_massa = classificar_correcao_massa(
      correcao_reportada,
      validacao_correcao
    )
  )

estoques_escolhidos <- purrr::pmap_dfr(
  dados_preparados %>%
    select(
      tipo_cos_preferido,
      estoque_calculado_t_ha,
      estoque_reportado_t_ha,
      correcao_massa
    ),
  escolher_estoque
)

dados_preparados <- bind_cols(dados_preparados, estoques_escolhidos) %>%
  mutate(
    flag_reportado_correcao_desconhecida = if_else(
      tipo_cos_preferido == "reportado" &
        correcao_massa == "desconhecida",
      "sim",
      "nao"
    )
  )

# Valores diferentes de "sim" ou "nao" são registrados e não entram na harmonização.
controle_flags_selecao <- dados_preparados %>%
  filter(!entra_norm %in% c("sim", "nao")) %>%
  select(
    ordem_linha,
    id_final,
    cronossequencia_id,
    campanha_id,
    uso,
    idade_da_pastagem,
    entra_ou_nao_na_modelagem,
    status,
    tipo_cos_preferido,
    correcao_massa
  )

if (nrow(controle_flags_selecao) > 0) {
  warning(
    nrow(controle_flags_selecao),
    " linha(s) apresentam marcador de inclusão vazio ou inválido. ",
    "Essas linhas não entrarão na harmonização; consulte a tabela ",
    "controle_flags_selecao."
  )
}

# Linhas efetivamente selecionadas para a harmonização
dados_selecionados <- dados_preparados %>%
  filter(
    entra_norm == "sim",
    !linha_agregada,
    !cronossequencia_id %in% CRONOSSEQUENCIAS_EXCLUIR
  )

camadas_invalidas <- dados_selecionados %>%
  filter(
    is.na(prof_inicial_cm) |
      is.na(prof_final_cm) |
      is.na(estoque_selecionado) |
      prof_inicial_cm < 0 |
      prof_final_cm <= prof_inicial_cm |
      estoque_selecionado < 0
  )

camadas_validas <- dados_selecionados %>%
  filter(
    !is.na(prof_inicial_cm),
    !is.na(prof_final_cm),
    !is.na(estoque_selecionado),
    prof_inicial_cm >= 0,
    prof_final_cm > prof_inicial_cm,
    estoque_selecionado >= 0
  )

camadas_duplicadas <- camadas_validas %>%
  count(perfil_id, prof_inicial_cm, prof_final_cm, name = "n") %>%
  filter(n > 1)

# Padroniza os nomes das profundidades e resolve intervalos duplicados.
camadas <- camadas_validas %>%
  transmute(
    perfil_id,
    cronossequencia_id,
    classe_uso,
    id_final,
    ordem_linha,
    prof_i = prof_inicial_cm,
    prof_f = prof_final_cm,
    estoque_selecionado,
    origem_estoque,
    flag_fallback_origem
  ) %>%
  group_by(perfil_id) %>%
  group_modify(~ resolver_camadas_duplicadas(.x)) %>%
  ungroup()


# 4. CÁLCULO DOS FATORES EMPÍRICOS -------------------------------------------

fatores_20_30 <- camadas %>%
  filter(
    proximo_de(prof_i, 10),
    proximo_de(prof_f, 20)
  ) %>%
  select(perfil_id, classe_uso, cos_10_20 = estoque_selecionado) %>%
  inner_join(
    camadas %>%
      filter(
        proximo_de(prof_i, 20),
        proximo_de(prof_f, 30)
      ) %>%
      select(perfil_id, cos_20_30 = estoque_selecionado),
    by = "perfil_id"
  ) %>%
  filter(cos_10_20 > 0, cos_20_30 >= 0) %>%
  mutate(fator = cos_20_30 / cos_10_20) %>%
  filter(is.finite(fator))

fatores_0_20_0_30 <- purrr::map_dfr(
  unique(camadas$perfil_id),
  function(perfil) {
    d <- camadas %>% filter(perfil_id == perfil)
    soma_20 <- buscar_sequencia(d, 20)
    soma_30 <- buscar_sequencia(d, 30)

    if (soma_20$ok && soma_30$ok && soma_20$estoque > 0) {
      tibble(
        perfil_id = perfil,
        classe_uso = primeiro_nao_na(d$classe_uso),
        fator = soma_30$estoque / soma_20$estoque
      )
    } else {
      tibble(
        perfil_id = perfil,
        classe_uso = primeiro_nao_na(d$classe_uso),
        fator = NA_real_
      )
    }
  }
) %>%
  filter(!is.na(fator), is.finite(fator), fator >= 1)

resumir_fatores <- function(tabela, nome) {
  por_uso <- tabela %>%
    group_by(classe_uso) %>%
    summarise(
      n = n(),
      mediana = median(fator),
      .groups = "drop"
    )

  global <- tibble(
    classe_uso = "GLOBAL",
    n = nrow(tabela),
    mediana = if (nrow(tabela) > 0) median(tabela$fator) else NA_real_
  )

  bind_rows(por_uso, global) %>%
    mutate(fator_nome = nome, .before = 1)
}

resumo_fatores <- bind_rows(
  resumir_fatores(fatores_20_30, "20_30_sobre_10_20"),
  resumir_fatores(fatores_0_20_0_30, "0_30_sobre_0_20")
)

obter_fator <- function(nome, classe_alvo) {
  especifico <- resumo_fatores %>%
    filter(
      fator_nome == .env$nome,
      .data$classe_uso == .env$classe_alvo,
      n >= N_MINIMO_FATOR_POR_USO
    ) %>%
    slice(1)

  if (nrow(especifico) == 1 && fator_valido(especifico$mediana)) {
    return(list(
      valor = especifico$mediana,
      classe = classe_alvo,
      n = especifico$n
    ))
  }

  global <- resumo_fatores %>%
    filter(fator_nome == .env$nome, .data$classe_uso == "GLOBAL") %>%
    slice(1)

  if (nrow(global) == 1 && fator_valido(global$mediana)) {
    return(list(
      valor = global$mediana,
      classe = "GLOBAL",
      n = global$n
    ))
  }

  list(valor = NA_real_, classe = NA_character_, n = NA_integer_)
}


# 5. PROTOCOLO HIERÁRQUICO DE HARMONIZAÇÃO -----------------------------------

harmonizar_perfil <- function(d) {
  d <- resolver_camadas_duplicadas(d)

  if (nrow(d) == 0) {
    return(resultado_harmonizacao(motivo = "sem_camadas_validas"))
  }

  classe_uso <- primeiro_nao_na(d$classe_uso)
  fator_20_30 <- obter_fator("20_30_sobre_10_20", classe_uso)
  fator_0_20_0_30 <- obter_fator("0_30_sobre_0_20", classe_uso)

  # a.1) Valor direto de 0–30 cm
  direto <- d %>%
    filter(proximo_de(prof_i, 0), proximo_de(prof_f, 30)) %>%
    slice(1)

  if (nrow(direto) == 1) {
    usados <- direto %>%
      transmute(
        id_final,
        prof_i,
        prof_f,
        estoque = estoque_selecionado,
        origem = origem_estoque,
        fracao = 1
      )

    return(resultado_harmonizacao(
      estoque = direto$estoque_selecionado,
      metodo = "dado_original_0_30",
      incerteza = "baixa",
      usados = usados
    ))
  }

  # a.2) Soma de camadas contíguas
  soma_30 <- buscar_sequencia(d, 30)

  if (soma_30$ok) {
    return(resultado_harmonizacao(
      estoque = soma_30$estoque,
      metodo = "soma_contigua_0_30",
      incerteza = "baixa",
      usados = soma_30$usados
    ))
  }

  # b) Soma de camadas contíguas com corte proporcional da última camada
  soma_corte <- buscar_sequencia(d, 30, corte = TRUE)

  if (soma_corte$ok && soma_corte$usou_corte && nrow(soma_corte$usados) >= 2) {
    return(resultado_harmonizacao(
      estoque = soma_corte$estoque,
      metodo = "soma_contigua_com_corte_parcial_0_30",
      incerteza = "media",
      usados = soma_corte$usados
    ))
  }

  acumulados <- d %>%
    filter(proximo_de(prof_i, 0)) %>%
    arrange(prof_f)

  # c) Corte proporcional de uma camada acumulada
  acumulado_isolado <- acumulados %>%
    filter(
      prof_f > 30 + TOLERANCIA_CM,
      prof_f <= PROFUNDIDADE_MAXIMA_ACUMULADA_CM
    ) %>%
    arrange(prof_f) %>%
    slice(1)

  if (nrow(acumulado_isolado) == 1) {
    fracao <- 30 / acumulado_isolado$prof_f

    usados <- acumulado_isolado %>%
      transmute(
        id_final,
        prof_i,
        prof_f,
        estoque = estoque_selecionado,
        origem = origem_estoque,
        fracao
      )

    return(resultado_harmonizacao(
      estoque = acumulado_isolado$estoque_selecionado * fracao,
      metodo = "corte_proporcional_acumulado_0_x_para_0_30",
      incerteza = "media",
      usados = usados
    ))
  }

  # d) Extrapolações empíricas para completar 0–30 cm
  # d.1) Estimativa empírica da camada de 20–30 cm
  camada_0_10 <- d %>%
    filter(proximo_de(prof_i, 0), proximo_de(prof_f, 10)) %>%
    slice(1)

  camada_10_20 <- d %>%
    filter(proximo_de(prof_i, 10), proximo_de(prof_f, 20)) %>%
    slice(1)

  camada_20_30 <- d %>%
    filter(proximo_de(prof_i, 20), proximo_de(prof_f, 30)) %>%
    slice(1)

  if (
    nrow(camada_0_10) == 1 &&
    nrow(camada_10_20) == 1 &&
    nrow(camada_20_30) == 0 &&
    fator_valido(fator_20_30$valor)
  ) {
    usados <- bind_rows(camada_0_10, camada_10_20) %>%
      transmute(
        id_final,
        prof_i,
        prof_f,
        estoque = estoque_selecionado,
        origem = origem_estoque,
        fracao = 1
      )

    estoque_estimado <- camada_0_10$estoque_selecionado +
      camada_10_20$estoque_selecionado * (1 + fator_20_30$valor)

    return(resultado_harmonizacao(
      estoque = estoque_estimado,
      metodo = "extrapolacao_empirica_20_30",
      incerteza = "alta",
      usados = usados,
      fator_20_30 = fator_20_30$valor,
      classe_fator = fator_20_30$classe,
      n_fator = fator_20_30$n
    ))
  }

  # d.2) Expansão empírica do estoque de 0–20 para 0–30 cm
  soma_20 <- buscar_sequencia(d, 20)

  if (
    soma_20$ok &&
    fator_valido(fator_0_20_0_30$valor) &&
    fator_0_20_0_30$valor >= 1
  ) {
    return(resultado_harmonizacao(
      estoque = soma_20$estoque * fator_0_20_0_30$valor,
      metodo = "extrapolacao_empirica_0_20_para_0_30",
      incerteza = "alta",
      usados = soma_20$usados,
      fator_0_20_0_30 = fator_0_20_0_30$valor,
      classe_fator = fator_0_20_0_30$classe,
      n_fator = fator_0_20_0_30$n
    ))
  }

  # e) Ausência de estimativa
  resultado_harmonizacao(
    motivo = "sem_condicao_para_harmonizacao_0_30"
  )
}


# 6. APLICAÇÃO DO PROTOCOLO E CONSOLIDAÇÃO DE P89 ----------------------------

perfis_avaliados <- sort(unique(dados_selecionados$perfil_id))

resultado_perfis <- purrr::map_dfr(
  perfis_avaliados,
  function(perfil) {
    d <- camadas %>% filter(perfil_id == perfil)

    harmonizar_perfil(d) %>%
      mutate(perfil_id = perfil, .before = 1)
  }
)

metadados_perfil <- dados_selecionados %>%
  group_by(perfil_id) %>%
  summarise(
    perfil_campanha_id = primeiro_nao_na(perfil_campanha_id),
    campanha_id = primeiro_nao_na(campanha_id),
    cronossequencia_id = primeiro_nao_na(cronossequencia_id),
    perfil_analitico_id = primeiro_nao_na(perfil_analitico_id),
    latitude = media_segura(latitude),
    longitude = media_segura(longitude),
    uso = primeiro_nao_na(uso),
    classe_uso = primeiro_nao_na(classe_uso),
    idade_da_pastagem = primeiro_nao_na(idade_da_pastagem),
    tipo_cos_preferido = colapsar_unicos(tipo_cos_preferido),
    estado_da_pastagem = colapsar_unicos(estado_da_pastagem),
    tipo_de_trab = primeiro_nao_na(tipo_de_trab),
    ano_de_publicacao = primeiro_nao_na(ano_de_publicacao),
    estado = primeiro_nao_na(estado),
    titulo = primeiro_nao_na(titulo),
    link = primeiro_nao_na(link),
    status = primeiro_nao_na(status),
    n_linhas_perfil = n(),
    n_camadas_validas = sum(
      !is.na(prof_inicial_cm) &
        !is.na(prof_final_cm) &
        !is.na(estoque_selecionado) &
        prof_inicial_cm >= 0 &
        prof_final_cm > prof_inicial_cm &
        estoque_selecionado >= 0
    ),
    houve_fallback_origem = if_else(
      any(flag_fallback_origem == "sim"),
      "sim",
      "nao"
    ),
    reportado_correcao_desconhecida = if_else(
      any(flag_reportado_correcao_desconhecida == "sim"),
      "sim",
      "nao"
    ),
    .groups = "drop"
  )

# Base anterior à consolidação de P89. P89A e P89B permanecem separadas.
controle_perfis_campanha <- metadados_perfil %>%
  left_join(resultado_perfis, by = "perfil_id") %>%
  mutate(
    status_harmonizacao_campanha = if_else(
      !is.na(estoque_0_30),
      "harmonizado",
      "nao_harmonizado"
    )
  )

# Base usada para contabilizar os métodos e as classes de incerteza.
base_harmonizada_0_30 <- controle_perfis_campanha %>%
  filter(status_harmonizacao_campanha == "harmonizado")

perfis_campanha_nao_harmonizados <- controle_perfis_campanha %>%
  filter(status_harmonizacao_campanha == "nao_harmonizado")

# Consolida P89A e P89B somente após a harmonização. PAS3 e PAS4 permanecem separados.
consolidar_perfil_analitico <- function(d) {
  crono <- primeiro_nao_na(d$cronossequencia_id)
  eh_p89 <- identical(crono, "P89")

  n_campanhas_esperadas <- dplyr::n_distinct(d$campanha_id)
  harmonizados <- d %>% filter(!is.na(estoque_0_30))
  n_campanhas_harmonizadas <- nrow(harmonizados)

  consolidacao_completa <- if (eh_p89) {
    n_campanhas_esperadas == 2 && n_campanhas_harmonizadas == 2
  } else {
    n_campanhas_harmonizadas >= 1
  }

  estoque_final <- if (consolidacao_completa) {
    mean(harmonizados$estoque_0_30)
  } else {
    NA_real_
  }

  metodo_final <- dplyr::case_when(
    !consolidacao_completa ~ "sem_estimativa",
    eh_p89 ~ "media_pos_harmonizacao_campanhas_sazonais",
    TRUE ~ primeiro_nao_na(harmonizados$metodo_0_30)
  )

  motivo_final <- dplyr::case_when(
    consolidacao_completa ~ NA_character_,
    eh_p89 ~ "p89_sem_duas_campanhas_sazonais_harmonizadas",
    TRUE ~ dplyr::coalesce(
      primeiro_nao_na(d$motivo_sem_estimativa),
      "sem_condicao_para_harmonizacao_0_30"
    )
  )

  valores_campanhas <- harmonizados$estoque_0_30

  tibble::tibble(
    perfil_id = primeiro_nao_na(d$perfil_analitico_id),
    cronossequencia_id = crono,
    latitude = media_segura(d$latitude),
    longitude = media_segura(d$longitude),
    uso = primeiro_nao_na(d$uso),
    classe_uso = primeiro_nao_na(d$classe_uso),
    idade_da_pastagem = primeiro_nao_na(d$idade_da_pastagem),
    tipo_cos_preferido = colapsar_unicos(d$tipo_cos_preferido),
    estado_da_pastagem = colapsar_unicos(d$estado_da_pastagem),
    tipo_de_trab = primeiro_nao_na(d$tipo_de_trab),
    ano_de_publicacao = primeiro_nao_na(d$ano_de_publicacao),
    estado = primeiro_nao_na(d$estado),
    titulo = primeiro_nao_na(d$titulo),
    link = primeiro_nao_na(d$link),
    status = primeiro_nao_na(d$status),
    n_linhas_perfil = sum(d$n_linhas_perfil, na.rm = TRUE),
    n_camadas_validas = sum(d$n_camadas_validas, na.rm = TRUE),
    houve_fallback_origem = if_else(
      any(d$houve_fallback_origem == "sim", na.rm = TRUE),
      "sim",
      "nao"
    ),
    reportado_correcao_desconhecida = if_else(
      any(d$reportado_correcao_desconhecida == "sim", na.rm = TRUE),
      "sim",
      "nao"
    ),
    estoque_0_30 = estoque_final,
    metodo_0_30 = metodo_final,
    incerteza_0_30 = if (consolidacao_completa) {
      pior_incerteza(harmonizados$incerteza_0_30)
    } else {
      NA_character_
    },
    motivo_sem_estimativa = motivo_final,
    flag_extrapolacao = if_else(
      consolidacao_completa &&
        any(harmonizados$flag_extrapolacao == "sim", na.rm = TRUE),
      "sim",
      "nao"
    ),
    fator_empirico_20_30 = primeiro_nao_na(
      harmonizados$fator_empirico_20_30
    ),
    fator_empirico_0_20_para_0_30 = primeiro_nao_na(
      harmonizados$fator_empirico_0_20_para_0_30
    ),
    classe_fator_empirico = colapsar_unicos(
      harmonizados$classe_fator_empirico
    ),
    n_perfis_fator_empirico = if (
      all(is.na(harmonizados$n_perfis_fator_empirico))
    ) {
      NA_integer_
    } else {
      as.integer(max(harmonizados$n_perfis_fator_empirico, na.rm = TRUE))
    },
    origem_camadas = colapsar_unicos(harmonizados$origem_camadas),
    ids_usados = colapsar_unicos(harmonizados$ids_usados, " | "),
    camadas_usadas = colapsar_unicos(
      harmonizados$camadas_usadas,
      " | "
    ),
    fracoes_usadas = colapsar_unicos(
      harmonizados$fracoes_usadas,
      " | "
    ),
    flag_consolidacao_sazonal = if_else(eh_p89, "sim", "nao"),
    n_campanhas_esperadas = n_campanhas_esperadas,
    n_campanhas_harmonizadas = n_campanhas_harmonizadas,
    campanhas_origem = colapsar_unicos(d$campanha_id),
    perfis_campanha_origem = colapsar_unicos(
      d$perfil_campanha_id
    ),
    estoques_campanhas_0_30 = if (n_campanhas_harmonizadas > 0) {
      paste0(
        harmonizados$campanha_id,
        "=",
        format(
          round(harmonizados$estoque_0_30, 4),
          trim = TRUE,
          scientific = FALSE
        ),
        collapse = "; "
      )
    } else {
      NA_character_
    },
    metodos_campanhas = colapsar_unicos(
      harmonizados$metodo_0_30
    ),
    incertezas_campanhas = colapsar_unicos(
      harmonizados$incerteza_0_30
    ),
    desvio_padrao_entre_campanhas = desvio_padrao_seguro(
      valores_campanhas
    ),
    amplitude_entre_campanhas = if (
      length(valores_campanhas[!is.na(valores_campanhas)]) >= 2
    ) {
      diff(range(valores_campanhas, na.rm = TRUE))
    } else {
      NA_real_
    },
    status_harmonizacao_perfil = if_else(
      !is.na(estoque_final),
      "harmonizado",
      "nao_harmonizado"
    )
  )
}

controle_todos_perfis <- controle_perfis_campanha %>%
  group_by(perfil_analitico_id) %>%
  group_split(.keep = TRUE) %>%
  purrr::map_dfr(consolidar_perfil_analitico) %>%
  arrange(cronossequencia_id, classe_uso, idade_da_pastagem, perfil_id)

# Base final: P89A e P89B consolidadas como uma única cronossequência P89.
base_analitica_independente_0_30 <- controle_todos_perfis %>%
  filter(status_harmonizacao_perfil == "harmonizado")

perfis_analiticos_nao_harmonizados <- controle_todos_perfis %>%
  filter(status_harmonizacao_perfil == "nao_harmonizado")

controle_p89_consolidacao <- controle_perfis_campanha %>%
  filter(cronossequencia_id == "P89") %>%
  arrange(perfil_analitico_id, campanha_id)

# Controle das amostragens seca e úmida de P89.
p89_campanhas_harmonizadas <- controle_p89_consolidacao %>%
  filter(status_harmonizacao_campanha == "harmonizado") %>%
  select(
    cronossequencia_id,
    campanha_id,
    perfil_campanha_id,
    perfil_analitico_id,
    classe_uso,
    idade_da_pastagem,
    estado_da_pastagem,
    estoque_0_30,
    metodo_0_30,
    incerteza_0_30
  )

controle_fallback <- dados_selecionados %>%
  filter(flag_fallback_origem == "sim") %>%
  select(
    ordem_linha,
    cronossequencia_id,
    campanha_id,
    perfil_analitico_id,
    perfil_campanha_id,
    id_final,
    tipo_cos_preferido,
    estoque_calculado_t_ha,
    estoque_reportado_t_ha,
    correcao_massa,
    estoque_selecionado,
    origem_estoque,
    prof_inicial_cm,
    prof_final_cm
  )


# 7. CONTROLE DAS CRONOSSEQUÊNCIAS -------------------------------------------

# Situação de cada cronossequência após a harmonização.
status_cronossequencias <- controle_todos_perfis %>%
  group_by(cronossequencia_id) %>%
  summarise(
    n_perfis_avaliados = n(),
    n_perfis_harmonizados = sum(status_harmonizacao_perfil == "harmonizado"),
    n_perfis_nao_harmonizados = sum(
      status_harmonizacao_perfil == "nao_harmonizado"
    ),
    n_perfis_floresta = sum(classe_uso == "FLO", na.rm = TRUE),
    n_perfis_pastagem = sum(classe_uso == "PAS", na.rm = TRUE),
    n_florestas_harmonizadas = sum(
      classe_uso == "FLO" &
        status_harmonizacao_perfil == "harmonizado",
      na.rm = TRUE
    ),
    n_pastagens_harmonizadas = sum(
      classe_uso == "PAS" &
        status_harmonizacao_perfil == "harmonizado",
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    status_harmonizacao_cronossequencia = case_when(
      n_perfis_harmonizados == 0 ~ "nao_harmonizada",
      n_perfis_nao_harmonizados == 0 ~ "totalmente_harmonizada",
      TRUE ~ "parcialmente_harmonizada"
    ),
    possui_par_flo_pas_harmonizado = if_else(
      n_florestas_harmonizadas >= 1 &
        n_pastagens_harmonizadas >= 1,
      "sim",
      "nao"
    )
  ) %>%
  arrange(cronossequencia_id)


# 8. PARES FLORESTA–PASTAGEM --------------------------------------------------

criar_pares <- function(base) {
  florestas <- base %>%
    filter(classe_uso == "FLO") %>%
    select(
      cronossequencia_id,
      perfil_flo = perfil_id,
      cos_flo = estoque_0_30,
      metodo_flo = metodo_0_30,
      incerteza_flo = incerteza_0_30
    )

  # Confere se existe mais de uma floresta harmonizada por cronossequência.
  florestas_repetidas <- florestas %>%
    count(cronossequencia_id, name = "n_florestas") %>%
    filter(n_florestas > 1)

  if (nrow(florestas_repetidas) > 0) {
    stop(
      "Há mais de uma floresta harmonizada em uma ou mais cronossequências: ",
      paste(florestas_repetidas$cronossequencia_id, collapse = ", ")
    )
  }

  pastagens <- base %>%
    filter(classe_uso == "PAS") %>%
    select(
      cronossequencia_id,
      perfil_pas = perfil_id,
      idade_pas = idade_da_pastagem,
      cos_pas = estoque_0_30,
      metodo_pas = metodo_0_30,
      incerteza_pas = incerteza_0_30
    )

  pastagens %>%
    left_join(florestas, by = "cronossequencia_id") %>%
    mutate(
      delta_cos = cos_pas - cos_flo,
      delta_percentual = if_else(
        !is.na(cos_flo) & cos_flo != 0,
        100 * delta_cos / cos_flo,
        NA_real_
      ),
      direcao = case_when(
        is.na(cos_flo) ~ "sem_floresta_pareada",
        delta_cos > 0 ~ "aumento",
        delta_cos < 0 ~ "diminuicao",
        TRUE ~ "sem_alteracao"
      )
    )
}

pares_floresta_pastagem <- criar_pares(base_analitica_independente_0_30)


# 9. RESUMOS E VALIDAÇÕES -----------------------------------------------------

resumo_geral <- tibble(
  indicador = c(
    "Linhas na base bruta",
    "Linhas selecionadas para harmonização",
    "Perfis avaliados antes da consolidação de P89",
    "Perfis harmonizados antes da consolidação de P89",
    "Perfis não harmonizados antes da consolidação de P89",
    "Perfis avaliados após a consolidação de P89",
    "Perfis harmonizados após a consolidação de P89",
    "Perfis não harmonizados após a consolidação de P89",
    "Cronossequências avaliadas após consolidação de P89",

    "Cronossequências totalmente harmonizadas",
    "Cronossequências parcialmente harmonizadas",
    "Cronossequências não harmonizadas",
    "Cronossequências com par FLO–PAS harmonizado",
    "Pares floresta–pastagem harmonizados",
    "Perfis harmonizados por extrapolação antes da consolidação de P89",
    "Perfis harmonizados por extrapolação após a consolidação de P89",
    "Perfis consolidados a partir de P89A e P89B",
    "Linhas com fallback da origem do estoque"
  ),
  n = c(
    nrow(dados_brutos),
    nrow(dados_selecionados),
    nrow(controle_perfis_campanha),
    sum(
      controle_perfis_campanha$status_harmonizacao_campanha ==
        "harmonizado"
    ),
    sum(
      controle_perfis_campanha$status_harmonizacao_campanha ==
        "nao_harmonizado"
    ),
    nrow(controle_todos_perfis),
    nrow(base_analitica_independente_0_30),
    nrow(perfis_analiticos_nao_harmonizados),
    nrow(status_cronossequencias),
    sum(
      status_cronossequencias$status_harmonizacao_cronossequencia ==
        "totalmente_harmonizada"
    ),
    sum(
      status_cronossequencias$status_harmonizacao_cronossequencia ==
        "parcialmente_harmonizada"
    ),
    sum(
      status_cronossequencias$status_harmonizacao_cronossequencia ==
        "nao_harmonizada"
    ),
    sum(status_cronossequencias$possui_par_flo_pas_harmonizado == "sim"),
    sum(!is.na(pares_floresta_pastagem$cos_flo)),
    sum(base_harmonizada_0_30$flag_extrapolacao == "sim", na.rm = TRUE),
    sum(base_analitica_independente_0_30$flag_extrapolacao == "sim", na.rm = TRUE),
    sum(base_analitica_independente_0_30$flag_consolidacao_sazonal == "sim"),
    nrow(controle_fallback)
  )
)

resumo_metodos <- controle_todos_perfis %>%
  count(
    status_harmonizacao_perfil,
    metodo_0_30,
    incerteza_0_30,
    name = "n"
  ) %>%
  arrange(status_harmonizacao_perfil, desc(n))

resumo_metodos_campanha <- controle_perfis_campanha %>%
  count(
    status_harmonizacao_campanha,
    metodo_0_30,
    incerteza_0_30,
    name = "n"
  ) %>%
  arrange(status_harmonizacao_campanha, desc(n))


# Síntese final dos métodos de harmonização por cronossequência --------------
#
# Esta tabela reproduz a estrutura utilizada na dissertação:
# método, classe de incerteza, número de cronossequências independentes,
# número de perfis-campanha e respectivos IDs.
#
# A contagem é feita na base canônica por campanha, porque é essa base que
# sustenta os 113 perfis anteriores à consolidação de P89 e a distribuição dos métodos.
# P89A e P89B permanecem separadas como campanhas, mas a coluna
# cronossequencia_id já as identifica como uma única cronossequência P89.

ordenar_ids_crono <- function(x) {
  x <- unique(as.character(x[!is.na(x)]))
  x[order(readr::parse_number(x), x)]
}

rotulos_metodos <- c(
  "corte_proporcional_acumulado_0_x_para_0_30" =
    "Corte proporcional acumulado de 0–x para 0–30 cm",
  "extrapolacao_empirica_20_30" =
    "Extrapolação empírica 20–30 cm",
  "extrapolacao_empirica_0_20_para_0_30" =
    "Extrapolação empírica de 0–20 para 0–30 cm",
  "soma_contigua_0_30" =
    "Soma de camadas contíguas 0–30 cm",
  "dado_original_0_30" =
    "Dado original 0–30 cm",
  "soma_contigua_com_corte_parcial_0_30" =
    "Soma de camadas contíguas com corte parcial em 30 cm"
)

ordem_metodos <- c(
  "corte_proporcional_acumulado_0_x_para_0_30",
  "extrapolacao_empirica_20_30",
  "extrapolacao_empirica_0_20_para_0_30",
  "soma_contigua_0_30",
  "dado_original_0_30",
  "soma_contigua_com_corte_parcial_0_30"
)

# Confere se cada cronossequência foi tratada por apenas um método.
# Essa verificação impede dupla contagem na coluna "Nº crono".
controle_metodo_por_crono <- base_harmonizada_0_30 %>%
  distinct(cronossequencia_id, metodo_0_30, incerteza_0_30) %>%
  count(cronossequencia_id, name = "n_metodos") %>%
  filter(n_metodos > 1)

if (nrow(controle_metodo_por_crono) > 0) {
  stop(
    "Erro: uma ou mais cronossequências aparecem em mais de um método de ",
    "harmonização: ",
    paste(controle_metodo_por_crono$cronossequencia_id, collapse = ", ")
  )
}

tabela_sintese_metodos <- base_harmonizada_0_30 %>%
  group_by(metodo_0_30, incerteza_0_30) %>%
  summarise(
    `Nº crono` = n_distinct(cronossequencia_id),
    `Nº perfis` = n(),
    `IDs das cronossequências` = paste(
      ordenar_ids_crono(cronossequencia_id),
      collapse = ", "
    ),
    .groups = "drop"
  ) %>%
  mutate(
    ordem = match(metodo_0_30, ordem_metodos),
    `Método de harmonização` = dplyr::recode(
      metodo_0_30,
      !!!rotulos_metodos
    ),
    Incerteza = dplyr::recode(
      incerteza_0_30,
      baixa = "Baixa",
      media = "Média",
      alta = "Alta"
    )
  ) %>%
  arrange(ordem) %>%
  select(
    `Método de harmonização`,
    Incerteza,
    `Nº crono`,
    `Nº perfis`,
    `IDs das cronossequências`
  )

linha_total_metodos <- tibble::tibble(
  `Método de harmonização` = "Total",
  Incerteza = "—",
  `Nº crono` = dplyr::n_distinct(
    base_harmonizada_0_30$cronossequencia_id
  ),
  `Nº perfis` = nrow(base_harmonizada_0_30),
  `IDs das cronossequências` = NA_character_
)

tabela_sintese_metodos <- bind_rows(
  tabela_sintese_metodos,
  linha_total_metodos
)

# Confere os totais da síntese dos métodos.
if (
  sum(
    tabela_sintese_metodos$`Nº perfis`[
      tabela_sintese_metodos$`Método de harmonização` != "Total"
    ]
  ) != nrow(base_harmonizada_0_30)
) {
  stop("Erro: o total de perfis da síntese dos métodos não fecha.")
}

if (
  sum(
    tabela_sintese_metodos$`Nº crono`[
      tabela_sintese_metodos$`Método de harmonização` != "Total"
    ]
  ) != dplyr::n_distinct(base_harmonizada_0_30$cronossequencia_id)
) {
  stop("Erro: o total de cronossequências da síntese dos métodos não fecha.")
}


resumo_perfis_por_cronossequencia <- status_cronossequencias %>%
  select(
    cronossequencia_id,
    status_harmonizacao_cronossequencia,
    n_perfis_avaliados,
    n_perfis_harmonizados,
    n_perfis_nao_harmonizados,
    n_perfis_floresta,
    n_perfis_pastagem,
    n_florestas_harmonizadas,
    n_pastagens_harmonizadas,
    possui_par_flo_pas_harmonizado
  )

p89_estrutura <- controle_perfis_campanha %>%
  filter(cronossequencia_id == "P89") %>%
  count(perfil_analitico_id, name = "n_campanhas") %>%
  arrange(perfil_analitico_id)

# Verificações estruturais ----------------------------------------------------

if (
  any(CRONOSSEQUENCIAS_EXCLUIR %in% base_harmonizada_0_30$cronossequencia_id) ||
    any(CRONOSSEQUENCIAS_EXCLUIR %in%
      base_analitica_independente_0_30$cronossequencia_id)
) {
  stop("Erro: cronossequência previamente excluída apareceu em uma base final.")
}

if (
  nrow(base_harmonizada_0_30) + nrow(perfis_campanha_nao_harmonizados) !=
    nrow(controle_perfis_campanha)
) {
  stop("Erro: a contagem dos perfis-campanha não fecha.")
}

if (
  nrow(base_analitica_independente_0_30) +
      nrow(perfis_analiticos_nao_harmonizados) !=
    nrow(controle_todos_perfis)
) {
  stop("Erro: a contagem dos perfis analíticos não fecha.")
}

if (
  sum(
    controle_perfis_campanha$status_harmonizacao_campanha %in%
      c("harmonizado", "nao_harmonizado")
  ) != nrow(controle_perfis_campanha)
) {
  stop("Erro: a contagem dos perfis-campanha não fecha.")
}

if (
  sum(
    status_cronossequencias$status_harmonizacao_cronossequencia %in%
      c(
        "totalmente_harmonizada",
        "parcialmente_harmonizada",
        "nao_harmonizada"
      )
  ) != nrow(status_cronossequencias)
) {
  stop("Erro: a classificação das cronossequências não fecha.")
}

if (
  any(base_analitica_independente_0_30$cronossequencia_id %in%
    c("P89A", "P89B"))
) {
  stop("Erro: P89A ou P89B foi contado como cronossequência independente.")
}

if (nrow(p89_estrutura) > 0 && any(p89_estrutura$n_campanhas != 2)) {
  stop(
    "Erro: cada perfil analítico de P89 deve possuir duas campanhas ",
    "(P89A e P89B)."
  )
}

if (
  nrow(controle_p89_consolidacao) > 0 &&
  !all(c("P89A", "P89B") %in% controle_p89_consolidacao$campanha_id)
) {
  stop("Erro: uma das campanhas sazonais de P89 está ausente.")
}



# Validação dos resultados de referência -------------------------------------

validar_contagem <- function(nome, observado, esperado) {
  observado <- as.integer(observado)
  esperado <- as.integer(esperado)

  if (!identical(observado, esperado)) {
    stop(
      "A validação da reprodução falhou para '", nome, "': ",
      "observado = ", observado, "; esperado = ", esperado, "."
    )
  }
}

contagens_observadas <- c(
  linhas_brutas = nrow(dados_brutos),
  linhas_selecionadas = nrow(dados_selecionados),
  perfis_antes_consolidacao_P89 = nrow(base_harmonizada_0_30),
  perfis_finais = nrow(base_analitica_independente_0_30),
  cronossequencias_independentes = nrow(status_cronossequencias),
  comparacoes_flo_pas = sum(!is.na(pares_floresta_pastagem$cos_flo)),
  perfis_nao_harmonizados_antes = nrow(perfis_campanha_nao_harmonizados),
  perfis_nao_harmonizados_finais = nrow(perfis_analiticos_nao_harmonizados)
)

if (VALIDAR_CONTAGENS_ESTUDO) {
  purrr::walk(
    names(CONTAGENS_ESPERADAS),
    ~ validar_contagem(
      .x,
      contagens_observadas[[.x]],
      CONTAGENS_ESPERADAS[[.x]]
    )
  )
}

# 10. EXPORTAÇÃO E REGISTRO DA EXECUÇÃO --------------------------------------

# Base canônica por campanha e base analítica independente
readr::write_csv(
  base_harmonizada_0_30,
  file.path(pasta_saida, "base_harmonizada_COS_0_30_por_campanha.csv")
)

readr::write_csv(
  base_analitica_independente_0_30,
  file.path(pasta_saida, "base_analitica_COS_0_30_P89_consolidada.csv")
)

readr::write_csv(
  pares_floresta_pastagem,
  file.path(pasta_saida, "pares_FLO_PAS_0_30.csv")
)

# Tabelas de controle e auditoria
readr::write_csv(
  resumo_geral,
  file.path(pasta_saida, "resumo_harmonizacao_COS_0_30.csv")
)

readr::write_csv(
  tabela_sintese_metodos,
  file.path(pasta_saida, "sintese_METODOS_HARMONIZACAO.csv")
)

readr::write_csv(
  controle_todos_perfis,
  file.path(pasta_saida, "controle_PERFIS_ANALITICOS.csv")
)

readr::write_csv(
  perfis_analiticos_nao_harmonizados,
  file.path(pasta_saida, "controle_PERFIS_ANALITICOS_NAO_HARMONIZADOS.csv")
)

readr::write_csv(
  perfis_campanha_nao_harmonizados,
  file.path(pasta_saida, "controle_PERFIS_CAMPANHA_NAO_HARMONIZADOS.csv")
)

readr::write_csv(
  controle_perfis_campanha,
  file.path(pasta_saida, "controle_PERFIS_CAMPANHA.csv")
)

readr::write_csv(
  controle_p89_consolidacao,
  file.path(pasta_saida, "controle_P89_CONSOLIDACAO_SAZONAL.csv")
)

readr::write_csv(
  p89_campanhas_harmonizadas,
  file.path(pasta_saida, "P89_CAMPANHAS_SAZONAIS_HARMONIZADAS.csv")
)

readr::write_csv(
  status_cronossequencias,
  file.path(pasta_saida, "controle_CRONOSSEQUENCIAS_HARMONIZACAO.csv")
)

readr::write_csv(
  controle_fallback,
  file.path(pasta_saida, "controle_FALLBACK_origem_estoque.csv")
)

readr::write_csv(
  controle_flags_selecao,
  file.path(pasta_saida, "controle_FLAGS_SELECAO.csv")
)

# Se o Excel de auditoria estiver aberto ou bloqueado, salva uma cópia alternativa.
arquivo_excel_padrao <- file.path(
  pasta_saida,
  "harmonizacao_COS_0_30_auditoria_completa_FINAL.xlsx"
)

arquivo_excel <- arquivo_excel_padrao

if (file.exists(arquivo_excel_padrao)) {
  removido <- suppressWarnings(file.remove(arquivo_excel_padrao))

  if (!isTRUE(removido)) {
    arquivo_excel <- file.path(
      pasta_saida,
      "harmonizacao_COS_0_30_auditoria_completa_COPIA.xlsx"
    )

    warning(
      "O arquivo Excel anterior está aberto ou bloqueado. ",
      "A auditoria será salva em: ",
      arquivo_excel
    )
  }
}

writexl::write_xlsx(
  list(
    resumo_geral = resumo_geral,
    sintese_metodos = tabela_sintese_metodos,
    base_por_campanha = base_harmonizada_0_30,
    base_analitica = base_analitica_independente_0_30,
    pares_flo_pas = pares_floresta_pastagem,
    perfis_analiticos = controle_todos_perfis,
    perfis_analiticos_nao_harm = perfis_analiticos_nao_harmonizados,
    perfis_campanha = controle_perfis_campanha,
    perfis_campanha_nao_harm = perfis_campanha_nao_harmonizados,
    p89_consolidacao = controle_p89_consolidacao,
    p89_campanhas = p89_campanhas_harmonizadas,
    cronossequencias = status_cronossequencias,
    perfis_por_cronossequencia = resumo_perfis_por_cronossequencia,
    metodos = resumo_metodos,
    metodos_campanha = resumo_metodos_campanha,
    fallback_origem = controle_fallback,
    flags_selecao = controle_flags_selecao,
    camadas_invalidas = camadas_invalidas,
    camadas_duplicadas = camadas_duplicadas,
    fatores_resumo = resumo_fatores,
    fatores_20_30 = fatores_20_30,
    fatores_0_20_0_30 = fatores_0_20_0_30,
    p89_estrutura = p89_estrutura
  ),
  path = arquivo_excel
)

# Preserva a base de entrada utilizada.
file.copy(
  arquivo_entrada,
  file.path(pasta_saida, basename(arquivo_entrada)),
  overwrite = TRUE
)

capture.output(
  sessionInfo(),
  file = file.path(pasta_saida, "sessionInfo.txt")
)



readr::write_csv(
  tibble::enframe(contagens_observadas, name = "indicador", value = "n"),
  file.path(pasta_saida, "contagens_observadas.csv")
)

cat("\n============================================================\n")
cat("HARMONIZAÇÃO DO COS PARA 0–30 cm CONCLUÍDA — PROTOCOLO ", VERSAO_PROTOCOLO, "\n", sep = "")
cat("============================================================\n\n")
print(resumo_geral, n = Inf)
cat("\nSíntese dos métodos de harmonização:\n")
print(tabela_sintese_metodos, n = Inf)
cat("\nP89A e P89B foram harmonizadas separadamente e consolidadas como P89.\n")
cat("PAS3 e PAS4 de P89 permaneceram separados por representarem manejos distintos.\n")
cat("\nPasta de saída:\n", pasta_saida, "\n", sep = "")
cat("\nArquivo Excel:\n", arquivo_excel, "\n", sep = "")
