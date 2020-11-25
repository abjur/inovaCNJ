#' verificacao UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_verificacao_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(

    bs4Dash::box(
      width = 12,
      inputId = ns("caixa-dados"),
      title = "Baixar dados com as correções consolidadas",
      collapsible = FALSE,
      closable = FALSE,
      maximizable = FALSE,

      shiny::radioButtons(
        ns("tipo_base"),
        "O que você gostaria de exportar?",
        c(
          "Apenas correções manuais" = "manual",
          "Apenas correções automatizadas" = "auto",
          "Todas as correções" = "full"
        )
      ),

      shiny::fluidRow(col_12(
        shiny::downloadButton(
          ns("base_json"),
          label = "Download (JSON)"
        ),
        shiny::downloadButton(
          ns("base_csv"),
          label = "Download (CSV)"
        )
      )),

      shiny::fluidRow(
        shiny::span(
          "A base de dados disponibilizada pode contemplar todas as alterações",
          " sugeridas pelos analistas na aba de inconsistências, além de todas",
          " as alterações realizadas automaticamente.",
          style = "margin-top: 20px;"
        )
      )

    ),

    bs4Dash::box(
      width = 12,
      inputId = ns("caixa"),
      title = "Lista de sugestões manuais",
      collapsible = TRUE,
      closable = FALSE,
      maximizable = FALSE,

      shiny::fluidRow(col_12(
        reactable::reactableOutput(ns("tabela")) %>%
          shinycssloaders::withSpinner()
      )),
      shiny::br(),
      shiny::fluidRow(
        shiny::actionButton(
          ns("atualizar"), "Atualizar",
          icon = shiny::icon("sync")
        )
      )
    ),

    bs4Dash::box(
      width = 12,
      inputId = ns("caixa-tab"),
      title = "Visualizar sugestão manuais",
      collapsible = TRUE,
      collapsed = TRUE,
      closable = FALSE,
      maximizable = FALSE,

      shiny::fluidRow(col_12(
        reactable::reactableOutput(ns("tabela_selecionada")) %>%
          shinycssloaders::withSpinner(),
      )),
      shiny::br(),
      shiny::fluidRow(
        shiny::downloadButton(ns("download"))
      )

    )
  )
}

#' verificacao Server Functions
#'
#' @noRd
mod_verificacao_server <- function(id, app_data){
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns


    dados_sugestoes <- shiny::reactive({
      input$atualizar
      con <- conectar()
      dados <- RPostgres::dbReadTable(con, "sugestoes")
      desconectar(con)
      dados
    })

    selected <- shiny::reactive(
      reactable::getReactableState(
        "tabela",
        "selected",
        session = session
      )
    )

    dados_tabela <- shiny::reactive({

      selecionado <- selected()
      linha <- dados_sugestoes()[selecionado, ]
      tab <- linha[["inconsistencia"]]
      num <- linha[["id_sub"]]
      if (length(tab) > 0) {
        con <- conectar()
        dados <- con %>%
          dplyr::tbl(tab) %>%
          dplyr::filter(id_sub == num) %>%
          dplyr::collect()
        desconectar(con)
        dados
      }
    })

    base_arrumada <- shiny::reactive({


      if (input$tipo_base == "manual") {
        con <- conectar()
        tabelas <- RPostgres::dbListTables(con)
        tabelas <- tabelas[!tabelas %in% c("sugestoes")]
        da_sugestoes <- purrr::map_dfr(tabelas, ler_sugestoes, con) %>%
          dplyr::group_by(id) %>%
          dplyr::summarise(
            dplyr::across(dplyr::starts_with("sol_"), ~dplyr::first(.[!is.na(.)])),
            .groups = "drop"
          )
        desconectar(con)

      } else if (input$tipo_base == "auto") {
        da_sugestoes <- inovaCNJ::da_incos %>%
          dplyr::select(id, dplyr::starts_with("sol_")) %>%
          dplyr::filter_at(
            dplyr::vars(dplyr::starts_with("sol_"), -sol_digito),
            dplyr::any_vars(!is.na(.))
          ) %>%
          dplyr::distinct(id, .keep_all = TRUE)
      } else {

        con <- conectar()
        tabelas <- RPostgres::dbListTables(con)
        tabelas <- tabelas[!tabelas %in% c("sugestoes")]
        da_sugestoes_manual <- purrr::map_dfr(tabelas, ler_sugestoes, con) %>%
          dplyr::group_by(id) %>%
          dplyr::summarise(
            dplyr::across(dplyr::starts_with("sol_"), ~dplyr::first(.[!is.na(.)])),
            .groups = "drop"
          )
        desconectar(con)

        da_sugestoes_auto <- inovaCNJ::da_incos %>%
          dplyr::select(id, dplyr::starts_with("sol_")) %>%
          dplyr::filter_at(
            dplyr::vars(dplyr::starts_with("sol_"), -sol_digito),
            dplyr::any_vars(!is.na(.))
          )

        da_sugestoes <- da_sugestoes_manual %>%
          dplyr::bind_rows(da_sugestoes_auto) %>%
          dplyr::distinct(id, .keep_all = TRUE)
      }

      todos_dados <- inovaCNJ::da_incos %>%
        dplyr::select(
          -dplyr::starts_with("inc_"),
          -dplyr::starts_with("sol_")
        )

      dados_para_arrumar <- todos_dados %>%
        dplyr::distinct(id, .keep_all = TRUE) %>%
        dplyr::semi_join(da_sugestoes, "id")

      dados_arrumados <- purrr::reduce(
        names(da_sugestoes)[-1],
        corrigir_coluna,
        da_sugestoes,
        .init = dados_para_arrumar
      )

      dados_arrumados

    })

    output$tabela <- reactable::renderReactable({

      shiny::validate(shiny::need(
        auth_admin(session),
        message = "Usuário precisa ser administrador para acessar essa parte da aplicação."
      ))

      reactable::reactable(
        dados_sugestoes(),
        selection = "single",
        onClick = "select"
      )

    })

    output$tabela_selecionada <- reactable::renderReactable({

      shiny::validate(shiny::need(
        auth_admin(session),
        message = "Usuário precisa ser administrador para acessar essa parte da aplicação."
      ))

      shiny::validate(shiny::need(
        dados_tabela(),
        message = "Nenhuma inconsistência selecionada."
      ))

      dados_tabela() %>%
        dplyr::select(-user, -input_date) %>%
        reactable::reactable(
          compact = TRUE,
          defaultPageSize = 8
        )

    })

    output$download <- shiny::downloadHandler(
      filename = function() {
        selecionado <- selected()
        linha <- dados_sugestoes()[selecionado, ]
        tab <- linha[["inconsistencia"]]
        paste0(Sys.Date(), "-inc_", tab, ".xlsx")
      },
      content = function(file) {
        dados_tabela() %>%
          dplyr::select(-user, -input_date) %>%
          writexl::write_xlsx(file)
      }
    )

    # exportar dados arrumados
    output$base_json <- shiny::downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "-base_arrumada.json")
      },
      content = function(file) {
        jsonlite::write_json(base_arrumada(), file)
      }
    )
    output$base_csv <- shiny::downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "-base_arrumada.csv")
      },
      content = function(file) {
        readr::write_csv(base_arrumada(), file)
      }
    )




  })
}

ler_sugestoes <- function(tab, con) {
  con %>%
    dplyr::tbl(tab) %>%
    dplyr::arrange(dplyr::desc(input_date)) %>%
    dplyr::select(id, input_date, dplyr::starts_with("sol_")) %>%
    dplyr::collect() %>%
    dplyr::distinct(id, .keep_all = TRUE)
}

corrigir_coluna <- function(arrumar, nm, arrumado) {

  nm_arrumar <- stringr::str_replace(nm, "^sol_", "info_")
  arrumado <- dplyr::select(arrumado, dplyr::all_of(c("id", nm)))

  arrumar %>%
    dplyr::inner_join(arrumado, "id") %>%
    dplyr::mutate({{nm_arrumar}} := dplyr::coalesce({{nm}}, {{nm_arrumar}})) %>%
    dplyr::select(-dplyr::all_of(nm))

}
