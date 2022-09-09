library(tidyverse)
library(tjsp)
library(abjutils)
library(rvest)

odebretch <- read_excel("odebretch.xlsx")%>%
  rename( Processo = 'Nº do Processo')%>%
  mutate(Processo = str_remove(Processo, "\\-"))%>%
  mutate(Processo = str_remove_all(Processo, "\\."))

odebretch_estadual <- odebretch %>%
  filter(Justiça ==  "Estadual")%>%
  pull(Processo)


odebretch_trabalho <- odebretch %>%
  filter(Justiça ==  "Trabalho")%>%
  pull()


reais2 <-function (x) 
{
  x %>% stringr::str_remove("R\\$ ")
  }

#baixando metadados

tjsp::autenticar(login = "", password = "")
tjsp::baixar_cpopg(processos = odebretch_estadual, diretorio = "estadual/metadados")

#lista comarcas cnj

library(readxl)
lista_foro_comarca <- read_excel("lista_foro_comarca.xlsx")

lista_foro_comarca<- lista_foro_comarca%>%
  filter(sigla== "TJSP")%>%

  processos_por_comarca<- tibble(odebretch_estadual)%>%
  rename("Processo"=odebretch_estadual)%>%
  mutate(dividido = abjutils::build_id(Processo))%>%
  separate_cnj(dividido)%>%
  full_join(lista_foro_comarca, by=c("O"="id_foro"))%>%
  tidyr::drop_na(Processo)%>%
  select(Processo, comarca, sigla)%>%
  rename("Comarca" = comarca)%>%
  mutate(Comarca= str_to_title(Comarca))%>%
  mutate(Comarca= case_when(
    Comarca =="Sao Paulo" ~ "São Paulo",
    Comarca == "Maua" ~ "Mauá",
    Comarca == "Carapicuiba" ~ "Carapicuíba",
    TRUE ~ as.character(Comarca)
  ))



#lendo metadados
odebretch_estadual_metadados <- tjsp::tjsp_ler_dados_cpopg(diretorio = "estadual/metadados", wide = TRUE)%>%
  rename("Processo"= processo, "Código do Processo"= codigo_processo)%>%
  
odebretch_estadual_metadados_tidy<- odebretch_estadual_metadados%>%
  mutate(digital= case_when(
    digital== TRUE ~ "Eletrônica",
    digital== FALSE ~ "Física",
  ))%>%
  rename( "Tramitação" = digital)%>%
  mutate(Juiz=str_to_title(Juiz))%>%
  mutate(`Valor da ação`= reais2(odebretch_estadual_metadados$`Valor da ação`))%>%
  mutate(Distribuição = str_extract(Distribuição, "\\d*\\/\\d*\\/\\d*"))%>%
  mutate("orgaos_secretarias" = case_when(
    str_detect(Vara, "Fisca") ~ "Vara de Execuções Fiscais",
    str_detect(Vara, "Fazenda Pública") ~ "Vara da Fazenda Pública",
    str_detect(Vara, "Federal") ~ "Vara Federal",
    str_detect(Vara, "Vara Cível|Vara Civel") ~ "Vara Cível",
    str_detect(Vara, "Juizado") ~ "Juizado Especial Cível",
    TRUE ~ "Verificar"
  ))%>%
  mutate("Local (Nº da Vara)" = dplyr::if_else(str_detect(Vara, "\\d"), str_extract(Vara, "\\d*"), "0"))%>%
  mutate(Área = case_when(
    orgaos_secretarias %in% c("Vara de Execuções Fiscais","Vara da Fazenda Pública","Vara Federal") ~ "Tributário",
    TRUE ~ as.character(Área)
  ))%>%
  rename("Orgãos/Secretarias"= orgaos_secretarias)%>%
  relocate(`Outros assuntos`, .after = Assunto)%>%
  #join com a continguência depois de ler partes. roda lá embaixo antes de rodar aqui.
  #Cuidado para nao confundir contigencia da df de partes com a couna contingencuia dentro do df contingencia vetor
  inner_join(contingênciavetor, by= "Processo")%>%
  relocate(Contingência, .after = `Valor da ação`)%>%
  rename("Valor da Causa" =`Valor da ação`)%>%
  #join com as comarcas feitas em cima, rodar antes la em cima.
  inner_join(processos_por_comarca, by= "Processo")%>%
  relocate(c( sigla, Comarca), .before = Foro)%>%
  rename("Tribunal"=sigla)
  
#######  ler partes

tjsp_ler_parte2 <- function (arquivos = NULL, diretorio = ".") 
{
  if (is.null(arquivos)) {
    arquivos <- list.files(path = diretorio, pattern = ".html", 
                           full.names = TRUE)
  }
  processos <- stringr::str_extract(arquivos, "\\d{20}")
  pb <- progress::progress_bar$new(total = length(arquivos))
  lista <- purrr::map2_dfr(arquivos, processos, purrr::possibly(~{
    pb$tick()
    x <- xml2::read_html(.x)
    x %>% xml2::xml_find_first("//table[@id='tableTodasPartes']") %>% 
      rvest::html_table() %>% setNames(c("tipo_parte", 
                                         "parte")) %>% tidyr::separate(parte, c("parte", 
                                                                                "representate"), sep = "(?<=\\w)\\s{10,}") %>% 
      tibble::add_column(processo = .y, .before = 1)
    
  }, NULL))
}




#ler partes principais - 130 rows
odebretch_estadual_partes <- tjsp_ler_partes(diretorio =  "estadual/metadados")
#le todas as partes (litisconsóricos - partes principais) - 58 rows
odebretch_estadual_partes2 <- tjsp_ler_parte2(diretorio =  "estadual/metadados")


#tirando duplas por join e remover os duplicados
#odebretch_estadual_partes3 <- full_join(odebretch_estadual_partes,odebretch_estadual_partes2, by= "processo", suffix=c(x="_x", y="_y"))
#odebretch_estadual_partes4 <- odebretch_estadual_partes3%>%
 # mutate(parte_y = ifelse( is.na(odebretch_estadual_partes3$parte_y), as.character(parte_x), as.character(parte_y)))


#tirando duplas filtrando antes para depois dar join - cuidado que se vc filtrar da ruim nas operaçoes acima

processos_litisconsórcio<- odebretch_estadual_partes2%>%
  pull(processo)
## filtrando processos litisconsorcio -  106 row
odebretch_estadual_partes<- odebretch_estadual_partes%>%
  filter(!processo %in% processos_litisconsórcio)

odebretch_estadual_partes5<- odebretch_estadual_partes%>%
 full_join(odebretch_estadual_partes2, by= NULL)%>%
  #pegando advs que ficaram dentro de parte
  mutate(parte=str_squish(parte))%>%
  mutate(representate = ifelse( is.na(odebretch_estadual_partes5$representate), str_extract(parte, "Advogado.*"), as.character(representate)))%>%
  separate(col= representate, sep = "Advogado:|Advogada:",into = c("Adv", "Adv1","Adv2", "Adv3", "Adv4", "Adv5"), extra= "merge")

odebretch_estadual_partes_tidy<- odebretch_estadual_partes5%>%
  mutate(Adv1 = ifelse( is.na(odebretch_estadual_partes5$Adv1), "Sem adv.", as.character(Adv1)))%>%
  select(!(Adv))%>%
  mutate(across(Adv1:Adv5, str_squish))%>%
  #padronização partes
  mutate(parte= case_when(
    str_detect(parte, "Bairro Novo") ~ "Bairro Novo Empreendimentos Imobiliários S.A.|08.758.695/0001-87",
    str_detect(parte, "Arena Itaquera") ~ "Arena Itaquera S.A.|14.278.551/0001-26",
    str_detect(parte, "Odebrech Realizações Imobiliárias") ~"Odebrecht Realizações Imobiliárias S.A.|06.206.132/0001-50", 
    str_detect(parte, "Odebrecht Serviços e Participações") ~"Odebrecht Serviços e Participações S.A.|10.904.193/0001-69",
    str_detect(parte, "Odebrecht Empreendimentos Imobiliários") ~"Odebrecht Empreendimentos Imobiliários S.A.",
    TRUE ~ as.character(parte)
  ))%>%
  rename("Contingência" = tipo_parte)%>%
  mutate(Contingência= case_when(
    Contingência %in% c("Reqte", "Imptte", "Exeqte")~ "PoloAtivo",
    Contingência %in% c("Reqdo", "Reqda", "Imptdo","Imptda", "Exectdo","Exectda")~ "PoloPassivo",
  ))%>%
  separate(col= parte, sep = "\\|", into= c("parte", "CPF/CNPJ"), extra = "merge")%>%
  mutate(Classificação=case_when(
    parte %in% c("Arena Itaquera S.A.","Bairro Novo Empreendimentos Imobiliários S.A.",
                 "Odebrecht Realizações Imobiliárias S.A.", "Odebrecht Serviços e Participações S.A.", 
                 "Odebrecht Empreendimentos Imobiliários S.A.") ~ "Empresa",
  ))%>%
  relocate(Classificação, .after = Contingência)

Empresa_regex <- "Bairro Novo|Arena Itaquera|Odebrech Realizações Imobiliárias|Odebrecht Serviços e Participações|Odebrecht Empreendimentos Imobiliários"

classificação_partes <- odebretch_estadual_partes_tidy %>%
  tidyr::pivot_wider (id_cols = c(processo, ), names_from = Contingência, values_from = parte)%>%
  mutate(Contingência=case_when(
    str_detect(PoloAtivo, Empresa_regex) ~ "Ativa",
    str_detect(PoloPassivo, Empresa_regex) ~ "Passiva",
    TRUE~ "Conferir"
  ))%>%
  unnest_wider(PoloAtivo)%>%
  rename("Processo"=1,"PoloAtivo1"=2, "PoloAtivo2"=3,"PoloAtivo3"=4, "PoloAtivo4"=5, "PoloAtivo5"=6, "PoloAtivo6"=7, "PoloPassivo"=8)%>%
  unnest_wider(PoloPassivo)%>%
  rename("PoloPassivo1"=8, "PoloPassivo2"=9,"PoloPassivo3"=10, "PoloPassivo4"=11, "PoloPassivo5"=12)%>%
  relocate(Contingência, .after=Processo)

contingênciavetor<- classificação_partes%>%
  select(Processo, Contingência)


#rename("Processo"=1,"Polo Ativo1"=2, "Polo Ativo2"=3,"Polo Ativo3"=4, "Polo Ativo4"=5, "Polo Ativo5"=6, "Polo Ativo6"=7, "PoloPassivo"=8)%>%
#unnest_wider(PoloPassivo)%>%
 # rename("Polo Passivo1"=8, "Polo Passivo2"=9,"Polo Passivo3"=10, "Polo Passivo4"=11, "Polo Passivo5"=12)%>%
  



#tentando classificar
  tidyr::pivot_wider (id_cols = processo, names_from = Contingência, values_from = parte)%>%
  mutate(polo_ativo = case_when(
  str_detect(PoloAtivo, Empresa_regex) ~ "Empresa",
  TRUE ~ "Parte Contrária"),
  polo_passivo= case_when(
    str_detect(PoloPassivo, Empresa_regex) ~ "Litisconsorte",
    TRUE ~ "Parte Contrária"))%>%
  str_remove_all()
    unnest(PoloPassivo)%>%
  
#modo abj

group_by(id_porcesso,polo)%>%
summarise( tipo= dplyr::case_when(any(tipo == "npf")~ npf, TRUE ~ pf), part= paste(nome, collapse = ", "), .groups = "drop") %>%
tidyr::pivot_wider (id_cols = idProcesso, names_from = c(polo), values_from = c(part, tipo)) %>%
  tidyr::unite(part_tipo_ligitio, tipo_ativo, tipo_passivo, sep = "_")



##baixando docs
  tjsp::autenticar(login = "", password = "")
tjsp::tjsp_baixar_tabela_docs(process = odebretch_estadual, diretorio = "documentos")

odebretch_estadual_tabelasdocs<- tjsp::tjsp_ler_tabela_docs(diretorio = "documentos")%>%
  count(processo)%>%
  pull(processo)




#Movimentação

odebretch_estadual_mov <- tjsp::ler_movimentacao_cposg(diretorio = "estadual/metadados")



odebretch_estadual_mov_agregados <- odebretch_estadual_mov%>%
  mutate(movimentacao= str_to_lower(movimentacao))%>%
  mutate(status=case_when(
    str_detect(movimentacao, "arquivado")~ "Arquivado"
  ))

ler_ultimas_mov <- function (arquivos = NULL, diretorio = ".") 
{
  if (is.null(arquivos)) {
    arquivos <- list.files(path = diretorio, pattern = ".html", 
                           full.names = TRUE)
  }
  pb <- progress::progress_bar$new(total = length(arquivos))
  purrr::map_dfr(arquivos, purrr::possibly(~{
    pb$tick()
    processo <- stringr::str_extract(.x, "\\d{20}")
    texto <- xml2::read_html(.x) %>% xml2::xml_find_first(xpath = "//table/tbody[@id='tabelaUltimasMovimentacoes']")
    data <- xml2::xml_find_all(texto, ".//td[@width='120']") %>% 
      xml2::xml_text(trim = TRUE) %>% lubridate::dmy()
    mov <- xml2::xml_find_all(texto, ".//td[@style='vertical-align: top; padding-bottom: 5px']") %>% 
      xml2::xml_text(trim = TRUE)
    tibble::tibble(processo = processo, data = data, movimentacao = mov)
  }, otherwise = NULL))
}

odebretch_estadual_ultimas_mov <- ler_ultimas_mov(diretorio = "estadual/metadados")%>%
  mutate(movimentacao = str_to_lower(movimentacao))%>%
  mutate(movimentacao = abjutils::rm_accent(movimentacao))%>%
  mutate(status=case_when(
    str_detect(movimentacao, "arquivado definitivamente")~ "Arquivado definitivamente",
    str_detect(movimentacao, "arquivado provisoriamente")~ "Arquivado provisoriamente",
    str_detect(movimentacao, "(?=aguardando).*(transito)")~ "aguardando Trânsito",
    str_detect(movimentacao, "\b(?<![certifi][apos]).*(transito)")~ "Transitou em Julgado",
    str_detect(movimentacao, "baixa") ~ "Baixado",
    str_detect(movimentacao, "suspenso")~ "Suspenso"
  ))

#Levando pro Excel
library(XLConnect)
XLConnect::
  
  
xlsx::write.xlsx(odebretch_estadual_metadados_tidy, "Odebrecht TJSP.xlsx", sheet="Dados do Processo", append= FALSE)

writexl::write_xlsx(x= list(odebretch_estadual_metadados_tidy, classificação_partes,
                         odebretch_estadual_partes_tidy,odebretch_estadual_ultimas_mov, odebretch_estadual_mov), path = "Odebrecht TJSP.xlsx",
                    col_names= TRUE)

install.packages("xlsx")
