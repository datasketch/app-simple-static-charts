library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shi18ny)
library(dsmodules)
library(ggmagic)
library(treemapify)
library(hotr)
library(dsthemer)
library(knitr)
library(dspins)
library(shinycustomloader)
library(shinydisconnect)


frtypes_doc <- suppressWarnings(yaml::read_yaml("conf/frtypes.yaml"))
available_ftypes <- names(frtypes_doc)

ui <- panelsPage(
  showDebug(),
  useShi18ny(),
  disconnectMessage(
    text = "Oh no!, la sesión a finalizado, si estabas trabajando en la app, por favor contacta a soporte y cuentanos que ha sucedido//Oh no, the session has ended, if you were working on the app, please contact support and tell us what has happened",
    refresh = "O, intenta de nuevo//Or try again",
    background = "#385573",
    colour = "white",
    overlayColour = "grey",
    overlayOpacity = 0.3,
    refreshColour = "#FBC140"
  ),
  langSelectorInput("lang", position = "fixed"),
  panel(title = ui_("upload_data"),
        collapse = TRUE,
        width = 300,
        body = uiOutput("table_input")),
  panel(title = ui_("view_data"), 
        collapse = FALSE,
        width = 450,
        body = div(
          div(
            uiOutput("select_var"),
            withLoader(uiOutput("dataset"),type = "image", loader = "img/loading_gris.gif")
          )
        )),
  panel(title = ui_("edit_viz"),
        color = "chardonnay",
        width = 350,
        body = uiOutput("controls")
  ),
  panel(title =  ui_("view_viz"),
        title_plugin = uiOutput("download"),
        color = "chardonnay",
        can_collapse = FALSE,
        body = withLoader(uiOutput("end_sol"),
                          type = "image", loader = "img/loading_gris.gif"),
        footer = uiOutput("viz_icons"))
)

server <- function(input, output, session) {
  
  # Idiomas -----------------------------------------------------------------
  
  i18n <- list(
    defaultLang = "en",
    availableLangs = c("es","en")
  )
  lang <- callModule(langSelector,"lang", i18n = i18n, showSelector=FALSE)
  
  
  observeEvent(lang(),{
    uiLangUpdate(input$shi18ny_ui_classes, lang())
  })
  
  # Modulo de carga de datos ------------------------------------------------
  
  output$table_input <- renderUI({
    
    choices <- c("sampleData", "pasted", "fileUpload", "googleSheets")
    if (lang() == "") return()
    names(choices) <- i_(c("sample", "paste", "upload_doc", "google"), lang = lang())
    tableInputUI("initial_data",
                 div(class="title-data-select",i_("table_label", lang())),
                 choices = choices,
                 selected =  "sampleData")
  })
  
  # lista labels módulos en idioma seleccionado
  labels <- reactive({
    
    lang <- lang()
    if (lang == "") return()
    list_files <- list.files("data/samples/")
    if (is.null(list_files)) return()
    list_files <- list_files[grep(paste0('_', lang), list_files)]
    files <- paste0("data/samples/", list_files)
    names(files) <- i_(c("emissions", "population", "leaders"), lang = lang)
    
    
    list(sampleLabel = i_("select_sample", lang()), 
         sampleFiles = files,
         
         pasteLabel = i_("paste", lang()), 
         pasteValue = "", 
         pastePlaceholder = i_("paste_pl", lang()), 
         pasteRows = 5,
         
         uploadLabel = i_("upload_lb", lang()),
         uploadButtonLabel = i_("upload_bt_lb", lang()), 
         uploadPlaceholder = i_("upload_pl", lang()),
         
         googleSheetLabel = i_("google_sh_lb", lang()),
         googleSheetValue = "",
         googleSheetPlaceholder = i_("google_sh_pl", lang()),
         googleSheetPageLabel = i_("google_sh_pg_lb", lang())
    )
  })
  
  inputData <- eventReactive(labels(), {
    do.call(tableInputServer, c("initial_data", labels()))
  })
  
  # output$debug <- renderPrint({
  #   inputData()
  # })
  
  # Vista de datos ----------------------------------------------------------
  
  output$dataset <- renderUI({
    suppressWarnings(
      hotr("data_input", data = inputData(), options = list(height = 530))
    )
  })
  
  data_fringe <- reactive({
    req(input$data_input)
    suppressWarnings( hotr::hotr_fringe(input$data_input))
  })
  
  data_load <- reactive({
    req(data_fringe())
    data_fringe()$data
  })
  
  dic_load <- reactive({
    req(data_fringe())
    data_fringe()$dic
  })
  
  
  output$select_var <- renderUI({
    
    req(dic_load())
    dic_load <- dic_load() 
    data_ftype <- paste0(dic_load$hdType, collapse = "-")
    
    if (data_ftype %in% available_ftypes) {
      sample_end <- dic_load$id
    } else {
      if (sum(c("Cat", "Dat", "Yea") %in% dic_load$hdType) > 0) {
        sample_cats <- sample(dic_load$id[grepl("Cat|Dat|Yea", dic_load$hdType)], 1)}
      if (sum(c("Num") %in% dic_load$hdType) > 0) {
        sample_nums <-  sample(dic_load$id[grepl("Num", dic_load$hdType)], 1)}
      sample_end <- c(sample_cats, sample_nums)
    }
    
    
    if(is.null(sample_end)) return()
    
    list_var <- dic_load$id
    names(list_var) <- dic_load$label[match(list_var, dic_load$id)]
    names(sample_end) <- dic_load$label[match(sample_end, dic_load$id)]
    
    selectizeInput("var_order",
                   div(class="title-data-select",i_("var_selector", lang())),
                   choices = list_var,
                   multiple = TRUE,
                   selected = sample_end,
                   options = list(plugins= list('remove_button', 'drag_drop'))
    )
  })
  
  
  variables <- reactiveValues(id = NULL)
  
  
  observe({
    if (is.null(input$var_order)) return()
    variables$id <- input$var_order 
  })
  
  
  
  # Preparación data para graficar ------------------------------------------
  
  
  dic_draw <- reactive({
    req(dic_load())
    if (is.null(variables$id)) return()
    var_select <- variables$id
    dic_load()[dic_load()$id %in% var_select,]
  })
  
  data_draw <- reactive({
    if (is.null(variables$id)) return()
    if (is.null(dic_draw())) return()
    var_select <- dic_draw()$id 
    d <- data_load()[var_select]
    names(d) <- dic_draw()$label
    d
  })
  
  
  
  ftype_draw <- reactive({
    if (is.null(dic_draw())) return()
    paste0(dic_draw()$hdType, collapse = "-")
  })
  
  possible_viz <- reactive({
    if (is.null(ftype_draw())) return()
    frtypes_doc[[ftype_draw()]]
  })
  
  
  actual_but <- reactiveValues(active = 'bar')
  
  observe({
    viz_rec <- possible_viz()
    if (is.null(viz_rec)) return()
    if (is.null(input$viz_selection)) return()
    if (!( input$viz_selection %in% viz_rec)) {
      actual_but$active <- viz_rec[1]
    } else {
      actual_but$active <- input$viz_selection
    }
  })
  
  
  output$viz_icons <- renderUI({
    buttonImageInput('viz_selection',
                     div(class="title-data-select",i_('viz_type', lang())),
                     images = possible_viz(),
                     path = 'img/svg/',
                     format = 'svg',
                     active = actual_but$active)
  })
  # 
  # Renderizar inputs con parmesan ------------------------------------------
  
  parmesan <- parmesan_load()
  parmesan_input <- parmesan_watch(input, parmesan)
  parmesan_alert(parmesan, env = environment())
  parmesan_lang <- reactive({i_(parmesan, lang(), keys = c("label", "inputs", "text"))})
  output_parmesan("controls",
                  parmesan = parmesan_lang,
                  input = input,
                  output = output,
                  env = environment())
  
  # reactivos para los condicionales del layout -----------------------------
  
  viz_select <- reactive({
    if (is.null(actual_but$active)) return()
    actual_but$active
  })
  
  hor_title <- reactive({
    req(data_draw())
    
    tx <- names(data_draw())
    if (ncol(data_draw()) == 2) {
      tx <- tx[1]
    } else {
      tx <- tx[2]
    }
    tx
  })
  
  ver_title <- reactive({
    req(data_draw())
    tx <- names(data_draw())
    if (ncol(data_draw()) == 2) {
      tx <- tx[2]
    } else {
      tx <- tx[3]
    }
    tx
  })
  
  tooltip_info <- reactive({
    i_("tool_info", lang = lang())
  })
  
  
  only_cat <- reactive({
    if (is.null(dic_draw())) return()
    dic_draw()$label[!grepl("Num", dic_draw()$hdType)]
  })
  
  isTruePerc <- reactive({
    if(is.null(input$percentage)) return()
    input$percentage
  })
  
  show_dataLabels_format <- reactive({
    if (is.null(input$dataLabels_show)) return()
    input$dataLabels_show
  })
  
  hasdataNum <- reactive({
    if (is.null(dic_draw())) return()
    TRUE %in% grepl('Num', dic_draw()$hdType)
  })
  
  numberCats <- reactive({
    if (is.null(dic_draw())) return()
    length(grep('Cat|Yea', dic_draw()$hdType))
  })
  
  highlightNull <- reactive({
    is.null(input$highlight_value)
  })
  
  order_opts <- reactive({
    if (is.null(ftype_draw())) return()
    if (ftype_draw() %in% c("Cat-Cat", "Cat-Cat-Num", "Cat-Yea", "Cat-Yea-Num")) {
      cat <-  unique(data_draw()[,2])
    } else {
      cat <- unique(data_draw()[,1])
    }
    cat
  })
  
  order_legend_opts <- reactive({
    if (is.null(data_draw)) return()
    unique(data_draw()[,1])
  })
  
  agg_opts <- reactive({
    choices <- c("sum", "mean", "median")
    names(choices) <- i_(c("sum", "mean", "median"), lang = lang())
    choices
  })
  
  bar_type_opts <- reactive({
    choices <- c("grouped", "stacked")
    names(choices) <- i_(c("grouped", "stacked"), lang = lang())
    choices
  })
  
  format_cat_opts <- reactive({
    choices <- c("As Title", "UPPER", "lower", "Firstupper")
    names(choices) <- i_(c("Title", "Upper", "Lower", "Firstupper"), lang = lang())
    choices
  })
  
  legend_position_opts <- reactive({
    choices <- c("bottom", "top", "left", "right")
    choices
  })
  
  num_cat <- reactive({
    req(data_draw())
    if (numberCats() >= 2) return()
    dim(unique(data_draw()[,1]))[1]
  })
  
  hasdataNA <- reactive({
    TRUE
    # if (ftype_draw() %in% c("Cat-Cat", "Cat-Cat-Num", "Cat-Yea", "Cat-Yea-Num")) {
    #   hasNA <- anyNA(data_draw()[,2])
    # } else {
    #   hasNA <- anyNA(data_draw()[,1])
    # }
    # hasNA
  })
  
  haslegendNA <- reactive({
    hasNA <- anyNA(data_draw()[,1])
    hasNA
  })
  
  input_drop_na <- reactive({
    input$drop_na
  })
  
  conditional_legend_show <- reactive({
    ft <- dic_draw()$hdType
    if (is.null(ft)) return()
    if (length(ft) > 2) {
      cl <- TRUE
      if (actual_but$active %in% c("treemap")) cl <- FALSE
    } else {
      cl <- FALSE
      if (actual_but$active %in% c("pie", "donut")) cl <- TRUE
    }
    cl
  })
  
  input_legend_show <- reactive({
    input$legend_show
  })
  
  sort_opts <- reactive({
    choices <- c("noypunto", "asc", "desc")
    names(choices) <- i_(c("noypunto", "asc", "desc"), lang = lang())
    choices
  })
  

  
  
  # Valores de URL ----------------------------------------------------------
  
  info_url <- reactiveValues(id = NULL)
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (identical(query, list())) return()
    info_url$id <- query
  })
  info_org <- reactiveValues(name = NULL, id = NULL, org = "public")
  
  observe({
    info_url <- info_url$id
    if (is.null(info_url)) return()
    available_params <- names(info_url)
    
    if ("org" %in% available_params) {
      info_org$org <- info_url$org }
    
    if ("org_name" %in% available_params) {
      info_org$name <- info_url$org_name }
    
    if ("org_id" %in% available_params) {
      info_org$id <- info_url$org_id }
    
  })
  
  
  
  
  
  # Adicionar theme ---------------------------------------------------------
  
  
  theme_load <- reactive({
    theme_select <- input$theme
    print(info_org$org)
    if (is.null(theme_select)) return()
    th <- dsthemer_get(info_org$org, theme = theme_select)
    if (is.null(th)) return()
    th
  })
  
  background <- reactive({
    req(theme_load())
    theme_load()$background_color
    
  })
  
  theme_draw <- reactive({
    req(theme_load())
    l <- theme_load()
    l <- l[setdiff(names(l), c('logo','background_color', 'palette_colors', 'branding_include'))]
    l
  })
  
  
  color_by_opts <- reactive({
    names(data_draw())
  })
  
  agg_palette <- reactive({
    req(theme_load())
    colors <- theme_load()$palette_colors
    colors
  })
  
  # # Renderizar highchart plot -----------------------------------------------
  # 
  opts_viz <- reactive({
    
    if (is.null(info_org$org)) return()
    opts_viz <- parmesan_input()
    if (is.null(opts_viz)) return()
    opts_viz <- opts_viz[setdiff(names(opts_viz), c('theme'))]
    opts_viz$logo <- info_org$org
    opts_viz
  })
  
  viz_name <- reactive({
    req(data_draw())
    if (is.null(ftype_draw())) return()
    if (ftype_draw() == "") return()
    if (!(ftype_draw() %in% available_ftypes)) return()
    
    ctype <- gsub("-", "", ftype_draw())
    print(ctype)
    
    gtype <- actual_but$active
    if (is.null(gtype)) return()
    typeV <- paste0('gg_', gtype, '_', ctype)
    typeV
  })
  
  gg_viz <- reactive({
    
    if (is.null(viz_name())) return()
    
    viz <- do.call(viz_name(), c(list(data = data_draw(),
                                      opts_viz(),
                                      theme = theme_draw()
    )))
    viz
    
  })
  
  output$view_gg_viz <- renderPlot({
    req(gg_viz())
    suppressWarnings(
      gg_viz()
    )
  })
  
  
  output$end_sol <- renderUI({
    if (ftype_draw() %in% available_ftypes) {
      s <- plotOutput("view_gg_viz", height = 500)
    } else {
      s <- infomessage(type = "warning", p(style="max-width:300px;",i_("ftypes_warning", lang())))
    }
    s
  })
  
  

}

shinyApp(ui, server)