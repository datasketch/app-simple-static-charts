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
library(shinybusy)


frtypes_doc <- suppressWarnings(yaml::read_yaml("conf/frtypes.yaml"))
available_ftypes <- names(frtypes_doc)

ui <- panelsPage(
  showDebug(),
  useShi18ny(),
  disconnectMessage(
    text = "Tu sesión ha finalizado, si tienes algún problema trabajando con la app por favor contáctanos y cuéntanos qué ha sucedido // Your session has ended, if you have any problem working with the app please contact us and tell us what happened.",
    refresh = "REFRESH",
    background = "#ffffff",
    colour = "#435b69",
    size = 14,
    overlayColour = "#2a2e30",
    overlayOpacity = 0.85,
    refreshColour = "#ffffff",
    css = "padding: 4.8em 3.5em !important; box-shadow: 0 1px 10px 0 rgba(0, 0, 0, 0.1) !important;"
  ),
  busy_start_up(
    loader = tags$img(
      src = "img/loading_gris.gif",
      width = 100
    )),
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
        body = div(uiOutput("info_ftype"),
                   #verbatimTextOutput("aver"),
                   withLoader( plotOutput("view_gg_viz"),
                              type = "image", loader = "img/loading_gris.gif")),
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
    files <- paste0("data/samples/", list_files)[1:2]
    names(files) <- i_(c("emissions", "population"), lang = lang)
    
    
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
    tryCatch(hotr("data_input", data = inputData()(), options = list(height = 530)), 
             error = function(e) {infomessage(HTML(i_("data_error", lang = lang())))})
  })
  
  
  
  data_load <- reactive({
    req(inputData()())
    input$data_input$data
  })
  
  dic_load <- reactive({
    req(inputData()())
    input$data_input$dic
    
  })
  
  
  indicator <- reactive({
    if (is.null(inputData()())) return()
    if (is.null(input$data_input$dic)) return()
    all(names(inputData()()) %in% input$data_input$dic$label)
  })
  
  var_select <- reactiveValues(id_default = NULL, all_vars_data = NULL)
  
  observe({
    if(is.null(inputData()())) return()
    
    req(dic_load())
    d <- dic_load()
    cat_var <- grep("Cat|Yea|Dat", d$hdType)
    num_var <- grep("Num", d$hdType)
    
    
    if (!identical(cat_var, integer())) {
      n_cats <- ifelse(length(grep("Cat|Yea|Dat", d$hdType)) > 1, 2, 1)
      v <-  sample(d$id[grep("Cat|Yea|Dat", d$hdType)], n_cats)
      if (!identical(num_var, integer())) {
        v <- c(v, sample(d$id[grep("Num", d$hdType)], 1))
      }
      var_select$id_default <- v
    } else {
      var_select$id_default <- NULL
    }
    
    var_select$all_vars_data <- set_names(d$id, d$label)
    
    if (sum(names(var_select$all_vars_data) %in% names(inputData()())) < 1) var_select$id_default <- NULL
    
  })
  
  
  output$select_var <- renderUI({
    if (is.null(inputData()())) return(infomessage(HTML(i_("data_wait", lang = lang()))))
    if (is.null(var_select$all_vars_data)) return()
    
    # if (sum(names(var_select$all_vars_data) %in% names(inputData()())) < 1) return()
    var_s <- var_select$id_default
    
    
    selectizeInput("var_order",   div(class="title-data-select", i_("var_selector", lang())),
                   choices =  var_select$all_vars_data,
                   multiple = TRUE,
                   selected =  var_s,
                   options = list(plugins= list('remove_button', 'drag_drop')))
    
  })
  
  var_default <- reactiveValues(id = 0)
  observe({
    if (is.null(inputData()())) {
      var_default$id <- 0
    } else {
      var_default$id <- 1
    }
    
  })
  
  observe({
    if(var_default$id == 0) {
      updateSelectizeInput(session, "var_order", selected = character(0))
      session$sendCustomMessage(type = "resetValue", message = "var_order")
    }
  })
  
  var_plot <- reactive({
    var_sel <- input$var_order
    var_sel
  })
  
  dic_draw <- reactive({
    
    req(inputData()())
    req(dic_load())
    if (is.null(var_plot())) return()
    d <- dic_load()
    
    var_sel <- var_plot()
    
    d <- d[d$id %in% var_sel,]
    
    l_f <- strsplit(available_ftypes, "-")
    l_f <- map(l_f, function(i){
      paste0(sort(i), collapse = "-")
    })
    
    ind_ftype <- which(sapply(l_f, function(y){
      paste0(sort(d$hdType), collapse = "-") %in% y
    })
    )[1]
    ind <- strsplit(available_ftypes[ind_ftype], "-") %>% unlist()
    
    order <- union(ind, unique(d[["hdType"]]))
    d <- d[order(match(d[["hdType"]], order)), ]
    
    d
  })
  
  data_draw <- reactive({
    
    if(is.null(indicator())) return()
    if(!indicator()) return()
    req(data_load())
    if (is.null(var_plot())) return()
    req(dic_draw())
    d <- data_load()[dic_draw()[["id"]]]
    
    names(d) <- dic_draw()[["label"]][match(dic_draw()[["id"]], names(d))]
    
    if (is.null(input$grouping) | isFALSE(input$grouping %in% names(d))) {
      d <- d
    } else {
      d <- d[c(input$grouping, setdiff(names(d), input$grouping))]
    }
    
    d
  })
  
  
  
  output$aver <- renderPrint({
    print(var_plot())
    print(ftype_draw())
    print(input$grouping)
    data_draw()
    
  })
  
  
  
  ###########################
  
  ftype_draw <- reactive({
    if (is.null(inputData()())) return()
    req(dic_draw())
    f_t <- paste0(dic_draw()$hdType, collapse = "-")
    if (!(f_t %in% available_ftypes)) return()
    f_t
  })
  
  output$info_ftype <- renderUI({
    if (is.null(inputData()())) return()
    if (is.null(var_plot())) return(infomessage(HTML(i_("ftype_null", lang = lang()))))
    if (!is.null(ftype_draw())) return()
    infomessage(HTML(i_("ftype_ms", lang = lang())))
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
    req(possible_viz())
    suppressWarnings(
      buttonImageInput('viz_selection',
                       div(class="title-data-select",i_('viz_type', lang())),
                       images = possible_viz(),
                       path = 'img/svg/',
                       #format = 'svg',
                       active = actual_but$active)
    )
  })
  
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
  hasdataCat <- reactive({
    if (is.null(inputData()())) return(FALSE)
    sum(c("Cat", "Yea")  %in% dic_draw()$hdType) > 0
  })
  
  data_cat <- reactive({
    if (is.null(inputData()())) return()
    req(dic_draw())
    print(dic_draw()$label[dic_draw()$hdType %in% c("Cat", "Yea")])
    dic_draw()$label[dic_draw()$hdType %in% c("Cat", "Yea")]
  })
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
    length(unique(data_draw()[,1]))
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
    #print(info_org$org)
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
    #print(ctype)
    
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
  
  
  output$download <- renderUI({
    lb <- i_("download_viz", lang())
    dw <- i_("download", lang())
    gl <- i_("get_link", lang())
    mb <- list(textInput("name", i_("gl_name", lang())),
               textInput("description", i_("gl_description", lang())),
               selectInput("license", i_("gl_license", lang()), choices = c("CC0", "CC-BY")),
               selectizeInput("tags", i_("gl_tags", lang()), choices = list("No tag" = "no-tag"), multiple = TRUE, options = list(plugins= list('remove_button', 'drag_drop'))),
               selectizeInput("category", i_("gl_category", lang()), choices = list("No category" = "no-category")))
    downloadDsUI("download_data_button", dropdownLabel = lb, text = dw, formats = c("jpeg", "png", "svg", "pdf"), 
                 display = "dropdown", dropdownWidth = 180, getLinkLabel = gl, modalTitle = gl, modalBody = mb,
                 modalButtonLabel = i_("gl_save", lang()), modalLinkLabel = i_("gl_url", lang()), modalIframeLabel = i_("gl_iframe", lang()),
                 modalFormatChoices = c("PNG" = "png", "SVG" = "svg"))
  })
  
  
  
  par <- list(user_name = "brandon")
  url_par <- reactive({
    url_params(par, session)
  })
  
  pin_ <- function(x, bkt, ...) {
    x <- dsmodules:::eval_reactives(x)
    bkt <- dsmodules:::eval_reactives(bkt)
    nm <- input$`download_data_button-modal_form-name`
    if (!nzchar(input$`download_data_button-modal_form-name`)) {
      nm <- paste0("saved", "_", gsub("[ _:]", "-", substr(as.POSIXct(Sys.time()), 1, 19)))
      updateTextInput(session, "download_data_button-modal_form-name", value = nm)
    }
    dv <- dsviz(x,
                name = nm,
                description = input$`download_data_button-modal_form-description`,
                license = input$`download_data_button-modal_form-license`,
                tags = input$`download_data_button-modal_form-tags`,
                category = input$`download_data_button-modal_form-category`)
    dspins_user_board_connect(bkt)
    Sys.setlocale(locale = "en_US.UTF-8")
    pin(dv, bucket_id = bkt)
  }
  
  
  observe({
    req(gg_viz())
    if (is.null(url_par()$inputs$user_name)) return()
    
    downloadDsServer("download_data_button", element = reactive(gg_viz()),
                     formats = c("svg", "jpeg", "pdf", "png"),
                     errorMessage = i_("error_down", lang()),
                     modalFunction = pin_, reactive(gg_viz()),
                     bkt = url_par()$inputs$user_name)
  })
  
  
  

}

shinyApp(ui, server)