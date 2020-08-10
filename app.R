library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shi18ny)
library(dsmodules)
library(ggmagic)
library(hotr)
library(dsthemer)
library(knitr)
library(shinycustomloader)
library(dspins)

styles <- "
.shiny-output-error { 
visibility: hidden !important; }
.shiny-output-error:before { 
visibility: hidden !important; }
.title-data-select {
 color: #B70F7F;
 font-weight: 700;
 text-transform: uppercase;
}
"

frtypes_doc <- suppressWarnings(yaml::read_yaml("conf/frtypes.yaml"))

ui <- panelsPage(
  useShi18ny(),
  langSelectorInput("lang", position = "fixed"),
  styles = styles,
  panel(title = ui_("upload_data"),
        collapse = TRUE,
        width = 300,
        body = uiOutput("table_input")),
  panel(title = ui_("view_data"), 
        collapse = FALSE,
        width = 450,
        body = div(
          uiOutput("select_var"),
         withLoader(
            uiOutput("dataset"), 
           type = "image", loader = "loading_gris.gif"),
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
        body = plotOutput("view_gg"),
        footer = uiOutput("viz_icons"))
)

server <- function(input, output, session) {
  
# Idiomas -----------------------------------------------------------------

  i18n <- list(
    defaultLang = "es",
    availableLangs = c("es","en")
  )
  lang <- callModule(langSelector,"lang", i18n = i18n, showSelector=FALSE)
  
  
  observeEvent(lang(),{
    uiLangUpdate(input$shi18ny_ui_classes, lang())
  })

# Modulo de carga de datos ------------------------------------------------

  output$table_input <- renderUI({
    choices <- c("sampleData", "pasted", "fileUpload", "googleSheets")
    names(choices) <- i_(c("sample", "paste", "upload_doc", "google"), lang = lang())
    tableInputUI("initial_data",
                 choices = choices,
                 selected =  "sampleData")
  })
  
  # lista labels módulos en idioma seleccionado
  labels <- reactive({
    
    lang <- lang()
    list_files <- list.files("data/samples/")
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
    do.call(callModule, c(tableInput,
                          "initial_data",
                          labels()))
  })
  
  output$debug <- renderPrint({
    inputData()
  })

  # Vista de datos ----------------------------------------------------------
  
  output$dataset <- renderUI({
    if(is.null(inputData()))return()
    suppressWarnings(
      hotr("data_input", data = inputData(), options = list(height = 530))
    )
  })
  
  data_fringe <- reactive({
    suppressWarnings( hotr::hotr_fringe(input$data_input))
  })
  
  data_load <- reactive({
    data_fringe()$data
  })
  
  dic_load <- reactive({
    data_fringe()$dic
  })
  
  output$select_var <- renderUI({
    #print(dic_load())
    available_fTypes <- names(frtypes_doc)
    data_ftype <- data_fringe()$frtype
    if(is.null(dic_load)) return()
    
    if (data_ftype %in% available_fTypes) { 
      data_order <- dic_load()$id
    } else if (grepl("Cat|Yea|Dat",data_ftype)&grepl("Num",data_ftype)){
      data_order <- c(dic_load()$id[grep("Cat|Yea|Dat", dic_load()$hdType)[1]],
                      dic_load()$id[grep("Num", dic_load()$hdType)[1]])
    } else {
      data_order <- dic_load()$id[grep("Cat|Num", dic_load()$hdType)[1]]
    }
    
    list_var <- dic_load()$id
    if (is.null(list_var)) return()
    names(list_var) <- dic_load()$label[match(list_var, dic_load()$id)]
    
    
    selectizeInput("var_order",
                   div(class = "title-data-select", i_("var_selector", lang())),
                   choices = list_var,
                   multiple = TRUE, 
                   selected = data_order,
                   options = list(plugins= list('remove_button', 'drag_drop'))
    )
  })
  
  # Preparación data para graficar ------------------------------------------
  
  data_draw <- reactive({
    var_select <- input$var_order
    if (is.null(var_select)) return()
    d <- data_load()[var_select] 
    names(d) <- dic_draw()$label
    d
  })
  
  dic_draw <- reactive({
    var_select <- input$var_order
    if (is.null(var_select)) return()
    dic_load() %>% filter(id %in% var_select)
  })
  
  output$trip <- renderPrint({
    dic_draw()
  })
  ftype_draw <- reactive({
    if (is.null(dic_draw())) return()
    paste0(dic_draw()$hdType, collapse = "-")
  })
  
  possible_viz <- reactive({
    if (is.null(ftype_draw())) return()
    frtypes_doc[[ftype_draw()]]
  })
  
  
  actual_but <- reactiveValues(active = 'line')
  
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
                     div(class = "title-data-select",i_('viz_type', lang())), 
                     images = possible_viz(),
                     path = 'img/svg/',
                     format = 'svg',
                     active = actual_but$active)
  })
 
  # reactivos para los condicionales del layout -----------------------------
  
  viz_select <- reactive({
    actual_but$active 
  })
  
  hor_title <- reactive({
    #if (is.null(data_draw())) return()
    tx <- names(data_draw())
    if (ncol(data_draw()) == 2) {
      tx <- tx[1]
    } else {
      tx <- tx[2]
    }
    tx
  })
  
  ver_title <- reactive({
    #if (is.null(data_draw())) return()
    tx <- names(data_draw())
    if (ncol(data_draw()) == 2) {
      tx <- tx[2]
    } else {
      tx <- tx[3]
    }
    tx
  })
  
  only_bar <- reactive({
    actual_but$active == "bar"
  })
  
  show_dataLabels_format <- reactive({
    input$dataLabels_show
  })
  
  hasdataNum <- reactive({
    TRUE %in% grepl('Num', dic_draw()$hdType)
  })
  
  only_cat <- reactive({
    if (is.null(dic_draw())) return()
    dic_draw()$label[!grepl("Num", dic_draw()$hdType)]
  })
  
  numberCats <- reactive({
    print( length(grep('Cat|Yea', dic_draw()$hdType)))
    length(grep('Cat|Yea', dic_draw()$hdType))
  })
  
  isTruePerc <- reactive({
    if(is.null(input$percentage)) return()
    input$percentage
  })
  
  highlightNull <- reactive({
    is.null(input$highlight_value)
  })
  
  order_opts <- reactive({
    if (ftype_draw() %in% c("Cat-Cat", "Cat-Cat-Num", "Cat-Yea", "Cat-Yea-Num")) {
      cat <-  unique(data_draw()[,2]) 
    } else {
      cat <- unique(data_draw()[,1])   
    }
    cat
  })
  
  order_legend_opts <- reactive({
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
  
  
  hasdataNA <- reactive({
    if (ftype_draw() %in% c("Cat-Cat", "Cat-Cat-Num", "Cat-Yea", "Cat-Yea-Num")) {
      hasNA <- anyNA(data_draw()[,2]) 
    } else {
      hasNA <- anyNA(data_draw()[,1])   
    }
    hasNA
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
  
  legend_layout_opts <- reactive({
    choices <- c("horizontal", "vertical", "proximate")
    #names(choices) <- i_(c("horizontal", "vertical", "proximate"), lang = lang())
    choices
  })
  
  legend_align_opts <- reactive({
    choices <- c("center", "left", "right")
    #names(choices) <- i_(c("left", "center", "right"), lang = lang())
    choices
  })
  
  legend_verticalAlign_opts <- reactive({ 
    choices <- c("bottom", "top", "middle")
    #names(choices) <- i_(c("bottom", "top", "middle"), lang = lang())
    choices
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
  
  
  
  # Valores de URL ----------------------------------------------------------
  
  info_url <- reactiveValues(id = NULL)
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (identical(query, list())) return()
    info_url$id <- query
  })
  
  info_org <- reactiveValues(name = "public")
  observe({
    info_url <- info_url$id
    if (is.null(info_url)) return()
    available_params <- names(info_url)
    
    if ("org" %in% available_params) {
      info_org$name <- info_url$org }
  })
  
  info_user <- reactiveValues(id = "5efa17497caa2b00156a6468", name = "brandon")
  observe({
    info_url <- info_url$id
    if (is.null(info_url)) return()
    available_params <- names(info_url)
    
    if ("user_id" %in% available_params) {
      info_user$id <- info_url$user_id} 
    if ("user_name" %in% available_params) {
      info_user$name <- info_url$user_name} 
  })
  
  # Adicionar theme ---------------------------------------------------------

  theme_load <- reactive({
    theme_select <- input$theme
    if (is.null(theme_select)) return()
    dsthemer_get(org=info_org$name, theme = theme_select)
  })
  
  theme_draw <- reactive({
    l <- theme_load()
    l <- l[setdiff(names(l), c('background_color', 'palette_colors', 'branding_include'))]
    l
  })
  
  background <- reactive({
    theme_load()$background_color
  })
  
  color_by_opts <- reactive({
    names(data_draw())
  })
  
  agg_palette <- reactive({
    colors <- theme_load()$palette_colors
    colors
  })

  # Renderizar grafico ------------------------------------------------------
  
  opts_viz <- reactive({
    opts_viz <- parmesan_input()
    if (is.null(opts_viz)) return()
    opts_viz <- opts_viz[setdiff(names(opts_viz), c('theme'))]
    if (is.null(opts_viz$orientation)) opts_viz$orientation <- 'ver'
    opts_viz
  })
  
  viz_name <- reactive({
    if (is.null(ftype_draw())) return()
    if (ftype_draw() == "") return()
    ctype <- gsub("-", "", ftype_draw())
    gtype <- actual_but$active
    if (is.null(gtype)) return()
    typeV <- paste0('gg_', gtype, '_', ctype)
    typeV
  })
  
  gg_viz <- reactive({
    if (is.null(viz_name())) return()
    print(opts_viz())
    viz <- do.call(viz_name(), c(list(data = data_draw(), 
                                      opts = opts_viz(),
                                      theme = theme_draw()
                                      )
                                 ))
    viz
  })
  
  output$view_gg <- renderPlot({
    viz <- gg_viz()
    if (is.null(viz)) return()
    suppressWarnings(
      viz
    )
  })  

# Descarga de grafico -----------------------------------------------------

 
  output$download <- renderUI({
    lb <- i_("download_plot", lang())
    downloadImageUI("download_plot", 
                    dropdownLabel = lb, 
                    formats = c("svg","jpeg", "pdf", "png", "link"),
                    display = "dropdown")
  })
  
  
  callModule(downloadImage, "download_plot", lib = "ggplot", 
             graph = gg_viz(),
             formats =  c("svg","jpeg", "pdf", "png", "link"), name = "gg_image",
             modalFunction = pin_user_url,
             title = reactive(input$`download_plot-link-name`),
             element = reactive(gg_viz()), user_id = info_user$id, user_name = info_user$name)
  
}

shinyApp(ui, server)