#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(ggplot2)
library(ggiraph)
library(tidyverse)
library(magrittr)
library(rlang)
library(R6)


ggplot_colors <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}



scatter <- function(data, x, y, tip, rows) {
    tip_disp <- paste(tip, "disp_", sep = "_")
    tips <- map2(tip, tip_disp, ~ paste(.x, data[[.y]], sep = ": ")) %>% 
        set_names(tip_disp)
    
    dat <- data %>% 
        mutate(!!!tips,
               tip = paste(!!!syms(tip_disp), sep = "\n"))
    
    
    
    if (!is.null(rows)) {
        dat %<>% mutate(selected__ = id_disp_ %in% rows)
        dat_filt <- dat %>% filter(selected__)
        
        gr <- ggplot(dat) + 
            geom_point_interactive(aes(!!as.name(x), !!as.name(y), tooltip = tip,
                                       data_id = id_disp_)) +
            geom_point_interactive(data = dat_filt,
                                   aes(!!as.name(x), !!as.name(y), tooltip = tip,
                                       data_id = id_disp_, colour = group_disp_)) + 
            scale_colour_manual_interactive(values = dat$color_disp_ %>% setNames(dat$group_disp_)) +
            theme(legend.position="none")
    } else {
        gr <- ggplot(dat) + 
            geom_point_interactive(aes(!!as.name(x), !!as.name(y), tooltip = tip,
                                       data_id = id_disp_))
    }
    gr
}

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    fluidRow(column(3,
                    fileInput("file_dat", "Choose File",
                              multiple = FALSE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,
                                            text/plain",
                                         ".csv",
                                         ".tsv"),
                              width = NULL),
                    wellPanel(selectInput("FC_col", "Fold Change Column (x-axis):", 
                                             choices="")),
                    
                    wellPanel(selectInput("pv_col", "Score Column (x-axis):", 
                                             choices="")),
                    wellPanel(selectInput("hover_cols", "Hover Display:", 
                                          choices="", multiple = TRUE))),
             column(9, girafeOutput("volcano"))),

    fluidRow(DTOutput("data_tbl"))
)

# Define server logic 
server <- function(input, output, session) {
    
    data_df_disp <- reactive({
        req(input$file_dat)
        
        path <- input$file_dat$datapath
        ext <- tools::file_ext(path)
        
        tryCatch(
            df <- if (ext == "csv") {
                read.csv(path)
            } else {
                read.delim(path)
            },
            error = function(e) stop(safeError(e)))
        
        dat <- df
        
        cols <- colnames(df)
        cols_disp <- paste(cols, "disp_", sep = "_")

        dat %<>% mutate(id_disp_ = row_number(),
                        group_disp_ = factor(id_disp_),
                        !!!set_names(syms(cols), cols_disp) )
        
        n <- nrow(dat)
        dat %<>% mutate(color_disp_ = ggplot_colors(n))

        is_num <- map_lgl(df, is.numeric)
        if (any(is_num)) {
            num_col <- cols[is_num]
            num_col_disp <- cols_disp[is_num]

            dat %<>% mutate( !!!set_names(map(syms(num_col), ~ quo(sprintf("%.02f", !!.x))),
                                          num_col_disp) )
        }
        
        dat
    })
    
    
    data_df <- reactive({
        data_df_disp() %>% select(-ends_with("_disp_"))
    })
    
    
    data_df_usr <- reactive({
        dat <- data_df_disp()
        if (!is.null(selected_points())) dat %<>% filter(id_disp_ %in% selected_points())
        dat
    })
    
    
    selected_points <- reactive({
        input$volcano_selected
    })
    
    
    selected_rows <- reactive({
        idx <- input$data_tbl_rows_selected
        rows <- if (is.null(idx)) {
            NULL
        } else {
            data_df_usr() %>% slice(idx) %>% pull(id_disp_)
        }
        rows
    })
    
    
    
    observe({
        updateSelectInput(session, "FC_col", choices = colnames(data_df()),
                          selected = "")
        
        updateSelectInput(session, "pv_col", choices = colnames(data_df()),
                          selected = "")
        
        updateSelectInput(session, "hover_cols", choices = colnames(data_df()))
    })
    
    
    output$volcano <- renderGirafe({
        req(input$FC_col, input$pv_col, input$hover_cols)
        
        gr <- scatter(data_df_disp(), input$FC_col, input$pv_col, input$hover_cols,
                      selected_rows())
        girafe(ggobj = gr) %>% 
            girafe_options(opts_zoom(min=.5, max = 2))
        
    })
    

    output$data_tbl <- renderDT({
        dat <- data_df_usr()
        datatable(dat %>% select(-ends_with("_disp_")), rownames = FALSE, filter = "top")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
