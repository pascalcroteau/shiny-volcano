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
library(scales)
library(GGally)
library(ggiraph)
library(tidyverse)
library(magrittr)
library(rlang)
library(R6)


ggplot_colors <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}


scale_choices <- c(Raw="raw", Log2="log2", Log10="log10", `Square Root`="sqrt")

scale_x_log2 <- function(...) scale_x_continuous(..., trans = log2_trans())
scale_y_log2rev <- function(...) scale_y_continuous(..., trans = c("log2", "reverse"))
scale_y_log10rev <- function(...) scale_y_continuous(..., trans = c("log10", "reverse"))
scale_y_sqrtrev <- function(...) scale_y_continuous(..., trans = c("sqrt", "reverse"))


scatter <- function(data, x, y, x_scale, y_scale, tip, rows) {
    
    tip_disp <- paste(tip, "disp_", sep = "_")
    tips <- map2(tip, tip_disp, ~ paste(.x, data[[.y]], sep = ": ")) %>% 
        set_names(tip_disp)
    
    dat <- data %>% 
        mutate(!!!tips,
               tip = paste(!!!syms(tip_disp), sep = "\n"))
    
    
    
    if (!is.null(rows)) {
        dat %<>% mutate(selected__ = id_disp_ %in% rows)
        dat_filt <- dat %>% filter(selected__)
        
        cols <- dat$color_disp_ %>% setNames(dat$group_disp_) %>% .[dat$group_disp_]
        
        gr <- ggplot(dat) + 
            geom_point_interactive(aes(!!as.name(x), !!as.name(y), tooltip = tip,
                                       data_id = id_disp_)) +
            geom_point_interactive(data = dat_filt,
                                   aes(!!as.name(x), !!as.name(y), tooltip = tip,
                                       data_id = id_disp_, colour = group_disp_)) + 
            labs(colour = "Row ID") +
            scale_colour_manual_interactive(values = cols,
                                            labels = dat_filt$tip) +
            theme_bw() +
            theme(legend.title=element_text(size=18),
                  legend.key = element_rect(fill = "white", colour = "black"),
                  legend.text = element_text(size=16),
                  legend.key.width = unit(3,"line")) +
            guides(color = guide_legend(override.aes = list(size=4)))
        
        legend <- grab_legend(gr)
        
        gr <- gr + theme(legend.position="none")
    } else {
        gr <- ggplot(dat) + 
            geom_point_interactive(aes(!!as.name(x), !!as.name(y), tooltip = tip,
                                       data_id = id_disp_)) +
            theme_bw()
        legend <- NULL
    }
    
    
    if (x_scale == "log2") gr <- gr + scale_x_log2()
    if (x_scale == "log10") gr <- gr + scale_x_log10()
    if (x_scale == "sqrt") gr <- gr + scale_x_sqrt()
    
    gr <- gr + scale_y_reverse()
    
    if (y_scale == "log2") gr <- gr + scale_y_log2rev()
    if (y_scale == "log10") gr <- gr + scale_y_log10rev()
    if (y_scale == "sqrt") gr <- gr + scale_y_sqrtrev()
    
    list(gr, legend)
}

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Interactive Volcano Plot"),

    fluidRow(column(4,
                    fileInput("file_dat", "Choose File",
                              multiple = FALSE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,
                                            text/plain",
                                         ".csv",
                                         ".tsv"),
                              width = NULL),
                    
                    wellPanel(fluidRow(column(8, selectInput("FC_col", "Fold Change Column (x-axis):", 
                                                    choices="")),
                              column(4, selectInput("FC_scale", "Scale:", 
                                                    choices="")))),
                    
                    wellPanel(fluidRow(column(8, selectInput("pv_col", "Score Column (y-axis):", 
                                                             choices="")),
                                       column(4, selectInput("pv_scale", "Scale:", 
                                                             choices="")))),                          
                    
                    wellPanel(selectInput("hover_cols", "Hover Display:", 
                                          choices="", multiple = TRUE))),
             column(2, plotOutput("legend")),
             column(6, girafeOutput("volcano"))),

    fluidRow(DTOutput("data_tbl"))
)

# Define server logic 
server <- function(input, output, session) {
    
    legend_val <- reactiveValues(legend = NULL)

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
        
        updateSelectInput(session, "FC_scale", choices = scale_choices)
        
        updateSelectInput(session, "pv_col", choices = colnames(data_df()),
                          selected = "")
        
        updateSelectInput(session, "pv_scale", choices = scale_choices)
        
        updateSelectInput(session, "hover_cols", choices = colnames(data_df()))
    })
    
    
    output$volcano <- renderGirafe({
        req(input$FC_col, input$pv_col, input$hover_cols, input$FC_scale,
            input$pv_scale)
        
        gr <- scatter(data_df_disp(), input$FC_col, input$pv_col, input$FC_scale,
                      input$pv_scale, input$hover_cols, selected_rows())
        legend_val$legend <- gr[[2]]
        gr <- gr[[1]]
        
        girafe(ggobj = gr) %>%
            girafe_options(opts_zoom(min=.5, max = 2))

        
        
        
    })
    

    output$data_tbl <- renderDT({
        dat <- data_df_usr()
        datatable(dat %>% select(-ends_with("_disp_")), 
                  rownames = FALSE, filter = "top")
    })
    
    
    output$legend <- renderPlot(legend_val$legend)

}

# Run the application 
shinyApp(ui = ui, server = server)
