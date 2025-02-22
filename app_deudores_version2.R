library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(DT)
library(plotly)
library(readr)
library(tidyr)
library(ggcorrplot)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(cluster)
library(gridExtra)
library(caret)
library(rpart)
library(rpart.plot)
library(pROC)


df_bcra <- read_csv("df_bcra_individuals.csv") %>%
  filter(deuda_total_actual <= 100)

detalle_bcra <- read_csv("detalle_brca.csv") %>% rename(variable = Variable)


categoricas <- c("tipo_persona", "situacion_mes_actual", "max_situacion_mes", 
                 "max_sit_mes_con_garantia", "max_sit_mes_sin_garantia", "peor_situacion_respuesta")
binarias <- c("tiene_garantia_actual", "mora_30_dias_mes_actual", "default", "mora_mayor_30_dias")
numericas <- setdiff(names(df_bcra), c(categoricas, binarias, "id_individuo"))

df_bcra <- df_bcra %>%
  mutate(across(all_of(categoricas), as.factor),
         across(all_of(binarias), ~ factor(., levels = c("0", "1"), labels = c("No", "Sí"))))

# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(
    title = div(
      # Estilos flex para alinear el texto a la izquierda y la imagen a la derecha
      style = "display: flex; justify-content: space-between; align-items: center; width:100%;",
      
      # Título
      span("IMD: BCRA Deudas", style = "font-weight: bold;"),
      img(src = "BRCA_logo.png", height = "40px")
    ),tabPanel("Exploración de Datos",
                      fluidRow(
                        column(6, radioButtons("var_type", "Tipo de Variable:", 
                                               choices = c("Numérica" = "num", "Categórica" = "cat"), 
                                               selected = "num")),
                        column(6, conditionalPanel(
                          condition = "input.var_type == 'num'",
                          selectInput("plot_type", "Tipo de Gráfico:", 
                                      choices = c("Histograma" = "hist", "Boxplot" = "box"), 
                                      selected = "hist")
                        )
                        )
                      ),
                      fluidRow(
                        column(3, conditionalPanel(
                          condition = "input.var_type == 'num'",
                          uiOutput("outlier_summary")
                        )),
                        column(9, plotlyOutput("plot_var", height = "700px"))
                      )
             ),
             tabPanel("Matriz de Correlaciones",
                      fluidPage(
                        fluidRow(
                          
                          column(4,
                                 uiOutput("texto_cor")
                          ),
                          
                          column(8,
                                 tags$h5("Método de correlación:"),
                                 selectInput("cor_method", "Método:",
                                             choices = c("pearson", "kendall", "spearman"), 
                                             selected = "pearson"
                                 ),
                                 plotOutput("correlation_plot", height = "800", width = "800px")
                          )
                        )
                      )
             ),
             tabPanel("Análisis PCA",
                      tabsetPanel(
                        tabPanel("Círculo de correlación",
                                 fluidRow(
                                   column(8, plotOutput("varPlot", height = "600px", width = "100%")),
                                   column(4, uiOutput("texto_PCA"))
                                 )
                        ),
                        tabPanel("Contribución de las variables",
                                 fluidRow(
                                   column(8, plotOutput("contribPlot",height = "600px", width = "100%")),
                                   column(4, uiOutput("texto_coor"))
                                 )
                        ),
                        tabPanel("PCA agrupados por CUIT",
                                 fluidRow(
                                   column(8, plotOutput("indPlot",height = "600px", width = "100%")),
                                   column(4, uiOutput("texto_inter"))
                                 )
                        )
                      )
             ),tabPanel("Clusters",
                        tabsetPanel(
                          tabPanel("Resultados",
                                   fluidRow(
                                     column(
                                       width = 6,
                                       plotOutput("clusterPlot",        height = "500px", width = "600px"),
                                       plotOutput("tipoPersonaPlot",    height = "500px", width = "600px")
                                     ),
                                     # Columna derecha (texto)
                                     column(
                                       width = 4,
                                       uiOutput("texto_cluster")
                                     )
                                   )
                          ),
                          tabPanel("Analisis",
                                   fluidRow(
                                     column(
                                       width = 8,
                                       plotOutput("boxplotClusters", height = "600px", width = "800px"),
                                       div(style = "height: 300px; width: 500px; overflow-y: scroll;", uiOutput("clusterSummary"))
                                     ),
                                     column(
                                       width = 4,
                                       uiOutput("texto_analisis"))
                                   )
                          )
                        )
             ), tabPanel("Modelo Predictivo",
                         tabsetPanel(
                           tabPanel("Resultados",
                                    fluidRow(
                                      column(6,
                                             tags$div(
                                               style = "font-size: 1px; max-height: 400px; max-width: 400px; overflow-y: auto; white-space: pre-wrap;", 
                                               verbatimTextOutput("modelResults")
                                             ),
                                             uiOutput("texto_modelo")
                                      ),
                                      column(6,
                                             selectInput("modeloSeleccionado", "Seleccionar Modelo:",
                                                         choices = c("Modelo 1", "Modelo 2")),
                                             plotOutput("treePlot"),
                                             br(),
                                             plotOutput("rocPlot")
                                      )
                                    )
                           ),
                           tabPanel("Análisis",
                                    fluidRow(
                                      column(
                                        width = 12,
                                        uiOutput("texto_pre")
                                      )
                                    )
                           )
                         )
             )
  )
)







  
             
#---- Server ----

server <- function(input, output, session) {
  
#---- Descripcion variables ----
  
  output$plot_var <- renderPlotly({
    tipo <- input$var_type
    
    if (tipo == "num") {
      long_data <- df_bcra %>% 
        select(all_of(numericas)) %>% 
        pivot_longer(cols = everything(), names_to = "variable", values_to = "valor") %>%
        left_join(detalle_bcra, by = "variable") %>%  
        mutate(detalle = ifelse(is.na(detalle), "Descripción no disponible", detalle))
      long_data <- long_data %>%
        mutate(etiqueta = paste0("Variable: ", variable, "<br>Descripción: ", detalle))
      
      
      if (input$plot_type == "hist") {
        p <-  ggplot(long_data, aes(x = valor, label = etiqueta)) +
          geom_histogram(bins = 30, fill = "steelblue", color = "black") +
          facet_wrap(~ variable, scales = "free", ncol = 4) + 
          theme_dark() +
          labs(title = "Distribución de Variables", x = "Valor", y = "Frecuencia") +
          theme(
            plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),  
            strip.text.x = element_text(size = 8, face = "bold", hjust = 0.9, lineheight = 1.2),  
            panel.spacing = unit(0.5, "lines"),
            axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8)
          )
        
      } else {
        p <- ggplot(long_data, aes(x = variable, y = valor, label = paste0("Variable: ", variable, "<br>Descripción: ", detalle))) +
          geom_boxplot(fill = "tomato", outlier.color = "red") +
          theme_minimal() +
          labs(title = "Boxplots de Variables Numéricas", x = "Variable", y = "Valor") +
          coord_flip() +
          theme(strip.text = element_text(size = 10, face = "bold"))
      }
    } else {
      long_data <- df_bcra %>% 
        select(all_of(c(categoricas, binarias))) %>%  
        pivot_longer(cols = everything(), names_to = "variable", values_to = "categoría") %>%
        left_join(detalle_bcra, by = "variable") %>%  
        mutate(detalle = ifelse(is.na(detalle), "Descripción no disponible", detalle),
               detalle = iconv(detalle, "latin1", "ASCII", sub=""))
      
      p <- ggplot(long_data, aes(x = categoría, label = paste0("Variable: ", variable, "<br>Descripción: ", detalle))) +
        geom_bar(fill = "purple", color = "black") +
        facet_wrap(~ variable, scales = "free",ncol = 3) +
        
        labs(title = "Distribución de Variables Categóricas", x = "Categoría", y = "Frecuencia") +
        theme(strip.text = element_text(size = 10, face = "bold"))
    }
    ggplotly(p, tooltip = "label")
  })
  
  output$outlier_summary <- renderUI({
    tagList(
      tags$h3("Resumen outliers"),
      tags$p(
        HTML("Los valores atípicos fueron detectados utilizando la fórmula de cuantiles: 
            <b> x<(Q1−1.5×IQR)o  x>(Q3+1.5×IQR)</b>."
      )),
      tags$li(
        HTML("
          <b>Consideraciones</b> <br/>
          La tabla representa los valores que se podrian considerar outliers segun el metodo estadistico
          aplicado, pero es importante resaltar que esos datos pueden ser utililes para el analisis, o 
          entender el compartamiento de una variable.<br/>"
          
      )
    ),
      dataTableOutput("tabla_outliers") 
    )
  })
  
  output$tabla_outliers <- renderDataTable({
    req(input$var_type == "num") 
    
    
    num_vars <- df_bcra %>% select(where(is.numeric))
    
 
    outlier_data <- num_vars %>%
      summarise(across(everything(), ~ {
        q1 <- quantile(., 0.25, na.rm = TRUE)
        q3 <- quantile(., 0.75, na.rm = TRUE)
        iqr_val <- q3 - q1
        
        
        sum(. < (q1 - 1.5 * iqr_val) | . > (q3 + 1.5 * iqr_val), na.rm = TRUE)
      })) %>%
      pivot_longer(
        cols = everything(),
        names_to = "Variable",
        values_to = "N_Outliers"
      )

    datatable(outlier_data, options = list(searching = FALSE))
  })
  

#---- Correlation plot ----
  
  output$correlation_plot <- renderPlot({
    req(df_bcra)
    
   
    par(mar = c(1, 1, 1, 1)) 
    
    cor_data <- df_bcra %>% 
      select(all_of(numericas)) %>% 
      na.omit()
    
    cor_matrix <- cor(cor_data, method = input$cor_method, use = "complete.obs")
    
    corrplot(cor_matrix, 
             method = "color", 
             type = "lower", 
             diag = FALSE,
             col = colorRampPalette(c("red", "white", "blue"))(200),
             tl.col = "black",
             tl.srt = 90, 
             addCoef.col = "black", 
             number.cex = 0.7)
  })
  
  
  output$texto_cor <- renderUI({
    tagList(
      tags$h3("Puntos claves"),
      tags$p("Para una interpretación más detallada del corrplot, 
            podemos analizar la muestra con las correlaciones más fuertes (pearson) y 
            su impacto en las deudas del sistema financiero argentino "
      ),
      
      tags$h4(tags$b("Riesgo de default y morosidad")),
      tags$ul(
        tags$li(
          HTML("
          <b>prop_default_seg</b> ↔ <b>prop_mora_30_dias_seg</b> <br/>
          correlación positiva: La proporción de tiempo en mora de más de 30 días 
          está fuertemente relacionada con caer en default.<br/>
          <b>prop_mora_30_dias_seg</b> ↔ <b>media_prop_situacion_1</b> <br/>
          Correlación negativa: Si un individuo tuvo más meses en situación 1 (pago al día), 
          tiene menor probabilidad de entrar en  mora (situacion 2).<br/>
          <b>media_prop_situacion_1</b> ↔ <b>prop_default_seg</b> <br/>
         Cuanto más tiempo una persona ha estado en situación 1 (pagos al día), 
         menor es la probabilidad de caer en default.<br/>
          
        ")
        )
      ),
      
      tags$h4(tags$b("Estado y evolución de la deuda")),
      tags$ul(
        tags$li(
          HTML("
          <b>media_deuda_total</b> ↔ <b>deuda_total_actua</b> <br/>
          Los valores históricos de deuda están fuertemente relacionados 
          con la deuda en junio de 2019.<br/>
          <b>media_deuda_total</b> ↔ <b>media_deuda_sin_garantia</b>  <br/>
         Esto sugiere que la mayoría de los individuos en la muestra tienen deudas 
         no garantizadas como principal componente de su deuda total.<br/>
         <b>media_prop_situacion_1</b> ↔ <b>media_prop_situacion_2</b> <br/>
         Es decir, quienes han estado mucho tiempo en situación 1, casi nunca han estado en situación 2.
A la inversa, quienes han tenido varios meses en mora leve, no han tenido un historial de pagos perfectos.
         <br/>
          
        ")
        )
      )
  
      
    )
  })
 
#---- PCA ----
  
  
  df_pca <- df_bcra %>% 
    select(all_of(numericas)) %>% 
    select(-proxy_edad_actual) %>%  
    na.omit()
  
# PCA con FactoMineR
  pca_result <- PCA(df_pca, scale.unit = TRUE, graph = FALSE)
  output$screePlot <- renderPlot({
    fviz_eig(pca_result, addlabels = TRUE, main = "Varianza Explicada por Componentes")
  })
  

  output$varPlot <- renderPlot({
    fviz_pca_var(pca_result, col.var = "cos2",
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                 repel = TRUE, title = "Representación de Variables en PCA")
  })
  
  output$texto_PCA <- renderUI({
    tagList(
      tags$h3("Puntos claves"),
      tags$p("Este gráfico representa el círculo de correlación del Análisis de Componentes Principales (PCA).
             A continuación, se detallan los principales elementos a interpretar:"
      ),
      
      tags$h4(tags$b("Ejes del gráfico")),
      tags$ul(
        tags$li(
          HTML("
          <b>Dim 1 (27%) </b> y <b>Dim 2 (23.3%)</b>:
           Representan los dos primeros componentes principales, que explican el 27% y el 23.3% de 
          la variabilidad de los datos, respectivamente.<br/>
          <b>El total de varianza explicada por estos dos primeros componentes es del 50.3%</b>.
          Esto indica que, aunque hay reducción de dimensionalidad, aún queda un 49.7% de 
          información que no está capturada en este gráfico.<br/>
          <b>Las variables con flechas más cortas y cercanas al centro </b> tienen una representación baja en estos componentes y 
          podrían estar mejor explicadas en otros componentes principales.
          ")
        )
      ),
      
      tags$h4(tags$b("Relación entre variables")),
      tags$ul(
        tags$li(
          HTML("
          <b>Variables cercanas entre sí en el mismo cuadrante → están altamente correlacionadas:</b> <br/>
        prop_default, prop_default_seg y media_deuda_en_default están agrupadas y tienen direcciones similares, 
        indicando que representan conceptos similares en los datos.<br/>
          <b>media_prop_situacion_1</b> y <b>prop_mora_30_dias_seg</b> (Correlacion negativa): 
         Esto sugiere que cuando media_prop_situacion_1 es alta, prop_mora_30_dias_seg tiende a ser baja, y viceversa.
Esto tiene sentido financiero, ya que la situación 1 en términos crediticios suele representar un estado más saludable (sin atraso o con bajo riesgo), 
mientras que morosidad a 30 días indica un incumplimiento temprano.<br/>
          ")
        )
      )


    )
  })
  
  
  #contrib
  
  output$contribPlot <- renderPlot({
    
    contrib_matrix <- pca_result$var$contrib
    par(mar = c(4, 4, 4, 6))  
    corrplot(contrib_matrix, 
             is.corr = FALSE, 
             tl.col = "red", 
             tl.cex = 1.2,      
             number.cex = 0.7, 
             col = colorRampPalette(c("yellow", "orange", "red"))(200),
             addCoef.col = "black",  
             number.digits = 2,      
             cl.ratio = 0.6,         
             cl.offset = 1.5)        
  })
  
  output$texto_coor <- renderUI({
    tagList(
      tags$h3("Contribución de las Variables en el PCA"),
      tags$p("La imagen muestra la contribución de cada variable en las primeras 5 dimensiones (componentes principales) del Análisis de Componentes Principales (PCA). La contribución indica 
             cuánto aporta cada variable a la varianza explicada por cada componente:"
      ),
      
      tags$h4(tags$b("Variables dominantes en cada dimensión")),
      tags$ul(
        tags$li(
          HTML("
          <b>Dim 1</b>: media_deuda_con_garantia (14.06%)
          media_prop_con_garantia (13.85%),
          prop_con_garantia_actual (13.30%),
          deuda_con_garantia_actual (13.28%).<br/>
          Parece capturar diferencias en términos de deuda y garantías.
Variables relacionadas con la deuda total y la presencia de garantía tienen gran 
peso en esta dimensión.<br/>
        <b>Dim 2</b>:media_prop_situacion_1 (19.86%), prop_mora_30_dias_seg (19.39%),
media_prop_default (14.51%), prop_default_seg (14.16%).<br/>
Esta dimensión parece capturar diferencias en términos de situaciones crediticias.<br/>
     <b>Dim 3 </b>:media_deuda_sin_garantia (21.01%), media_deuda_situacion_1(15.40%),
Separa clientes por monto de deuda total y si tienen deuda con o sin garantía..<br/>
          ")
        )
       )
      )
    
    
  })
  
## PCA por grupos 
  
  # Filtrar solo los CUITs tipo 20 y 27
  df_pca_categ <- df_bcra %>%
    filter(tipo_persona %in% c(20, 27)) %>%  # Filtrar tipos específicos
    select(all_of(numericas), tipo_persona) %>%
    select(-proxy_edad_actual) %>%  
    na.omit()
  
  # Convertir tipo_persona en factor
  df_pca_categ$tipo_persona <- factor(df_pca_categ$tipo_persona, levels = c(20, 27))
  
  # Ejecutar PCA sin la variable categórica
  pca_result_categ <- PCA(df_pca_categ %>% select(-tipo_persona), 
                          scale.unit = TRUE, graph = FALSE)
  
  # Gráfico de individuos con diferenciación por Tipo de Persona
  output$indPlot <- renderPlot({
    req(pca_result_categ)  # Verifica que el PCA está generado
    
    fviz_pca_ind(pca_result_categ, 
                 geom = "point",  # Muestra solo los puntos
                 habillage = df_pca_categ$tipo_persona,  # Colorear por tipo de persona
                 palette = c("#00AFBB", "#E7B800"),  # Paleta de colores para diferenciar 20 y 27
                 addEllipses = TRUE,  # Elipses para visualizar agrupamiento
                 legend.title = "Tipo de Persona",
                 repel = TRUE  # Evita superposiciones de etiquetas
    ) + ggtitle("Diferenciación de CUITs según Tipo de Persona")
  })
  
  output$texto_inter <- renderUI({
    tagList(
      tags$h3("PCA por Tipo de Persona (CUIT 20 vs 27)"),
      tags$p("En el gráfico, los puntos representan individuos (CUIT) 
             proyectados en las primeras dos dimensiones principales."
      ),
      
      tags$h4(tags$b("Distribución y Superposición")),
      tags$ul(
        tags$li(
          HTML("
          <b> Ambos grupos</b>(tipo_persona 20 en azul y tipo_persona 27 en amarillo) tienen una 
          distribución bastante similar y se superponen significativamente en la región central.<br/>
     Esto indica que <b>las principales fuentes de variabilidad en los datos no separan</b> claramente los dos tipos de CUITs, 
     al menos en estas dos dimensiones. <br/>
     Las elipses están bastante solapadas, lo que sugiere que no hay una diferenciación fuerte 
     entre los dos tipos de persona en términos de los primeros dos componentes principales
          ")
        )
      )
    )
    
    
  })
  
# ---- 4. Determinar clusters ----
  cluster_data <- reactive({
    req(df_bcra) 
    
   
    df_cluster <- df_bcra %>%
      filter(tipo_persona %in% c(20, 27)) %>%
      mutate(tipo_persona_bin = ifelse(tipo_persona == 20, 0, 1)) %>%
     select(all_of(numericas), tipo_persona_bin) %>%
      select(-proxy_edad_actual) %>%
      na.omit()
    
   
    df_cluster_scaled <- scale(df_cluster)
    
    set.seed(123)
    k_opt <- 3  
    cluster_result <- kmeans(df_cluster_scaled, centers = k_opt, nstart = 25)
    
    
    df_cluster$cluster <- factor(cluster_result$cluster)
    
    list(data = df_cluster, result = cluster_result)
  })
  # 
 
  # output$validacionClusters <- renderPlot({
  #   req(cluster_data()) 
  #   
  #   # Ahora sí, hacemos el codo y silhouette sobre df_cluster (sin la variable 'cluster')
  #   datos_para_cluster <- cluster_data()$data %>%
  #     select(-cluster)
  #   
  #   plot_codo <- fviz_nbclust(datos_para_cluster, kmeans, method = "wss") +
  #     ggtitle("Método del Codo - Determinación de k")
  #   
  #   plot_silhouette <- fviz_nbclust(datos_para_cluster, kmeans, method = "silhouette") +
  #     ggtitle("Método de Silhouette - Validación de Clusters")
  #   
  #   gridExtra::grid.arrange(plot_codo, plot_silhouette, ncol = 2)
  # })
  

  output$clusterPlot <- renderPlot({
    req(cluster_data())
    ggplot(cluster_data()$data, aes(x = cluster, fill = cluster)) +
      geom_bar() +
      scale_fill_viridis_d() +
      labs(title = "Cantidad de Observaciones en cada Cluster",
           x = "Cluster", y = "Cantidad de Observaciones") +
      theme_minimal()
  })
  

  output$tipoPersonaPlot <- renderPlot({
    req(cluster_data())
    ggplot(cluster_data()$data, 
           aes(x = cluster, fill = factor(tipo_persona_bin))) +
      geom_bar(position = "fill") +
      scale_fill_manual(values = c("#00AFBB", "#E7B800"), 
                        name = "tipo_persona_bin\n(0=CUIT20, 1=CUIT27)") +
      labs(title = "Distribución de tipo_persona_bin en los Clusters",
           x = "Cluster", y = "Proporción") +
      theme_minimal()
  })
  

  output$clusterSummary <- renderUI({
    tagList(
      tags$h3("Resumen clusters"),
      tags$p(style = "font-size:12px;",
             "Tabla resumen: se muestran las medias de las variables numéricas por cluster."),    
      dataTableOutput("tabla_cluster_summary")
    )
  })
  
  output$tabla_cluster_summary <- renderDataTable({
    req(cluster_data())
    
 
    df_cluster <- cluster_data()$data
    
   
    df_numericas <- df_cluster %>% 
      select(where(is.numeric), cluster)
    

    df_summary <- df_numericas %>%
      group_by(cluster) %>%
      summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
      ungroup()
    
  
    df_long <- df_summary %>%
      pivot_longer(
        cols = -cluster, 
        names_to = "Variable", 
        values_to = "Mean"
      )
    
   
    df_wide <- df_long %>%
      pivot_wider(
        names_from = cluster, 
        values_from = Mean
      )
    
  
    df_wide[, -1] <- round(df_wide[, -1], 2)
    
 
    DT::datatable(df_wide, options = list(searching = FALSE))
  })
  
  
 
  output$boxplotClusters <- renderPlot({
    req(cluster_data())
    
   
    vars_a_graficar <- c("media_deuda_total", 
                         "prop_mora_30_dias_seg", 
                         "prop_con_garantia_actual",
                         "tipo_persona_bin",
                         "prop_default_seg",
                         "media_deuda_en_default",
                         "media_prop_default",
                         "media_deuda_situacion_1")
    
   
    vars_disponibles <- names(cluster_data()$data)
    vars_a_graficar <- vars_a_graficar[vars_a_graficar %in% vars_disponibles]
    
    if (length(vars_a_graficar) == 0) return(NULL)
    
    plots <- lapply(vars_a_graficar, function(var) {
      ggplot(cluster_data()$data, 
             aes(x = cluster, y = .data[[var]], fill = cluster)) +
        geom_boxplot() +
        labs(title = paste("Variable", var), 
             x = "Cluster", y = var) +
        theme_minimal()
    })
    
    do.call(grid.arrange, c(plots, ncol = 3))
  })
  
 
  output$texto_cluster <- renderUI({
    req(cluster_data())
    tagList(
      tags$h3("Metodologia propuesta"),
      tags$p("Se trata de una clasificación no supervisada que agrupa individuos en función de 
             similitudes en sus características. Se utilizan métodos como k-means, que es
             un algoritmo no jerárquico, y se validan los resultados con técnicas de 
             evaluación de clusters, asegurando así que la segmentación sea adecuada.:"),
      
      tags$h4("Preparación y transformación de los datos"),
      tags$ul(
        tags$li(
          HTML("
          <b> Se filtran solo CUITs 20 y 27. </b><br/>
     Se crea la variable binaria tipo_persona_bin (0 para CUIT 20, 1 para CUIT 27).<br/>
     Se estandarizan todas estas variables.
          ")
        )
      ),tags$h4("Determinación del Número Óptimo de Clusters"),
     tags$ul(
       tags$li(
         HTML("
          <b> Se utiliza el método del codo (WSS) </b> para evaluar la varianza dentro de los clusters y 
          determinar cuántos clusters son ideales.<br/>
          Se calcula el <b>índice de Silhouette</b>, que mide la separación y cohesión de los clusters, 
          ayudando a validar la elección de k. <br/>
          Se calculan las medias de cada variable numérica por cluster para su interpretación.<br/>
          El metodo se encuentra en la app de shiny, pero por una cuestion de tiempo en la renderizacion
          de esos procesos se decidio no presentar ambos graficos, pero se puede explorar en el codigo.
     
          ")
       )
     ),
     tags$h4(". Visualización y Caracterización de los Clusters"),
     tags$ul(
       tags$li(
         HTML("
          <b> Se crea un gráfico de barras</b> que muestra la cantidad
          de observaciones en cada cluster.<br/>
          Se analiza la proporción de <b>tipo_persona_bin</b> dentro de los clusters. <br/>
          Ambos métodos se visualizan en gráficos generados con fviz_nbclust()"
          )
       )
     )
    )
    
  })
  
  
  output$texto_analisis <- renderUI({
    req(cluster_data())
    tagList(
      tags$h3("¿Que observamos?"),
      tags$p("Por lo que muestran esos promedios por cluster y la magnitud de cada uno 
             (especialmente el enorme tamaño del cluster 2), se desprende:"),
      
      tags$h4("Cluster 1 (muy pocos casos)"),
      tags$ul(
        tags$li(
          HTML("
          <b> Deuda total relativamente alta (53.22)</b> y una parte importante es con garantía (≈41) <br/>
           <b>Muy baja deuda en default (0.41)</b>, y la “proporción en situación 1” (al día) es bastante alta (0.97).<br/>
            Podríamos pensar en un grupo de pocos individuos con deudas altas pero con garantías,
           y que en general pagan (baja incidencia de default).")
        )
      ),tags$h4("Cluster 2 (la mayoría, ∼15.000"),
     tags$ul(
       tags$li(
         HTML("
          <b> Baja deuda total actual (28.84)</b> y prácticamente sin garantías (0.02).<br/>
          Prácticamente <b>no hay mora ni default</b> (prop_default_seg = 0.00).<br/>
          Este gran grupo parece representar a los deudores más “básicos”, 
          con montos moderados y sin incidencias de mora o default.<br/>
     
          ")
       )
     ),
     tags$h4("Cluster 3"),
     tags$ul(
       tags$li(
         HTML("
          <b> Deuda total (30.54) parecida al cluster 2</b>, pero vemos que la media de deuda
          en default sube a 5.90 (vs. 0.02 o 0.41 en los otros clusters).<br/>
     <b>La prop_mora_30_dias_seg llega a 0.58 y la prop_default_seg a 0.32</b>, muy por encima de los otros<br/>
     Evidentemente un grupo más riesgoso con mayor proporción de cuentas en mora o en default.
     "
         )
       )
     )
    )
    
  })
  
#---- 5 Modelo Predictivo ----
  
  df_junio <- df_bcra %>% select(
    tipo_persona, proxy_edad_actual, n_deudas_actual, deuda_total_actual, 
    deuda_con_garantia_actual, prop_con_garantia_actual, tiene_garantia_actual, 
    mora_30_dias_mes_actual, n_meses_seg_bcra, media_deuda_total, 
    media_deuda_situacion_1, media_deuda_situacion_2, max_situacion_mes, 
    prop_tuvo_garantia, prop_mora_30_dias_seg, default
  )
  
  df_junio <- na.omit(df_junio)
  df_junio$default <- as.factor(df_junio$default)
  df_junio$tipo_persona <- as.factor(df_junio$tipo_persona)
  df_junio$tiene_garantia_actual <- as.factor(df_junio$tiene_garantia_actual)
  df_junio$mora_30_dias_mes_actual <- as.factor(df_junio$mora_30_dias_mes_actual)
  
  set.seed(123)
  trainIndex <- createDataPartition(df_junio$default, p = 0.8, list = FALSE)
  trainData <- df_junio[trainIndex, ]
  testData <- df_junio[-trainIndex, ]
  
  # Validación cruzada para encontrar el mejor cp
  train_control <- trainControl(method = "cv", number = 10)
  cart_model_cv <- train(default ~ ., data = trainData, method = "rpart", 
                         trControl = train_control, tuneLength = 10)
  best_cp <- cart_model_cv$bestTune$cp
  
  # Selector de modelo
  modeloSeleccionado <- reactive({
    if (input$modeloSeleccionado == "Modelo 1") {
      rpart(default ~ ., data = trainData, method = "class", 
            control = rpart.control(cp = 0.01, maxdepth = 8, minsplit = 20))
    } else {
      rpart(default ~ ., data = trainData, method = "class", 
            control = rpart.control(cp = best_cp, maxdepth = 8, minsplit = 20))
    }
  })
  
  output$modelResults <- renderText({
    modelo_actual <- modeloSeleccionado()
    y_pred_cart <- predict(modelo_actual, testData, type = "class")
    conf_matrix_cart <- confusionMatrix(y_pred_cart, testData$default)
    cart_text <- paste("Resultados:", 
                       paste(capture.output(conf_matrix_cart), collapse = "\n"))
    paste(cart_text)
  })
  
  output$treePlot <- renderPlot({
    rpart.plot(modeloSeleccionado(), main = paste("Árbol de decisión -", input$modeloSeleccionado))
  })
  
  output$rocPlot <- renderPlot({
    modelo_actual <- modeloSeleccionado()
    y_pred_cart <- predict(modelo_actual, testData, type = "class")
    roc_cart <- roc(testData$default, as.numeric(y_pred_cart))
    plot(roc_cart, col = "blue", main = paste("Curva ROC -", input$modeloSeleccionado))
    legend("bottomright", legend = c("CART"), col = c("blue"), lty = 1)
  })
  
  output$texto_modelo <- renderUI({
    tagList(
      tags$h3("Comparación entre Modelos"),
      tags$ul(
        tags$li(
          HTML("
          <b>  Estructura:</b> Modelo 1 es más simple; Modelo 2 tiene más ramas <br/>
           <b>  Estructura: </b>Modelo 1 usa menos variables, Modelo 2 añade más criterios<br/>
           <b>  Estructura: </b>Modelo 1 tiene mejor Balanced Accuracy (59.26% vs. 57.80%) y Kappa.<br/>
           <b>  Estructura: </b>Conclusión: Modelo 1 es más interpretable y evita sobreajuste.<br/>
    
          ")
        )
      )
    )
    
  
     })
  
  
  output$texto_pre <- renderUI({
    req(cluster_data())
    tagList(
      tags$h3("Explicación del Uso del Modelo de Árboles de Decisión (CART)"),
      tags$p("El uso de Árboles de Clasificación y Regresión (CART) en este contexto se justifica por varias razones clave relacionadas con la naturaleza de los
             datos y la necesidad de generar interpretabilidad en el modelo predictivo."),
      
      tags$h4("Uso de Árboles de Decisión"),
      tags$ul(
        tags$li(
          HTML("
           
           <b>Interpretabilidad</b> : Los árboles de decisión proporcionan
          un modelo visual y fácil de entender sobre cómo se toman las decisiones 
          en base a las características de los clientes. Esto es crucial para el 
          análisis de riesgo crediticio, ya que permite a los expertos financieros comprender 
          qué variables son más relevantes al predecir el default.<br/>
         <b>Capacidad para manejar relaciones no lineales: </b> A diferencia de 
         modelos lineales como la regresión logística, los árboles pueden modelar relaciones complejas
         entre variables sin necesidad de transformaciones avanzadas. ")
        )
      ),tags$h4("¿Que ocurrieron con los modelos propuestos?"),
      tags$ul(
        tags$li(
          HTML("
          <b> Ambos modelos presentan alta precisión global, pero baja capacidad para detectar correctamente los casos de default</b> 
          (especificidad baja). Esto puede indicar un desbalance en la distribución de clases, con más casos de No Default que de Default.<br/>
          En el Modelo 2, el mejor cp (complejidad del árbol) se seleccionó mediante validación cruzada con la función train() de caret y se agrego de 
          manera automatica para crear el arbol. Lo que se busco con esta tecnica es observar o evitar el sobreajuste seleccionando un árbol que generalice bien a nuevos datos.<br/>
          Si se prioriza la <b>interpretabilidad y robustez</b>: Modelo 1 es más adecuado.<br/>
Si se busca una <b>mejor precisión en la predicción</b> (a costa de posible sobreajuste): Modelo 2 podría ser una mejor opción.<br/>
La variable más importante en ambos modelos es max_situacion_mes: Si el nivel máximo de deuda del mes es 1, 
el modelo predice que la persona no tendrá problemas de pago.<br/>
          ")
        )
      ),
      tags$h4("Evaluación del modelo para futuros solicitantes de Crédito"),
      tags$ul(
        tags$li(
          HTML("
          <b> El modelo tiene una alta sensibilidad (detecta bien a los clientes en riesgo de default)</b><br/>
          <b>Pero tiene baja especificidad</b> (tiene dificultades para identificar correctamente a los clientes sin riesgo).<br/>
          Esto significa que puede generar un alto número de falsos positivos, es decir, personas 
          sin problemas de crédito que el modelo clasifica erróneamente como de alto riesgo.
     "
          )
        )
      )
    )
    
  }) 
  
  
  
  
  
}

shinyApp(ui, server)
