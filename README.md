# IDM:  ESTUDIO DE LAS DEUDAS REGISTRADAS DEL SISTEMA FINANCIERO ARGENTINO
**Autor:** Daniel Perez (Maestría Ciencia de Datos, Universidad Austral)

Este repositorio contiene el código de una aplicación Shiny desarrollada en R, cuyo objetivo es analizar y modelar datos de deudas del sistema financiero (BCRA). La aplicación permite realizar un análisis exploratorio, visualizar correlaciones, aplicar análisis de componentes principales (PCA), segmentar la información mediante clustering y construir modelos predictivos basados en árboles de decisión (CART).

![iPad-Air-4-agd-experta shinyapps io](https://github.com/user-attachments/assets/3aa25a16-8fd9-4fbd-b68e-c5bc6bb3233d)






*Características*

## Exploración de Datos:
Permite visualizar la distribución de variables numéricas (histogramas y boxplots) y categóricas (barras), además de identificar posibles outliers. Para la tabla de outliers simplemente se utilizo un metodo estadistico muy común, pero no significa que no se puedan utilizar esos datos, ya que en determinados contextos, pueden ser útiles.

## Matriz de Correlaciones:
Genera un corrplot interactivo que muestra la relación entre variables numéricas, utilizando diferentes métodos de correlación (Pearson, Kendall, Spearman).

## Análisis PCA:
1. Realiza un Análisis de Componentes Principales para explorar la estructura de la varianza de los datos. Se incluye:
2. Círculo de correlación.
3. Gráficos de contribución de las variables.
4. Proyección de individuos diferenciados por tipo de CUIT.

## Clustering:
1.Segmenta los datos utilizando el algoritmo k-means. Se presentan:
2. Gráficos de distribución de observaciones por cluster.
3. Análisis de la distribución de la variable tipo (CUIT) dentro de cada cluster.
4. Boxplots y resúmenes estadísticos por cluster para interpretar los grupos obtenidos.

## Modelo Predictivo:
Se implementan dos modelos de clasificación (CART) para predecir el default en el pago de deudas. Se comparan mediante:
1. Árbol de decisión.
2. Curva ROC.
3. Reportes de métricas (matriz de confusión, Balanced Accuracy, Kappa, etc).

## Datos
La aplicación utiliza dos archivos CSV: df_bcra_individuals.csv:
> **Nota:**  Contiene la información individual de deudas, la cual se filtra para incluir únicamente observaciones con deuda_total_actual <= 100.
*detalle_brca.csv*: Proporciona descripciones adicionales para las variables, facilitando la interpretación de los gráficos.

## Cómo Ejecutar la Aplicación
Clonar el repositorio:
git clone https://github.com/danielperezok1/IDM_final. 

*Se publico la app en en el link*: https://agd-experta.shinyapps.io/IDM_Final/ , 
estara dispobible hasta la calificación final de la materia. 

## Notas Adicionales
Algunos procesos, como la validación de clusters o la optimización del modelo predictivo, pueden tardar en renderizar dependiendo del tamaño del dataset y la capacidad de procesamiento.
Por eso se comento esas lineas para reproducir en shiny, pero se puede hacer eso de los mismos agregando los graficos de salida en la UI.
La aplicación incluye múltiples controles interactivos (radio buttons, select inputs, etc.) para facilitar el análisis exploratorio y la interpretación de resultados.
Se ha prestado especial atención a la documentación interna y a la generación de textos explicativos dentro de la interfaz, lo que permite una mejor comprensión del análisis realizado.

## Licencia
Este proyecto se distribuye bajo la licencia MIT. La unica finalidad es para rendir el examen final de IDM y no proporciona ningun dato comercial para decisiones corporativas. 



