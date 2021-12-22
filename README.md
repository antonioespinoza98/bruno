# bruno
Dashboard para el proyecto creativo de Regresión Lineal


library(shiny)
library(tidyverse)
library(lattice)
library(corrgram)
library(car)
library(lm.beta)
library(mathjaxr)
library(bslib)

base <- read.csv("base.csv", header = TRUE, sep = ";")
colnames(base) <- c("altura","peso","edad")
base1 <- base %>% mutate(imc = peso/(altura)**2 )
base2 <- subset(base1, base1$altura < 1.95)

modelo <- lm(altura ~ peso + edad + imc, data = base2)
modelo2 <- lm(altura ~ edad + imc, data = base2)
modelo3 <- lm(altura ~ peso, data = base2)

my_theme <- bs_theme(bootswatch = 'sketchy')

ui <- fluidPage(theme = my_theme,
    tabsetPanel(
    tabPanel("Descriptivo",
             column(12,
                    h1("Antes de iniciar, es importante ver nuestros datos"),
                    verbatimTextOutput("baseini"),
                    verbatimTextOutput("base1"),
                    verbatimTextOutput("base2"),
                    textOutput("text"),
                    tableOutput("base"),
                    verbatimTextOutput("codigo"),
                    textOutput("texto"),
                    verbatimTextOutput("code"),
                    plotOutput("corplot", width = "500px"),
                    textOutput("multicol"),
                    plotOutput("scatterplot", width = "500px"),
                    textOutput("linealidad"),
                    plotOutput("box", width = "500px"),
                    textOutput("boxplot"),
                    plotOutput("scatterdos", width = "500px"),
                    plotOutput("cordos", width = "500px"),
                    verbatimTextOutput("base2sum"),
                    h1("Conclusiones"),
                    textOutput("desconclu")
             )#Columna 1
    ),#tabpanel 1
    tabPanel("Modelo",
             column(12,
                    h1("El modelo lineal es..."),
                    withMathJax(),
                    uiOutput('ex1'),
                    h1("Sus dimensiones son: "),
                    uiOutput('ex2'),
                    h1("¿Que es el modelo de regresión lineal?"),
                    p("El modelo es una función matemática que puede explicar
                      los datos. El beta se conoce como la intersección mientras
                      que el beta 1 como la pendiente."),
                    uiOutput('ex3'),
                    h2("El modelo cumple tres propósitos esenciales:"),
                    p("Sus propósitos son descriptivos, de control y
                      de predicción"),
                    h2("¿Qué es la variabilidad condicional?"),
                    p("Esto es por ejemplo, si se restringe más los datos
                      esto da menos variaibilidad, por ende obtenemos
                      una mejor estimación de la media."),
                    h2("¿Qué es la matriz de estructura?"),
                    p("La matriz de estructura se compone de una columna
                      de unos ya que esta se mmultiplica por el intercepto
                      y las demás columnas son las x variables. "), 
                    h2("¿Qué son los betas?"),
                    uiOutput("ex4"),
                    verbatimTextOutput("mod"),
                    verbatimTextOutput("anova"),
                    h1("A continuación una serie de conceptos: "),
                    p("Se puede observar los residuales, que son la diferencia de
                      una observación a la media estimada de la población a la
                      que pertenece. Esto nos lleva a la Suma de Cuadrados Residual
                      la cual se desea fuera lo más pequeÃ±a posible. En el 
                      lenguaje estadístico R se puede utilizar la función: "),
                    code("anova(modelo)[Residuals, 2]"),
                    p("También podemos obtener la Suma de Cuadrados de Regresión
                      la cual nos indica lo que explica cada variable. Se puede
                      obtener también con el siguiente chunck de código:"),
                    code("anova(modelo)[variable, 3]"),
                    p("También se puede obtener lo que se conoce como el 
                      Cuadrado Medio Residual. Esto es la variabilidad condicional"),
                    code('anova(modelo)[Residuals, 3]'),
                    uiOutput("ex5"),
                    h2("Variables estandarizadas"),
                    p("Una forma de ver la importancia de las variables es 
                      estandarizando los coeficientes, para esto podemos utilizar
                      la función o se puede construir manualmente."),
                    code("library('lm.beta')"),
                    br(),
                    code('lm.beta(modelo)'),
                    verbatimTextOutput("stand"),
                    p("En este caso particular la variable peso es la más importante
                      seguida por índice de masa corporal y por último la edad."),
                    h2("¿Cómo podemos mejorar la variabilidad?"),
                    p("La variabilidad de las estimaciones se conoce como error
                      estándar. El error estándar de un coeficiente es la 
                      desviación estándar de la distribución de las estimaciones
                      de ese parámetro. Esta distribución tiende a la normal.
                      Un n más grande hay menos variabilidad y una mejor 
                      aproximación a la normal. POdemos obtener la matriz de 
                      variancia por medio de su forma manual o se puede utilizar
                      la función en R, donde la raíz cuadrada de la matriz
                      representa el error estándar."),
                    code("diag(vcov(modelo))"),
                    verbatimTextOutput("error"),
                    uiOutput("ex6"),
                    h2("¿Por qué esto resulta útil e importante?"),
                    p("Ya que con todo lo que se ha construido hasta ahora,
                      se pueden construir intervalos de confianza y pruebas
                      de hipótesis."),
                    verbatimTextOutput("confint"),
                    p("En este caso solo tendría sentido interpretar para las 
                      variables peso e IMC ya que los signos de los dos son
                      iguales."),
                    h3("¡También se pueden obtener valores ajustados!"),
                    p("Por ejemplo un jugador cuyo peso sea de 52 kilos
                      su edad sea de 15 aÃ±os y su IMC de 19... ¡este jugador es
                      Bruno!"),
                    verbatimTextOutput("ajustado"),
                    verbatimTextOutput("summary"),
                    uiOutput("ex7"),
                    h3("De la anterior salida de código veámos lo siguiente:"),
                    uiOutput('ex8')
             )#Columna 2
    ), #Tab Panel 2
    tabPanel("Valores Individuales",
             column(12,
                    h1('¿Para qué sirven los intervalos de valores individuales?'),
                    p('El intervalo de valores individuales es principalmente 
                      útil cuando se quiere saber en qué rango se ubican las
                      y estimadas para un x condicionado. hay que tener especial
                      cuidado, ya que esto no es un intervalo de confianza.'),
                    verbatimTextOutput('indi'),
                    p('¡Excelentes noticias para Bruno, su estatura está entre
                      el rango!'),
                    uiOutput('ex9'),
                    code('t <- qt(0.975, n - p) donde: n = nrow(base) y
                         p = length(modelo$coefficients)'),
                    br(),
                    code('sqrt(t) ')
                    ) #Columna 3
             ),#tab Panel 3
    tabPanel("Marginal",
             column(12,
                    h1("¿Qué podemos decir de la Suma de Cuadrados de Regresión Marginal?"),
                    h2('Como se habló con anteriordad: '),
                    p('1. La SCRes nunca puede aumentar cuando se agregan 
                      predictores al modelo'),
                    br(),
                    p('2. La SCTotal es fija pues es la variabilidad en Y y no
                      depende de los predictores'),
                    br(),
                    p('3. Con más predictores casi siempre el R cuadrado crece'),
                    br(),
                    p('4. Suma de Cuadrados de regresión marginal: mide el efecto
                      de introducir un X dos después de X uno'),
                    uiOutput('ex10'),
                    verbatimTextOutput('efect1'),
                    p('El aporte de peso después de edad e imc es de 0.005.
                      Ahora de la forma contraria, el aporte de edad e imc
                      después de peso: '),
                    verbatimTextOutput('efect2'),
                    p('Aquí se oserva que el aporte marginal de edad e imc después
                      de peso de apenas 0.002')
                    ) #columna 4
             ), # tab Panel 4
    navbarMenu("Sobre este proyecto",
               tabPanel("Bruno",
                        fluidRow(
                            column(6,
                                   h1("¿Quién es Bruno?"),
                                   p("Explicar conceptos y fórmulas matemáticas a veces
                            se puede tornar en algo tedioso, los jóvenes no 
                            encuentran en la estadística y la matemática una
                            herramienta entretenida que los haga pensar más
                            allá de fórmulas y teoremas. Bruno nace con la idea
                            de que los jóvenes logren conectar un tema tan 
                            extenso como lo es la regresión lineal con una
                            situación tan común a la que se enfrentan los 
                            jóvenes como lo es el crecimiento y el cambio
                            físico."),
                                   plotOutput('bruno')
                            
                                   )
                            )
                        ),
               tabPanel("Autores",
                        fluidRow(
                            column(12,
                                   fluidRow(
                                       column(6,
                                              h1("Nosotros"),
                                              fluidRow(
                                                  column(6,
                                                         h3("Ariana Chacón"),
                                                         p('Estudiante de 
                                                           estadística, le
                                                           apasiona el marketing
                                                           y la inteligencia
                                                           artificial.'),
                                                         plotOutput("ari", width = '10%', height = '10%')
                                                         ),
                                                  column(6,
                                                         h3("Isaura Gutiérrez"),
                                                         p('Estudiante de estadística,
                                                           economista, le apasiona la
                                                           econometría, la demografía
                                                           y la evaluación de 
                                                           políticas públicas.'),
                                                         plotOutput("isaura", width = '10%',height = '10%')
                                                         )
                                              
                                                  )
                                              ),
                                       column(width = 6,
                                              h3('Marco Espinoza'),
                                              p('Estudiante de estadística,
                                                le gusta la música en vivo, 
                                                crear visualizaciones de datos
                                                y el machine learning.'),
                                              plotOutput("marco", width = '10%',height = '10%')
                                              )
                                       )
                                   )
                            )
                        )
               )#Menu
    )
)
server <- function(input, output) {
    output$text <- renderText({"Primeramente, se carga la base junto con los
        paquetes que se utilizaron, para llevar acabo esto se utilizó R en su
        versión 4.0.3. Con esto se pretende ver medidas de posición central y variabilidad
        importantes para entender los datos"})
    output$baseini <- renderPrint({base <- read.csv("base.csv", header = TRUE, sep = ";")})
    output$base1 <- renderPrint({base1 <- base %>% mutate(imc = peso/(altura)**2 )})
    output$base2 <- renderPrint({base2 <- subset(base1, base1$altura < 1.95)})
    output$base <- renderTable(base)
    output$codigo <- renderPrint({
        str(base);
        summary(base);
        round(apply(base,2,sd),3)
    })
    output$texto <- renderText({"Se puede observar medidas de posición central
        interesantes como lo es la media, mediana, los máximos y mínimos.
        Vamos a crear una variable extra que se va a llamar IMC, que hace 
        referencia al índice de Masa corporal y nos da una medida de la obesidad"})
    output$code <- renderPrint({
        round(apply(base1,2,sd),3)
    })
    output$corplot <- renderPlot(corrgram(base1, diag.panel=panel.density, 
                                          upper.panel=panel.conf), res = 96)
    output$multicol <- renderText({"La correlación de la variable respuesta
        altura en cm es bastante alta con dos variables que es peso en kg
        e imc. La correlación con edad es media.
        Otro aspecto relevante es el supuesto de multi colinealidad, la 
        correlación entre variables predictoras se esperaría que fuera baja.
        En la figura matriz de correlación se puede observar que la correlación
        entre peso e IMC es muy alta, al igual que peso con edad y edad con 
        IMC es una correlación media."})
    output$scatterplot <- renderPlot(scatterplotMatrix(base1), res = 96)
    output$linealidad <- renderText({"Otro aspecto a ver aquí es la linealidad.
        En el scatterplot realizado arriba, se puede observar que la línea 
        suavizada que pasa por los promedios de los datos está cerca de la 
        línea de regresión, esto indica que existe la linealidad. 
        Otra cosa, la pendiente de regresión para peso e imc es bastante buena,
        sin embargo para edad esta no es tan pronunciada como las dos anteriores."})
    output$box <- renderPlot(boxplot(base2[-3]), res = 96)
    output$boxplot <- renderText({"Como existe un valor extremo, en el boxplot
        anterior se eliminó ese valor."})
    output$scatterdos <- renderPlot(scatterplotMatrix(base2))
    output$cordos <- renderPlot(corrgram(base2,diag.panel=panel.density, upper.panel=panel.conf))
    output$base2sum <- renderPrint({summary(base2);
        apply(base2,2,sd)
    })
    output$desconclu <- renderText({"Se puede observar que al eliminar el
        valor extremos la desviación estándar disminuyó, la correlación 
        disminuyó. Se sigue irrumpiendo el supuesto de Multi Colinealidad"})
    output$ex1 <- renderUI({
        withMathJax(helpText('$$\\mu_{y|x}=\\beta_0+\\beta_1 \\cdot X_1 +... \\beta_i \\cdot X_i$$'))
    })
    output$ex2 <- renderUI({withMathJax(helpText(
        '$$Y_{MX1}=X_{NXP} \\beta_{PX1}+ \\epsilon_{NX1}$$ donde la dimensión
        de Y es $$NX1$$ debido a que es la matriz de respuesta'
    ))})
    output$ex3 <- renderUI({withMathJax(helpText(
        '$$\\mu_{y|x}$$ representa la media condicional de la respuesta dada un 
        valor de la x'
    ))})
    output$ex4 <- renderUI({withMathJax(helpText(
        'Los betas son variables, para cada conjunto de betas lo que se 
        quiere encontrar es el que minimíce, para esto se utilizan métodos
        matemáticos como la derivación y se busca el mínimo. Para esto
        utilizamos la matriz de estructura y la fórmula de multiplicación
        matricial: $$\\hat{\\beta_0}=(X^tX)^{-1}(X^tY)$$'
    ))})
    output$mod <- renderPrint({modelo})
    output$anova <- renderPrint({anova(modelo)})
    output$ex5 <- renderUI({withMathJax(helpText(
        'La Suma de Cuadrados Residual es: $$\\sum r^2$$, se puede obtener de la
        siguiente manera $$(Y-X \\hat{\\beta})^t (Y-X \\hat{\\beta} )$$.
        También la Suma de Cuadrados de Regresión se puede obtener de la 
        siguiente manera: $$\\sum (\\hat{y}- \\bar{y})^2$$.
        El Cuadrado Medio Residual se puede obtener se la siguiente manera:
        $$\\hat{\\sigma}^2= \\frac{SCRes}{n - p}$$'
    ))})
    output$stand <- renderPrint({lm.beta(modelo)})
    output$error <- renderPrint({diag(vcov(modelo))})
    output$ex6 <- renderUI({withMathJax(helpText(
        '$$V(\\hat{\\beta})=\\sigma^2(X^tX)^{-1}$$ donde $$\\sigma^2 = CMRes$$'
    ))})
    output$confint <- renderPrint({confint(modelo)})
    output$ajustado <- renderPrint({predict(modelo, 
                                            data.frame(peso = 52.2137, edad = 15, 
                                                       imc = 19.4131), data = base2)})
    output$summary <- renderPrint(summary(modelo))
    output$ex7 <- renderUI({withMathJax(helpText(
        "La Suma de Cuadrados Total no depende de nadie, solo de la
                      variable respuesta, se puede obtener de varias maneras pero
                      la más sencilla es usando $$s_y^2 \\cdot (n-1)$$"))})
    output$ex8 <- renderUI({withMathJax(helpText(
        'El coeficiente de determinación que en la función es Multiple R-Squared
        es lo que se conoce como $$R^2$$ y es lo que explica el modelo. Si este
        es cercano a 1 significa que explica bastante, entre menos explique no se
        considera tan bueno. Pero ojo, esto también depende del área en que se esté
        aplicando, existen áreas donde un coeficiente de determinación bajo se 
        considera bueno. Otra manera de obtener este coeficiente $$\\frac{SCReg}{SCTotal}$$'
    ))})
    output$indi <- renderPrint({predict(modelo, 
                                        data.frame(peso = 52.2137, edad = 15, imc = 19.4131), 
                                        interval = "prediction")})
    output$ex9 <- renderUI({withMathJax(helpText(
        'También este intervalo se puede construir de forma manual. Para esto
        se recurre a la variancia de la respuesta media cuya fórmula es
        $$V( \\hat{y}_h )= X_h^t V( \\hat{ \\beta } )X_h$$ después se obtiene la 
        variancia de los valores individuales 
        $$ V(Y_{individuales})= \\sigma^2 +V( \\hat{y}_h )$$
        para esto se recurre a la t-student, se utiliza el código que se incluye 
        abajo, se obtiene el error estándar de los valores individuales, para
        así obtener $$IC( \\mu )= \\bar{y} \\pm t_{n-p} \\cdot E.E(Var_{ind})$$'
        ))})
    output$ex10 <- renderUI(withMathJax(helpText(
        '$$SCReg(X_2|X_1)=SCRes(X_1)-SCRes(X_1,X_2)$$ se puede observar por ejemplo
        el efecto marginal de peso después de edad e imc'
    )))
    output$efect1 <- renderPrint({modelo2; anova(modelo2,modelo)})
    output$efect2 <- renderPrint({modelo3; anova(modelo3, modelo)})
    output$bruno <- renderImage({
        filename <- normalizePath(file.path('./images',
                                            paste('bruno', input$n, '.png',sep = '')))
        list(src = filename)
        
    }, deleteFile = FALSE)

        output$ari <- renderImage({
        filename <- normalizePath(file.path('./images',
                                            paste('ari', input$n, '.png', sep = '')))
        list(src = filename, width = 200, height = 250)
    }, deleteFile = FALSE)
    
    output$isaura <- renderImage({
        filename <- normalizePath(file.path('./images',
                                            paste('isaura', input$n, '.png', sep = '')))
        list(src = filename, width = 200, height = 200)
    }, deleteFile = FALSE)
    
    output$marco <- renderImage({
        filename <- normalizePath(file.path('./images',
                                            paste('marco', input$n, '.png', sep = '')))
        list(src = filename, width = 400, height = 300)
    }, deleteFile = FALSE)
    
}

shinyApp(ui = ui, server = server)
