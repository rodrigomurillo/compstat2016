library(shiny)


shinyUI(fluidPage(
  titlePanel("Estadistica Computacional"),
  sidebarLayout(
    #--------------------------------------------NOMBRE TAREAS-----------------------------------------------      
    sidebarPanel(
      selectInput("opcion", "Tarea:", choices = c("Funcion Inversa", "Integral Montecarlo",
                                                  "MCMC"), selected = 'Funcion Inversa'),
      
      #--------------------------<<<<<<<<<<<<<------------INPUTS----------------->>>>>>>>>>>>>>>>>>>>>>---------------------------------------      
      
      #-----------------------------------TAREA 1---------------------------------------------------------
      
      conditionalPanel(
        'input.opcion === "Funcion Inversa"',
        numericInput('nsim', label = 'Numero de simulaciones', value = 1000, min = 1),
        numericInput('lambda', label = 'lambda', value = .5, min = 0),
        numericInput('nbin', label = 'nbin', value = 1, min = 0),
        sliderInput('range', label = 'Limites de la grafica', value = c(0,10), min = 0, max = 10, step = 0.01)
      ),
      
      #-----------------------------------TAREA 3---------------------------------------------------------          
      
      # conditionalPanel(
      #   'input.opcion === "Integrales-Montecarlo"',
      #   numericInput('n', label = 'Dimension de la funcion', value = 1, min = 1, max = 5),
      #   textInput(
      #     inputId="expresionMC", 
      #     label="Funcion g(x)",
      #     value="mean(x)"
      #   ),
      #   sliderInput('a', label = 'Limites de integracion', min = -6, max = 6, value = c(0,2), step = 0.01),
      #   
      #   sliderInput('N', label = 'Numero de simulaciones', value = 50, min = 1, max = 2000, step = 1),
      #   sliderInput('alpha', label = 'Significancia del intervalo', min = 0.001, max = 0.1, value = 0.05, step = 0.001),
      #   actionButton('reset_input', 'Reset input'))
      
      
      #------------------------------------TAREA 2------------------------------------------------------------------------          
      
      conditionalPanel(
        'input.opcion === "Integrales-Montecarlo"',
        sliderInput("n_sim",
                    "N??mero de puntos a simular:",
                    min = 10,
                    max = 1e4,
                    value = 100)
        )
      
    ), # Slidebar panel No Borrar
    
    #--------------------------<<<<<<<<<<<<<------------OUTPUTS----------------->>>>>>>>>>>>>>>>>>>>>>---------------------------------------      
    
    mainPanel(
      
      
      #------------------------------------TAREA 1------------------------------------------------------------------------          
      conditionalPanel(condition = "input.opcion=='Funcion Inversa'",
                       tabsetPanel(
                         tabPanel('Hist', plotOutput('random_hist')),
                         tabPanel('Plot', plotOutput('random_plot')),
                         tabPanel('Table', dataTableOutput('random_numbers'))
                         )
                       ),
      
      #------------------------------------TAREA 2------------------------------------------------------------------------          
      conditionalPanel(condition="input.opcion=='Integral Montecarlo'",
                       textInput(
                         inputId="funcion", 
                         label="Funci??n g",
                         value="function(x) (log(1/x))^3 - factorial(3)"
                       ),
                       sliderInput("a", "a", min=-30, max=30, value=0),
                       sliderInput("b", "b", min=-30, max=30, value=1),
                       sliderInput("n_min", "n_min", min=2, max=10000, value=10),
                       sliderInput("n_max", "n_max", min=2, max=10000, value=1000),
                       sliderInput("alpha", "alpha", min=.001, max=.5, value=.05),
                       plotOutput("distPlot")
                       
                       ),
      
      
      
      #------------------------------------TAREA 3,4,5------------------------------------------------------------------------          
      
      # radioButtons("opcion", label = h3("Tarea:"), choices = "BayesLinReg", selected = 'BayesLinReg'),
      
      #----------------------------------.
      conditionalPanel('input.opcion === "MCMC"',
                       
                       h5("Este conjunto de datos contiene informacion recopilada por el Servicio del Censo de los Estados Unidos sobre la vivienda en la zona de la Misa de Boston. Se obtuvo del archivo StatLib (http://lib.stat.cmu.edu/datasets/boston) y se ha utilizado ampliamente en la literatura para comparar algoritmos."),
                       dataTableOutput('table'),
                       
                       selectInput("dep", "Variable Dependiente:", 
                                   choices = c('medv'=1,'lstat'=2,'crim'=3,'indus'=4,'black'=5,'pratio'=6)), 
                       checkboxGroupInput("checkGroup", label = "Variables Independientes, elige diferente que la dependiente", 
                                          choices = c('valor medio de la casa'=1,'porcentaje de pob en pobreza'=2,'criminalidad'=3,
                                                      'indusstria'=4,'habitantes afroamericanos'=5,'% de alumnos x maestro'=6),
                                          selected = 3:4),
                       sliderInput('long4', label = 'Numero de cadenas', value = 5, min = 1, max = 10, step = 1),
                       sliderInput('N4', label = 'Longitud de la cadena', min = 1000, max = 10000, value =1000, step = 1000),
                       actionButton('Simular', 'Simular'),
                       fluidRow(
                         h5('A continuacion se muestra un analisis exploratorio de la base de datos Boston.'),
                         plotOutput('boston1'),
                         h6('Algo de llamar la atencion en esta grafica es como rm y age no parecen tener una relacion positiva, pues se podria pensar que cuando hay m??s ni??os se necesitan m??s cuartos, aunque tamb??en el n??mero de hijos por familia esta indirectamente relacionada al ingreso. Por ello tamb??en se podr??a explicar la relaci??n que guardan ptratio y medv. Por otro lado me llam?? la atenci??n la relaci??n de lstat con tax pues no se esperaria ver una relacion negativa, y de lstat con medv positiva, al menos moralmente hablando.'),
                         plotOutput('boston2'),
                         h6('Hay varias variables que se comportan como distribuciones conocidas, por ejemplo a age podriamos asignarle una exponencial, al igual que black; A chas, se le puede asignar una beta aunque se ve insuficiente. Luego a dis e istatlas podr??amos asociar a una poisson, a medv tambien o a una gamma. Y a rm le asociaria una binomial o una normal si se tiene suficiente densidad en los datos. Y a estas variables les aplicar??a un boxplot ya que al poderlas asociar a distribuciones conocidas los estad??sticos reflejados en el boxplot son mas parsimoniosos, de otra manera podr??amos tener valores importantes y considerarlos outlayers o perder de vista huecos importantes en los datos como en el caso de la variable nox.'),
                         plotOutput('boston3'),
                         h6('En este grafico combinado para la variable medv, podemos ver primeramente en el piso gracias a la ayuda del rug, el porque de los outlayers que teniamos en el boxplot, es porque cerca del cincuenta hay una segunda joroba formando una especie de distribuci??n bimodal, aunque en comparativa mucho menor a la moda. Adem??s podemos ver como en algunas partes se generan m??nimos locales, esto justifica la existencia de peque??os gaps en el strichart'),
                         h5('RESULTADO:'),
                         withMathJax(uiOutput('tableMH2')),
                         plotOutput('boston4'),
                         h4('histogramas por par??metro despu??s de simular.'),
                         plotOutput('ploH'),
                         h4('Simulaci??n de las c??denas por par??metro.'),
                         dataTableOutput("tablechain"))
      ),
      tabPanel('Datos', dataTableOutput('data'))
      
      
    ) # Main Panel No Borrar
  )))


