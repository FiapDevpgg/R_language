#########################################################
##                                                     ##
##          REGRESSÃO LINEAR SENSOR ULTRASSOM          ##
##                                                     ##
#########################################################

######################################
##    Instalação de pacotes         ##
######################################
install.packages("DBI")
install.packages("odbc")
sort(unique(odbcListDrivers()[[1]]))#Para verificar os drivers existentes


######################################
##     Arquitetura do  Dataframe    ##
######################################

con <- dbConnect(odbc(), "DataSource_Ultrassom")#Pode mudar o nome do datasource "" se necessário
con <- dbConnect(odbc(),
                 Driver = '',#Driver a ser usado. Consultar linha 12 para verificar possibilidades
                 Server = '',#Endereço
                 Database = '',#Nome do banco
                 Trusted_Connection = 'True'
                 )

y <- dbGetQuery(con, "SELECT leituras FROM tabela")#Query para selecionar as leituras
x <- (1:length(y))
plot(x,y)#Testar a estrutura

#Criação do data frame Sensor.dados
sensor.dados= data.frame(id = c(1:41),
  Cm <- sqlFetch(con, "tabela", colnames="", rownames=NULL),
  tempo <- c(1:length(Cm)))

str(sensor.dados)#Para verificar a arquitetura do data frame
head(sensor.dados)

######################################
##      Operando com Dataframe      ##
######################################
data = sensor.dados

#Linear Model
lm(Cm~tempo)

#Summary vai chamar dados como média, moda e mediana, e afins
summary(lm(Cm~tempo))

#Para o gráfico de pontos e da reta de regressão
plot(tempo , Cm)
abline(lm(Cm~tempo))

#Resíduos e valores estimados
lm.dis <- lm(Cm ~tempo)
fitted(lm.dis) #valores estimados de acordo com o melhor ajuste
resid(lm.dis)#Diferença entre os valores estimados e os observados (R²)

plot(tempo,Cm)
lines(Cm,fitted(lm.dis))

#Para trabalhar com os dados faltantes
options(na.action=na.exclude)
lm.dis <- lm(Cm~tempo)
fitted(lm.dis)

#gráfico dos valores estimados à reta
segments(Cm,fitted(lm.dis),Cm,tempo)
abline(lm(Cm~tempo))
