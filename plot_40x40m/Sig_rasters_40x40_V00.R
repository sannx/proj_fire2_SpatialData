###############################################################################
###############################################################################
#Author: Oliveira-Santos, claudinei
#Instituto de Pesquisa Ambiental da Amazônia (IPAM)
#Agosto 31, 2015
#
#Script para criar rasters do inventário, comportamento do fogo e combustível 
#Parcelas de 40x40 metros do projeto "fogos_pequenos"
#
###############################################################################
###############################################################################
#PACOTES UTILIZADOS NESTE SCRIPT
 library(raster)
 library(gstat)
 library(rasterVis)
 library(lubridate)


###############################################################################
###############################################################################
#DIRETÓRIO PRINCIPAL
setwd("H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_40x40m\\sig_40x40")
path = "H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_40x40m\\sig_40x40\\"

###############################################################################
###############################################################################
#IMPORTAR OS ARQUIVOS UTILIZADOS NESTE SCRIPT

#Censo de mortalidade
csm = read.csv("H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_40x40m\\censo\\1.MASTER_parcelas_40x40m.csv", header = T)

#Combustivel (transecto de Brown)
prc13 = read.csv("H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_40x40m\\combustivel\\combustivel_pre_fogo_40X40_2013.csv",header=T) #Pre combustivel
psc13 = read.csv("H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_40x40m\\combustivel\\combustivel_pos_fogo_40X40_2013.csv",header=T) #pos combustivel
psc14 = read.csv("H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_40x40m\\combustivel\\combustivel_pos_fogo_40X40_2014.csv",header=T) #pos combustivel

#Liteira do combustivel (transecto de Brown)
prl13 = read.csv("H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_40x40m\\liteira_combustivel\\liteira_combustivel_pre_fogo_40x40_2013.csv",header=T) #Pre combustivel
psl13 = read.csv("H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_40x40m\\liteira_combustivel\\liteira_combustivel_pos_fogo_40x40_2013.csv",header=T) #pos combustivel
psl14 = read.csv("H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_40x40m\\liteira_combustivel\\liteira_combustivel_pos_fogo_40x40_2014.csv",header=T) #pos combustivel

#Comportamento do fogo
cpf = read.csv("H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_40x40m\\comportamento_fogo\\comportamento_do_fogo.csv",header=T) #comportamento do fogo

#LAI
lai = read.csv("H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_40x40m\\lai\\1.MASTER_LAI_40x40m_2014.csv",header=T) #leaf area index

#Psycrometro
# psy = read.csv("H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_40x40m\\psychrometro\\.csv",header=F) #area queimada

#Área queimada
# aqm = read.csv("H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_40x40m\\mapa_area_queimada\\.csv",header=F) #area queimada

###############################################################################
###############################################################################
#OBJETOS NECESSÁRIOS PARA CRIAR OS RASTERS
##
##
#XY para os dados de combustível
xycbrown = data.frame(ponto = c('ns', 'sn', 'lo', 'ol'), x = c(10, 30, 10, 30), y = c(10, 30, 30, 10))

#XY para os dados liteira do de combustível
xyltbrown05 = data.frame(ponto = c('ns', 'sn', 'lo', 'ol'), x = c(10, 30, 35, 5), y = c(5, 35, 30, 10))

xyltbrown20 = data.frame(ponto = c('ns', 'sn', 'lo', 'ol'), x = c(10, 30, 20, 20), y = c(20, 20, 30, 10))

#xy para os dados de LAI
xylai = data.frame(ponto = c('A', 'B', 'C', 'D', 'E'), x = c(10, 30, 30, 10, 20), y = c(10, 10, 30, 30, 20))

#Raster vazio para ser preenchido
r = raster(ncols = 40, nrows=40,
xmn = 0, xmx = 40, ymn = 0, ymx = 40)

##
## FUNCAO PARA CRIAR OS RASTERS (INTERPOLATE USANDO O MODELO DO GSTAT)
##

r40 = function (data, var)
{ 
	rlist = list()
	dataplot = as.character(unique(data$parc))
	
	for(i in 1:length(dataplot))
	{
		data2 = subset(data,parc==dataplot[[i]])
		
		data2$Resp = data2[,c(var) ]
		data2 = na.omit(data2[c('x','y','Resp')])
		
		mg <- gstat(id = "Resp", formula = Resp~1, locations = ~x+y, data=data2)
		datavar <- interpolate(r, mg)
		names(datavar) = dataplot[[i]]
		rlist[i] = stack(datavar)	
	}
	rlist	
} 

###############################################################################
###############################################################################
##
#DADOS DE COMPORTAMENTO DO FOGO
#calculando o tempo "tdur" necessário para o fogo percorrer um metro de distância
cpf$tdur =strptime(cpf$hora.2, "%H:%M:%S") - strptime(cpf$hora.1, "%H:%M:%S")
cpf$tdur = ifelse(cpf$tdur<= seconds(20), seconds(NA), ifelse(cpf$tdur>= seconds(420), seconds(NA), cpf$tdur))
cpf$tdur = cpf$tdur * 2
cpf$tdur = 1/(cpf$tdur/60)

##
##JUNTANDO DADOS DE COMPORTAMENTO DO FOGO COM O XY DAS ÁRVORES
##
csmxy = csm[, c('plot','tag', 'x_dist', 'y_dist')]
names(csmxy) = c('plot','tag', 'x', 'y')

cpf1 = cpf[,c('parc','ponto', 'alt', 'larg', 'comp', 'velvento','tdur', 'disttree', 'dirttree' )]

cpf2 = merge(cpf1, csmxy, by.x = 'ponto', by.y = 'tag', all.x=TRUE)
cpf2$y = 40-cpf2$y #Y - 20 para deixar o y na posição correta
##
#Calculando xy com base na distância entre o cavalete e a árvore mais próxima
cpf2$dist2 =as.numeric(cpf2$disttree)/100
cpf2$x = as.numeric(ifelse(cpf2$dirttree == 'l',  cpf2$x - cpf2$dist2, 
				ifelse(cpf2$dirttree == 'o',  cpf2$x + cpf2$dist2, cpf2$x)))

cpf2$y = as.numeric(ifelse(cpf2$dirttree == 'n',  cpf2$y - cpf2$dist2, 
				ifelse(cpf2$dirttree == 's',  cpf2$y + cpf2$dist2, cpf2$y)))

##
##RASTERS DE COMPORTAMENTO DO FOGO
##

#Altura da chama
cpf2$alt = ifelse (cpf2$alt > 100, NA, cpf2$alt)

r40alt = stack(r40(cpf2, "alt"))

#Comprimento da chama
cpf2$comp = ifelse (cpf2$comp > 100, NA, cpf2$comp)

r40comp = stack(r40(cpf2, "comp"))

#Largura da chama
cpf2$larg = ifelse (cpf2$larg > 100, NA, cpf2$larg)

r40larg = stack(r40(cpf2, "larg"))

#Taxa de espalhamento do fogo
r40tdur = stack(r40(cpf2, "tdur"))

###############################################################################
###############################################################################
#COMBUSTÍVEL PRÉ FOGO 2013

#Adcionando xy
prc1 = merge(prc13, xycbrown, by = 'ponto', all=TRUE)

##combustível de 0.0 a 0.6
r40x06pr13 = stack(r40(prc1, "X0.0.6"))

##combustível de 0.6 a 2.6
r40x26pr13 = stack(r40(prc1, "X0.6.2.5"))

##combustível de 2.6 a 7.6
r40x76pr13 = stack(r40(prc1, "X2.5.7.6"))

##altura máxima do combustível
r40maxpr13 = stack(r40(prc1, "Max."))

###############################################################################
###############################################################################
#COMBUSTÍVEL PÓS FOGO 2013

#Adcionando xy
psc1 = merge(psc13, xycbrown, by = 'ponto', all=TRUE)

##combustível de 0.0 a 0.6
r40x06ps13 = stack(r40(psc1, "X0.0.6"))

##combustível de 0.6 a 2.6
r40x26ps13 = stack(r40(psc1, "X0.6.2.5"))

##combustível de 2.6 a 7.6
r40x76ps13 = stack(r40(psc1, "X2.5.7.6"))

##altura máxima do combustível
r40maxps13 = stack(r40(psc1, "Max."))

###############################################################################
###############################################################################
#COMBUSTÍVEL PÓS FOGO 2014

#Adcionando xy
psc1 = merge(psc14, xycbrown, by = 'ponto', all=TRUE)

##combustível de 0.0 a 0.6
r40x06ps14 = stack(r40(psc1, "X0.0.6"))

##combustível de 0.6 a 2.6
r40x26ps14 = stack(r40(psc1, "X0.6.2.5"))

##combustível de 2.6 a 7.6
r40x76ps14 = stack(r40(psc1, "X2.5.7.6"))

###############################################################################
###############################################################################
#LITEIRA COMBUSTÍVEL PRÉ FOGO 2013

#Adcionando xy
prl13.5 = prl13[,c(1:3,seq(4,14,2),16:18)]
	names(prl13.5) = c('parc', 'ponto', 'data', 'ac', 'al', 'at', 'pu', 'se', 'pseco', 'psaco', 'tratamento', 'obs')
prl13.5.2 = merge(prl13.5, xyltbrown05, by = 'ponto', all=TRUE)

prl13.20 = prl13[,c(1:3,seq(5,15,2),16:18)]
	names(prl13.20) = c('parc', 'ponto', 'data', 'ac', 'al', 'at', 'pu', 'se', 'pseco', 'psaco', 'tratamento', 'obs')
prl13.20.2 = merge(prl13.20, xyltbrown20, by = 'ponto', all=TRUE)

prl131 = rbind(prl13.5.2 , prl13.20.2)

prl131$um = (((prl131$pu - prl131$psaco) - (prl131$pseco - prl131$psaco))*100) / (prl131$pseco - prl131$psaco)
prl131$pu2 = (prl131$pu - prl131$psaco)

##Altura do combustivel
r40acpr13 = stack(r40(prl131, "ac"))

##Altura da liteira
r40alpr13 = stack(r40(prl131, "al"))

##Altura do tapete
r40atpr13 = stack(r40(prl131, "at"))

##Solo exposto
r40sepr13 = stack(r40(prl131, "se"))

##% de umidade
prl131$um = ifelse (prl131$um  > 100, NA, prl131$um )
r40umpr13 = stack(r40(prl131, "um"))

##Peso umido
r40pupr13 = stack(r40(prl131, "pu2"))

###############################################################################
###############################################################################
#LITEIRA COMBUSTÍVEL PÓS FOGO 2013

#Adcionando xy
psl13.5 = psl13[,c(1:3,seq(4,12,2),14:15)]
names(psl13.5) = c('parc', 'ponto', 'data', 'ac', 'al', 'at', 'perc', 'pseco', 'tratamento', 'obs')
psl13.5.2 = merge(psl13.5, xyltbrown05, by = 'ponto', all=TRUE)

psl13.20 = psl13[,c(1:3,seq(5,13,2),14,15)]
names(psl13.20) = c('parc', 'ponto', 'data', 'ac', 'al', 'at', 'perc', 'pseco','tratamento', 'obs')
psl13.20.2 = merge(psl13.20, xyltbrown20, by = 'ponto', all=TRUE)

psl131 = rbind(psl13.5.2 , psl13.20.2)

##Altura do combustivel
r40acps13 = stack(r40(psl131, "ac"))

##Altura da liteira
r40alps13 = stack(r40(psl131, "al"))

##Altura do tapete
r40atps13 = stack(r40(psl131, "at"))

##Solo exposto
r40psecops13 = stack(r40(psl131, "pseco"))

##% de area queimada
r40percps13 = stack(r40(psl131, "perc"))

###############################################################################
###############################################################################
#LITEIRA COMBUSTÍVEL PÓS FOGO 2014

#Adcionando xy
psl14.5 = psl14[,c(1:3,seq(4,14,2),16,17)]
names(psl14.5) = c('parc', 'ponto', 'data', 'ac', 'al', 'at', 'pu', 'se', 'pseco', 'psaco', 'tratamento')
psl14.5.2 = merge(psl14.5, xyltbrown05, by = 'ponto', all=TRUE)

psl14.20 = psl14[,c(1:3,seq(5,15,2),16,17)]
names(psl14.20) = c('parc', 'ponto', 'data', 'ac', 'al', 'at', 'pu', 'se', 'pseco', 'psaco', 'tratamento')
psl14.20.2 = merge(psl14.20, xyltbrown20, by = 'ponto', all=TRUE)

psl141 = rbind(psl14.5.2 , psl14.20.2)

psl141$um = (((psl141$pu - psl141$psaco) - (psl141$pseco - psl141$psaco))*100) / (psl141$pseco - psl141$psaco)
psl141$pu2 = (psl141$pu - psl141$psaco)

##Altura do combustivel
r40acps14 = stack(r40(psl141, "ac"))

##Altura da liteira
r40alps14 = stack(r40(psl141, "al"))

##Altura do tapete
r40atps14 = stack(r40(psl141, "at"))

##Solo exposto
r40seps14 = stack(r40(psl141, "se"))

##% de umidade
psl141$um = ifelse (psl141$um  > 100, NA, psl141$um )
r40umps14 = stack(r40(psl141, "um"))

##Peso umido
r40pups14 = stack(r40(psl141, "pu2"))

###############################################################################
###############################################################################
#DADOS DO CENSO DE MORTALIDADE
csmr = csm[, c('plot','x_dist', 'y_dist', 'dap_c11', 'dap_c12', 'dap_c13', 'dap_c14',
				'c11', 'c12', 'c13', 'cop_c14', 'altura', 'mv')]
names(csmr) = c('parc','x', 'y', 'dap_11', 'dap_12', 'dap_13', 'dap_14',
				'c_11', 'c_12', 'c_13', 'c_14', 'alt', 'vm')

##
##DAP
##

##Dap 11
#r40dap11 = stack(r40(csmr, "dap_11"))
#levelplot(r40dap11)

##Dap 12
r40dap12 = stack(r40(csmr, "dap_12"))

##Dap 13
r40dap13 = stack(r40(csmr, "dap_13"))

##Dap 14
r40dap14 = stack(r40(csmr, "dap_14"))

##
##COPA
##

##copa 11
#r40c12 = stack(r40(csmr, "c_11"))
#levelplot(r40c11)

##copa 12
r40c12 = stack(r40(csmr, "c_12"))

##copa 13
r40c13 = stack(r40(csmr, "c_13"))

##copa 14
r40c14 = stack(r40(csmr, "c_14"))

##
## VM = VIVA | MORTA
##

##vm = viva ou morta
r40vm = stack(r40(csmr, "vm"))

###############################################################################
###############################################################################
#DADOS DE ÁREA QUEIMADA

setwd("H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_40x40m\\mapa_area_queimada\\dados")
r.names = list.files(pattern = "*.csv")

raqm = list()
for(i in 1:length(r.names))
{	
	cat("iteration = ", r.names[i], "\n")
	
	import.r = as.matrix(read.csv(r.names[i],header=F))
	
	r2 = raster(import.r)
	names(r2) = paste(substr(r.names[[i]],9,9), substr(r.names[[i]],10,11), sep = "_")
	raqm[[i]] = stack(r2)	
}

raqm2 = stack(raqm)
r40aqm = disaggregate(raqm2, fact = c(2,2))


###############################################################################
###############################################################################
#DADOS DE LAI
#unique(substr(lai$Time,1, 10))

lai1 = merge(lai, xylai, by.x = 'Rspns2' , by.y = 'ponto', all=TRUE)
lai1$parc = lai1$Rspns1

#Raster LAI
r40lai = stack(r40(lai1, "LAI"))




###############################################################################
###############################################################################
#SALVAR RASTERS
setwd(paste(path, "raster", sep=""))

#COMPORTAMENTO DO FOGO
writeRaster(r40alt, filename="r40alt.grd", overwrite=TRUE)
writeRaster(r40comp, filename="r40comp.grd", overwrite=TRUE)
writeRaster(r40larg, filename="r40larg.grd", overwrite=TRUE)
writeRaster(r40tdur, filename="r40tdur.grd", overwrite=TRUE)



##COMBUSTIVEL PRE FOGO 2013
writeRaster(r40x06pr13, filename="r40x06pr13.grd", overwrite=TRUE)
writeRaster(r40x26pr13, filename="r40x26pr13.grd", overwrite=TRUE)
writeRaster(r40x76pr13, filename="r40x76pr13.grd", overwrite=TRUE)
writeRaster(r40maxpr13, filename="r40maxpr13.grd", overwrite=TRUE)

##COMBUSTIVEL POS FOGO 2013
writeRaster(r40x06ps13, filename="r40x06ps13.grd", overwrite=TRUE)
writeRaster(r40x26ps13, filename="r40x26ps13.grd", overwrite=TRUE)
writeRaster(r40x76ps13, filename="r40x76ps13.grd", overwrite=TRUE)
writeRaster(r40maxps13, filename="r40maxps13.grd", overwrite=TRUE)

##COMBUSTIVEL POS FOGO 2014
writeRaster(r40x06ps14, filename="r40x06ps14.grd", overwrite=TRUE)
writeRaster(r40x26ps14, filename="r40x26ps14.grd", overwrite=TRUE)
writeRaster(r40x76ps14, filename="r40x76ps14.grd", overwrite=TRUE)



##LITEIRA COMBUSTIVEL PRE FOGO 2013
writeRaster(r40acpr13, filename="r40acpr13.grd", overwrite=TRUE)
writeRaster(r40alpr13, filename="r40alpr13.grd", overwrite=TRUE)
writeRaster(r40atpr13, filename="r40atpr13.grd", overwrite=TRUE)
writeRaster(r40sepr13, filename="r40sepr13.grd", overwrite=TRUE)
writeRaster(r40umpr13, filename="r40umpr13.grd", overwrite=TRUE)
writeRaster(r40pupr13, filename="r40pupr13.grd", overwrite=TRUE)

##LITEIRA COMBUSTIVEL POS FOGO 2013
writeRaster(r40acps13, filename="r40acps13.grd", overwrite=TRUE)
writeRaster(r40alps13, filename="r40alps13.grd", overwrite=TRUE)
writeRaster(r40atps13, filename="r40atps13.grd", overwrite=TRUE)
writeRaster(r40psecops13, filename="r40psecops13.grd", overwrite=TRUE)
writeRaster(r40percps13, filename="r40percps13.grd", overwrite=TRUE)

##LITEIRA COMBUSTIVEL POS FOGO 2014
writeRaster(r40acps14, filename="r40acps14.grd", overwrite=TRUE)
writeRaster(r40alps14, filename="r40alps14.grd", overwrite=TRUE)
writeRaster(r40atps14, filename="r40atps14.grd", overwrite=TRUE)
writeRaster(r40seps14, filename="r40seps14.grd", overwrite=TRUE)
writeRaster(r40umps14, filename="r40umps14.grd", overwrite=TRUE)
writeRaster(r40pups14, filename="r40pups14.grd", overwrite=TRUE)

#LAI
#writeRaster(r40lai, filename="r40lai.grd", overwrite=TRUE)

#MAPA DE AREA QUEIMADA
writeRaster(r40aqm, filename="r40aqm.grd", overwrite=TRUE)



###############################################################################
###############################################################################
#SALVAR GRÁFICOS EM ARQUIVO PDF

##
##COMPORTAMENTO DO FOGO
##
setwd(paste(path, "graficos", sep=""))
trellis.device(pdf, file="Comportamento_do_Fogo_2013.pdf")
levelplot(r40alt[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Comp_Fogo: Altura da chama', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40alt[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Comp_Fogo: Altura da chama', sub = 'Parcelas com adicao de combustivel')

levelplot(r40larg[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Comp_Fogo: Largura da chama', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40larg[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Comp_Fogo: Largura da chama', sub = 'Parcelas com adicao de combustivel')

levelplot(r40comp[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Comp_Fogo: Comprimento da chama', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40comp[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Comp_Fogo: Comprimento da chama', sub = 'Parcelas com adicao de combustivel')

levelplot(r40tdur[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Comp_Fogo: Taxa de espalhamento do fogo', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40tdur[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Comp_Fogo: Taxa de espalhamento do fogo', sub = 'Parcelas com adicao de combustivel')
dev.off()


##
##COMBUSTIVEL PRÉ E PÓS FOGO
##
#Pre fogo 2013
trellis.device(pdf, file="combustivel_pre_fogo_2013.pdf")
levelplot(r40x06pr13[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Comb_Pre Fogo: 0.0 - 0.6', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40x06pr13[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Comb_Pre Fogo: 0.0 - 0.6', sub = 'Parcelas com adicao de combustivel')
levelplot(r40x06pr13[[c('e_17', 'e_21', 'f_27', 'j_27', 'k_16', 'k_22')]], main='Comb_Pre Fogo: 0.0 - 0.6', sub = 'Parcelas queima noturna (cancelada)')
levelplot(r40x06pr13[[c('e_22', 'f_16', 'f_26', 'j_21', 'k_17', 'k_26')]], main='Comb_Pre Fogo: 0.0 - 0.6', sub = 'Parcelas controle')

levelplot(r40x26pr13[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Comb_Pre Fogo: 0.6 - 2.6', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40x26pr13[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Comb_Pre Fogo: 0.6 - 2.6', sub = 'Parcelas com adicao de combustivel')
levelplot(r40x26pr13[[c('e_17', 'e_21', 'f_27', 'j_27', 'j_16', 'k_22')]], main='Comb_Pre Fogo: 0.6 - 2.6', sub = 'Parcelas queima noturna (cancelada)')
levelplot(r40x26pr13[[c('e_22', 'f_16', 'f_26', 'j_21', 'k_17', 'k_26')]], main='Comb_Pre Fogo: 0.6 - 2.6', sub = 'Parcelas controle')

levelplot(r40x76pr13[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Comb_Pre Fogo: 2.6 - 7.6', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40x76pr13[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Comb_Pre Fogo: 2.6 - 7.6', sub = 'Parcelas com adicao de combustivel')
levelplot(r40x76pr13[[c('e_17', 'e_21', 'f_27', 'j_27', 'k_16', 'k_22')]], main='Comb_Pre Fogo: 2.6 - 7.6', sub = 'Parcelas queima noturna (cancelada)')
levelplot(r40x76pr13[[c('e_22', 'f_16', 'f_26', 'j_21', 'k_17', 'k_26')]], main='Comb_Pre Fogo: 2.6 - 7.6', sub = 'Parcelas controle')

levelplot(r40maxpr13[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Comb_Pre Fogo: Altura máxima do comb.', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40maxpr13[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Comb_Pre Fogo: Altura máxima do comb.', sub = 'Parcelas com adicao de combustivel')
levelplot(r40maxpr13[[c('e_17', 'e_21', 'f_27', 'j_27', 'k_16', 'k_22')]], main='Comb_Pre Fogo: Altura máxima do comb.', sub = 'Parcelas queima noturna (cancelada)')
levelplot(r40maxpr13[[c('e_22', 'f_16', 'f_26', 'j_21', 'k_17', 'k_26')]], main='Comb_Pre Fogo: Altura máxima do comb.', sub = 'Parcelas controle')
dev.off()

#Pos fogo 2013
trellis.device(pdf, file="combustivel_Pos_fogo_2013.pdf")
levelplot(r40x06ps13[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Comb_Pos Fogo: 0.0 - 0.6', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40x06ps13[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Comb_Pos Fogo: 0.0 - 0.6', sub = 'Parcelas com adicao de combustivel')

levelplot(r40x26ps13[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Comb_Pos Fogo: 0.6 - 2.6', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40x26ps13[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Comb_Pos Fogo: 0.6 - 2.6', sub = 'Parcelas com adicao de combustivel')

levelplot(r40x76ps13[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Comb_Pos Fogo: 2.6 - 7.6', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40x76ps13[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Comb_Pos Fogo: 2.6 - 7.6', sub = 'Parcelas com adicao de combustivel')

levelplot(r40maxps13[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Comb_Pos Fogo: Altura máxima do comb.', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40maxps13[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Comb_Pos Fogo: Altura máxima do comb.', sub = 'Parcelas com adicao de combustivel')
dev.off()

#Pos fogo 2014
trellis.device(pdf, file="combustivel_Pos_fogo_2014.pdf")
levelplot(r40x06ps14[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Comb_Pos Fogo: 0.0 - 0.6', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40x06ps14[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Comb_Pos Fogo: 0.0 - 0.6', sub = 'Parcelas com adicao de combustivel')
levelplot(r40x06ps14[[c('e_17', 'e_21', 'f_27', 'j_27', 'k_16', 'k_22')]], main='Comb_Pos Fogo: 0.0 - 0.6', sub = 'Parcelas queima noturna (cancelada)')
levelplot(r40x06ps14[[c('e_22', 'f_16', 'f_26', 'j_21', 'k_17', 'k_26')]], main='Comb_Pos Fogo: 0.0 - 0.6', sub = 'Parcelas controle')

levelplot(r40x26ps14[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Comb_Pos Fogo: 0.6 - 2.6', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40x26ps14[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Comb_Pos Fogo: 0.6 - 2.6', sub = 'Parcelas com adicao de combustivel')
levelplot(r40x26ps14[[c('e_17', 'e_21', 'f_27', 'j_27', 'k_16', 'k_22')]], main='Comb_Pos Fogo: 0.6 - 2.6', sub = 'Parcelas queima noturna (cancelada)')
levelplot(r40x26ps14[[c('e_22', 'f_16', 'f_26', 'j_21', 'k_17', 'k_26')]], main='Comb_Pos Fogo: 0.6 - 2.6', sub = 'Parcelas controle')

levelplot(r40x76ps14[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Comb_Pos Fogo: 2.6 - 7.6', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40x76ps14[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Comb_Pos Fogo: 2.6 - 7.6', sub = 'Parcelas com adicao de combustivel')
levelplot(r40x76ps14[[c('e_17', 'e_21', 'f_27', 'j_27', 'k_16', 'k_22')]], main='Comb_Pos Fogo: 2.6 - 7.6', sub = 'Parcelas queima noturna (cancelada)')
levelplot(r40x76ps14[[c('e_22', 'f_16', 'f_26', 'j_21', 'k_17', 'k_26')]], main='Comb_Pos Fogo: 2.6 - 7.6', sub = 'Parcelas controle')
dev.off()



##
##LITEIRA DO COMBUSTIVEL PRÉ E PÓS FOGO
##
#Pre fogo 2013
trellis.device(pdf, file="liteira_combustivel_pre_fogo_2013.pdf")
levelplot(r40acpr13[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Lit_Comb_Pre fogo: Altura do combustivel', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40acpr13[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Lit_Comb_Pre fogo: Altura do combustivel', sub = 'Parcelas com adicao de combustivel')
levelplot(r40acpr13[[c('e_22', 'f_16', 'f_26', 'j_21', 'k_17', 'k_26')]], main='Lit_Comb_Pre fogo: Altura do combustivel', sub = 'Parcelas controle')

levelplot(r40alpr13[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Lit_Comb_Pre fogo: Altura da liteira', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40alpr13[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Lit_Comb_Pre fogo: Altura da liteira', sub = 'Parcelas com adicao de combustivel')
levelplot(r40alpr13[[c('e_22', 'f_16', 'f_26', 'j_21', 'k_17', 'k_26')]], main='Lit_Comb_Pre fogo: Altura da liteira', sub = 'Parcelas controle')

levelplot(r40atpr13[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Lit_Comb_Pre fogo: Altura do tapete', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40atpr13[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Lit_Comb_Pre fogo: Altura do tapete', sub = 'Parcelas com adicao de combustivel')
levelplot(r40atpr13[[c('e_22', 'f_16', 'f_26', 'j_21', 'k_17', 'k_26')]], main='Lit_Comb_Pre fogo: Altura do tapete', sub = 'Parcelas controle')

levelplot(r40sepr13[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Lit_Comb_Pre fogo: Solo exposto', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40sepr13[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Lit_Comb_Pre fogo: Solo exposto', sub = 'Parcelas com adicao de combustivel')
levelplot(r40sepr13[[c('e_22', 'f_16', 'f_26', 'j_21', 'k_17', 'k_26')]], main='Lit_Comb_Pre fogo: Solo exposto', sub = 'Parcelas controle')

levelplot(r40umpr13[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Lit_Comb_Pre fogo: Umidade da liteira', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40umpr13[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Lit_Comb_Pre fogo: Umidade da liteira', sub = 'Parcelas com adicao de combustivel')
levelplot(r40umpr13[[c('e_22', 'f_16', 'f_26', 'j_21', 'k_17', 'k_26')]], main='Lit_Comb_Pre fogo: Umidade da liteira', sub = 'Parcelas controle')

levelplot(r40pupr13[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Lit_Comb_Pre fogo: Peso umido', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40pupr13[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Lit_Comb_Pre fogo: Peso umido', sub = 'Parcelas com adicao de combustivel')
levelplot(r40pupr13[[c('e_22', 'f_16', 'f_26', 'j_21', 'k_17', 'k_26')]], main='Lit_Comb_Pre fogo: Peso umido', sub = 'Parcelas controle')
dev.off()


#Pos fogo 2013
trellis.device(pdf, file="liteira_combustivel_pos_fogo_2013.pdf")
levelplot(r40acps13[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Lit_Comb_Pos fogo: Altura do combustivel', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40acps13[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Lit_Comb_Pos fogo: Altura do combustivel', sub = 'Parcelas com adicao de combustivel')

levelplot(r40alps13[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Lit_Comb_Pos fogo: Altura da liteira', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40alps13[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Lit_Comb_Pos fogo: Altura da liteira', sub = 'Parcelas com adicao de combustivel')

levelplot(r40atps13[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Lit_Comb_Pos fogo: Altura do tapete', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40atps13[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Lit_Comb_Pos fogo: Altura do tapete', sub = 'Parcelas com adicao de combustivel')

levelplot(r40psecops13[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Lit_Comb_Pos fogo: Umidade da liteira', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40psecops13[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Lit_Comb_Pos fogo: Umidade da liteira', sub = 'Parcelas com adicao de combustivel')

levelplot(r40percps13[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Lit_Comb_Pos fogo: Peso seco', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40percps13[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Lit_Comb_Pos fogo: Peso seco', sub = 'Parcelas com adicao de combustivel')
dev.off()

#Pos fogo 2014
trellis.device(pdf, file="liteira_combustivel_pre_fogo_2014.pdf")
levelplot(r40acps14[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Lit_Comb_Pos fogo: Altura do combustivel', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40acps14[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Lit_Comb_Pos fogo: Altura do combustivel', sub = 'Parcelas com adicao de combustivel')
levelplot(r40acps14[[c('e_17', 'e_21', 'f_27', 'j_27', 'k_16', 'k_22')]], main='Lit_Comb_Pos fogo: Altura do combustivel', sub = 'Parcelas queima noturna (cancelada)')
levelplot(r40acps14[[c('e_22', 'f_16', 'f_26', 'j_21', 'k_17', 'k_26')]], main='Lit_Comb_Pos fogo: Altura do combustivel', sub = 'Parcelas controle')

levelplot(r40alps14[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Lit_Comb_Pos fogo: Altura da liteira', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40alps14[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Lit_Comb_Pos fogo: Altura da liteira', sub = 'Parcelas com adicao de combustivel')
levelplot(r40alps14[[c('e_17', 'e_21', 'f_27', 'j_27', 'k_16', 'k_22')]], main='Lit_Comb_Pos fogo: Altura da liteira', sub = 'Parcelas queima noturna (cancelada)')
levelplot(r40alps14[[c('e_22', 'f_16', 'f_26', 'j_21', 'k_17', 'k_26')]], main='Lit_Comb_Pos fogo: Altura da liteira', sub = 'Parcelas controle')

levelplot(r40atps14[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Lit_Comb_Pos fogo: Altura do tapete', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40atps14[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Lit_Comb_Pos fogo: Altura do tapete', sub = 'Parcelas com adicao de combustivel')
levelplot(r40atps14[[c('e_17', 'e_21', 'f_27', 'j_27', 'k_16', 'k_22')]], main='Lit_Comb_Pos fogo: Altura do tapete', sub = 'Parcelas queima noturna (cancelada)')
levelplot(r40atps14[[c('e_22', 'f_16', 'f_26', 'j_21', 'k_17', 'k_26')]], main='Lit_Comb_Pos fogo: Altura do tapete', sub = 'Parcelas controle')

levelplot(r40seps14[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Lit_Comb_Pos fogo: Solo exposto', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40seps14[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Lit_Comb_Pos fogo: Solo exposto', sub = 'Parcelas com adicao de combustivel')
levelplot(r40seps14[[c('e_17', 'e_21', 'f_27', 'j_27', 'k_16', 'k_22')]], main='Lit_Comb_Pos fogo: Solo exposto', sub = 'Parcelas queima noturna (cancelada)')
levelplot(r40seps14[[c('e_22', 'f_16', 'f_26', 'j_21', 'k_17', 'k_26')]], main='Lit_Comb_Pos fogo: Solo exposto', sub = 'Parcelas controle')

levelplot(r40umps14[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Lit_Comb_Pos fogo: Umidade da liteira', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40umps14[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Lit_Comb_Pos fogo: Umidade da liteira', sub = 'Parcelas com adicao de combustivel')
levelplot(r40umps14[[c('e_17', 'e_21', 'f_27', 'j_27', 'k_16', 'k_22')]], main='Lit_Comb_Pos fogo: Umidade da liteira', sub = 'Parcelas queima noturna (cancelada)')
levelplot(r40umps14[[c('e_22', 'f_16', 'f_26', 'j_21', 'k_17', 'k_26')]], main='Lit_Comb_Pos fogo: Umidade da liteira', sub = 'Parcelas controle')

levelplot(r40pups14[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Lit_Comb_Pos fogo: Peso umido', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40pups14[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Lit_Comb_Pos fogo: Peso umido', sub = 'Parcelas com adicao de combustivel')
levelplot(r40pups14[[c('e_17', 'e_21', 'f_27', 'j_27', 'k_16', 'k_22')]], main='Lit_Comb_Pos fogo: Peso umido', sub = 'Parcelas queima noturna (cancelada)')
levelplot(r40pups14[[c('e_22', 'f_16', 'f_26', 'j_21', 'k_17', 'k_26')]], main='Lit_Comb_Pos fogo: Peso umido', sub = 'Parcelas controle')
dev.off()



##
##CENSO DE MORTALIDADE
##
#dap
trellis.device(pdf, file="Censo_mortalidade_DAP.pdf")
levelplot(r40dap12[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Cens_Mortalidade:DAP 2012', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40dap12[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Cens_Mortalidade:DAP 2012', sub = 'Parcelas com adicao de combustivel')
levelplot(r40dap12[[c('e_17', 'e_21', 'f_27', 'j_27', 'k_16', 'k_22')]], main='Cens_Mortalidade:DAP 2012', sub = 'Parcelas queima noturna (cancelada)')
levelplot(r40dap12[[c('e_22', 'f_16', 'f_26', 'j_21', 'k_17', 'k_26')]], main='Cens_Mortalidade:DAP 2012', sub = 'Parcelas controle')

levelplot(r40dap13[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Cens_Mortalidade:DAP 2013', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40dap13[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Cens_Mortalidade:DAP 2013', sub = 'Parcelas com adicao de combustivel')
levelplot(r40dap13[[c('e_17', 'e_21', 'f_27', 'j_27', 'k_16', 'k_22')]], main='Cens_Mortalidade:DAP 2013', sub = 'Parcelas queima noturna (cancelada)')
levelplot(r40dap13[[c('e_22', 'f_16', 'f_26', 'j_21', 'k_17', 'k_26')]], main='Cens_Mortalidade:DAP 2013', sub = 'Parcelas controle')

levelplot(r40dap14[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Cens_Mortalidade:DAP 2014', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40dap14[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Cens_Mortalidade:DAP 2014', sub = 'Parcelas com adicao de combustivel')
levelplot(r40dap14[[c('e_17', 'e_21', 'f_27', 'j_27', 'k_16', 'k_22')]], main='Cens_Mortalidade:DAP 2014', sub = 'Parcelas queima noturna (cancelada)')
levelplot(r40dap14[[c('e_22', 'f_16', 'f_26', 'j_21', 'k_17', 'k_26')]], main='Cens_Mortalidade:DAP 2014', sub = 'Parcelas controle')
dev.off()

#copa
trellis.device(pdf, file="Censo_mortalidade_COPA.pdf")
levelplot(r40c12[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Cens_Mortalidade:COPA 2012', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40c12[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Cens_Mortalidade:COPA 2012', sub = 'Parcelas com adicao de combustivel')
levelplot(r40c12[[c('e_17', 'e_21', 'f_27', 'j_27', 'k_16', 'k_22')]], main='Cens_Mortalidade:COPA 2012', sub = 'Parcelas queima noturna (cancelada)')
levelplot(r40c12[[c('e_22', 'f_16', 'f_26', 'j_21', 'k_17', 'k_26')]], main='Cens_Mortalidade:COPA 2012', sub = 'Parcelas controle')

levelplot(r40c13[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Cens_Mortalidade:COPA 2013', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40c13[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Cens_Mortalidade:COPA 2013', sub = 'Parcelas com adicao de combustivel')
levelplot(r40c13[[c('e_17', 'e_21', 'f_27', 'j_27', 'k_16', 'k_22')]], main='Cens_Mortalidade:COPA 2013', sub = 'Parcelas queima noturna (cancelada)')
levelplot(r40c13[[c('e_22', 'f_16', 'f_26', 'j_21', 'k_17', 'k_26')]], main='Cens_Mortalidade:COPA 2013', sub = 'Parcelas controle')

levelplot(r40c14[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Cens_Mortalidade:COPA 2014', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40c14[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Cens_Mortalidade:COPA 2014', sub = 'Parcelas com adicao de combustivel')
levelplot(r40c14[[c('e_17', 'e_21', 'f_27', 'j_27', 'k_16', 'k_22')]], main='Cens_Mortalidade:COPA 2014', sub = 'Parcelas queima noturna (cancelada)')
levelplot(r40c14[[c('e_22', 'f_16', 'f_26', 'j_21', 'k_17', 'k_26')]], main='Cens_Mortalidade:COPA 2014', sub = 'Parcelas controle')
dev.off()

#altura	
trellis.device(pdf, file="censo_mortalidade_mv.pdf")
levelplot(r40vm[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Cens_Mortalidade: Viva|Morta', sub = 'Parcelas sem adicao de combustivel')
levelplot(r40vm[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Cens_Mortalidade: Viva|Morta', sub = 'Parcelas com adicao de combustivel')
levelplot(r40vm[[c('e_17', 'e_21', 'f_27', 'j_27', 'k_16', 'k_22')]], main='Cens_Mortalidade: Viva|Morta', sub = 'Parcelas queima noturna (cancelada)')
levelplot(r40vm[[c('e_22', 'f_16', 'f_26', 'j_21', 'k_17', 'k_26')]], main='Cens_Mortalidade: Viva|Morta', sub = 'Parcelas controle')
dev.off()

##
##MAPA DE ÁREA QUEIMADA
##
#Pos fogo 2013
trellis.device(pdf, file="mapa_de_area_queimada_2013.pdf")
levelplot(raqm2[[c('e_27', 'f_17', 'f_22', 'j_16', 'j_22', 'k_27')]], main='Mapa_area_queimada', sub = 'Parcelas sem adicao de combustivel')
levelplot(raqm2[[c('e_16', 'e_26', 'f_21', 'j_17', 'j_26', 'k_21')]], main='Mapa_area_queimada', sub = 'Parcelas com adicao de combustivel')
dev.off()

###############################################################################
###############################################################################
rm(list=ls())
###############################################################################
###############################################################################