###############################################################################
###############################################################################
#Author: Oliveira-Santos, claudinei
#Instituto de Pesquisa Ambiental da Amazônia (IPAM)
#Agosto 31, 2015
#
#Script para criar rasters do inventário, comportamento do fogo e combustível 
#Parcelas de 20x20 metros do projeto "fogos_pequenos"
#
###############################################################################
###############################################################################
#PACOTES UTILIZADOS NESTE SCRIPT
 library(raster)
 library(gstat)
 library(rasterVis)
 library(lubridate)
 library(data.table)

###############################################################################
###############################################################################
#FUNÇÃO PARA EXTRAIR OS VALORES DOS RASTER COM BASE NO XY DAS ÁRVORES

temp = list()  #Lista de dados extraidos 

  r40ext <- function (rvar, xy, variavel)
 {
	 rparc = as.character(names(rvar))
	 
	 for(i in 1:length(rparc))
	 {
		 rvar1 = rvar[[rparc[[i]]]]
		 xy2 = xy[xy$parc == rparc[[i]],]
		 
		 xy2$datavar = extract(rvar1, xy2[,c('x', 'y')], buffer = 2, fun = mean) 	
		 names(xy2) = c('tag', 'parc', 'x', 'y', variavel)
		 temp[[i]] = xy2
	 } 
	 xy3 = data.frame(data.table(do.call("rbind", temp)))
	 
	 return(xy3)
 }
 
###############################################################################
###############################################################################
#DIRETÓRIO PRINCIPAL
setwd("H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_40x40m\\sig_40x40\\raster")

###############################################################################
###############################################################################
#IMPORTAR OS ARQUIVOS UTILIZADOS NESTE SCRIPT

##
##dados do inventário
##
csm = read.csv("H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_40x40m\\censo\\1.MASTER_parcelas_40x40m.csv", header = T)

##
##COMPORTAMENTO DO FOGO
r40alt= brick("r40alt.grd")
r40comp= brick("r40comp.grd")
r40larg= brick("r40larg.grd")
r40tdur= brick("r40tdur.grd")

##COMBUSTIVEL PRE FOGO 2013
r40x06pr13= brick("r40x06pr13.grd")
r40x26pr13= brick("r40x26pr13.grd")
r40x76pr13= brick("r40x76pr13.grd")
r40maxpr13= brick("r40maxpr13.grd")

##COMBUSTIVEL POS FOGO 2013
r40x06ps13= brick("r40x06ps13.grd")
r40x26ps13= brick("r40x26ps13.grd")
r40x76ps13= brick("r40x76ps13.grd")
r40maxps13= brick("r40maxps13.grd")

##COMBUSTIVEL POS FOGO 2014
r40x06ps14= brick("r40x06ps14.grd")
r40x26ps14= brick("r40x26ps14.grd")
r40x76ps14= brick("r40x76ps14.grd")

##LITEIRA COMBUSTIVEL PRE FOGO 2013
r40acpr13= brick("r40acpr13.grd")
r40alpr13= brick("r40alpr13.grd")
r40atpr13= brick("r40atpr13.grd")
r40sepr13= brick("r40sepr13.grd")
r40umpr13= brick("r40umpr13.grd")
r40pupr13= brick("r40pupr13.grd")

##LITEIRA COMBUSTIVEL POS FOGO 2013
r40acps13= brick("r40acps13.grd")
r40alps13= brick("r40alps13.grd")
r40atps13= brick("r40atps13.grd")
r40psecops13= brick("r40psecops13.grd")
r40percps13= brick("r40percps13.grd")

##LITEIRA COMBUSTIVEL POS FOGO 2014
r40acps14= brick("r40acps14.grd")
r40alps14= brick("r40alps14.grd")
r40atps14= brick("r40atps14.grd")
r40seps14= brick("r40seps14.grd")
r40umps14= brick("r40umps14.grd")
r40pups14= brick("r40pups14.grd")

#LAI
#r40lai= brick("r40lai.grd")

#MAPA DE AREA QUEIMADA
r40aqm= brick("r40aqm.grd")

###############################################################################
###############################################################################
#EXTRAIR DADOS DOS RASTERS (EXRACT)
# placa e xy dos dados de inventário
csmxy = csm[,c('tag', 'plot', 'x_dist', 'y_dist')]
names(csmxy) = c('tag', 'parc', 'x', 'y')


r40alt= brick("r40alt.grd")

csm.alt = r40ext(r40alt, csmxy, 'alt_chama')

r40comp= brick("r40comp.grd")

csm.comp = r40ext(r40comp, csmxy, 'comp_chama')

r40larg= brick("r40larg.grd")

csm.larg = r40ext(r40larg, csmxy, 'larg_chama')

r40tdur= brick("r40tdur.grd")

csm.tdur = r40ext(r40tdur, csmxy, 'tdur_chama')

csm.cpf1 = merge(csm.alt, csm.comp, all.xy = TRUE)
csm.cpf2 = merge(csm.cpf1, csm.larg, all.xy = TRUE)
csm.cpf3 = merge(csm.cpf2, csm.tdur, all.xy = TRUE)


csm.cpf.all = merge(csm, csm.cpf3, by = 'tag', all = TRUE)

dim(csm)
head(csm)

dim(csm.cpf.all )
head(csm.cpf.all )


#		r40alt ,r40comp, r40larg, r40tdur,
#		r40alt ,r40comp, r40larg, r40tdur,
#		r40x06pr13, r40x26pr13, r40x76pr13, r40maxpr13,
#		r40x06ps13, r40x26ps13, r40x76ps13, r40maxps13,
#		r40x06ps14, r40x26ps14, r40x76ps14,
#		r40acpr13, r40alpr13, r40atpr13, r40sepr13, r40umpr13,  r40pupr13,
#		r40acps13, r40alps13, r40atps13, r40psecops13, r40percps13,
#		r40acps14, r40alps14, r40atps14, r40seps14, r40umps14, r40pups14,
#		r40aqm



write.csv(x, file = "master_40x40_fogo.csv", row.names=FALSE)


###############################################################################
###############################################################################