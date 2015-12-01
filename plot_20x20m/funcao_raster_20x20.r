###############################################################################
###############################################################################
#Author: Oliveira-Santos, claudinei
#Instituto de Pesquisa Ambiental da Amazônia (IPAM)
#Agosto 31, 2015
#
#Script para criar rasters do inventário, comportamento do fogo e combustível 
#Parcelas de 20x20 metros do projeto "fogos_pequenos"
#
###
##PACOTES
###
 library(raster)
 library(gstat)
 library(rasterVis)
 library(lubridate)

###############################################################################
###############################################################################
#OBJETOS NECESSÁRIOS PARA CRIAR OS RASTERS
 
##
#Raster vazio para ser preenchido
 r = raster(ncols = 20, nrows=20,
		 xmn = 0, xmx = 20, ymn = 0, ymx = 20)
 
##
## FUNCAO PARA CRIAR OS RASTERS (INTERPOLATE USANDO O MODELO DO GSTAT)
##
 
 r20 = function (data, var)
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