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
 library(data.table)

###############################################################################
###############################################################################
#FUNÇÃO PARA EXTRAIR OS VALORES DOS RASTER COM BASE NO XY DAS ÁRVORES

temp = list()  #Lista de dados extraidos 

r20ext <- function (rvar, xy, variavel)
{
	rparc = as.character(names(rvar))
	
	for(i in 1:length(rparc))
	{
		rvar1 = rvar[[rparc[[i]]]]
		xy2 = xy[xy$parc == substr(rparc[[i]],1,4),]
		
		xy2$datavar = extract(rvar1, xy2[,c('x', 'y')])#, buffer = 2, fun = mean)
		names(xy2) = c('tag', 'parc', 'cod', 'rec', 'vm', 'x', 'y', variavel)
		
		temp[[i]] = xy2
	} 
	xy3 = data.frame(data.table(do.call("rbind", temp)))
	
	return(xy3)
}

###############################################################################
###############################################################################