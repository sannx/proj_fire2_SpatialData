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

#Funcao local
source("H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_20x20m\\sig_20x20\\scripts\\scripts_objR\\funcao_raster_20x20.R", local=TRUE)
###############################################################################
###############################################################################
#DIRETÓRIO PRINCIPAL
setwd("H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_20x20m\\sig_20x20")
path = "H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_20x20m\\"
###############################################################################
###############################################################################
#IMPORTAR OS ARQUIVOS UTILIZADOS NESTE SCRIPT

csm = read.csv(paste(path,"censo\\xy_arvores_20x20m.csv",sep = ''))
cmb = read.csv(paste(path,"combustivel\\master_combustivel_20x20.csv",sep = ''))
cpf = read.csv(paste(path,"comportamento_fogo\\master_comportamento_fogo_20X20.csv",sep = ''))
ltc = read.csv(paste(path,"liteira_combustivel\\master_liteira_combustivel_20x20.csv",sep = ''))

###############################################################################
###############################################################################
###
##COMBUSTÍVEL PRÉ FOGO
###
cmb$parc2 = cmb$parc

cmb$parc = paste(cmb$parc2, cmb$ano, cmb$pre_pos, sep = '_')

cmbpre = cmb[cmb$pre_pos == 'pre',]

##combustível de 0.0 a 0.6
writeRaster(stack(r20(cmbpre, "BMg.ha.06")), filename=paste(path, "sig_20x20\\raster\\cmb_prex06.grd", sep=""), overwrite=TRUE)

##combustível de 0.6 a 2.6
writeRaster(stack(r20(cmbpre, "BMg.ha.25")), filename=paste(path, "sig_20x20\\raster\\cmb_prex26.grd", sep=""), overwrite=TRUE)

##combustível de 2.6 a 7.6
writeRaster(stack(r20(cmbpre, "BMg.ha.76")), filename=paste(path, "sig_20x20\\raster\\cmb_prex76.grd", sep=""), overwrite=TRUE)

##altura máxima do combustível
#writeRaster(stack(r20(cmbpre[cmbpre$ano==2013,], 'Max.')), filename=paste(path, "sig_20x20\\raster\\cmb_premax.grd", sep=""), overwrite=TRUE)

###
##COMBUSTÍVEL PÓS FOGO
###
cmbpos = cmb[cmb$pre_pos == 'pos',]

##combustível de 0.0 a 0.6
writeRaster(stack(r20(cmbpos, "BMg.ha.06")), filename=paste(path, "sig_20x20\\raster\\cmb_posx06.grd", sep=""), overwrite=TRUE)

##combustível de 0.6 a 2.6
writeRaster(stack(r20(cmbpos, "BMg.ha.25")), filename=paste(path, "sig_20x20\\raster\\cmb_posx26.grd", sep=""), overwrite=TRUE)

##combustível de 2.6 a 7.6
writeRaster(stack(r20(cmbpos, "BMg.ha.76")), filename=paste(path, "sig_20x20\\raster\\cmb_posx76.grd", sep=""), overwrite=TRUE)

##altura máxima do combustível
#writeRaster(stack(r20(cmbpos[cmbpos$ano==2013,], 'Max.')), filename=paste(path, "sig_20x20\\raster\\cmb_posmax.grd", sep=""), overwrite=TRUE)

###############################################################################
###############################################################################
###
##COMPORTAMENTO DO FOGO
###
cpf$parc2 = cpf$parc

cpf$parc = paste(cpf$parc, cpf$ano, sep = '_')

#Altura da chama
writeRaster(stack(r20(cpf, "alt")), filename=paste(path, "sig_20x20\\raster\\cpf_alt.grd", sep=""), overwrite=TRUE)

#Comprimento da chama
writeRaster(stack(r20(cpf, "comp")), filename=paste(path, "sig_20x20\\raster\\cpf_comp.grd", sep=""), overwrite=TRUE)

#Largura da chama
writeRaster(stack(r20(cpf, "larg")), filename=paste(path, "sig_20x20\\raster\\cpf_larg.grd", sep=""), overwrite=TRUE)

#Taxa de espalhamento do fogo
writeRaster(stack(r20(cpf, "tdur")), filename=paste(path, "sig_20x20\\raster\\cpf_tdur.grd", sep=""), overwrite=TRUE)

###############################################################################
###############################################################################
#LITEIRA DO COMBUSTÍVEL
ltc$parc2 = ltc$parc

ltc$parc = paste(ltc$parc2, ltc$ano, ltc$pre_pos, sep = '_')

##peso umido
r20x06 = stack(r20(ltc, "pu2"))

##percentual de umidade
r20x26 = stack(r20(ltc, "um"))

##altura do combustível
r20x76 = stack(r20(ltc, "ac"))

##altura da liteira
r20x76 = stack(r20(ltc, "al"))

##altura do tapete
r20x76 = stack(r20(ltc, "at"))

##Percentual de solo exposto
r20x76 = stack(r20(ltc, "se"))

###############################################################################
###############################################################################
#DADOS DE LAI
#unique(substr(lai$Time,1, 10))
#
#lai1 = merge(lai, xylai, by.x = 'Rspns2' , by.y = 'ponto', all=TRUE)
#	lai1$parc = lai1$Rspns1
#
##Raster LAI
#r20lai = stack(r20(lai1, "LAI"))


###############################################################################
###############################################################################
rm(list = ls())
###############################################################################
###############################################################################