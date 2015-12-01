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
 library(data.table)

#Funcao local
source("H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_20x20m\\sig_20x20\\scripts\\scripts_objR\\funcao_extract_raster_20x20.R", local=TRUE)

###############################################################################
###############################################################################
#DIRETÓRIO PRINCIPAL
setwd("H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_20x20m\\sig_20x20\\raster")
path = "H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_20x20m\\sig_20x20\\"

###############################################################################
###############################################################################
#IMPORTAR OS ARQUIVOS UTILIZADOS NESTE SCRIPT
csm = read.csv("H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_20x20m\\censo\\xy_arvores_20x20m.csv", header = T)
	names(csm) = c('tag', 'parc', 'cod', 'rec', 'vm', 'x', 'y')

###############################################################################
###############################################################################
#EXTRAIR VALORES
#COMPORTAMENTO DO FOGO

#altura da chama
cpf.alt = r20ext(brick("cpf_alt.grd"), csm, 'alt_chama')

#comprimento da chama
cpf.comp = r20ext( brick("cpf_comp.grd"), csm, 'comp_chama')

#largura da chama
cpf.larg = r20ext(brick("cpf_larg.grd"), csm, 'larg_chama')

#Taxa de espalhamento da chama
cpf.tdur = r20ext(brick("cpf_tdur.grd"), csm, 'tdur_chama')

###
##juntar e salvar os dados de comportamento do fogo
###

cpf.master = merge(cpf.alt,cpf.comp, all = TRUE)
cpf.master1 = merge(cpf.master,cpf.larg, all = TRUE)
cpf.master2 = merge(cpf.master1, cpf.tdur, all = TRUE)

write.csv(cpf.master2, file = paste(path, 'master_extraido\\master_ext_comp_fogo_20x20.csv', sep = ''), row.names = FALSE)


###############################################################################
###############################################################################

###
##COMBUSTIVEL PRÉ FOGO
###
#0 - 0.6
cmb.06 = r20ext(brick("cmb_prex06.grd"), csm, 'cmb_06')

#0.6 - 2.6
cmb.26 = r20ext(brick("cmb_prex26.grd"), csm, 'cmb_26')

#2.6 - 7.6
cmb.76 = r20ext(brick("cmb_prex76.grd"), csm, 'cmb_76')

cmb.master = merge(cmb.06,cmb.26, all = TRUE)
cmb.master1 = merge(cmb.master,cmb.76, all = TRUE)

write.csv(cmb.master1, file = paste(path, 'master_extraido\\master_ext_combustivel_pre_20x20.csv', sep = ''), row.names = FALSE)

###
##COMBUSTIVEL PÓS FOGO
###
#0 - 0.6
cmb.06 = r20ext(brick("cmb_posx06.grd"), csm, 'cmb_06')

#0.6 - 2.6
cmb.26 = r20ext(brick("cmb_posx26.grd"), csm, 'cmb_26')

#2.6 - 7.6
cmb.76 = r20ext(brick("cmb_posx76.grd"), csm, 'cmb_76')

cmb.master = merge(cmb.06,cmb.26, all = TRUE)
cmb.master1 = merge(cmb.master,cmb.76, all = TRUE)

write.csv(cmb.master1, file = paste(path, 'master_extraido\\master_ext_combustivel_pos_20x20.csv', sep = ''), row.names = FALSE)


#LAI

#LITEIRA DO COMBUSTIVEL

#PSYCHROMETRO

#MAPA DE ÁREA QUEIMADA

###############################################################################
###############################################################################
rm(list = ls())
###############################################################################
###############################################################################