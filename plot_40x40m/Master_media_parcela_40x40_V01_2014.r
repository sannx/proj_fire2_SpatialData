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
###
##Pacotes
###
library(plyr)
library(doBy)

###
##Diretorio
###
setwd("H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_20x20m\\sig_20x20")

###
##Arquivos
###
cmb = read.csv("H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_20x20m\\combustivel\\master_combustivel_20x20.csv")
lcmb = read.csv("H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_20x20m\\liteira_combustivel\\master_liteira_combustivel_20x20.csv")
cpf = read.csv('H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_20x20m\\comportamento_fogo\\master_comportamento_fogo_20x20.csv')

###
##liteira do combustivel
###
lcmb.parc = summaryBy(ac + al + at + pu2 + pseco2 + um ~ parc + pre_pos , data = lcmb, FUN = 'mean', na.rm = TRUE )

	names(lcmb.parc) = c('parc', 'pre_pos', 'lcomb_alt_comb', 'lcomb_alt_lit', 
		'lcomb_alt_tap', 'lcomb_pes_umid', 'lcomb_pes_seco', 'lcomb_perc_umid')

##
pre = lcmb.parc[lcmb.parc$pre_pos == 'pre',]

	names(pre) = c('parc', 'pre_pos', 'prelcmb_alt_comb', 'prelcmb_alt_lit', 
		'prelcmb_alt_tap', 'prelcmb_pes_umid', 'prelcmb_pes_seco', 'prelcmb_perc_umid')

##
pos = lcmb.parc[lcmb.parc$pre_pos == 'pos',]

	names(pos) = c('parc', 'pre_pos', 'poslcmb_alt_comb', 'poslcmb_alt_lit', 
		'poslcmb_alt_tap', 'poslcmb_pes_umid', 'poslcmb_pes_seco', 'poslcmb_perc_umid')

##
lcmb.parc1 = merge(pre[,-2], pos[,-2], all = TRUE)

lcmb.parc1 = lcmb.parc1[with(lcmb.parc1, order(parc)),]


###
##combustivel
###
cmb.parc = summaryBy(BMg.ha.06 + BMg.ha.25 + BMg.ha.76 + boa + media + podre ~ 
				parc + tratamento + pre_pos + ano, data = cmb, FUN = 'mean', na.rm = TRUE )

	names(cmb.parc) = c('parc', 'trat', 'pre_pos', 'ano', 'comb_ha_06', 'comb_ha_25', 
		'comb_ha_76', 'comb_>76_boa', 'comb_>76_media', 'comb_>76_podre')

##
pre = cmb.parc[cmb.parc$pre_pos == 'pre',]

	names(pre) = c('parc', 'trat', 'pre_pos', 'ano', 'precmb_ha_06', 'precmb_ha_25', 
		'precmb_ha_76', 'precmb_>76_boa', 'precmb_>76_media', 'precmb_>76_podre')

##
pos = cmb.parc[cmb.parc$pre_pos == 'pos',]
	
	names(pos) = c('parc', 'trat', 'pre_pos', 'ano', 'poscmb_ha_06', 'poscmb_ha_25', 
		'poscmb_ha_76', 'poscmb_>76_boa', 'poscmb_>76_media', 'poscmb_>76_podre')

##
cmb.parc1 = merge(pre[,-3], pos[,-3], all = TRUE)

cmb.parc1 = cmb.parc1[with(cmb.parc1, order(parc)),]

###
##Comportamento do fogo
###
cpf.parc = summaryBy(alt + larg + comp + tdur + velvento ~ parc, data = cpf, FUN = 'mean', na.rm=TRUE)

	names(cpf.parc) = c('parc', 'cpf_alt_chama', 'cpf_larg_chama', 'cpf_comp_chama', 'cpf_tax_espalh', 'cpf_vel_vento')


###
##Merge media por parcela
###
master = merge(cmb.parc1, lcmb.parc1, all = TRUE)

	master1 = merge(master, cpf.parc, all = TRUE)

###############################################################################
###############################################################################
setwd("H:\\Dropbox\\trabalho\\projetos\\projeto_fogos_pequenos\\parcelas_20x20m\\sig_20x20\\")

write.csv(master1, file = 'master_parcela_queima_20x20.csv', row.names=FALSE)

###############################################################################
###############################################################################
rm(list = ls())
###############################################################################
###############################################################################