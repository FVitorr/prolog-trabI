/* ======================================================
   SISTEMA ESPECIALISTA: DIAGNÓSTICO DE PROBLEMAS AUTOMOTIVOS
   ======================================================
   
   Este sistema especialista auxilia na identificacão e resolucão
   de problemas comuns em veículos automotores.
*/

/* ==================== FATOS SOBRE COMPONENTES ==================== */

/* Estrutura: carro(Modelo, Ano, Motor, KM, [ComponentesComProblema]) */
carro('Ford Ka', 2018, '1.0 Flex', 45000, []).
carro('Volkswagen Gol', 2015, '1.6 MSI', 78000, [sistema_eletrico]).
carro('Chevrolet Onix', 2020, '1.0 Turbo', 32000, [sistema_combustivel]).
carro('Fiat Uno', 2012, '1.4 Fire', 120000, [motor, sistema_suspensao]).
carro('Toyota Corolla', 2019, '2.0 Flex', 25000, []).


/* ==================== PROBLEMAS CONHECIDOS POR MARCA/MODELO ==================== */

/* Ford */
problema_marca('Ford', 'Ford Ka', [bateria_fraca, alternador_defeituoso, vazamento_oleo_motor]).
problema_marca('Ford', 'Ford Fiesta', [problema_cambio_powershift, sensor_maf, vazamento_radiador]).
problema_marca('Ford', 'Ford Focus', [problema_cambio_powershift, bomba_combustivel_fraca, barulho_suspensao]).

/* Volkswagen */
problema_marca('Volkswagen', 'Gol', [sensor_maf_sujo, problema_vela_ignicao, vazamento_tampa_valvulas]).
problema_marca('Volkswagen', 'Polo', [problema_sensor_estacionamento, central_multimidia_defeituosa]).
problema_marca('Volkswagen', 'Saveiro', [problema_cabine, barulho_cambio, vazamento_diferencial]).

/* Chevrolet */
problema_marca('Chevrolet', 'Onix', [problema_modulo_freio, sensor_combustivel_defeituoso, barulho_portas]).
problema_marca('Chevrolet', 'Prisma', [problema_bico_injetor, vazamento_oleo_cambio]).
problema_marca('Chevrolet', 'S10', [problema_suspensao_traseira, vazamento_turbo]).

/* Fiat */
problema_marca('Fiat', 'Uno', [vazamento_oleo_motor, junta_cabecote_queimada, problema_embreagem]).
problema_marca('Fiat', 'Palio', [problema_bomba_combustivel, barulho_tambor_freio]).
problema_marca('Fiat', 'Strada', [problema_cabine, vazamento_diferencial]).

/* Toyota */
problema_marca('Toyota', 'Corolla', [problema_sensor_estacionamento, barulho_painel]).
problema_marca('Toyota', 'Hilux', [problema_sensor_combustivel, vazamento_tampa_valvulas]).
problema_marca('Toyota', 'Etios', [problema_borrachas_portas, barulho_suspensao]).

/* Honda */
problema_marca('Honda', 'Civic', [problema_central_multimidia, vazamento_oleo_motor]).
problema_marca('Honda', 'Fit', [problema_bateria, barulho_suspensao_traseira]).

/* ==================== PROBLEMAS ESPECÍFICOS DE COMPONENTES ==================== */

/* Problemas específicos do câmbio PowerShift da Ford */
problema_especifico('Ford', 'Focus', problema_cambio_powershift, 
    [tranco_cambio, marcha_nao_engata, barulho_cambio], 
    'Falha comum no sistema de embreagem dupla', 
    [substituir_embreagem_powershift, atualizacao_software], 
    critico).

/* Problema de módulo de freio do Onix */
problema_especifico('Chevrolet', 'Onix', problema_modulo_freio,
    [pedal_freio_duro, luz_abs_acessa, freio_nao_retorna],
    'Defeito no módulo hidráulico do ABS',
    [substituir_modulo_freio],
    grave).


/* Componentes principais do veículo */
componente(motor).
componente(sistema_eletrico).
componente(sistema_combustivel).
componente(sistema_refrigeracao).
componente(sistema_freios).
componente(sistema_transmissao).
componente(sistema_suspensao).
componente(sistema_exaustao).
componente(sistema_direcao).
componente(pneus).

/* Subcomponentes do motor */
subcomponente(motor, pistoes).
subcomponente(motor, aneis).
subcomponente(motor, virabrequim).
subcomponente(motor, bielas).
subcomponente(motor, comando_valvulas).
subcomponente(motor, valvulas).
subcomponente(motor, junta_cabecote).
subcomponente(motor, correia_dentada).
subcomponente(motor, bomba_oleo).
subcomponente(motor, filtro_oleo).

/* Subcomponentes do sistema elétrico */
subcomponente(sistema_eletrico, bateria).
subcomponente(sistema_eletrico, alternador).
subcomponente(sistema_eletrico, motor_partida).
subcomponente(sistema_eletrico, velas_ignicao).
subcomponente(sistema_eletrico, cabos_vela).
subcomponente(sistema_eletrico, bobina_ignicao).
subcomponente(sistema_eletrico, fusivel).
subcomponente(sistema_eletrico, rele).
subcomponente(sistema_eletrico, central_eletrica).
subcomponente(sistema_eletrico, sensores).

/* Subcomponentes do sistema de combustível */
subcomponente(sistema_combustivel, tanque).
subcomponente(sistema_combustivel, bomba_combustivel).
subcomponente(sistema_combustivel, filtro_combustivel).
subcomponente(sistema_combustivel, injetores).
subcomponente(sistema_combustivel, linhas_combustivel).
subcomponente(sistema_combustivel, regulador_pressao).
subcomponente(sistema_combustivel, carburador).
subcomponente(sistema_combustivel, boia_tanque).
subcomponente(sistema_combustivel, sensor_nivel_combustivel).
subcomponente(sistema_combustivel, unidade_controle).

/* Subcomponentes do sistema de refrigeracão */
subcomponente(sistema_refrigeracao, radiador).
subcomponente(sistema_refrigeracao, bomba_agua).
subcomponente(sistema_refrigeracao, termostato).
subcomponente(sistema_refrigeracao, ventilador).
subcomponente(sistema_refrigeracao, mangueiras).
subcomponente(sistema_refrigeracao, reservatorio_expansao).
subcomponente(sistema_refrigeracao, sensor_temperatura).
subcomponente(sistema_refrigeracao, liquido_arrefecimento).
subcomponente(sistema_refrigeracao, valvula_pressao).
subcomponente(sistema_refrigeracao, tampa_radiador).

/* ==================== FATOS SOBRE SINTOMAS ==================== */

/* Sintomas relacionados ao motor */
sintoma(dificuldade_partida).
sintoma(motor_nao_liga).
sintoma(motor_falhando).
sintoma(ruido_anormal_motor).
sintoma(perda_potencia).
sintoma(consumo_excessivo).
sintoma(fumaca_escapamento).
sintoma(fumaca_branca).
sintoma(fumaca_azul).
sintoma(fumaca_preta).
sintoma(superaquecimento).
sintoma(marcha_lenta_instavel).
sintoma(vibracão_motor).
sintoma(batidas_motor).
sintoma(vazamento_oleo).

/* Sintomas relacionados ao sistema elétrico */
sintoma(farol_fraco).
sintoma(luzes_piscando).
sintoma(bateria_descarregada).
sintoma(sem_energia_eletrica).
sintoma(alternador_com_ruido).
sintoma(partida_lenta).
sintoma(sem_partida).
sintoma(painel_apagado).
sintoma(sem_comunicacao_central).
sintoma(curto_circuito).

/* Sintomas relacionados ao sistema de combustível */
sintoma(sem_combustivel_no_motor).
sintoma(consumo_anormal).
sintoma(cheiro_combustivel).
sintoma(vazamento_combustivel).
sintoma(motor_engasgando).
sintoma(aceleracao_irregular).
sintoma(carro_morre_em_marcha_lenta).
sintoma(falta_pressao_combustivel).
sintoma(hesitacao_aceleracao).
sintoma(dificuldade_ligar_frio).

/* Sintomas relacionados ao sistema de refrigeracão */
sintoma(vazamento_liquido_refrigeracao).
sintoma(aquecimento_rapido).
sintoma(indicador_temperatura_alto).
sintoma(radiador_entupido).
sintoma(ventilador_sempre_ligado).
sintoma(ventilador_nao_liga).
sintoma(vapor_saindo_capô).
sintoma(perda_refrigerante).
sintoma(fervendo).
sintoma(cheiro_queimado).

/* ==================== FATOS SOBRE CAUSAS ==================== */

/* Causas de problemas no motor */
causa(aneis_desgastados).
causa(pistoes_danificados).
causa(junta_cabecote_queimada).
causa(correia_dentada_quebrada).
causa(oleo_contaminado).
causa(oleo_insuficiente).
causa(virabrequim_danificado).
causa(comando_valvulas_desgastado).
causa(valvulas_queimadas).
causa(biela_quebrada).
causa(filtro_ar_sujo).
causa(filtro_oleo_entupido).
causa(bomba_oleo_defeituosa).
causa(folga_valvulas_incorreta).
causa(compressao_baixa).

/* Causas de problemas no sistema elétrico */
causa(bateria_fraca).
causa(bateria_defeituosa).
causa(terminais_bateria_oxidados).
causa(alternador_defeituoso).
causa(regulador_tensao_falha).
causa(motor_partida_com_defeito).
causa(velas_ignicao_gastas).
causa(cabos_vela_rompidos).
causa(bobina_ignicao_falha).
causa(fusivel_queimado).
causa(rele_defeituoso).
causa(curto_sistema_eletrico).
causa(sensor_falha).
causa(chicote_eletrico_rompido).
causa(mau_contato_eletrico).

/* Causas de problemas no sistema de combustível */
causa(bomba_combustivel_fraca).
causa(bomba_combustivel_queimada).
causa(filtro_combustivel_sujo).
causa(injetores_entupidos).
causa(linha_combustivel_obstruida).
causa(linha_combustivel_vazamento).
causa(regulador_pressao_defeituoso).
causa(tanque_contaminado).
causa(boia_tanque_travada).
causa(sensor_nivel_falha).
causa(sensor_fluxo_falha).
causa(carburador_desregulado).
causa(carburador_sujo).
causa(combustivel_contaminado).
causa(combustível_inadequado).

/* Causas de problemas no sistema de refrigeracão */
causa(radiador_entupido).
causa(radiador_vazando).
causa(bomba_agua_quebrada).
causa(termostato_preso_fechado).
causa(termostato_preso_aberto).
causa(ventilador_defeituoso).
causa(sensor_temperatura_falha).
causa(mangueiras_rachadas).
causa(tampa_radiador_defeituosa).
causa(liquido_arrefecimento_contaminado).
causa(nivel_refrigerante_baixo).
causa(bolha_ar_sistema).
causa(valvula_pressao_defeituosa).
causa(correia_ventilador_frouxa).
causa(correia_ventilador_quebrada).

/* ==================== FATOS RELACIONANDO SUBCOMPONENTES E CAUSAS ==================== */
/* Relacão entre subcomponentes e causas */
subcomponente_causa(pistoes, pistoes_danificados).
subcomponente_causa(aneis, aneis_desgastados).
subcomponente_causa(virabrequim, virabrequim_danificado).
subcomponente_causa(bielas, biela_quebrada).
subcomponente_causa(comando_valvulas, comando_valvulas_desgastado).
subcomponente_causa(valvulas, valvulas_queimadas).
subcomponente_causa(junta_cabecote, junta_cabecote_queimada).
subcomponente_causa(correia_dentada, correia_dentada_quebrada).
subcomponente_causa(bomba_oleo, bomba_oleo_defeituosa).
subcomponente_causa(filtro_oleo, filtro_oleo_entupido).
subcomponente_causa(filtro_ar, filtro_ar_sujo).
subcomponente_causa(bateria, bateria_fraca).
subcomponente_causa(bateria, bateria_defeituosa).
subcomponente_causa(terminais_bateria, terminais_bateria_oxidados).
subcomponente_causa(alternador, alternador_defeituoso).
subcomponente_causa(motor_partida, motor_partida_com_defeito).
subcomponente_causa(velas_ignicao, velas_ignicao_gastas).
subcomponente_causa(cabos_vela, cabos_vela_rompidos).
subcomponente_causa(bobina_ignicao, bobina_ignicao_falha).
subcomponente_causa(fusivel, fusivel_queimado).
subcomponente_causa(rele, rele_defeituoso).
subcomponente_causa(sensor, sensor_falha).
subcomponente_causa(chicote_eletrico, chicote_eletrico_rompido).
subcomponente_causa(bomba_combustivel, bomba_combustivel_fraca).
subcomponente_causa(bomba_combustivel, bomba_combustivel_queimada).
subcomponente_causa(filtro_combustivel, filtro_combustivel_sujo).
subcomponente_causa(injetores, injetores_entupidos).
subcomponente_causa(linha_combustivel, linha_combustivel_obstruida).
subcomponente_causa(linha_combustivel, linha_combustivel_vazamento).
subcomponente_causa(regulador_pressao, regulador_pressao_defeituoso).
subcomponente_causa(tanque, tanque_contaminado).
subcomponente_causa(boia_tanque, boia_tanque_travada).
subcomponente_causa(sensor_nivel, sensor_nivel_falha).
subcomponente_causa(sensor_fluxo, sensor_fluxo_falha).
subcomponente_causa(carburador, carburador_desregulado).
subcomponente_causa(carburador, carburador_sujo).
subcomponente_causa(radiador, radiador_entupido).
subcomponente_causa(radiador, radiador_vazando).
subcomponente_causa(bomba_agua, bomba_agua_quebrada).
subcomponente_causa(termostato, termostato_preso_fechado).
subcomponente_causa(ventilador, ventilador_defeituoso).
subcomponente_causa(sensor_temperatura, sensor_temperatura_falha).
subcomponente_causa(mangueiras, mangueiras_rachadas).
subcomponente_causa(tampa_radiador, tampa_radiador_defeituosa).
subcomponente_causa(liquido_arrefecimento, liquido_arrefecimento_contaminado).
subcomponente_causa(nivel_refrigerante, nivel_refrigerante_baixo).
subcomponente_causa(bolha_ar_sistema, bolha_ar_sistema).
subcomponente_causa(valvula_pressao, valvula_pressao_defeituosa).
subcomponente_causa(correia_ventilador, correia_ventilador_frouxa).
subcomponente_causa(correia_ventilador, correia_ventilador_quebrada).


/* ==================== FATOS SOBRE GRAVIDADE ==================== */

/* Níveis de gravidade dos problemas */
gravidade(critico).      /* Requer parada imediata */
gravidade(grave).        /* Requer atencão urgente */
gravidade(moderado).     /* Requer atencão em breve */
gravidade(leve).         /* Pode aguardar manutencão programada */

/* Relacão problema-gravidade */
/* ==================== RELAÇÃO PROBLEMA-GRAVIDADE COMPLETA ==================== */

/* Problemas no motor */
nivel_gravidade(aneis_desgastados, moderado).
nivel_gravidade(pistoes_danificados, grave).
nivel_gravidade(junta_cabecote_queimada, grave).
nivel_gravidade(correia_dentada_quebrada, critico).
nivel_gravidade(oleo_contaminado, moderado).
nivel_gravidade(oleo_insuficiente, grave).
nivel_gravidade(virabrequim_danificado, critico).
nivel_gravidade(comando_valvulas_desgastado, grave).
nivel_gravidade(valvulas_queimadas, grave).
nivel_gravidade(biela_quebrada, critico).
nivel_gravidade(filtro_ar_sujo, leve).
nivel_gravidade(filtro_oleo_entupido, moderado).
nivel_gravidade(bomba_oleo_defeituosa, grave).
nivel_gravidade(folga_valvulas_incorreta, moderado).
nivel_gravidade(compressao_baixa, moderado).

/* Problemas no sistema elétrico */
nivel_gravidade(bateria_fraca, leve).
nivel_gravidade(bateria_defeituosa, moderado).
nivel_gravidade(terminais_bateria_oxidados, leve).
nivel_gravidade(alternador_defeituoso, grave).
nivel_gravidade(regulador_tensao_falha, moderado).
nivel_gravidade(motor_partida_com_defeito, moderado).
nivel_gravidade(velas_ignicao_gastas, moderado).
nivel_gravidade(cabos_vela_rompidos, moderado).
nivel_gravidade(bobina_ignicao_falha, grave).
nivel_gravidade(fusivel_queimado, leve).
nivel_gravidade(rele_defeituoso, moderado).
nivel_gravidade(curto_sistema_eletrico, grave).
nivel_gravidade(sensor_falha, moderado).
nivel_gravidade(chicote_eletrico_rompido, grave).
nivel_gravidade(mau_contato_eletrico, moderado).

/* Problemas no sistema de combustível */
nivel_gravidade(bomba_combustivel_fraca, grave).
nivel_gravidade(bomba_combustivel_queimada, grave).
nivel_gravidade(filtro_combustivel_sujo, moderado).
nivel_gravidade(injetores_entupidos, moderado).
nivel_gravidade(linha_combustivel_obstruida, grave).
nivel_gravidade(linha_combustivel_vazamento, critico).
nivel_gravidade(regulador_pressao_defeituoso, grave).
nivel_gravidade(tanque_contaminado, grave).
nivel_gravidade(boia_tanque_travada, moderado).
nivel_gravidade(sensor_nivel_falha, moderado).
nivel_gravidade(sensor_fluxo_falha, moderado).
nivel_gravidade(carburador_desregulado, moderado).
nivel_gravidade(carburador_sujo, moderado).
nivel_gravidade(combustivel_contaminado, grave).
nivel_gravidade(combustível_inadequado, grave).

/* Problemas no sistema de refrigeração */
nivel_gravidade(radiador_entupido, grave).
nivel_gravidade(radiador_vazando, grave).
nivel_gravidade(bomba_agua_quebrada, grave).
nivel_gravidade(termostato_preso_fechado, critico).
nivel_gravidade(termostato_preso_aberto, moderado).
nivel_gravidade(ventilador_defeituoso, grave).
nivel_gravidade(sensor_temperatura_falha, grave).
nivel_gravidade(mangueiras_rachadas, grave).
nivel_gravidade(tampa_radiador_defeituosa, moderado).
nivel_gravidade(liquido_arrefecimento_contaminado, moderado).
nivel_gravidade(nivel_refrigerante_baixo, moderado).
nivel_gravidade(bolha_ar_sistema, moderado).
nivel_gravidade(valvula_pressao_defeituosa, grave).
nivel_gravidade(correia_ventilador_frouxa, moderado).
nivel_gravidade(correia_ventilador_quebrada, grave).

/* ==================== FATOS SOBRE SOLUcÕES ==================== */

/* Solucões para problemas automotivos */
solucao(trocar_aneis).
solucao(substituir_pistoes).
solucao(trocar_junta_cabecote).
solucao(substituir_correia_dentada).
solucao(trocar_oleo_filtro).
solucao(completar_nivel_oleo).
solucao(retificar_motor).
solucao(substituir_virabrequim).
solucao(ajustar_valvulas).
solucao(substituir_valvulas).
solucao(trocar_bielas).
solucao(limpar_filtro_ar).
solucao(substituir_filtro_ar).
solucao(substituir_filtro_oleo).
solucao(substituir_bomba_oleo).
solucao(carregar_bateria).
solucao(substituir_bateria).
solucao(limpar_terminais_bateria).
solucao(substituir_alternador).
solucao(substituir_regulador_tensao).
solucao(substituir_motor_partida).
solucao(trocar_velas).
solucao(substituir_cabos_vela).
solucao(substituir_bobina).
solucao(substituir_fusivel).
solucao(substituir_rele).
solucao(reparar_chicote_eletrico).
solucao(substituir_bomba_combustivel).
solucao(substituir_filtro_combustivel).
solucao(limpar_injetores).
solucao(substituir_injetores).
solucao(reparar_linha_combustivel).
solucao(substituir_regulador_pressao).
solucao(limpar_tanque).
solucao(substituir_boia_tanque).
solucao(substituir_sensor_nivel).
solucao(regular_carburador).
solucao(limpar_carburador).
solucao(substituir_carburador).
solucao(drenar_combustivel_contaminado).
solucao(limpar_radiador).
solucao(substituir_radiador).
solucao(substituir_bomba_agua).
solucao(substituir_termostato).
solucao(substituir_ventilador).
solucao(substituir_sensor_temperatura).
solucao(substituir_mangueiras).
solucao(substituir_tampa_radiador).
solucao(trocar_liquido_refrigeracao).
solucao(completar_refrigerante).
solucao(purgar_sistema_refrigeracao).
solucao(substituir_valvula_pressao).
solucao(ajustar_correia_ventilador).
solucao(substituir_correia_ventilador).

/* ==================== FATOS SOBRE RELAcÕES ==================== */

/* Relacão entre sintoma e possíveis causas */
causa_sintoma(motor_nao_liga, bateria_fraca).
causa_sintoma(motor_nao_liga, bateria_defeituosa).
causa_sintoma(motor_nao_liga, terminais_bateria_oxidados).
causa_sintoma(motor_nao_liga, motor_partida_com_defeito).
causa_sintoma(motor_nao_liga, correia_dentada_quebrada).
causa_sintoma(motor_nao_liga, bomba_combustivel_queimada).
causa_sintoma(motor_nao_liga, sem_combustivel).
causa_sintoma(motor_nao_liga, velas_ignicao_gastas).
causa_sintoma(motor_nao_liga, cabos_vela_rompidos).
causa_sintoma(motor_nao_liga, bobina_ignicao_falha).

causa_sintoma(dificuldade_partida, bateria_fraca).
causa_sintoma(dificuldade_partida, velas_ignicao_gastas).
causa_sintoma(dificuldade_partida, filtro_combustivel_sujo).
causa_sintoma(dificuldade_partida, bomba_combustivel_fraca).
causa_sintoma(dificuldade_partida, injetores_entupidos).

causa_sintoma(motor_falhando, velas_ignicao_gastas).
causa_sintoma(motor_falhando, cabos_vela_rompidos).
causa_sintoma(motor_falhando, bobina_ignicao_falha).
causa_sintoma(motor_falhando, injetores_entupidos).
causa_sintoma(motor_falhando, filtro_combustivel_sujo).
causa_sintoma(motor_falhando, valvulas_queimadas).

causa_sintoma(fumaca_branca, junta_cabecote_queimada).
causa_sintoma(fumaca_branca, liquido_arrefecimento_vazando_motor).

causa_sintoma(fumaca_azul, aneis_desgastados).
causa_sintoma(fumaca_azul, guias_valvula_gastas).
causa_sintoma(fumaca_azul, vazamento_oleo_motor).

causa_sintoma(fumaca_preta, mistura_rica).
causa_sintoma(fumaca_preta, filtro_ar_sujo).
causa_sintoma(fumaca_preta, injetores_com_problema).
causa_sintoma(fumaca_preta, sensor_oxigenio_defeituoso).

causa_sintoma(superaquecimento, radiador_entupido).
causa_sintoma(superaquecimento, bomba_agua_quebrada).
causa_sintoma(superaquecimento, termostato_preso_fechado).
causa_sintoma(superaquecimento, ventilador_defeituoso).
causa_sintoma(superaquecimento, nivel_refrigerante_baixo).

causa_sintoma(perda_potencia, filtro_ar_sujo).
causa_sintoma(perda_potencia, filtro_combustivel_sujo).
causa_sintoma(perda_potencia, compressao_baixa).
causa_sintoma(perda_potencia, sistema_exaustao_obstruido).
causa_sintoma(perda_potencia, conversor_catalitico_obstruido).

/* Relacão entre causa e solucão */
resolve(aneis_desgastados, trocar_aneis).
resolve(aneis_desgastados, retificar_motor).
resolve(pistoes_danificados, substituir_pistoes).
resolve(pistoes_danificados, retificar_motor).
resolve(junta_cabecote_queimada, trocar_junta_cabecote).
resolve(correia_dentada_quebrada, substituir_correia_dentada).
resolve(oleo_contaminado, trocar_oleo_filtro).
resolve(oleo_insuficiente, completar_nivel_oleo).
resolve(oleo_insuficiente, verificar_vazamentos).
resolve(virabrequim_danificado, substituir_virabrequim).
resolve(virabrequim_danificado, retificar_motor).

resolve(bateria_fraca, carregar_bateria).
resolve(bateria_defeituosa, substituir_bateria).
resolve(terminais_bateria_oxidados, limpar_terminais_bateria).
resolve(alternador_defeituoso, substituir_alternador).
resolve(regulador_tensao_falha, substituir_regulador_tensao).
resolve(motor_partida_com_defeito, substituir_motor_partida).
resolve(velas_ignicao_gastas, trocar_velas).
resolve(cabos_vela_rompidos, substituir_cabos_vela).
resolve(bobina_ignicao_falha, substituir_bobina).
    
resolve(bomba_combustivel_fraca, substituir_bomba_combustivel).
resolve(bomba_combustivel_queimada, substituir_bomba_combustivel).
resolve(filtro_combustivel_sujo, substituir_filtro_combustivel).
resolve(injetores_entupidos, limpar_injetores).
resolve(injetores_entupidos, substituir_injetores).
resolve(linha_combustivel_obstruida, reparar_linha_combustivel).
resolve(linha_combustivel_vazamento, reparar_linha_combustivel).
resolve(regulador_pressao_defeituoso, substituir_regulador_pressao).

resolve(radiador_entupido, limpar_radiador).
resolve(radiador_vazando, substituir_radiador).
resolve(bomba_agua_quebrada, substituir_bomba_agua).
resolve(termostato_preso_fechado, substituir_termostato).
resolve(termostato_preso_aberto, substituir_termostato).
resolve(ventilador_defeituoso, substituir_ventilador).
resolve(nivel_refrigerante_baixo, completar_refrigerante).
resolve(nivel_refrigerante_baixo, verificar_vazamentos).
resolve(bolha_ar_sistema, purgar_sistema_refrigeracao).

/* Relacão entre componente e técnico especialista recomendado */
especialista(motor, mecanico_motor).
especialista(sistema_eletrico, eletricista_automotivo).
especialista(sistema_combustivel, mecanico_injecao).
especialista(sistema_refrigeracao, mecanico_geral).
especialista(sistema_freios, mecanico_freios).
especialista(sistema_transmissao, mecanico_transmissao).
especialista(sistema_suspensao, mecanico_suspensao).
especialista(sistema_exaustao, mecanico_escapamento).
especialista(sistema_direcao, mecanico_direcao).
especialista(pneus, borracheiro).

/* Relacão entre componente e ferramentas necessárias */
ferramenta_necessaria(motor, scanner_automotivo).
ferramenta_necessaria(motor, compressometro).
ferramenta_necessaria(motor, torquimetro).
ferramenta_necessaria(sistema_eletrico, multimetro).
ferramenta_necessaria(sistema_eletrico, testador_bateria).
ferramenta_necessaria(sistema_combustivel, manometro_combustivel).
ferramenta_necessaria(sistema_combustivel, scanner_automotivo).
ferramenta_necessaria(sistema_refrigeracao, termometro).
ferramenta_necessaria(sistema_refrigeracao, teste_pressao_radiador).

/* Tempo médio de reparo em horas (completando todos os que faltavam) */

/* Soluções para o motor */
tempo_reparo(trocar_aneis, 8).
tempo_reparo(substituir_pistoes, 10).
tempo_reparo(trocar_junta_cabecote, 5).
tempo_reparo(substituir_correia_dentada, 3).
tempo_reparo(trocar_oleo_filtro, 0.5).
tempo_reparo(completar_nivel_oleo, 0.1).
tempo_reparo(retificar_motor, 20).
tempo_reparo(substituir_virabrequim, 15).
tempo_reparo(ajustar_valvulas, 1).
tempo_reparo(substituir_valvulas, 3).
tempo_reparo(trocar_bielas, 8).
tempo_reparo(limpar_filtro_ar, 0.5).
tempo_reparo(substituir_filtro_ar, 0.5).
tempo_reparo(substituir_filtro_oleo, 0.5).
tempo_reparo(substituir_bomba_oleo, 2).

/* Soluções para sistema elétrico */
tempo_reparo(carregar_bateria, 1).
tempo_reparo(substituir_bateria, 0.5).
tempo_reparo(limpar_terminais_bateria, 0.3).
tempo_reparo(substituir_alternador, 1.5).
tempo_reparo(substituir_regulador_tensao, 1).
tempo_reparo(substituir_motor_partida, 2).
tempo_reparo(trocar_velas, 1).
tempo_reparo(substituir_cabos_vela, 0.8).
tempo_reparo(substituir_bobina, 1).
tempo_reparo(substituir_fusivel, 0.2).
tempo_reparo(substituir_rele, 0.5).
tempo_reparo(reparar_chicote_eletrico, 2).

/* Soluções para sistema de combustível */
tempo_reparo(substituir_bomba_combustivel, 2).
tempo_reparo(substituir_filtro_combustivel, 0.5).
tempo_reparo(limpar_injetores, 1.5).
tempo_reparo(substituir_injetores, 2).
tempo_reparo(reparar_linha_combustivel, 1.5).
tempo_reparo(substituir_regulador_pressao, 1).
tempo_reparo(limpar_tanque, 3).
tempo_reparo(substituir_boia_tanque, 1).
tempo_reparo(substituir_sensor_nivel, 1).
tempo_reparo(regular_carburador, 1.5).
tempo_reparo(limpar_carburador, 1).
tempo_reparo(substituir_carburador, 2).
tempo_reparo(drenar_combustivel_contaminado, 1).

/* Soluções para sistema de refrigeração */
tempo_reparo(limpar_radiador, 1).
tempo_reparo(substituir_radiador, 2).
tempo_reparo(substituir_bomba_agua, 3).
tempo_reparo(substituir_termostato, 1).
tempo_reparo(substituir_ventilador, 1).
tempo_reparo(substituir_sensor_temperatura, 1).
tempo_reparo(substituir_mangueiras, 1).
tempo_reparo(substituir_tampa_radiador, 0.5).
tempo_reparo(trocar_liquido_refrigeracao, 1).
tempo_reparo(completar_refrigerante, 0.2).
tempo_reparo(purgar_sistema_refrigeracao, 0.5).
tempo_reparo(substituir_valvula_pressao, 1).
tempo_reparo(ajustar_correia_ventilador, 0.5).
tempo_reparo(substituir_correia_ventilador, 1).

/* Soluções para outros sistemas */
tempo_reparo(substituir_modulo_freio, 2.5).
tempo_reparo(atualizacao_software, 0.5).
tempo_reparo(substituir_embreagem_powershift, 6).


/* Custos médios de pecas em unidades monetárias */
custo_peca(aneis, 200).
custo_peca(pistoes, 500).
custo_peca(junta_cabecote, 150).
custo_peca(correia_dentada, 100).
custo_peca(oleo_motor, 50).
custo_peca(filtro_oleo, 20).
custo_peca(virabrequim, 800).
custo_peca(bateria, 300).
custo_peca(alternador, 400).
custo_peca(motor_partida, 350).
custo_peca(velas_ignicao, 80).
custo_peca(cabos_vela, 120).
custo_peca(bobina_ignicao, 180).
custo_peca(bomba_combustivel, 250).
custo_peca(filtro_combustivel, 30).
custo_peca(injetores, 400).
custo_peca(radiador, 300).
custo_peca(bomba_agua, 150).
custo_peca(termostato, 40).
custo_peca(ventilador, 200).
custo_peca(liquido_refrigeracao, 60).


/*1 - Consulta de sintomas */
consultar_sintomas(Sintomas) :-
    findall(S, sintoma(S), Sintomas).

/*2 - Consultar componentes/sistemas */
consultar_componentes(Componentes) :-
    findall(C, componente(C), Componentes).
    
/*3 - Consultar subcomponentes por componente/sistema */
consultar_subcomponentes(Componente, Subcomponentes) :-
    findall(Sub, subcomponente(Componente, Sub), Subcomponentes).
    
/*4 - Consultar causas por sintomas */
consultar_causas_por_sintoma(Sintoma, Causas) :-
    findall(C, causa_sintoma(Sintoma, C), Causas).

/*5 - Retornar custos por causa */
consultar_custos_por_causa(Causa, Custo) :-
    custo_peca(Causa, Custo).

/*6 - Retornar tempo de reparo por causa */
consultar_tempo_reparo_por_causa(Causa, Tempo) :-
    tempo_reparo(Causa, Tempo).

/*7 - Retornar solucão por causa */
consultar_solucao_por_causa(Causa, Solucao) :-
    resolve(Causa, Solucao).

/*8 - Retornar especialista por componente */
consultar_especialista_por_componente(Componente, Especialista) :-
    especialista(Componente, Especialista).

/*9 - Retornar ferramentas necessárias por componente */
consultar_ferramentas_por_componente(Componente, Ferramentas) :-
    findall(F, ferramenta_necessaria(Componente, F), Ferramentas).

/*10 - Retornar gravidade por causa */
consultar_gravidade_por_causa(Causa, Gravidade) :-
    nivel_gravidade(Causa, Gravidade).

/*11 - Retornar gravidade por sintoma */
consultar_gravidade_por_sintoma(Sintoma, Gravidade) :-
    causa_sintoma(Sintoma, Causa),
    nivel_gravidade(Causa, Gravidade).

/* 12 - Passar uma lista de sintomas e retornar custos e tempo de reparo */
consultar_custos_tempo_por_sintomas(Sintomas, Custos, Tempos) :-
    findall((Custo, Tempo), (member(S, Sintomas), causa_sintoma(S, Causa), custo_peca(Causa, Custo), tempo_reparo(Causa, Tempo)), Resultados),
    findall(Custo, member((Custo, _), Resultados), Custos),
    findall(Tempo, member((_, Tempo), Resultados), Tempos).


% Versão alternativa com acumulador e corte
consultar_combinacoes_servicos_por_tempo2(TempoMaximo, Combinacoes) :-
    findall(Servico, resolve(_, Servico), TodosServicos),
    list_to_set(TodosServicos, ServicosUnicos),
    filtrar_por_tempo(ServicosUnicos, TempoMaximo, [], Combinacoes), !.

filtrar_por_tempo([], _, Acum, Acum).
filtrar_por_tempo([Servico|Resto], TempoMaximo, Acum, Combinacoes) :-
    tempo_reparo(Servico, Tempo), 
    Tempo =< TempoMaximo ->
        filtrar_por_tempo(Resto, TempoMaximo, [Servico|Acum], Combinacoes)
    ;
        filtrar_por_tempo(Resto, TempoMaximo, Acum, Combinacoes).
/* 14 - Retornar combinacões de servicos possiveis por custo */
consultar_combinacoes_servicos_por_custo(CustoMaximo, Combinacoes) :-
    findall((Causa, Custo), (resolve(Causa, Servico), custo_peca(Causa, Custo), Custo =< CustoMaximo), Resultados),
    findall(Servico, member((_, Servico), Resultados), Combinacoes).

/* 15 - Possiveis causas por sintoma excluindo as já verificadas */
consultar_causas_excluindo_verificadas(Sintoma, Verificadas, CausasPossiveis) :-
    findall(Causa, (causa_sintoma(Sintoma, Causa),     % Filtrar reparos que cabem individualmente no tempo
    \+ member(Causa, Verificadas)), CausasPossiveis).

/* 16 - Recomendar profissional por sintoma */
listar_profissional_por_sintoma(Sintoma, Profissionais) :-
    findall(Profissional, 
            (causa_sintoma(Sintoma, Causa),
             subcomponente_causa(Subcomponente, Causa),
             subcomponente(Componente, Subcomponente),
             especialista(Componente, Profissional)),
            ProfissionaisSemRepeticao),
    sort(ProfissionaisSemRepeticao, Profissionais).

/* 18 - Ordenar por gravidade uma lista de problemas*/
ordenar_problemas_por_gravidade(Problemas, Ordenados) :-
    mapear_gravidades(Problemas, ListaComGravidades),
    quicksort(ListaComGravidades, ListaOrdenada),
    extrair_problemas(ListaOrdenada, Ordenados).

/* Mapeia cada problema para uma tupla (Gravidade, Problema) recursivamente */
mapear_gravidades([], []).
mapear_gravidades([P|Ps], [(G,P)|Resto]) :-
    nivel_gravidade(P, G),
    mapear_gravidades(Ps, Resto).

/* Algoritmo quicksort para ordenar por gravidade */
quicksort([], []).
quicksort([(G,P)|Cauda], Ordenado) :-
    partir(G, Cauda, Menores, Maiores),
    quicksort(Menores, MenoresOrd),
    quicksort(Maiores, MaioresOrd),
    append(MenoresOrd, [(G,P)|MaioresOrd], Ordenado).

/* Particiona a lista em elementos menores e maiores que o pivô */
partir(_, [], [], []).
partir(G, [(G1,P1)|Cauda], [(G1,P1)|Menores], Maiores) :-
    gravidade_maior_ou_igual(G1, G),
    partir(G, Cauda, Menores, Maiores).
partir(G, [(G1,P1)|Cauda], Menores, [(G1,P1)|Maiores]) :-
    gravidade_menor(G1, G),
    partir(G, Cauda, Menores, Maiores).

/* Comparacão de gravidades (critico > grave > moderado > leve) */
gravidade_maior_ou_igual(G1, G2) :-
    valor_gravidade(G1, V1),
    valor_gravidade(G2, V2),
    V1 >= V2.

gravidade_menor(G1, G2) :-
    valor_gravidade(G1, V1),
    valor_gravidade(G2, V2),
    V1 < V2.

/* Valor numérico para cada nível de gravidade */
valor_gravidade(critico, 4).
valor_gravidade(grave, 3).
valor_gravidade(moderado, 2).
valor_gravidade(leve, 1).

/* Extrai apenas os problemas da lista ordenada de tuplas */
extrair_problemas([], []).
extrair_problemas([(_,P)|Ps], [P|Resto]) :-
    extrair_problemas(Ps, Resto).

/* 19 - Calcular custo medio de um reparo por veiculo */
% Caso base: lista vazia, custo total e contador zerados
calcular_custo_medio([], 0, 0, 0).

% Passo recursivo: acumula custo e conta componentes
calcular_custo_medio([Componente | Resto], CustoTotal, Contador, CustoMedio) :-
    custo_peca(Componente, Custo),
    calcular_custo_medio(Resto, CustoAcumulado, ContadorAcumulado, _),
    CustoTotal is CustoAcumulado + Custo,
    Contador is ContadorAcumulado + 1,
    (Contador > 0 -> CustoMedio is CustoTotal / Contador ; CustoMedio = 0).

% Predicado principal (wrapper)
calcular_custo_medio_reparo(Veiculo, CustoMedio) :-
    carro(Veiculo, _, _, _, Componentes),
    calcular_custo_medio(Componentes, CustoTotal, Contador, CustoMedio).

/* 20 - Passar uma lista de causa, hora e retornas quais fazer primeiro para ter mais lucro*/
/* 20 - Versão totalmente corrigida do prioriza_reparo */
prioriza_reparo(Problemas, HorasTrabalhadas, MelhoresReparos) :-
    % Obter todas as soluções viáveis com tempo e custo
    findall(sol(Solucao, Tempo, Custo),
        (member(Problema, Problemas),
         resolve(Problema, Solucao),
         tempo_reparo(Solucao, Tempo),
         Tempo =< HorasTrabalhadas,
         (custo_peca(Problema, Custo) ; (custo_peca(Solucao, Custo) ; Custo = 0)),
         Tempo > 0
        ),
        Solucoes),
    
    % Converter para estrutura que o knapsack pode processar
    maplist(sol_to_knapsack, Solucoes, Itens),

    % Resolver o problema da mochila
    knapsack(Itens, HorasTrabalhadas, Selecionados),
    write('Soluções: '), write(Selecionados), nl,

    % Extrair apenas os nomes das soluções
    maplist(selecionado_to_solucao, Selecionados, MelhoresReparos).

% Conversor de solução para formato knapsack
sol_to_knapsack(sol(S, V, P), item(S, V, P)).

% Conversor de item selecionado para nome de solução
selecionado_to_solucao(item(S, _, _), S).

% Implementação simplificada do knapsack
knapsack(Itens, Capacidade, Selecionados) :-
    % Ordenar por valor/peso (maior primeiro)
    predsort(compare_itens, Itens, ItensOrdenados),
    write(ItensOrdenados), nl,
    
    
    % Selecionar gananciosamente
    selecionar_itens(ItensOrdenados, Capacidade, [], Selecionados).

compare_itens(>, item(_, V1, P1), item(_, V2, P2)) :-
    (V1/P1) > (V2/P2).
compare_itens(<, _, _).

selecionar_itens([], _, Acum, Acum).
selecionar_itens([item(N, V, P)|Resto], Capacidade, Acum, Selecionados) :-
    (P =< Capacidade ->
        NovaCapacidade is Capacidade - P,
        selecionar_itens(Resto, NovaCapacidade, [item(N, V, P)|Acum], Selecionados)
    ;
        selecionar_itens(Resto, Capacidade, Acum, Selecionados)
    ).