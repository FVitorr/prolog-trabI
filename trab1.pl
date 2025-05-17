/* ======================================================
   SISTEMA ESPECIALISTA: DIAGNÓSTICO DE PROBLEMAS AUTOMOTIVOS
   ======================================================
   
   Este sistema especialista auxilia na identificação e resolução
   de problemas comuns em veículos automotores.
*/

/* ==================== FATOS SOBRE COMPONENTES ==================== */

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

/* Subcomponentes do sistema de refrigeração */
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
sintoma(fumaça_escapamento).
sintoma(fumaça_branca).
sintoma(fumaça_azul).
sintoma(fumaça_preta).
sintoma(superaquecimento).
sintoma(marcha_lenta_instavel).
sintoma(vibração_motor).
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

/* Sintomas relacionados ao sistema de refrigeração */
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

/* Causas de problemas no sistema de refrigeração */
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

/* ==================== FATOS SOBRE GRAVIDADE ==================== */

/* Níveis de gravidade dos problemas */
gravidade(critico).      /* Requer parada imediata */
gravidade(grave).        /* Requer atenção urgente */
gravidade(moderado).     /* Requer atenção em breve */
gravidade(leve).         /* Pode aguardar manutenção programada */

/* Relação problema-gravidade */
nivel_gravidade(aneis_desgastados, moderado).
nivel_gravidade(pistoes_danificados, grave).
nivel_gravidade(junta_cabecote_queimada, grave).
nivel_gravidade(correia_dentada_quebrada, critico).
nivel_gravidade(oleo_contaminado, moderado).
nivel_gravidade(oleo_insuficiente, grave).
nivel_gravidade(virabrequim_danificado, critico).
nivel_gravidade(biela_quebrada, critico).
nivel_gravidade(compressao_baixa, moderado).
nivel_gravidade(bateria_fraca, leve).
nivel_gravidade(bateria_defeituosa, moderado).
nivel_gravidade(terminais_bateria_oxidados, leve).
nivel_gravidade(alternador_defeituoso, grave).
nivel_gravidade(motor_partida_com_defeito, moderado).
nivel_gravidade(bomba_combustivel_queimada, grave).
nivel_gravidade(filtro_combustivel_sujo, moderado).
nivel_gravidade(injetores_entupidos, moderado).
nivel_gravidade(linha_combustivel_vazamento, grave).
nivel_gravidade(radiador_vazando, grave).
nivel_gravidade(bomba_agua_quebrada, grave).
nivel_gravidade(termostato_preso_fechado, grave).
nivel_gravidade(nivel_refrigerante_baixo, moderado).
nivel_gravidade(superaquecimento, critico).
nivel_gravidade(correia_ventilador_quebrada, grave).

/* ==================== FATOS SOBRE SOLUÇÕES ==================== */

/* Soluções para problemas automotivos */
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

/* ==================== FATOS SOBRE RELAÇÕES ==================== */

/* Relação entre sintoma e possíveis causas */
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

causa_sintoma(fumaça_branca, junta_cabecote_queimada).
causa_sintoma(fumaça_branca, liquido_arrefecimento_vazando_motor).

causa_sintoma(fumaça_azul, aneis_desgastados).
causa_sintoma(fumaça_azul, guias_valvula_gastas).
causa_sintoma(fumaça_azul, vazamento_oleo_motor).

causa_sintoma(fumaça_preta, mistura_rica).
causa_sintoma(fumaça_preta, filtro_ar_sujo).
causa_sintoma(fumaça_preta, injetores_com_problema).
causa_sintoma(fumaça_preta, sensor_oxigenio_defeituoso).

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

/* Relação entre causa e solução */
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

/* Relação entre componente e técnico especialista recomendado */
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

/* Relação entre componente e ferramentas necessárias */
ferramenta_necessaria(motor, scanner_automotivo).
ferramenta_necessaria(motor, compressometro).
ferramenta_necessaria(motor, torquimetro).
ferramenta_necessaria(sistema_eletrico, multimetro).
ferramenta_necessaria(sistema_eletrico, testador_bateria).
ferramenta_necessaria(sistema_combustivel, manometro_combustivel).
ferramenta_necessaria(sistema_combustivel, scanner_automotivo).
ferramenta_necessaria(sistema_refrigeracao, termometro).
ferramenta_necessaria(sistema_refrigeracao, teste_pressao_radiador).

/* Tempo médio de reparo em horas */
tempo_reparo(trocar_aneis, 8).
tempo_reparo(substituir_pistoes, 10).
tempo_reparo(trocar_junta_cabecote, 5).
tempo_reparo(substituir_correia_dentada, 3).
tempo_reparo(trocar_oleo_filtro, 0.5).
tempo_reparo(completar_nivel_oleo, 0.1).
tempo_reparo(retificar_motor, 20).
tempo_reparo(substituir_virabrequim, 15).
tempo_reparo(carregar_bateria, 1).
tempo_reparo(substituir_bateria, 0.5).
tempo_reparo(limpar_terminais_bateria, 0.3).
tempo_reparo(substituir_alternador, 1.5).
tempo_reparo(substituir_motor_partida, 2).
tempo_reparo(trocar_velas, 1).
tempo_reparo(substituir_cabos_vela, 0.8).
tempo_reparo(substituir_bomba_combustivel, 2).
tempo_reparo(substituir_filtro_combustivel, 0.5).
tempo_reparo(limpar_injetores, 1.5).
tempo_reparo(substituir_injetores, 2).
tempo_reparo(substituir_radiador, 2).
tempo_reparo(substituir_bomba_agua, 3).
tempo_reparo(substituir_termostato, 1).
tempo_reparo(substituir_ventilador, 1).
tempo_reparo(trocar_liquido_refrigeracao, 1).
tempo_reparo(completar_refrigerante, 0.2).
tempo_reparo(purgar_sistema_refrigeracao, 0.5).

/* Custos médios de peças em unidades monetárias */
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