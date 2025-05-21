/*Principais Sistemas de um carro*/
sistema(sistema_transmissao).
sistema(sistema_direcao).
sistema(sistema_arrefecimento).
sistema(sistema_eletrico).

componente(embreagem).
componente(cambio).
componente(eixo_transmissao).
componente(diferencial).
componente(semieixo).

componente(volante).
componente(coluna_direcao).
componente(caixa_direcao).
componente(barra_direcao).
componente(bomba_hidraulica).

componente(radiador).
componente(ventoinha).
componente(bomba_agua).
componente(valvula_termostatica).
componente(reservatorio_expansao).

componente(bateria).
componente(alternador).
componente(motor_partida).
componente(fusivel).
componente(farois).

componente_sistema(embreagem, sistema_transmissao).
componente_sistema(cambio, sistema_transmissao).
componente_sistema(eixo_transmissao, sistema_transmissao).
componente_sistema(diferencial, sistema_transmissao).
componente_sistema(semieixo, sistema_transmissao).

componente_sistema(volante, sistema_direcao).
componente_sistema(coluna_direcao, sistema_direcao).
componente_sistema(caixa_direcao, sistema_direcao).
componente_sistema(barra_direcao, sistema_direcao).
componente_sistema(bomba_hidraulica, sistema_direcao).

componente_sistema(radiador, sistema_arrefecimento).
componente_sistema(ventoinha, sistema_arrefecimento).
componente_sistema(bomba_agua, sistema_arrefecimento).
componente_sistema(valvula_termostatica, sistema_arrefecimento).
componente_sistema(reservatorio_expansao, sistema_arrefecimento).

componente_sistema(bateria, sistema_eletrico).
componente_sistema(alternador, sistema_eletrico).
componente_sistema(motor_partida, sistema_eletrico).
componente_sistema(fusivel, sistema_eletrico).
componente_sistema(farois, sistema_eletrico).

% problema(nome_do_problema, [sintomas], [componentes]).
% Problemas do Sistema de Transmissão
problema(embreagem_desgastada, [dificuldade_engatar_marcha, trepidacao_embreagem, perda_potencia], [embreagem], grave).
problema(cambio_com_defeito, [dificuldade_engatar_marcha, ruido_transmissao], [cambio], grave).
problema(diferencial_desgastado, [barulho_transmissao, carro_puxando_lado], [diferencial], moderado).
problema(semieixo_danificado, [vibracao_em_movimento, estalos_curvas], [semieixo], grave).
problema(eixo_transmissao_desequilibrado, [vibracao_em_alta_velocidade, ruido_transmissao], [eixo_transmissao], moderado).

% Problemas do Sistema de Direção
problema(bomba_hidraulica_com_defeito, [direcao_dura, ruido_direcao, vazamento_fluido_direcao], [bomba_hidraulica], grave).
problema(caixa_direcao_folgada, [folga_direcao, dificuldade_controlar_direcao], [caixa_direcao], grave).
problema(barra_direcao_torta, [carro_puxando_lado, desgaste_pneus_irregular], [barra_direcao], moderado).
problema(coluna_direcao_com_folga, [volante_folgado, barulho_ao_girar], [coluna_direcao], leve).
problema(volante_desalinhado, [volante_torto, dificuldade_direcionar], [volante], leve).

% Problemas do Sistema de Arrefecimento
problema(radiador_obstruido, [motor_esquentando, vazamento_liquido_arrefecimento], [radiador], grave).
problema(bomba_agua_quebrada, [superaquecimento_motor, barulho_bomba_agua], [bomba_agua], critico).
problema(valvula_termostatica_travada, [motor_nao_aquece, superaquecimento_motor], [valvula_termostatica], grave).
problema(ventoinha_nao_funciona, [motor_esquentando, ventoinha_nao_liga], [ventoinha], grave).
problema(reservatorio_rasgado, [vazamento_liquido_arrefecimento, nivel_liquido_baixo], [reservatorio_expansao], moderado).

% Problemas do Sistema Elétrico
problema(bateria_descarregando, [dificuldade_partida, luzes_fraquejando], [bateria], moderado).
problema(alternador_sem_carga, [luz_bateria_painel, descarga_bateria], [alternador], grave).
problema(fusivel_queimado, [componente_nao_funciona, luz_painel_apagada], [fusivel], leve).
problema(motor_partida_queimado, [carro_nao_liga, clique_ao_girar_chave], [motor_partida], critico).
problema(farois_queimados, [luz_fraca, farois_nao_acendem], [farois], leve).

% Sintomas relacionados à transmissão
sintoma(dificuldade_engatar_marcha).
sintoma(trepidacao_embreagem).
sintoma(perda_potencia).
sintoma(ruido_transmissao).
sintoma(barulho_transmissao).
sintoma(carro_puxando_lado).
sintoma(vibracao_em_movimento).
sintoma(estalos_curvas).
sintoma(vibracao_em_alta_velocidade).

% Sintomas relacionados à direção
sintoma(direcao_dura).
sintoma(ruido_direcao).
sintoma(vazamento_fluido_direcao).
sintoma(folga_direcao).
sintoma(dificuldade_controlar_direcao).
sintoma(desenho_direcao_puxando_lado).
sintoma(danos_pneu_direcao).
sintoma(volante_folgado).
sintoma(barulho_ao_girar).
sintoma(volante_torto).
sintoma(dificuldade_direcionar).

% Sintomas relacionados ao arrefecimento
sintoma(motor_esquentando).
sintoma(vazamento_liquido_arrefecimento).
sintoma(superaquecimento_motor).
sintoma(barulho_bomba_agua).
sintoma(motor_nao_aquece).
sintoma(ventoinha_nao_liga).
sintoma(nivel_liquido_baixo).

% Sintomas relacionados ao sistema elétrico
sintoma(dificuldade_partida).
sintoma(luzes_fraquejando).
sintoma(luz_bateria_painel).
sintoma(descarga_bateria).
sintoma(componente_nao_funciona).
sintoma(luz_painel_apagada).
sintoma(carro_nao_liga).
sintoma(clique_ao_girar_chave).
sintoma(luz_fraca).
sintoma(farois_nao_acendem).


gravidade(critico, 4).      /* Requer parada imediata */
gravidade(grave, 3).        /* Requer atencão urgente */
gravidade(moderado, 2).     /* Requer atencão em breve */
gravidade(leve, 1).         /* Pode aguardar manutencão programada */

%reparo(nome_do_problema, tempo_estimado, custo_estimado).
% Sistema de Transmissão
reparo(embreagem_desgastada, 6, 1500).
reparo(cambio_com_defeito, 8, 2500).
reparo(diferencial_desgastado, 5, 1800).
reparo(semieixo_danificado, 3, 800).
reparo(eixo_transmissao_desequilibrado, 2, 600).

% Sistema de Direção
reparo(bomba_hidraulica_com_defeito, 4, 1200).
reparo(caixa_direcao_folgada, 6, 1600).
reparo(barra_direcao_torta, 2, 500).
reparo(coluna_direcao_com_folga, 3, 700).
reparo(volante_desalinhado, 1, 150).

% Sistema de Arrefecimento
reparo(radiador_obstruido, 3, 900).
reparo(bomba_agua_quebrada, 4, 1000).
reparo(valvula_termostatica_travada, 2, 400).
reparo(ventoinha_nao_funciona, 2, 500).
reparo(reservatorio_rasgado, 1, 250).

% Sistema Elétrico
reparo(bateria_descarregando, 1, 400).
reparo(alternador_sem_carga, 3, 900).
reparo(fusivel_queimado, 0.5, 50).
reparo(motor_partida_queimado, 3, 800).
reparo(farois_queimados, 1, 200).


% 1. Regra para Diagnosticar Problemas Baseado em Sintomas
diagnostico(Sintomas, Problema) :-
    problema(Problema, SintomasProblema, _, _),
    
    subset(SintomasProblema, Sintomas).

% 2. Regra para Sugerir Reparo Dado um Problema
solucao(Problema, Tempo, Custo) :-
    reparo(Problema, Tempo, Custo).

% 3. Regra para Verificar Gravidade do Problema
nivel_criticidade(Problema, Nivel) :-
    problema(Problema, _, _, Nivel).

% 4. Regra para Obter os Componentes Atingidos por um Problema
componentes_afetados(Problema, Componentes) :-
    problema(Problema, _, Componentes, _).

% 5. Regra para Descobrir o Sistema de um Problema
sistema_problema(Problema, Sistema) :-
    problema(Problema, _, [Componente | _], _),
    componente_sistema(Componente, Sistema).

todos_os_problemas(Problemas) :-
    findall(Problema, problema(Problema, _, _, _), Problemas).

% 6. Regra para Listar Problemas Possíveis com Um Sintoma
% Regra principal: retorna lista de problemas que contêm o Sintoma
possiveis_problemas(Sintoma, Problemas) :-
    todos_os_problemas(Todos),
    filtra_por_sintoma(Sintoma, Todos, Problemas), !.

filtra_por_sintoma(_, [], []).
filtra_por_sintoma(Sintoma, [P|R], [P|ProblemasResto]) :-
    problema(P, Sintomas, _, _),
    member(Sintoma, Sintomas),
    filtra_por_sintoma(Sintoma, R, ProblemasResto).
filtra_por_sintoma(Sintoma, [P|R], ProblemasResto) :-
    problema(P, Sintomas, _, _),
    \+ member(Sintoma, Sintomas),
    filtra_por_sintoma(Sintoma, R, ProblemasResto).


% 7. Regra para Listar Todos os Sintomas de um Sistema
sintomas_sistema(Sistema, SintomasUnicos) :-
    findall(Sintoma,
        (componente_sistema(Componente, Sistema),
         problema(_, Sintomas, [Componente], _),
         member(Sintoma, Sintomas)),
        SintomasDuplicados),
    sort(SintomasDuplicados, SintomasUnicos).

% Dada uma lista de problemas, retorne os problemas ordenador por gravidade - recusivo
% ?- ordenar_problemas_por_gravidade([fusivel_queimado, bomba_agua_quebrada, caixa_direcao_folgada, bateria_descarregando], L).
% Retorna o peso de um problema com base na sua gravidade
peso_problema(Problema, Peso) :-
    problema(Problema, _, _, Gravidade),
    gravidade(Gravidade, Peso).

% Caso base: lista vazia
ordenar_problemas_por_gravidade([], []).

% Caso recursivo: ordena o restante e insere o elemento atual na posição correta
ordenar_problemas_por_gravidade([H|T], Ordenada) :-
    ordenar_problemas_por_gravidade(T, ParcialOrdenada),
    inserir_ordenado_por_gravidade(H, ParcialOrdenada, Ordenada).

% Insere um problema na lista ordenada com base na gravidade (decrescente)
inserir_ordenado_por_gravidade(Problema, [], [Problema]).
inserir_ordenado_por_gravidade(Problema, [H|T], [Problema, H|T]) :-
    peso_problema(Problema, Peso1),
    peso_problema(H, Peso2),
    Peso1 >= Peso2.
inserir_ordenado_por_gravidade(Problema, [H|T], [H|Resto]) :-
    peso_problema(Problema, Peso1),
    peso_problema(H, Peso2),
    Peso1 < Peso2,
    inserir_ordenado_por_gravidade(Problema, T, Resto).

% 9- Algoritimo da mochila: dada uma lista de problemas, retorne os problemas que podem ser resolvidos a tempo , com maior custo
% ?- problemas_resolvidos_a_tempo([fusivel_queimado, bomba_agua_quebrada, caixa_direcao_folgada, bateria_descarregando], TempoMaximo, ProblemasResolvidos).
% Reuso da base de dados: problema(nome, _, _, gravidade) e reparo(nome, tempo, custo)

% Solução: problemas_resolvidos_a_tempo(+Problemas, +TempoMaximo, -MelhorConjunto)
problemas_resolvidos_a_tempo(Problemas, TempoMaximo, MelhorConjunto) :-
    findall((P, Tempo, Custo), 
        (member(P, Problemas), reparo(P, Tempo, Custo)), 
        ListaProblemas),
    subconjuntos(ListaProblemas, TodosSubconjuntos),
    inclui_apenas_dentro_do_tempo(TodosSubconjuntos, TempoMaximo, SubconjuntosValidos),
    melhor_custo(SubconjuntosValidos, _, MelhorConjunto).

% Gera todos os subconjuntos possíveis
subconjuntos([], [[]]).
subconjuntos([H|T], Subconjuntos) :-
    subconjuntos(T, SubT),
    adiciona_cada(H, SubT, ComH),
    append(SubT, ComH, Subconjuntos).

adiciona_cada(_, [], []).
adiciona_cada(X, [L|Ls], [[X|L]|R]) :-
    adiciona_cada(X, Ls, R).

% Filtra apenas subconjuntos cujo tempo total seja <= TempoMaximo
inclui_apenas_dentro_do_tempo([], _, []).
inclui_apenas_dentro_do_tempo([Sub|Resto], Max, [Sub|Filtrados]) :-
    soma_tempo(Sub, SomaTempo),
    SomaTempo =< Max,
    inclui_apenas_dentro_do_tempo(Resto, Max, Filtrados).
inclui_apenas_dentro_do_tempo([Sub|Resto], Max, Filtrados) :-
    soma_tempo(Sub, SomaTempo),
    SomaTempo > Max,
    inclui_apenas_dentro_do_tempo(Resto, Max, Filtrados).

% Soma o tempo total de um conjunto
soma_tempo([], 0).
soma_tempo([(_, T, _)|R], Total) :-
    soma_tempo(R, Parcial),
    Total is Parcial + T.

% Soma o custo total de um conjunto
soma_custo([], 0).
soma_custo([(_, _, C)|R], Total) :-
    soma_custo(R, Parcial),
    Total is Parcial + C.

% Encontra o subconjunto com maior custo
melhor_custo([Sub], Custo, Sub) :-
    soma_custo(Sub, Custo).
melhor_custo([Sub|Resto], MelhorCusto, MelhorSub) :-
    melhor_custo(Resto, C1, S1),
    soma_custo(Sub, C2),
    (C2 >= C1 ->
        (MelhorCusto = C2, MelhorSub = Sub)
    ;
        (MelhorCusto = C1, MelhorSub = S1)
    ).

% 10 - todos os sistemas
todos_os_sistemas(Sistemas) :-
    findall(Sistema, sistema(Sistema), Sistemas).

% 11 - todos os componentes
todos_os_componentes(Componentes) :-
    findall(Componente, componente(Componente), Componentes).

% 12 - todos os componentes de um sistema
todos_os_problemas(Problemas) :-
    findall(Problema, problema(Problema, _, _, _), Problemas).