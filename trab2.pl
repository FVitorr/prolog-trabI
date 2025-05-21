%%% TIPOS DE CARTA
tipo_carta(terreno).
tipo_carta(criatura).
tipo_carta(magia_instantanea).

%%% SUBTIPOS DE MAGIA
subtipo_magia(destruir).
subtipo_magia(curar).

%%% HABILIDADES
habilidade(voar).
habilidade(alcance).
habilidade(atropelar).
habilidade(ameacar).
habilidade(duplo_ataque).
habilidade(defensor).

%%% FATOS SOBRE CRIATURAS
%criatura('Dragão Branco', poder, defese, [habilidade]).
criatura('Dragão Branco', 5, 5, [voar]).
criatura('Goblin Agressivo', 2, 1, [ameacar]).
criatura('Elefante Gigante', 4, 4, [atropelar]).
criatura('Arqueiro Élfico', 1, 3, [alcance]).
criatura('Cavaleiro da Aurora', 3, 3, [duplo_ataque]).
criatura('Tartaruga Defensora', 1, 6, []).
criatura('Hidra de Cabeças Múltiplas', 6, 6, []).
criatura('Espreitador Noturno', 2, 2, [voar]).
criatura('Lobo da Estepe', 3, 2, []).
criatura('Anjo Guardião', 4, 4, [voar]).
criatura('Zumbi Voraz', 2, 2, []).
criatura('Grifo Alado', 3, 3, [voar]).
criatura('Elemental de Terra', 5, 3, [atropelar]).
criatura('Fada Ilusionista', 1, 1, [voar]).
criatura('Troll Regenerador', 3, 3, []).
criatura('Demônio Sanguinário', 6, 6, [ameacar]).
criatura('Serpente das Profundezas', 5, 5, []).
criatura('Gárgula de Pedra', 3, 3, [voar]).
criatura('Cavaleiro Negro', 2, 2, [duplo_ataque]).
criatura('Gigante das Nuvens', 7, 7, [voar, atropelar]).
criatura('Goblin Defensor', 1, 2, [defensor]).

%%% FATOS SOBRE MAGIAS
magia('Raios Solares', destruir, 3). % Destrói criatura com custo 3 ou menos
magia('Cura Divina', curar, 5). % Cura 5 pontos de vida
magia('Desintegrar', destruir, 6). % Destrói qualquer criatura
magia('Renascimento', curar, 3). % Cura 3 pontos de vida
magia('Seta Flamejante', destruir, 1). % Destrói criatura com custo 1 ou menos

%%% FATOS SOBRE TERRENOS
terreno('Terreno').

%%% ESTADO DO JOGO (exemplo)
% jogador(nome, vida, numero de terrenos, mao, campo)
:- dynamic jogador/5.
jogador(eu, 15, 3, [], ['Tartaruga Defensora', 'Arqueiro Élfico', 'Dragão Branco', 'Goblin Defensor']).
jogador(oponente1, 12, 2, [], ['Demônio Sanguinário', 'Goblin Agressivo', 'Espreitador Noturno', 'Cavaleiro Negro']).

%%% REGRAS DE DECISÃO

% Regra 1: Trocar mão inicial se tiver menos de 2 ou mais de 4 terrenos na mão
trocar_mao_inicial(Jogador) :-
    jogador(Jogador, _, _, Mao, _),
    findall(T, (member(T, Mao), terreno(T)), TerrenosMao),
    length(TerrenosMao, Quantidade),
    (Quantidade < 2; Quantidade > 4),
    write('Troque a mão inicial: quantidade de terrenos inadequada ('), 
    write(Quantidade), write(').'), nl.

% Regra 2: Manter mão inicial se tiver entre 2 e 4 terrenos na mão
manter_mao_inicial(Jogador) :-
    jogador(Jogador, _, _, Mao, _),
    findall(T, (member(T, Mao), terreno(T)), TerrenosMao),
    length(TerrenosMao, Quantidade),
    Quantidade >= 2, Quantidade =< 4,
    write('Mantenha a mão inicial: quantidade de terrenos adequada ('), 
    write(Quantidade), write(').'), nl.

% Regra 3: Atacar oponente com menor vida se possível
oponente_prioritario(Oponente) :-
    jogador(Oponente, Vida, _, _, _),
    \+ Oponente = eu,
    findall(V, (jogador(O, V, _, _, _), O \= eu, O \= Oponente), OutrasVidas),
    min_list([Vida|OutrasVidas], Vida),
    write('Priorize atacar '), write(Oponente), 
    write(' (vida mais baixa: '), write(Vida), write(')'), nl.

% Regra 4: Usar criatura com voar para atacar se oponente não tem criaturas com voar ou alcance
usar_voar_para_atacar(Criatura) :-
    jogador(eu, _, _, _, MinhasCriaturas),
    member(Criatura, MinhasCriaturas),
    criatura(Criatura, _, _, Habilidades),
    member(voar, Habilidades),
    jogador(Oponente, _, _, _, CriaturasOponente),
    Oponente \= eu,
    \+ (member(C, CriaturasOponente), 
        (criatura(C, _, _, H), (member(voar, H); member(alcance, H)))),
    write('Use '), write(Criatura), 
    write(' para atacar '), write(Oponente),
    write(' (voar sem bloqueadores).'), nl.

% Regra 5: Usar magia de destruir em criatura mais forte do oponente
usar_magia_destruir_em(Magia, Alvo) :-
    jogador(eu, _, _, MinhaMao, _),
    member(Magia, MinhaMao),
    magia(Magia, destruir, CustoMax),
    jogador(Oponente, _, _, _, CriaturasOponente),
    Oponente \= eu,
    member(Alvo, CriaturasOponente),
    criatura(Alvo, Poder, _, _),
    Poder =< CustoMax,
    format('Use a magia ~w para destruir ~w (criatura de ~w) do oponente ~w.~n', 
           [Magia, Alvo, Poder, Oponente]).

% Regra 6: Curar-se quando vida estiver baixa
usar_magia_curar(Magia) :-
    jogador(eu, Vida, _, MinhaMao, _),
    Vida < 10,
    member(Magia, MinhaMao),
    magia(Magia, curar, _),
    write('Use '), write(Magia), write(' para curar (vida baixa: '), write(Vida), write(')'), nl.

% Regra 7: Atacar com criaturas com ameaçar primeiro
atacar_com_ameacar_primeiro(Criatura) :-
    jogador(eu, _, _, _, MinhasCriaturas),
    member(Criatura, MinhasCriaturas),
    criatura(Criatura, _, _, Habilidades),
    member(ameacar, Habilidades),
    write('Atacar primeiro com '), write(Criatura), write(' (possui ameaçar)'), nl.

% Regra 8: Defender com criatura de maior resistência contra atacante
defensor_ideal(Atacante, Defensor) :-
    criatura(Atacante, PoderAtk, _, _),
    jogador(eu, _, _, _, MinhasCriaturas),
    findall(Resistencia-Def, 
            (member(Def, MinhasCriaturas), 
             criatura(Def, _, Resistencia, _)),
            Resistencias),                      % <- agora está correto
    keysort(Resistencias, Ordenadas),
    reverse(Ordenadas, [MaxRes-Defensor|_]),   % pega a maior resistência
    MaxRes >= PoderAtk,
    write('Defenda com '), write(Defensor), write(' contra '), write(Atacante), nl.

% Regra 9: Não bloquear criaturas com atropelar se defensor for mais fraco
nao_bloquear_atropelar(Atacante) :-
    criatura(Atacante, _, _, Habilidades),
    member(atropelar, Habilidades),
    write('Não bloqueie '), write(Atacante), write(' (possui atropelar)'), nl.

% Regra 10: Usar duplo ataque para eliminar múltiplas ameaças
usar_duplo_ataque(Criatura) :-
    criatura(Criatura, _, _, Habilidades),
    member(duplo_ataque, Habilidades),
    write('Use '), write(Criatura), write(' para atacar duas vezes (duplo ataque)'), nl.

% Regra 13: Não atacar com criaturas fracas se oponente pode bloquear facilmente
nao_atacar_com(Criatura) :-
    criatura(Criatura, Poder, _, _),
    Poder < 2,
    jogador(_, _, _, _, CriaturasOponente),
    member(Blocker, CriaturasOponente),
    criatura(Blocker, _, Resistencia, _),
    Resistencia >= Poder,
    write('Não ataque com '), write(Criatura), write(' (muito fraca contra bloqueadores)'), nl.

% Regra 14: Usar alcance para bloquear criaturas com voar
usar_alcance_contra_voar(Defensor, Atacante) :-
    criatura(Defensor, _, _, Habilidades),
    member(alcance, Habilidades),
    criatura(Atacante, _, _, HabilidadesAtk),
    member(voar, HabilidadesAtk),
    write('Use '), write(Defensor), write(' para bloquear '), 
    write(Atacante), write(' (alcance vs voar)'), nl.

% Regra 15: Manter criaturas fortes para defesa se oponente tem muitas ameaças
manter_para_defesa(Criatura) :-
    criatura(Criatura, _, Resistencia, _),
    Resistencia >= 4,
    jogador(_, _, _, _, CriaturasOponente),
    length(CriaturasOponente, Quant),
    Quant >= 3,
    write('Mantenha '), write(Criatura), write(' para defesa (oponente tem muitas criaturas)'), nl.

% Regra 18: Priorizar destruir criaturas com habilidades perigosas
priorizar_destruir(Alvo) :-
    criatura(Alvo, _, _, Habilidades),
    (member(duplo_ataque, Habilidades); member(ameacar, Habilidades); member(atropelar, Habilidades)),
    write('Priorize destruir '), write(Alvo), write(' (habilidade perigosa)'), nl.

% Regra 19: Jogar criaturas com habilidades de evasão primeiro
jogar_evasao_primeiro(Criatura) :-
    criatura(Criatura, _, _, Habilidades),
    (member(voar, Habilidades); member(alcance, Habilidades)),
    write('Jogue '), write(Criatura), write(' primeiro (habilidade de evasão)'), nl.

% Regra 20: Usar magias no momento mais impactante
usar_magia_estrategicamente(Magia) :-
    magia(Magia, destruir, _),
    jogador(_, _, _, _, [Alvo|_]),
    priorizar_destruir(Alvo),
    write('Use '), write(Magia), write(' agora para eliminar maior ameaça'), nl.

% Regra 21: Não atacar se vantagem não é clara
nao_atacar_se :-
    jogador(eu, _, _, _, MinhasCriaturas),
    length(MinhasCriaturas, Minhas),
    jogador(_, _, _, _, CriaturasOponente),
    length(CriaturasOponente, Deles),
    Deles >= Minhas,
    write('Não ataque neste turno (não há vantagem clara)'), nl.

% Regra 22: Trocar vida por vantagem de tabuleiro
aceitar_dano_por_vantagem :-
    jogador(eu, Vida, _, _, _),
    Vida > 5,
    write('Aceite algum dano para manter vantagem no tabuleiro'), nl.

% Regra 23: Usar criaturas pequenas para bloquear ameaças grandes se necessário
bloquear_com_fraco(Defensor, Atacante) :-
    criatura(Atacante, Poder, _, _),
    criatura(Defensor, _, Resistencia, _),
    Resistencia < Poder,
    write('Use '), write(Defensor), write(' para bloquear '), write(Atacante), 
    write(' (sacrifício necessário)'), nl.

% Regra 24: Manter magias para situações críticas
manter_magia_para_critico(Magia) :-
    magia(Magia, _, _),
    jogador(eu, Vida, _, _, _),
    Vida > 10,
    write('Mantenha '), write(Magia), write(' para situação mais crítica'), nl.

% Regra 25: Descartar terreno se mão estiver cheia e não precisar de mais
descartar_terreno :-
    jogador(eu, _, Terrenos, Mao, _),
    length(Mao, TamanhoMao),
    TamanhoMao >= 7,
    Terrenos >= 4,
    write('Descarte um terreno (mão cheia e terrenos suficientes)'), nl.

% Regra 26: Listar Criaturas
listar_todas_criaturas :-
    findall(Criatura, criatura(Criatura, _, _, _), ListaCriaturas),
    write('Criaturas disponíveis: '), write(ListaCriaturas), nl.

% Retorna lista de criaturas que podem atacar com segurança
criaturas_ataque_viavel(Jogador, ListaAtaque) :-
    jogador(Jogador, _, _, _, MinhasCriaturas),
    findall(Criatura, 
            (member(Criatura, MinhasCriaturas),
             criatura(Criatura, Poder, _, _),
             Poder >= 2,  % Criaturas com poder relevante
             nao_deve_defender(Criatura)), % Não são melhores para defesa
            ListaAtaque),
    ListaAtaque \= [].

nao_deve_defender(Criatura) :-
    criatura(Criatura, _, Resistencia, Habilidades),
    \+ member(ameacar, Habilidades),
    Resistencia < 4.


% Ordena criaturas oponentes por prioridade de destruição
prioridade_destruicao(Oponente, AlvosOrdenados) :-
    jogador(Oponente, _, _, _, Criaturas),
    maplist(calcular_perigo, Criaturas, CriaturasComPerigo),
    keysort(CriaturasComPerigo, CriaturasOrdenadas),
    reverse(CriaturasOrdenadas, AlvosOrdenados).

calcular_perigo(Criatura, Perigo-Criatura) :-
    criatura(Criatura, Poder, Resistencia, Habilidades),
    length(Habilidades, QuantHabilidades),
    Perigo is Poder * 2 + Resistencia + QuantHabilidades * 3.

combinacao_eficaz(H1, H2) :-
    (member(voar, H1), member(atropelar, H2));
    (member(ameacar, H1), member(duplo_ataque, H2)).

% Encontra combinações de habilidades complementares
combinacoes_ataque_eficaz(ListaCombinacoes) :-
    findall([Atacante1, Atacante2],
            (jogador(eu, _, _, _, MinhasCriaturas),
             member(Atacante1, MinhasCriaturas),
             member(Atacante2, MinhasCriaturas),
             Atacante1 \= Atacante2,
             criatura(Atacante1, _, _, H1),
             criatura(Atacante2, _, _, H2),
             combinacao_eficaz(H1, H2)),
            ListaCombinacoes).
    
% Calcula dano total que pode ser infligido
dano_total_atacantes([], 0).
dano_total_atacantes([Atacante|Resto], DanoTotal) :-
    criatura(Atacante, Poder, _, _),
    dano_total_atacantes(Resto, DanoRestante),
    DanoTotal is Poder + DanoRestante.

% Gera todas as combinações possíveis de ataque
gerar_combinacoes_ataque(Profundidade, Combinacoes) :-
    gerar_combinacoes(Profundidade, [], Combinacoes).

gerar_combinacoes(0, Atual, [Atual]).
gerar_combinacoes(Profundidade, Atual, Combinacoes) :-
    Profundidade > 0,
    jogador(eu, _, _, _, MinhasCriaturas),
    member(Criatura, MinhasCriaturas),
    \+ member(Criatura, Atual),
    NovaProfundidade is Profundidade - 1,
    gerar_combinacoes(NovaProfundidade, [Criatura|Atual], Combinacoes).


melhor_bloqueador(Atacante, Bloqueador) :-
    criatura(Atacante, PoderAtk, _, _),
    jogador(eu, _, _, _, MinhasCriaturas),
    member(Bloqueador, MinhasCriaturas),
    criatura(Bloqueador, _, Resistencia, _),
    Resistencia >= PoderAtk,
    !,  % Corte após encontrar o primeiro bloqueador adequado
    write('Melhor bloqueador: '), write(Bloqueador), nl.

melhor_bloqueador(Atacante, 'Nenhum') :-
    write('Nenhum bloqueador ideal encontrado para '), write(Atacante), nl.


jogar_terreno_se_necesario :-
    jogador(eu, _, Terrenos, Mao, _),
    member('Terreno', Mao),
    Terrenos < 3,
    !,  % Corte se pode jogar terreno
    write('Jogue um terreno agora.'), nl.

jogar_terreno_se_necesario :-
    write('Não é necessário jogar terreno neste momento.'), nl.


usar_cura_emergencial :-
    jogador(eu, Vida, _, Mao, _),
    Vida < 5,
    member(Magia, Mao),
    magia(Magia, curar, _),
    !,  % Corte se encontrar magia de cura com vida baixa
    write('Use '), write(Magia), write(' imediatamente!'), nl.

usar_cura_emergencial :-
    write('Situação de vida estável, não use cura agora.'), nl.

definir_bloqueadores(Oponente) :-
    % Obter listas de atacantes e defensores
    jogador(Oponente, _, _, _, Atacantes),
    Atacantes \= [],
    jogador(eu, _, _, _, Defensores),
    
    % Ordenar atacantes por poder (decrescente)
    ordenar_atacantes_por_poder(Atacantes, AtacantesOrdenados),
    copy_term(Defensores, DefensoresDisponiveis),
    
    % Processar cada atacante
    processar_atacantes(AtacantesOrdenados, DefensoresDisponiveis, [], Bloqueios),
    
    % Exibir recomendações
    exibir_recomendacoes(Bloqueios).

% Ordenar atacantes por poder (decrescente)
ordenar_atacantes_por_poder(Atacantes, AtacantesOrdenados) :-
    maplist(obter_poder, Atacantes, AtacantesComPoder),
    keysort(AtacantesComPoder, Temp),
    reverse(Temp, AtacantesOrdenadosReversos),
    pairs_values(AtacantesOrdenadosReversos, AtacantesOrdenados).

obter_poder(Atacante, Poder-Atacante) :-
    criatura(Atacante, Poder, _, _).

% Processar cada atacante na ordem de prioridade
processar_atacantes([Atacante|Resto], Defensores, Acumulador, Bloqueios) :-
    criatura(Atacante, PoderAtk, _, HabilidadesAtk),
    (
        member(ameacar, HabilidadesAtk),
        encontrar_dois_bloqueadores(Defensores, PoderAtk, HabilidadesAtk, Bloqueadores, NovosDefensores),
        write(NovosDefensores)

    ->
        append(Acumulador, [Atacante-Bloqueadores-0], NovoAcumulador)
    ;
        member(atropelar, HabilidadesAtk),
        encontrar_bloqueador_normal(Defensores, PoderAtk, Bloqueador, NovosDefensores),
    ->
        calcular_dano_atropelar(PoderAtk, Bloqueador, Dano),
        append(Acumulador, [Atacante-[Bloqueador]-Dano], NovoAcumulador)
    ;
        member(voar, HabilidadesAtk),
        encontrar_bloqueador_voar(Defensores, PoderAtk, Bloqueador, NovosDefensores)
    ->
        append(Acumulador, [Atacante-[Bloqueador]-0], NovoAcumulador)
    ;
        encontrar_bloqueador_normal(Defensores, PoderAtk, Bloqueador, NovosDefensores)
    ->
        append(Acumulador, [Atacante-[Bloqueador]-0], NovoAcumulador)
    ;
        % Sem defensores disponíveis
        append(Acumulador, [Atacante-[]-PoderAtk], NovoAcumulador),
        NovosDefensores = Defensores
            
    ),
    processar_atacantes(Resto, NovosDefensores, NovoAcumulador, Bloqueios).


% Encontrar 2 bloqueadores para ameaçar
encontrar_dois_bloqueadores(Defensores, _, _, [B1, B2], NovosDefensores) :-
    findall(Res-Def, (member(Def, Defensores), criatura(Def, _, Res, _)), DefensoresComRes),
    length(DefensoresComRes, L), L >= 2,  % Garantir que existem ao menos 2 defensores
    keysort(DefensoresComRes, Ordenados),
    reverse(Ordenados, [_-B1, _-B2|_]),
    select(B1, Defensores, Temp),
    select(B2, Temp, NovosDefensores).

% Calcular dano de ameaçar
calcular_dano_atropelar(PoderAtk, Bloqueador, Dano) :-
    criatura(Bloqueador, _, Resistencia, _),
    Dano is max(0, PoderAtk - Resistencia).

% Encontrar bloqueador para voar
encontrar_bloqueador_voar(Defensores, PoderAtk, Bloqueador, NovosDefensores) :-
    findall(Def, (member(Def, Defensores), criatura(Def, _, _, H), (member(voar, H); member(alcance, H))), DefensoresVoar),
    encontrar_melhor_bloqueador(DefensoresVoar, PoderAtk, Bloqueador, NovosDefensores).

% Encontrar bloqueador normal
encontrar_bloqueador_normal(Defensores, PoderAtk, Bloqueador, NovosDefensores) :-
    encontrar_melhor_bloqueador(Defensores, PoderAtk, Bloqueador, NovosDefensores).

% Encontrar melhor bloqueador disponível
encontrar_melhor_bloqueador(Defensores, PoderAtk, Bloqueador, NovosDefensores) :-
    findall(Res-Def, (member(Def, Defensores), criatura(Def, _, Res, _)), DefensoresComRes),

    (PoderAtk > 0 ->
        include_maior_igual(PoderAtk, DefensoresComRes, DefensoresSuficientes),
        ( DefensoresSuficientes \= [] ->
            keysort(DefensoresSuficientes, [_-Bloqueador|_])
        ;
            keysort(DefensoresComRes, Temp),
            reverse(Temp, [_-Bloqueador|_])
        )
    ;
        member(_-Bloqueador, DefensoresComRes)
    ),

    select(Bloqueador, Defensores, NovosDefensores).

% Auxiliar: filtra pares Res-Def com Res >= Poder
include_maior_igual(_, [], []).
include_maior_igual(Poder, [Res-Def|Resto], [Res-Def|Filtrados]) :-
    Res >= Poder,
    include_maior_igual(Poder, Resto, Filtrados).
include_maior_igual(Poder, [_|Resto], Filtrados) :-
    include_maior_igual(Poder, Resto, Filtrados).

% Exibir recomendações
exibir_recomendacoes([]) :-
    write('Todas as ameaças foram bloqueadas adequadamente.'), nl.
exibir_recomendacoes([Atacante-Bloqueadores-Dano|Resto]) :-
    criatura(Atacante, PoderAtk, _, HabilidadesAtk),
    (length(Bloqueadores, 1) ->
        Bloqueadores = [Bloqueador],
        criatura(Bloqueador, _, Res, HabilidadesDef),
        format('Sua criatura ~w (~w) deve bloquear ~w (~w)~n', [Bloqueador, Res, Atacante, PoderAtk]),
        (HabilidadesDef \= [] -> 
            format('    Habilidades do defensor: ~w~n', [HabilidadesDef])
        ; true)
    ;
        format('Suas criaturas ~w devem bloquear juntas ~w (~w)~n', [Bloqueadores, Atacante, PoderAtk]),
        (member(ameacar, HabilidadesAtk) ->
            format('    Dano recebido: ~w~n', [Dano])
        ; true)
    ),
    (HabilidadesAtk \= [] -> 
        format('    Habilidades do atacante: ~w~n', [HabilidadesAtk])
    ; true),
    exibir_recomendacoes(Resto).