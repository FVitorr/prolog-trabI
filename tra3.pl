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
criatura('Elemental de Terra', 7, 3, [atropelar]).
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
jogador(eu, 24, 3, ['Raios Solares', 'Desintegrar', 'Seta Flamejante', 'Elemental de Terra', 'Demônio Sanguinário'], ['Hidra de Cabeças Múltiplas', 'Tartaruga Defensora', 'Arqueiro Élfico', 'Dragão Branco', 'Goblin Defensor']).
jogador(oponente1, 12, 2, [], ['Hidra de Cabeças Múltiplas', 'Elemental de Terra', 'Demônio Sanguinário', 'Goblin Defensor', 'Espreitador Noturno', 'Cavaleiro Negro']).

%%% REGRAS DE DECISÃO


% Regra 1: Trocar mão inicial se tiver menos de 2 ou mais de 4 terrenos na mão
trocar_mao_inicial(Jogador) :-
    jogador(Jogador, _, _, Mao, _),
    findall(T, (member(T, Mao), terreno(T)), TerrenosMao),
    length(TerrenosMao, Quantidade),
    (Quantidade < 2; Quantidade > 4),
    write('Troque a mão inicial: quantidade de terrenos inadequada ('), 
    write(Quantidade), write(').'), nl.


% Regra 2: Listar todas criaturas no campo do jogador
listar_criaturas_jogador(Jogador) :-
    jogador(Jogador, _, _, _, Campo),
    findall(Nome, (member(Nome, Campo), criatura(Nome, _, _, _)), Lista),
    write('Criaturas no campo do jogador '), write(Jogador), write(':'), nl,
    listar(Lista).


% Regra 3: Listar todas as criaturas com uma habilidade específica
listar_criaturas_com_habilidade_especifica(Habilidade) :-
    habilidade(Habilidade),
    findall(Nome, (criatura(Nome, _, _, Habilidades), member(Habilidade, Habilidades)), Lista),
    write('Criaturas com a habilidade '), write(Habilidade), write(':'), nl,
    listar(Lista).


% Auxiliar para imprimir listas linha por linha
listar([]).
listar([H|T]) :-
    write('- '), write(H), nl,
    listar(T).


% Regra 4: Listar todas as criaturas com poder maior que um valor específico
listar_criaturas_com_poder_maior_que(Valor) :-
    findall(Nome, (criatura(Nome, Poder, _, _), Poder > Valor), Lista),
    write('Criaturas com poder maior que '), write(Valor), write(':'), nl,
    listar(Lista).


% Regra 5: Listar todas as criaturas com defesa maior que um valor específico
listar_criaturas_com_defesa_maior_que(Valor) :-
    findall(Nome, (criatura(Nome, _, Defesa, _), Defesa > Valor), Lista),
    write('Criaturas com defesa maior que '), write(Valor), write(':'), nl,
    listar(Lista).


% Regra 6: Listar possíveis respostas (criatura) -> informa criatura que oponente jogou
% e lista mágicas que temos com custo de mana maior que o poder da criatura
% e com custo que seja possível conjurar com base nos nossos terrenos
listar_respostas_criatura(Oponente, Criatura) :-
    jogador(Oponente, _, _, _, Campo),
    member(Criatura, Campo),
    criatura(Criatura, Poder, _, _),
    jogador(eu, _, _, Mao, _),
    findall(Magia,
        (member(Magia, Mao),
         magia(Magia, destruir, Custo),
         Custo >= Poder),
        MagiasPossiveis),
    write('Respostas possíveis para a criatura '), write(Criatura),
    write(' do oponente '), write(Oponente), write(': '), nl,
    listar(MagiasPossiveis).


% Regra 7: Escolher melhor defensor para criatura sem habilidade nenhuma
% Prioridade:
% 1. Criatura que sobreviva (defesa > poder) e mate (poder >= defesa oponente)
% 2. Criatura que apenas sobreviva (defesa > poder)
% 3. Criatura sem habilidades
% 4. Criatura com menor ataque
escolher_defensor(CriaturaOponente) :-
    jogador(oponente1, _, _, _, CampoOponente),
    member(CriaturaOponente, CampoOponente),
    criatura(CriaturaOponente, PoderOponente, DefesaOponente, []), % sem habilidades
    jogador(eu, _, _, _, CampoDefensor),

    % 1. Criaturas que matam e sobrevivem
    findall(D,
        (member(D, CampoDefensor),
         criatura(D, PoderD, DefesaD, _),
         DefesaD > PoderOponente,
         PoderD >= DefesaOponente),
        Lista1),
    (Lista1 \= [] ->
        write('Melhor defensor (mata e sobrevive):'), nl,
        listar(Lista1),
        member(Defensor, Lista1)
    ;
        % 2. Criaturas que apenas sobrevivem
        findall(D,
            (member(D, CampoDefensor),
             criatura(D, _, DefesaD, _),
             DefesaD > PoderOponente),
            Lista2),
        (Lista2 \= [] ->
            write('Defensores que sobrevivem:'), nl,
            listar(Lista2),
            member(Defensor, Lista2)
        ;
            % 3. Criaturas sem habilidades
            findall(D,
                (member(D, CampoDefensor),
                 criatura(D, _, _, Habs),
                 Habs == []),
                Lista3),
            (Lista3 \= [] ->
                write('Defensores sem habilidades:'), nl,
                listar(Lista3),
                member(Defensor, Lista3)
            ;
                % 4. Criatura com menor ataque
                findall((PoderD, D),
                    (member(D, CampoDefensor),
                     criatura(D, PoderD, _, _)),
                    Pares),
                sort(Pares, [(MenorPoder, Defensor)|_]),
                write('Defensor com menor ataque:'), nl,
                write('- '), write(Defensor), nl
            ;
                write('Nenhum defensor disponível.'), nl,
                fail
            )
        )
    ).


% Regra 7: Escolher melhores defensores para criatura com atropelar, existe dano residual (poder - defesa)
% Prioridade:
% Primeira opção: criatura que sobreviva (defesa maior que poder) e que mate a criatura do oponente (poder maior que defesa)
% Segunda opção: criatura que sobreviva (defesa maior que poder)
% Terceira opção: criatura com maior defesa
escolher_defensores_atropelar(CriaturaOponente) :-
    jogador(oponente1, _, _, _, CampoOponente),
    member(CriaturaOponente, CampoOponente),
    criatura(CriaturaOponente, PoderOponente, DefesaOponente, Habilidades),
    member(atropelar, Habilidades), % Verifica se tem habilidade atropelar
    jogador(eu, _, _, _, CampoDefensor),

    % 1. Criaturas que matam e sobrevivem
    findall(D,
        (member(D, CampoDefensor),
         criatura(D, PoderD, DefesaD, _),
         DefesaD > PoderOponente,
         PoderD >= DefesaOponente),
        Lista1),
    (Lista1 \= [] ->
        write('Melhor defensor (mata e sobrevive):'), nl,
        listar(Lista1)
    ;
        % 2. Criaturas que apenas sobrevivem
        findall(D,
            (member(D, CampoDefensor),
             criatura(D, _, DefesaD, _),
             DefesaD > PoderOponente),
            Lista2),
        (Lista2 \= [] ->
            write('Defensores que sobrevivem:'), nl,
            listar(Lista2)
        ;
            % 3. Criatura com maior defesa
            findall((DefesaD, D),
                (member(D, CampoDefensor),
                 criatura(D, _, DefesaD, _)),
                Pares),
            (Pares \= [] ->
                sort(0, @>=, Pares, [(MaiorDefesa, MelhorDefensor)|_]),
                write('Defensor com maior defesa:'), nl,
                write('- '), write(MelhorDefensor), nl
            ;
                write('Nenhum defensor disponível.'), nl,
                fail
            )
        )
    ).


% Regra 8: Escolher melhores defensores para criatura com voar
% Prioridade:
% 1. Criatura com alcance ou voar que sobreviva (defesa maior que poder) e que mate a criatura do oponente (poder maior que defesa)
% 2. Criatura com alcance ou voar que sobreviva (defesa maior que poder)
% 3. Criatura com alcance ou voar
escolher_defensores_voar(CriaturaOponente) :-
    jogador(oponente1, _, _, _, CampoOponente),
    member(CriaturaOponente, CampoOponente),
    criatura(CriaturaOponente, PoderOponente, DefesaOponente, Habilidades),
    member(voar, Habilidades), % Verifica se tem habilidade voar
    jogador(eu, _, _, _, CampoDefensor),

    % 1. Criaturas que matam e sobrevivem
    findall(D,
        (member(D, CampoDefensor),
         criatura(D, PoderD, DefesaD, Habs),
         (member(alcance, Habs); member(voar, Habs)),
         DefesaD > PoderOponente,
         PoderD >= DefesaOponente),
        Lista1),
    (Lista1 \= [] ->
        write('Melhor defensor (mata e sobrevive):'), nl,
        listar(Lista1)
    ;
        % 2. Criaturas que apenas sobrevivem
        findall(D,
            (member(D, CampoDefensor),
             criatura(D, _, DefesaD, Habs),
             (member(alcance, Habs); member(voar, Habs)),
             DefesaD > PoderOponente),
            Lista2),
        (Lista2 \= [] ->
            write('Defensores que sobrevivem:'), nl,
            listar(Lista2)
        ;
            % 3. Criatura com alcance ou voar
            findall(D,
                (member(D, CampoDefensor),
                 criatura(D, _, _, Habs),
                 (member(alcance, Habs); member(voar, Habs))),
                Lista3),
            (Lista3 \= [] ->
                write('Criaturas com alcance ou voar:'), nl,
                listar(Lista3)
            ;
                write('Nenhum defensor disponível.'), nl,
                fail
            )
        )
    ).


% Regra 9: Avaliar dato letal iminente (calcula se podemos morrer com base na nossa vida e nos poderes das criaturas inimigas, desconsiderando nossas bloqueadoras)
avaliar_dano_letal_iminente :-
    jogador(oponente1, _, _, _, CampoOponente),
    jogador(eu, Vida, _, _, _),
    findall(Poder,
        (member(Criatura, CampoOponente),
         criatura(Criatura, Poder, _, _)),
        Poderes),
    sumlist(Poderes, DanoTotal),
    (DanoTotal >= Vida ->
        write('Atenção: dano letal iminente!'), nl
    ;
        write('Nenhum dano letal iminente.'), nl
    ).


% Regra 10: Calcular dano total que jogador pode causar
calcular_dano_total_jogador(Jogador, DanoTotal) :-
    jogador(Jogador, _, _, _, Campo),
    findall(Poder,
        (member(Criatura, Campo),
         criatura(Criatura, Poder, _, _)),
        Poderes),
    sumlist(Poderes, DanoTotal).


% Regra 11: Gerar possiveis combinações de criaturas para atacar
gerar_combinacoes_atacar_jogador(Jogador, Combinacoes) :-
    jogador(Jogador, _, _, _, Campo),
    findall(Criatura, member(Criatura, Campo), Criaturas),
    findall(Comb, combinacao(Criaturas, Comb), Combinacoes).
combinacao([], []).
combinacao([H|T], [H|Comb]) :-
    combinacao(T, Comb).
combinacao([_|T], Comb) :-
    combinacao(T, Comb).


% Regra 12: Listar criaturas que podem ser destruídas por uma magia específica
criaturas_podem_ser_destruidas_por_magia(Magia, Criaturas) :-
    magia(Magia, destruir, CustoMagia),
    findall(Criatura,
         (criatura(Criatura, Poder, _, _),
         Poder =< CustoMagia),
        Criaturas).


% Regra 13: Listar criaturas que podem ser curadas por uma magia específica
criaturas_podem_ser_curadas_por_magia(Magia, Criaturas) :-
    magia(Magia, curar, CustoMagia),
    findall(Criatura,
         (criatura(Criatura, _, Defesa, _),
         Defesa =< CustoMagia),
        Criaturas).


% Regra 14: Lista minha mão
listar_mao(Magias, Criaturas, Terrenos) :-
    jogador(eu, _, _, Mao, _),
    findall(Magia, (member(Magia, Mao), magia(Magia, _, _)), Magias),
    findall(Criatura, (member(Criatura, Mao), criatura(Criatura, _, _, _)), Criaturas),
    findall(Terreno, (member(Terreno, Mao), terreno(Terreno)), Terrenos).

