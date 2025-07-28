% my_agent.pl
% Implementação de um agente lógico para o Mundo do Wumpus
% Baseado em modelos e regras condição-ação.

% --- Declaração de Predicados Dinâmicos ---
% Estes predicados representam o estado interno do agente e seu conhecimento sobre o mundo.
% Eles são dinâmicos pois seu conteúdo muda durante a execução do agente.
:- dynamic stench/3.          % stench(Lin, Col, K) - Percepto de fedor na interação K
:- dynamic breeze/3.          % breeze(Lin, Col, K) - Percepto de brisa na interação K
:- dynamic glitter/3.         % glitter(Lin, Col, K) - Percepto de brilho na interação K
:- dynamic bump/3.            % bump(Lin, Col, K) - Percepto de colisão na interação K
:- dynamic scream/3.          % scream(Lin, Col, K) - Percepto de grito na interação K
:- dynamic interaction/1.     % interaction(K) - Número da interação atual
:- dynamic position/3.        % position(Lin, Col, K) - Posição do agente (linha, coluna) na interação K
:- dynamic orientation/2.     % orientation(Dir, K) - Orientação do agente (direção) na interação K
:- dynamic act/2.             % act(Action, K) - Ação realizada na interação K

% Conhecimento do Agente sobre o Mundo (ADICIONADOS/COMPLETADOS):
:- dynamic wall/3.            % wall(Lin, Col, Dir) - Agente sabe que há uma parede em (Lin,Col) na direção Dir
:- dynamic gold1/2.           % gold1(Lin, Col) - Agente sabe que há ouro em (Lin,Col)
:- dynamic visited/2.         % visited(Lin, Col) - Quadrado (Lin,Col) já foi visitado
:- dynamic safe/2.            % safe(Lin, Col) - Quadrado (Lin,Col) é conhecido como seguro (sem poço ou wumpus)
:- dynamic wumpus_possible/2. % wumpus_possible(Lin, Col) - Wumpus pode estar em (Lin,Col)
:- dynamic pit_possible/2.    % pit_possible(Lin, Col) - Poço pode estar em (Lin,Col)
:- dynamic wumpus_known/2.    % wumpus_known(Lin, Col) - Wumpus é definitivamente conhecido em (Lin,Col)
:- dynamic pit_known/2.       % pit_known(Lin, Col) - Poço é definitivamente conhecido em (Lin,Col)

% Estado Interno do Agente (ADICIONADOS/COMPLETADOS):
:- dynamic has_arrow/0.       % has_arrow - Agente possui a flecha
:- dynamic wumpus_alive/0.    % wumpus_alive - Wumpus está vivo
:- dynamic wumpus_dead/0.     % wumpus_dead - Wumpus está morto
:- dynamic holding_gold/0.    % holding_gold - Agente está segurando o ouro

% IMPORTANTE: Declarações para acessar predicados do simulador
:- multifile wumpus_world_extent/1. % Permite que este predicado seja definido em múltiplos arquivos
:- dynamic wumpus_world_extent/1.  % Declara que este predicado pode ser modificado dinamicamente

% --- Inicialização do Agente ---

% Predicado principal para inicializar a base de conhecimento do agente.
init_agent:-
    write('--- init_agent: Starting ---'), nl,
    retractall_predicates, % Limpa todos os fatos dinâmicos anteriores
    write('--- init_agent: retractall_predicates completed ---'), nl,
    assertall_current,     % Asserta os fatos iniciais do agente
    write('--- init_agent: assertall_current completed ---'), nl,
    write('--- init_agent: Finished ---'), nl.

% Retrata (remove) todos os fatos dinâmicos da base de conhecimento.
% Essencial para reiniciar o agente em um novo mundo ou tentativa.
retractall_predicates:-
    write('  retractall_predicates: Starting...'), nl,
    retractall(stench(_,_,_)),
    retractall(breeze(_,_,_)),
    retractall(glitter(_,_,_)),
    retractall(bump(_,_,_)),
    retractall(scream(_,_,_)),
    retractall(interaction(_)),
    retractall(position(_,_,_)),
    retractall(orientation(_,_)),
    retractall(act(_,_)),
    retractall(wall(_,_,_)),
    retractall(gold1(_,_)),
    % ADICIONADOS: Retratar fatos do modelo interno
    retractall(visited(_,_)),
    retractall(safe(_,_)),
    retractall(wumpus_possible(_,_)),
    retractall(pit_possible(_,_)),
    retractall(wumpus_known(_,_)),
    retractall(pit_known(_,_)),
    retractall(has_arrow),
    retractall(wumpus_alive),
    retractall(wumpus_dead),
    retractall(holding_gold),
    write('  retractall_predicates: Finished.'), nl.

% Asserta os fatos iniciais na base de conhecimento do agente.
% Representa o estado inicial do agente no mundo.
assertall_current:-
    write('  assertall_current: Starting...'), nl,
    asserta(interaction(0)),
    asserta(position(1,1,0)),
    asserta(orientation(east,0)),
    asserta(act([],0)),
    % ADICIONADOS: Asserta fatos iniciais do agente e do modelo
    asserta(has_arrow),             % Agente começa com uma flecha
    asserta(wumpus_alive),          % Wumpus é considerado vivo no início
    asserta(safe(1,1)),             % O quadrado inicial (1,1) é sempre seguro
    asserta(visited(1,1)),          % O quadrado inicial (1,1) é marcado como visitado
    write('  assertall_current: Finished.'), nl.

% Predicado para reiniciar o agente, que é o mesmo que inicializá-lo.
restart_agent:-
    write('--- restart_agent: Starting ---'), nl,
    init_agent,
    write('--- restart_agent: Finished ---'), nl.

% --- Ciclo de Agente (Percepção -> Ação) ---

% Predicado principal do agente lógico.
% Recebe os perceptos do ambiente e determina a próxima ação.
my_agent(Percept,Action):-
    write('--- my_agent: Starting ---'), nl,
    see(Percept,S), % 1. Percepção: Traduz os perceptos brutos em átomos simbólicos (S)
    write('--- my_agent: see completed ---'), nl,
    next(S),        % 2. Atualização do Modelo: Atualiza o estado interno do agente com base nos perceptos e na última ação
    write('--- my_agent: next completed ---'), nl,
    action(Action), % 3. Decisão: Seleciona a próxima ação com base no modelo atualizado
    write('--- my_agent: action completed ---'), nl,
    write('--- my_agent: Finished ---'), nl.

% --- Processamento de Perceptos ---

% Converte a lista de perceptos brutos (yes/no) em uma lista de átomos simbólicos.
see(LPercepts,LAtoms):-
    see1(LPercepts,1,LAtoms), % Chama o auxiliar see1 para processar a lista
    log_see(LPercepts,LAtoms). % Registra os perceptos para depuração

% Auxiliar recursivo para processar a lista de perceptos.
see1([],_,[]):- !. % Caso base: lista vazia

% Se o percepto é 'no', avança para o próximo sem adicionar átomo.
see1([no|RPercepts],X,LAtoms):-
    !,X1 is X+1,
    see1(RPercepts,X1,LAtoms).

% Se o percepto é 'yes', traduz o índice para um átomo e adiciona à lista.
see1([yes|RPercepts],X,[Atom|RAtoms]):-
    X1 is X+1,
    translate(X,Atom), % Traduz o índice X para o átomo correspondente (e.g., 1 -> stench)
    see1(RPercepts,X1,RAtoms).

% Mapeamento de índices de perceptos para seus átomos simbólicos.
translate(1,stench):- !.
translate(2,breeze):- !.
translate(3,glitter):- !.
translate(4,bump):- !.
translate(5,scream).

% Registra os perceptos brutos e os átomos simbólicos extraídos.
log_see(LPercepts,LAtoms):-
    nl,write('Log_see'),nl,
    write('Percepts = '),write(LPercepts),nl,
    write('Atoms = '),write(LAtoms),nl.

% --- Atualização de Estado (Baseado em Modelo) ---

% Atualiza o modelo interno do agente sobre o mundo.
next(LAtoms):-
    write('--- next: Starting ---'), nl,
    % 1. Obtém os detalhes da interação atual antes da atualização.
    current_interaction(Lin,Col,Dir,A,Knew,LAtoms),
    write('--- next: current_interaction completed ---'), nl,
    % 2. Aplica o efeito da última ação (A) na posição e orientação do agente.
    effect(Lin,Col,Dir,A,Knew),
    write('--- next: effect completed ---'), nl,
    % 3. Atualiza a base de conhecimento do agente com base nos novos perceptos e efeitos.
    update_knowledge(Knew, LAtoms), % COMPLETO: Chamada ao novo predicado update_knowledge
    write('--- next: update_knowledge completed ---'), nl, % This line *should* appear if update_knowledge succeeds
    % 4. Evolui o estado do mundo (e.g., ouro sendo pego, parede descoberta).
    write('--- next: Calling evolution...'), nl, % NEW DEBUG - This should appear before evolution starts
    evolution(Knew),
    write('--- next: evolution completed ---'), nl,
    log_next, % Registra o estado da base de conhecimento para depuração
    write('--- next: Finished ---'), nl.

% Recupera os detalhes da interação anterior e asserta os novos perceptos para a interação atual (Knew).
current_interaction(Lin,Col,Dir,A,Knew,LAtoms):-
    retract(interaction(K)),    % Remove o número da interação anterior
    position(Lin,Col,K),        % Obtém a posição anterior
    orientation(Dir,K),         % Obtém a orientação anterior
    act(A,K),                   % Obtém a ação anterior
    Knew is K+1,                % Incrementa o contador de interação
    asserta(interaction(Knew)), % Asserta o novo número da interação
    assertall_predicates(Knew,LAtoms). % Asserta os novos perceptos com o novo K

% Asserta todos os átomos percebidos para a interação atual (K).
assertall_predicates(_,[]):- !.
assertall_predicates(K,[Atom|RAtoms]):-
    Predicate =..[Atom,_,_,K], % Constrói o termo do predicado (e.g., stench(_,_,K))
    asserta(Predicate),        % Asserta o predicado na base de conhecimento
    assertall_predicates(K,RAtoms).

% Define os efeitos das ações na posição e orientação do agente.

% Efeito de uma ação vazia (ou estado inicial): agente permanece no lugar.
effect(Lin,Col,Dir,[],Knew):-
    !,asserta(position(Lin,Col,Knew)),
    asserta(orientation(Dir,Knew)).

% Efeito da ação 'grab': agente permanece no lugar, e o ouro é removido do mapa.
effect(Lin,Col,Dir,grab,Knew):-
    retract(gold1(Lin,Col)),!, % Se havia ouro, remove-o do mapa
    asserta(position(Lin,Col,Knew)),
    asserta(orientation(Dir,Knew)).
effect(Lin,Col,Dir,grab,Knew):- % Caso não haja ouro para pegar, agente ainda fica no lugar
    !,asserta(position(Lin,Col,Knew)),
    asserta(orientation(Dir,Knew)).

% Efeito da ação 'goforward':
% Se percebeu 'bump', significa que bateu em uma parede e não se moveu.
effect(Lin,Col,Dir,goforward,Knew):-
    bump(_,_,Knew),!, % Se 'bump' foi percebido nesta interação
    asserta(position(Lin,Col,Knew)), % Agente permanece na mesma posição
    asserta(orientation(Dir,Knew)).
% Se não houve 'bump', agente se move na direção atual.
effect(Lin,Col,east,goforward,Knew):-
    !, Col1 is Col+1,
    asserta(position(Lin,Col1,Knew)),
    asserta(orientation(east,Knew)).
effect(Lin,Col,north,goforward,Knew):-
    !, Lin1 is Lin+1,
    asserta(position(Lin1,Col,Knew)),
    asserta(orientation(north,Knew)).
effect(Lin,Col,west,goforward,Knew):-
    !, Col1 is Col-1,
    asserta(position(Lin,Col1,Knew)),
    asserta(orientation(west,Knew)).
effect(Lin,Col,south,goforward,Knew):-
    !, Lin1 is Lin-1,
    asserta(position(Lin1,Col,Knew)),
    asserta(orientation(south,Knew)).

% Efeito da ação 'turnleft': agente permanece no lugar, muda a orientação.
effect(Lin,Col,east,turnleft,Knew):-
    !, asserta(position(Lin,Col,Knew)),
    asserta(orientation(north,Knew)).
effect(Lin,Col,north,turnleft,Knew):-
    !, asserta(position(Lin,Col,Knew)),
    asserta(orientation(west,Knew)).
effect(Lin,Col,west,turnleft,Knew):-
    !, asserta(position(Lin,Col,Knew)),
    asserta(orientation(south,Knew)).
effect(Lin,Col,south,turnleft,Knew):-
    !, asserta(position(Lin,Col,Knew)),
    asserta(orientation(east,Knew)).

% ADICIONADO: Efeito da ação 'turnright': agente permanece no lugar, muda a orientação.
effect(Lin,Col,east,turnright,Knew):-
    !, asserta(position(Lin,Col,Knew)),
    asserta(orientation(south,Knew)).
effect(Lin,Col,north,turnright,Knew):-
    !, asserta(position(Lin,Col,Knew)),
    asserta(orientation(east,Knew)).
effect(Lin,Col,west,turnright,Knew):-
    !, asserta(position(Lin,Col,Knew)),
    asserta(orientation(north,Knew)).
effect(Lin,Col,south,turnright,Knew):-
    !, asserta(position(Lin,Col,Knew)),
    asserta(orientation(west,Knew)).

% ADICIONADO: Efeito da ação 'shoot': agente permanece no lugar, perde a flecha (gerenciado em do/1).
effect(Lin,Col,Dir,shoot,Knew):-
    !, asserta(position(Lin,Col,Knew)),
    asserta(orientation(Dir,Knew)).

% ADICIONADO: Efeito da ação 'climb': agente permanece no lugar (1,1), efetivamente "sai" do mundo.
effect(Lin,Col,Dir,climb,Knew):-
    !, asserta(position(Lin,Col,Knew)),
    asserta(orientation(Dir,Knew)).

% NOVO PREDICADO: Atualiza o conhecimento do agente sobre o mundo com base nos perceptos e no estado atual.
update_knowledge(K, LAtoms):-
    write('  update_knowledge: Starting...'), nl,
    % PRIMEIRA VERIFICAÇÃO: Acessibilidade de wumpus_world_extent
    ( wumpus_world_extent(Extent) -> % Consulta o fato do simulador
        format('  update_knowledge: World extent is ~w~n', [Extent])
    ;
        write('  update_knowledge: ERROR: wumpus_world_extent/1 not found or accessible! This will cause failure.'), nl,
        fail % Força a falha se não conseguir o extent
    ),
    position(X, Y, K),       % Posição atual do agente
    orientation(Dir, K),     % Orientação atual do agente
    format('  update_knowledge: Agent at (~w,~w), facing ~w, K=~w~n', [X,Y,Dir,K]),

    % Marca o quadrado atual como visitado e seguro.
    assert_if_new(visited(X, Y)),
    assert_if_new(safe(X, Y)),
    write('  update_knowledge: Current square marked visited/safe.'), nl,

    % TRATAMENTO DE STENCH
    handle_stench(X, Y, LAtoms),
    write('  update_knowledge: Stench inference completed.'), nl,

    % TRATAMENTO DE BREEZE
    handle_breeze(X, Y, LAtoms),
    write('  update_knowledge: Breeze inference completed.'), nl,

    % TRATAMENTO DE AUSÊNCIA DE STENCH E BREEZE (adjacentes seguros)
    handle_no_stench_no_breeze(X, Y, LAtoms),
    write('  update_knowledge: Safe adjacent inference completed.'), nl,

    % TRATAMENTO DE BUMP
    handle_bump(X, Y, Dir, LAtoms),
    write('  update_knowledge: Bump inference completed.'), nl,

    % TRATAMENTO DE SCREAM
    handle_scream(LAtoms),
    write('  update_knowledge: Scream inference completed.'), nl,
    write('  update_knowledge: Finished.'), nl.

% Predicados auxiliares para update_knowledge/2 para melhor modularidade e robustez

handle_stench(X, Y, LAtoms) :-
    write('  update_knowledge: Checking for stench percept.'), nl,
    member(stench, LAtoms),
    write('  update_knowledge: Stench detected. Asserting wumpus_possible for adjacent.'), nl,
    forall((adjacent(X, Y, AX, AY), is_valid_coord(AX, AY)),
           assert_if_new(wumpus_possible(AX, AY))).
handle_stench(_, _, _) :-
    write('  update_knowledge: No stench. Retracting wumpus_possible for adjacent.'), nl,
    interaction(K), % Need K to get current position
    position(X, Y, K),
    forall((adjacent(X, Y, AX, AY), is_valid_coord(AX, AY)),
           retractall(wumpus_possible(AX, AY))).

handle_breeze(X, Y, LAtoms) :-
    write('  update_knowledge: Checking for breeze percept.'), nl,
    member(breeze, LAtoms),
    write('  update_knowledge: Breeze detected. Asserting pit_possible for adjacent.'), nl,
    forall((adjacent(X, Y, AX, AY), is_valid_coord(AX, AY)),
           assert_if_new(pit_possible(AX, AY))).
handle_breeze(_, _, _) :-
    write('  update_knowledge: No breeze. Retracting pit_possible for adjacent.'), nl,
    interaction(K), % Need K to get current position
    position(X, Y, K),
    forall((adjacent(X, Y, AX, AY), is_valid_coord(AX, AY)),
           retractall(pit_possible(AX, AY))).

handle_no_stench_no_breeze(X, Y, LAtoms) :-
    write('  update_knowledge: Checking for no stench/breeze.'), nl,
    \+ member(stench, LAtoms),
    \+ member(breeze, LAtoms),
    write('  update_knowledge: No stench/breeze. Asserting adjacent as safe.'), nl,
    forall((adjacent(X, Y, AX, AY), is_valid_coord(AX, AY)),
           assert_if_new(safe(AX, AY))).
handle_no_stench_no_breeze(_, _, _). % Always succeed if the condition is false

handle_bump(X, Y, Dir, LAtoms) :-
    write('  update_knowledge: Checking for bump percept.'), nl,
    member(bump, LAtoms),
    write('  update_knowledge: Bump percept found. Asserting wall.'), nl,
    assert_if_new(wall(X, Y, Dir)).
handle_bump(_, _, _, _). % Always succeed if the condition is false

handle_scream(LAtoms) :-
    write('  update_knowledge: Checking for scream percept.'), nl,
    member(scream, LAtoms),
    write('  update_knowledge: Scream percept found. Wumpus is dead.'), nl,
    retractall(wumpus_alive),
    asserta(wumpus_dead),
    retractall(wumpus_possible(_, _)),
    retractall(wumpus_known(_, _)).
handle_scream(_). % Always succeed if the condition is false


% Registra o estado atual da base de conhecimento do agente para depuração.
log_next:-
    nl,write('Log_next'),nl,
    write('Knowledge Base'),nl,
    listing(interaction),
    listing(position),
    listing(orientation),
    listing(stench/3),
    listing(breeze/3),
    listing(glitter/3),
    listing(bump/3),
    listing(scream/3),
    listing(wall/3),
    listing(gold1/2),
    listing(act/2),
    % ADICIONADOS: Listar fatos do modelo interno
    listing(visited/2),
    listing(safe/2),
    listing(wumpus_possible/2),
    listing(pit_possible/2),
    listing(has_arrow/0),
    listing(wumpus_alive/0),
    listing(wumpus_dead/0),
    listing(holding_gold/0).

% --- Evolução do Mundo (Conhecimento do Agente sobre Mudanças Externas) ---

% Predicado para evoluir o estado do mundo na perspectiva do agente.
% Dispara regras para atualizar o conhecimento com base em eventos específicos.
evolution(K):-
    write('--- evolution: Starting ---'), nl,
    (evol(K) ; true), % Tenta evoluir, mas sempre succeed para não falhar evolution/1
    write('--- evolution: Finished ---'), nl. % Movido para dentro da primeira cláusula para garantir que sempre apareça

% Se 'glitter' foi percebido, asserta a localização do ouro.
evol(K):-
    write('  evol: Checking glitter...'), nl, % NEW DEBUG
    retract(glitter(_,_,K)), % Consome o percepto de glitter para evitar reprocessamento
    position(Lin,Col,K),
    asserta(glitter(Lin,Col,K)), % Re-asserta para K atual, se necessário para logging
    \+ holding_gold, % ALTERADO: not/1 para \+/1. Só asserta gold1 se ainda não estiver segurando o ouro
    \+ gold1(Lin,Col), % ALTERADO: not/1 para \+/1. Se o ouro ainda não é conhecido nesta localização
    asserta(gold1(Lin,Col)),
    write('  evol: Gold found and asserted.'), nl.

% Se 'bump' foi percebido, asserta uma parede na direção da colisão.
evol(K):-
    write('  evol: Checking bump...'), nl, % NEW DEBUG
    retract(bump(_,_,K)), % Consome o percepto de bump
    position(Lin,Col,K),
    asserta(bump(Lin,Col,K)), % Re-asserta para K atual, se necessário para logging
    orientation(Dir,K),
    \+ wall(Lin,Col,Dir), % ALTERADO: not/1 para \+/1. Se a parede ainda não é conhecida
    asserta(wall(Lin,Col,Dir)),
    write('  evol: Wall found and asserted.'), nl.

% ADICIONADO: Se 'scream' foi percebido, o Wumpus está morto.
evol(K):-
    write('  evol: Checking scream...'), nl, % NEW DEBUG
    retract(scream(_,_,K)), % Consome o percepto de scream
    position(Lin,Col,K), % Posição do agente quando ouviu o grito
    asserta(scream(Lin,Col,K)), % Re-asserta para K atual, se necessário para logging
    wumpus_alive, % Se o Wumpus estava vivo
    retractall(wumpus_alive),     % Wumpus não está mais vivo
    asserta(wumpus_dead),         % Wumpus está morto
    retractall(wumpus_possible(_, _)), % Não há mais locais possíveis para o Wumpus
    retractall(wumpus_known(_, _)), % Nem locais conhecidos
    write('  evol: Wumpus screamed and is dead.'), nl.

% --- Seleção de Ação ---

% Seleciona a próxima ação a ser executada pelo agente.
action(A):-
    write('--- action: Attempting to find action...'), nl, % DEBUG: Indica que a busca por ação começou
    do(A), % Tenta encontrar uma ação válida seguindo as regras de prioridade
    write(user_output, '--- action: Action selected: '), write(user_output, A), nl, % DEBUG: Ação selecionada
    interaction(K),
    asserta(act(A,K)), % Registra a ação escolhida para a interação atual
    log_action(A), % Registra a ação para depuração
    write('--- action: Finished ---'), nl.

% Define as regras de ação do agente, priorizadas da mais importante para a menos.

% 1. Pegar Ouro: Prioridade máxima se há brilho e o agente não está segurando o ouro.
do(grab) :-
    write('Trying grab...'), nl, % DEBUG
    interaction(K),
    glitter(_,_,K),        % Há brilho no quadrado atual
    \+ holding_gold,     % ALTERADO: not/1 para \+/1. Agente não está segurando o ouro
    asserta(holding_gold), % Agente agora está segurando o ouro
    !.                     % Corta para cometer esta ação (não busca outras)

% 2. Sair do Mundo: Se o agente está na posição inicial (1,1) e está segurando o ouro.
do(climb) :-
    write('Trying climb...'), nl, % DEBUG
    interaction(K),
    position(1, 1, K),     % Agente está na posição (1,1)
    holding_gold,          % Agente está segurando o ouro
    !.                     % Corta para cometer esta ação

% 3. Atirar no Wumpus: Se há fedor, o agente tem uma flecha e pode mirar em um possível Wumpus.
do(shoot) :-
    write('Trying shoot...'), nl, % DEBUG
    interaction(K),
    stench(_,_,K),         % Há fedor no quadrado atual
    has_arrow,             % Agente tem uma flecha
    wumpus_alive,          % Wumpus ainda está vivo
    position(X, Y, K),
    orientation(Dir, K),
    adjacent_square(X, Y, Dir, NX, NY), % Calcula o quadrado à frente do agente
    is_valid_coord(NX, NY),             % O quadrado à frente é válido
    wumpus_possible(NX, NY),            % Wumpus é possível no quadrado à frente
    retract(has_arrow),                 % Agente usa a flecha
    !.                                  % Corta para cometer esta ação

% 4. Ir para frente (Explorar): Mover para um quadrado seguro e não visitado.
do(goforward) :- % Primeira regra de goforward (para não visitados)
    write('Trying goforward (unvisited safe)...'), nl, % DEBUG
    interaction(K),
    position(X, Y, K),
    orientation(Dir, K),
    adjacent_square(X, Y, Dir, NX, NY), % Calcula o quadrado à frente
    is_valid_coord(NX, NY),             % O quadrado é válido
    safe(NX, NY),                       % O quadrado é seguro
    \+ visited(NX, NY),               % ALTERADO: not/1 para \+/1. O quadrado ainda não foi visitado
    !.                                  % Corta para cometer esta ação

% 5. Virar à Esquerda (Explorar): Virar para uma direção que leve a um quadrado seguro e não visitado.
do(turnleft) :- % Primeira regra de turnleft (para não visitados)
    write('Trying turnleft (to unvisited safe)...'), nl, % DEBUG
    interaction(K),
    position(X, Y, K),
    orientation(CurrentDir, K),
    % Determina a nova direção após virar à esquerda
    ( ( CurrentDir = east, NewDir = north ) ;
      ( CurrentDir = north, NewDir = west ) ;
      ( CurrentDir = west, NewDir = south ) ;
      ( CurrentDir = south, NewDir = east )
    ),
    % Verifica se o quadrado na NovaDireção é seguro e não visitado
    adjacent_square(X, Y, NewDir, NX, NY),
    is_valid_coord(NX, NY),
    safe(NX, NY),
    \+ visited(NX, NY),               % ALTERADO: not/1 para \+/1
    !. % Corta para cometer esta ação se uma rota for encontrada

% 6. Virar à Direita (Explorar): Virar para uma direção que leve a um quadrado seguro e não visitado.
do(turnright) :- % Primeira regra de turnright (para não visitados)
    write('Trying turnright (to unvisited safe)...'), nl, % DEBUG
    interaction(K),
    position(X, Y, K),
    orientation(CurrentDir, K),
    % Determina a nova direção após virar à direita
    ( ( CurrentDir = east, NewDir = south ) ;
      ( CurrentDir = north, NewDir = east ) ;
      ( CurrentDir = west, NewDir = north ) ;
      ( CurrentDir = south, NewDir = west )
    ),
    adjacent_square(X, Y, NewDir, NX, NY),
    is_valid_coord(NX, NY),
    safe(NX, NY),
    \+ visited(NX, NY),               % ALTERADO: not/1 para \+/1
    !. % Corta para cometer esta ação se uma rota for encontrada

% 7. Ir para frente (Retornar/Re-explorar): Se não há quadrados seguros e não visitados,
%    move para um quadrado seguro JÁ visitado para evitar ficar preso ou explorar outras rotas.
do(goforward) :- % Segunda regra de goforward (para visitados seguros)
    write('Trying goforward (visited safe)...'), nl, % DEBUG
    interaction(K),
    position(X, Y, K),
    orientation(Dir, K),
    adjacent_square(X, Y, Dir, NX, NY), % Calcula o quadrado à frente
    is_valid_coord(NX, NY),             % O quadrado é válido
    safe(NX, NY),                       % O quadrado é seguro
    visited(NX, NY),                    % O quadrado já foi visitado
    % Evita um retorno imediato (U-turn) para o quadrado de onde acabou de vir.
    % Esta parte pode ser o problema se o simulador não suportar K-1 ou se a lógica for falha
    interaction(PrevK), PrevK is K-1, % Pega a interação anterior
    position(PrevX, PrevY, PrevK),   % Pega a posição anterior
    \+ (NX = PrevX, NY = PrevY),   % ALTERADO: not/1 para \+/1. Garante que não está voltando para o quadrado anterior
    !.

% 8. Virar à Esquerda (Padrão/Desbloqueio): Se nenhuma das ações acima é aplicável,
%    o agente simplesmente vira à esquerda. Isso ajuda a explorar ou a sair de situações de "canto".
do(turnleft) :- % Última regra de turnleft (fallback)
    write('Trying turnleft (fallback)...'), nl. % DEBUG: Não tem corte, deveria sempre ser satisfeita

% Registra a ação escolhida para depuração.
log_action(A):-
    write('Log_action'),nl,
    write('Action = '),write(A),nl.

% --- Predicados Auxiliares ---

% Verifica se as coordenadas (X, Y) estão dentro dos limites do mundo.
% Agora consulta a extensão do mundo do simulador.
is_valid_coord(X, Y) :-
    wumpus_world_extent(Extent), % Consulta o predicado do simulador
    X >= 1, X =< Extent,
    Y >= 1, Y =< Extent.

% Gera todas as 4 coordenadas de quadrados adjacentes a (X, Y).
% Usado para inferir possíveis locais de perigos (Wumpus/Poço).
adjacent(X, Y, AX, AY) :-
    ( AX is X+1, AY is Y
    ; AX is X-1, AY is Y
    ; AX is X, AY is Y+1
    ; AX is X, AY is Y-1
    ).

% Calcula as coordenadas do quadrado diretamente à frente do agente, dada sua orientação.
adjacent_square(X, Y, east, NX, NY) :- NX is X, NY is Y+1.
adjacent_square(X, Y, north, NX, NY) :- NX is X+1, NY is Y.
adjacent_square(X, Y, west, NX, NY) :- NX is X, NY is Y-1.
adjacent_square(X, Y, south, NX, NY) :- NX is X-1, NY is Y.

% Predicado auxiliar para asserir um fato apenas se ele ainda não existe na base de conhecimento.
assert_if_new(Fact) :-
    call(Fact), !. % Se o Fato já existe, succeed e corta (não faz nada)
assert_if_new(Fact) :-
    asserta(Fact). % Caso contrário, asserte o Fato
