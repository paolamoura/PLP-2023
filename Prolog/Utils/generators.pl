:- module(generators, [
    generate_dates/2,
    populate_list/1
    ]).

:- use_module(library(date)).

% Obtém a data e hora atuais
get_current_datetime(DateTime) :-
    get_time(Stamp),
    stamp_date_time(Stamp, DateTime, 'local').

% Adiciona um número de dias a uma data
add_days(DateTime, DaysToAdd, ResultDateTime) :-
    date_time_stamp(DateTime, Stamp),
    NewStamp is Stamp + DaysToAdd * 86400,  % 86400 segundos em um dia
    stamp_date_time(NewStamp, ResultDateTime, 'local').

% Cria uma lista no formato [data, horario]
create_entry(DateTime, Horario, Entry) :-
    format_time(atom(DateAtom), '%Y-%m-%d', DateTime),
    Entry = [DateAtom, Horario].

% Povoar a lista com entradas para o dia atual e os próximos 7 dias
populate_list(List) :-
    get_current_datetime(CurrentDateTime),
    populate_list_aux(CurrentDateTime, 0, List).

populate_list_aux(_, 8, []). % Parar após 8 dias
populate_list_aux(CurrentDateTime, DayOffset, [Entry | Rest]) :-
    add_days(CurrentDateTime, DayOffset, TargetDateTime),
    Horarios = ['8 horas', '9 horas', '14 horas', '15 horas'],
    member(Horario, Horarios),
    create_entry(TargetDateTime, Horario, Entry),
    NextDayOffset is DayOffset + 1,
    populate_list_aux(CurrentDateTime, NextDayOffset, Rest).

% Exemplo de uso
% Chame populate_list(List) para obter a lista desejada.

% Exemplo de consulta para exibir a lista gerada:
% ?- populate_list(List), writeln(List).

generate_dates(N, Dates) :-
    get_time(Now),
    generate_dates(N, Now, Dates).

    generate_dates(0, _, []).
generate_dates(N, CurrentTime, [DateStr | RestDates]) :-
    N > 0,
    stamp_date_time(CurrentTime, DateTime, 'UTC'),
    date_time_value(date, DateTime, Date),
    format_time(atom(DateStr), '%d-%m-%Y', Date),
    NextDay is 86400,
    NextTime is CurrentTime + NextDay,
    NextN is N - 1,
    generate_dates(NextN, NextTime, RestDates).