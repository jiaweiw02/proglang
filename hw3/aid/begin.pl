directSeniorOf(Senior, Junior). % Senior spends more time in Hogwarts than Juniors

isSeniorOf(PersonA, PersonB) :-
    (
        directSeniorOf(PersonA, PersonB),
        PersonA \= PersonB
    ) ;
    (
        directSeniorOf(PersonA, PersonC),
        isSeniorOf(PersonC, PersonB),
        PersonA \= PersonB,
        PersonA \= PersonC,
        PersonB \= PersonC
    ).