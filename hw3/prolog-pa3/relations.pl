
:- [hogwarts].

studentOf(Student, Teacher) :-
    teacherOf(Teacher, Student).

% rules
classmates(StudentOne, StudentTwo) :- 
    teacherOf(Teacher, StudentOne),
    teacherOf(Teacher, StudentTwo),
    StudentOne \= StudentTwo.

liveFarAway(StudentOne, StudentTwo) :-
    houseOf(HouseOne, StudentOne),
    houseOf(HouseTwo, StudentTwo),
    farLocation(HouseOne, HouseTwo),
    HouseOne \= HouseTwo,
    StudentOne \= StudentTwo.

isSeniorOf(PersonA, PersonB) :-
    (
        directSeniorOf(PersonA, PersonB)
        % PersonA \= PersonB
    ) ;
    (
        directSeniorOf(PersonA, PersonC),
        isSeniorOf(PersonC, PersonB)
        % PersonA \= PersonB,
        % PersonA \= PersonC,
        % PersonB \= PersonC.
    ).

listSeniors(Person, Seniors) :-
    findall(Senior, isSeniorOf(Senior, Person), Seniors).

listJuniors(Person, Juniors) :-
    findall(Junior, isSeniorOf(Person, Junior), Juniors).

oldestStudent(Person, House) :-
    houseOf(House, Person),
    birthYear(Person, Year),
    \+ (houseOf(House, Person2), birthYear(Person2, Year2), Year2 < Year).

youngestStudent(Person, House) :-
    houseOf(House, Person),
    birthYear(Person, Year),
    \+ (houseOf(House, Person2), birthYear(Person2, Year2), Year2 > Year).


oldestQuidditchStudent(Team, Student) :-
    quidditchTeamOf(Team, Student),
    birthYear(Student, Year),
    \+ (quidditchTeamOf(Team, Student2), birthYear(Student2, Year2), Year2 < Year).

youngestQuidditchStudent(Team, Student) :-
    quidditchTeamOf(Team, Student),
    birthYear(Student, Year),
    \+ (quidditchTeamOf(Team, Student2), birthYear(Student2, Year2), Year2 > Year).

rival(StudentOne, StudentTwo) :-
    houseOf(House1, StudentOne),
    houseOf(House2, StudentTwo),
    House1 \= House2,
    StudentOne \= StudentTwo.

farRival(StudentOne, StudentTwo) :-
    houseOf(House1, StudentOne),
    houseOf(House2, StudentTwo),
    House1 \= House2,
    StudentOne \= StudentTwo,
    farLocation(House1, House2).
