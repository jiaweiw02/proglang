
:- [hogwarts].

studentOf(Student, Teacher) :-
    teacherOf(Teacher, Student).

% rules
classmates(StudentOne, StudentTwo) :- 
    studentOf(StudentOne, Teacher),
    studentOf(StudentTwo, Teacher),
    StudentOne \= StudentTwo.

liveFarAway(StudentOne, StudentTwo) :-
    houseOf(HouseOne, StudentOne),
    houseOf(HouseTwo, StudentTwo),
    farLocation(HouseOne, HouseTwo),
    HouseOne \= HouseTwo,
    StudentOne \= StudentTwo.

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

listSeniors(Person, Seniors) :-
    findall(Senior, isSeniorOf(Senior, Person), Seniors).

listJuniors(Person, Juniors) :-
    findall(Junior, isSeniorOf(Person, Junior), Juniors).

oldestStudent(Person, House) :-
    houseOf(House, Person),
    birthYear(Person, BirthYear),
    \+ (houseOf(House, Person2), birthYear(Person2, BirthYear2), BirthYear2 < BirthYear).

youngestStudent(Person, House) :-
    houseOf(House, Person),
    birthYear(Person, BirthYear),
    \+ (houseOf(House, Person2), birthYear(Person2, BirthYear2), BirthYear2 > BirthYear).


oldestQuidditchStudent(Team, Student) :-
    quidditchTeamOf(Team, Student),
    birthYear(Student, BirthYear),
    \+ (quidditchTeamOf(Team, Student2), birthYear(Student2, BirthYear2), BirthYear2 < BirthYear).

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
    farLocation(House1, House2),
    House1 \= House2,
    StudentOne \= StudentTwo.
