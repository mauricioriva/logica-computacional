curso(selene,7013).
curso(odin,7017).
curso(liliana,7019).
curso(noe,7037).
curso(karla,7059).
curso(susana,7061).
estudiante(itzel,7059).
estudiante(dafne,7061).
estudiante(alexis,7017).
estudiante(ailyn,7037).
estudiante(luis,7017).
estudiante(izumi,7059).
estudiante(gibran,7059).
estudiante(diego,7059).
estudiante(javier,7037).
estudiante(claudia,7013).
estudiante(david,7019).

estudiante_de(E,P) :- curso(P1,X), estudiante(E1,X), (E=E1, P1=P, X=X).
