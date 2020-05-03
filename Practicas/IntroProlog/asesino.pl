vive_mansion(tiaAlicia).
vive_mansion(bartolo).
vive_mansion(carnicero).

odia(tiaAlicia,bartolo).
odia(carnicero,bartolo).
odia(bartolo,tiaAlicia).
odia(bartolo,carnicero).

mas_rico(carnicero,tiaAlicia).
mas_rico(carnicero,bartolo).
mas_rico(tiaAlicia,bartolo).

asesino(A,P) :- odia(A,P), mas_rico(P,A), vive_mansion(A).
