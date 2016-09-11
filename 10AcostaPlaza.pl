%10 Acosta Plaza
%PROGRAMA GENERAL

consulta:- write('Elige el idioma de entrada'), nl, 
 read(I),
 write('Escribe frase entre corchetes separando palabras con comas '), nl, 
 write('o lista vacía para parar '), nl, 
 read(F), 
 trata(I,F). 
 
trata(_,[]):- write('final'). 
% tratamiento final 
 
trata(I,F):- traducir(Salida,I, F, []), write(Salida),nl, consulta. 
% tratamiento caso general 

traducir(Traduccion, ingles)--> g_nom(_,G,Num,TraducGn),
								g_verb(_,G, Num, TraducGv),
								{
								  append(TraducGn,TraducGv,Traduccion)
								}.
traducir(Traduccion, ingles)--> g_nom(_,G,Num,TraducGn),
								g_verb(_,G, Num, TraducGv),
								g_prep(_,_, TraducGP),
								{
								  append(TraducGn,TraducGv,Traduc),
								  append(Traduc, TraducGP, Traduccion)
								}.
traducir(Traduccion, ingles)--> g_nom(_,G,Num,TraducGn),
								g_verb(_,G, Num, TraducGv),
								g_prep(_,_, TraducGP),
								g_prep(_,_,TraGPr),
								{
								  append(TraducGn,TraducGv,Traduc),
								  append(Traduc, TraducGP, Traduc2),
								  append(Traduc2, TraGPr, Traduccion)
								}.

traducir(Traduccion, español)--> g_nom_esp(_,_,_,Traduccion).
traducir(Traduccion, español)--> g_nom_esp(_,G,Num,TraducGn),
								g_verb_esp(_,G, Num, TraducGv),
								{
								  append(TraducGn,TraducGv,Traduccion)
								}.
traducir(Traduccion, español)--> g_nom_esp(_,G,Num,TraducGn),
								g_verb_esp(_,G, Num, TraducGv),
								g_prep_esp(_,_, TraducGP),
								{
								  append(TraducGn,TraducGv,Traduc),
								  append(Traduc, TraducGP, Traduccion)
								}.
traducir(Traduccion, español)--> g_nom_esp(_,G,Num,TraducGn),
								g_verb_esp(_,G, Num, TraducGv),
								g_prep_esp(_,_, TraducGP),
								g_prep_esp(_,_,TraGPr),
								{
								  append(TraducGn,TraducGv,Traduc),
								  append(Traduc, TraducGP, Traduc2),
								  append(Traduc2, TraGPr, Traduccion)
								}.
g_nom(gn(Nombre),G, Num, [Traduc])--> names(Nombre,G,Num, Traduc).
g_nom(gn(Art,Nombre),G, Num, Traduccion)--> article(Art,G,Num,Tr),
								names(Nombre,G,Num,Traduc),
								{
								  append([Tr],[Traduc],Traduccion)
								}.
g_nom(gn(Art,Adj,Nombre),G,Num,Traduccion)-->
								article(Art,G,Num,TrArt),
								adjective(Adj,G,Num,TrAdj),
								names(Nombre,G,Num,TrNam),
								{
								  append([TrArt],[TrNam],TrAux2),
								  append(TrAux2,[TrAdj],Traduccion)
								}.

g_verb(gv(Av,V),_,Num, Traduccion)-->
								auxVerb(Av, Num, TrAv),
								verb(V,TrV),
								{
								  append([TrAv],[TrV], Traduccion)
								}.
g_verb(gv(Av,V,CD),_,Num, Traduccion)-->
								auxVerb(Av, Num, TrAv),
								verb(V,TrV),
								complement(CD,_,_,TrGN),
								{
								  append([TrAv],[TrV], TraducAux),
								  append(TraducAux, TrGN, Traduccion)
								}.
g_verb(gv(Av,V,CD),_,Num, Traduccion)-->
								auxVerb(Av, Num, TrAv),
								verb(V,TrV),
								g_nom(CD,_,_,TrGN),
								{
								  append([TrAv],[TrV], TraducAux),
								  append(TraducAux, TrGN, Traduccion)
								}.
g_verb(gv(Av,V,C),_,Num,Traduccion)-->
								auxVerb(Av, Num, TrAv),
								verb_intransitivo(V,TrV),
								g_prep(C,_,TrGN),
								{
								  append([TrAv],[TrV], TraducAux),
								  append(TraducAux, TrGN, Traduccion)
								}.
g_verb(gv(Av,V,C),_,Num,Traduccion)-->
								auxVerb(Av,Num, TrAv),
								verb_copulativo(V,TrV),
								g_nom(C,_,_,TrGN),
								{
								  append([TrAv],[TrV], TraducAux),
								  append(TraducAux, TrGN, Traduccion)
								}.

g_verb(gv(Av,V,C),G,Num,Traduccion)-->
								auxVerb(Av, Num, TrAv),
								verb_copulativo(V,TrV),
								complement(C,G,Num,TrGN),
								{
								  append([TrAv],[TrV], TraducAux),
								  append(TraducAux, TrGN, Traduccion)
								}.

g_prep(gp(P,N),Num,Traduccion)-->
								preposition(P,Tr),
								g_nom(N,_,Num,TrGN),
								{
								  append([Tr],TrGN,Traduccion)
								}.
complement(c(Adj,Nombre),_,Num,Traduccion)-->
								adjective(Adj,G,Num,TrAdj),
								names(Nombre,G,Num,TrNam),
								{
								  append([TrNam],[TrAdj],Traduccion)
								}.
complement(c(Adj),G,Num,[TrAdj])--> adjective(Adj,G,Num,TrAdj).

g_nom_esp(gn(Nombre),_, Num, [Traduc])--> nombres(Nombre,Num,_, Traduc).
g_nom_esp(gn(Art,Nombre),G, Num, Traduccion)--> articulo(Art,Num,G,TrArt),
								nombres(Nombre,Num,G,Traduc),
								{
								  append([TrArt],[Traduc],Traduccion)
								}.
g_nom_esp(gn(Art,Adj,Nombre),G,Num,Traduccion)-->
								articulo(Art,Num,G,TrArt),
								nombres(Nombre,Num,G,TrNam),
								adjetivo(Adj,Num,G,TrAdj),
								{
								  append([TrArt],[TrAdj], TrAux2),
								  append(TrAux2,[TrNam],Traduccion)
								}.

g_verb_esp(gv(Av,V),_,Num, Traduccion)-->
								auxVerb_esp(Av, Num, TrAv),
								verbo(V,TrV),
								{
								  append([TrAv],[TrV], Traduccion)
								}.

g_verb_esp(gv(Av,V,CD),_,Num, Traduccion)-->
								auxVerb_esp(Av, Num, TrAv),
								verbo(V,TrV),
								complemento(CD,_,_,TrGN),
								{
								  append([TrAv],[TrV], TraducAux),
								  append(TraducAux, TrGN, Traduccion)
								}.

g_verb_esp(gv(Av,V,CD),_,Num, Traduccion)-->
								auxVerb_esp(Av, Num, TrAv),
								verbo(V,TrV),
								g_nom_esp(CD,_,_,TrGN),
								{
								  append([TrAv],[TrV], TraducAux),
								  append(TraducAux, TrGN, Traduccion)
								}.

g_verb_esp(gv(Av,V,C),_,Num,Traduccion)-->
								auxVerb_esp(Av, Num, TrAv),
								verbo_intransitivo(V,TrV),
								g_prep_esp(C,_,TrGN),
								{
								  append([TrAv],[TrV], TraducAux),
								  append(TraducAux, TrGN, Traduccion)
								}.

g_verb_esp(gv(Av,V,C),_,Num,Traduccion)-->
								auxVerb_esp(Av, Num, TrAv),
								verbo_copulativo(V,TrV),
								g_nom_esp(C,_,_,TrGN),
								{
								  append([TrAv],[TrV], TraducAux),
								  append(TraducAux, TrGN, Traduccion)
								}.

g_verb_esp(gv(Av,V,C),G,Num,Traduccion)-->
								auxVerb_esp(Av, Num, TrAv),
								verbo_copulativo(V,TrV),
								complemento(C,G,Num,TrGN),
								{
								  append([TrAv],[TrV], TraducAux),
								  append(TraducAux, TrGN, Traduccion)
								}.

g_prep_esp(gp(P,N),Num,Traduccion)-->
								preposicion(P,Tr),
								g_nom_esp(N,_,Num,TrGN),
								{
								  append([Tr],TrGN,Traduccion)
								}.

complemento(c(Adj,Nombre),_,_,Traduccion)-->
								nombres(Nombre,Num,G,Tx),
								adjetivo(Adj,Num, G,TrAdj),
								{
								  append([TrAdj],[Tx],Traduccion)
								}.
complemento(c(Adj),G,Num,[TrAdj])--> adjetivo(Adj,Num,G,TrAdj).

%ELEMENTOS EN INGLES
article(a(A), G,Number, Traduccion)-->
			[A],
			{
			  es_articulo(Traduccion, G,Number, A)
			}.


adjective(ad(Ad),G,Num,Traduccion)-->
			[Ad],
			{
			  is_adjective(Ad,TrAdj),
			  terminacion_adjetivo(TAd,G,Num),
			  name(TrAdj,Aux1),
			  name(TAd,Aux2),
			  append(Aux1, Aux2, TrAux1),
			  name(Traduccion,TrAux1)
			}.


names(n(N),G,Number,Traduccion)-->
			[N],
			{
			  name(N, NombreEntero),
			  append(Nombre, Terminacion, NombreEntero),
			  name(Nom, Nombre),
			  es_nombre(Traduc,G,Nom),
			  name(Ter, Terminacion),
			  end_name(Ter, Number),
			  name(Traduc, TRaux),
			  name(Ter, TEaux),
			  append(TRaux, TEaux, Tra),
			  name(Traduccion, Tra)
			}.


auxVerb(av(Av),Number,Traduccion)-->
			[Av],
			{
			  is_conjug(Av, Number,Traduccion)
			}.

verb(v(V),T)-->
	[V],
	{
	  verb_conj(V,T)
	}.

verb_intransitivo(v(V),T)-->
	[V],
	{
	  verb_conj_intrans(V,T)
	}.

verb_copulativo(v(V),T)-->
	[V],
	{
	  verb_conj_cop(V,T)
	}.

preposition(p(P),Traduccion)-->
			[P],
			{
			  is_preposition(P, Traduccion)
			}.

%ELEMENTOS EN ESPAÑOL
articulo(art(A), Number, Genero, Traduccion)-->
			[A],
			{
			  es_articulo(A, Genero,Number,Traduccion)
			}.
adjetivo(ad(Ad),Num,G,Traduccion)-->
			[Ad],
			{
			  name(Ad, Adje),
			  append(Adject, Ter, Adje),
			  name(Adjetivo, Adject),
			  name(Terminacion, Ter),
			  terminacion_adjetivo(Terminacion, G, Num),
			  is_adjective(Traduccion, Adjetivo)
			}.


nombres(nom(N),Number,Genero,Traduccion)-->
			[N],
			{
			  name(N, NombreEntero),
			  append(Nombre, Terminacion, NombreEntero),
			  name(Nom, Nombre),
			  es_nombre(Nom, Genero, Traduc),
			  name(Ter, Terminacion),
			  end_name(Ter, Number),
			  name(Traduc, TRaux),
			  name(Ter, TEaux),
			  append(TRaux, TEaux, Tra),
			  name(Traduccion, Tra)
			}.

auxVerb_esp(av(Av),Number,Traduccion)-->
			[Av],
			{
			  is_conjug(Traduccion, Number, Av)
			}.

verbo(v(V),T)-->
	[V],
	{
	  verb_conj(T,V)
	  %es_verbo(V,T)
	}.

preposicion(p(P),Traduccion)-->
			[P],
			{
			  is_preposition(Traduccion, P)
			}.

verbo_intransitivo(v(V),T)-->
	[V],
	{
	  verb_conj_intrans(T,V)
	}.

verbo_copulativo(v(V),T)-->
	[V],
	{
	  verb_conj_cop(T,V)
	}.

%DICCIONARIO INGLES

is_conjug(has, singular, ha).
is_conjug(have, plural, han).

verb_conj(eaten, comido).
verb_conj(drunk, bebido).
verb_conj_intrans(been, estado).
verb_conj_cop(been, sido).

is_adjective(little, pequeñ).
is_adjective(fresh, fresc).
is_adjective(red, roj).
is_adjective(famous, famos).

end_name('s',plural).
end_name('',singular).

is_preposition(in, en).
is_preposition('for', durante).

es_articulo(el, masculino, singular, the).
es_articulo(la, femenino, singular, the).
es_articulo(los, masculino, plural,the).
es_articulo(las, femenino, plural,the).
es_articulo(un, masculino, singular, a).
es_articulo(una, femenino, singular,a).

es_adjetivo(roj, red).
es_adjetivo(fresc, fresh).
es_adjetivo(pequeñ, little).
es_adjetivo(famos, famous).

es_nombre(juan, masculino, juan).
es_nombre(madrid, masculino, madrid).
es_nombre(niño, masculino, boy).
es_nombre(niña, femenino, girl).
es_nombre(manzana, femenino, apple).
es_nombre(batido, masculino, milkshake).
es_nombre(cuadro, masculino, painting).
es_nombre(semana, femenino, week).
es_nombre(año, masculino, year).

terminacion_adjetivo(o, masculino, singular).
terminacion_adjetivo(a, femenino, singular).
terminacion_adjetivo(os, masculino, plural).
terminacion_adjetivo(as, femenino, plural).
