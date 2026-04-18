Comandos para correrlo:
erl -sname master -setcookie micookie
c(prueba).
% Esperar a que nodes() esté vacío
erl -sname nodo1 -setcookie micookie
net_adm:ping('master@TU_HOSTNAME').
erl -sname nodo2 -setcookie micookie
net_adm:ping('master@TU_HOSTNAME').
nodes(). % debe mostrar ['nodo1@...', 'nodo2@...']
nl(prueba).
prueba:start(8000).
racket parte-erlang.rkt
