# Copyright (C) 2007, Jordi Gutiérrez Hermoso

# This is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3, or (at your option) any
# later version.

# This is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/


## Algunas variables útiles
CPP = g++ #El compilador
LINKING = -lX11 -lpthread #Enlazar con X11 y con pthreads
CFLAGS = -O3
OPTIONS = -Wall -Wfatal-errors -Werror

## Cambien el nombre de los archivos objetos que tengan que generar...
OBJECTFILES = main.o newton.o utils.o
## El de las cabeceras...
HEADERFILES = newton.hpp utils.hpp
## Y el del binario.
BINARY = newton

## Esto dice que las terminaciones .cpp y .o tendrán una regla especial.
.SUFFIXES : .cpp .o

## Esta es una regla genérica que dice cómo generar un .o (objeto) de
## un .cpp (código fuente de C++)
.cpp.o :
	$(CPP) $(CFLAGS) $(OPTIONS) -c $< 

## Una regla para generar el binario final, que depende de que estén
## todos los archivos objetos ya compilados
$(BINARY): $(OBJECTFILES) 
	$(CPP) -o $(BINARY) $(OBJECTFILES)  $(LINKING)


## Ejecutar el binario. Nótese que depende de que el binario ya esté
## hecho
run: $(BINARY)
	  ./$(BINARY)

## No toquen nada más, sólo esto que sigue , que define las
## dependencias entre sus código fuente y el objeto código que debe
## definir. Recuerden que cada objeto código es una compilada
## separada.

main.o: main.cpp $(HEADERFILES) #Una compilada
newton.o: newton.cpp newton.hpp
utils.o: utils.cpp utils.hpp

clean:
	rm -f *.o $(BINARY)
