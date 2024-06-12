
# Yahtzee en Prolog
## Laboratorio Programación Lógica 2023
## UdelaR
### Versión 1.0

Este laboratorio permite jugar al juego de dados Yahtzee. Se hace uso de predicados implementados en Prolog que representan la lógica del juego, la IA que puede tomar decisiones basadas en el estado actual del juego y la IA Prob que se basa en Problog para tomar decisiones.

# Instalación
- Asegurarse de tener instalado Python (3.9+).
- Instalar la biblioteca de Problog ejecutando `pip install problog` (en Windows instalar con derechos de administrador para que quede en el `PATH`)
- Verificar que SWI Prolog esté instalado y que su ejecutable esté en el Path del sistema.

# Reglas del juego

Yahtzee es un juego de dados donde el objetivo es obtener la mayor cantidad de puntos completando ciertas combinaciones de dados. Cada jugador tiene tres tiradas por turno para intentar obtener una de las combinaciones. Después de cada tirada, el jugador puede decidir cuáles dados guardar y cuáles volver a lanzar. Al final del turno, el jugador debe elegir una combinación y registrar su puntuación para esa ronda.

El juego continúa hasta que todas las combinaciones hayan sido usadas. El jugador con la mayor cantidad de puntos al final del juego gana.

Una implementación de referencia está disponible en [React Yahtzee](https://react-yahtzee.netlify.app/).

El juego de Yahtzee en Prolog incluye los siguientes componentes principales:

- **Motor de Juego**: Implementado en Prolog, maneja la lógica del juego, incluyendo el lanzamiento de dados, la selección de categorías y el cálculo de puntuaciones.
- **Generador de Números Aleatorios**: Utilizado para simular los lanzamientos de dados.
- **IA para Jugar Yahtzee**: Un conjunto de estrategias implementadas en Prolog que deciden los movimientos a realizar.
- **Gestión del Estado del Juego**: Predicados Prolog que mantienen el estado actual del juego, incluyendo el tablero de puntuación y los dados.
- **Puente Prolog-Problog**: Si se utilizan funcionalidades de Problog, este componente permite la comunicación entre el código Prolog y Problog.

# Instrucciones de Uso

## Ejecución del Juego

### Ejecución del Juego para un Jugador Humano

Para jugar como un humano, ejecute el predicado `yahtzee(humano, Seed)` en SWI Prolog:

```prolog
?- yahtzee(humano, 12345).
```

Donde `12345` es la semilla para el generador de números aleatorios.

Durante el juego, se le mostrará el estado actual del tablero y los dados. Deberá indicar cuáles dados desea volver a lanzar ingresando 1 para volver a lanzar o 0 para mantener el dado. El juego continuará hasta que todas las categorías estén llenas.

Ejemplo de Interacción:

```prolog
Este es el tablero actual:
[s(aces,nil), s(twos,nil), s(threes,nil)]
Estos son tus dados:
[1,2,3,4,5]
Indica si quieres volver a tirar el dado (1 si, 0 no): [0,1,0,1,1]
```
Ingrese 1 o 0 para cada dado, y el juego procederá según sus elecciones hasta que todas las categorías estén llenas.

### Ejecución del Juego para una Estrategia AI Determinista

Para jugar con la estrategia AI determinista, ejecute el predicado `yahtzeelog(ia_det, Seed)` en SWI Prolog:

```prolog
?- yahtzeelog(ia_det, 12345).
```

Donde `12345` es la semilla para el generador de números aleatorios.

### Ejecución del Juego para una Estrategia AI Probabilística

Para jugar con la estrategia AI probabilística utilizando Problog, ejecute el predicado `yahtzeelog(ia_prob, Seed)` en SWI Prolog:

```prolog
?- yahtzeelog(ia_prob, 12345).
```

Donde `12345` es la semilla para el generador de números aleatorios.

# Archivos

- `categorias.pl`: Definiciones de categorías superiores y verificaciones de combinaciones.
- `puntaje.pl`: Cálculo de puntajes para diferentes combinaciones de dados y categorías.
- `tablero.pl`: Manejo del estado del tablero y ajuste de puntuaciones.
- `humano.pl`: Estrategia para permitir la entrada del usuario.
- `ia_det.pl`: Estrategia AI determinista para decidir movimientos.
- `modelo_problog.pl`: Estrategia AI probabilística utilizando Problog para calcular el mejor patrón de dados.
