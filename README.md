# BusCEMinas

Una implementación del clásico juego Buscaminas desarrollado en Racket usando programación funcional pura.

## Descripción

BusCEMinas es una versión funcional del popular juego de lógica Buscaminas, implementado como parte de la Tarea #2 del curso Paradigmas de Programación (CE1106) del Instituto Tecnológico de Costa Rica.

## Características

- **Tableros configurables**: Tamaños desde 8x8 hasta 15x15 celdas
- **Tres niveles de dificultad**: Fácil (10%), Medio (15%), Difícil (20%)
- **Sistema de modos**: Alterna entre modo descubrir y modo bandera
- **Descubrimiento automático**: Expansión en cascada para celdas vacías
- **Contador dinámico**: Seguimiento de minas restantes en tiempo real
- **Interfaz gráfica**: Diseño intuitivo usando Racket GUI

## Estructura del Proyecto

```
BusCEMinas/
├── interface/
│   ├── main-window.rkt      # Ventana principal y navegación
│   ├── config-screen.rkt    # Pantalla de configuración
│   └── game-screen.rkt      # Interfaz del juego
├── logic/
│   └── board-generator.rkt  # Lógica del tablero y algoritmos
└── README.md
```

## Instalación

1. Asegúrate de tener Racket instalado en tu sistema
2. Clona este repositorio:
   ```bash
   git clone https://github.com/usuario/BusCEMinas.git
   cd BusCEMinas
   ```

## Ejecución

Ejecuta el archivo principal desde la línea de comandos:

```bash
racket interface/main-window.rkt
```

O abre el archivo `interface/main-window.rkt` en DrRacket y presiona "Run".

## Cómo Jugar

1. **Configuración**: Selecciona el tamaño del tablero (filas y columnas) y nivel de dificultad
2. **Modos de juego**:
   - **Modo Descubrir**: Revela el contenido de las celdas
   - **Modo Bandera**: Marca celdas sospechosas con banderas
3. **Objetivo**: Descubre todas las celdas que no contienen minas
4. **Contador**: Observa las minas restantes basándose en las banderas colocadas

## Tecnologías

- **Lenguaje**: Racket
- **Paradigma**: Programación funcional pura
- **Interfaz**: Racket GUI Toolkit
- **Estructuras**: Listas inmutables

## Desarrollo

**Autores:**
- Steven Aguilar Alvarez
- Allan Zheng Tang  
- Javier Mora Masis

**Institución:** Instituto Tecnológico de Costa Rica  
**Escuela:** Ingeniería en Computadores  
**Curso:** CE1106 - Paradigmas de Programación  
**Semestre:** II-2025

## Documentación Adicional

Para información detallada sobre el uso del sistema, consulte el Manual de Usuario incluido en la carpeta `docs/`.
