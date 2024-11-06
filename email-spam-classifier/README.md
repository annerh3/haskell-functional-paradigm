# Email Spam Classifier

Este proyecto es una aplicación escrita en Haskell que clasifica correos electrónicos como spam o no spam mediante una heurística sencilla que analiza el contenido y el remitente cada correo.

## Requisitos

1. **Haskell**: Este proyecto requiere que Haskell esté instalado en tu máquina. Puedes descargarlo desde [GHCup](https://www.haskell.org/ghcup/).

2. **Cabal**: La herramienta de construcción Cabal se utiliza para administrar dependencias y construir el proyecto. Cabal suele venir con la instalación de Haskell, pero asegúrate de que esté actualizada ejecutando el siguiente comando:
      ```bash
      cabal update
      ```
   Puedes encontrar más información en [Cabal Docs](https://cabal.readthedocs.io/en/stable/getting-started.html#installing-cabal).

3. **Dependencias del Proyecto**: Este proyecto utiliza las siguientes librerías:

   

   `aeson`: Para manipulación de JSON.

   `bytestring`: Para manejar datos binarios.

   Para instalar estas dependencias, usa el siguiente comando:
   ```bash
   cabal install aeson bytestring
   ```
   


## Instalación


1. **Clona el repositorio:**: 

```bash
   git clone https://github.com/tuusuario/EmailSpamClassifier.git
```
2. **Compila el proyecto:**:  

```bash
   cabal build
```

2. **Ejecuta el proyecto:**:  

```bash
   cabal run
```
## Estudiantes

- **Hector Vega** 
  - GitHub: [TETvega](https://github.com/TETvega)

- **Anner Henriquez** 
  - GitHub: [annerh3](https://github.com/annerh3)



