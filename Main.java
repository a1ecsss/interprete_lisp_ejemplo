import java.io.*;
import java.util.*;

//"C:\Program Files\Java\jdk-22\bin\java" Main
public class Main {
    public static void main(String[] args) {
        String rutaArchivo = "codigo.lisp";
        List<Object> codigo = null; // Iniciar con null para que se pueda verificar si se llenó correctamente.

        try {
            // Intentamos obtener el código desde el archivo
            codigo = Lector.getCodigo(rutaArchivo);
        } catch (Exception e) {
            // Si ocurre un error al leer el archivo, lo imprimimos
            System.err.println("Error al leer el archivo: " + e.getMessage());
        }

        // Verificamos si la lista 'codigo' es válida (no es null ni vacía)
        if (codigo != null && !codigo.isEmpty()) {
            try {
                // Si todo está bien, creamos el entorno y ejecutamos el código
                Environment MainEnviroment = new Environment(codigo, null, null);
                MainEnviroment.ejecutarCodigo();
            } catch (Exception e) {
                // Si ocurre un error al ejecutar el código, lo imprimimos
                System.err.println(e.getMessage());
            }
        } else {
            // Si 'codigo' es null o vacío, lanzamos un error personalizado.
            System.err.println("El archivo no contiene código o no se pudo leer correctamente.");
        }
    }

}
