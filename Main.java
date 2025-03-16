import java.io.*;
import java.util.*;

//"C:\Program Files\Java\jdk-22\bin\java" Main
public class Main {
    public static void main(String[] args) {
        String rutaArchivo = "codigo.lisp";

        try {
            List<Object> codigo = Lector.getCodigo(rutaArchivo);
            //System.out.println("Lista anidada generada: " + codigo);
            Environment MainEnviroment = new Environment(codigo, "Main", null);
            try{
                MainEnviroment.ejecutarCodigo();
            }catch (Exception e){
                System.err.println(e.getMessage());
            }
        } catch (IOException e) {
            System.err.println("Error al leer el archivo: " + e.getMessage());
        } catch (IllegalArgumentException e) {
            System.err.println(e.getMessage());
        }
    }
}
