import java.io.*;
import java.util.*;

public class Lector {
    private static List<Object> codigo = new ArrayList<>();

    public static List<Object> getCodigo(String root) throws IOException {
        if (!root.endsWith(".txt") && !root.endsWith(".lisp")) {
            throw new IllegalArgumentException("Error: Solo se permiten archivos .txt o .lisp");
        }

        String contenido = leerArchivo(root);
        codigo = parsearCodigo(contenido);
        return codigo;
    }

    private static String leerArchivo(String root) throws IOException {
        StringBuilder contenido = new StringBuilder();
        try (BufferedReader br = new BufferedReader(new FileReader(root))) {
            String linea;
            while ((linea = br.readLine()) != null) {
                int indiceComentarioSimple = linea.indexOf(";");
                int indiceComentarioDoble = linea.indexOf(";;");

                // Si ambos existen, tomamos el primero que aparece
                int indiceComentario = (indiceComentarioSimple == -1) ? indiceComentarioDoble
                        : (indiceComentarioDoble == -1) ? indiceComentarioSimple
                        : Math.min(indiceComentarioSimple, indiceComentarioDoble);

                if (indiceComentario != -1) {
                    linea = linea.substring(0, indiceComentario); // Cortamos antes del comentario
                }

                contenido.append(linea).append(" ");
            }
        }
        return contenido.toString().trim();
    }

    private static List<Object> parsearCodigo(String codigo) {
        return parsearLista(new StringTokenizer(codigo, " ()\"'", true));
    }

    private static List<Object> parsearLista(StringTokenizer listaAnidadas) {
        List<Object> lista = new ArrayList<>();
        boolean enString = false;
        StringBuilder bufferString = new StringBuilder();

        while (listaAnidadas.hasMoreTokens()) {
            String token = listaAnidadas.nextToken();

            if (token.equals("\"") && !enString) {
                enString = true;
                bufferString.setLength(0);
                bufferString.append("\"");
                continue;
            } else if (token.equals("\"") && enString) {
                enString = false;
                bufferString.append("\"");
                lista.add(bufferString.toString());
                continue;
            }

            if (enString) {
                bufferString.append(token);
                continue;
            }

            token = token.trim();
            if (token.isEmpty()) continue;

            if (token.equals("'")) {
                // Si encontramos un ', lo envolvemos en la siguiente lista
                if (listaAnidadas.hasMoreTokens()) {
                    String siguiente = listaAnidadas.nextToken().trim();
                    if (siguiente.equals("(")) {
                        List<Object> subLista = parsearLista(listaAnidadas);
                        subLista.add(0, "'");  // Agregar ' como primer elemento
                        lista.add(subLista);
                    } else {
                        lista.add(List.of("'", parsearValor(siguiente)));
                    }
                }
            } else if (token.equals("(")) {
                lista.add(parsearLista(listaAnidadas));
            } else if (token.equals(")")) {
                return lista;
            } else {
                lista.add(parsearValor(token));
            }
        }
        return lista;
    }

    private static Object parsearValor(String valor) {
        // Verifica si es un número entero
        if (valor.matches("-?\\d+")) {
            return Integer.parseInt(valor);
        }

        // Verifica si es un número decimal (con punto)
        if (valor.matches("-?\\d+\\.\\d+")) {
            return Double.parseDouble(valor);
        }

        // Si no es número, devuelve el valor como string
        return valor;
    }
}
