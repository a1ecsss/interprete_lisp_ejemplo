import java.util.List;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class EntradaSalida implements ISExpression {
    private static final List<String> SExpressions = List.of("print", "format", "write", "read");
    private final Ejecutador ejecutador;
    private final Scanner scanner;

    public EntradaSalida(Ejecutador ejecutador) {
        this.ejecutador = ejecutador;
        this.scanner = new Scanner(System.in);
    }

    @Override
    public List<String> getSExpressions() {
        return SExpressions;
    }

    @Override
    public Object evaluarExpresion(List<Object> expresion) {
        if (expresion.isEmpty()) {
            throw new IllegalArgumentException("IOError: Missing operator -> " + expresion);
        }

        String operador = (String) expresion.get(0);

        switch (operador) {
            case "print":
                return evaluarPrint(expresion);

            case "write":
                return evaluarWrite(expresion);

            case "format":
                return evaluarFormat(expresion);

            case "read":
                return evaluarRead();

            default:
                throw new RuntimeException("OperatorError: Unknown IO operator -> " + operador);
        }
    }

    private Object evaluarPrint(List<Object> expresion) {
        if (expresion.size() < 2) {
            throw new IllegalArgumentException("IOError: 'print' expects at least one argument -> " + expresion);
        }

        StringBuilder resultadoFinal = new StringBuilder();
        Object resultado = null;

        for (int i = 1; i < expresion.size(); i++) {
            resultado = this.toStringNil(ejecutador.ejecutarExpresion(expresion.get(i)));
            resultadoFinal.append(resultado).append(" ");
        }

        System.out.println(resultadoFinal.toString().trim());
        return resultado;
    }

    private Object evaluarWrite(List<Object> expresion) {
        if (expresion.size() < 2) {
            throw new IllegalArgumentException("IOError: 'write' expects at least one argument -> " + expresion);
        }

        Object resultado = ejecutador.ejecutarExpresion(expresion.get(1));

        System.out.print(this.toStringNil(resultado));  // Sin salto de línea
        return resultado;
    }

    private Object evaluarRead() {
        String input = scanner.nextLine().trim();
        // Intentamos convertir a número
        try {
            if (input.contains(".")) {  // Si tiene punto, lo tratamos como Double
                return Double.parseDouble(input);
            } else {  // Si no tiene punto, lo tratamos como Integer
                return Integer.parseInt(input);
            }
        } catch (NumberFormatException e) {
            // Si falla la conversión, devolver el input como String
            return input;
        }
    }
    

    private Object evaluarFormat(List<Object> expresion) {
        if (expresion.size() < 3) {
            throw new IllegalArgumentException("IOError: 'format' expects at least two arguments -> " + expresion);
        }

        Object destino = expresion.get(1);
        if (!"t".equalsIgnoreCase(destino.toString()) && !"NIL".equalsIgnoreCase(destino.toString())) {
            throw new IllegalArgumentException("IOError: 'format' expects T, NIL, or a string as first argument -> " + destino);
        }

        Object formatoBruto = ejecutador.ejecutarExpresion(expresion.get(2));
        if (!(formatoBruto instanceof String)) {
            throw new IllegalArgumentException("IOError: The format string must be a string -> " + formatoBruto);
        }

        String formato = (String) formatoBruto;
        String resultado = procesarFormato(formato, expresion.subList(3, expresion.size()));

        if ("T".equalsIgnoreCase(destino.toString())) {
            System.out.print(resultado);  // No usa println porque `format` no siempre agrega salto de línea
        }

        return resultado;
    }

    private String procesarFormato(String formato, List<Object> valores) {
        Pattern pattern = Pattern.compile("~(\\d*,?\\d*)?[ADFS%&C]");
        Matcher matcher = pattern.matcher(formato);
        StringBuffer resultado = new StringBuffer();

        int indiceValor = 0;

        while (matcher.find()) {
            String codigo = matcher.group();// Extrae `~X`
            if(!"~%".equals(codigo.toString()) && !"~&".equals(codigo.toString())){
                if (indiceValor >= valores.size()) {
                    throw new IllegalArgumentException("IOError: Not enough arguments for format string -> " + formato);
                }
                Object valorReemplazo = ejecutador.ejecutarExpresion(valores.get(indiceValor));
                String reemplazo = obtenerValorFormateado(codigo, valorReemplazo);
                matcher.appendReplacement(resultado, reemplazo);
                indiceValor++;
            }else{
                String reemplazo = obtenerValorFormateado(codigo, null);
                matcher.appendReplacement(resultado, reemplazo);
            } 
        }
        matcher.appendTail(resultado);

        return resultado.toString();
    }

    private String obtenerValorFormateado(String codigo, Object valor) {
        // Extraer ancho mínimo y decimales si están presentes (ejemplo: ~8,D o ~10,2F)
        Pattern pattern = Pattern.compile("~(\\d+)?(?:,(\\d+))?([ADFS%&C])");
        Matcher matcher = pattern.matcher(codigo);
        int anchoMinimo = 0;
        int decimales = 2;
        String tipo = codigo;
    
        if (matcher.matches()) {
            if (matcher.group(1) != null) {
                anchoMinimo = Integer.parseInt(matcher.group(1));
            }
            if (matcher.group(2) != null) {
                decimales = Integer.parseInt(matcher.group(2));
            }
            tipo = "~" + matcher.group(3);
        }
    
        switch (tipo) {
            case "~A":  // Cualquier tipo convertido a String sin `String.format()`
                return (anchoMinimo > 0) 
                    ? String.format("%" + anchoMinimo + "s", valor.toString())
                    : this.toStringNil(valor);
    
            case "~D":  // Números enteros con ancho mínimo
                if (valor instanceof Number) {
                    int intValue = ((Number) valor).intValue();
                    return (anchoMinimo > 0) 
                        ? String.format("%" + anchoMinimo + "d", intValue)
                        : String.valueOf(intValue);
                }
                throw new IllegalArgumentException("IOError: Expected a number for ~D but got -> " + valor);
    
            case "~F":  // Números con decimales y ancho mínimo
                if (valor instanceof Number) {
                    double doubleValue = ((Number) valor).doubleValue();
                    return (anchoMinimo > 0) 
                        ? String.format("%" + anchoMinimo + "." + decimales + "f", doubleValue)
                        : String.format("%." + decimales + "f", doubleValue);
                }
                throw new IllegalArgumentException("IOError: Expected a number for ~F but got -> " + valor);
    
            case "~C":  // Carácter
                if (valor instanceof Character) {
                    return this.toStringNil(valor);
                } else if (valor instanceof String && ((String) valor).length() == 1) {
                    return (String) valor;
                }
                throw new IllegalArgumentException("IOError: Expected a single character for ~C but got -> " + valor);
    
            case "~S":  // Evaluar y mostrar su representación exacta en Lisp
                //System.out.println("EN S: "+ valor);
                //Object resultadoEvaluado = ejecutador.ejecutarExpresion(valor);
                return this.toStringNil(valor);
    
            case "~%":  // Salto de línea
                return "\n";
    
            case "~&":  // Salto de línea si es necesario
                return System.lineSeparator();
    
            default:
                throw new IllegalArgumentException("IOError: Unsupported format code -> " + codigo);
        }
    }
    
    String toStringNil(Object valor) {
        return (valor != null) ? valor.toString() : "NIL";
    }
    

}
