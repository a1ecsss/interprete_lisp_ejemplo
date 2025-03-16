import java.util.Arrays;
import java.util.List;

public class Comparaciones implements ISExpression {
    private static final List<String> SExpressions = List.of("=", "/=", "<", ">", "<=", ">=");
    private final Ejecutador ejecutador;

    public Comparaciones(Ejecutador ejecutador) {
        this.ejecutador = ejecutador;
    }

    @Override
    public List<String> getSExpressions() {
        return SExpressions;
    }

    @Override
    public Object evaluarExpresion(List<Object> expresion) {
        if (expresion.size() < 3) {
            throw new IllegalArgumentException("ComparisonError: Invalid comparison expression -> " + expresion);
        }
        
        String operador = (String) expresion.get(0);
        Object operando1 = expresion.get(1);
        Object operando2 = expresion.get(2);
        
        // Evaluar si los operandos son listas y ejecutarlas recursivamente
        operando1 = ejecutador.ejecutarExpresion(operando1);
        operando2 = ejecutador.ejecutarExpresion(operando2);

        // Validar si son null antes de convertir
        if (operando1 == null || operando2 == null) {
            throw new RuntimeException("TypeError: Null value found in comparison expression -> " + expresion);
        }

        // Convertir los operandos a números
        double num1, num2;
        try {
            num1 = Double.parseDouble(operando1.toString());
            num2 = Double.parseDouble(operando2.toString());
        } catch (NumberFormatException e) {
            throw new RuntimeException("TypeError: Expected numbers but got -> " + operando1 + ", " + operando2);
        }
        
        boolean resultado = switch (operador) {
            case "=" -> num1 == num2;
            case "/=" -> num1 != num2;
            case "<" -> num1 < num2;
            case ">" -> num1 > num2;
            case "<=" -> num1 <= num2;
            case ">=" -> num1 >= num2;
            default -> throw new RuntimeException("OperatorError: Unknown comparison operator -> " + operador);
        };
        
        return resultado ? "T" : null;
    }
}