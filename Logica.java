import java.util.List;

public class Logica implements ISExpression {
    private static final List<String> SExpressions = List.of("and", "or", "not");
    private final Ejecutador ejecutador;

    public Logica(Ejecutador ejecutador) {
        this.ejecutador = ejecutador;
    }

    @Override
    public List<String> getSExpressions() {
        return SExpressions;
    }

    @Override
    public Object evaluarExpresion(List<Object> expresion) {
        if (expresion.isEmpty()) {
            throw new IllegalArgumentException("LogicError: Invalid logical expression -> " + expresion);
        }
        
        String operador = (String) expresion.get(0);
        
        // Operación NOT: Solo debe tener un operando
        if (operador.equals("not")) {
            if (expresion.size() != 2) {
                throw new IllegalArgumentException("LogicError: 'not' expects exactly one argument -> " + expresion);
            }
            Object operando = expresion.get(1);
            operando = ejecutador.ejecutarExpresion(operando);
            return (operando == null) ? 1 : null;
        }
        
        // Operaciones AND y OR
        if (expresion.size() < 3) {
            throw new IllegalArgumentException("LogicError: Invalid number of arguments for logical operation -> " + expresion);
        }
        
        Object operando1 = expresion.get(1);
        Object operando2 = expresion.get(2);

        operando1 = ejecutador.ejecutarExpresion(operando1);
        operando2 = ejecutador.ejecutarExpresion(operando2);

        boolean resultado = switch (operador) {
            case "and" -> (operando1 != null) && (operando2 != null);
            case "or" -> (operando1 != null) || (operando2 != null);
            default -> throw new RuntimeException("OperatorError: Unknown logical operator -> " + operador);
        };
        
        return resultado ? 1 : null;
    }
}