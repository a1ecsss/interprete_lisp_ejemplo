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

        boolean resultado = true;
        for (int i = 1; i < expresion.size(); i++) {
            //resultado += expresion.get(i);
            Object operando = expresion.get(i);
            // Evaluar si los operandos son listas y ejecutarlas recursivamente
            operando = ejecutador.ejecutarExpresion(operando);
            switch (operador) {
                case "and" -> resultado = resultado && (operando != null);
                case "or" -> resultado = resultado || (operando != null);
            };
            if (!resultado){
                return null;
            }
            if (operador.equals("or") && resultado) {
                return "T";
            }
            
        }
        return "T";
    }
}

