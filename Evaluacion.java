import java.util.List;

public class Evaluacion implements ISExpression {
    private static final List<String> SExpressions = List.of("quote", "eval", "'","atom");
    private final Ejecutador ejecutador;

    public Evaluacion(Ejecutador ejecutador) {
        this.ejecutador = ejecutador;
    }

    @Override
    public List<String> getSExpressions() {
        return SExpressions;
    }

    @Override
    public Object evaluarExpresion(List<Object> expresion) {
        if (expresion.isEmpty()) {
            throw new IllegalArgumentException("EvaluationError: Invalid evaluation expression -> " + expresion);
        }
        
        String operador = (String) expresion.get(0);

        switch (operador) {
            case "quote":
                if (expresion.size() != 2) {
                    throw new IllegalArgumentException("EvaluationError: 'quote' expects exactly one argument -> " + expresion);
                }
                return expresion.get(1); // Devuelve el dato sin evaluarlo
            case "'":
                if (expresion.size() < 2) { // devuelve un error si la lista esta vacia '()
                    throw new IllegalArgumentException("EvaluationError: ' expects at least one argument -> " + expresion);
                }
                return expresion.subList(1,expresion.size()); // Devuelve el dato sin evaluarlo
            case "eval":
                if (expresion.size() != 2) {
                    throw new IllegalArgumentException("EvaluationError: 'eval' expects exactly one argument -> " + expresion);
                }
                return ejecutador.ejecutarExpresion(ejecutador.ejecutarExpresion(expresion.get(1))); // Evalúa el contenido
            case "atom":
                if (expresion.size() != 2) {
                    throw new IllegalArgumentException("EvaluationError: 'atom' expects exactly one argument -> " + expresion);
                }
                return ISExpression.isAtom(ejecutador.ejecutarExpresion(expresion.get(1))) ? "T" : null; // Evalúa el contenido

            default:
                throw new RuntimeException("OperatorError: Unknown evaluation operator -> " + operador);
        }
    }
    
}

