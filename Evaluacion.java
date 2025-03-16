import java.util.List;

public class Evaluacion implements ISExpression {
    private static final List<String> SExpressions = List.of("quote", "eval", "'","atom","numberp","evenp","oddp","min","max", "progn");
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
            case "numberp":
                if (expresion.size() != 2) {
                    throw new IllegalArgumentException("EvaluationError: 'numberp' expects exactly one argument -> " + expresion);
                }
                return ISExpression.isNumber(ejecutador.ejecutarExpresion(expresion.get(1))) ? "T" : null; // Evalúa el contenido
            case "evenp":
                if (expresion.size() != 2) {
                    throw new IllegalArgumentException("EvaluationError: 'evenp' expects exactly one argument -> " + expresion);
                }
                return ISExpression.isEven(ejecutador.ejecutarExpresion(expresion.get(1))) ? "T" : null; // Evalúa el contenido
            case "oddp":
                if (expresion.size() != 2) {
                    throw new IllegalArgumentException("EvaluationError: 'evenp' expects exactly one argument -> " + expresion);
                }
                return ISExpression.isOdd(ejecutador.ejecutarExpresion(expresion.get(1))) ? "T" : null; // Evalúa el contenido
            case "max":
                if (expresion.size() != 3) {
                    throw new IllegalArgumentException("EvaluationError: 'min' expects exactly two argument -> " + expresion);
                }
                return ISExpression.max(ejecutador.ejecutarExpresion(expresion.get(1)), ejecutador.ejecutarExpresion(expresion.get(2))); // Evalúa el contenido
            case "min":
                if (expresion.size() != 3) {
                    throw new IllegalArgumentException("EvaluationError: 'max' expects exactly two argument -> " + expresion);
                }
                return ISExpression.min(ejecutador.ejecutarExpresion(expresion.get(1)), ejecutador.ejecutarExpresion(expresion.get(2))); // Evalúa el contenido
            case "progn":
                Object resultado = null;
                // Comenzamos desde el índice 1 en lugar de 0
                for (int i = 1; i < expresion.size(); i++) {
                    resultado = ejecutador.ejecutarExpresion(expresion.get(i));
                }
                return resultado;
            default:
                throw new RuntimeException("OperatorError: Unknown evaluation operator -> " + operador);
        }
    }
    
}

