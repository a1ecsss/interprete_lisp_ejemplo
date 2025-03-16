import java.util.List;

public class Variables implements ISExpression {
    private static final List<String> SExpressions = List.of("setq", "setf");
    private final Ejecutador ejecutador;

    public Variables(Ejecutador ejecutador) {
        this.ejecutador = ejecutador;
    }

    @Override
    public List<String> getSExpressions() {
        return SExpressions;
    }

    @Override
    public Object evaluarExpresion(List<Object> expresion) {
        if (expresion.size() != 3) {
            throw new IllegalArgumentException("VariableError: '" + expresion.get(0) + "' expects a variable name and a value, but got -> " + expresion.get(1) + " and -> " + expresion.get(2));
        }

        String operador = (String) expresion.get(0);
        Object nombreVar = expresion.get(1);
        Object valor = ejecutador.ejecutarExpresion(expresion.get(2));

        switch (operador) {
            case "setq":
                if (!(nombreVar instanceof String)) {
                    throw new IllegalArgumentException("VariableError: Variable name must be a string -> " + nombreVar);
                }
                ejecutador.environment.setVariable((String) nombreVar, valor);
                return valor;

            case "setf":
                // Si el primer argumento es un átomo, se comporta como `setq`
                if (ISExpression.isAtom(nombreVar)) {
                    ejecutador.environment.setVariable((String) nombreVar, valor);
                    return valor;
                }

                // Si no es un átomo, significa que queremos modificar una estructura
                if (ISExpression.isAtom(nombreVar)) {
                      throw new IllegalArgumentException("SetfError: Invalid location -> " + nombreVar);
                }

                List<?> modificador = (List<?>) nombreVar;
                if (modificador.isEmpty()) {
                    throw new IllegalArgumentException("SetfError: Invalid structure to modify -> " + nombreVar);
                }

                // Extraer el nombre de la variable que queremos modificar
                Object estructuraNombre = modificador.get(1);
                if (!(estructuraNombre instanceof String)) {
                    throw new IllegalArgumentException("SetfError: Expected a variable name but got -> " + estructuraNombre);
                }

                // Evaluar la expresión interna para obtener la estructura a modificar
                System.out.println("estructuraNombre: "+estructuraNombre);
                Object estructura = ejecutador.ejecutarExpresion(estructuraNombre);
                if (!(estructura instanceof List)) {
                    throw new IllegalArgumentException("SetfError: Expected a list but got -> " + estructura);
                }

                // Modificar la estructura
                ejecutador.environment.modificarEstructura((List<Object>) estructura, modificador.get(0), valor);
                return valor;

            default:
                throw new IllegalArgumentException("VariableError: Unknown operation -> " + operador);
        }
    }
}
