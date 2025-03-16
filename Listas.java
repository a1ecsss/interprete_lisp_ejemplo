import java.util.List;
import java.util.ArrayList;

public class Listas implements ISExpression {
    private static final List<String> SExpressions = List.of("list", "car", "cdr", "cons", "append", "length", "reverse");
    private final Ejecutador ejecutador;

    public Listas(Ejecutador ejecutador) {
        this.ejecutador = ejecutador;
    }

    @Override
    public List<String> getSExpressions() {
        return SExpressions;
    }

    @Override
    public Object evaluarExpresion(List<Object> expresion) {
        if (expresion.isEmpty()) {
            throw new IllegalArgumentException("ListError: Invalid list expression -> " + expresion);
        }
        // Se evalúa cada expresión antes de asignarlas a una lista
        String operador = (String) expresion.get(0);
        for (int i = 1; i < expresion.size(); i++) {
            expresion.set(i, ejecutador.ejecutarExpresion(expresion.get(i)));
        }
        
        List<Object> args = expresion.subList(1, expresion.size());

        switch (operador) {
            case "list":
                return new ArrayList<>(args);  // Devuelve una lista con los elementos dados.

            case "car":
                if (args.size() != 1 || ISExpression.isAtom(args.get(0)) == true) {
                    throw new IllegalArgumentException("ListError: 'car' expects a single list -> " + expresion);
                }
                List<?> carList = (List<?>) args.get(0);
                return carList.isEmpty() ? null : carList.get(0);  // Devuelve el primer elemento.

            case "cdr":
                if (args.size() != 1 || ISExpression.isAtom(args.get(0)) == true) {
                    throw new IllegalArgumentException("ListError: 'cdr' expects a single list -> " + expresion);
                }
                List<?> cdrList = (List<?>) args.get(0);
                return cdrList.isEmpty() ? List.of() : new ArrayList<>(cdrList.subList(1, cdrList.size()));  // Devuelve la cola.

            case "cons":
                if (args.size() != 2) {
                    throw new IllegalArgumentException("ListError: 'cons' expects exactly two arguments -> " + expresion);
                }
            
                // Convertir ambos elementos en listas
                List<Object> primerElemento = (ISExpression.isAtom(args.get(0)) == false)
                    ? new ArrayList<>((List<?>) args.get(0))
                    : new ArrayList<>(List.of(args.get(0)));
            
                List<?> segundoElemento = (ISExpression.isAtom(args.get(1)) == false)
                    ? (List<?>) args.get(1)
                    : List.of(args.get(1));
            
                // Concatenar las listas 
                primerElemento.addAll(segundoElemento);
                
                return primerElemento;
            
            case "append":
                List<Object> resultAppend = new ArrayList<>();
                for (Object arg : args) {
                    if (ISExpression.isAtom(arg) == true) {
                        throw new IllegalArgumentException("ListError: 'append' expects only lists -> " + expresion);
                    }
                    resultAppend.addAll((List<?>) arg);
                }
                return resultAppend;  // Concatena las listas.

            case "length":
                if (args.size() != 1 || ISExpression.isAtom(args.get(0)) == true) {
                    throw new IllegalArgumentException("ListError: 'length' expects a single list -> " + expresion);
                }
                return ((List<?>) args.get(0)).size();  // Devuelve la cantidad de elementos.

            case "reverse":
                if (args.size() != 1 || ISExpression.isAtom(args.get(0)) == true) {
                    throw new IllegalArgumentException("ListError: 'reverse' expects a single list -> " + expresion);
                }
                List<Object> reversedList = new ArrayList<>((List<?>) args.get(0));
                java.util.Collections.reverse(reversedList); // Invierte la lista.
                return reversedList;  

            default:
                throw new RuntimeException("OperatorError: Unknown list operator -> " + operador);
        }
    }
}
