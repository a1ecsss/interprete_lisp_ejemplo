import java.util.List;

public class Funciones implements ISExpression {
    private static final List<String> SExpressions = List.of("defun", "lambda");
    private final Ejecutador ejecutador;

    public Funciones(Ejecutador ejecutador) {
        this.ejecutador = ejecutador;
    }

    @Override
    public List<String> getSExpressions() {
        return SExpressions;
    }

    @Override
    public Object evaluarExpresion(List<Object> expresion) {
        if (expresion.isEmpty()) {
            throw new IllegalArgumentException("FunctionError: Invalid function definition -> " + expresion);
        }
        
        String operador = (String) expresion.get(0);
        
        switch (operador) {
            case "defun":
                if (expresion.size() < 4) {
                    throw new IllegalArgumentException("FunctionError: 'defun' expects a name, parameters, and body -> " + expresion);
                }
                Object nombreFuncion = expresion.get(1);
                Object parameters = expresion.get(2);
                //Object cuerpo = expresion.subList(3, expresion.size());
                
                if (!(nombreFuncion instanceof String)) {
                    throw new IllegalArgumentException("FunctionError: Function name must be a string -> " + nombreFuncion);
                }
                if (ISExpression.isAtom(parameters)) {
                    throw new IllegalArgumentException("FunctionError: Function parameters must be a list -> " + parameters);
                }
                    
                /* 
                Environment nuevoEntorno = new Environment((List<Object>) cuerpo, (String) nombreFuncion);
                for (Object variable : (List<Object>) parametros) {
                    nuevoEntorno.setVariable((String) variable, null);
                }
                */    
                //System.out.println("sublist: "+expresion.subList(2, expresion.size()));
                Defun defun = new Defun(expresion.subList(3, expresion.size()), parameters);
                ejecutador.environment.setFuncion((String) nombreFuncion, defun);

                return nombreFuncion;
            
            case "lambda":
                if (expresion.size() < 3) {
                    throw new IllegalArgumentException("FunctionError: 'lambda' expects parameters and body -> " + expresion);
                }
                return List.of("lambda", expresion.get(1), expresion.subList(2, expresion.size()));
            
            default:
                throw new RuntimeException("OperatorError: Unknown function definition operator -> " + operador);
        }
    }
}