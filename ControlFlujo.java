import java.util.Arrays;
import java.util.List;

public class ControlFlujo implements ISExpression {
    private static final List<String> SExpressions = List.of("if", "cond","case");
    private final Ejecutador ejecutador;

    public ControlFlujo(Ejecutador ejecutador) {
        this.ejecutador = ejecutador;
    }

    @Override
    public List<String> getSExpressions() {
        return SExpressions;
    }

    @Override
    public Object evaluarExpresion(List<Object> expresion) {
        if (expresion.isEmpty()) {
            throw new IllegalArgumentException("ControlFlowError: Invalid control flow expression -> " + expresion);
        }
        
        String operador = (String) expresion.get(0);
        
        switch (operador) {
            case "if":
                if (expresion.size() < 3 || expresion.size() > 4) {
                    throw new IllegalArgumentException("ControlFlowError: 'if' expects 2 or 3 arguments -> " + expresion);
                }
                Object condicion = ejecutador.ejecutarExpresion(expresion.get(1));
                if (condicion != null) {
                    return ejecutador.ejecutarExpresion(expresion.get(2));
                } else if (expresion.size() == 4) {
                    return ejecutador.ejecutarExpresion(expresion.get(3));
                }
                return null;

            case "cond":
                for (int i = 1; i < expresion.size(); i++) {
                    Object elemento = expresion.get(i);
                    if (ISExpression.isAtom(elemento)) {
                        throw new IllegalArgumentException("ControlFlowError: 'cond' expects a list of (condition expression) pairs -> " + expresion);
                    }
                    List<Object> par = (List<Object>) elemento;
                    if (par.isEmpty()) {
                        continue; // Si hay una lista vacía, simplemente la ignoramos
                    }
                    if (par.size() < 2) {
                        throw new IllegalArgumentException("ControlFlowError: 'cond' expects at least a condition and an expression -> " + expresion);
                    }
                    Object cond = par.get(0);
                    //System.out.println("PAR: "+par);
                    //System.out.println("cond: "+cond+" equals: "+"t".equals(cond));
                    if("t".equals(cond)){
                        Object resultado = null;
                        for(Object proceso : par.subList(1, par.size())){
                            resultado = ejecutador.ejecutarExpresion(proceso);
                        }
                        //System.out.println("PAR en la 1: "+par.get(1));
                        return resultado;
                    }
                    cond = ejecutador.ejecutarExpresion(cond);
                    if (cond != null) {
                        Object resultado = null;
                        for(Object proceso : par.subList(1, par.size())){
                            resultado = ejecutador.ejecutarExpresion(proceso);
                        }
                        //System.out.println("PAR en la 1: "+par.get(1));
                        return resultado;
                        //return ejecutador.ejecutarExpresion(par.get(1));
                    }
                }
                return null;
            case "case":
                if (expresion.size() < 3) {
                    throw new IllegalArgumentException("ControlFlowError: 'case' expects a value and at least one case -> " + expresion);
                }
            
                Object valor = ejecutador.ejecutarExpresion(expresion.get(1));  // Evalúa el valor a comparar
            
                for (int i = 2; i < expresion.size(); i++) {
                    Object elemento = expresion.get(i);
                    if (ISExpression.isAtom(elemento)) {
                        throw new IllegalArgumentException("ControlFlowError: 'case' expects a list of (value expression) pairs -> " + expresion);
                    }
            
                    List<Object> par = (List<Object>) elemento;
                    if (par.isEmpty()) {
                        continue;  // Si hay una lista vacía, simplemente la ignoramos
                    }
            
                    if (par.size() < 2) {
                        throw new IllegalArgumentException("ControlFlowError: 'case' expects at least a value and an expression -> " + expresion);
                    }
            
                    Object caso = par.get(0);
            
                    if (!ISExpression.isAtom(caso)) {  // Soporte para varios valores en un solo caso (case1 case2 case3)
                        for (Object opcion : (List<?>) caso) {
                            if (valor.equals(ejecutador.ejecutarExpresion(opcion))) {
                                Object resultado = null;
                                for(Object proceso : par.subList(1, par.size())){
                                    resultado = ejecutador.ejecutarExpresion(proceso);
                                }
                                return resultado;
                                //return ejecutador.ejecutarExpresion(par.get(1));
                            }
                        }
                    } else if (caso.equals("otherwise")) {  // Caso por defecto
                        Object resultado = null;
                        for(Object proceso : par.subList(1, par.size())){
                            resultado = ejecutador.ejecutarExpresion(proceso);
                        }
                        return resultado;
                        //return ejecutador.ejecutarExpresion(par.get(1));
                    } else if (valor.equals(ejecutador.ejecutarExpresion(caso))) {  // Coincidencia exacta
                        Object resultado = null;
                        for(Object proceso : par.subList(1, par.size())){
                            resultado = ejecutador.ejecutarExpresion(proceso);
                        }
                        return resultado;
                        //return ejecutador.ejecutarExpresion(par.get(1));
                    }
                }
            
                return null;  // Si no hay coincidencia, retorna `null`
            

            default:
                throw new RuntimeException("OperatorError: Unknown control flow operator -> " + operador);
        }
    }
}
