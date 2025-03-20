import java.util.Arrays;
import java.util.List;

public class Aritmetica implements ISExpression {
    private static final List<String> SExpressions = List.of("+", "-", "*", "/","mod","expt");
    private final Ejecutador ejecutador;

    public Aritmetica(Ejecutador ejecutador) {
        this.ejecutador = ejecutador;
    }

    @Override
    public List<String> getSExpressions() {
        return SExpressions;
    }

    @Override
    public Object evaluarExpresion(List<Object> expresion) {
        if (expresion.size() < 3) {
            throw new IllegalArgumentException("ArithmeticError: Invalid arithmetic expression -> " + expresion);
        }
        String operador = (String) expresion.get(0);
        double resultado = this.checkNum(expresion, 1);
        for (int i = 2; i < expresion.size(); i++) {
            double num = this.checkNum(expresion, i);
            switch (operador) {
                case "+" -> resultado = resultado + num;
                case "-" -> resultado = resultado - num;
                case "*" -> resultado = resultado * num;
                case "/" -> {
                    if (num == 0) throw new ArithmeticException("ZeroDivisionError: Division by zero");
                    resultado = resultado / num;
                }
                case "mod" -> resultado = resultado % num;
                case "expt" -> resultado = Math.pow(resultado, num);
                default -> throw new RuntimeException("OperatorError: Unknown arithmetic operator -> " + operador);
            };
            
        }
        return resultado;
    }

    private double checkNum(List<Object> expresion, int index){
        Object operando = ejecutador.ejecutarExpresion(expresion.get(index));
        // Validar si son null antes de convertir
        if (operando == null) {
            throw new RuntimeException("TypeError: NIL value found in arithmetic expression -> " + expresion);
        }
        double resultado;
        try {
            resultado = Double.parseDouble(operando.toString());
        } catch (NumberFormatException e) {
            throw new RuntimeException("TypeError: Expected numbers but got -> " + expresion.get(1) + " in arithmetic expression -> " + expresion);
        }
        return resultado;
    }

}
