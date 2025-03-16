import java.util.List;

public interface ISExpression {

    List<String> getSExpressions();
    Object evaluarExpresion(List<Object> expresion);

    static boolean isAtom(Object valor) {
        //un átomo es cualquier cosa que NO sea una lista
        if (!(valor instanceof List)) {
            return true; // En Lisp, los valores verdaderos suelen representarse con "T"
        }
        return false; // En Lisp, los valores falsos suelen representarse con "NIL"
    }

    // Verifica si un valor es un número en Lisp
    static boolean isNumber(Object valor) {
        return valor instanceof Number;
    }

    // Verifica si un número es par
    static boolean isEven(Object valor) {
        if (isNumber(valor)) {
            return ((Number) valor).intValue() % 2 == 0;
        }
        return false;
    }

    // Verifica si un número es impar
    static boolean isOdd(Object valor) {
        if (isNumber(valor)) {
            return ((Number) valor).intValue() % 2 != 0;
        }
        return false;
    }

    // Devuelve el mayor de dos números
    static Object max(Object a, Object b) {
        if (isNumber(a) && isNumber(b)) {
            return Math.max(((Number) a).doubleValue(), ((Number) b).doubleValue());
        }
        throw new IllegalArgumentException("EvaluationError: 'max' expects two numbers but got -> " + a + ", " + b);
    }

    // Devuelve el menor de dos números
    static Object min(Object a, Object b) {
        if (isNumber(a) && isNumber(b)) {
            return Math.min(((Number) a).doubleValue(), ((Number) b).doubleValue());
        }
        throw new IllegalArgumentException("EvaluationError: 'min' expects two numbers but got -> " + a + ", " + b);
    }

    // Verifica si un valor es nulo (equivalente a NIL en Lisp)
    static boolean isNil(Object valor) {
        return "NIL".equalsIgnoreCase(String.valueOf(valor)) || valor == null;
    }

    // Verifica si un valor es nulo (equivalente a NIL en Lisp)
    static boolean isT(Object valor) {
        return "T".equalsIgnoreCase(String.valueOf(valor)) || valor == null;
    }
    

}