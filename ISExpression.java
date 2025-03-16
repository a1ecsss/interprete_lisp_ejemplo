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

}