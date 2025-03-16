import java.util.*;

class Environment {
    public List<Object> codigo;
    public Map<String, Object> variables;
    private Map<String, Defun> funciones;
    private Ejecutador ejecutador;

    public Environment(List<Object> codigo, String name, Object parameters) {
        this.codigo = codigo;
        //System.out.println("CODIGO: " + codigo);
        this.variables = new HashMap<>();
        this.funciones = new HashMap<>();
        this.ejecutador = new Ejecutador(this);
        Defun defun = new Defun(codigo, parameters);
        this.funciones.put(name, defun); // Se referencia a sí misma en funciones
    }

    public Object ejecutarCodigo() {
        Object resultado = null;
        for (int i = 0; i < codigo.size(); i++) {
            resultado = this.ejecutador.ejecutarExpresion(codigo.get(i));
        }
        return resultado;
    }
    
    public Object getCodigo() {
        return codigo;
    }
    

    public Map<String, Object> getVariables() {
        return variables;
    }

    public Map<String, Defun> getFunciones() {
        return funciones;
    }

    public void setVariable(String nombre, Object valor) {
        variables.put(nombre, valor);
    }

    public Object getVariable(String nombre) {
        if (!variables.containsKey(nombre)) {
            throw new RuntimeException("VariableError: Undefined variable -> " + nombre);
        }
        return variables.get(nombre);
    }

    public void modificarEstructura(List<Object> lista, Object operador, Object nuevoValor) {
        if (operador.equals("car")) {
            if (lista.isEmpty()) {
                throw new IllegalArgumentException("SetfError: Cannot modify `car` of an empty list.");
            }
            lista.set(0, nuevoValor);  // Modifica el primer elemento
        } else if (operador.equals("cdr")) {
            if (lista.isEmpty()) {
                throw new IllegalArgumentException("SetfError: Cannot modify `cdr` of an empty list.");
            }
            lista.clear();
            lista.addAll((List<?>) nuevoValor);  // Reemplaza el resto de la lista
        } else {
            throw new IllegalArgumentException("SetfError: Unsupported operation -> " + operador);
        }
    }
    
}

