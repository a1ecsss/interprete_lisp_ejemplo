import java.util.*;

class Environment {
    private List<Object> codigo;
    private Map<String, Object> variables;
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
        this.variables.put("nil", null);
        this.variables.put("t", "T");
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
        if("nil".equals(nombre.toLowerCase())){throw new RuntimeException("Error: Attempt to set constant symbol -> NIL");}
        if("t".equals(nombre.toLowerCase())){ throw new RuntimeException("Error: Attempt to set constant symbol -> T");}
        variables.put(nombre.toLowerCase(), valor);
    }

    public Object getVariable(String nombre) {
        if (!variables.containsKey(nombre.toLowerCase())) {
            throw new RuntimeException("VariableError: Undefined variable -> " + nombre);
        }
        return variables.get(nombre.toLowerCase());
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

