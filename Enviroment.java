import java.util.*;

class Environment {
    public Environment parentEnviroment;
    private List<Object> codigo;
    private Map<String, Object> variables;
    private Map<String, Defun> funciones;
    private Ejecutador ejecutador;

    public Environment(List<Object> codigo, Object parameters, Environment parentEnviroment) {
        this.codigo = codigo;
        this.variables = new HashMap<>();
        this.funciones = new HashMap<>();
        this.ejecutador = new Ejecutador(this);
        this.variables.put("nil", null);
        this.variables.put("t", "T");
        this.parentEnviroment = parentEnviroment;
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
        return this.variables;  
    }

    public Map<String, Defun> getFunciones() {
        return this.funciones;  
    }

    public void setFuncion(String nombre, Defun defun){
        funciones.put(nombre.toLowerCase(), defun);
    }
    

    public void setVariable(String nombre, Object valor) {
        if("nil".equalsIgnoreCase(nombre) || "t".equalsIgnoreCase(nombre)){
            throw new RuntimeException("Error: Attempt to set constant symbol -> "+nombre);
        }
        variables.put(nombre.toLowerCase(), valor);
    }

    public Object getVariable(String nombre) {
        Environment env = this;
        while (env != null) {
            Map<String, Object> vars = env.getVariables();
            if (vars.containsKey(nombre.toLowerCase())) {
                return vars.get(nombre.toLowerCase());
            }
            env = env.parentEnviroment;
        }
        throw new RuntimeException("VariableError: Undefined variable -> " + nombre);
    }

    public Defun getFuncion(String nombre) {
        Environment env = this;
        while (env != null) {
            Map<String, Defun> funs = env.getFunciones();
            if (funs.containsKey(nombre.toLowerCase())) {
                return funs.get(nombre.toLowerCase());
            }
            env = env.parentEnviroment;
        }
        throw new RuntimeException("DefunError: Undefined defun -> " + nombre);
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

