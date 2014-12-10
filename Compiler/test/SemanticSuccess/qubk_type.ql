def func_test(qubk z) : qubk ret_name { 
        qubk a; 
        a = |00>; 
        a = z;
        ret_name = a; 
}
def compute():qubk tr {
        
        qubk a;
        
        a= |001>;
        tr=func_test(a);
}
