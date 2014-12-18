def compute() : mat output{
        
        mat a;
        mat b;
        mat c;
        mat k;

        a = |11>;
        b = |0>;
        k = <0|;
        
        c = a @ b;      # illustration of dirac notation for qubits
        printq(c);
        
        c = H*b;        # application of gates on qubits
        printq(c);      # output in qubit form
        
        output = b*k;   # simple matrix multiplication
}
