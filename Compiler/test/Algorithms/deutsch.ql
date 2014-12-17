def hadamard(int n) : mat gate{
        
        #returns Hadamard gate of 2^n dimensions
        int i; 
        gate = H;
        for(i from 0 to 2 by 1){
                gate = gate @ H;                 
        }       
}

def measure(mat top) : mat result{
        
        # returns the measurement matrix
        
       mat adjoint;
       adjoint = adj(top);
       result = top * adjoint;
}


def deutsch(int n, mat top, mat U) : int outcomeZero{

        # input is tensor product of top and bottom register       
        
        mat bottom;  # qubit     
        mat input;
        mat had;        
        mat meas;
        mat Idt;
        float m;       
 
        Idt = [(1,0)(0,1)];
        bottom = |1>;
        input = top @ bottom;
        had = hadamard(n);
        input = had * input;
        
        input = U*input;
        input = (H @ Idt)*input;
        
        meas = measure (top);        
        input = (meas @ Idt)*input;
        
        m = norm(input);
        
        if( m eq 0.0 ){
                outcomeZero = 0;
        }
        else{
                outcomeZero = 1;
        }
}

def compute () : int outcome{
        mat top; # qubit
        mat Ub;
        mat Uc;
        int n;
        
        n = 1;
        top = |0>;
        Ub = [(1,0,0,0)(0,1,0,0)(0,0,0,1)(0,0,1,0)];
        Uc = [(1,0,0,0)(0,1,0,0)(0,0,1,0)(0,0,0,1)];

        outcome = deutsch (n, top, Ub);
        print(outcome);
        outcome = deutsch (n, top, Uc);
        print(outcome);
}
