def hadamard(int n) : mat gate{
        
        #returns Hadamard gate of 2^n dimensions
        
        gate = H; 
        for( i from 0 to 2 by 1 ){
                gate = gate @ H;                 
        }       
}

def measure(qubk top) : mat result{
        
        # returns the measurement matrix
        
        mat adjoint = adj(top);
        result = top * adjoint;
}


def deutsch(int n, qubk top, mat U) : int outcomeZero{

        # input is tensor product of top and bottom register       
        
        qubk bottom;       
        mat input;
        mat had;        
        mat meas;

        bottom = |1>;
        input = top @ bottom;
        had = hadamard(n);
        input = had * input;
        
        input = U*input;
        input = (H @ I)*input;
        
        meas = measure (top);        
        input = ((meas@I)*input;
        
        input = norm(input);
        
        if (eq(input,0)){
                outcomeZero = 0;
        }
        else{
                outcomeZero = 1;
        }
}

def execute () : int outcome{
        qubk top;
        mat Ub;
        mat Uc;
        int n;
        
        n = 1;
        top = |0>;
        Ub = [(1,0,0,0)(0,1,0,0)(0,0,0,1)(0,0,1,0)];
        Uc = [(1,0,0,0)(0,1,0,0)(0,0,1,0)(0,0,0,1)];

        outcome = deutsch (2, top, Ub);
        print(outcome);
        outcome = deutsch (2, top, Uc);
        print(outcome);
}
