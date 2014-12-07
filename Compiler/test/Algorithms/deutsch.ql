def hadamard(int n) = mati gate{
        
        #returns Hadamard gate of 2^n dimensions
        
        gate = H; 
        for i from 0 to 2 by 1{
                gate = gate @ H;                 
        }       
}

def measure(qubk top) = mati result{
        
        # returns the measurement matrix
        
        mati adjoint = Adj(top);
        result = top * adjoint;
}


def deutsch(int n, qubk top, mati U) = int outcomeZero{

        # input is tensor product of top and bottom register       
        
        qubk bottom;       
        mati input;
        mati had;        
        mati meas;

        bottom = |1>;
        input = top @ bottom;
        had = hadamard(n);
        input = had * input;
        
        input = U*input;
        input = (H @ I)*input;
        
        meas = measure (top);        
        input = ((meas@I)*input;
        
        input = norm(input);
        
        if (eq(input,0){
                outcomeZero = 0;
        }
        else{
                outcomeZero = 1;
        }
}

def execute () = int outcome{
        qubk top;
        mati Ub;
        mati Uc;
        int n;
        
        n = 1;
        top = |0>;
        Ub = [(1,0,0,0)(0,1,0,0)(0,0,0,1)(0,0,1,0)];
        Uc = [(1,0,0,0)(0,1,0,0)(0,0,1,0)(0,0,0,1)];

        outcome = deutsch (top, Ub);
        print(outcome)
        outcome = deutsch (top, Uc);
        print(outcome)
}
