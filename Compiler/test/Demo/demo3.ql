def measure (mat top) : mat outcome{
        # returns the measurement matrix for top qubit        
        mat ad;

        ad = adj(top);
        outcome = top * ad;
}

def hadamard (int n) : mat gate{
        # returns Hadamard gate for n qubit system
        int i;
        gate = H;

        for (i from 0 to n-1 by 1){
            gate = gate @ H; 
        }
}

def topqubit (int n) : mat input{
        #returns zero qubit for n qubit system
        int i;
        input = |0>;

        for (i from 0 to n-1 by 1){
                input = input @ |0>;
        }          
}

def deutsch (int n, mat U) : float outcomeZero{
        # series of unitary transformation followed by measurement
        mat bottom; mat top; mat input;
        mat hadtop; mat meas;

        bottom = |1>;
        top = topqubit(n);
        input = top @ bottom;   
        
        hadtop = hadamard(n);
        input = (hadtop @ H)*input;     
        input = U * input;
        input = (hadtop @ IDT)*input;
        meas = measure(top);

        input = (meas @ IDT)* input;
        outcomeZero = norm(input);
}


def compute () : float outcome{

        int n; mat Ub; mat Uc;
        #test for n equals 1
        n = 1;
        # Ub is balanced, Uc is constant
        Ub = [(1,0,0,0)(0,1,0,0)(0,0,0,1)(0,0,1,0)];
        Uc = [(1,0,0,0)(0,1,0,0)(0,0,1,0)(0,0,0,1)];

        outcome = deutsch(n, Ub);
        print(outcome);
        
        outcome = deutsch(n, Uc);
        print(outcome);
        
        # test for n equals 2
        n = 2;
        Ub = [(1,0,0,0,0,0,0,0) 
              (0,1,0,0,0,0,0,0)
              (0,0,1,0,0,0,0,0)
              (0,0,0,1,0,0,0,0) 
              (0,0,0,0,0,1,0,0) 
              (0,0,0,0,1,0,0,0)
              (0,0,0,0,0,0,0,1)
              (0,0,0,0,0,0,1,0)];

        outcome = deutsch(n, Ub);
}
