def measure (mat top) : mat outcome{
        
        mat ad;

        ad = adj(top);
        outcome = top * ad;
}

def hadamard (int n) : mat gate{
        
        int i;
        gate = H;

        for (i from 0 to n-1 by 1){
            gate = gate @ H; 
        }
}

def topqubit (int n) : mat input{

        int i;
        input = |0>;

        for (i from 0 to n-1 by 1){
                input = input @ |0>;
        }          
}

def deutsch (int n, mat U) : float outcomeZero{

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

        n = 1;
        Ub = [(1,0,0,0)(0,1,0,0)(0,0,0,1)(0,0,1,0)];
        Uc = [(1,0,0,0)(0,1,0,0)(0,0,1,0)(0,0,0,1)];

        outcome = deutsch(n, Ub);
        print(outcome);
        
        outcome = deutsch(n, Uc);
        print(outcome);

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
