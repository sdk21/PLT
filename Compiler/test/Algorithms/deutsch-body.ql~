def hadamard(int n) : mat gate{
        
        #returns Hadamard gate of 2^n dimensions
        
        int i; 
        gate = H;
        for(i from 0 to n-1 by 1){
                gate = gate @ H;                 
        }       
}

def measure(mat top) : mat result{
       
        # returns the measurement matrix
        
        mat ad;
        ad = adj(top);
        result = top * ad;
}

def deutsch (int n, mat top, mat U) : mat outcomeZero{
        
        mat bottom;
        mat input;
        mat hadtop;
        mat meas;
        mat Idt;
        float m;

        Idt = [(1,0)(0,1)];
        bottom = |0>;

        input = [(0)(1)(0)(0)];
        
        hadtop = hadamard(n);
        
        input = (hadtop @ H) * input;

        input = U*input;
        
        input = (hadtop @ Idt) *input;

        meas = measure (top);

        input = (meas @ Idt) * input;

        m = norm (input);

        print(m);

        outcomeZero = input;

}
def compute () : mat outcome{
        
        mat top;
        int n;
        mat Ub;
        mat Uc;

        n = 1;
        top = |0>;
        Ub = [(1,0,0,0)(0,1,0,0)(0,0,0,1)(0,0,1,0)];
        Uc = [(1,0,0,0)(0,1,0,0)(0,0,1,0)(0,0,0,1)];

        outcome = deutsch(n, top, Ub);


}
