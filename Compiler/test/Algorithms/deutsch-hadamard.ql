def hadamard(int n) : mat gate{
        #returns Hadamard gate of 2^n dimensions
        int i; 
        gate = H;
        for(i from 0 to n-1 by 1){
                gate = gate @ H;                 
        }       
}

def compute () : mat outcome{
        mat top; 
        mat Ub;
        mat Uc;
        int n;
        
        n = 2;
        outcome = hadamard(n);
}
