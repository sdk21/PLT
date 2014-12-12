

def hadamard(int n) : mat gate{
        
        #returns Hadamard gate of 2^n dimensions
        int i; 
        gate = H; 
        for( i from 1 to n by 1 ){
                gate = gate @ H;                 
        }       
}

def measure(mat top) : mat result{
        
        # returns the measurement matrix
        mat ad;        
        
        ad = adj(top);
        result = top * ad;
}

def deutsch(int n, mat top, mat U) : int outcomeZero{

        # input is tensor product of top and bottom register       
        
        mat bottom;       
        mat input;
        mat hadn;        
        mat meas;
        mat Idt;
        mat prob;

        bottom = |1>;
        Idt = [(1,0)(0,1)];
        
        # input qubit is the tensor of top and bottom qubit
        input = top @ bottom;
        
        # 2 qubit hadamard gate
        hadn = hadamard(n);
        
        # application of hadamard
        input = hadn * input;
        
        # application of U
        input = U*input;
         
        # application of H on top and Identity on bottom
        input = (H @ Idt)*input;
       
         
        # measurement matrix
        meas = measure(top);        
        input = (meas @Idt)*input;
        
        # probability measurement
        prob = norm(input);
       
        if (0. eq 0.0)){
                outcomeZero = 0;
        } 
}

def compute () : int outcome{
        mat top;
        mat Ub;
        mat Uc;
        int n;
        
        n = 2;
        top = |0>;
        Ub = [(1,0,0,0)(0,1,0,0)(0,0,0,1)(0,0,1,0)];
        Uc = [(1,0,0,0)(0,1,0,0)(0,0,1,0)(0,0,0,1)];

        outcome = deutsch (n, top, Ub);
        #print(outcome);
        #outcome = deutsch (n, top, Uc);
}
