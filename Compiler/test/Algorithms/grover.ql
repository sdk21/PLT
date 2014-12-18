def measure (mat top) : mat outcome{
        
        mat ad;

        ad = adj(top);
        outcome = top * ad;
}

def ntensor (int n, mat k) : mat gate{
        
        int i;
        gate = k;

        for (i from 0 to n-1 by 1){
            gate = gate @ k; 
        }
}

def prepareU (int n) : mat gate {
        # prepare the oracle for grover, Uw
        mat i;
        mat u;

        i = [(1,0)
             (0,0)];

        u = ntensor(n+1, i);
        gate = ntensor(n+1,IDT)-2*u;
}

def prepareG (int n) : mat gate{
        # grover diffusive operator
        mat s; mat sa; mat i; mat h;

        s = ntensor(n,|0>);
        sa = adj(s);
        i = ntensor(n,IDT);
        gate = 2*s*sa - i;
        h = ntensor(n, H);
        gate = h*gate*h;
        gate = gate @ IDT;         
}

def grover (int n) : float outcomeZero{

        mat bottom; mat top; mat input;
        mat hadtop; mat u; mat g; mat go; mat meas;
        int i;

        bottom = |1>;
        top = ntensor(n, |0>);
        input = top @ bottom;
        
        hadtop = ntensor(n, H);
        input = (hadtop @ H)*input;
        u = prepareU(n);
        g = prepareG(n);
        
        # grover operator
        go = g*u;
        
        for (i from 0 to n by 1){
                input = go*input; 
        }

        meas = measure(top);
        input = (meas @ IDT)* input;
        outcomeZero = norm(input);
}


def compute () : float outcome{
        #simulate the grover for f(0)=1
        
        int n; mat Ub; mat Uc;
        n = 1;
        
        outcome = grover(n);
        print(outcome);
        
        n = 2;
        outcome = grover(n);
}
