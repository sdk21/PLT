def measure (qubk q) = mat outcome {
        
        # returns measurement matrix
        mat adjoint;
        
        adjoint = adj(q);
        outcome = q * adjoint
}

def outcomezero(qubk bottom) = int probability{
        
        qubk top;
        qubk input;
        mat hteni;
        mat cnot;
        mat ynot;
        mat output;
        mat meas;
        
        top = |0>;
        input = top @ bottom;
        
        hteni = H @ I;
        cnot = [(1,0,0,0)(0,1,0,0)(0,0,1,0)(0,0,0,1)];
        ynot = [(1,0,0,0)(0,0,0,c(i))(0,0,1,0)(0,c(-i),0,0)];
        
        output = hteni * input;
        output = cnot * output;
        output = ynot * output;
        
        meas = measure(top);
        
        meas = meas @ I;

        output = meas * output;

        probability = norm(output);      
}

def execute() = int outcome{
        
        qubk bottom;
        int probab;

        bottom = |1>;
        probab = outcomezero(bottom);

}
