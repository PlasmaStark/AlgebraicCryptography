function isprimitive(A,p) return GCD(A cat [p]) eq 1; end function;

function pointADD(P,Q,A,B,p)
    // adds two points 
    X1 := P[1]; X2 := Q[1];
    Y1 := P[2]; Y2 := Q[2];
    Z1 := P[3]; Z2 := Q[3];
    psq := p^2;
    // ------ calculate addition w.r.t. law 1 -----------
    s1 := (X1*Y2-X2*Y1)*(Y1*Z2 + Y2*Z1) + (X1*Z2-X2*Z1)*Y1*Y2-A*(X1*Z2-X2*Z1)*(X1*Z2 + X2*Z1)-3*B*(X1*Z2-X2*Z1)*Z1*Z2;
    s2 := -3*X1*X2*(X1*Y2-X2*Y1)-Y1*Y2*(Y1*Z2-Y2*Z1)-A*(X1*Y2-X2*Y1)*Z1*Z2 + A*(Y1*Z2-Y2*Z1)*(X1*Z2 + X2*Z1) + 3*B*(Y1*Z2-Y2*Z1)*Z1*Z2;
    s3 := 3*X1*X2*(X1*Z2-X2*Z1)-(Y1*Z2-Y2*Z1)*(Y1*Z2 + Y2*Z1) + A*(X1*Z2-X2*Z1)*Z1*Z2;
    S := [s1 mod psq,s2 mod psq,s3 mod psq];
    // ------ calculate addition w.r.t. law 2 -----------
    t1 := Y1*Y2*(X1*Y2 + X2*Y1) - A*X1*X2*(Y1*Z2 + Y2*Z1)- A*(X1*Y2 + X2*Y1)*(X1*Z2 + X2*Z1) 
        - 3*B*(X1*Y2 + X2*Y1)*Z1*Z2- 3*B*(X1*Z2 + X2*Z1)*(Y1*Z2 + Y2*Z1) + A^2*(Y1*Z2 + Y2*Z1)*Z1*Z2;
    t2 := Y1^2*Y2^2 + 3*A*X1^2*X2^2 + 9*B*X1*X2*(X1*Z2 + X2*Z1) - A^2*X1*Z2*(X1*Z2 + 2*X2*Z1) - A^2*X2*Z1*(2*X1*Z2 + X2*Z1)
        - 3*A*B*Z1*Z2*(X1*Z2 + X2*Z1) - (A^3 + 9*B^2)*Z1^2*Z2^2;
    t3 := 3*X1*X2*(X1*Y2 + X2*Y1) + Y1*Y2*(Y1*Z2 + Y2*Z1) + A*(X1*Y2 + X2*Y1)*Z1*Z2 + A*(X1*Z2 + X2*Z1)*(Y1*Z2 + Y2*Z1)
        + 3*B*(Y1*Z2 + Y2*Z1)*Z1*Z2;
    T := [t1 mod psq,t2 mod psq,t3 mod psq];

    // ------ return something primitive -----------
    // note that something primitive must exist
    if isprimitive(S,p) then return S; end if;
    if isprimitive(T,p) then return T; end if; 

    R := [(S[i]+T[i]) mod p : i in [1..3]]; 
    while not isprimitive(R,p) do
        i := Random([1..p-1]);
        j := Random([1..p-1]);
        R := [(i*S[k]+j*T[k]) mod psq : k in [1..3]]; 
    end while;
    return R;
end function;

function pointMULTIPLY(n,P,A,B,p)
    // multiply the two points together - exploits double&add
    iter := Intseq(n,2);
    temppoint := P;
    value := [0,1,0];
    for i in iter do
        if i eq 1 then
            value := pointADD(value,temppoint,A,B,p);
        end if;
        temppoint := pointADD(temppoint,temppoint,A,B,p);
    end for;
    return value;
end function;

function AnomalousAttack(P,Q,A,B,p) 
    // INPUTS:  -P is a base point of the curve
    //          -Q is a point of the curve, Q=kP.
    //          -A, B are the parameters of the curve in Weierstrass form.
    //          -p is prime defining the base field of the curve
    // OUTPUT: -k is the discrete log_P of Q.

    psq := p^2;

    // ------ calculate lift parameters ---------------------

    function coefficient(T)
    // function for the lifting coefficient of point T
        x := T[1]; y := T[2]; z := T[3];
        return ( (Modexp(x,3,psq) + A*x*Modexp(z,2,psq)+B*Modexp(z,3,psq)) * Modinv(z,psq) - Modexp(y,2,psq))*Modinv(2*y,psq);
    end function;
    Plift := [P[1] , (P[2] + coefficient(P)) mod psq, P[3]];
    Qlift := [Q[1] , (Q[2] + coefficient(Q)) mod psq, Q[3]];

    // ------ apply morphism theta ---------------------
    function theta(T)
        // the morphism from the article 
        pT := pointMULTIPLY(p,T,A,B,p);
        res := pT[1] * Modinv(pT[2],p);
        return (res div p) mod p;
    end function;

    // ------ conclude the attack ---------------------
    return (theta(Qlift) * Modinv(theta(Plift),p) ) mod p;
end function;

procedure doPAPER()
    // performs the examples from the paper
    p := 730750818665451459112596905638433048232067471723;
    A := 425706413842211054102700238164133538302169176474;
    B := 203362936548826936673264444982866339953265530166;
    P := [1 , 203362936548826936673264444982866339953265530166 , 1];
    Q := [3 , 38292783053156441019740319553956376819943854515 , 1];
    AnomalousAttack(P,Q,A,B,p);

end procedure;

procedure doATTACK()
    // performs all the tests given to us 
    printf"-----------------------------------\n";
    printf"------- powered by Max Sala -------\n";
    printf"-----------------------------------\n";

    procedure claimresult(k,P,Q,A,B,p)
        printf"-------->  ";
        time kprime := AnomalousAttack(P,Q,A,B,p);
        if kprime eq k then printf"> DLOG: %10o     (correct)\n",k;
        else printf"user k: %o | correct k: %o\n",kprime,k;
        end if;
    end procedure;

    p:=223;
    A:=-1314;
    B:=621;
    P:=[1,73,1]; Q:=[152,217,1]; k:=113;
    claimresult(k,P,Q,A,B,p);
    
    //Test 2;
    P:=[5,33,1]; Q:=[111,152,1]; k := 19;
    claimresult(k,P,Q,A,B,p);

    //Test 3;
    P:=[68,5,1]; Q:=[102,212,1]; k:=206;
    claimresult(k,P,Q,A,B,p);

    //Test4:
    p:=730750818665451459112596905638433048232067471723;
    A:=425706413842211054102700238164133538302169176474;
    B:=203362936548826936673264444982866339953265530166;
    P:=[ 1, 310536468939899693718962354338996655381367569020, 1 ]; 
    Q:=[ 157801211189512312459339070833226423718315860582, 437052518347159869381356866752577105001466096269, 1 ];
    k:=1711620914;
    claimresult(k,P,Q,A,B,p);

    //Test 5:
    P:=[3, 38292783053156441019740319553956376819943854515,1];
    Q:=[346656487490943407121538398002457319000803898493, 481424869038146622462984309184774835977932379872, 1];
    k:=1833518826;
    claimresult(k,P,Q,A,B,p);


    //Test 6:
    P:= [17, 173827014976148521051073746232750578872372755801, 1];
    Q:= [203667819583814319389402073408354564719372108606, 586626210933970881663327161756122896184694265801, 1];
    k:= 861927585;
    claimresult(k,P,Q,A,B,p);

    //Test 7:
    P:=[27 , 243378333297517865785335268550248214179950561006, 1];
    Q:= [522368097011866531942103781161731359042937948081, 164097083367518761683388291236884297833403897890, 1];
    k:=1522172158;
    claimresult(k,P,Q,A,B,p);

    //Test 8:
    p:=13;
    A:=7;
    B:=3;
    P:=[6,1,1]; Q:=[0,4,1]; k:=9;
    claimresult(k,P,Q,A,B,p);

    //Test 9:
    P := [ 8, 5, 1 ]; Q := [ 6, 10, 2 ]; k:= 3;
    claimresult(k,P,Q,A,B,p);

    //Test 10:
    P:=[4,2,1]; Q:=[8,8,1]; k:=2;
    claimresult(k,P,Q,A,B,p);
    printf"-----------------------------------\n";

end procedure;