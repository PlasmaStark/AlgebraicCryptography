// -------------------------------------------------
// ---------------------- EXERCISE 1 ---------------
// -------------------------------------------------

function PollardRhoEC(p, A, B, P, Q, N)
    // -p prime;
    // -A,B parameters defining the curve;
    // -P, Q=kP points on the curve;
    // -N is the order of P.

    // some of the values commented out (e.g. s = 3) are from a sanity check I performed
    // using the same parameters as the book

    s := Random([20..30]);
    //s := 3;
    F := FiniteField(p);
    EC := EllipticCurve([F | A,B]);
    // ^    y^2 = x^3+Ax+B
    Ms := []; // < list of all the Ms
    //ai := [4,9,19];
    //bi := [3,17,6];
    for i in [1..s] do
        a := Random([1..N]);
        b := Random([1..N]);
        M := a*P + b*Q;
        Ms := Append(Ms,<M,a,b>); // < keep track of them
    end for;

    a := Random([1..N]);
    b := Random([1..N]);
    // a := 3; b:= 5;
    d := 0;
    function pollardF(point,apoint,bpoint)
        x := point[1];
        // printf"x:%o\n",x;
        i := IntegerRing()!x mod s;
        return point + Ms[i+1][1],apoint+Ms[i+1][2],bpoint+Ms[i+1][3];
    end function;

    point := a*P + b*Q;
    //pointprime,aprime,bprime := pollardF(point,a,b);
    found := false;
    j := 1;
    while not found do
        
        
        if j eq 1 then 
            
            pointprime,aprime,bprime := pollardF(point,a,b); 
            if IntegerRing()! pointprime[3] eq 0 then return (-aprime*Modinv(bprime,N)) mod N;
            end if; // ^ there is a chance this check could be performed in PollardF, but I did not find a way to do so
                    // and this ugly thing works
            point,a,b := pollardF(point,a,b);
        else 
            point,a,b := pollardF(point,a,b);
            pointprime,aprime,bprime := pollardF(pointprime,aprime,bprime); 
        end if;
            if IntegerRing()! pointprime[3] eq 0 then return (-aprime*Modinv(bprime,N)) mod N;
            end if;
        pointprime,aprime,bprime := pollardF(pointprime,aprime,bprime);
        if IntegerRing()! pointprime[3] eq 0 then return (-aprime*Modinv(bprime,N)) mod N;
        end if;
        //printf"P_%3o |  (%4o,%4o)\n",j,point[1],point[2];
        //printf"    P_%3o |  (%4o,%4o)\n",2*j,pointprime[1],pointprime[2];
        if point eq pointprime then 
            found := true; 
            
        end if;
        j := j + 1;
    end while;
    // printf"found a match: %o | %o\n",point,pointprime;
    // printf"   a: %5o  b:%5o\n",a,b;
    // printf"   a': %5o  b':%5o\n",aprime,bprime;
    
    
    d := GCD(bprime-b,N);
    //printf"GCD : %o\n",d;

    // from now down it proceeds exactly as my submission from the 
    // past week - it is actually the copy-pasted code with a minor modification,
    // I reduced the amount of comments

    if d ne 1 then 
        
        m := N div d;
        solution := 0;
        for k in [1..m] do 
            if (k*(bprime-b)) mod m eq (a-aprime) mod m then 
                solution := k;
                break;
            end if;
        end for;
        sol := [solution + i*m : i in [0..d-1]];
        
        for s in sol do // for every solution, try if it fits (there are not many of them)
            if s*Q eq P then return s;
            end if;
        end for;
        return -1; // < clear fatal error
    else // ...........else, we are the nice case
        return (aprime-a mod N)*Modinv((b-bprime),N) mod N;
    end if;

    return -1;
end function;

procedure testPollard()

    printf" ----------- testing Pollards algorithm -----------\n";
    printf" --------------------------------------------------\n\n";

    EC := EllipticCurve([FiniteField(1093) | 1,1]);
    P := EC![0,1];
    Q := EC![413,959];
    time a:=PollardRhoEC(1093,1,1,P,Q,1067);
    if P*a eq Q then printf"%16o * %4o  is %16o - correct\n",P,a,Q;
    end if;
    
    EC := EllipticCurve([FiniteField(229) | 1,44]);
    P := EC![5,116];
    Q := EC![155,166];
    time a:=PollardRhoEC(1093,1,1,P,Q,239);
    if P*a eq Q then printf"%16o * %4o  is %16o - correct\n",P,a,Q;
    end if;
    
end procedure;


// -------------------------------------------------
// ---------------------- EXERCISE 2 ---------------
// -------------------------------------------------

function ECaddition(P,Q,A,N) 
    // self-explanatory, implements the addition on an EC
    // - Q is a touple <x,y>
    // - P is a touple <x,y>
    // - A,N are parameters from the main function
    if P ne Q then // case P =\= Q
        denum := Q[1] - P[1];
        d := GCD(N,denum);
        if d eq 1 then // nice case
            coeff := ((Q[1] - P[1])*Modinv(denum,N)) mod N;
            return (coeff^2 - P[1] - Q[1]) mod N, (coeff*(P[1]-Q[1]) - P[2]) mod N, true;
        else return d,d,false; end if; // error, d is transmitted through the stack call
    else // case P = Q
        denum := 2*P[2];
        d := GCD(N,denum);
        if d eq 1 then
            coeff := ((3*P[1]^2 + A)*Modinv(denum,N)) mod N;
            return  (coeff^2 - P[1] - Q[1]) mod N, (coeff*(P[1]-Q[1]) - P[2]) mod N, true;
        else return d,d,false; end if;
    end if;
end function;

function ECpointmultiplication(P,k,A,N)
    // EC multiplication, returns k*P
    // - P is a touple <x,y>
    // - A,N are parameters from the main function
    iter := Intseq(k,2);
    temppoint := P; // < increased by x2 each round
    value := <0,0>; // < total value till now
    for i in iter do
        if i eq 1 then // in this case, add value 
            a,b,bool := ECaddition(value,temppoint,A,N);
            if bool eq false then return <a,a>, false; end if;
            value := <value[1] + a, value[2] + b>;
        end if;

        // increase x2 the temp multiplier 
        a,b,bool := ECaddition(temppoint,temppoint,A,N);
        if bool eq false then return <a,a>, false; end if;
        temppoint := <temppoint[1] + a, temppoint[2] + b>;
    end for;
    return value,true;
end function;

function padic_evaluation(N,p) // < from my lab1 submission, copy-pasted
    // computes the p-adic evaluation of N, i.e. it 
    // outputs (in order) k,r such that N=(p^k)*r

    // no checks are done, the user has no access to this 
    // function; we shall assume other functions using this
    // already performed the right checks

    k := 0;
    // if p=1 then there is no need to do any of this...
    if p eq 1 then return 1,N;
    end if;
    // ... otherwise, till N is divisible by p, 
    // divide it and go on; eventually N will not be 
    // divisible by p and become r, while we keep the exponent
    while N mod p eq 0 do
        k:=k+1;
        N:= IntegerRing()! N div p; // < I absolutely do not trust Magma
    end while;
    return k,N;
end function;

function LenstraAlgorithm(N)
    //-N integer to be factorized.
    //OUTPUTS: -a factor of N.

    // NOTE that instead of trying a new curve (after the current one fails) in the current algorithm run, I
    // much prefer returning a recursive call so that entropy and PRNGs do the job for me 
    // it might not be suitable for long calculations since those calls are expensive

    A := Random([1..N]);
    a := Random([1..N]);
    b := Random([1..N]);
    B := (b^2-a^3-A*a) mod N;
    // E : y^2 = x^3 + Ax + B

    delta := GCD(27*B^2+4*A^3,N);

    if (delta eq N) then 
        return LenstraAlgorithm(N); // bad factor, try again
    elif (delta ne 1) then 
        return delta; // good factor, we like it 
    end if;

    P := <a,b>;
    k := 100; j := 0;

    while (j le k) do 
        j := j + 1;
        Q,possible := ECpointmultiplication(P,j,A,N);
        // "possible" contains info about errors in the computation, if an error 
        // occurred we just (possibly) found a non-trivial factor
        // - TRUE  | no error occurred
        // - FALSE | erorr occurred
        
        if (not possible) and (Q[1] ne N) then return Q[1]; 
        elif (not possible)  then break; // try again if the factor is trivial
        end if;
    end while;
    return LenstraAlgorithm(N);
end function;


function Lenstra(N)
    //-N integer to be factorized, I assumed it to be > 0
    //OUTPUTS: -Seq factorization of N.

    factors :=[];

    while N ne 1 do // while N is > 1...
        if IsPrime(N) then // ... and not prime...
            // this check is needed because my LenstraAlgorithm avoids returning N,
            // a slight modification can change this possibly ugly - but nicer - behaviour,
            // it suffices to perforn a check p =\= N after each iteration here
            factors := Append(factors,<N,1>);
            N := 1;
            break;
        end if;

        p := 0;
        while (not IsPrime(p)) do // ... try to iteratively find a prime factor (that indeed EXISTS)...
            //printf"%5o | inputing to Lenstra : %o\n",N,p;
            p := LenstraAlgorithm(N);
            //printf"  %5o | Lenstra output : %o\n",N,p;
        end while;
        //printf"  %5o | %o is prime \n",N,p;
        // ... the p-adic evaluation tells us all about this factor ...
        k,r := padic_evaluation(N,p);
        //printf"  %5o | %o=%o ^ %o * %o is prime \n",N,N,p,k,r;
        factors := Append(factors,<p,k>);
        N := r; // ... so we can go on till N = 1
    end while;

    return factors;
end function;

procedure testLenstra()
    correct := 0;
    // some header for a random slap of epicness:
    printf" ----------- testing Lenstras algorithm -----------\n";
    printf" --------------------------------------------------\n\n";

    times := 10000;
    max := 10000000;
    for i in [1..times] do
        rand := Random(2,max); //printf"  N = %o \n",rand;
        test := Lenstra(rand);
        
        fun := Factorization(rand);
        empiricalproduct := &*[test[i][1]^test[i][2] : i in [1..#test]];

        if empiricalproduct ne rand then 
            printf"  N = %o |  incorrect factorization: %o \n",rand,test;
        end if;

        if (test subset fun) and (fun subset test) then 
            correct := correct + 1;
        else 
            printf"  N = %o |  incorrect form: %o \n",rand,test;
            printf"  N = %o |  MAGMA function: %o\n\n",rand,fun;
        end if;
    end for;
    if times - correct ne 0 then
        printf" --------------------------------------------------\n\n";
    end if;
    printf"    correctness: %o out of %o \n",correct,times;
    printf"         errors: %o out of %o \n",times-correct,times;
    printf" empirical err. probability: %o percent \n\n", RealField()! (100*(times-correct))/times;

end procedure;

procedure LenstraDoesMyHomework()
    // the tests we were asked to perform
    tests := [187, 6887, 589, 26167];
    for t in tests do
        Lenstra(t);
    end for;
    // thanks Lenstra
end procedure;