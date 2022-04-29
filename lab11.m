// MAGMA lab assignment #1
// Leonardo Errati

// as always, I prefer writing clear programs than making fast convoluted ones;
// nevertheless, some cool observations can be made while doing this that bring 
// to faster programs




// -------------------------------------------------
// ---------------------- EXERCISE 1 ---------------
// -------------------------------------------------


function padic_evaluation(N,p)
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

function MillerRabinTest(N,times)
    // INPUTS:  - N integer to be tested.
    // OUTPUTS: - true if and only if N is prime.
    //                   ^ (iff? what?)


    // [preliminary checks] ---------------------------

    error if (not IsIntegral(N)), "the test requires inputs to be in Z";

    // N is assumed to be integer, so either positive or negative
    // if N is even != 2, or is 0, or is +-1, there is no need to perform this...
    // note that MAGMA outputs IsEven(0) = true, I just want to be specific 
    if N eq 2 or N eq -2 then return true; 
    end if;
    if IsEven(N) or (N eq 0) or (N eq 1) or (N eq -1) then return false; 
    end if;
    // ... while if N is negative, we can proceed with -N...
    if N lt 0 then N := -N; 
    end if;

    // [setting values] ---------------------------

    k,m := padic_evaluation(N-1,2);
    probprime := true;

    // [core algorithm] ---------------------------

    for t in [1..times] do
        if (probprime) then
            roundpassed:=false;

            a := Random(1,N-1);
            while GCD(a,N) ne 1 do 
                a := Random(1,N-1);
            end while;

            b := Modexp(a,m,N);
            if b mod N eq 1 then 
                probprime:=true; roundpassed:=true;
            end if;

            for i in [0..k-1] do
                if b mod N eq -1 mod N then 
                    probprime:=true; roundpassed:=true;
                else b := Modexp(b,2,N);
                end if;
            end for;

            if (not roundpassed) then
                probprime:=false;
            end if;

        end if;
    end for;

    return probprime;
end function;

function refineFactorization(list)
    // input is a LIST containing factors, e.g.
    // [ <3, 4> ]
    // the goal is to split it as follows for further use
    // [ <3, 1>, <3, 1>, <3, 1>, <3, 1> ]

    factors := [];
    for l in list do 
    i := l[2];
        while i gt 0 do
            factors:=Append(factors,<l[1],1>);
            i := i - 1;
        end while;
    end for;

    return factors;
end function;

procedure TestTest(times,testrounds,max)
    // tests the RabinMiller test
    // - times is the times this is done
    // - max is the range that will serve as random pool
    // - testrounds is the rounds the test performs
    correct := 0;
    errors := 0;
    // some header for a random slap of formalism:
    printf" ---------- testing RabinMiller ----------\n";
    printf" %o round(s), %o numbers in [-%o,%o] \n",testrounds,times,max,max;
    printf" error probability: %o precent\n", (0.25^testrounds)*100;
    printf" -----------------------------------------\n\n";


    for i in [1..times] do
        rand := Random(-max,max);
        test := MillerRabinTest(rand,testrounds);
        fun := IsPrime(rand);
        if test eq fun then 
            // the test worked correctly
            correct := correct + 1;
        else 
            // the test was incorrect, print the result and 
            // the factorization of the number
            errors := errors + 1;
            printf"N = %o | user function: %o | MAGMA function: %o\n",rand,test,fun;
            fact := refineFactorization(Factorization(rand));
            if rand lt 0 then 
                fact := Append(fact,<-1,1>);
            end if;
            printf"     (erorr %o) %o = %o", errors,rand,fact[1][1];
            for f in [2..#fact] do
                printf" x %o", fact[f][1];
            end for;
            printf"\n";
        end if;
    end for;
    printf" -----------------------------------------\n\n";
    printf"    correctness: %o out of %o \n",correct,times;
    printf"         errors: %o out of %o \n",errors,times;
    // ^ errors could be counted with times-correct, but I wanted to 
    // print the error number above, so I have the variable...
    printf" empirical err. probability: %o percent \n", RealField()! (100*errors)/times;
    // ^ theoretical error probability is quite possibly not .25^rounds 
    // in our case, we authomatically removed even numbers, accepted negative
    // integers, etc...
    // still interesting though
    end procedure;



// -------------------------------------------------
// ---------------------- EXERCISE 2 ---------------
// -------------------------------------------------



function Pollardp(N,B)
    // pollard's p-1 factorization algorithm
    // INPUTS:  -N integer to be factorized,
    //          -B bound.
    // OUTPUTS: -a non-trivial factor of N.
    // this is NOT verbatim from the pdf we were given, that 
    // algorithm returned FAILURE in some cases, which is ugly 

    // NOTE that this version tries everything possible (multiple stack calls) to 
    // find a non-trivial factor, hence this function should NOT be used with N prime
    // if one wishes to avoid stack overflows - that is suggested only for the bravest
    // (this is not an issue, user has no access to this function and the factorizing 
    // function checks for primality; the test can be moved right here if one fancies that)

    error if not IsIntegral(N), "invalid input"; 

    if IsEven(N) then return 2;
    end if;
    // ^ trivial case; note that i can assume WLOG N>0 since the main algorithm
    // (the one using this function) already checks that and adds -1 to the factors

    if N eq 1 then return [<1,1>]; 
    end if;

    // a:=2; // < if N is odd, 2 is coprime
    // ^ I do not recommend this, Pollardp(85,1) is stuck in an endless loop of 
    // increasing and decreasing: 
    // > Pollardp(85,1);
    // increasing: 2
    // 4 mod 85
    // increasing: 3
    // 4 mod 85
    // 64 mod 85
    // increasing: 4
    // 4 mod 85
    // 64 mod 85
    // 1 mod 85
    // reducing: 3 ............. eventually leading to a stack overflow
    // and so does Pollardp(85,2), and so on
    // it is very worth the effort of choosing a random a,
    // after all there are phi(N) of them
    a := 0;
    while GCD(a,N) ne 1 do
        a := Random(1,N);
    end while;

    for j in [2..B] do
        a:=Modexp(a,j,N);
         //printf"%o mod %o\n",a,N;
    end for;
    d:=GCD(a-1,N);
    
    // now the cases in which the algorithm from the book fails,
    // that can somewhat elegantly be saved:
    if d eq 1 then //printf"increasing: %o\n",B+1; 
        d := Pollardp(N,B+1); 
    elif d eq N then //printf"reducing: %o\n",B-1; 
        d := Pollardp(N,B-1); 
    end if;

    return d;
end function;

function Pollard(n,B)
    // full factorization via pollard's p-1 factorization algorithm
    // INPUTS:  -n integer to be factorized,
    //          -B bound.
    // OUTPUTS: -Seq factorization of n.

    // NOTE that if the integer is negative I decided to output also -1 together
    // with the factors, e.g. Pollard(-7,1) -> [ <-1, 1>, <7, 1> ]
    // this is done by design, and of course is arguable

    error if not IsIntegral(n), "invalid input"; 
    fact:=[];
    
    if n eq 1 then return [<1,1>]; 
    elif n eq -1 then return [<-1,1>];
    end if;

    if n lt 0 then // < handles the n<0 cases, since it is specified the INPUT could be negative
        fact := Append(fact,<-1,1>); n := -n; 
    end if;
    
    k := n; // < set to 0 but this value is irrelevant for now

    while k ne 1 do
        // if p is a strong Fermat pseudoprime, then we have
        // enough evidence to add it to the list of factors...
        // the number of rounds of MR (4) is chosen in order not to 
        // burden the algorithm with too many calculations, but
        // it is inevitable; I am sure MAGMA's IsPrime is way faster,
        // although I consider using that a tad cheeky 
        if MillerRabinTest(k,4) eq true then // < redundant notation, but I wanted to be easy on the reader
            fact := Append(fact,<k,1>);
            //printf" %o passed the test \n",k;
            k := 1;
        else 
            p := Pollardp(k,B);
            // force the search of a prime factor...
            while MillerRabinTest(p,4) ne true do
                p := Pollardp(p,B);
            end while;
            // ... and proceed by exhaustion
            m,k:= padic_evaluation(n,p);
            fact := Append(fact,<p,m>);
            //printf" %o is the output of Pollard(%o,%o) \n",p,n,B;
            //printf" %o-adic evaluation of %o : %o, %o \n",p,n,m,k;
            n := k; // < the algorithm proceeds to factor the rest
        end if;
    end while;
    return fact;
end function;


procedure TestPollard(times,max)
    // tests the Pollard factorization (more specifically, the function exploiting it...)
    // - times is the times this is done
    // - max is the range that will serve as random pool
    correct := 0;
    // some header for a random slap of epicness:
    printf" ---------- testing Pollard's p-1 method ----------\n";
    printf" %o round(s), numbers in [2,%o] \n",times,max;
    printf" --------------------------------------------------\n\n";

    for i in [1..times] do
        rand := Random(2,max); 
        test := Pollard(rand,2);
        fun := Factorization(rand);
        if (test subset fun) and (fun subset test) then // < by using the two subsets I do not longer
                                                        //   need to reorder the arrays, as "eq" would require
            // the test worked correctly
            correct := correct + 1;
        else 
            // the test was incorrect, print the result and 
            // the factorization of the number
            printf"  N = %o |  user function: %o \n",rand,test;
            printf"  N = %o | MAGMA function: %o\n\n",rand,fun;
        end if;
    end for;
    if times - correct ne 0 then
        printf" --------------------------------------------------\n\n";
    end if;
    printf"    correctness: %o out of %o \n",correct,times;
    printf"         errors: %o out of %o \n",times-correct,times;
    printf" empirical err. probability: %o percent \n\n", RealField()! (100*(times-correct))/times;

end procedure;



// -------------------------------------------------
// ---------------------- EXERCISE 3 ---------------
// -------------------------------------------------

// the following functions are done to somewhat speed up 
// the execution and remove unused variables:

function padic_EXPONENT(N,p)
    // returns the exponent of the p-adic evaluation
    if p eq -1 then
        if N le 0 then return 1; 
        else return 0;
        end if;
    end if;

    k := 0;
    if p eq 1 then return 1,N;
    end if;
    while N mod p eq 0 do
        k:=k+1;
        N:= IntegerRing()! N div p;
    end while;
    return k;
end function;

function padic_MULTIPLIER(N,p)
    // returns the coefficient of the p-adic evaluation
    k := 0;
    if p eq 1 then return N;
    end if;
    while N mod p eq 0 do
        k:=k+1;
        N:= IntegerRing()! N div p;
    end while;
    return N;
end function;

function represent(m,N)
    // represent m mod N in such a way that m has the representant 
    // with the lowest module possible 
    if m lt N/2 then 
        return m;
    else return m-N;
    end if;
end function;

function DixonSieve(N,B)
    // Dixon's sieve: N is the integer, B the bound
    // note that B will now become a list of primes...

    // I find fascinating the following algorithm is able
    // to factorize N=21,77 with bound 2 (there are more),
    // it should not be posible, I quite possibly 
    // just broke the universe
    // NOTE: I modified the Dixon() function to include a depth parameter
    // (see below), now it is perfectly normal Dixon factorizes these nomber with 
    // a non-ideal bound... but it did so even before my modification in some cases...

    B := PrimesUpTo(B);
    x := []; y := []; // < the future bases and their exponentiations
    i := 0; m := 1;
    
    // old code to find the xi and yi, the new code works even better
    // inf := Floor(Sqrt(N));
    //while #x le #B+1 do
    //    xi := inf + i; 
    //    yi := represent(xi ^ 2 mod N, N);
    //    if PrimeDivisors(yi) subset B then 
    //        x := Append(x,xi);
    //        y := Append(y,yi);
    //    end if;
    //    i := i+1;
    //end while;

    while #x le #B+1 do // find the B-smooth bases
        xi := Floor(Sqrt(m*N)) + (i mod 2); // < bases
        yi := represent(xi ^ 2 mod N, N); // < their exponentiations
        //yi; xi;

        if yi ne 0 then
            if PrimeDivisors(yi) subset B then 
            x := Append(x,xi);
            y := Append(y,yi);
            end if;
        end if;
        i := i+1;
        if (i eq 2) then
            m := m + 1; i := 0;
        end if;
    end while;
    // ^ note that some combinations of xi and yi could already tell us
    // something about the factorization, but I did not want to exploit them
    // to stay true to the core algorithm

    B := [-1] cat B; // < from now on let us consider also -1 
                     //   (adding it before would have been a waste of computational power)
    exps := [      [ padic_EXPONENT(yi,p) : p in B]       : yi in y];
    exps_mod2 := [ [ GF(2) ! exp : exp in exps[i]] : i in [1..#y]]; 
    // ^ one list could have been enough, but I do not trust Magma and wanted to check everything

    //for i in [1..#y] do
        //printf"%o : %o \n", y[i], exps[i];
        //printf"   list of factors mod2 | %o \n", exps_mod2[i];
    //end for;
    //    printf"\n";
    // ^ debug stuff
    
    K:= Rows(  KernelMatrix(  Matrix(#x,#B,exps_mod2)  )  ); // A is the usual matrix for this algorithm

    //printf"factor base | %o \n", B;
    //printf"bases   | %o \n", x;
    //printf"squares | %o \n", y;
    //printf" - the kernel contains %o element(s) \n \n", #K;
    // ^ debug

    unsolved := true; // < I like naming booleans like this, it feels perfect to 
                      //   read them in conditions ("while unsolved do...")

    d1 := N;
    d2 := 1;

    for k in K do
        if unsolved then
            indices := [i : i in [1..#x]  | k[i] ne 0];
            selectedx := [x[j] : j in indices];
            selectedy := [y[j] : j in indices];

            X := Floor(Sqrt(&*[a^2 : a in selectedx])) mod N;
            Y :=  Floor(Sqrt(&*selectedy)) mod N;

            d1 := GCD(N,X+Y);
            d2 := GCD(N,X-Y);
            
            //printf"iteration on %o | X: %o, Y: %o, d1: %o, d2: %o \n", k, X, Y, d1, d2;
            //printf"   selected indices | %o \n", indices;
            //printf"   selected x | %o \n", selectedx;
            //printf"   selected y | %o \n", selectedy;
            // ^ debug

            // now a lot of cases follow, I wanted to be pedantic
            if (d1 ne 1) and (d2 ne 1)  then
                unsolved := false;
            end if;

            if (X+Y eq 0) and (not unsolved) then
                return GCD(X-Y, N), N div GCD(X-Y, N);
            elif (X+Y eq N) and (not unsolved) then
                return GCD(X,N), N div GCD(X,N);
            end if;
            if (X-Y eq 0) and (not unsolved) then
                return GCD(X+Y, N), N div GCD(X+Y, N);
            elif (X-Y eq N) and (not unsolved) then
                return GCD(X,N), N div GCD(X,N);
            elif not unsolved  then
                return d1, N div d1;
            end if;

            
        end if;
    end for;
    return d1,d2;
end function;

function factorADDtolist(list,factor)
    added := false;
    for i in [1..#list] do
        if list[i][1] eq factor[1] then
        list[i][2] := list[i][2] + factor[2];
        added := true;
        //printf"added factor %o to list %o",factor,list;
        end if;
    end for;
    if not added then
        list := Append(list,factor);
        //printf"catted factor %o to list %o \n",factor,list;
    end if;
    return list;
end function;

function factorlistMERGE(list1,list2)
    for i in [1..#list2] do
        factorADDtolist(list1,list2[i]);
    end for;
    return list1;
end function;


function DixonDepthed(N,B,depth)
    
    // factorizes positive integer N with prime bound B > 0
    // - N positive integer
    // - B positive prime bound 
    // - depth is used to avoid overflow (see below)

    // I use depth to avoid stack overflow: Dixon does not have
    // a procedure to deal with wrong bound B in itself, so 
    // I tried coming up with something...

    //printf"depth: %o\n",depth; // < debug
    if depth gt 15 then
        return DixonDepthed(N,10*B,0);
    end if;
    // ^ if Dixon notices it is taking too long to factorize, it tries
    // modifying the bound in a somewhat ingenious way - it is not the 
    // ideal solution, I just wanted to have fun and I liked the idea

    // also notice that this renders useless the tentative to factorize a number
    // that would require more than a 15-deep recursion, so Dixon should also include
    // a fourth parameter dictating when depth is too large, and so on...

    // this is the end of my shenanigans, the algorithm proceeds as usual

    factors := []; // < list of factors
    if IsPrimePower(N) then
        _,base,pow:=IsPrimePower(N);
        factors := Append(factors,<base,pow>);
        a:=1; b:=1;
    else 
        a,b := DixonSieve(N,B);
    end if;

    // again a lot of cases, but I wanted to be pedantic;
    // of course if one of a,b is 1 we need to proceed differently, and 
    // the same goes for when both are 1

    // then, if the non-1 ones are prime powers, we can deal with them
    // and store them in the factorization; if they are not, Dixon 
    // is called again

    //printf"%o : factors: %o, %o \n",N , a,b;
    if (a ne 1 and b ne 1) then
        //printf"a and b are not 1 \n";
        if (IsPrimePower(a)) then 
        _,base,pow:=IsPrimePower(a);
        factors:=Append(factors,<base,pow>);
        else
            factors := factors cat DixonDepthed(a,B,depth+1);
        end if;
        if (IsPrimePower(b)) then
        _,base,pow:=IsPrimePower(b);
        factors:=Append(factors,<base,pow>);
        else
            factors := factors cat DixonDepthed(b,B,depth+1);
        end if;
    elif (a ne 1 and b eq 1) then
        //printf"b is 1\n";
        if (IsPrimePower(a)) then
        _,base,pow:=IsPrimePower(a);
        factors:=Append(factors,<base,pow>);
        else
            factors := factors cat DixonDepthed(a,B,depth+1);
        end if;
    elif (a eq 1 and b ne 1) then
        //printf"a is 1\n";
        if (IsPrimePower(b)) then
        _,base,pow:=IsPrimePower(b);
        factors:=Append(factors,<base,pow>);
        else
            factors := factors cat DixonDepthed(b,B,depth+1);
        end if;
    end if;

    factors_clean := [];  // < reordered factors
    for i in [1..#factors] do
        factors_clean := factorADDtolist(factors_clean,factors[i]);
    end for;

    return factors_clean;

    // while I build Pollard without using Magma's functions, e.g. using our 
    // primality test and ignoring IsPrimePower, here I wanted to take some
    // more freedom with these since Dixon is already heavy as-is

    // the reordering of factors could have been avoided, but I am a perfectionist
end function;

function Dixon(N,B) // < this is the classic version... somewhat
    return DixonDepthed(N,B,15);
end function;


//   N = 408 |  user function: [ <2, 6>, <3, 1>, <17, 1> ]
// todo: check this, wrong factorization (done)

procedure TestDixon(times,max,B)
    // tests the Dixon factorization (or rather, the function exploiting it...)
    // - times is the times this is done
    // - max is the range that will serve as random pool
    // - B is the bound
    correct := 0;
    // some header for a random slap of epicness:
    printf" ---------- testing Dixon's algorithm ----------\n";
    printf" %o round(s), numbers in [2,%o] \n",times,max;
    printf" bound %o \n",B;
    printf" -----------------------------------------------\n\n";

    for i in [1..times] do
        rand := Random(2,max); 
        test := DixonDepthed(rand,2,30);
        fun := Factorization(rand);
        if (test subset fun) and (fun subset test) then 
            // the test worked correctly
            correct := correct + 1;
        else 
            // the test was incorrect, print the result and 
            // the factorization of the number
            printf"  N = %o |  user function: %o \n",rand,test;
            printf"  N = %o | MAGMA function: %o\n\n",rand,fun;
        end if;
    end for;
    if times - correct ne 0 then
        printf" --------------------------------------------------\n\n";
    end if;
    printf"    correctness: %o out of %o \n",correct,times;
    printf"         errors: %o out of %o \n",times-correct,times;
    printf" empirical err. probability: %o percent \n\n", RealField()! (100*(times-correct))/times;

end procedure;



// --------------------------------------------------
// ---------------------- EXERCISE 4 ----------------
// --------------------------------------------------


function EuclideanDivision(a,b) 
    // computes a = qb + r for positive a,b
    return a div b, a mod b;
end function;

function LongEuclideanDivision(a,b)
    // computes a list of coefficients q_i from the euclidean division

    coefficients := [];
    r := 1;

    while r ne 0 do
        q,r := EuclideanDivision(a,b);
        coefficients := Append(coefficients,q);
        //printf" %o = %o * %o + %o \n", a,b,q,r;

        a := b; 
        b := r; 
    end while;

    return coefficients;
end function;

function CFfromlist(list)
    // - list is a list of integers
    // recursively builds a continuous fraction from the classical
    // representation via a list, e.g.
    // > A := [2,5,7,9];
    // > CFfromlist(A);
    // 722/329
    // ^ if they are finite they tend to do this
    
    if #list eq 1 then
        cf := list[1];
    else
        cf := list[1] + 1 / CFfromlist([list[j] : j in [2..#list]]);
    end if;

    return cf;
end function;

function buildconvergents(q)
    // builds the convergents from the list of coefficients
    // - q is the list of coefficients

    // not the most efficient way, but I thought it was a fun thing
    // to implement (it was)
    return [ CFfromlist([q[i] : i in [1..j]]) : j in [1..#q]] ;
end function;

function Wiener(N,B)
    // Wiener's attack
    // - (N,B) is a public RSA key
    // - B satisfies a certain superior bound (OTHERWISE THIS ATTACK FAILS)

    R<x> := PolynomialRing(RealField());
    C := buildconvergents(LongEuclideanDivision(B,N)); // < CF convergents via Euclidean Division

    for j in [2..#C] do
        nprime := (1/C[j])*B - 1/Numerator(C[j]);

        // if nprime is in Z, then build a polyomial and search for integer roots 
        if IsIntegral(nprime) then
            f := x^2 - (N-nprime+1)*x + N;
            //f; < debug
            if #Roots(f) eq 2 then
                p := Roots(f)[1][1];
                q := Roots(f)[2][1];
            else 
                p := -1; q := -1; d:= -1; // < just to output something that is clearly an error,
                                          //   I do no appreciate the "return false" thingy since 
                                          //   it stops authomatic tests... this way it is just wrong!
            end if;
            // if the following conditions are satisfied, output 
            if (p gt 0) and (p le N) and (q gt 0) and (q le N) and (IsIntegral(p)) and (IsIntegral(q)) then
                p := IntegerRing()! p;
                q := IntegerRing()!q;
                //IntegerRing()! (p*q);
                d := Modinv(B,EulerPhi(IntegerRing()! (p*q)));
                return IntegerRing()! p, IntegerRing()! q,d;
            end if;
        end if;
    end for;

    // as before, I prefer a clearly wrong output than "false" since my goal is only authomated testing,
    // of course other environments would require "false" not to stall everything with weird nonsense
    return -1, -1, -1;
end function;

//160523347
//60728973

function buildWeakRSA(p,q)

    // builds a weak RSA system to test the attack

    N := p*q;
    printf"--------------'e=1? why not, it is way faster'-------------\n",p,q;
    printf"-----------------(weakening RSA since 1977)----------------\n";
    printf"RSA  primes: %o, %o\n",p,q;
    printf"RSA modulus: %o\n",N;
    printf"searching for weak exponents... ";
    d := 0;
    found := false; 
    limit := IntegerRing()! Floor(Root(N,4) / 3);
    feasible := limit gt 2;
    phiN:=(p-1)*(q-1);
    while not found and feasible do
        d := Random(2,limit); // < this should suffice if N is large enough
        if GCD(d,phiN) eq 1 then found := true;
        end if;
    end while;

    if not found then 
        d := 0;
        e := 0;
        printf"not found.\n";
    else 
        e := Modinv(d,phiN);
        printf"d = %o found, inverse: %o \n", d, e;
    end if;

    return p,q,N,e,d;
end function;

function generatePrimeInInterval(a,b)
    // generates a prime in the interval (a,b)
    prime:= false;
    p := 0;
    while not prime do
        p := Random(a,b);
        if IsPrime(p) then prime := true;
        end if;
    end while;
    return p;
end function;

procedure WienerTest(times)
    // tests the Wiener Attack for "times" times
    for i in [1..times]do 
        printf"--------- TRIAL NUMBER %o ---------------------------------\n",i;
        p := generatePrimeInInterval(1000,100000);
        q := generatePrimeInInterval(1000,100000); // < it's highly improbable they are equal... we hope...

        p,q,N,e,d := buildWeakRSA(p,q);

        if (e ne 0) and (d ne 0) then
            printf"\n    public key: (%o,%o)\n",N,e;
            printf"\nmounting Wiener's attack on RSA... ";

            pprime,qprime,dprime := Wiener(N,e);
            if (pprime eq p) and (qprime eq q) and (dprime eq d) then
                printf"> SUCCESSFUL\n";
                printf"    private key: (%o,%o)\n",N,d;
                printf"    factorization: %o = %o x %o\n",N,p,q;
            else
                printf"> UNSUCCESSFUL\n";
                printf"    Wiener output: p = %o, q = %o\n",pprime,qprime;
                printf"    real factorization: %o = %o x %o\n",N,p,q;
                printf"    private key: (%o,%o)\n",N,d;
            end if;
        else 
            printf"\n    public key: (%o,%o)\n",N,e;
            printf"conditions impossible for this attack\n";
            printf"    private key: (%o,%o)\n",N,d;
        end if;
            printf"-----------------------------------------------------------\n";

    end for;


end procedure;