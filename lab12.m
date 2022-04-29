// MAGMA lab assignment #2
// Leonardo Errati



// -------------------------------------------------
// ---------------------- EXERCISE 1 ---------------
// -------------------------------------------------



//function sortForShanks(list)
//    // - list is a list of two entries coming from Shanks's algorithm
//    // no controls are necessary, user has no access to this function
//
//    sortee := Sort([list[i] : i in [1..#list]]);
//    output := [];
//    
//    for i in [1..#sortee] do
//        for j in [1..#list] do
//            if sortee[i] eq list[j] then
//                output := Append(output,list[j]);
//            end if;
//        end for;
//
//    end for;
//    return output;
//end function;
// ^ apparently not useful, so there it goes 

function search(list,value)
    max := #list;
    min := 1;
    //printf"max: %o, min: %o, element searched: %o",max,min,value;

    if value lt list[min][2] or value gt list[max][2] then
        return -1,false;
    end if;

    while min le max do 
        
        index := (max + min) div 2;
        element := list[index][2];
        if element eq value then return index,true;
        elif element gt value then
            max := index - 1;
        else 
            min := index + 1;
        end if;
    end while;

    return -1,false;
    
end function;

function Shanks(a,b,p)
    //INPUTS: -a primitive root modulo p;
    //-b element of Fp*;
    //-p prime.
    //OUTPUTS: -t integer.

    N := p-1;
    m := Ceiling(Sqrt(N));

    L1:=[<j,Modexp(a,m*j,p)> : j in [0..m-1]]; // first list
    L2:=[<i,b*Modexp(a,-i,p) mod p>  : i in [0..m-1]]; // second list
    //printf"List 1: %o\n",L1;
    //printf"List 2: %o\n",L2;
    L1indices:=[L1[i][2] : i in [1..m]]; // lists of indices for sorting purposes
    L2indices:=[L2[i][2] : i in [1..m]];

    ParallelSort(~L1indices,~L1); // < sorts both lists according to the first
    ParallelSort(~L2indices,~L2);

    //printf"List 1 (sorted): %o\n",L1;
    //printf"List 2 (sorted): %o\n",L2;

    found := false;

    //k := 1;
    //q := 1;

    //while (k in [0..#L2-1]) and (not found) do
    //
    //    k := k + 1;
    //    q := 1;
    //    while (not found) and (q in [1..#L1]) do
    //        if L1[q][2] eq L2[k][2] then
    //            found := true;
    //        else
    //            q := q+1;
    //        end if;
    //    end while;

    //end while;
    //if found then return m*L1[q][1]+L2[k][1];
    //end if;
    
    index := 0;
    k := 0;

    while (k in [0..m-1]) and (not found) do
        k := k + 1;
        index,found:=search(L1,L2[k][2]);
    end while;
    if found then return m*L1[index][1]+L2[k][1];
    end if;

    return -1;

    // this algorithm is fast and excellent and
    // has my full approval

end function;

procedure testShanks()
    // all the tests in our 
    a := [3,106,6,19,11,29];
    b := [525,12375,248388,24717,41387,5953042];
    p := [809,24691,458009,48611,81799,15239131];

    for i in [1..6] do
        time s:=Shanks(a[i],b[i],p[i]);
        if Modexp(a[i],s,p[i]) eq b[i] mod p[i] then
            printf"Shanks: (%7o)^(%7o) = %7o mod %7o\n",a[i],s,b[i],p[i];
        else 
            printf"Shanks failed: (%7o)^(%7o) != %7o mod %7o\n",a[i],s,b[i],p[i];
        end if;
    end for;

end procedure;




// -------------------------------------------------
// ---------------------- EXERCISE 2 ---------------
// -------------------------------------------------



function PollardRho(alpha,beta,p)

    // divided := p div 3; < used for the old partition, now it's better
    N := p-1;
    //S1:=[i : i in [1..N] | i mod 3 eq 1];
    //S2:=[i : i in [1..N] | i mod 3 eq 0];
    //S3:=[i : i in [1..N] | i mod 3 eq 2];
    // ^ partitions according to the book, why not follow it?
    //   (now deleted for speed, see below)

    function Pollardf(x,a,b) // Pollard's function
        //printf"  PollardF input: %o, %o, %o\n",x,a,b;
    if x mod 3 eq 1 then // < evaluating the partition this way is MUCH faster
        return beta*x mod p,a,b+1 mod N;
    elif x mod 3 eq 0 then
        return Modexp(x,2,p),2*a mod N, 2*b mod N;
    elif x mod 3 eq 2 then
        return alpha*x mod p, a+1 mod N, b;
    end if;
    end function;

    x,a,b:=Pollardf(1,0,0);
    xprime,aprime,bprime := Pollardf(x,a,b);
    //i := 1; 
    //printf"%3o | (%3o,%3o,%3o)  (%3o,%3o,%3o) \n",i,x,a,b,xprime,aprime,bprime;
    while (x - xprime mod p ne 0) do
        //i := i+1;
        x,a,b:= Pollardf(x,a,b);
        xprime,aprime,bprime := Pollardf(xprime,aprime,bprime);
        xprime,aprime,bprime := Pollardf(xprime,aprime,bprime);
        //printf"%3o | (%3o,%3o,%3o)  (%3o,%3o,%3o) \n",i,x,a,b,xprime,aprime,bprime;

    end while;
    d := GCD(bprime-b,N);//d;

    if d ne 1 then // if the GCD is not 1 ...........
        // here y(b'-b) = (a-a') mod N with a non-invertible (b'-b),
        // but there are d different solutions to the equation

        // let us write ax = b mod n, if d=gcd(a,n) is not 1 then
        // all solutions are of the form x0 and y0 (mod n/d) for 
        // certain values of (x0,y0) 
        
        m := N div d;
        solution := 0;
        //
        for k in [1..m] do 
            if (k*(bprime-b)) mod m eq (a-aprime) mod m then 
                solution := k;
                break;
            end if;
        end for;
        sol := [solution + i*m : i in [0..d-1]];
        // sol;

        // ---------------------------------------------------------------------------
        // what follows is the algorithm using the non-clever way to find all solutions,
        // I want to note that the clever way is not that fast compared to this one - and 
        // if the reader wishes to run the automated tests, it will appreciate how 
        // slow this still is

        // I built this just to try that, and I was surprised the overhead was not
        // that much appreciable...

        //sol2:=[];
        //deltab := bprime-b mod N; deltab;
        //deltaa := a-aprime mod N; deltaa;

        //for k in [1..N] do 
        //    if (k*(deltab) mod N) eq ((deltaa) mod N) then 
        //        sol2:=Append(sol2,k);
        //end if;
        //end for;
        //sol2;

        // ---------------------------------------------------------------------------

        for s in sol do // for every solution, try if it fits (there are not many of them)
            if (Modexp(alpha,s,p) - beta) mod N eq 0 then return s;
            end if;
        end for;
        return -1; // < clear fatal error
    else // ...........else, we are the nice case
        return (a-aprime mod N)*Modinv((b-bprime),N) mod N;
    end if;

    // at first I very much did not like this algorithm since it seemed wonky, sloppy and slow, but
    // the current implementation is FAST, so I must reconsider my stance on PollardRho

    // the algorithm has my utmost appreciation

end function;

procedure testPollardRho()
    a := [3,106,6,19,11,29];
    b := [525,12375,248388,24717,41387,5953042];
    p := [809,24691,458009,48611,81799,15239131];

    for i in [1..6] do
        time s:=PollardRho(a[i],b[i],p[i]);
        if Modexp(a[i],s,p[i]) eq b[i] mod p[i] then
            printf"PollardRho: (%7o)^(%7o) = %7o mod %o\n",a[i],s,b[i],p[i];
        else 
            printf"PollardRho failed: (%7o)^(%7o) != %7o mod %7o\n",a[i],s,b[i],p[i];
        end if;
    end for;

end procedure;