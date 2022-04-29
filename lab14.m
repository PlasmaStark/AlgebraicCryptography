// Leonardo Errati








// -----------------------------------------------------------------------------------
//                                README
// this is EXACTLY identical to the previosly received code, but it has a nicer test
// - L.
// -----------------------------------------------------------------------------------






// -----------------------------------------------------------------------------------
// --------------------------------------------------
// ---------------------- EXERCISE 1 ----------------
// --------------------------------------------------




tableIntegerToHexadecimal:=["0","1","2","3","4","5","6","7","8","9","A","B","C","D","E","F"];

function HexToBin(s) // < returns a list from MSB to LSB
return [b : b in Reverse(Intseq(StringToInteger(s[i],16),2,4)), i in [1..#s]];
end function;

function BinToHex(b) // < requires a list of bits from MSB to LSB as input
D:=Partition(b,4);
return &cat[tableIntegerToHexadecimal[Seqint(Reverse([b : b in d]),2)+1] : d in D];
end function;

function IntToBin(i, numbits)
    return Reverse( Intseq(i,2,numbits) );
end function;

function BinToInt(bit)
    return Seqint( Reverse(bit) ,2);
end function;

function listXOR(a,b)
    return ChangeUniverse([GF(2)! a[i] + b[i] : i in [1..#a]],IntegerRing());
end function;

// -----------------------------------------------------------------------------------

// ---- trapdoor permutation -------------------
function trapdoorg(x,n,exponent)
    q := x div n;
    r := x mod n;
    if (q+1)*n lt 2^160 then
        return q*n + Modexp(r,exponent,n);
    else
        return x;
    end if;
end function;

//---- our hash H' -------------------
function hashprime(s,k)
    return SHA1(s cat k);
end function;



function RingSign(m, P, S, s)
    n := P[1][1]; 
    k := SHA1(m); 
    R := #P;
    // ---- calculate the various xi and yi -------------------
    x := [Random([1..(2^160 - 1)]) : i in [1..R]];                // < a list of INTEGERS
    y := [IntToBin(   trapdoorg(x[i],n,P[i][2])   ,   160)   : i in [1..R]];      // < a list of BINARY VALUES
    
    // yes, we also calculate (xs, ys) but they will be overwritten; it's 
    // just more readable this way

    // ---- calculate the glue-value v -------------------
    rand := [Random([0,1]) : i in [1..160]];
    v := BinToHex(rand);
        //printf"SIGNATURE\n";
        //printf" message | %o\n", m;
        //printf" signer  | %o\n", s;
        //printf" pr. key | (%o,%o)\n", S,n;
        //for i in [1..R] do printf"   > y%o : %o\n",i,BinToHex(y[i]);
        //end for;
        //printf"combining function: going forward... \n";
        
    // we pack the random value into a series of 
    // hash functions (see below for the reason why)

    v :=  hashprime(v,k);
    //printf"   H(random)\n", R;
    for j in [s+1..R] do                            // for j in range(R,s+1)
        //printf"   H(y%o + ...[stuff]... )\n", j;
        h := HexToBin(v);                                   
        // ^ previous values, IN BINARY
        epsilon := y[j]; 
        // ^ yj, IN BINARY
        temp := listXOR(h,epsilon);                         // yj + H(...)
        // ^ xor of the two IN BINARY
        v := hashprime(BinToHex(temp),k);                   // H( yj + H(...) )    
        // ^ final resut IN HEXADECIMAL
    end for;

    // ---- solve the combining function for ys -------------------
    // we know the position s of the signer, and we "packed" the random value into a series of hash functions
    // like this: H(H(... H(rand || k))) s times, hence we are left with the following equality    ( + is a xor)
    //
    // -> H(yR + H( yR-1 + ... H(y1 + v))) = H(yR + H(... y_(s-1) + H(rand)))
    // -> yR + H(stuff) = yR + H(other stuff)
    // ... (do this for a total of s times) ...
    // -> H(ys + H(...)) = H(rand)
    // -> ys =  rand + H(r-s times stuff) 
    //
    // and indeed, H(r-s times stuff) can be calculated
                                
    //printf"combining function: going backwards... \n";
    H := v;
    ys := rand;
    
    for j in [1..s-1] do                                      // for j in range(2,R-s+1)
            //printf"   H(y%o + ...[stuff]... )\n", j;
            temp := BinToHex( listXOR(y[j], HexToBin(H)) );         // < yj + H(...)
            // ^ the temporary value IN HEXADECIMAL   
            H := hashprime( temp, k);                               // < H( yj + H(...) )
            // ^ the hash, IN HEXADECIMAL
    end for;

    ys := listXOR( ys, HexToBin(H) );
    //printf"   > ys : %o\n",BinToHex(ys);
    ys := BinToInt(ys);
    x[s] := trapdoorg(ys,n,S);
    //printf"   > xs : %o\n",BinToHex(IntToBin(x[s],160));
;
    return <P,v,x>;

end function;




// -----------------------------------------------------------------------------------
// --------------------------------------------------
// ---------------------- EXERCISE 2 ----------------
// --------------------------------------------------




function Ringverify(signature,m)
    P := signature[1];
    v := signature[2];
    x := signature[3];

    n := P[1][1]; 
    k := SHA1(m); 
    R := #P;
    correct := false;

    //printf"VERIFICATION\n";

    y := [IntToBin(   trapdoorg(x[i],n,P[i][2])   ,   160)   : i in [1..R]];
    H := hashprime( BinToHex(listXOR(y[1], HexToBin(v))) , k );
    for j in [2..R] do                                       // for j in range(2,R)
        //printf"   H(y%o + ...[stuff]... )\n", j;   
        H := listXOR( y[j] ,HexToBin(H));                    // < yj + H(...)
        H := hashprime(BinToHex(H),k);                       // < H( yj + H(...) )
    end for;
    
    if H eq v then
        correct := true;
    end if;

    //printf" v | %o\n", v;
    //printf" z | %o\n", H;
    return correct;
end function;




// -----------------------------------------------------------------------------------
// --------------------------------------------------
// ---------------------- BIG TEST!! ----------------
// --------------------------------------------------




charlist := ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z",
             "a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z",
             ",",".","!","-"," ", "?", "<",">",";",":"];

function encodeCharacter(c)
    for i in [1..#charlist] do
        if charlist[i]eq c then return Intseq(i,2,6);
        end if;
    end for;
end function;
function encodeString(str)
    returnvalue := [];
    for j in [1..#str] do
        returnvalue := returnvalue cat encodeCharacter(str[j]);
    end for;
    return returnvalue;
end function;
function decodeCharacter(bin)
    // "character" is a list of 5 entries 
    index := Seqint(bin,2);
    return charlist[index];
end function;
function decodeString(bin)
    str :="";
    total := #bin div 6;
    for j in [1..total] do
        character := bin[j*6-5..j*6];
        c := decodeCharacter(character);
        str := str cat c;
    end for;
    return str;
end function;


data := ["Who rides, so late, through night and wind?", "It is the father with his child.", "He has the boy well in his arm,",
    "He holds him safely, he keeps him warm.","My son, why do you hide your face in fear?","Father, do you not see the Erl-King?",
    "The Erl-King with crown and cape?","My son, it is a streak of fog.","<You dear child, come, go with me!","Very beautiful games, I play with you;",
    "Many colourful flowers are on the beach,","My mother has many a golden robe.>","My father, my father, and do you not hear","What the Erl-King quietly promises me?",
    "Be calm, stay calm, my child;","Through dry leaves, the wind is sighing.","<Do you, fine boy, want to go with me?","My daughters shall wait on you finely;",
    "My daughters lead the nightly dance,","And rock and dance and sing to bring you in.>","My father, my father, and dont you see there",
    "The Erl-Kings daughters in the gloomy place?","My son, my son, I see it clearly:","There shimmer the old willows so grey.","<I love you, your beautiful form excites me;",
    "And if youre not willing, then I will use force.>","My father, my father, hes touching me now!","The Erl-King has done me harm!","It horrifies the father; he swiftly rides on,",
    "He holds the moaning child in his arms,","Reaches the farm with great difficulty;","In his arms, the child was dead."];

// DOCUMENTATION for the text : https://youtu.be/hSY-bIwfxqw?t=18
// this will hopefully make someone's boring task more enjoyable 

procedure testSignature(R)
    m := SHA1("e0");

    p := Random(PrimesUpTo(1000000));
    q := Random(PrimesUpTo(1000000));
    N := p*q;
    RSA := [];

    for i in [1..R] do
        d := Random([2..N]);
        while GCD(d,EulerPhi(N)) ne 1 do
            d := Random([2..N]);
        end while;
        e := Modinv(d,EulerPhi(N));

        RSA := Append(RSA, <d,e,N>);
    end for;

    s := Random([1..R]);
    P := [<RSA[i][3],RSA[i][2]> : i in [1..R]];

    correct := 0;
    total := #data;
    printf"----------- starting tests -----------\n";
    printf"someone in this group of %o sent you lots of messages:\n",R;
    printf"--------------------------------------\n";
    for i in [1..total] do
        // create message
        message := data[i];

        while #message mod 4 ne 0 do
            message := message cat " ";
        end while;

        printf"%o\n",message;

        // send the hash to be signed
        tosign := SHA1(BinToHex(encodeString(message)));
        sigma := RingSign(tosign,P,RSA[s][1],s);
        correctsign := Ringverify(sigma,tosign);


        // check signature
        if correctsign then
            correct := correct + 1;
        end if;

        // the following is just pro forma, indeed the hashes coincide...
        received := message; // < exactly for this reason 
        if SHA1(BinToHex(encodeString(received))) ne tosign then 
            printf"TAMPERED MESSAGE RECEIVED";
        end if;
    end for;
    printf"----------- ending tests -----------\n";
    // conclusions
    printf"out of %o sent messages, %o had their source verified\n",total,correct;
    printf"(spoiler: it was user number %o)\n",s;

end procedure;






