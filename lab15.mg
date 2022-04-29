// DE MODO TRANSLITTERANDI PUNTCORUM CURVAE 
// We shall consider a generic Edwards Curve in Fp with a 
// given value of p (2^256 - 19) and generic d. The 
// points on the curve will be internally handled as 
// touples P=<xp,yp>.

// my code starts on line 567












// --------------------------------------------------
// ---------------------- HASH FUNCT ----------------
// --------------------------------------------------
// this hash seems broken, 
// > SHA512("lol");
// CF83E1357EEFB8BDF1542850D66D8007D620E4050B5715DC83F4A921D36CE9CE47D0D13C5D85F2B\
// 0FF8318D2877EEC2F63B931BD47417A81A538327AF927DA3E
// > SHA512("scrooge");
// 1C10DCB4249D98CBB973B39DB2274B7E50737C71820F47CD7AD61BFEC51BF672F36135A96310F2D\
// C3D8FEA1514FD51CB087BA8E6195D8AE5E03C8CB23CB424F6
// > SHA512("il mio cane e' un carlino e sta sul cremlino");
// 0762936BEB2149D4A215173906C905B5CB68AA262B5C870936A13970E230D78587CD31AA9EC33E1\
// B3869AF720391E8CB3D69B33DA6C1B36AFE576AB9D0566077
// the above should not be possible (we work in hexadec), moreover its digests on hexa strings
// do NOT match the ones of the reals SHA512 
// I do not know what to make of this 

// my code starts on line 567











a := 0;
b := 7;
Gx := StringToInteger("79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798", 16);
Gy := StringToInteger("483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8", 16);
G := [Gx, Gy];
n := StringToInteger("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141", 16);
p := StringToInteger("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F", 16);

//-----------ENVIROMENT----------

H:=["6A09E667F3BCC908","BB67AE8584CAA73B","3C6EF372FE94F82B","A54FF53A5F1D36F1","510E527FADE682D1","9B05688C2B3E6C1F","1F83D9ABFB41BD6B","5BE0CD19137E2179"];

K:=["428A2F98D728AE22 ", "7137449123EF65CD ", "B5C0FBCFEC4D3B2F ", "E9B5DBA58189DBBC", "3956C25BF348B538 ", "59F111F1B605D019 ", "923F82A4AF194F9B ", "AB1C5ED5DA6D8118",
"D807AA98A3030242 ", "12835B0145706FBE ", "243185BE4EE4B28C ", "550C7DC3D5FFB4E2",
"72BE5D74F27B896F ", "80DEB1FE3B1696B1 ", "9BDC06A725C71235 ", "C19BF174CF692694",
"E49B69C19EF14AD2 ", "EFBE4786384F25E3 ", "0FC19DC68B8CD5B5 ", "240CA1CC77AC9C65",
"2DE92C6F592B0275 ", "4A7484AA6EA6E483 ", "5CB0A9DCBD41FBD4 ", "76F988DA831153B5",
"983E5152EE66DFAB ", "A831C66D2DB43210 ", "B00327C898FB213F ", "BF597FC7BEEF0EE4",
"C6E00BF33DA88FC2 ", "D5A79147930AA725 ", "06CA6351E003826F ", "142929670A0E6E70",
"27B70A8546D22FFC ", "2E1B21385C26C926 ", "4D2C6DFC5AC42AED ", "53380D139D95B3DF",
"650A73548BAF63DE ", "766A0ABB3C77B2A8 ", "81C2C92E47EDAEE6 ", "92722C851482353B",
"A2BFE8A14CF10364 ", "A81A664BBC423001 ", "C24B8B70D0F89791 ", "C76C51A30654BE30",
"D192E819D6EF5218 ", "D69906245565A910 ", "F40E35855771202A ", "106AA07032BBD1B8",
"19A4C116B8D2D0C8 ", "1E376C085141AB53 ", "2748774CDF8EEB99 ", "34B0BCB5E19B48A8",
"391C0CB3C5C95A63 ", "4ED8AA4AE3418ACB ", "5B9CCA4F7763E373 ", "682E6FF3D6B2B8A3",
"748F82EE5DEFB2FC ", "78A5636F43172F60 ", "84C87814A1F0AB72 ", "8CC702081A6439EC",
"90BEFFFA23631E28 ", "A4506CEBDE82BDE9 ", "BEF9A3F7B2C67915 ", "C67178F2E372532B",
"CA273ECEEA26619C ", "D186B8C721C0C207 ", "EADA7DD6CDE0EB1E ", "F57D4F7FEE6ED178",
"06F067AA72176FBA ", "0A637DC5A2C898A6 ", "113F9804BEF90DAE ", "1B710B35131C471B",
"28DB77F523047D84 ", "32CAAB7B40C72493 ", "3C9EBE0A15C9BEBC ", "431D67C49C100D4C",
"4CC5D4BECB3E42B6 ", "597F299CFC657E2A ", "5FCB6FAB3AD6FAEC ", "6C44198C4A475817"];

//FUNZIONE COMPLETAMENTO A s BITS

function Complete(r,s) //input r è numero intero e s è la lunghezza della sequenza a cui si vuole arrivare

r:=Reverse(Intseq(r,2)); // è una sequenza di bits

return ([ 0 : i in [1..s-#r]] cat r);
end function;

//--------SUB FUNCTIONS----------
// FUNZIONE AND

function AND (x,y)

return [(x[i]*y[i]) : i in [1..#x]];
end function;

//FUNZIONE XOR

function XOR (x,y)
return [(EuclideanNorm(x[i]-y[i])) : i in [1..#x]];
//return [((x[i]+y[i])mod 2) :i in [1..#x]];
end function;

//FUNZIONE NOT

function NOT (x)
return [((x[i]+1)mod 2) : i in [1..#x]];
end function;

//FUNZIONE CH

function Ch (x,y,z)

return (XOR (AND (x,y), AND (NOT(x),z)));
end function;

//FUNZIONE MAJ

function Maj (x,y,z)

return (XOR(XOR (AND(x,y),AND(x,z)), AND(y,z)));
end function;

//FUNZIONE SIGMA_ZERO

function SigmaZero (x)

return (XOR(XOR(Rotate(x,28), Rotate(x,34)),Rotate(x,39)));
end function;

//FUNZIONE SIGMA_UNO

function SigmaUno (x)

return (XOR(XOR(Rotate(x,14), Rotate(x,18)),Rotate(x,41)));
end function;

//FUNZIONE SHIFT A DESTRA

function Shift (x,s)

return ([ 0 : i in [1..s]] cat [x[i] : i in [1..(#x-s)]]);
end function;

//FUNZIONE SIGMA_PICCOLO_ZERO

function SigmaPiccoloZero (x)

return XOR(XOR(Rotate(x,1), Rotate(x,8)),Shift (x,7));
end function;

//FUNZIONE SIGMA_PICCOLO_UNO

function SigmaPiccoloUno (x)

return XOR(XOR(Rotate(x,19), Rotate(x,61)),Shift (x,6));
end function;

//FUNZIONE HEX_CHAR_TO_BIN

function HexCharToBin (c)
bin := [];
	case c:
			when "0":
				bin := bin cat [0,0,0,0];
			when "1":
				bin := bin cat [0,0,0,1];
			when "2":
				bin := bin cat [0,0,1,0];
			when "3":
				bin := bin cat [0,0,1,1];
			when "4":
				bin := bin cat [0,1,0,0];
			when "5":
				bin := bin cat [0,1,0,1];
			when "6":
				bin := bin cat [0,1,1,0];
			when "7":
				bin := bin cat [0,1,1,1];
			when "8":
				bin := bin cat [1,0,0,0];
			when "9":
				bin := bin cat [1,0,0,1];
			when "A":
				bin := bin cat [1,0,1,0];
			when "a":
				bin := bin cat [1,0,1,0];
			when "B":
				bin := bin cat [1,0,1,1];	
			when "b":
				bin := bin cat [1,0,1,1];
			when "C":
				bin := bin cat [1,1,0,0];
			when "c":
				bin := bin cat [1,1,0,0];
			when "D":
				bin := bin cat [1,1,0,1];
			when "d":
				bin := bin cat [1,1,0,1];
			when "E":
				bin := bin cat [1,1,1,0];
			when "e":
				bin := bin cat [1,1,1,0];
			when "F":
				bin := bin cat [1,1,1,1];
			when "f":
				bin := bin cat [1,1,1,1];

	end case;
	
	return bin;
end function;

//FUNZIONE HEX_TO_BIN

function HexToBin(h) 

return (&cat[HexCharToBin(h[i]) : i in [1..#h]]);
end function;


//FUNZIONE DA BINARIO A ESADECIMALE

function BinToHex(b) // b Ë una LISTA binaria; i bit indicano potenze di 2 CRESCENTI (magma style).
// la conversione in base 16, visto che 16=2^4, si fa a blocchi di 4
	r := #b mod 4; 
	//if r ne 0 then
	//	b := b cat [0: i in [1..r]];
	//end if;
	d := #b div 4;
	hex := ""; // la scrittura in base 16 sara' una stringa con caratteri relativi a potenze di 16 DECRESCENTI
	for i in [0..d-1] do
		case [b[4*i+k]: k in [1..4]]:
			when [0,0,0,0]:
				hex := hex cat "0";
			when [0,0,0,1]:
				hex := hex cat "1";
			when [0,0,1,0]:
				hex := hex cat "2";
			when [0,0,1,1]:
				hex := hex cat "3";
			when [0,1,0,0]:
				hex := hex cat "4";
			when [0,1,0,1]:
				hex := hex cat "5";
			when [0,1,1,0]:
				hex := hex cat "6";
			when [0,1,1,1]:
				hex := hex cat "7";
			when [1,0,0,0]:
				hex := hex cat "8";
			when [1,0,0,1]:
				hex := hex cat "9";
			when [1,0,1,0]:
				hex := hex cat "A";
			when [1,0,1,1]:
				hex := hex cat "B";
			when [1,1,0,0]:
				hex := hex cat "C";
			when [1,1,0,1]:
				hex := hex cat "D";
			when [1,1,1,0]:
				hex := hex cat "E";			
			when [1,1,1,1]:
				hex := hex cat "F";
		end case;
	end for;
	return hex;
end function;

//FUNZIONE SUM

function Sum2 (u,v) //input: due sequenze di bits
//Z:=Integers();
// u:=[u[i] : i in [1..#u]];
// v:=[v[i] : i in [1..#v]];

return Complete(((Seqint(Reverse(u),2) + Seqint(Reverse(v),2)) mod 2^64),64);
end function;	



//FUNZIONE PADDING

function padding (M) //L'input è una stringa esadecimale
M:= HexToBin(M);
l:=#M;
k,_:=Solution(1, 896-(l+1), 1024); //Solution(a,b,n) ritorna a*k = b mod n e n

return (M cat [ 1] cat [ 0 : i in [1..k]] cat Complete(l,128));

end function;

k:=[HexToBin(K[i]) : i in [1..#K]];

//---------------------MAIN FUNCTION----------------------------------

//Input: Hexdecimal string message of arbitrary length
//Output: 256 bits in hexdecimal
function SHA512(M)

W:= [[0 : i in [1..64]]:j in [1..80]];
M:= [Partition(block,64) : block in Partition(padding(M),1024)];
N:=#M; //N is the number of blocks
h:= [HexToBin(H[i]) : i in [1..#H]]; //H[i]:Setting initial value
//h is a list of lists (in binary)

//The following operations have to be performed on every block from 1 to N

for i in [1..N] do

//------------------------MESSAGES TABLE-------------------------------
	for t in [1..16] do 
		W[t]:= M[i][t]; 
	end for;

	for t in [17..80] do
		sigma1_piccolo := Seqint(Reverse(SigmaPiccoloUno(W[t-2])),2);
		sigma0_piccolo := Seqint(Reverse(SigmaPiccoloZero(W[t-15])),2);
		w7 := Seqint(Reverse(W[t-7]),2);
		w16 := Seqint(Reverse(W[t-16]),2);

		W[t]:= Complete((sigma1_piccolo + w7 + sigma0_piccolo + w16) mod 2^64, 64);
		// W[t]:= Sum2(Sum2(Sum2 (SigmaPiccoloUno(W[t-2]), W[t-7]),SigmaPiccoloZero(W[t-15])),W[t-16]);
	end for;

//-------------------VARIABLES INITIALIZATION---------------------------
a:=h[1];
b:=h[2];
c:=h[3];
d:=h[4];
e:=h[5];
f:=h[6];
g:=h[7];
q:=h[8];

//------------------OPERATIONS BETWEEN VARIABLES-------------------------


	for t in [1..80] do
		W_t := Seqint(Reverse(W[t]),2);
		k_t := Seqint(Reverse(k[t]),2);
		c_h := Seqint(Reverse(Ch(e,f,g)),2);
		sigma1 := Seqint(Reverse(SigmaUno(e)),2);
		q_i := Seqint(Reverse(q),2);
		maj := Seqint(Reverse(Maj(a,b,c)),2);
		sigma0 := Seqint(Reverse(SigmaZero(a)),2);
		d_i := Seqint(Reverse(d), 2);

		T1:= (q_i + sigma1 + c_h + k_t + W_t) mod 2^64;
		T2:= (sigma0 + maj) mod 2^64;
		q:=g;
		g:=f;
		f:=e;
		e:= Complete((d_i + T1) mod 2^64, 64);
		d:=c;
		c:=b;
		b:=a;
		a:=Complete((T1 + T2) mod 2^64, 64);
	end for;

//------------------CALCULATION OF i-TH HASH VALUE-----------------------
h[1]:= Sum2(a,h[1]);
h[2]:= Sum2(b,h[2]);
h[3]:= Sum2(c,h[3]);
h[4]:= Sum2(d,h[4]);
h[5]:= Sum2(e,h[5]);
h[6]:= Sum2(f,h[6]);
h[7]:= Sum2(g,h[7]);
h[8]:= Sum2(q,h[8]);	

end for;

//------------------------MESSAGE DIGEST---------------------------------
return BinToHex(h[1] cat h[2] cat h[3] cat h[4] cat h[5] cat h[6] cat h[7] cat h[8]);
end function;














// --------------------------------------------------
// ---------------------- EXERCISE 1 ----------------
// --------------------------------------------------









// ------------ suggested parameters ----------------------
	q := 2^255 - 19;
	b := 256;
	c := 3;
	//d := (-121665)*Modinv(121666,q); < shows my calculations
	d := 37095705934669439343138083508754565189542113879843219016388785533085940283555;

	// P := ;
	L := 2^252 + 27742317777372353535851937790883648493;
	
	// -- B is the unique point <x,4/5> for which x is positive -----------

	// the following calculates x,y of B, this is to show my calculations
	// y := (4*Modinv(5,q)) mod q;    <- 46316835694926478169428394003475163141307993866256225615783033603165251855960
	// tempsol := ( (y^2-1)*Modinv(d*y^2+1,q) ) mod q;
	// F:=FiniteField(q);
	// P<x> := PolynomialRing(F);
	// f := x^2 - tempsol;
	// Roots(f);
	// ^ [ <15112221349535400772501151409588531511454012693041857206046113283949847762202, 1>, 
	//  <42783823269122696939284341094755422415180979639778424813682678720006717057747, 1> ]
	// 
	B := <15112221349535400772501151409588531511454012693041857206046113283949847762202
		,46316835694926478169428394003475163141307993866256225615783033603165251855960>;
	// ^ no need to calculate them again 

function pointADD(P,Q)
    // add two points P,Q belonging to and EdCurve together
    partialfrac := (d*P[1]*Q[1]*P[2]*Q[2]) mod q;
    Rx := (P[1]*Q[2] + P[2]*Q[1])*Modinv(1+partialfrac,q);
    Ry := (P[2]*Q[2] + P[1]*Q[1])*Modinv(1-partialfrac,q);
    return <Rx mod q, Ry mod q>;
end function;

function pointMULTIPLY(n,P)
    // multiply a point belonging to an EdCurve exploiting double-and-add
    iter := Intseq(n,2);
    temppoint := P; // < increased by x2 each round
    value := <0,1>; // < total value till now (as of now, identity)
    for i in iter do
        if i eq 1 then // in this case, add value 
            value := pointADD(value,temppoint);
        end if;
        // increase x2 the temp multiplier 
        temppoint := pointADD(temppoint,temppoint);
    end for;
    return value;
end function;

function encodepoint(P)
	// encode a point according to the documentation
	// INPUT: P = <Px,pY>
	Px := Intseq(P[1],2,256)[1];
	Py := Intseq(P[2],2,256);
	return Py[1..255] cat [Px];
end function;

function decodepoint(bitstring)
	// decode a point according to the documentation
	// INPUT: bitstring y[1..255] cat x[1]
	Py := bitstring[1..255] cat [0];
	Py := Seqint(Py,2);
	Px := bitstring[256];

	u := (Py^2-1);
	v := (1+d*Py^2);

	xbar := (u*(v^3)*Modexp(u*v^7,7237005577332262213973186563042994240829374041602535252466099000494570602493,q) mod q);
	if ((v*xbar^2) mod q) eq (u mod q) then
		x1 := xbar mod q;
		x2 := q-xbar mod q;
	else 
		xbarbaro := ( xbar*Modexp(2,14474011154664524427946373126085988481658748083205070504932198000989141204987,q) ) mod q;
		x1 := xbarbaro mod q;
		x2 := q-xbarbaro mod q;
	end if;
	if Intseq(x1,2)[1] eq Px then Px:=x1; else Px := x2; end if;
	return <Px mod q, Py mod q>;
end function;

function in25519(P)
	// verifies if P lies in the curve ED25519 -x^2 + y^2 = c^2(1 + dx^2y^2)
	x2 := Modexp(P[1],2,q);
	y2 := Modexp(P[2],2,q);
	left := (-x2+y2) mod q;
	right := (1 + d*x2*y2) mod q;
	if left eq right then return true;
	else return false; end if;
end function;

function Ed25519_Sign(sk,m)
	//INPUTS:   -sk private signing key, as a hex string;
	//	    	-m message to be signed, as a hex string.
	//OUTPUTS:  -sigma signature on the message, as a hex string.

	// ------------ KEY GENERATION ----------------------
	// sk is the private key 
	H := SHA512(sk);
	Hbin := HexToBin(H);
	s := (2^(b-1) + &+[2^i * Hbin[i] : i in [3..b-2]]) mod q;
	A := pointMULTIPLY(s,B);
	A := encodepoint(A); // public key
	A := BinToHex(A);

	// ------------ SIGNATURE ----------------------
	r := SHA512(H cat m);
	r := Seqint(HexToBin(r), 2) mod L;
	R := encodepoint(pointMULTIPLY(r,B));
	R := BinToHex(R);

	k := HexToBin(SHA512(R cat A cat m));
	k := Seqint(k,2) mod L;

	S := (r+k*s) mod L;
	
	return A, R cat BinToHex(Intseq(S,2,256));
end function;

function Ed25519_Verify(pk, m, sigma)
	//INPUTS:   -pk public verification key, as a hex string;
	//			-m message to be signed, as a hex string;
	//			-sigma signature on the message, as a hex string.
	//OUTPUTS:  -true if and only is the signature is valid.

	bits := HexToBin(sigma);

	left := bits[1..256];
	right := bits[257..512];

	R := decodepoint(left);
	s := Seqint(right,2);
	A := decodepoint(HexToBin(pk));
	if not 2 in [0..L] then 
		printf"> integer s not in interval, invalid signature\n";
		return false; 
	end if;
	if not in25519(R) then
		printf"> point R not in the curve, invalid signature\n";
		return false;
	end if;
	if not in25519(A) then
		printf"> key A not in the curve, invalid signature\n";
		return false;
	end if;

	k := SHA512( BinToHex(left) cat pk cat m);
	k := Seqint(HexToBin(k),2);

	bool := false;

	lefteq := pointMULTIPLY(8*s,B);
	righteq := pointADD( pointMULTIPLY(8,R) , pointMULTIPLY(8*k,A) );

	// the signature, with valid parameters (see checks above),
	// is valid iff the following condition holds
	if lefteq eq righteq then bool := true; end if;

	return bool;
end function;







// --------------------------------------------------
// ---------------------- TESTS ---------------------
// --------------------------------------------------









// -------------- SOME FUNCTIONS FOR STRING ENCODING ------------


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


// -------------- DATA THAT WILL BE SENT/SIGNED ------------


data := ["Who rides, so late, through night and wind?", "It is the father with his child.", "He has the boy well in his arm,",
    "He holds him safely, he keeps him warm.","My son, why do you hide your face in fear?","Father, do you not see the Erl-King?",
    "The Erl-King with crown and cape?","My son, it is a streak of fog.","<You dear child, come, go with me!","Very beautiful games, I play with you;",
    "Many colourful flowers are on the beach,","My mother has many a golden robe.>","My father, my father, and do you not hear","What the Erl-King quietly promises me?",
    "Be calm, stay calm, my child;","Through dry leaves, the wind is sighing.","<Do you, fine boy, want to go with me?","My daughters shall wait on you finely;",
    "My daughters lead the nightly dance,","And rock and dance and sing to bring you in.>","My father, my father, and dont you see there",
    "The Erl-Kings daughters in the gloomy place?","My son, my son, I see it clearly:","There shimmer the old willows so grey.","<I love you, your beautiful form excites me;",
    "And if youre not willing, then I will use force.>","My father, my father, hes touching me now!","The Erl-King has done me harm!","It horrifies the father; he swiftly rides on,",
    "He holds the moaning child in his arms,","Reaches the farm with great difficulty;","In his arms, the child was dead."];



// -------------- BIG TEST ------------

procedure testSignature()

	printf"----------- loading parameters -----------\n";

	sk := [Random([0,1]) : i in [1..256]];
	sk := BinToHex(sk);

	correct := 0;
	total := #data;

	for i in [1..#data] do
		// send the hash of d to be signed (ideally that will be sent - signed - along the message d, but
		// this is just for funsies)
		m := encodeString(data[i]);
		m := SHA512( BinToHex(m) );
		printf"   %o \n",data[i];

		// sign & verify
		pk, sigma := Ed25519_Sign(sk,m);
		bool := Ed25519_Verify(pk,m,sigma);

		// the following is just pro forma, indeed the hashes coincide...
        received := data[i]; // < exactly for this reason 
        if SHA512( BinToHex(encodeString(data[i])) ) ne m then 
            printf"TAMPERED MESSAGE RECEIVED\n";
        end if;

		// this holds the number of correct signatures
		if bool then
			correct := correct + 1;
		end if;
	end for;
	printf"-------------- ending tests --------------\n";
	// conclusions
	printf"out of %o sent messages, %o had their source verified\n",total,correct;

end procedure;





// -------------- SMALL TEST ------------

procedure test(hexastring,sk)

	// sign & verify
	pk, sigma := Ed25519_Sign(sk,hexastring);
	bool := Ed25519_Verify(pk,hexastring,sigma);
	// printf"test: %o",bool;

end procedure;

function setkey()
	sk := [Random([0,1]) : i in [1..256]];
	return BinToHex(sk);

end function;