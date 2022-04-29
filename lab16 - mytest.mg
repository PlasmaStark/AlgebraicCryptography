//Test vectors for the Anomalous Curve Attack. The test is composed of:
//1) The prime p definig the field.
//2) The coefficients of the Weierstrass equation A,B.
//3) The base point P for the curve.
//4) Another point Q=kP.

//Test 1:

p:=223;
A:=-1314;
B:=621;

P:=[1,73,1];
Q:=[152,217,1];
k:=113;

//Test 2;

p:=223;
A:=-1314;
B:=621;

P:=[5,33,1];
Q:=[111,152,1];
k := 19;

//Test 3;

p:=223;
A:=-1314;
B:=621;

P:=[68,5,1];
Q:=[102,212,1];
k:=206;


//Test4:

p:=730750818665451459112596905638433048232067471723;
A:=425706413842211054102700238164133538302169176474;
B:=203362936548826936673264444982866339953265530166;

P:=[ 1, 310536468939899693718962354338996655381367569020, 1 ];
Q:=[ 157801211189512312459339070833226423718315860582, 437052518347159869381356866752577105001466096269, 1 ];
k:=1711620914;


//Test 5:

p:=730750818665451459112596905638433048232067471723;
A:=425706413842211054102700238164133538302169176474;
B:=203362936548826936673264444982866339953265530166;

P:=[3, 38292783053156441019740319553956376819943854515,1];
Q:=[346656487490943407121538398002457319000803898493, 481424869038146622462984309184774835977932379872, 1];
k:=1833518826;


//Test 6:

p:=730750818665451459112596905638433048232067471723;
A:=425706413842211054102700238164133538302169176474;
B:=203362936548826936673264444982866339953265530166;

P:= [17, 173827014976148521051073746232750578872372755801, 1];
Q:= [203667819583814319389402073408354564719372108606, 586626210933970881663327161756122896184694265801, 1];
k:= 861927585;

//Test 7:

p:=730750818665451459112596905638433048232067471723;
A:=425706413842211054102700238164133538302169176474;
B:=203362936548826936673264444982866339953265530166;

P:=[27 , 243378333297517865785335268550248214179950561006, 1];
Q:= [522368097011866531942103781161731359042937948081, 164097083367518761683388291236884297833403897890, 1];
k:=1522172158;

//Test 8:

p:=13;
A:=7;
B:=3;

P:=[6,1,1];
Q:=[0,4,1];
k:=9;

//Test 9:

p:=13;
A:=7;
B:=3;

P := [ 8, 5, 1 ];
Q := [ 6, 10, 2 ];
k:= 3;


//Test 10:

p:=13;
A:=7;
B:=3;

P:=[4,2,1];
Q:=[8,8,1];
k:=2;








