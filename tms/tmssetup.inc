{ Reduce EXE size by disabling as much of RTTI as possible (delphi 2009/2010) }
{$WEAKLINKRTTI ON} //this affects the whole project, not just the unit, at link time.
{.$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])} // we are using RTTI for serialization, do not disable RTTI!

{$A8,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N-,O+,P+,Q+,R+,S-,T+,U-,V+,W-,X+,Y+,Z1}
{$SCOPEDENUMS ON}
{$WEAKLINKRTTI ON}
{$ZEROBASEDSTRINGS OFF}
